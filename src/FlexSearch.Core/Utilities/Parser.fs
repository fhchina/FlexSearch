// ----------------------------------------------------------------------------
// (c) Seemant Rajvanshi, 2013
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.txt file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------
namespace FlexSearch.Core

[<AutoOpen>]
module Parsers = 
    open FParsec
    open FParsec.CharParsers
    open FParsec.Primitives
    open FlexSearch.Api
    open FlexSearch.Common
    open System
    open System.Collections.Generic
    open System.Linq
    
    let ws = spaces
    let str_ws s = pstringCI s .>> ws
    
    /// <summary>
    /// String literal parser. Takes '\' as espace character
    /// Based on: http://www.quanttec.com/fparsec/tutorial.html
    /// </summary>
    let stringLiteral = 
        let escape = anyOf "'" |>> function 
                     | c -> string c // every other char is mapped to itself
        between (pstring "\'") (pstring "\'") 
            (stringsSepBy (manySatisfy (fun c -> c <> '\'' && c <> '\\')) (pstring "\\" >>. escape)) |>> SingleValue 
        .>> ws
    
    let private stringLiteralAsString = 
        let escape = anyOf "'" |>> function 
                     | c -> string c // every other char is mapped to itself
        between (pstring "\'") (pstring "\'") 
            (stringsSepBy (manySatisfy (fun c -> c <> '\'' && c <> '\\')) (pstring "\\" >>. escape)) .>> ws
    
    let private stringLiteralList = 
        let escape = anyOf "'" |>> function 
                     | c -> string c // every other char is mapped to itself
        between (pstring "\'") (pstring "\'") 
            (stringsSepBy (manySatisfy (fun c -> c <> '\'' && c <> '\\')) (pstring "\\" >>. escape)) .>> ws
    
    let listOfValues = (str_ws "[" >>. sepBy1 stringLiteralList (str_ws ",") .>> str_ws "]") |>> ValueList .>> ws
    
    /// <summary>
    /// Value parser
    /// Note: THe order of choice is important as stringLiteral uses
    /// character backtracking.This is done to avoid the use of attempt.
    /// </summary>
    let private value = choice [ stringLiteral; listOfValues ]
    
    /// <summary>
    /// Identifier implementation. Alphanumeric character without spaces
    /// </summary>
    let private identifier = 
        many1SatisfyL (fun c -> c <> ' ' && c <> '(' && c <> ')' && c <> ':' && c <> ''') 
            "Field name should be alpha number without '(', ')' and ' '." .>> ws
    
    let private DictionaryOfList(elements : (string * string) list) = 
        let result = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        for (key, value) in elements do
            result.Add(key, value)
        result
    
    // ----------------------------------------------------------------------------
    // Query string parser 
    // Format: fieldname:'value',fieldname:'value',fieldname:'value'
    // ----------------------------------------------------------------------------
    let private keyValue = identifier .>>. (str_ws ":" >>. ws >>. stringLiteralAsString) .>> ws
    let private keyValuePairs = (sepBy keyValue (str_ws ",")) |>> DictionaryOfList .>> ws
    let private keyValuePairsBetweenBracket = between (str_ws "{") (str_ws "}") keyValuePairs .>> ws
    let private queryStringParser : Parser<_, unit> = ws >>. keyValuePairs .>> eof
    
    /// <summary>
    /// Search profile query string parser 
    /// Format: fieldname:'value',fieldname:'value',fieldname:'value'
    /// </summary>
    /// <param name="input"></param>
    let ParseSearchProfileQuery(input : string) = 
        let parse (queryString) (parser) = 
            match run parser queryString with
            | Success(result, _, _) -> Choice1Of2(result)
            | Failure(errorMsg, _, _) -> 
                Choice2Of2(Errors.QUERYSTRING_PARSING_ERROR
                           |> GenerateOperationMessage
                           |> Append("Message", errorMsg))
        assert (input <> null)
        queryStringParser |> parse input
    

    // ----------------------------------------------------------------------------
    // Predicate Queries
    // ----------------------------------------------------------------------------

    type Assoc = Associativity
    
    // Operator precedence parser that is only executed once in the application lifetime
    let private predicateParser = 
        let parameters = opt (ws >>. keyValuePairsBetweenBracket .>> ws)
    
        // Syntax: {FieldName} {Operator} {SingleValue|MultiFieldValue} {optional Boost}
        // Example: firstname eq 'a'
        let predicate = pipe4 identifier identifier value parameters (fun l o r b -> Condition(l, o, r, b))
    
        /// Generate all possible case combinations for the keywords
        let orCases = [ "or"; "oR"; "Or"; "OR" ]
        let andCases = [ "and"; "anD"; "aNd"; "aND"; "And"; "AnD"; "ANd"; "AND" ]
        let notCases = [ "not"; "noT"; "nOt"; "nOT"; "Not"; "NoT"; "NOt"; "NOT" ]

        let opp = new OperatorPrecedenceParser<Predicate, unit, unit>()
        let expr = opp.ExpressionParser
        let term = 
            // Use >>? to avoid the usage of attempt
            choice [ (str_ws "(" >>? expr .>> str_ws ")")
                     predicate ]
        
        opp.TermParser <- term
        orCases 
        |> List.iter (fun x -> opp.AddOperator(InfixOperator(x, ws, 1, Assoc.Left, fun x y -> OrPredidate(x, y))))
        andCases 
        |> List.iter (fun x -> opp.AddOperator(InfixOperator(x, ws, 2, Assoc.Left, fun x y -> AndPredidate(x, y))))
        notCases 
        |> List.iter (fun x -> opp.AddOperator(PrefixOperator(x, ws, 3, true, fun x -> NotPredicate(x))))

        opp

    /// <summary>
    /// Parses the given input with an OperatorPrecedenceParser. Using this parser's
    /// ExpressionParser is thread safe as long as the OPP instance is not modified at
    /// the same time.
    /// </summary>
    let ParsePredicateQuery input = 
        assert (input <> null)
        match run (ws >>. predicateParser.ExpressionParser .>> eof) input with
        | Success(result, _, _) -> Choice1Of2(result)
        | Failure(errorMsg, _, _) -> 
            Choice2Of2(Errors.QUERYSTRING_PARSING_ERROR
                        |> GenerateOperationMessage
                        |> Append("Message", errorMsg))