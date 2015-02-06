// ----------------------------------------------------------------------------
// (c) Vladimir Negacevschi, 2015
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.txt file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------
namespace FlexSearch.Core

open org.apache.lucene.search
open org.apache.lucene.analysis
open org.apache.lucene.search.highlight
open org.apache.lucene.index
open java.io
open java.lang
open System
open System.Collections.Generic
open FlexSearch.Api
open FlexSearch.Utility

// ------------------------
/// Determines whether it's an infinite value and if it's minimum or maximum
// ------------------------
type Infinite =
    | MinInfinite
    | MaxInfinite
    | NoInfinite

[<AutoOpen>]
module JavaHelpers = 
    // These are needed to satisfy certain Lucene query requirements
    let JavaLongMax = java.lang.Long(java.lang.Long.MAX_VALUE)
    let JavaLongMin = java.lang.Long(java.lang.Long.MIN_VALUE)
    let JavaDoubleMax = java.lang.Double(java.lang.Double.MAX_VALUE)
    let JavaDoubleMin = java.lang.Double(java.lang.Double.MIN_VALUE)
    let JavaIntMax = java.lang.Integer(java.lang.Integer.MAX_VALUE)
    let JavaIntMin = java.lang.Integer(java.lang.Integer.MIN_VALUE)

    let inline GetJavaDouble infinite (value : float) = 
        match infinite with
        | MaxInfinite -> JavaDoubleMax
        | MinInfinite -> JavaDoubleMin
        | _ -> java.lang.Double(value)
    let inline GetJavaInt infinite (value : int) = 
        match infinite with
        | MaxInfinite -> JavaIntMax
        | MinInfinite -> JavaIntMin
        | _ -> java.lang.Integer(value)
    let inline GetJavaLong infinite (value : int64) = 
        match infinite with
        | MaxInfinite -> JavaLongMax
        | MinInfinite -> JavaLongMin
        | _ -> java.lang.Long(value)

[<AutoOpen>]
module LuceneProviders =

    // ------------------------
    // CharTermAttributes
    // ------------------------
    let FlexCharTermAttribute =
        lazy java.lang.Class.forName 
                 (typeof<org.apache.lucene.analysis.tokenattributes.CharTermAttribute>.AssemblyQualifiedName)

    // ------------------------
    // Helpers
    // ------------------------
    /// Utility function to get tokens from the search string based upon the passed analyzer
    /// This will enable us to avoid using the Lucene query parser
    /// We cannot use simple white space based token generation as it really depends 
    /// upon the analyzer used
    let inline ParseTextUsingAnalyzer(analyzer : org.apache.lucene.analysis.Analyzer, fieldName, queryText) = 
        let tokens = new List<string>()
        let source : TokenStream = analyzer.tokenStream (fieldName, new StringReader(queryText))

        // Get the CharTermAttribute from the TokenStream
        let termAtt = source.addAttribute (FlexCharTermAttribute.Value)
        try 
            try 
                source.reset()
                while source.incrementToken() do
                    tokens.Add(termAtt.ToString())
                source.``end``()
            with ex -> ()
        finally
            source.close()
        tokens

    // Check if the passed field is numeric field
    let inline IsNumericField(flexField : FlexField) = 
        match flexField.FieldType with
        | FlexDate | FlexDateTime | FlexInt | FlexDouble | FlexLong -> true
        | _ -> false

    
    // ------------------------
    // Clauses
    // ------------------------
    let inline getBooleanClause (parameters : Dictionary<string, string> option)  =
        match parameters with
        | Some(p) -> 
            match p.TryGetValue("clausetype") with
            | true, b -> 
                match b with
                | InvariantEqual "or" -> BooleanClause.Occur.SHOULD
                | _ -> BooleanClause.Occur.MUST
            | _ -> BooleanClause.Occur.MUST
        | _ -> BooleanClause.Occur.MUST
    let addBooleanClause inheritedQuery occur (baseQuery : BooleanQuery) = 
        baseQuery.add(new BooleanClause(inheritedQuery, occur))
        baseQuery
    
    // ------------------------
    // Terms
    // ------------------------
    let inline getTerm (fieldName : string) (text : string) = 
        new Term(fieldName, text)

    // Find terms associated with the search string
    let inline GetTerms(flexField : FlexField, value) = 
        // Get a search query parser associated with the field 
        let inline GetSearchAnalyzer(flexField : FlexField) = 
            match flexField.FieldType with
            | FlexCustom(a, b, c) -> Some(a)
            | FlexHighlight(a, _) -> Some(a)
            | FlexText(a, _) -> Some(a)
            | FlexExactText(a) -> Some(a)
            | FlexBool(a) -> Some(a)
            | FlexDate | FlexDateTime | FlexInt | FlexDouble | FlexStored | FlexLong -> None

        match GetSearchAnalyzer(flexField) with
        | Some(a) -> ParseTextUsingAnalyzer(a, flexField.SchemaName, value)
        | None -> new List<string>([ value ])

    // ------------------------
    // Queries
    // ------------------------
    let getMatchAllDocsQuery = fun () -> new MatchAllDocsQuery() :> Query
    let getBooleanQuery = fun () -> new BooleanQuery() 
    let getTermQuery fieldName text = 
        new TermQuery(getTerm fieldName text) :> Query
    let getFuzzyQuery fieldName slop prefixLength text = 
        new FuzzyQuery((getTerm fieldName text), slop, prefixLength) :> Query
    let getPhraseQuery = fun () -> new PhraseQuery()
    let getWildCardQuery fieldName text =
        new WildcardQuery(getTerm fieldName text) :> Query
    let getRegexpQuery fieldName text =
        new RegexpQuery(getTerm fieldName text) :> Query

    // ------------------------
    // Query generators
    // ------------------------
    let getBoolQueryFromTerms boolClauseType innerQueryProvider terms = 
        let boolQuery = getBooleanQuery()
        terms
        |> Seq.iter (fun term -> 
            let innerQuery = innerQueryProvider term
            boolQuery 
            |> addBooleanClause innerQuery boolClauseType 
            |> ignore)
        boolQuery :> Query
    let zeroOneOrManyQuery terms innerQueryProvider boolClause = 
        match terms |> Seq.length with
        | 0 -> getMatchAllDocsQuery()
        | 1 -> innerQueryProvider (terms |> Seq.head)
        | _ -> 
            getBoolQueryFromTerms
                boolClause
                innerQueryProvider
                terms
        |> Choice1Of2
    
    // ------------------------
    // Range Queries
    // ------------------------
    let getRangeQuery 
        value 
        (includeLower, includeUpper) 
        (infiniteMin, infiniteMax)
        fIdxFld =

        let errorMessage = 
            Errors.DATA_CANNOT_BE_PARSED
            |> GenerateOperationMessage
            |> Append("Field Name", fIdxFld.FieldName)
            |> Choice2Of2

        match IsNumericField(fIdxFld) with
        | true -> 
            match fIdxFld.FieldType with
            | FlexDate | FlexDateTime | FlexLong -> 
                match Int64.TryParse(value) with
                | true, value' -> 
                    NumericRangeQuery.newLongRange
                      ( fIdxFld.SchemaName,
                        value' |> GetJavaLong infiniteMin,
                        value' |> GetJavaLong infiniteMax,
                        includeLower,
                        includeUpper )
                    :> Query |> Choice1Of2
                | _ -> errorMessage
            | FlexInt -> 
                match Int32.TryParse(value) with
                | true, value' -> 
                    NumericRangeQuery.newIntRange 
                        (fIdxFld.SchemaName, 
                        value' |> GetJavaInt infiniteMin, 
                        value' |> GetJavaInt infiniteMax, 
                        includeLower, 
                        includeUpper )
                    :> Query |> Choice1Of2
                | _ -> errorMessage
            | FlexDouble -> 
                match Double.TryParse(value) with
                | true, value' -> 
                    NumericRangeQuery.newDoubleRange 
                       ( fIdxFld.SchemaName, 
                        value' |> GetJavaDouble infiniteMin, 
                        value' |> GetJavaDouble infiniteMax, 
                        includeLower, 
                        includeUpper )
                    :> Query |> Choice1Of2
                | _ -> errorMessage
            | _ -> errorMessage
        | false -> errorMessage

    // ------------------------
    // Sorting
    // ------------------------
    let getSort (fieldName : string) (sortfieldType : SortField.Type) = 
        new Sort(new SortField(fieldName, sortfieldType))

    // ------------------------
    // Highlighting
    // ------------------------
    let getHighlighter (searchQuery : SearchQuery) flexIndex (query : Query) = 
        if searchQuery.Highlights <> Unchecked.defaultof<_> then 
            match searchQuery.Highlights.HighlightedFields with
            | x when x |> Seq.length = 1 -> 
                match flexIndex.IndexSetting.FieldsLookup.TryGetValue(x |> Seq.head) with
                | (true, field) -> 
                    let htmlFormatter = new SimpleHTMLFormatter(searchQuery.Highlights.PreTag, searchQuery.Highlights.PostTag)
                    Some(field, new Highlighter(htmlFormatter, new QueryScorer(query)))
                | _ -> None
            | _ -> None
        else None

    

    