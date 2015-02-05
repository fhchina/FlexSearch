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

open org.apache.lucene.search
open org.apache.lucene.analysis
open org.apache.lucene.search.highlight
open java.io
open System.Collections.Generic
open FlexSearch.Api

[<AutoOpen>]
module LuceneProviders =
    
    // CharTermAttributes
    let FlexCharTermAttribute =
        lazy java.lang.Class.forName 
                 (typeof<org.apache.lucene.analysis.tokenattributes.CharTermAttribute>.AssemblyQualifiedName)

    // Queries
    let getMatchAllDocsQuery = fun () -> new MatchAllDocsQuery()
    let getBooleanQuery = fun () -> new BooleanQuery() 

    // Clauses
    let addBooleanClause inheritedQuery occur (baseQuery : BooleanQuery) = 
        baseQuery.add(new BooleanClause(inheritedQuery, occur))
        baseQuery

    // Sorting
    let getSort (fieldName : string) (sortfieldType : SortField.Type) = 
        new Sort(new SortField(fieldName, sortfieldType))

    // Highlighter
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

    // Parsing

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