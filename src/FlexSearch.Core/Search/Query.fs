// ----------------------------------------------------------------------------
// Flexsearch predefined queries (Queries.fs)
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

open FlexSearch.Api
open FlexSearch.Core
open FlexSearch.Utility
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Linq
open java.io
open java.util
open org.apache.lucene.analysis
open org.apache.lucene.analysis.core
open org.apache.lucene.analysis.miscellaneous
open org.apache.lucene.analysis.standard
open org.apache.lucene.analysis.tokenattributes
open org.apache.lucene.analysis.util
open org.apache.lucene.index
open org.apache.lucene.queries
open org.apache.lucene.queryparser.classic
open org.apache.lucene.queryparser.flexible
open org.apache.lucene.search
open org.apache.lucene.search.highlight
open org.apache.lucene.search.postingshighlight

// ----------------------------------------------------------------------------
/// Term Query
// ----------------------------------------------------------------------------
[<Name("term_match")>]
[<Sealed>]
type FlexTermQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| "eq"; "=" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            match IsNumericField(flexIndexField) with
            | true -> 
                LuceneProviders.getRangeQuery
                    values.[0]
                    (true, true)
                    (NoInfinite, NoInfinite)
                    flexIndexField
            | false -> 
                // If there are multiple terms returned by the parser then we will create a boolean query
                // with all the terms as sub clauses with And operator
                // This behaviour will result in matching of both the terms in the results which may not be
                // adjacent to each other. The adjacency case should be handled through phrase query
                LuceneProviders.zeroOneOrManyQuery
                    (GetTerms(flexIndexField, values.[0]))
                    (LuceneProviders.getTermQuery flexIndexField.SchemaName)
                    (LuceneProviders.getBooleanClause parameters)

// ----------------------------------------------------------------------------
/// Fuzzy Query
// ---------------------------------------------------------------------------- 
[<Name("fuzzy_match")>]
[<Sealed>]
type FlexFuzzyQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| "fuzzy"; "~=" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            let slop = GetIntValueFromMap parameters "slop" 1
            let prefixLength = GetIntValueFromMap parameters "prefixlength" 0

            LuceneProviders.zeroOneOrManyQuery
                (GetTerms(flexIndexField, values.[0]))
                (LuceneProviders.getFuzzyQuery flexIndexField.SchemaName slop prefixLength)
                BooleanClause.Occur.MUST

// ----------------------------------------------------------------------------
/// Match all Query
// ---------------------------------------------------------------------------- 
[<Name("match_all")>]
[<Sealed>]
type FlexMatchAllQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| "matchall" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            LuceneProviders.getMatchAllDocsQuery() |> Choice1Of2

// ----------------------------------------------------------------------------
/// Phrase Query
// ---------------------------------------------------------------------------- 
[<Name("phrase_match")>]
[<Sealed>]
type FlexPhraseQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| "match" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            let terms = GetTerms(flexIndexField, values.[0])
            let query = LuceneProviders.getPhraseQuery()
            terms
            |> Seq.iter (fun term ->
                query.add (LuceneProviders.getTerm flexIndexField.SchemaName term))
            let slop = GetIntValueFromMap parameters "slop" 0
            query.setSlop (slop)
            query :> Query |> Choice1Of2

// ----------------------------------------------------------------------------
/// Wildcard Query
// ---------------------------------------------------------------------------- 
[<Name("like")>]
[<Sealed>]
type FlexWildcardQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| "like"; "%=" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            // Like query does not go through analysis phase as the analyzer would remove the
            // special character
            LuceneProviders.zeroOneOrManyQuery
                (values |> Seq.map (fun x -> x.ToLowerInvariant()))
                (LuceneProviders.getWildCardQuery flexIndexField.SchemaName)
                BooleanClause.Occur.MUST

// ----------------------------------------------------------------------------
/// Regex Query
// ---------------------------------------------------------------------------- 
[<Name("regex")>]
[<Sealed>]
type RegexQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| "regex" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            // Regex query does not go through analysis phase as the analyzer would remove the
            // special character
           LuceneProviders.zeroOneOrManyQuery
                (values |> Seq.map (fun x -> x.ToLowerInvariant()))
                (LuceneProviders.getRegexpQuery flexIndexField.SchemaName)
                BooleanClause.Occur.MUST

// ----------------------------------------------------------------------------
// Range Queries
// ---------------------------------------------------------------------------- 
[<Name("greater")>]
[<Sealed>]
type FlexGreaterQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| ">" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            // Greater query does not go through analysis phase as the analyzer would remove the
            // special character
            LuceneProviders.getRangeQuery
                values.[0]
                (false, true)
                (NoInfinite, MaxInfinite)
                flexIndexField

[<Name("greater_than_equal")>]
[<Sealed>]
type FlexGreaterThanEqualQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| ">=" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            // Greater query does not go through analysis phase as the analyzer would remove the
            // special character
            LuceneProviders.getRangeQuery
                values.[0]
                (true, true)
                (NoInfinite, MaxInfinite)
                flexIndexField

[<Name("less_than")>]
[<Sealed>]
type FlexLessThanQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| "<" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            // Greater query does not go through analysis phase as the analyzer would remove the
            // special character
            LuceneProviders.getRangeQuery
                values.[0]
                (true, false)
                (MinInfinite, NoInfinite)
                flexIndexField

[<Name("less_than_equal")>]
[<Sealed>]
type FlexLessThanEqualQuery() = 
    interface IFlexQuery with
        member this.QueryName() = [| "<=" |]
        member this.GetQuery(flexIndexField, values, parameters) = 
            // Greater query does not go through analysis phase as the analyzer would remove the
            // special character
            LuceneProviders.getRangeQuery
                values.[0]
                (true, true)
                (MinInfinite, NoInfinite)
                flexIndexField