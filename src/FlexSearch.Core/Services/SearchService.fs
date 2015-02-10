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
namespace FlexSearch.Core.Services

open FlexSearch.Api
open FlexSearch.Common
open FlexSearch.Core
open FlexSearch.Utility
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Data
open System.IO
open System.Linq

/// <summary>
/// Search service class which will be dynamically injected using IOC. This will
/// provide the interface for all kind of search functionality in flex.
/// Exposes high level operations that can performed across the system.
/// Most of the services basically act as a wrapper around the functions 
/// here. Care should be taken to not introduce any mutable state in the
/// module but to only pass mutable state as an instance of NodeState
/// </summary>
[<Sealed>]
type SearchService(regManager : RegisterationManager, queryFactory : IFlexFactory<IFlexQuery>) = 
    
    // Generate query types from query factory. This is necessary as a single query can support multiple
    // query names
    let queryTypes = 
        let result = new Dictionary<string, IFlexQuery>(StringComparer.OrdinalIgnoreCase)
        for pair in queryFactory.GetAllModules() do
            for queryName in pair.Value.QueryName() do
                result.Add(queryName, pair.Value)
        result
    
    let GetSearchPredicate(flexIndex : FlexIndex, search : SearchQuery, inputValues : Dictionary<string, string> option) = 
        maybe { 
            if String.IsNullOrWhiteSpace(search.SearchProfile) <> true then 
                // Search profile based
                match flexIndex.IndexSetting.SearchProfiles.TryGetValue(search.SearchProfile) with
                | true, (p, sq) -> 
                    search.MissingValueConfiguration <- sq.MissingValueConfiguration
                    let! values = match inputValues with
                                  | Some(values) -> Choice1Of2(values)
                                  | None -> Parsers.ParseSearchProfileQuery(search.QueryString)
                    return! Choice1Of2(p, Some(values))
                | _ -> return! Choice2Of2(Errors.SEARCH_PROFILE_NOT_FOUND |> GenerateOperationMessage)
            else let! predicate = Parsers.ParsePredicateQuery(search.QueryString)
                 return! Choice1Of2(predicate, None)
        }
    
    let GenerateSearchQuery(flexIndex : FlexIndex, search : SearchQuery, inputValues : Dictionary<string, string> option) = 
        maybe { 
            let! (predicate, searchProfile) = GetSearchPredicate(flexIndex, search, inputValues)
            match predicate with
            | NotPredicate(_) -> return! Choice2Of2(Errors.NEGATIVE_QUERY_NOT_SUPPORTED |> GenerateOperationMessage)
            | _ -> 
                return! SearchDsl.GenerateQuery
                            (flexIndex.IndexSetting.FieldsLookup, predicate, search, searchProfile, queryTypes)
        }
    
    let GetSearchResult (searchQuery : SearchQuery) inputFields =
        maybe {
            let! flexIndex = regManager.IsOpen(searchQuery.IndexName)
            let! query = GenerateSearchQuery(flexIndex.Index.Value, searchQuery, inputFields)
            return! SearchDsl.SearchDocumentSeq(flexIndex.Index.Value, query, searchQuery)
        }

    let ToSearchResults (results : seq<_>, recordsReturned, totalAvailable) =
        let searchResults = new SearchResults()
        searchResults.Documents <- results.ToList()
        searchResults.TotalAvailable <- totalAvailable
        searchResults.RecordsReturned <- recordsReturned
        searchResults

    interface ISearchService with
        
        member this.SearchUsingProfile (searchQuery, inputFields) = 
            maybe { 
                let! output = GetSearchResult searchQuery (Some(inputFields))
                return! 
                    output
                    |> ToSearchResults
                    |> Choice1Of2
            }
        
        member this.Search searchQuery  = 
            maybe { 
                let! output = GetSearchResult searchQuery None
                return! 
                    output
                    |> ToSearchResults
                    |> Choice1Of2
            }
        
        member this.SearchAsDocumentSeq searchQuery = GetSearchResult searchQuery None

        member this.SearchAsDictionarySeq(searchQuery : SearchQuery) = 
            maybe { let! flexIndex = regManager.IsOpen(searchQuery.IndexName)
                    let! query = GenerateSearchQuery(flexIndex.Index.Value, searchQuery, None)
                    return! SearchDsl.SearchDictionarySeq(flexIndex.Index.Value, query, searchQuery) }
