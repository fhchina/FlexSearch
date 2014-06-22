﻿// ----------------------------------------------------------------------------
// (c) Seemant Rajvanshi, 2013
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.txt file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------
namespace FlexSearch.Core.HttpModule

open FlexSearch.Api
open FlexSearch.Api.Message
open FlexSearch.Core
open FlexSearch.Core.HttpHelpers
open FlexSearch.Utility
open Microsoft.Owin
open Newtonsoft.Json
open Owin
open System
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.Composition
open System.Linq
open System.Net
open System.Net.Http

[<Name("index")>]
[<Sealed>]
type IndexModule(indexService : IIndexService) = 
    inherit HttpModuleBase()
    override this.Get(indexName, owin) = owin |> ResponseProcessor (indexService.GetIndex(indexName)) OK BAD_REQUEST
    
    override this.Post(indexName, owin) = 
        match GetRequestBody<Index> (owin.Request) with
        | Choice1Of2(index) -> 
            // Index name passed in URL takes precedence
            index.IndexName <- indexName
            owin |> ResponseProcessor (indexService.AddIndex(index)) OK BAD_REQUEST
        | Choice2Of2(error) -> 
            if error.ErrorCode = 6002 then 
                // In case the error is no body defined then still try to create the index based on index name
                let index = new Index()
                index.IndexName <- indexName
                owin |> ResponseProcessor (indexService.AddIndex(index)) OK BAD_REQUEST
            else owin |> BAD_REQUEST error
    
    override this.Delete(indexName, owin) = 
        owin |> ResponseProcessor (indexService.DeleteIndex(indexName)) OK BAD_REQUEST
    override this.Put(indexName, owin) = 
        match GetRequestBody<Index> (owin.Request) with
        | Choice1Of2(index) -> 
            // Index name passed in URL takes precedence
            index.IndexName <- indexName
            owin |> ResponseProcessor (indexService.UpdateIndex(index)) OK BAD_REQUEST
        | Choice2Of2(error) -> owin |> BAD_REQUEST error

[<Name("documents")>]
[<Sealed>]
type DocumentModule(state : INodeState, documentService : IDocumentService) = 
    inherit HttpModuleBase()
    
    override this.Get(indexName, owin) = 
        let processRequest = 
            maybe { 
                match CheckIdPresent (owin) with
                | Some(id) -> // documents/{id}
                              
                    match documentService.GetDocument(indexName, id) with
                    | Choice1Of2(v') -> return! Choice1Of2(v' :> obj)
                    | Choice2Of2(e) -> return! Choice2Of2(e)
                | None -> // documents
                          
                    // Return top 10 documents
                    match documentService.GetDocuments(indexName) with
                    | Choice1Of2(v') -> return! Choice1Of2(v' :> obj)
                    | Choice2Of2(e) -> return! Choice2Of2(e)
            }
        owin |> ResponseProcessor processRequest OK BAD_REQUEST
    
    override this.Post(indexName, owin) = 
        let processRequest = 
            maybe { 
                match CheckIdPresent (owin) with
                | Some(id) -> 
                    // documents/{id}
                    // Add the document by id
                    let! fields = GetRequestBody<Dictionary<string, string>> (owin.Request)
                    match fields.TryGetValue(Constants.IdField) with
                    | true, _ -> 
                        // Override dictionary id with the URL id
                        fields.[Constants.IdField] <- id
                    | _ -> fields.Add(Constants.IdField, id)
                    return! documentService.AddDocument(indexName, id, fields)
                | None -> // documents
                          
                    return! Choice2Of2(MessageConstants.HTTP_NOT_SUPPORTED)
            }
        owin |> ResponseProcessor processRequest OK BAD_REQUEST
    
    override this.Delete(indexName, owin) = 
        let processRequest = 
            maybe { 
                match CheckIdPresent (owin) with
                | Some(id) -> // documents/{id}
                              
                    return! documentService.DeleteDocument(indexName, id)
                | None -> // documents
                          
                    return! documentService.DeleteAllDocuments(indexName)
            }
        owin |> ResponseProcessor processRequest OK BAD_REQUEST
    
    override this.Put(indexName, owin) = 
        let processRequest = 
            maybe { 
                match CheckIdPresent (owin) with
                | Some(id) -> // documents/{id}
                              
                    let! fields = GetRequestBody<Dictionary<string, string>> (owin.Request)
                    match fields.TryGetValue(Constants.IdField) with
                    | true, _ -> 
                        // Override dictionary id with the URL id
                        fields.[Constants.IdField] <- id
                    | _ -> fields.Add(Constants.IdField, id)
                    return! documentService.AddOrUpdateDocument(indexName, id, fields)
                | None -> // documents
                          
                    return! Choice2Of2(MessageConstants.HTTP_NOT_SUPPORTED)
            }
        owin |> ResponseProcessor processRequest OK BAD_REQUEST

[<Name("search")>]
[<Sealed>]
type SearchModule(searchService : ISearchService) = 
    inherit HttpModuleBase()
    
    let processRequest (indexName, owin : IOwinContext) = 
        maybe { 
            let query = 
                match GetRequestBody<SearchQuery> (owin.Request) with
                | Choice1Of2(q) -> q
                | Choice2Of2(_) -> 
                    // It is possible that the query is supplied through query-string
                    new SearchQuery()
            query.QueryString <- GetValueFromQueryString "q" query.QueryString owin
            query.Columns <- match owin.Request.Query.Get("c") with
                             | null -> query.Columns
                             | v -> v.Split([| ',' |], System.StringSplitOptions.RemoveEmptyEntries).ToList()
            query.Count <- GetIntValueFromQueryString "count" query.Count owin
            query.Skip <- GetIntValueFromQueryString "skip" query.Skip owin
            query.OrderBy <- GetValueFromQueryString "orderby" query.OrderBy owin
            query.ReturnFlatResult <- GetBoolValueFromQueryString "returnflatresult" query.ReturnFlatResult owin
            query.IndexName <- indexName
            match searchService.Search(query) with
            | Choice1Of2(v') -> 
                if query.ReturnFlatResult then 
                    owin.Response.Headers.Add("RecordsReturned", [| v'.RecordsReturned.ToString() |])
                    owin.Response.Headers.Add("TotalAvailable", [| v'.TotalAvailable.ToString() |])
                    let result = v'.Documents |> Seq.map (fun x -> x.Fields)
                    return! Choice1Of2(result :> obj)
                else return! Choice1Of2(v' :> obj)
            | Choice2Of2(e) -> return! Choice2Of2(e)
        }
    
    override this.Get(indexName, owin) = owin |> ResponseProcessor (processRequest (indexName, owin)) OK BAD_REQUEST
    override this.Post(indexName, owin) = owin |> ResponseProcessor (processRequest (indexName, owin)) OK BAD_REQUEST

[<Name("exists")>]
[<Sealed>]
type ExistsModule(indexService : IIndexService) = 
    inherit HttpModuleBase()
    
    let processRequest (indexName : string, owin : IOwinContext) = 
        match indexService.IndexExists(indexName) with
        | true -> Choice1Of2()
        | false -> Choice2Of2(MessageConstants.INDEX_NOT_FOUND)
    
    override this.Get(indexName, owin) = owin |> ResponseProcessor (processRequest (indexName, owin)) OK BAD_REQUEST
    override this.Post(indexName, owin) = owin |> ResponseProcessor (processRequest (indexName, owin)) OK BAD_REQUEST

[<Name("status")>]
[<Sealed>]
type StatusModule(indexService : IIndexService) = 
    inherit HttpModuleBase()
    
    override this.Get(indexName, owin) = 
        let processRequest = 
            match indexService.GetIndexStatus(indexName) with
            | Choice1Of2(status) -> Choice1Of2(new IndexStatusResponse(status))
            | Choice2Of2(e) -> Choice2Of2(e)
        owin |> ResponseProcessor processRequest OK BAD_REQUEST
    
    override this.Post(indexName, owin) = 
        let processRequest = 
            match CheckIdPresent (owin) with
            | Some(id) -> 
                match id with
                | InvariantEqual "online" -> indexService.OpenIndex(indexName)
                | InvariantEqual "offline" -> indexService.CloseIndex(indexName)
                | _ -> Choice2Of2(MessageConstants.HTTP_NOT_SUPPORTED)
            | None -> Choice2Of2(MessageConstants.HTTP_NOT_SUPPORTED)
        owin |> ResponseProcessor processRequest OK BAD_REQUEST

[<Name("analysis")>]
[<Sealed>]
type AnalysisModule() = 
    inherit HttpModuleBase()
    let processRequest (indexName, owin) = maybe { return! Choice1Of2() }
    override this.Get(indexName, owin) = owin |> ResponseProcessor (processRequest (indexName, owin)) OK BAD_REQUEST
    override this.Post(indexName, owin) = owin |> ResponseProcessor (processRequest (indexName, owin)) OK BAD_REQUEST

[<Name("/")>]
[<Sealed>]
type RootModule() = 
    inherit HttpModuleBase()
    override this.Get(indexName, owin) = 
        owin.Response.ContentType <- "text/html"
        owin.Response.StatusCode <- 200
        await 
            (owin.Response.WriteAsync
                 ("FlexSearch " + System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString()))

[<Name("jobs")>]
[<Sealed>]
type JobModule(state : INodeState) = 
    inherit HttpModuleBase()
    
    let processRequest (indexName, owin) = 
        maybe { 
            match CheckIdPresent (owin) with
            | Some(id) -> return! state.PersistanceStore.Get<Job>(id)
            | None -> return! Choice2Of2(MessageConstants.JOBID_IS_NOT_FOUND)
        }
    
    override this.Post(indexName, owin) = owin |> ResponseProcessor (processRequest (indexName, owin)) OK BAD_REQUEST
