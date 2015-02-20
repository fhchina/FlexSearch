﻿namespace FlexSearch.IntegrationTests.Rest

open FlexSearch.Api
open FlexSearch.Core
open FlexSearch.Utility
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Net
open System.Net.Http
open System.Text
open System.Threading
open FlexSearch.TestSupport
open Autofac
open Xunit
open Xunit.Extensions
open Microsoft.Owin.Testing
open FlexSearch.TestSupport.RestHelpers
open FlexSearch.Client

type Dummy() =
    member val DummyProperty = "" with get, set

module ``Document Tests`` = 
    let TestIndex(indexName) = 
        let index = new Index(IndexName = indexName, Online = true)
        index.Fields.Add(new Field("firstname", FieldType.Text))
        index.Fields.Add(new Field("lastname", FieldType.Text))
        index

    let AddTestIndexWithData (client : IFlexClient) (idxSvc : IIndexService) indexName =
        let idx = TestIndex indexName
        let docs = 
          [ new FlexDocument(indexName, "1") 
            new FlexDocument(indexName, "2") ]
        docs.[0].Fields.Add("firstName", "Seemant")
        docs.[0].Fields.Add("lastName", "Rajvanshi")
        docs.[1].Fields.Add("firstName", "Vladimir")
        docs.[1].Fields.Add("lastName", "Negacevschi")
        
        idxSvc.AddIndex(idx) |> FlexSearch.TestSupport.Helpers.ExpectSuccess
        docs |> Seq.iter (fun doc ->
            client.AddDocument(indexName, doc).Result |> ExpectSuccess)
        idxSvc.Commit indexName |> ignore
        idxSvc.Refresh indexName |> ignore
    
    [<Theory; AutoMockIntegrationData; Example("get-indices-id-documents-1", "")>]
    let ``Get top 10 documents from an index`` (client : IFlexClient, indexName : Guid, handler : LoggingHandler) = 
        let actual = client.GetTopDocuments("country", 10).Result
        actual |> ExpectSuccess
        handler |> VerifyHttpCode HttpStatusCode.OK
        Assert.Equal<int>(10, actual.Data.RecordsReturned)
    
    [<Theory; AutoMockIntegrationData; Example("post-indices-id-documents-id-2", "")>]
    let ``Add a document to an index`` (client : IFlexClient, indexName : Guid, handler : LoggingHandler) = 
        client.AddIndex(TestIndex(indexName.ToString("N"))).Result |> ExpectSuccess
        let document = new FlexDocument(indexName.ToString("N"), "1")
        document.Fields.Add("firstname", "Seemant")
        document.Fields.Add("lastname", "Rajvanshi")
        let actual = client.AddDocument(indexName.ToString("N"), document).Result
        actual |> ExpectSuccess
        Assert.Equal<string>("1", actual.Data.Id)
        handler |> VerifyHttpCode HttpStatusCode.Created

    [<Theory; AutoMockIntegrationData>]
    let ``Cannot add a document without an id`` (client : IFlexClient, indexName : Guid, handler : LoggingHandler) = 
        client.AddIndex(TestIndex(indexName.ToString("N"))).Result |> ExpectSuccess
        let document = new FlexDocument(indexName.ToString("N"), " ")
        client.AddDocument(indexName.ToString("N"), document).Result |> ignore
        handler |> VerifyHttpCode HttpStatusCode.BadRequest
        printfn "%s" (handler.Log().ToString())

    [<Theory; AutoMockIntegrationData; Example("put-indices-id-documents-id-1", "")>]
    let ``Update a document to an index`` (client : IFlexClient, indexName : Guid, handler : LoggingHandler) = 
        ``Add a document to an index`` (client, indexName, handler)
        let document = new FlexDocument(indexName.ToString("N"), "1")
        document.Fields.Add("firstname", "Seemant")
        document.Fields.Add("lastname", "Rajvanshi1")
        let actual = client.UpdateDocument(indexName.ToString("N"), document).Result
        actual |> ExpectSuccess
        handler |> VerifyHttpCode HttpStatusCode.OK

    [<Theory; AutoMockIntegrationData; Example("get-indices-id-documents-id-1", "")>]
    let ``Get a document from an index`` (client : IFlexClient, indexName : Guid, handler : LoggingHandler) = 
        let actual = client.GetDocument("country", "10").Result
        actual |> ExpectSuccess
        Assert.Equal<String>("10", actual.Data.Id)

    [<Theory; AutoMockIntegrationData; Example("get-indices-id-documents-id-2", "")>]
    let ``Non existing document should return Not found`` (client : IFlexClient, indexName : Guid, handler : LoggingHandler) = 
        let actual = client.GetDocument("country", "355").Result
        handler |> VerifyHttpCode HttpStatusCode.NotFound

    [<Theory; AutoMockIntegrationData; Example("delete-indices-id-documents", "")>]
    let ``Delete all documents from an index`` (client : IFlexClient, indexName : Guid, handler : LoggingHandler, idxSvc : IIndexService) = 
        let idxStr = indexName.ToString("N")
        idxStr |> AddTestIndexWithData client idxSvc

        let docsBeforeDelete = client.GetTopDocuments(idxStr, 10).Result
        docsBeforeDelete |> ExpectSuccess
        Assert.True(docsBeforeDelete.Data.TotalAvailable > 0)

        client.DeleteAllDocuments(idxStr).Result |> ExpectSuccess
        idxSvc.Commit idxStr |> ignore

        let docsAfterDelete = client.GetTopDocuments(idxStr, 10).Result
        Assert.Equal<int>(docsAfterDelete.Data.TotalAvailable, 0)

    [<Theory; AutoMockIntegrationData; Example("delete-indices-id-documents-id", "")>]
    let ``Delete a document from an index`` (client : IFlexClient, indexName : Guid, handler : LoggingHandler, idxSvc : IIndexService) = 
        let idxStr = indexName.ToString("N")
        idxStr |> AddTestIndexWithData client idxSvc
        
        let docBeforeDelete = client.GetDocument(idxStr, "1").Result
        docBeforeDelete |> ExpectSuccess
        Assert.Equal<String>(docBeforeDelete.Data.Id, "1")

        client.DeleteDocument(idxStr, "1").Result |> ExpectSuccess
        idxSvc.Commit idxStr |> ignore
        
        let docAfterDelete = client.GetDocument(idxStr, "1").Result
        Assert.NotNull(docAfterDelete.Error)
        Assert.Equal<String>(
            (Errors.INDEXING_DOCUMENT_ID_NOT_FOUND |> GenerateOperationMessage).ErrorCode,
            docAfterDelete.Error.ErrorCode)