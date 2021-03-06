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
namespace FlexSearch.Core

open FlexSearch.Api
open FlexSearch.Core
open FlexSearch.Utility
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Data
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Threading.Tasks.Dataflow
open java.io
open java.util
open org.apache.lucene.analysis
open org.apache.lucene.analysis.core
open org.apache.lucene.analysis.miscellaneous
open org.apache.lucene.analysis.util
open org.apache.lucene.codecs
open org.apache.lucene.codecs.lucene42
open org.apache.lucene.document
open org.apache.lucene.index
open org.apache.lucene.search
open org.apache.lucene.store
open FlexSearch.Common

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Index = 
    /// <summary>
    /// Index auto commit changes job 
    /// </summary>
    /// <param name="flexIndex"></param>
    let internal CommitJob(flexIndex : FlexIndex) = 
        // Looping over array by index number is usually the fastest
        // iteration method
        for i in 0..flexIndex.Shards.Length - 1 do
            // Lucene 4.4.0 feature to check for uncommitted changes
            if flexIndex.Shards.[i].IndexWriter.hasUncommittedChanges() then flexIndex.Shards.[i].IndexWriter.commit()
    
    /// <summary>
    /// Index auto commit changes job
    /// </summary>
    /// <param name="flexIndex"></param>
    let internal RefreshIndexJob(flexIndex) = 
        // Looping over array by index number is usually the fastest
        // iteration method
        for i in 0..flexIndex.Shards.Length - 1 do
            flexIndex.Shards.[i].SearcherManager.maybeRefresh() |> ignore
    
    /// <summary>
    /// Creates a async timer which can be used to execute a function at specified
    /// period of time. This is used to schedule all recurring indexing tasks
    /// </summary>
    /// <param name="delay">Delay to be applied</param>
    /// <param name="work">Method to perform the work</param>
    /// <param name="flexIndex">Index on which the job is to be scheduled</param>
    let private ScheduleIndexJob delay (work : FlexIndex -> unit) flexIndex = 
        let rec loop time (cts : CancellationTokenSource) = 
            async { 
                do! Async.Sleep(time)
                if (cts.IsCancellationRequested) then cts.Dispose()
                else 
                    try 
                        work (flexIndex)
                    with e -> cts.Dispose()
                return! loop delay cts
            }
        loop delay flexIndex.Token
    
    /// <summary>
    /// Add index to the registration
    /// </summary>
    /// <param name="state">Index state</param>
    /// <param name="flexIndexSetting">Index setting</param>
    let internal AddIndex(flexIndexSetting : FlexIndexSetting) = 
        maybe { 
            /// Generate shards for the newly added index
            let generateShards flexIndexSetting = 
                try 
                    let shards = 
                        Array.init flexIndexSetting.ShardConfiguration.ShardCount (fun a -> 
                            let path = Path.Combine([|flexIndexSetting.BaseFolder; "shards"; a.ToString() ; "index"|])
                            // Only create directory for non-ram index
                            if flexIndexSetting.IndexConfiguration.DirectoryType <> DirectoryType.Ram then
                                Directory.CreateDirectory(path) |> ignore
                            let writers = 
                                IndexingHelpers.GetIndexWriter
                                    (flexIndexSetting, path)
                            match writers with
                            | Choice2Of2(e) -> failwith e.UserMessage
                            | Choice1Of2(indexWriter, trackingIndexWriter) -> 
                                let shard = 
                                    { ShardNumber = a
                                      SearcherManager = new SearcherManager(indexWriter, true, new SearcherFactory())
                                      IndexWriter = indexWriter
                                      TrackingIndexWriter = trackingIndexWriter }
                                shard)
                    Choice1Of2(shards)
                with e -> 
                    Choice2Of2(Errors.ERROR_OPENING_INDEXWRITER
                               |> GenerateOperationMessage
                               |> Append("Message", e.Message))

            let! shards = generateShards flexIndexSetting
            let flexIndex = 
                { IndexSetting = flexIndexSetting
                  Shards = shards
                  ThreadLocalStore = new ThreadLocal<ThreadLocalDocument>()
                  VersioningManager = new VersioningManger(flexIndexSetting, shards) :> IVersionManager
                  Token = new System.Threading.CancellationTokenSource() }
            // Add the scheduler for the index
            // Commit Scheduler
            Async.Start
                (ScheduleIndexJob (flexIndexSetting.IndexConfiguration.CommitTimeSeconds * 1000) CommitJob flexIndex)
            // NRT Scheduler
            Async.Start
                (ScheduleIndexJob flexIndexSetting.IndexConfiguration.RefreshTimeMilliseconds RefreshIndexJob flexIndex)
            return! Choice1Of2(flexIndex)
        }
    
    /// <summary>
    /// Close an open index
    /// </summary>
    /// <param name="state"></param>
    /// <param name="flexIndex"></param>
    let internal CloseIndex(flexIndex : FlexIndex) = 
        try 
            flexIndex.Token.Cancel()
            for shard in flexIndex.Shards do
                try 
                    shard.SearcherManager.close()
                    shard.IndexWriter.commit()
                    shard.IndexWriter.close()
                with e -> ()
        with e -> () //logger.Error("Error while closing index:" + flexIndex.IndexSetting.IndexName, e)
    
    /// <summary>
    /// Function to check if the requested index is available. If yes then tries to 
    /// retrieve the document template associated with the index from thread local store.
    /// If there is no template document for the requested index then goes ahead
    /// and creates one. 
    /// </summary>
    /// <param name="state"></param>
    /// <param name="indexName"></param>
    let internal GetDocumentTemplate(flexIndex : FlexIndex) = 
        match flexIndex.ThreadLocalStore.IsValueCreated with
        | true -> Choice1Of2(flexIndex, flexIndex.ThreadLocalStore.Value)
        | _ -> 
            let luceneDocument = new Document()
            let fieldLookup = new Dictionary<string, Field>(StringComparer.OrdinalIgnoreCase)
            let idField = 
                new StringField(flexIndex.IndexSetting.FieldsLookup.[Constants.IdField].SchemaName, "", 
                                Field.Store.YES)
            luceneDocument.add (idField)
            fieldLookup.Add(Constants.IdField, idField)
            let lastModifiedField = 
                new LongField(flexIndex.IndexSetting.FieldsLookup.[Constants.LastModifiedField].SchemaName, 
                                GetCurrentTimeAsLong(), Field.Store.YES)
            luceneDocument.add (lastModifiedField)
            fieldLookup.Add(Constants.LastModifiedField, lastModifiedField)
            let lastModifiedFieldDv = 
                new NumericDocValuesField(flexIndex.IndexSetting.FieldsLookup.[Constants.LastModifiedFieldDv].SchemaName, 
                                            GetCurrentTimeAsLong())
            luceneDocument.add (lastModifiedFieldDv)
            fieldLookup.Add(Constants.LastModifiedFieldDv, lastModifiedFieldDv)
            for field in flexIndex.IndexSetting.Fields do
                // Ignore these 4 fields here.
                if (field.FieldName = Constants.IdField || field.FieldName = Constants.LastModifiedField 
                    || field.FieldName = Constants.LastModifiedFieldDv) then ()
                else 
                    let defaultField = FlexField.CreateDefaultLuceneField field
                    luceneDocument.add (defaultField)
                    fieldLookup.Add(field.FieldName, defaultField)
            let documentTemplate = 
                { Document = luceneDocument
                  FieldsLookup = fieldLookup
                  LastGeneration = 0 }
            flexIndex.ThreadLocalStore.Value <- documentTemplate
            Choice1Of2(flexIndex, documentTemplate)

    let inline private GetTargetShard(id : string, count : int) = 
        if (count = 1) then 0
        else IndexingHelpers.MapToShard id count
        
    /// <summary>
    /// Updates the current thread local index document with the incoming data
    /// </summary>
    /// <param name="flexIndex"></param>
    /// <param name="documentTemplate"></param>
    /// <param name="documentId"></param>
    /// <param name="version"></param>
    /// <param name="fields"></param>
    let internal UpdateDocument(flexIndex : FlexIndex, document : FlexDocument) = 
        let UpdateFields(documentTemplate) = 
            // Create a dynamic dictionary which will be used during scripting
            let dynamicFields = new DynamicDictionary(document.Fields)
            for field in flexIndex.IndexSetting.Fields do
                // Ignore these 3 fields here.
                if (field.FieldName = Constants.IdField || field.FieldName = Constants.LastModifiedField 
                    || field.FieldName = Constants.LastModifiedFieldDv) then ()
                else 
                    // If it is computed field then generate and add it otherwise follow standard path
                    match field.Source with
                    | Some(s) -> 
                        try 
                            // Wrong values for the data type will still be handled as update Lucene field will
                            // check the data type
                            let value = s.Invoke(dynamicFields)
                            FlexField.UpdateLuceneField field documentTemplate.FieldsLookup.[field.FieldName] value
                        with e -> 
                            FlexField.UpdateLuceneFieldToDefault field documentTemplate.FieldsLookup.[field.FieldName]
                    | None -> 
                        match document.Fields.TryGetValue(field.FieldName) with
                        | (true, value) -> 
                            FlexField.UpdateLuceneField field documentTemplate.FieldsLookup.[field.FieldName] value
                        | _ -> 
                            FlexField.UpdateLuceneFieldToDefault field documentTemplate.FieldsLookup.[field.FieldName]
        maybe { 
            let documentTemplate = flexIndex.ThreadLocalStore.Value
            let targetShard = GetTargetShard(document.Id, flexIndex.Shards.Length)
            let timeStamp = GetCurrentTimeAsLong()
            let! existingVersion = flexIndex.VersioningManager.VersionCheck(document, targetShard, timeStamp)
            if flexIndex.VersioningManager.AddOrUpdate(document.Id, targetShard, timeStamp, existingVersion) then             
                documentTemplate.FieldsLookup.[Constants.IdField].setStringValue(document.Id)
                documentTemplate.FieldsLookup.[Constants.LastModifiedField].setLongValue(timeStamp)
                documentTemplate.FieldsLookup.[Constants.LastModifiedFieldDv].setLongValue(timeStamp)
                UpdateFields(documentTemplate)            
                return! Choice1Of2(targetShard, documentTemplate)
            else
                return! Choice2Of2(Errors.INDEXING_VERSION_CONFLICT |> GenerateOperationMessage)
        }
