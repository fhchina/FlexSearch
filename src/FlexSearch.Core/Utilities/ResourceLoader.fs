﻿// ----------------------------------------------------------------------------
// (c) Seemant Rajvanshi, 2014
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

/// <summary>
/// Concrete implementation of IResourceLoader
/// </summary>
[<Sealed>]
type ResourceLoader(persistenceStore : IPersistanceStore) = 
    interface IResourceLoader with
        
        member this.LoadResourceAsString(resourceName) = 
            let path = Helpers.GenerateAbsolutePath(".\\conf\\" + resourceName)
            Helpers.LoadFile(path)
        
        member this.LoadFilterList(resourceName) = persistenceStore.Get<FilterList>(resourceName)
        member this.LoadMapList(resourceName) = persistenceStore.Get<MapList>(resourceName)