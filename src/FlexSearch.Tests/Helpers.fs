﻿module Helpers

open FlexSearch.Api
open FlexSearch.Api.Message
open FlexSearch.Core
open FsUnit
open Fuchu
open System
open System.Collections.Generic
open System.Linq
open System.Threading
open System.Collections.Concurrent

//let pluginContainer = PluginContainer(false).Value
//let factoryCollection = new FactoryCollection(pluginContainer) :> IFactoryCollection
//let settingBuilder = SettingsBuilder.SettingsBuilder factoryCollection (new Validator.IndexValidator(factoryCollection))
//let persistanceStore = new PersistanceStore("", true)
//let searchService = new SearchService(GetQueryModules(factoryCollection), getParserPool (2)) :> ISearchService
//let indicesState = 
//            { IndexStatus = new ConcurrentDictionary<string, IndexState>(StringComparer.OrdinalIgnoreCase)
//              IndexRegisteration = new ConcurrentDictionary<string, FlexIndex>(StringComparer.OrdinalIgnoreCase)
//              ThreadLocalStore = 
//                  new ThreadLocal<ConcurrentDictionary<string, ThreadLocalDocument>>(fun () -> 
//                  new ConcurrentDictionary<string, ThreadLocalDocument>(StringComparer.OrdinalIgnoreCase)) }
//let nodeState = 
//    { 
//        PersistanceStore = persistanceStore
//        ServerSettings = Unchecked.defaultof<_>
//        CacheStore = Unchecked.defaultof<_>
//        IndicesState = indicesState
//        SettingsBuilder = settingBuilder
//        SearchService = searchService }

let GetBasicIndexSettingsForContact() = 
    let index = new Index()
    index.IndexName <- Guid.NewGuid().ToString("N")
    index.Online <- true
    index.IndexConfiguration.DirectoryType <- DirectoryType.Ram
    index.Fields.Add("gender", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("title", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("givenname", new FieldProperties(FieldType = FieldType.Text))
    index.Fields.Add("middleinitial", new FieldProperties(FieldType = FieldType.Text))
    index.Fields.Add("surname", new FieldProperties(FieldType = FieldType.Text))
    index.Fields.Add("streetaddress", new FieldProperties(FieldType = FieldType.Text))
    index.Fields.Add("city", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("state", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("zipcode", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("country", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("countryfull", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("emailaddress", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("username", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("password", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("cctype", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("ccnumber", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("occupation", new FieldProperties(FieldType = FieldType.Text))
    index.Fields.Add("cvv2", new FieldProperties(FieldType = FieldType.Int))
    index.Fields.Add("nationalid", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("ups", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("company", new FieldProperties(FieldType = FieldType.Stored))
    index.Fields.Add("pounds", new FieldProperties(FieldType = FieldType.Double))
    index.Fields.Add("centimeters", new FieldProperties(FieldType = FieldType.Int))
    index.Fields.Add("guid", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("latitude", new FieldProperties(FieldType = FieldType.Double))
    index.Fields.Add("longitude", new FieldProperties(FieldType = FieldType.Double))
    index.Fields.Add("importdate", new FieldProperties(FieldType = FieldType.Date))
    index.Fields.Add("timestamp", new FieldProperties(FieldType = FieldType.DateTime))
    index.Fields.Add("topic", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("abstract", new FieldProperties(FieldType = FieldType.Text))
    // Computed fields
    index.Fields.Add("fullname", new FieldProperties(FieldType = FieldType.Text, ScriptName = "fullname"))
    index.Scripts.Add
        ("fullname", 
         new ScriptProperties("""return fields["givenname"] + " " + fields["surname"];""", ScriptType.ComputedField))
    let searchProfileQuery = 
        new SearchQuery(index.IndexName, "givenname = '' AND surname = '' AND cvv2 = '1' AND topic = ''")
    searchProfileQuery.MissingValueConfiguration.Add("givenname", MissingValueOption.ThrowError)
    searchProfileQuery.MissingValueConfiguration.Add("cvv2", MissingValueOption.Default)
    searchProfileQuery.MissingValueConfiguration.Add("topic", MissingValueOption.Ignore)
    index.SearchProfiles.Add("test1", searchProfileQuery)
    index

/// <summary>
/// Utility method to add data to an index
/// </summary>
/// <param name="indexService"></param>
/// <param name="index"></param>
/// <param name="testData"></param>
let AddTestDataToIndex(index : Index, testData : string, documentService: IDocumentService, indexService: IIndexService) = 
    let lines = testData.Split([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)
    let headers = lines.[0].Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
    for line in lines.Skip(1) do
        let items = line.Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
        let indexDocument = new Document()
        indexDocument.Id <- items.[0]
        indexDocument.Index <- index.IndexName
        for i in 1..items.Length - 1 do
            indexDocument.Fields.Add(headers.[i], items.[i])
        let result = documentService.AddDocument(index.IndexName, indexDocument.Id, indexDocument.Fields)
        ()
    indexService.Commit(index.IndexName) |> ignore
    Thread.Sleep(100)

/// <summary>
/// Utility to generate test data one line at a time from the source
/// </summary>
/// <param name="testData"></param>
let GenerateTestDataLines(testData : string) = 
    let lines = testData.Split([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)
    let headers = lines.[0].Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
    seq { 
        for line in lines.Skip(1) do
            let items = line.Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
            let indexDocument = new Dictionary<string, string>()
            for i in 1..items.Length - 1 do
                indexDocument.Add(headers.[i], items.[i])
            yield (items.[0], indexDocument)
    }

let expectedFailureMessage (message : OperationMessage) (f : Choice<_, OperationMessage>) = 
    match f with
    | Choice1Of2(_) -> Assert.AreEqual(1, 2, sprintf "Expected an error of type: %i." message.ErrorCode)
    | Choice2Of2(error) -> 
        if message.ErrorCode = error.ErrorCode then Assert.AreEqual(1, 1)
        else Assert.AreEqual(1, 2, sprintf "Expected an error of type: %i." message.ErrorCode)

let expectedSuccessMessage (message : 'T) (f : Choice<'T, _>) = 
    match f with
    | Choice1Of2(a) -> Assert.AreEqual(message, a)
    | Choice2Of2(error) -> Assert.AreEqual(1, 2, "Expected Choice1Of1 but received Choice1Of2.")

let getResult (f : Choice<SearchResults, OperationMessage>) = 
    match f with
    | Choice1Of2(a) -> a
    | Choice2Of2(b) -> failtest b.DeveloperMessage

let resourceLoaderMock = 
    { new IResourceLoader with
          member this.LoadResourceAsString str = "hello"
          member this.LoadResourceAsList str = ([| "hello"; "world" |].ToList())
          member this.LoadResourceAsMap str = 
              let result = new List<string []>()
              result.Add([| "easy"; "simple"; "clear" |])
              result }

/// <summary>
/// Mock test data to be used for documentation and testing
/// </summary>
let MockTestData = """
id,firstname,lastname,email,country,ipaddress,phone,cvv2,description
1,Kathy,Banks,kbanks@thoughtbeat.edu,Botswana,43.219.148.22,1-(666)874-5613,116,Donec odio justo  sollicitudin ut  suscipit a  feugiat et  eros. Vestibulum ac est lacinia nisi venenatis tristique. Fusce congue  diam id ornare imperdiet  sapien urna pretium nisl  ut volutpat sapien arcu sed augue. Aliquam erat volutpat. In congue. Etiam justo. Etiam pretium iaculis justo.
2,Cathy,Matthews,cmatthews@skinte.mil,Tokelau,190.10.95.135,4-(777)223-1981,689,Vestibulum quam sapien  varius ut  blandit non  interdum in  ante. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Duis faucibus accumsan odio. Curabitur convallis. Duis consequat dui nec nisi volutpat eleifend. Donec ut dolor. Morbi vel lectus in quam fringilla rhoncus. Mauris enim leo  rhoncus sed  vestibulum sit amet  cursus id  turpis. Integer aliquet  massa id lobortis convallis  tortor risus dapibus augue  vel accumsan tellus nisi eu orci.
3,Stephen,Griffin,sgriffin@innotype.name,Costa Rica,146.86.88.66,9-(371)760-3883,501,In hac habitasse platea dictumst. Maecenas ut massa quis augue luctus tincidunt. Nulla mollis molestie lorem. Quisque ut erat. Curabitur gravida nisi at nibh. In hac habitasse platea dictumst.
4,Angela,Owens,aowens@jabbersphere.edu,Malaysia,0.68.246.247,4-(989)963-1320,444,Maecenas pulvinar lobortis est.
5,Elizabeth,Watson,ewatson@youfeed.name,Malawi,199.220.62.219,6-(920)253-7491,219,Cum sociis natoque penatibus et magnis dis parturient montes  nascetur ridiculus mus.
6,Debra,Ferguson,dferguson@mycat.name,Oman,176.14.185.16,7-(462)398-5908,149,Nullam orci pede  venenatis non  sodales sed  tincidunt eu  felis. Fusce posuere felis sed lacus. Morbi sem mauris  laoreet ut  rhoncus aliquet  pulvinar sed  nisl.
7,Timothy,George,tgeorge@pixope.com,Bangladesh,97.3.237.152,3-(267)055-6128,523,Nulla nisl. Nunc nisl. Duis bibendum  felis sed interdum venenatis  turpis enim blandit mi  in porttitor pede justo eu massa. Donec dapibus. Duis at velit eu est congue elementum. In hac habitasse platea dictumst.
8,Anne,Cook,acook@gabtune.biz,Hungary,239.67.98.94,0-(910)578-5894,801,Cras non velit nec nisi vulputate nonummy.
9,Howard,Green,hgreen@realfire.gov,Anguilla,132.31.161.171,8-(523)305-6460,768,Nunc purus. Phasellus in felis. Donec semper sapien a libero. Nam dui.
10,Kenneth,Weaver,kweaver@skinte.com,Saint Helena,9.84.248.23,1-(985)345-3705,284,Maecenas leo odio  condimentum id  luctus nec  molestie sed  justo. Pellentesque viverra pede ac diam. Cras pellentesque volutpat dui. Maecenas tristique  est et tempus semper  est quam pharetra magna  ac consequat metus sapien ut nunc.
11,Wanda,Garrett,wgarrett@lajo.net,Congo  Republic of,101.45.254.118,3-(000)975-8974,454,Etiam pretium iaculis justo. In hac habitasse platea dictumst. Etiam faucibus cursus urna.
12,Carlos,Dixon,cdixon@geba.com,Dominica,38.113.58.102,1-(266)117-4775,306,Nulla ac enim. In tempor  turpis nec euismod scelerisque  quam turpis adipiscing lorem  vitae mattis nibh ligula nec sem. Duis aliquam convallis nunc. Proin at turpis a pede posuere nonummy.
13,Jennifer,Hayes,jhayes@omba.gov,Antigua and Barbuda,117.90.101.129,8-(788)597-9654,863,Morbi non lectus. Aliquam sit amet diam in magna bibendum imperdiet. Nullam orci pede  venenatis non  sodales sed  tincidunt eu  felis. Fusce posuere felis sed lacus. Morbi sem mauris  laoreet ut  rhoncus aliquet  pulvinar sed  nisl. Nunc rhoncus dui vel sem. Sed sagittis.
14,Christine,Carter,ccarter@yakijo.edu,Laos,12.128.3.129,7-(566)286-0812,225,Integer tincidunt ante vel ipsum. Praesent blandit lacinia erat. Vestibulum sed magna at nunc commodo placerat. Praesent blandit.
15,Victor,Bowman,vbowman@skippad.name,Slovakia,47.180.43.191,7-(710)057-4206,927,Duis consequat dui nec nisi volutpat eleifend. Donec ut dolor. Morbi vel lectus in quam fringilla rhoncus. Mauris enim leo  rhoncus sed  vestibulum sit amet  cursus id  turpis.
16,Gregory,Dean,gdean@realblab.edu,Timor-Leste,234.90.160.60,7-(187)768-0030,327,Integer pede justo  lacinia eget  tincidunt eget  tempus vel  pede. Morbi porttitor lorem id ligula. Suspendisse ornare consequat lectus. In est risus  auctor sed  tristique in  tempus sit amet  sem. Fusce consequat.
17,Rebecca,Harvey,rharvey@brainbox.edu,French Polynesia,36.241.213.163,2-(211)768-2446,470,Nullam porttitor lacus at turpis.
18,Arthur,Hamilton,ahamilton@dazzlesphere.org,Laos,97.85.215.182,8-(924)357-0340,583,Quisque erat eros  viverra eget  congue eget  semper rutrum  nulla. Nunc purus. Phasellus in felis. Donec semper sapien a libero.
19,John,White,jwhite@meedoo.com,Finland,133.237.122.26,0-(051)515-6307,250,Cum sociis natoque penatibus et magnis dis parturient montes  nascetur ridiculus mus. Etiam vel augue. Vestibulum rutrum rutrum neque.
20,Diana,Webb,dwebb@voonyx.edu,Chad,143.210.185.14,7-(330)202-4871,130,Integer non velit.
21,Todd,Wilson,twilson@gigabox.biz,American Samoa,24.112.196.100,0-(261)418-2572,906,Praesent blandit. Nam nulla. Integer pede justo  lacinia eget  tincidunt eget  tempus vel  pede. Morbi porttitor lorem id ligula. Suspendisse ornare consequat lectus. In est risus  auctor sed  tristique in  tempus sit amet  sem.
22,Martin,Carpenter,mcarpenter@wikizz.net,Tokelau,7.155.111.35,3-(416)680-4238,358,Sed vel enim sit amet nunc viverra dapibus. Nulla suscipit ligula in lacus. Curabitur at ipsum ac tellus semper interdum. Mauris ullamcorper purus sit amet nulla. Quisque arcu libero  rutrum ac  lobortis vel  dapibus at  diam.
23,Daniel,Long,dlong@demivee.biz,Saint Kitts and Nevis,213.192.51.165,0-(763)323-1346,125,Donec vitae nisi. Nam ultrices  libero non mattis pulvinar  nulla pede ullamcorper augue  a suscipit nulla elit ac nulla. Sed vel enim sit amet nunc viverra dapibus. Nulla suscipit ligula in lacus.
24,Stephen,Hall,shall@divavu.com,Venezuela,80.75.235.32,6-(380)682-3670,364,Maecenas pulvinar lobortis est. Phasellus sit amet erat. Nulla tempus. Vivamus in felis eu sapien cursus vestibulum. Proin eu mi. Nulla ac enim.
25,Carlos,Patterson,cpatterson@jumpxs.net,Hungary,101.21.92.45,1-(098)610-6606,140,Quisque ut erat. Curabitur gravida nisi at nibh. In hac habitasse platea dictumst. Aliquam augue quam  sollicitudin vitae  consectetuer eget  rutrum at  lorem. Integer tincidunt ante vel ipsum. Praesent blandit lacinia erat.
26,Steve,Wells,swells@latz.mil,Cayman Islands,97.193.47.79,2-(686)361-5510,903,In hac habitasse platea dictumst. Maecenas ut massa quis augue luctus tincidunt. Nulla mollis molestie lorem. Quisque ut erat. Curabitur gravida nisi at nibh. In hac habitasse platea dictumst. Aliquam augue quam  sollicitudin vitae  consectetuer eget  rutrum at  lorem. Integer tincidunt ante vel ipsum. Praesent blandit lacinia erat.
27,Justin,Brooks,jbrooks@dabz.com,Cameroon,33.160.41.172,4-(141)304-4915,748,Aenean sit amet justo. Morbi ut odio. Cras mi pede  malesuada in  imperdiet et  commodo vulputate  justo. In blandit ultrices enim. Lorem ipsum dolor sit amet  consectetuer adipiscing elit. Proin interdum mauris non ligula pellentesque ultrices. Phasellus id sapien in sapien iaculis congue. Vivamus metus arcu  adipiscing molestie  hendrerit at  vulputate vitae  nisl.
28,Adam,Ramos,aramos@kwinu.gov,Turkmenistan,50.218.203.75,5-(313)293-1978,194,Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec pharetra  magna vestibulum aliquet ultrices  erat tortor sollicitudin mi  sit amet lobortis sapien sapien non mi. Integer ac neque.
29,Shirley,Lynch,slynch@talane.gov,Montserrat,19.221.223.66,7-(816)990-6552,953,Morbi vestibulum  velit id pretium iaculis  diam erat fermentum justo  nec condimentum neque sapien placerat ante. Nulla justo. Aliquam quis turpis eget elit sodales scelerisque. Mauris sit amet eros. Suspendisse accumsan tortor quis turpis. Sed ante. Vivamus tortor. Duis mattis egestas metus. Aenean fermentum. Donec ut mauris eget massa tempor convallis.
30,Harold,Ferguson,hferguson@skiba.info,Madagascar,68.40.14.3,7-(746)052-0857,322,In hac habitasse platea dictumst. Morbi vestibulum  velit id pretium iaculis  diam erat fermentum justo  nec condimentum neque sapien placerat ante. Nulla justo. Aliquam quis turpis eget elit sodales scelerisque. Mauris sit amet eros. Suspendisse accumsan tortor quis turpis. Sed ante. Vivamus tortor. Duis mattis egestas metus. Aenean fermentum.
31,Albert,Garcia,agarcia@kwinu.com,Saint Helena,133.176.249.28,5-(121)118-7802,423,Nullam orci pede  venenatis non  sodales sed  tincidunt eu  felis. Fusce posuere felis sed lacus. Morbi sem mauris  laoreet ut  rhoncus aliquet  pulvinar sed  nisl. Nunc rhoncus dui vel sem. Sed sagittis.
32,Jack,Taylor,jtaylor@katz.gov,Iraq,46.90.98.247,6-(619)713-5794,541,Aliquam quis turpis eget elit sodales scelerisque. Mauris sit amet eros. Suspendisse accumsan tortor quis turpis. Sed ante. Vivamus tortor. Duis mattis egestas metus. Aenean fermentum. Donec ut mauris eget massa tempor convallis.
33,Janet,Day,jday@gigashots.org,Mauritius,180.200.126.70,2-(865)090-6132,958,Morbi porttitor lorem id ligula. Suspendisse ornare consequat lectus. In est risus  auctor sed  tristique in  tempus sit amet  sem. Fusce consequat. Nulla nisl. Nunc nisl.
34,Samuel,Crawford,scrawford@blognation.com,Nauru,59.65.231.0,1-(681)007-8507,435,Nulla tempus. Vivamus in felis eu sapien cursus vestibulum. Proin eu mi. Nulla ac enim. In tempor  turpis nec euismod scelerisque  quam turpis adipiscing lorem  vitae mattis nibh ligula nec sem. Duis aliquam convallis nunc. Proin at turpis a pede posuere nonummy. Integer non velit. Donec diam neque  vestibulum eget  vulputate ut  ultrices vel  augue. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec pharetra  magna vestibulum aliquet ultrices  erat tortor sollicitudin mi  sit amet lobortis sapien sapien non mi.
35,Matthew,Reynolds,mreynolds@devbug.biz,Vatican City State (Holy See),181.123.220.233,0-(954)950-2333,244,Cras mi pede  malesuada in  imperdiet et  commodo vulputate  justo. In blandit ultrices enim. Lorem ipsum dolor sit amet  consectetuer adipiscing elit. Proin interdum mauris non ligula pellentesque ultrices. Phasellus id sapien in sapien iaculis congue. Vivamus metus arcu  adipiscing molestie  hendrerit at  vulputate vitae  nisl. Aenean lectus. Pellentesque eget nunc.
36,Phillip,Ramirez,pramirez@skipfire.net,Azerbaijan,29.174.180.128,0-(089)416-6523,800,Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Mauris viverra diam vitae quam. Suspendisse potenti. Nullam porttitor lacus at turpis. Donec posuere metus vitae ipsum. Aliquam non mauris. Morbi non lectus. Aliquam sit amet diam in magna bibendum imperdiet. Nullam orci pede  venenatis non  sodales sed  tincidunt eu  felis.
37,Walter,Ramirez,wramirez@skiptube.org,Syria,200.237.216.215,7-(413)881-9052,900,Aliquam non mauris. Morbi non lectus. Aliquam sit amet diam in magna bibendum imperdiet. Nullam orci pede  venenatis non  sodales sed  tincidunt eu  felis. Fusce posuere felis sed lacus. Morbi sem mauris  laoreet ut  rhoncus aliquet  pulvinar sed  nisl. Nunc rhoncus dui vel sem. Sed sagittis. Nam congue  risus semper porta volutpat  quam pede lobortis ligula  sit amet eleifend pede libero quis orci. Nullam molestie nibh in lectus.
38,Louis,Murray,lmurray@livez.edu,Cambodia,194.139.73.210,7-(345)990-1844,735,Nulla ut erat id mauris vulputate elementum. Nullam varius. Nulla facilisi. Cras non velit nec nisi vulputate nonummy. Maecenas tincidunt lacus at velit. Vivamus vel nulla eget eros elementum pellentesque. Quisque porta volutpat erat. Quisque erat eros  viverra eget  congue eget  semper rutrum  nulla. Nunc purus.
39,George,Mendoza,gmendoza@skinder.name,China,18.96.60.63,2-(282)995-1726,441,Pellentesque viverra pede ac diam. Cras pellentesque volutpat dui. Maecenas tristique  est et tempus semper  est quam pharetra magna  ac consequat metus sapien ut nunc. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Mauris viverra diam vitae quam. Suspendisse potenti. Nullam porttitor lacus at turpis. Donec posuere metus vitae ipsum. Aliquam non mauris. Morbi non lectus.
40,Bruce,Tucker,btucker@bluezoom.mil,Saint Barthelemy,54.27.106.157,6-(197)119-4930,,Proin leo odio  porttitor id  consequat in  consequat ut  nulla. Sed accumsan felis. Ut at dolor quis odio consequat varius. Integer ac leo. Pellentesque ultrices mattis odio. Donec vitae nisi. Nam ultrices  libero non mattis pulvinar  nulla pede ullamcorper augue  a suscipit nulla elit ac nulla.
41,Shawn,Kennedy,skennedy@wikido.net,Slovenia,17.112.149.236,3-(622)428-3050,667,Vivamus vel nulla eget eros elementum pellentesque. Quisque porta volutpat erat. Quisque erat eros  viverra eget  congue eget  semper rutrum  nulla.
42,Sara,Pierce,spierce@bubblebox.edu,Slovakia,130.48.227.80,8-(344)571-0419,936,Fusce consequat. Nulla nisl. Nunc nisl. Duis bibendum  felis sed interdum venenatis  turpis enim blandit mi  in porttitor pede justo eu massa. Donec dapibus. Duis at velit eu est congue elementum.
43,Irene,Richardson,irichardson@quamba.edu,Moldova,96.133.5.65,,234,Phasellus sit amet erat. Nulla tempus.
44,Amanda,Bowman,abowman@jaloo.name,Paraguay,154.199.96.21,5-(885)666-8367,254,Proin at turpis a pede posuere nonummy. Integer non velit. Donec diam neque  vestibulum eget  vulputate ut  ultrices vel  augue. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec pharetra  magna vestibulum aliquet ultrices  erat tortor sollicitudin mi  sit amet lobortis sapien sapien non mi. Integer ac neque. Duis bibendum. Morbi non quam nec dui luctus rutrum. Nulla tellus. In sagittis dui vel nisl.
45,Michael,Ford,mford@wikizz.name,Chile,152.247.30.141,3-(877)557-3923,706,Nulla justo. Aliquam quis turpis eget elit sodales scelerisque. Mauris sit amet eros. Suspendisse accumsan tortor quis turpis. Sed ante.
46,Melissa,Kennedy,mkennedy@vinder.org,Martinique,55.121.39.172,9-(711)524-9910,831,Maecenas leo odio  condimentum id  luctus nec  molestie sed  justo. Pellentesque viverra pede ac diam. Cras pellentesque volutpat dui. Maecenas tristique  est et tempus semper  est quam pharetra magna  ac consequat metus sapien ut nunc. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Mauris viverra diam vitae quam. Suspendisse potenti. Nullam porttitor lacus at turpis. Donec posuere metus vitae ipsum.
47,Kathy,Freeman,kfreeman@tagtune.org,Solomon Islands,143.191.202.120,2-(281)797-1538,164,Aenean lectus. Pellentesque eget nunc. Donec quis orci eget orci vehicula condimentum. Curabitur in libero ut massa volutpat convallis. Morbi odio odio  elementum eu  interdum eu  tincidunt in  leo. Maecenas pulvinar lobortis est. Phasellus sit amet erat. Nulla tempus.
48,Roy,Edwards,redwards@linkbridge.edu,Latvia,246.159.90.100,2-(311)084-3008,689,Nunc purus. Phasellus in felis. Donec semper sapien a libero. Nam dui.
49,Patrick,Hughes,phughes@tazzy.gov,Angola,120.238.93.251,7-(202)294-5137,645,Vivamus in felis eu sapien cursus vestibulum. Proin eu mi. Nulla ac enim. In tempor  turpis nec euismod scelerisque  quam turpis adipiscing lorem  vitae mattis nibh ligula nec sem. Duis aliquam convallis nunc. Proin at turpis a pede posuere nonummy. Integer non velit. Donec diam neque  vestibulum eget  vulputate ut  ultrices vel  augue.
50,George,Stone,gstone@gabtune.mil,Venezuela,172.238.29.138,4-(440)853-7427,698,Ut tellus. Nulla ut erat id mauris vulputate elementum. Nullam varius. Nulla facilisi. Cras non velit nec nisi vulputate nonummy. Maecenas tincidunt lacus at velit. Vivamus vel nulla eget eros elementum pellentesque.
"""

/// <summary>
/// Baisc index configuration
/// </summary>
let MockIndexSettings() = 
    let index = new Index()
    index.IndexName <- Guid.NewGuid().ToString("N")
    index.Online <- true
    index.IndexConfiguration.DirectoryType <- DirectoryType.Ram
    index.Fields.Add("firstname", new FieldProperties(FieldType = FieldType.Text))
    index.Fields.Add("lastname", new FieldProperties(FieldType = FieldType.Text))
    index.Fields.Add("email", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("country", new FieldProperties(FieldType = FieldType.Text))
    index.Fields.Add("ipaddress", new FieldProperties(FieldType = FieldType.ExactText))
    index.Fields.Add("cvv2", new FieldProperties(FieldType = FieldType.Int))
    index.Fields.Add("description", new FieldProperties(FieldType = FieldType.Highlight))
    // Computed fields
    index.Fields.Add("fullname", new FieldProperties(FieldType = FieldType.Text, ScriptName = "fullname"))
    index.Scripts.Add
        ("fullname", 
         new ScriptProperties("""return fields["firstname"] + " " + fields["lastname"];""", ScriptType.ComputedField))
    let searchProfileQuery = 
        new SearchQuery(index.IndexName, "firstname = '' AND lastname = '' AND cvv2 = '116' AND country = ''")
    searchProfileQuery.MissingValueConfiguration.Add("firstname", MissingValueOption.ThrowError)
    searchProfileQuery.MissingValueConfiguration.Add("cvv2", MissingValueOption.Default)
    searchProfileQuery.MissingValueConfiguration.Add("topic", MissingValueOption.Ignore)
    index.SearchProfiles.Add("test1", searchProfileQuery)
    index

type Conf = 
    { DocumentationFolder : string
      ApiFile : string }

let DocumentationConf = 
    { DocumentationFolder = @"E:\Python27\Scripts\pelican\Scripts\OneDrive\Sites\documentation\content\posts"
      ApiFile = @"F:\Github\FlexSearch\idl\Api.thrift" }