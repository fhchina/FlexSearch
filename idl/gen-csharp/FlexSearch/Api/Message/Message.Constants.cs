/**
 * Autogenerated by Thrift Compiler (0.9.1)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.IO;
using Thrift;
using Thrift.Collections;
using System.ServiceModel;
using System.Runtime.Serialization;

namespace FlexSearch.Api.Message
{
  public static class MessageConstants
  {
    public static OperationMessage INDEX_NOT_FOUND = new OperationMessage();
    public static OperationMessage INDEX_ALREADY_EXISTS = new OperationMessage();
    public static OperationMessage INDEX_SHOULD_BE_OFFLINE = new OperationMessage();
    public static OperationMessage INDEX_IS_OFFLINE = new OperationMessage();
    public static OperationMessage INDEX_IS_OPENING = new OperationMessage();
    public static OperationMessage INDEX_REGISTERATION_MISSING = new OperationMessage();
    public static OperationMessage INDEXING_DOCUMENT_ID_MISSING = new OperationMessage();
    public static OperationMessage ERROR_OPENING_INDEXWRITER = new OperationMessage();
    public static OperationMessage ERROR_ADDING_INDEX_STATUS = new OperationMessage();
    public static OperationMessage INDEX_IS_ALREADY_ONLINE = new OperationMessage();
    public static OperationMessage INDEX_IS_ALREADY_OFFLINE = new OperationMessage();
    public static OperationMessage INDEX_IS_IN_INVALID_STATE = new OperationMessage();
    public static OperationMessage PROPERTY_CANNOT_BE_EMPTY = new OperationMessage();
    public static OperationMessage REGEX_NOT_MATCHED = new OperationMessage();
    public static OperationMessage VALUE_NOT_IN = new OperationMessage();
    public static OperationMessage VALUE_ONLY_IN = new OperationMessage();
    public static OperationMessage GREATER_THAN_EQUAL_TO = new OperationMessage();
    public static OperationMessage GREATER_THAN = new OperationMessage();
    public static OperationMessage LESS_THAN_EQUAL_TO = new OperationMessage();
    public static OperationMessage LESS_THAN = new OperationMessage();
    public static OperationMessage FILTER_CANNOT_BE_INITIALIZED = new OperationMessage();
    public static OperationMessage FILTER_NOT_FOUND = new OperationMessage();
    public static OperationMessage TOKENIZER_CANNOT_BE_INITIALIZED = new OperationMessage();
    public static OperationMessage TOKENIZER_NOT_FOUND = new OperationMessage();
    public static OperationMessage ATLEAST_ONE_FILTER_REQUIRED = new OperationMessage();
    public static OperationMessage UNKNOWN_FIELD_TYPE = new OperationMessage();
    public static OperationMessage SCRIPT_NOT_FOUND = new OperationMessage();
    public static OperationMessage ANALYZERS_NOT_SUPPORTED_FOR_FIELD_TYPE = new OperationMessage();
    public static OperationMessage UNKNOWN_SCRIPT_TYPE = new OperationMessage();
    public static OperationMessage ANALYZER_NOT_FOUND = new OperationMessage();
    public static OperationMessage SCRIPT_CANT_BE_COMPILED = new OperationMessage();
    public static OperationMessage MODULE_NOT_FOUND = new OperationMessage();
    public static OperationMessage INVALID_QUERY_TYPE = new OperationMessage();
    public static OperationMessage INVALID_FIELD_NAME = new OperationMessage();
    public static OperationMessage MISSING_FIELD_VALUE = new OperationMessage();
    public static OperationMessage MISSING_FIELD_VALUE_1 = new OperationMessage();
    public static OperationMessage UNKNOWN_MISSING_VALUE_OPTION = new OperationMessage();
    public static OperationMessage QUERYSTRING_PARSING_ERROR = new OperationMessage();
    public static OperationMessage DATA_CANNOT_BE_PARSED = new OperationMessage();
    public static OperationMessage QUERY_OPERATOR_FIELD_TYPE_NOT_SUPPORTED = new OperationMessage();
    public static OperationMessage STORED_FIELDS_CANNOT_BE_SEARCHED = new OperationMessage();
    public static OperationMessage SEARCH_PROFILE_NOT_FOUND = new OperationMessage();
    public static OperationMessage NEGATIVE_QUERY_NOT_SUPPORTED = new OperationMessage();
    public static OperationMessage HTTP_UNABLE_TO_PARSE = new OperationMessage();
    public static OperationMessage HTTP_UNSUPPORTED_CONTENT_TYPE = new OperationMessage();
    public static OperationMessage HTTP_NO_BODY_DEFINED = new OperationMessage();
    public static OperationMessage HTTP_NOT_SUPPORTED = new OperationMessage();
    public static OperationMessage HTTP_URI_ID_NOT_SUPPLIED = new OperationMessage();
    public static OperationMessage KEY_NOT_FOUND = new OperationMessage();
    public static OperationMessage IMPORTER_NOT_FOUND = new OperationMessage();
    public static OperationMessage IMPORTER_DOES_NOT_SUPPORT_BULK_INDEXING = new OperationMessage();
    public static OperationMessage IMPORTER_DOES_NOT_SUPPORT_INCREMENTAL_INDEXING = new OperationMessage();
    public static OperationMessage JOBID_IS_NOT_FOUND = new OperationMessage();
    static MessageConstants()
    {
      INDEX_NOT_FOUND.DeveloperMessage = "The requested index does not exist.";
      INDEX_NOT_FOUND.UserMessage = "The requested index does not exist.";
      INDEX_NOT_FOUND.ErrorCode = 1000;
      INDEX_ALREADY_EXISTS.UserMessage = "The requested index already exist.";
      INDEX_ALREADY_EXISTS.ErrorCode = 1002;
      INDEX_ALREADY_EXISTS.DeveloperMessage = "The requested index already exist.";
      INDEX_SHOULD_BE_OFFLINE.ErrorCode = 1003;
      INDEX_SHOULD_BE_OFFLINE.UserMessage = "Index should be made offline before attempting the operation.";
      INDEX_SHOULD_BE_OFFLINE.DeveloperMessage = "Index should be made offline before attempting to update index settings.";
      INDEX_IS_OFFLINE.ErrorCode = 1004;
      INDEX_IS_OFFLINE.UserMessage = "The index is offline or closing. Please bring the index online to use it.";
      INDEX_IS_OFFLINE.DeveloperMessage = "The index is offline or closing. Please bring the index online to use it.";
      INDEX_IS_OPENING.DeveloperMessage = "The index is in opening state. Please wait some time before making another request.";
      INDEX_IS_OPENING.ErrorCode = 1005;
      INDEX_IS_OPENING.UserMessage = "The index is in opening state. Please wait some time before making another request.";
      INDEX_REGISTERATION_MISSING.DeveloperMessage = "Registeration information associated with the index is missing.";
      INDEX_REGISTERATION_MISSING.ErrorCode = 1006;
      INDEX_REGISTERATION_MISSING.UserMessage = "Registeration information associated with the index is missing.";
      INDEXING_DOCUMENT_ID_MISSING.UserMessage = "Document Id is required in order to index an document. Please specify _id and submit the document for indexing.";
      INDEXING_DOCUMENT_ID_MISSING.DeveloperMessage = "Document id missing.";
      INDEXING_DOCUMENT_ID_MISSING.ErrorCode = 1007;
      ERROR_OPENING_INDEXWRITER.UserMessage = "Unable to open index writer.";
      ERROR_OPENING_INDEXWRITER.DeveloperMessage = "{To be populated by the developer code}";
      ERROR_OPENING_INDEXWRITER.ErrorCode = 1008;
      ERROR_ADDING_INDEX_STATUS.DeveloperMessage = "Unable to set the index status.";
      ERROR_ADDING_INDEX_STATUS.ErrorCode = 1009;
      ERROR_ADDING_INDEX_STATUS.UserMessage = "Unable to set the index status.";
      INDEX_IS_ALREADY_ONLINE.ErrorCode = 1010;
      INDEX_IS_ALREADY_ONLINE.DeveloperMessage = "The index is already online or opening at the moment.";
      INDEX_IS_ALREADY_ONLINE.UserMessage = "The index is already online or opening at the moment.";
      INDEX_IS_ALREADY_OFFLINE.DeveloperMessage = "The index is already offline or closing at the moment.";
      INDEX_IS_ALREADY_OFFLINE.ErrorCode = 1011;
      INDEX_IS_ALREADY_OFFLINE.UserMessage = "The index is already offline or closing at the moment.";
      INDEX_IS_IN_INVALID_STATE.DeveloperMessage = "Index is in invalid state.";
      INDEX_IS_IN_INVALID_STATE.ErrorCode = 1012;
      INDEX_IS_IN_INVALID_STATE.UserMessage = "Index is in invalid state.";
      PROPERTY_CANNOT_BE_EMPTY.DeveloperMessage = "Field:{propertyName} cannot be empty.";
      PROPERTY_CANNOT_BE_EMPTY.UserMessage = "Field:{propertyName} cannot be empty.";
      PROPERTY_CANNOT_BE_EMPTY.ErrorCode = 2001;
      REGEX_NOT_MATCHED.ErrorCode = 2002;
      REGEX_NOT_MATCHED.UserMessage = "Field:{propertyName} does not match the regex pattern {value}.";
      REGEX_NOT_MATCHED.DeveloperMessage = "Field:{propertyName} does not match the regex pattern {value}.";
      VALUE_NOT_IN.DeveloperMessage = "Field:{propertyName} cannot have the following as valid values: {value}.";
      VALUE_NOT_IN.UserMessage = "Field:{propertyName} cannot have the following as valid values: {value}.";
      VALUE_NOT_IN.ErrorCode = 2003;
      VALUE_ONLY_IN.UserMessage = "Field:{propertyName} can only have the following as valid values: {value}.";
      VALUE_ONLY_IN.DeveloperMessage = "Field:{propertyName} can only have the following as valid values: {value}.";
      VALUE_ONLY_IN.ErrorCode = 2004;
      GREATER_THAN_EQUAL_TO.DeveloperMessage = "Field:{propertyName} should be greater than or equal to: {value}.";
      GREATER_THAN_EQUAL_TO.UserMessage = "Field:{propertyName} should be greater than or equal to: {value}.";
      GREATER_THAN_EQUAL_TO.ErrorCode = 2005;
      GREATER_THAN.UserMessage = "Field:{propertyName} should be greater than: {value}.";
      GREATER_THAN.ErrorCode = 2006;
      GREATER_THAN.DeveloperMessage = "Field:{propertyName} should be greater than: {value}.";
      LESS_THAN_EQUAL_TO.ErrorCode = 2005;
      LESS_THAN_EQUAL_TO.DeveloperMessage = "Field:{propertyName} should be less than or equal to: {value}.";
      LESS_THAN_EQUAL_TO.UserMessage = "Field:{propertyName} should be less than or equal to: {value}.";
      LESS_THAN.DeveloperMessage = "Field:{propertyName} should be less than: {value}.";
      LESS_THAN.UserMessage = "Field:{propertyName} should be less than: {value}.";
      LESS_THAN.ErrorCode = 2006;
      FILTER_CANNOT_BE_INITIALIZED.UserMessage = "Filter:{propertyName} cannot be initialized.";
      FILTER_CANNOT_BE_INITIALIZED.ErrorCode = 2007;
      FILTER_CANNOT_BE_INITIALIZED.DeveloperMessage = "Filter:{propertyName} cannot be initialized: {value}";
      FILTER_NOT_FOUND.DeveloperMessage = "Filter:{propertyName} not found.";
      FILTER_NOT_FOUND.ErrorCode = 2008;
      FILTER_NOT_FOUND.UserMessage = "Filter:{propertyName} not found.";
      TOKENIZER_CANNOT_BE_INITIALIZED.ErrorCode = 2009;
      TOKENIZER_CANNOT_BE_INITIALIZED.UserMessage = "Tokenizer:{propertyName} cannot be initialized.";
      TOKENIZER_CANNOT_BE_INITIALIZED.DeveloperMessage = "Tokenizer:{propertyName} cannot be initialized: {value}";
      TOKENIZER_NOT_FOUND.DeveloperMessage = "Tokenizer:{propertyName} not found.";
      TOKENIZER_NOT_FOUND.ErrorCode = 2010;
      TOKENIZER_NOT_FOUND.UserMessage = "Tokenizer:{propertyName} not found.";
      ATLEAST_ONE_FILTER_REQUIRED.ErrorCode = 2011;
      ATLEAST_ONE_FILTER_REQUIRED.UserMessage = "Atleast one filter should be specified for a custom analyzer.";
      ATLEAST_ONE_FILTER_REQUIRED.DeveloperMessage = "Atleast one filter should be specified for a custom analyzer.";
      UNKNOWN_FIELD_TYPE.UserMessage = "Unsupported field type specified in the Field Properties.";
      UNKNOWN_FIELD_TYPE.ErrorCode = 2012;
      UNKNOWN_FIELD_TYPE.DeveloperMessage = "Unsupported field type specified in the Field Properties.";
      SCRIPT_NOT_FOUND.DeveloperMessage = "Script{propertyName} not found.";
      SCRIPT_NOT_FOUND.ErrorCode = 2013;
      SCRIPT_NOT_FOUND.UserMessage = "Script{propertyName} not found.";
      ANALYZERS_NOT_SUPPORTED_FOR_FIELD_TYPE.DeveloperMessage = "FieldType:{propertyName} does not support custom analyzer.";
      ANALYZERS_NOT_SUPPORTED_FOR_FIELD_TYPE.ErrorCode = 2014;
      ANALYZERS_NOT_SUPPORTED_FOR_FIELD_TYPE.UserMessage = "FieldType:{propertyName} does not support custom analyzer.";
      UNKNOWN_SCRIPT_TYPE.ErrorCode = 2015;
      UNKNOWN_SCRIPT_TYPE.DeveloperMessage = "ScriptType:{propertyName} is not supported.";
      UNKNOWN_SCRIPT_TYPE.UserMessage = "ScriptType:{propertyName} is not supported.";
      ANALYZER_NOT_FOUND.UserMessage = "Analyzer:{propertyName} not found.";
      ANALYZER_NOT_FOUND.ErrorCode = 2016;
      ANALYZER_NOT_FOUND.DeveloperMessage = "Analyzer:{propertyName} not found.";
      SCRIPT_CANT_BE_COMPILED.ErrorCode = 3000;
      SCRIPT_CANT_BE_COMPILED.UserMessage = "Script:{propertyName} cannot be compiled.";
      SCRIPT_CANT_BE_COMPILED.DeveloperMessage = "Script:{propertyName} cannot be compiled. {value}";
      MODULE_NOT_FOUND.ErrorCode = 4000;
      MODULE_NOT_FOUND.UserMessage = "Module:{propertyName} can not be found. Please make sure all the compiled dependecies are accessible by the server.";
      MODULE_NOT_FOUND.DeveloperMessage = "Module:{propertyName} can not be found. Please make sure all the compiled dependecies are accessible by the server.";
      INVALID_QUERY_TYPE.UserMessage = "QueryType:{propertyName} can not be found. Please make sure all the compiled dependecies are accessible by the server.";
      INVALID_QUERY_TYPE.ErrorCode = 5000;
      INVALID_QUERY_TYPE.DeveloperMessage = "QueryType:{propertyName} can not be found. Please make sure all the compiled dependecies are accessible by the server.";
      INVALID_FIELD_NAME.ErrorCode = 5001;
      INVALID_FIELD_NAME.DeveloperMessage = "FieldName:{propertyName} can not be found.";
      INVALID_FIELD_NAME.UserMessage = "FieldName:{propertyName} can not be found.";
      MISSING_FIELD_VALUE.DeveloperMessage = "Search value canot be empty.";
      MISSING_FIELD_VALUE.UserMessage = "Search value canot be empty. No value provided for the field.";
      MISSING_FIELD_VALUE.ErrorCode = 5002;
      MISSING_FIELD_VALUE_1.DeveloperMessage = "FieldName:{propertyName} No value provided for the field.";
      MISSING_FIELD_VALUE_1.ErrorCode = 5003;
      MISSING_FIELD_VALUE_1.UserMessage = "FieldName:{propertyName} No value provided for the field.";
      UNKNOWN_MISSING_VALUE_OPTION.UserMessage = "MissingValueOption:{propertyName} is not supported.";
      UNKNOWN_MISSING_VALUE_OPTION.ErrorCode = 5004;
      UNKNOWN_MISSING_VALUE_OPTION.DeveloperMessage = "MissingValueOption:{propertyName} is not supported.";
      QUERYSTRING_PARSING_ERROR.DeveloperMessage = "Unable to parse the passed query string. {value}";
      QUERYSTRING_PARSING_ERROR.UserMessage = "Unable to parse the passed query string.";
      QUERYSTRING_PARSING_ERROR.ErrorCode = 5005;
      DATA_CANNOT_BE_PARSED.UserMessage = "Field:{propertyName} The passed data cannot be parsed. Check if the passed data is in the correct format required by the query operator";
      DATA_CANNOT_BE_PARSED.ErrorCode = 5006;
      DATA_CANNOT_BE_PARSED.DeveloperMessage = "Field:{propertyName} The passed data cannot be parsed. Check if the passed data is in the correct format required by the query operator";
      QUERY_OPERATOR_FIELD_TYPE_NOT_SUPPORTED.DeveloperMessage = "Field:{propertyName} Query operator does not support the passed field type.";
      QUERY_OPERATOR_FIELD_TYPE_NOT_SUPPORTED.ErrorCode = 5007;
      QUERY_OPERATOR_FIELD_TYPE_NOT_SUPPORTED.UserMessage = "Field:{propertyName} Query operator does not support the passed field type.";
      STORED_FIELDS_CANNOT_BE_SEARCHED.ErrorCode = 5008;
      STORED_FIELDS_CANNOT_BE_SEARCHED.UserMessage = "Field:{propertyName} is of type stored and cannot be searched.";
      STORED_FIELDS_CANNOT_BE_SEARCHED.DeveloperMessage = "Field:{propertyName} is of type stored and cannot be searched.";
      SEARCH_PROFILE_NOT_FOUND.ErrorCode = 5009;
      SEARCH_PROFILE_NOT_FOUND.DeveloperMessage = "The requested search profile does not exist.";
      SEARCH_PROFILE_NOT_FOUND.UserMessage = "The requested search profile does not exist.";
      NEGATIVE_QUERY_NOT_SUPPORTED.DeveloperMessage = "Purely negative queries (top not query) are not supported.";
      NEGATIVE_QUERY_NOT_SUPPORTED.ErrorCode = 5010;
      NEGATIVE_QUERY_NOT_SUPPORTED.UserMessage = "Purely negative queries (top not query) are not supported.";
      HTTP_UNABLE_TO_PARSE.DeveloperMessage = "The server is unable to parse the request body. {value}";
      HTTP_UNABLE_TO_PARSE.UserMessage = "The server is unable to parse the request body.";
      HTTP_UNABLE_TO_PARSE.ErrorCode = 6000;
      HTTP_UNSUPPORTED_CONTENT_TYPE.UserMessage = "Unsupported content-type.";
      HTTP_UNSUPPORTED_CONTENT_TYPE.DeveloperMessage = "Unsupported content-type.";
      HTTP_UNSUPPORTED_CONTENT_TYPE.ErrorCode = 6001;
      HTTP_NO_BODY_DEFINED.DeveloperMessage = "No body defined.";
      HTTP_NO_BODY_DEFINED.ErrorCode = 6002;
      HTTP_NO_BODY_DEFINED.UserMessage = "Expecting body. But no body defined.";
      HTTP_NOT_SUPPORTED.UserMessage = "The request Uri endpoint is not supported.";
      HTTP_NOT_SUPPORTED.DeveloperMessage = "The request Uri endpoint is not supported.";
      HTTP_NOT_SUPPORTED.ErrorCode = 6003;
      HTTP_URI_ID_NOT_SUPPLIED.DeveloperMessage = "The request URI expects an id to be supplied as a part of URI.";
      HTTP_URI_ID_NOT_SUPPLIED.UserMessage = "The request URI expects an id to be supplied as a part of URI.";
      HTTP_URI_ID_NOT_SUPPLIED.ErrorCode = 6004;
      KEY_NOT_FOUND.ErrorCode = 7001;
      KEY_NOT_FOUND.UserMessage = "The requested key is not present in the persistences store.";
      KEY_NOT_FOUND.DeveloperMessage = "The requested key is not present in the persistences store.";
      IMPORTER_NOT_FOUND.ErrorCode = 8001;
      IMPORTER_NOT_FOUND.DeveloperMessage = "The requested importer does not exist.";
      IMPORTER_NOT_FOUND.UserMessage = "The requested importer does not exist.";
      IMPORTER_DOES_NOT_SUPPORT_BULK_INDEXING.DeveloperMessage = "The requested importer does not support bulk indexing.";
      IMPORTER_DOES_NOT_SUPPORT_BULK_INDEXING.UserMessage = "The requested importer does not support bulk indexing";
      IMPORTER_DOES_NOT_SUPPORT_BULK_INDEXING.ErrorCode = 8002;
      IMPORTER_DOES_NOT_SUPPORT_INCREMENTAL_INDEXING.DeveloperMessage = "The requested importer does not support incremental indexing.";
      IMPORTER_DOES_NOT_SUPPORT_INCREMENTAL_INDEXING.UserMessage = "The requested importer does not support incremental indexing";
      IMPORTER_DOES_NOT_SUPPORT_INCREMENTAL_INDEXING.ErrorCode = 8003;
      JOBID_IS_NOT_FOUND.UserMessage = "The requested job id does not exist.";
      JOBID_IS_NOT_FOUND.ErrorCode = 8004;
      JOBID_IS_NOT_FOUND.DeveloperMessage = "The requested job id does not exist.";
    }
  }
}
