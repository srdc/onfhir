package io.onfhir

import io.onfhir.api.util.FHIRUtil
import io.onfhir.config.OnfhirConfig
import org.json4s.JsonAST.JObject

import scala.collection.immutable._

/**
  * FHIR related constants and important value sets used in the implementation
  */
package object api {
  /**
    * Our representation of FHIR Content as Jackson parsed hash map
    */
  //type Resource = mutable.LinkedHashMap[String, Any]
  type Resource = JObject

  val ONFHIR_CODE_SYSTEM = "http://onfhir.io"

  /**
    * Default FHIR Version
    */
  val DEFAULT_FHIR_VERSION = "r4"

  /**
    * Default Root Folder for FHIR infrastructure resource configurations
    */
  var DEFAULT_ROOT_FOLDER:Option[String] = None

  /**
    * Names of main FHIR versions to be used within configurations
    */
  object MAIN_FHIR_VERSIONS {
    val DSTU2 = Set("dstu2", "stu2")
    val STU3 = Set("dstu3", "stu3")
    val R4 = Set("r4", "R4")
  }

  /**
    * Root URL for base FHIR definitions
    */
  val FHIR_ROOT_URL_FOR_DEFINITIONS = "http://hl7.org/fhir"

  val FHIR_URL_FOR_EXTERNAL_FHIR_CODE_SYSTEMS = "http://terminology.hl7.org"

  /**
    * Default id of the Conformance [DSTU2] or CapabilityStatement [STU3 or above] resource held in the db, and returned for "metadata" queries
    */
  val SERVER_CONFORMANCE_STATEMENT_ID = "server-conf-statement"

  /**
    * Default paths for the infrastructure resources to configure the platform
    */
  object DEFAULT_RESOURCE_PATHS {
    def BASE_DEFINITONS:String = FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER, s"definitions${FOUNDATION_RESOURCES_FILE_SUFFIX}.zip")
    def PROFILES_FOLDER:String = FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER, "profiles.zip")
    def CONFORMANCE_PATH:String = FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER, s"conformance-statement$FOUNDATION_RESOURCES_FILE_SUFFIX")
    def SEARCH_PARAMETER:String = FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER,  "search-parameters.zip")
    def COMPARTMENTS_PATH:String = FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER , "compartments.zip")
    def VALUESETS_PATH:String = FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER, "value-sets.zip")
    def CODESYSTEMS_PATH:String = FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER, "code-systems.zip")
    def OPDEFS_PATH:String =  FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER,"operation-definitions.zip")
    def INDEX_CONF_PATH:String = FHIRUtil.mergeFilePath(DEFAULT_ROOT_FOLDER,"db-index-conf.json")
  }

  /**
    * Indicates the file extension for Conformance statement
    */
  var FOUNDATION_RESOURCES_FILE_SUFFIX = ".json"

  /**
    * Different types of FHIR Bundle See https://www.hl7.org/fhir/valueset-bundle-type.html
    */
  object FHIR_BUNDLE_TYPES {
    val BUNDLE = "Bundle"
    val HISTORY = "history"
    val SEARCH_SET  = "searchset"
    val TRANSACTION = "transaction"
    val BATCH = "batch"
    val TRANSACTION_RESPONSE = "transaction-response"
    val BATCH_RESPONSE = "batch-response"
    val DOCUMENT = "document"
    val MESSAGE =  "message"
    val COLLECTION = "collection"
  }

  /**
    * Search Entry Modes with bundle See https://www.hl7.org/fhir/valueset-search-entry-mode.html
    */
  object FHIR_BUNDLE_ENTRY_TYPES {
    val MATCH = "match"
    val INCLUDE = "include"
    val OUTCOME = "outcome"
  }

  /**
    * Fields used while creating the resource bundle
    */
  object FHIR_BUNDLE_FIELDS {
    val METHOD = "method"
    val LINK = "link"
    val ETAG = "etag"
    val ENTRY = "entry"
    val MODE = "mode"
    val FULL_URL = "fullUrl"
    val URL = "url"
    val RESOURCE = "resource"
    val REQUEST = "request"
    val RESPONSE = "response"
    val SEARCH = "search"
    val TOTAL = "total"
    val LAST_MODIIFED = "lastModified"
    val SELF_LINK = "self"
    val FIRST_LINK = "first"
    val NEXT_LINK = "next"
    val PREVIOUS_LINK = "previous"
    val LAST_LINK = "last"
    val RELATION = "relation"
    val STATUS = "status"
  }

  /**
    * FHIR HTTP Method names to be used in the Bundle resource to indicate method
    */
  object FHIR_METHOD_NAMES {
    val METHOD_POST = "POST"
    val METHOD_PUT = "PUT"
    val METHOD_GET = "GET"
    val METHOD_DELETE = "DELETE"
  }

  /**
    * Common resource fields used for search and other purposes
    */
  object FHIR_COMMON_FIELDS {
    val RESOURCE = "resource"
    val PARAMETER = "parameter"
    val EXTENSION = "extension"
    val NAME = "name"
    val RESOURCE_TYPE = "resourceType"
    val METHOD = "method"
    val ENTRY = "entry"
    val TYPE = "type"
    val URL = "url"
    val XPATH = "xpath"
    // Base resource fields
    val TAG = "tag"
    val SECURITY = "security"
    val LAST_UPDATED = "lastUpdated"
    val META = "meta"
    val PROFILE = "profile"
    val MONGO_ID = "_id"
    val ID = "id"
    val VERSION_ID = "versionId"
    val VERSION = "version"
    val LANGUAGE= "language"

    // Inner Search Fields
    val CODE = "code"
    val CODING = "coding"
    val SYSTEM = "system"
    val CODEABLE_SYSTEM = s"$CODING.$SYSTEM"
    val CODEABLE_CODE = s"$CODING.$CODE"
    val USE = "use"
    val VALUE = "value"
    val UNIT = "unit"
    val TEXT = "text"
    val DISPLAY = "display"
    val REFERENCE = "reference"
    val START = "start"
    val END = "end"
    val HIGH = "high"
    val LOW = "low"
    val REPEAT = "repeat"
    val BOUNDS_PERIOD = "boundsPeriod"
    val ORIGIN = "origin"
    val LOWER_LIMIT = "lowerLimit"
    val UPPER_LIMIT = "upperLimit"
    val EVENT = "event"
    val IDENTIFIER = "identifier"
    val CURRENCY = "currency"
  }

  /**
    * Our own fields for resource management as described in FHIR version management (Should start with 2 underscores)
    * Warning! remove this fields before presenting to user
    */
  object FHIR_EXTRA_FIELDS {
    //Indicate that the resource is current version (latest version)
    val CURRENT = "__current"
    //The method that the resource is created
    val METHOD = "__method"
    //The HTTP status code returned, for the operation that creates this version of resource
    val STATUS_CODE ="__scode"

    // Prefix for generated fields for Date/DateTime fields to also keep their original formats before converting them to MongoDB time formats
    val TIME_ORIGINAL = "__original" //Original string given in FHIR date/dateTime/Instant
    val TIME_TIMESTAMP = "__ts" // Conversion of FHIR value to Timestamp
    val TIME_DATE = "__date" // Date part of the FHIR value
    val TIME_RANGE_START = "__rangeStart" //If the given FHIR value indicates a range, the start of that range; e.g. 2019-02-01 --> 2019-02-01T00:00:00Z
    val TIME_RANGE_END = "__rangeEnd" //If the given FHIR value indicates a range,the end of that range; e.g. 2019-02-01 --> 2019-02-01T23:59:59Z

    //Extra fields within FHIR reference elements
    val REFERENCE_URL = "__url" // URL part of the reference
    val REFERENCE_RESOURCE_TYPE = "__rtype" // Target resource type that is refered
    val REFERENCE_RESOURCE_ID  = "__rid" // Referenced resource id
    val REFERENCE_RESOURCE_VERSION = "__rversion" //If a specific verison is referred, the version itself
  }

  /**
    * Extra fields used in onFhir within the documents for management of documents
    */
  val ONFHIR_EXTRA_FIELDS = Set(FHIR_COMMON_FIELDS.MONGO_ID, /*FHIR_EXTRA_FIELDS.CURRENT,*/ FHIR_EXTRA_FIELDS.METHOD, FHIR_EXTRA_FIELDS.STATUS_CODE)

  /**
    * Mandatory fields for all resources to exist inside a returned result
    */
  val FHIR_MANDATORY_ELEMENTS = Set(FHIR_COMMON_FIELDS.RESOURCE_TYPE, FHIR_COMMON_FIELDS.ID, FHIR_COMMON_FIELDS.META)

  /**
    * Constants that represents search parameter types of the fhir. See https://www.hl7.org/fhir/search.html
    */
  object FHIR_PARAMETER_TYPES {
    val NUMBER = "number"
    val DATE = "date"
    val STRING = "string"
    val URI = "uri"
    val TOKEN = "token"
    val QUANTITY = "quantity"
    val REFERENCE = "reference"
    val COMPOSITE = "composite"
  }

  /**
    * A high level categorization of the parameters for onFhir
    */
  object FHIR_PARAMETER_CATEGORIES {
    val NORMAL = "normal" //All other normal parameter types
    val RESULT = "result" //FHIR search result parameters
    val SPECIAL = "special" // Special search parameters in FHIR e.g. _id, _list, _filter, etc
    val CHAINED = "chained" // For chained search in FHIR
    val REVCHAINED = "revchained" //For _has parameter in FHIR
    val COMPARTMENT = "compartment" //Indicates a Compartment search
  }

  /**
    * FHIR special search parameter names
    */
  object FHIR_SEARCH_SPECIAL_PARAMETERS {
    val HAS = "_has"
    val LIST = "_list"
    val FILTER = "_filter"
    val ID = "_id" //This is only special for onFHIR
    val QUERY = "_query"
    val TEXT = "_text"
    val CONTENT = "_content"
  }


  /**
    * Primitive and Complex data types defined in FHIR. See https://www.hl7.org/fhir/datatypes.html
    */
  object FHIR_DATA_TYPES {
    val INTEGER = "integer"
    val DECIMAL = "decimal"
    val DATETIME = "dateTime"
    val DATE = "date"
    val TIME = "time"
    val INSTANT = "instant"
    val STRING = "string"
    val BOOLEAN = "boolean"
    val URI = "uri"
    val CANONICAL = "canonical"
    val URL = "url"
    val BASE64BINARY = "base64Binary"
    val CODE = "code"
    val OID = "oid"
    val UUID = "uuid"
    val ID = "id"
    val MARKDOWN = "markdown"
    val UNSIGNEDINT = "unsignedInt"
    val POSITIVEINT = "positiveInt"
    val XHTML = "xhtml"
    //Complex types
    //Special purpose Types
    val ELEMENT = "Element"
    val RESOURCE = "Resource"
    val PARAMETERS = "Parameters"
    val META = "Meta"
    val REFERENCE = "Reference"
    val EXTENSION = "Extension"
    val NARRATIVE = "Narrative"
    val BACKBONE = "BackboneElement"
    val DOSAGE = "Dosage"
    val ELEMENT_DEFINITION = "ElementDefinition"
    //MetadataTypes
    val CONTACT_DETAIL = "ContactDetail"
    val USAGE_CONTEXT = "UsageContext"
    val DATA_REQUIREMENT = "DataRequirement"
    val CONTRIBUTOR = "Contributor"
    val RELATED_ARTIFACT = "RelatedArtifact"
    val PARAMETER_DEFINITION = "ParameterDefinition"
    val TRIGGER_DEFINITION = "TriggerDefinition"
    val EXPRESSION= "Expression"
    //General Purpose
    val RANGE = "Range"
    val PERIOD = "Period"
    val TIMING = "Timing"
    val RATIO = "Ratio"
    val SAMPLED_DATA = "SampledData"
    val ATTACHMENT = "Attachment"
    val CODING = "Coding"
    val CODEABLE_CONCEPT = "CodeableConcept"
    val HUMAN_NAME = "HumanName"
    val ADDRESS = "Address"
    val CONTACT_POINT = "ContactPoint"
    val SIGNATURE= "Signature"
    val IDENTIFIER = "Identifier"
    val ANNOTATION = "Annotation"
    val QUANTITY = "Quantity"
    val AGE = "Age"
    val COUNT = "Count"
    val DISTANCE = "Distance"
    val DURATION = "Duration"
    val MONEY = "Money"
    val SIMPLE_QUANTITY = "SimpleQuantity"
    val MONEY_QUANTITY = "MoneyQuantity"
  }

  /**
    * Fhir search result parameter names. See Search Control Parameters in https://www.hl7.org/fhir/search.html
    */
  object FHIR_SEARCH_RESULT_PARAMETERS {
    val ELEMENTS = "_elements"
    val SUMMARY = "_summary"
    val COUNT = "_count"
    val TOTAL = "_total"
    val PAGE = "_page"
    val SORT = "_sort"
    val INCLUDE = "_include"
    val REVINCLUDE = "_revinclude"
    val CONTAINED = "_contained"
    val CONTAINED_TYPE = "_containedType"
    //Only for history
    val SINCE = "_since"
    val AT = "_at"
  }

  /**
    * Possible values for FHIR _summary search parameter See https://www.hl7.org/fhir/search.html#summary
    */
  object FHIR_SUMMARY_OPTIONS {
    val TRUE = "true"
    val TEXT = "text"
    val DATA = "data"
    val COUNT = "count"
    val FALSE = "false"
  }

  /**
    * Possible values for FHIR CapabilityStatement.rest.resource.versioning to indicate versioning mechanism to support
    */
  object FHIR_VERSIONING_OPTIONS {
     val NO_VERSION = "no-version" //Versioning not supported for the resource type
     val VERSIONED = "versioned" //Versioning supported for the resource type
     val VERSIONED_UPDATE = "versioned-update" //Versioning supported for the resource type and for updates client is required to give version
  }

  /**
    * Prefixes and modifiers used in search in fhir. See https://www.hl7.org/fhir/search.html
    */
  object FHIR_PREFIXES_MODIFIERS {
    // Modifiers
    val MISSING = ":missing"
    val EXACT = ":exact"
    val CONTAINS = ":contains"
    val ABOVE = ":above"
    val BELOW = ":below"
    val TEXT = ":text"
    val NOT = ":not"
    val TYPE = ":type"
    val IDENTIFIER = ":identifier"
    val ASCENDING_OLD = ":asc"
    val DESCENDING_OLD = ":desc"
    val IN = ":in"
    val NOT_IN = ":not-in"
    val OF_TYPE = ":of-type"
    // onFHIR specific extension for handling starts-with queries in coded fields
    val STARTS_WITH = ":sw"
    val NOT_STARTS_WITH=":nsw"

    // Prefixes
    val DESCENDING="-"
    val BLANK_EQUAL = ""
    val EQUAL = "eq"
    val GREATER_THAN =  "gt"
    val GREATER_THAN_M = ">"
    val LESS_THAN = "lt"
    val LESS_THAN_M = "<"
    val GREATER_THAN_EQUAL = "ge"
    val LESS_THAN_EQUAL = "le"
    val NOT_EQUAL = "ne"
    val STARTS_AFTER = "sa"
    val ENDS_BEFORE = "eb"
    val APPROXIMATE = "ap"
  }


  /**
    * Http Header names or other options for the FHIR
    */
  object FHIR_HTTP_OPTIONS {
    val FHIR_RETURN_MINIMAL = "return=minimal"
    val FHIR_RETURN_REPRESENTATION = "return=representation"
    val FHIR_RETURN_OPERATION_OUTCOME = "return=OperationOutcome"
    val FHIR_SEARCH_LENIENT = "handling=lenient"
    val FHIR_SEARCH_STRICT = "handling=strict"
    val FORMAT  = "_format"
    val SEARCH = "_search"
    val HISTORY = "_history"
    val IF_NONE_EXIST = "If-None-Exist"
    val rIF_NONE_EXIST = "ifNoneExist"
    val IF_MATCH = "If-Match"
    val rIF_MATCH = "ifMatch"
    val IF_NONE_MATCH = "If-None-Match"
    val rIF_NONE_MATCH = "ifNoneMatch"
    val IF_MODIFIED_SINCE = "If-Modified-Since"
    val rIF_MODIFIED_SINCE = "ifModifiedSince"
    val PREFER = "Prefer"
    val rPREFER = "prefer"
    val CONTENT_TYPE ="Content-Type"

  }

  /**
    * Implemented fhir restful operations
    */
  object FHIR_API_SERVICES {
    val BATCH = "FHIRBatchService"
    val CREATE = "FHIRCreateService"
    val DELETE = "FHIRDeleteService"
    val HISTORY = "FHIRHistoryService"
    val READ = "FHIRReadService"
    val UPDATE = "FHIRUpdateService"
    val SEARCH = "FHIRSearchService"
    val BASE_OPERATIONS = "FHIRBaseOperations"
    val PATCH = "FHIRPatchService"
  }

  /**
    * Possible values for FHIR missing modifier See https://www.hl7.org/fhir/search.html#modifiers
    */
  object MISSING_MODIFIER_VALUES {
    val STRING_TRUE = "true"
    val STRING_FALSE = "false"
  }

  /** Mandatory Summary Fields (Will be revised) */
  val FHIR_MANDATORY_SUMMARY_FIELDS = Seq(
    FHIR_COMMON_FIELDS.RESOURCE_TYPE,
    FHIR_COMMON_FIELDS.ID,
    FHIR_COMMON_FIELDS.META,
    FHIR_EXTRA_FIELDS.METHOD,
    FHIR_EXTRA_FIELDS.STATUS_CODE
  )


  /**
    * Allowed target types for Search Parameter types
    */
  val FHIR_PARAMETER_TYPE_TARGETS = scala.collection.immutable.Map(
    FHIR_PARAMETER_TYPES.DATE -> Set(FHIR_DATA_TYPES.DATE, FHIR_DATA_TYPES.DATETIME, FHIR_DATA_TYPES.INSTANT, FHIR_DATA_TYPES.PERIOD, FHIR_DATA_TYPES.TIMING),
    FHIR_PARAMETER_TYPES.URI -> Set(FHIR_DATA_TYPES.URI, FHIR_DATA_TYPES.URL, FHIR_DATA_TYPES.STRING, FHIR_DATA_TYPES.CANONICAL, FHIR_DATA_TYPES.UUID, FHIR_DATA_TYPES.OID),
    FHIR_PARAMETER_TYPES.STRING -> Set(FHIR_DATA_TYPES.STRING, FHIR_DATA_TYPES.HUMAN_NAME, FHIR_DATA_TYPES.ADDRESS, FHIR_DATA_TYPES.MARKDOWN),
    FHIR_PARAMETER_TYPES.TOKEN -> Set(FHIR_DATA_TYPES.CODEABLE_CONCEPT, FHIR_DATA_TYPES.CODING, FHIR_DATA_TYPES.CODE, FHIR_DATA_TYPES.IDENTIFIER, FHIR_DATA_TYPES.CONTACT_POINT, FHIR_DATA_TYPES.QUANTITY, FHIR_DATA_TYPES.ID, FHIR_DATA_TYPES.BOOLEAN, FHIR_DATA_TYPES.STRING, FHIR_DATA_TYPES.STRING),
    FHIR_PARAMETER_TYPES.REFERENCE -> Set(FHIR_DATA_TYPES.REFERENCE, FHIR_DATA_TYPES.CANONICAL, FHIR_DATA_TYPES.RESOURCE),
    FHIR_PARAMETER_TYPES.NUMBER -> Set(FHIR_DATA_TYPES.INTEGER, FHIR_DATA_TYPES.DECIMAL, FHIR_DATA_TYPES.RANGE, FHIR_DATA_TYPES.POSITIVEINT, FHIR_DATA_TYPES.UNSIGNEDINT),
    FHIR_PARAMETER_TYPES.QUANTITY -> Set(FHIR_DATA_TYPES.QUANTITY, FHIR_DATA_TYPES.SIMPLE_QUANTITY, FHIR_DATA_TYPES.MONEY, FHIR_DATA_TYPES.MONEY_QUANTITY, FHIR_DATA_TYPES.COUNT, FHIR_DATA_TYPES.DURATION, FHIR_DATA_TYPES.DISTANCE, FHIR_DATA_TYPES.AGE, FHIR_DATA_TYPES.RANGE, FHIR_DATA_TYPES.SAMPLED_DATA)
  )

  /**
    * Paths to the string type search parameter for complex types
    */
  val STRING_TYPE_SEARCH_SUBPATHS = scala.collection.immutable.Map(
    FHIR_DATA_TYPES.HUMAN_NAME -> Seq("family", "given", "text"),
    FHIR_DATA_TYPES.ADDRESS -> Seq("line", "city", "state", "country", "postalCode", "text")
  )

  /**
    * Paths to the system and code fields for each Data Type
    */
  val TOKEN_TYPE_SEARCH_SUBPATHS = scala.collection.immutable.Map(
    FHIR_DATA_TYPES.CODEABLE_CONCEPT -> (FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE),
    FHIR_DATA_TYPES.CODING -> (FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.CODE),
    FHIR_DATA_TYPES.IDENTIFIER -> (FHIR_COMMON_FIELDS.SYSTEM, FHIR_COMMON_FIELDS.VALUE),
    FHIR_DATA_TYPES.CONTACT_POINT ->  (FHIR_COMMON_FIELDS.USE, FHIR_COMMON_FIELDS.VALUE)
  )

  /**
    * Paths to the text part for the token Data Types
    */
  val TOKEN_TYPE_SEARCH_DISPLAY_PATHS = scala.collection.immutable.Map(
    FHIR_DATA_TYPES.CODEABLE_CONCEPT -> Seq(FHIR_COMMON_FIELDS.TEXT, s"${FHIR_COMMON_FIELDS.CODING}.${FHIR_COMMON_FIELDS.DISPLAY}"),
    FHIR_DATA_TYPES.CODING -> Seq(FHIR_COMMON_FIELDS.DISPLAY),
    FHIR_DATA_TYPES.IDENTIFIER -> Seq(s"${FHIR_COMMON_FIELDS.TYPE}.${FHIR_COMMON_FIELDS.TEXT}")
  )

  /**
    * language_override parameter name for all text indices in MongoDB.
    * The default parameter "language" needs to be changed as it can appear in any FHIR Resource.
    */
  val INDEX_LANGUAGE_OVERRIDE = "language"

  /**
    * INDEX subpaths for FHIR Complex Datatypes
    *   - String --> Simple Indexes
    *   - (String, String) --> Compound index
    */
  val INDEX_SUBPATHS = scala.collection.immutable.Map(
    // token type parameters targets
    FHIR_DATA_TYPES.CODEABLE_CONCEPT ->     Seq(s".${FHIR_COMMON_FIELDS.CODEABLE_CODE}"),
    FHIR_DATA_TYPES.CODING ->               Seq(s".${FHIR_COMMON_FIELDS.CODE}"),
    FHIR_DATA_TYPES.IDENTIFIER ->           Seq(s".${FHIR_COMMON_FIELDS.VALUE}"),
    FHIR_DATA_TYPES.CONTACT_POINT ->        Seq( s".${FHIR_COMMON_FIELDS.VALUE}"),
    // date type parameters targets
    FHIR_DATA_TYPES.DATE ->                 Seq((s".${FHIR_EXTRA_FIELDS.TIME_RANGE_START}", s".${FHIR_EXTRA_FIELDS.TIME_RANGE_END}")), //For dates we are using these
    FHIR_DATA_TYPES.DATETIME ->             Seq((s".${FHIR_EXTRA_FIELDS.TIME_RANGE_START}", s".${FHIR_EXTRA_FIELDS.TIME_RANGE_END}"), s".${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}", s".${FHIR_EXTRA_FIELDS.TIME_DATE}"), //all are possible
    FHIR_DATA_TYPES.INSTANT ->              Seq(s".${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}",  s".${FHIR_EXTRA_FIELDS.TIME_DATE}"), //all are possible
    FHIR_DATA_TYPES.PERIOD ->               Seq(s".${FHIR_COMMON_FIELDS.START}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}", s".${FHIR_COMMON_FIELDS.END}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
    FHIR_DATA_TYPES.TIMING ->               Seq(s".${FHIR_COMMON_FIELDS.EVENT}", s".${FHIR_COMMON_FIELDS.BOUNDS_PERIOD}.${FHIR_COMMON_FIELDS.START}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}", s".${FHIR_COMMON_FIELDS.BOUNDS_PERIOD}.${FHIR_COMMON_FIELDS.END}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
    // reference type parameters
    FHIR_DATA_TYPES.REFERENCE ->            Seq(s".${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID}"), //Only index the referenced resource id
    // quantity type parameters
    FHIR_DATA_TYPES.QUANTITY ->             Seq(s".${FHIR_COMMON_FIELDS.VALUE}"),
    FHIR_DATA_TYPES.SAMPLED_DATA ->         Seq(s".${FHIR_COMMON_FIELDS.LOWER_LIMIT}", s".${FHIR_COMMON_FIELDS.UPPER_LIMIT}"),
    // string type parameters
    FHIR_DATA_TYPES.HUMAN_NAME ->           Seq(".family", ".given", ".text"),
    FHIR_DATA_TYPES.ADDRESS ->              Seq(".line", ".city", ".state", ".country", ".postalCode", ".text"),
    // number type parameters
    FHIR_DATA_TYPES.RANGE ->                Seq(s".${FHIR_COMMON_FIELDS.LOW}", s".${FHIR_COMMON_FIELDS.HIGH}")
  )

  /**
    * Subpaths for sorting on the parameters that targets the FHIR complex types
    */
  val SORTING_SUBPATHS = scala.collection.immutable.Map(
    FHIR_DATA_TYPES.CODEABLE_CONCEPT ->     Seq(s".${FHIR_COMMON_FIELDS.CODEABLE_CODE}"),
    FHIR_DATA_TYPES.CODING ->               Seq(s".${FHIR_COMMON_FIELDS.CODE}"),
    FHIR_DATA_TYPES.IDENTIFIER ->           Seq(s".${FHIR_COMMON_FIELDS.VALUE}"),
    FHIR_DATA_TYPES.CONTACT_POINT ->        Seq( s".${FHIR_COMMON_FIELDS.VALUE}"),
    // date type parameters targets
    FHIR_DATA_TYPES.DATE ->                 Seq(s".${FHIR_EXTRA_FIELDS.TIME_RANGE_START}"), //For dates we are using these
    FHIR_DATA_TYPES.DATETIME ->             Seq(s".${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}", s".${FHIR_EXTRA_FIELDS.TIME_RANGE_START}"), //all are possible
    FHIR_DATA_TYPES.INSTANT ->              Seq(s".${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"), //all are possible
    FHIR_DATA_TYPES.PERIOD ->               Seq(s".${FHIR_COMMON_FIELDS.START}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}", s".${FHIR_COMMON_FIELDS.END}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
    FHIR_DATA_TYPES.TIMING ->               Seq(s".${FHIR_COMMON_FIELDS.BOUNDS_PERIOD}.${FHIR_COMMON_FIELDS.START}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}", s".${FHIR_COMMON_FIELDS.BOUNDS_PERIOD}.${FHIR_COMMON_FIELDS.END}.${FHIR_EXTRA_FIELDS.TIME_TIMESTAMP}"),
    // reference type parameters
    FHIR_DATA_TYPES.REFERENCE ->            Seq(s".${FHIR_COMMON_FIELDS.REFERENCE}.${FHIR_EXTRA_FIELDS.REFERENCE_RESOURCE_ID}"), //Only index the referenced resource id
    // quantity type parameters
    FHIR_DATA_TYPES.QUANTITY ->             Seq(s".${FHIR_COMMON_FIELDS.VALUE}"),
    FHIR_DATA_TYPES.SAMPLED_DATA ->         Seq(s".${FHIR_COMMON_FIELDS.LOWER_LIMIT}", s".${FHIR_COMMON_FIELDS.UPPER_LIMIT}"),
    // string type parameters
    FHIR_DATA_TYPES.HUMAN_NAME ->           Seq(".family", ".text"),
    FHIR_DATA_TYPES.ADDRESS ->              Seq(".text"),
    // number type parameters
    FHIR_DATA_TYPES.RANGE ->                Seq(s".${FHIR_COMMON_FIELDS.LOW}", s".${FHIR_COMMON_FIELDS.HIGH}")
  )

  /**
    * Number of days in Months
     */
  val MONTH_DAY_MAP = HashMap(
    1 -> "-31", 2 -> "-28", 3 -> "-31",
    4 -> "-30", 5 -> "-31", 6 -> "-30",
    7 -> "-31", 8 -> "-31", 9 -> "-30",
    10 -> "-31", 11 -> "-30", 12 -> "-31"
  )

  /**
    * FHIR Interaction names
    */
  object FHIR_INTERACTIONS {
    val CREATE = "create"
    val UPDATE = "update"
    val DELETE = "delete"
    val SEARCH = "search-type"
    val READ = "read"
    val VREAD = "vread"
    val HISTORY_TYPE="history-type"
    val HISTORY_INSTANCE="history-instance"
    val HISTORY_SYSTEM = "history-system"
    val CAPABILITIES = "capabilities"
    val PATCH = "patch"
    val SEARCH_SYSTEM = "search-system"
    val TRANSACTION = "transaction"
    val BATCH = "batch"
    val UNKNOWN = "unknown"
  }

  /**
    * FHIR Default Operation Names
    */
  object FHIR_OPERATIONS {
    val VALIDATION = "$validate"
  }

  /**
    * Resource Content Validation alternativees for configuration
    */
  object FHIR_VALIDATION_ALTERNATIVES {
    val BASE = "base" // Base FHIR validation
    val PROFILE = "profile" //Profile specific validation
    val NONE = "none" // No validation
  }

  /**
   * FHIR Subscription channel type
   */
  object SubscriptionChannelTypes {
    val WebSocket = "websocket"
    val RestHook = "rest-hook"
    val Email = "email"
    val Sms = "sms"
    val Message = "message"
  }

  val SUPPORTED_SUBSCRIPTION_CHANNELS = Set(SubscriptionChannelTypes.WebSocket, SubscriptionChannelTypes.RestHook)

  object SubscriptionStatusCodes {
    val error = "error"
    val active = "active"
    val requested = "requested"
    val off = "off"
  }

  /**
    * FHIR Operations that are defined by FHIR and implemented in default within onFhir
    */
  val DEFAULT_IMPLEMENTED_FHIR_OPERATIONS:Map[String, String] =
    Map(
      "http://hl7.org/fhir/OperationDefinition/Resource-meta" -> "io.onfhir.operation.MetaOperationHandler",
      "http://hl7.org/fhir/OperationDefinition/Resource-meta-add" -> "io.onfhir.operation.MetaOperationHandler",
      "http://hl7.org/fhir/OperationDefinition/Resource-meta-delete" -> "io.onfhir.operation.MetaOperationHandler",
      "http://hl7.org/fhir/OperationDefinition/Resource-validate" -> "io.onfhir.operation.ValidationOperationHandler",
      "http://hl7.org/fhir/OperationDefinition/ValueSet-expand" -> "io.onfhir.operation.ExpandOperationHandler",
      "http://hl7.org/fhir/OperationDefinition/Composition-document" -> "io.onfhir.operation.DocumentOperationHandler",
      "http://hl7.org/fhir/OperationDefinition/Observation-lastn" -> "io.onfhir.operation.LastNObservationOperationHandler"
    )


  /**
    *  General Authorization Mechanisms supported (scopes, access token parameters, etc)
    *  Smart on FHIR --> see http://docs.smarthealthit.org/authorization/
    *  ?
    */
  final val AUTHZ_METHOD_FHIR_ON_SMART = "smart-on-fhir"
  final val AUTHZ_METHOD_NONE = "none"

  /**
    * General token resolution mechanisms
    * JWT --> Enforce policy directly by validating JWT and using its contents without introspection
    * INTROSPECTION --> Use OAuth2 introspection to validate the access tokens and enforce policy based on the introspection response
    * JWT-INTROSPECTION --> Access tokens will be JWT but still need introspection
    */
  final val TOKEN_RESOLUTION_JWT = "jwt"
  final val TOKEN_RESOLUTION_INTROSPECTION = "introspection"
  final val TOKEN_RESOLUTION_JWT_WITH_INTROSPECTION = "jwt-introspection"

  /**
    * Authorization Server Discovery methods
    */
  final val AUTHZ_DISCOVERY_NONE = "none"
  final val AUTHZ_DISCOVERY_OAUTH = "oauth2"
  final val AUTHZ_DISCOVERY_OPENID_CONNECT = "oidc"

}

