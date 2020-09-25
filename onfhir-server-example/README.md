# Setting up a tailored secure FHIR repository by using onFHIR.io

In this example project, we try to demonstrate how onFHIR.io can be used to setup a secure FHIR repository that meets 
your requirements. This document and sample project try to answer the following questions where each of them is detailed in sub sections;
* How can I configure onFHIR.io to support my FHIR profiles, value sets and code systems?
* Can I make my server support a search parameter not defined in the base standard easily?   

Before going into details, onFHIR.io is configured with a root configuration file called "application.conf", which you 
can use the one for this sample project and change according to your needs (under src/main/resources). The details of 
the configurations will be given in the related sections.

## Configuring onFHIR with requested FHIR capabilities and custom models
Assume that you need to set up a secure FHIR repository in UK for your healthcare ecosystem. You are required to conform 
to the national FHIR profiles, UK Core Profiles published by NHS Digital (https://simplifier.net/guide/ukcoredevelopment/home). 
In addition, you have your own data requirements, so you need to further restrict/extend these profiles with your own profiles.

First step is to create a configuration (FHIR configuration) folder for onFHIR.io. We have created the 'conf' folder in 
this example. 

### Configuring onFHIR for FHIR version you want to support
Lets start with configuring onFHIR.io with the base specifications. onFHIR.io can support any FHIR version as it also 
takes the base specifications as input and configure itself accordingly. For each major FHIR version (R5, R4, STU3), 
we provide a onFHIR.io module (onfhir-server-r5, onfhir-server-r4, onfhir-server-stu3) and a configurator class 
(e.g. io.onfhir.r4.config.FhirR4Configurator) within the module that handles the configuration of onFHIR.io runtime. 
These configurators take the FHIR definitions zip file (in JSON format) that is published by FHIR itself which includes all the base specifications 
(StructureDefinition, SearchParameter, etc). You can download the zip file of the version you want from the Downloads 
section in FHIR web site (e.g. https://www.hl7.org/fhir/downloads.html). In default, when you don't provide such zip file 
in your configurations, the configurator module you used to initiate onFHIR will use the definitions zip file for the latest 
minor version at that major version branch. In this example although we want to use FHIR 4.0.1 which is the latest in R4 
(which also UK Core package is based on) we still provide the zip file ourselves to demonstrate the functionality.  
We have downloaded the zip file (http://hl7.org/fhir/definitions.json.zip) and put it under the conf directory.       

Then we only need to provide the path of this zip file in main configuration file (application.conf) by assuming we 
will run our server in the root directory of this sample project;
```
fhir.initialization.base-definitions-path = "./conf/definitions.json.zip"
```

When you don't set this configuration parameter, onFHIR will use the zip file for the latest minor version in default. 
e.g. For R4 --> 4.0.1  

### Configuring onFHIR for specific FHIR profiles, value sets and code systems 
For validation of FHIR resources and execution of some mechanisms, onFHIR needs the FHIR resource and 
extension profiles (FHIR StructureDefinition resources) that you want to support for your server. In addition, it needs 
all the CodeSystem and ValueSet definitions that are mentioned in these profiles. We want our server to conform UK Core 
specifications, therefore we first download the UKCore package (https://simplifier.net/packages/uk.core.r4/1.3.0). 
For every foundation resource type (StructureDefinition, CodeSystem, ValueSet, etc) used for onFHIR.io configuration, 
we can create a sub folder; 'profiles', 'codesystems' and 'valusets' in this example. Then we can copy the corresponding 
files in UKCore package into these sub folders. 

IMPORTANT NOTE: **Please be careful to have a complete closed set of profiles, valuesets 
and codesystems**. For example, if you mention a FHIR profile within your profiles (within an element definition), but that 
FHIR profile does not exist in your configuration folder, onFHIR.io will throw an exception at the startup and inform you. 

You don't need to copy base FHIR standard specifications (StructureDefinition, ValueSet, etc) into this folder because 
base specifications (also the FHIR defined value sets, etc) are supplied in the previous step.

Similarly, we should set the path of these sub folders in the 'application.conf' file;
```
fhir.initialization {
    profiles-path = "./conf/profiles"
    
    valuesets-path = "./conf/valuesets"
    
    codesystems-path = "./conf/codesystems"
}
```

Now assume that, I want to create a new Patient profile derived from UK Core Patient profile, which makes the ethnicity 
information required (which is defined as extension in UK Core package) and further restrict the valueset binding to 
required (in UK Core it was extensible). We can do this by using the Forge tool (https://fire.ly/products/forge/) as 
shown in the following snapshot.

<img src="/assets/forge-patient-profiling.png">

We save the profile definition (MyFhirPatient.StructureDefinition.json) and put it also under the "conf/profiles" directory. 

Assume that you will store some patient fitness data in this repository which your mobile application and wearables 
will be integrated. You search some existing profiles and find USZ Telemedicine project which publishes similar profiles. 
You want to use some of them, for example USZ SZIVE Activity Observation to store patient reported activity data. You 
download the profile (USZ-SZIVE-Activity-Observation.json), update it based on your requirements and put it under the 
'profiles' directory.

Now we can continue with the definition of CapabilityStatement for our server which will be entry point for onFHIR.io to 
tailor our FHIR server. CapabilityStatement is used in FHIR to describe your existing server's capabilities, but onFHIR.io 
use it to generate the server from the definition and apart from some exceptions it uses all the details in this 
definition to tailor the server to your requirements. We create a file, 'capability-statement.json'  under the conf folder 
and set the following configuration in application.conf to indicate the path.
```
fhir.initialization.conformance-path = "./conf/capability-statement.json"
```

Assume we only want to store Patient and Observation resources in our server. First we start with basics; 
we only want to support json in content format for our repository and FHIR Path Patch format for FHIR patch interactions.
```
 "fhirVersion": "4.0.1",
 "format": ["json"],
 "patchFormat": ["application/json-patch+json"],
```
For content formats, "xml" and "json" is supported (not "ttl" yet). But, please note that onFHIR.io is JSON neutral, 
XML content is always transformed to JSON internally before doing any interaction and transformed again back to XML when 
returning, so it is a bit slower. If you omit 'patchFormat' element, onFHIR.io will both support 'FHIR Path Patch' and 
'JSON Patch'. XML Patch is not supported yet.

Then for every resource type, we should define the desired REST capabilities. As shown in the figure below, for Patient 
resource type we want **every Patient resource to conform to our UK Core Patient derived MyFhirPatient profile**. Therefore 
we put its url in the "profile" element. For Patient resources, we don't want to keep history of resources (only latest 
information about patients are enough), so we set "versioning" to "no-version" and only support CRUD and search 
interactions by specifying them in interaction section. Also we indicated if we want the conditional CRUD operations or not.  
```
...
"resource": [
   {
          "type": "Patient",
          "profile": "http://myfhir.com/fhir/StructureDefinition/MyFhirPatient",
          "interaction": [
            {
              "code": "read", "documentation": "..."
            },
            {
              "code": "update", "documentation": "..."
            },
            {
              "code": "delete", "documentation": "..."
            },
            {
              "code": "create", "documentation": "..."
            },
            {
              "code": "search-type", "documentation": "..."
            }
          ],
          "versioning": "no-version",
          "conditionalCreate": true,
          "conditionalUpdate": true,
          "conditionalDelete": "multiple",    
          ...
   },
    ...
]
...  
```     
We do the same configurations for Observation resource. This time we want every Observation resource to store in our 
repository to conform to UK Core Observation profile, but we also want to support other profiles for different type of 
observations. We put the urls of all these extra profiles in "supportedProfile" element. If you try to save an Observation resource 
claiming to conform a profile other than these given profiles, onFHIR will ignore it as the validation cannot be done. 
Also any profile mentioned in 'profile' or 'supportedProfile' part should be given in profiles folder. Regarding versioning 
this time we want to support versioning and enable clients to access historic content. Another important thing is the 
policy for referencing. As Observation resources generally refer a patient, we want this references to be literal references 
(direct reference to a Patient resource not with a identifier provided to give a uniques identifier about a patient) and 
also correct (referential integrity). We also don't want referencing to another repository. Therefore, we use 'literal', 
'local' and 'enforced'. In this way, for Observation resources onFHIR.io will not allow logical and remote references and 
every time check the existence of the referred resource (e.g. the Patient resource) during validations. 
```
  {
          "type" : "Observation",
          "profile" : "https://fhir.nhs.uk/R4/StructureDefinition/UKCore-Observation",
          "supportedProfile": ["ttp://u-szeged.hu/fhir/StructureDefinition/usz-szive-activity-observation"],
          "interaction" : [
            {
                "code" : "read","documentation" : "..."
            },
            {
              "code" : "vread", "documentation" : "..."
            },
            {
              "code" : "update", "documentation" : "..."
            },
            {
              "code" : "delete", "documentation" : "..."
            },
            {
              "code" : "patch", "documentation" : "..."
            },
            {
              "code" : "history-instance", "documentation" : "..."
            },
            {
              "code" : "create", "documentation" : "..."
            },
            {
              "code" : "search-type", "documentation" : "..."
            }
          ],
          "versioning": "versioned",
          "conditionalCreate" : true,
          "conditionalUpdate" : true,
          "conditionalDelete" : "multiple",
          "referencePolicy" : ["literal", "enforced", "local"],
           ...
    }
    ...
 ```

## Configuring onFHIR with search parameters to support for each resource type
In the capability statement, for each resource type you also can specifies the search parameters to support. 
But sometimes, generally when you have extensions defined, you need new search parameters. onFHIR.io also enables this 
without any new development. You just need to provide the FHIR SearchParameter definition for the search parameter you want 
to support and refer it in the CapabilityStatement.

Lets assume, our applications need to search our patients also by their ethnic category which as you remember from 
previous sections defined as an extension in UK Core profile and we make it mandatory in our derived MyFhirPatient profile. 
The following illustrates the definition of the search parameter. The 'code' elements give the name of the search 
parameter we will use in our FHIR queries. The 'base' indicates that this definition is only for Patient resources. 
The 'type' indicates that the search is a token based search, as the target element to execute a search is a coded element.
Then finally, in the 'expression', you write the FHIR Path (https://www.hl7.org/fhir/fhirpath.html) expression indicating the path to search on. In this example, 
it is the value of the extension with the given defined extension url.      
```
{
  "resourceType": "SearchParameter",
  "id": "patient-ethnicity",
  "url": "http://myfhir.com/fhir/SearchParameter/patient-ethnicity",
  "name": "patient-ethnicity",
  "status": "active",
  "publisher": "SRDC A.S.",
  "code": "ethnicity",
  "base": [
    "Patient"
  ],
  "type": "token",
  "description": "Searching based on ethnicity of patient",
  "expression": "Patient.extension('https://fhir.nhs.uk/R4/StructureDefinition/Extension-UKCore-EthnicCategory').valueCodeableConcept"
}
``` 
We put the definition under another sub folder called 'searchparameters' under our configuration folder and set the path 
in application.conf.

```
fhir.initialization.parameters-path = "./conf/searchparameters"
```
Then we need to update the part in CapabilityStatement. As you can see from the following snapshot, we already configure 
our server to use some FHIR defined search parameters like 'family' or 'email' for Patient resource. At the end of the 
parameter list, we append the new parameter by mentioning its definition url and name. 

```
      {
          "type": "Patient",
          ...
          "searchParam": [
            ...
            {
              "name": "family",
              "definition": "http://hl7.org/fhir/SearchParameter/individual-family",
              "type": "string",
              "documentation": "A portion of the family name of the patient"
            },
            {
              "name": "address-city",
              "definition": "http://hl7.org/fhir/SearchParameter/individual-address-city",
              "type": "string",
              "documentation": "A city specified in an address"
            },
            {
              "name": "email",
              "definition": "http://hl7.org/fhir/SearchParameter/individual-email",
              "type": "token",
              "documentation": "A value in an email contact"
            },
            {
              "name": "patient-ethnicity",
              "definition": "http://myfhir.com/fhir/SearchParameter/patient-ethnicity",
              "type": "token",
              "documentation": "Ethnicity of patient"
            }
          ] 
        ...
    }
```
After doing this it is ready, we can use the parameter for searching; 
```
/Patient?ethnicity=https://fhir.nhs.uk/R4/CodeSystem/UKCore-EthnicCategory|A
/Patient?ethnicity=B   
```

