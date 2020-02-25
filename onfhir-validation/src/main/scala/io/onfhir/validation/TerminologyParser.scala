package io.onfhir.validation
import io.onfhir.api.{FHIR_COMMON_FIELDS, Resource}
import io.onfhir.api.util.FHIRUtil
import io.onfhir.api.validation.{ValueSetDef, ValueSetRestrictions}
import org.json4s.JsonAST.{JBool, JField, JObject, JString, JValue}

class TerminologyParser {


  /**
   * Parse a FHIR value set into a compact form for validation
   * @param valueSetOrCodeSystems FHIR ValueSet or CodeSystem (with an element valueSet inside) resources that may be linked to each other
   * @return Map(URL of ValueSet -> Map(URL of CodeSystem -> Set of codes))
   */
  def parseValueSetBundle(valueSetOrCodeSystems:Seq[Resource]):Map[String, Map[String, ValueSetRestrictions]] = {
    val withResourceType =
      valueSetOrCodeSystems
        .map(v => FHIRUtil.extractValue[String](v, FHIR_COMMON_FIELDS.RESOURCE_TYPE) -> v)

    //Get referable CodeSystem (with url) into format url -> (version -> CodeSystem def)
    val codeSystems =
      prepareMapForInfrastructureResources(
        withResourceType.filter(_._1 == "CodeSystem").map(_._2)
      )

    prepareMapForInfrastructureResources(
      withResourceType.filter(_._1 == "ValueSet").map(_._2)
    )
      .mapValues(_.mapValues(vs => parseValueSet(vs, codeSystems)).filter(_._2.isDefined).mapValues(_.get))
      .filter(_._2.nonEmpty)
  }

  /**
   * Parse a Code System and returns a ValueSetRestriction if CodeSystem is used as ValueSet with all codes
   * @param codeSystem FHIR CodeSystem resource
   * @return
   */
  protected def parseCodeSystemAsValueSet(codeSystem:Resource, filters:Seq[(String, String, String)]):Option[Set[String]] = {
    //Extract defined properties
    val definedProperties = extractDefinedProperties(codeSystem)
    //Find out if there is a hierarchy filter
    val hierarchyFilter = filters.find(f => Set("is-a", "descendant-of", "generalizes").contains(f._2)).map(f => f._2 -> f._3)
    //Found all concepts in the hierarchy conforming to hierarchy filter
    val foundConceptsInHierarchy = filterConceptsAndRelated(hierarchyFilter, codeSystem, definedProperties).getOrElse(Nil)

    //Other type of filters
    val otherFilters = filters.filterNot(f => Set("is-a", "descendant-of", "generalizes").contains(f._2))

    foundConceptsInHierarchy
      .filter(c => isConceptActiveAndConformToFilters(c, otherFilters))
      .map(_._1) match {
        case Nil => None
        case concepts => Some(concepts.toSet)
    }
  }

  /**
   * Parse and filter the concepts and related concepts in the hierachy a
   * @param hiearchicalFilter   Hierarchical filter if exist
   * @param parent              Parent object to search
   * @param definedProperties   Defined properties for the CodeSystem concepts
   * @return
   */
  protected def filterConceptsAndRelated(hiearchicalFilter:Option[(String, String)], parent:JObject, definedProperties:Map[String, String]):Option[Seq[(String, Map[String, JValue])]] = {
    val childConcepts:Seq[JObject] = FHIRUtil.extractValue[Seq[JObject]](parent, "concept")
    //If there is no child concept return empty
    if(childConcepts.isEmpty)
      None
    else {
          if(hiearchicalFilter.forall(_._1 == "is-not-a")){
            //if there is no filter, get all concepts
            if(hiearchicalFilter.isEmpty)
              Some(childConcepts.flatMap(concept => getAndParseConcept(concept, definedProperties)))
            else {//if there is is-not-a filter
              val conceptSearching = hiearchicalFilter.get._2
              childConcepts.indexWhere(childConcept => FHIRUtil.extractValue[String](childConcept, "code") == conceptSearching) match {
                //If the searched concept is not among children go deep with recursion by adding these concepts
                case -1 =>
                  Some(childConcepts.flatMap(childConcept =>
                    getAndParseConcept(childConcept, definedProperties, isOnlyChildren = Some(false)) ++ //Then add the concept
                      filterConceptsAndRelated(hiearchicalFilter, childConcept, definedProperties).getOrElse(Nil) //And search
                  ))
                //If found
                case i =>
                  //Get all concepts except the found one and its children
                  Some(
                    (childConcepts.slice(0, i) ++ childConcepts.slice(i+1, childConcepts.length))
                      .flatMap(getAndParseConcept(_, definedProperties))
                  )
              }
            }
          } else {
            val (hiearachyRelation, conceptSearching) = hiearchicalFilter.get
            //Go over all childs from left
            childConcepts.foldLeft[Option[Seq[(String, Map[String, JValue])]]](None) {
              case (Some(found), _) => Some(found) // if already found skip others
              //If not found yet check this concept
              case (None, concept) =>
                val code = FHIRUtil.extractValue[String](concept, "code")

                //if we found the concept, try to get it and children
                if (conceptSearching == code)
                  hiearachyRelation match {
                    //if is-a get the concept and children
                    case "is-a" =>
                      Some(getAndParseConcept(concept, definedProperties))
                    //do not get the concept itself but children
                    case "descendent-of" =>
                      Some(getAndParseConcept(concept, definedProperties, isOnlyChildren = Some(true)))
                    //get the concept and parents
                    case "generalizes" =>
                      Some(getAndParseConcept(concept, definedProperties, isOnlyChildren = Some(false)))
                  }
                else
                  hiearachyRelation match {
                    //Otherwise continue to search children and return the results
                    case "is-a" | "descendent-of" =>
                      filterConceptsAndRelated(hiearchicalFilter, concept, definedProperties)
                    //Otherwise continue to search children
                    case "generalizes" =>
                      filterConceptsAndRelated(hiearchicalFilter, concept, definedProperties) match {
                        //If found among children, also return this concept together with children
                        case Some(found) =>
                          Some(getAndParseConcept(concept, definedProperties, isOnlyChildren = Some(false)) ++ found)
                        //Otherwise return nothing
                        case None =>
                          None
                      }
                  }
            }
          }
    }
  }

  /**
   *  Check if a concept is active and conform to filters
   * @param concept
   * @param otherFilters
   */
  protected def isConceptActiveAndConformToFilters(concept:(String, Map[String, JValue]), otherFilters:Seq[(String, String, String)]):Boolean = {
    !concept._2.get("status").exists(_ != JString("active")) && //status should not be retired/deprecated
      !concept._2.get("notSelectable").contains(JBool(true)) &&  //concept should not be notSelectable
        otherFilters.forall {
          //Equality check on the properties
          case (property, "=", value) =>
            concept._2.get(property).exists {
              case JString(v) => v == value
              case JBool(b) => if(b) value == "true"else value == "false"
              case _ => false
            }
          //In the given set
          case (property, "in", values) =>
            concept._2.get(property).exists {
              case JString(v) => values.split(',').contains(v)
              case _ => false
            }
          //Not in given set
          case (property, "not-in", values) =>
            concept._2.get(property).forall {
              case JString(v) => !values.split(',').contains(v)
              case _ => false
            }
          //Exists check
          case (property, "exists", "true") => concept._2.isDefinedAt(property)
          case (property, "exists", "false") => !concept._2.isDefinedAt(property)
          //regex
          case ("code", "regex", regExp) => concept._1.matches(regExp)
          case (property, "regex", regExp) =>
            concept._2.get(property).exists {
              case JString(v) => v.matches(regExp)
              case _ => false
            }
          //Don't know any more filter
          case _ => true
        }
  }

  /**
   * Get the concept details and child concept details
   * @param concept             FHIR CodeSystem.concept element
   * @param definedProperties   All defined properties for CodeSystem
   * @return
   */
  protected def getAndParseConcept(concept:JObject, definedProperties:Map[String, String], isOnlyChildren:Option[Boolean] = None):Seq[(String, Map[String, JValue])] = {
    //Get the properties of the concept
    val properties =
      FHIRUtil
        .extractValueOption[Seq[JObject]](concept, "property").getOrElse(Nil)
        .map(extractPropertyCodeAndValue(_, definedProperties)).toMap

    val conceptCode = FHIRUtil.extractValue[String](concept, "code")

    isOnlyChildren match {
      //Return both
      case None =>
        (conceptCode -> properties) +:
          FHIRUtil.extractValue[Seq[JObject]](concept, "concept")
            .flatMap(childConcept => getAndParseConcept(childConcept, definedProperties))
      //Only children
      case Some(true) =>
        FHIRUtil.extractValue[Seq[JObject]](concept, "concept")
          .flatMap(childConcept => getAndParseConcept(childConcept, definedProperties))
      //Only parent
      case Some(false) =>
        Seq(conceptCode -> properties)
    }
  }

  /**
   * Defined properties and their data types
   * @param codeSystem FHIR CodeSystem resource
   * @return
   */
  protected def extractDefinedProperties(codeSystem:Resource):Map[String, String] = {
    val definedProperties =
      for {
        JObject(child) <- codeSystem
        JField("code", JString(code))  <- child
        JField("type", JString(dtype))  <- child
      } yield (code, dtype)
    definedProperties.toMap
  }

  /**
   * Extract the property code and value dedined in a CodeSystem.concept.property
   * @param property
   * @param definedProperties
   * @return
   */
  protected def extractPropertyCodeAndValue(property:JObject, definedProperties:Map[String, String]):(String, JValue) = {
    val code = FHIRUtil.extractValue[String](property, "code")
    val dtype = definedProperties(code)
    code -> property.obj.find(_._1 == "value" + dtype.capitalize).get._2
  }

  /**
   * Parse a ValueSet definition and identify all the codes included or excluded in the ValueSet
   * @param valueSet
   * @return
   */
  protected def parseValueSet(valueSet:Resource, codeSystems:Map[String, Map[String, Resource]]):Option[ValueSetRestrictions] ={
    val included = getValueSetDef(valueSet, codeSystems, isIncluded = true)
    val excluded = getValueSetDef(valueSet, codeSystems, isIncluded = false)
    //If there is no direct code, do not put it (TODO handle filters; intensionally defined value sets)
    if(included.isEmpty)
      None
    else
      Some(ValueSetRestrictions(included.head, excluded))

  }

  /**
   * Get the URL and version of an Infrastructure resource
   * @param infResource
   * @return
   */
  protected def getUrlAndVersionForInfrastructureResource(infResource:Resource):Option[(String, String, Resource)] = {
    FHIRUtil.extractValueOption[String](infResource, FHIR_COMMON_FIELDS.URL)
      .map(url =>
        (url,
          FHIRUtil.extractValueOption[String](infResource, FHIR_COMMON_FIELDS.VERSION).getOrElse("*"),
          infResource))
  }

  /**
   * Prepare a map for infrastructure resources
   * @param infResources
   * @return
   */
  protected def prepareMapForInfrastructureResources(infResources:Seq[Resource]):Map[String, Map[String, Resource]] = {
    infResources
      .flatMap(getUrlAndVersionForInfrastructureResource)
      .groupBy(_._1)
      .map(g => g._1 -> g._2.map(v => v._2 -> v._3).toMap)
  }

  /**
   * Parse the specific compose part
   * @param valueSet   Given ValueSet content
   * @param isIncluded if true 'include' part, else 'exclude' part
   * @return
   */
  protected def getValueSetDef(valueSet:Resource, codeSystemMap:Map[String, Map[String, Resource]], isIncluded:Boolean):Option[ValueSetDef] = {
    val results =
      FHIRUtil
        .extractValueOptionByPath[Seq[JObject]](valueSet, s"compose.${if(isIncluded) "include" else "exclude"}") //Get the compose component
        .getOrElse(Nil)
        .map(inc => {
          val codeLists = FHIRUtil.extractValueOption[String](inc, "system") match { //Get the refered code system
            //If exists
            case Some(csUrl) =>
              //try to get the directly given codes
              FHIRUtil.extractValueOptionByPath[Seq[String]](inc, "concept.code").getOrElse(Nil) match {
                //If not exist
                case Nil =>
                  val filters = getFilters(inc:JObject)
                  getReferedCodeSystem(codeSystemMap, csUrl, FHIRUtil.extractValueOption[String](inc, "version"))
                     .flatMap(cs => parseCodeSystemAsValueSet(cs, filters))
                     .map(csUrl -> _)

                //If there are codes listed, only include them
                case codes =>
                  Some(csUrl -> codes.toSet)
              }
            //If code system url is not given skip
            case None => None
          }
          codeLists ->
            FHIRUtil.extractValueOption[Seq[String]](inc, "valueSet").getOrElse(Nil) //Also get the referred value sets
        })

    val codeSystems =
      results
        .flatMap(_._1)
        .groupBy(_._1)
        .map(g => g._1 -> g._2.flatMap(_._2).toSet)
    val valueSets = results.flatMap(_._2).toSet
    //If there is nothing to include as far as we can, return nothing
    if(codeSystems.isEmpty && valueSets.isEmpty)
      None
    else
      Some(ValueSetDef(codeSystems, valueSets))
  }

  /**
   * Get defined filters in ValueSet.include (or exclude)
   * @param parent
   * @return
   */
  protected def getFilters(parent:JObject):Seq[(String, String, String)] = {
    FHIRUtil.extractValue[Seq[JObject]](parent, "filter")
      .map(filter =>
        (
        FHIRUtil.extractValue[String](filter, "property"),
          FHIRUtil.extractValue[String](filter, "op"),
          FHIRUtil.extractValue[String](filter, "value")
          )
      )
  }

  /**
   * Get referred code system with the given URL and version
   * @param codeSystemMap
   * @param csUrl
   * @param version
   * @return
   */
  protected def getReferedCodeSystem(codeSystemMap:Map[String, Map[String, Resource]], csUrl:String, version:Option[String]):Option[Resource] = {
    val referedCodeSystemVersions = codeSystemMap.get(csUrl)
    version match {
      case None | Some("*") => referedCodeSystemVersions.map(_.head._2)
      case Some(v) => referedCodeSystemVersions.flatMap(_.get(v))
    }
  }

}
