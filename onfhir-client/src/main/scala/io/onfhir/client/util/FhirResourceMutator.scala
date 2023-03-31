package io.onfhir.client.util

import io.onfhir.api.Resource
import io.onfhir.api.util.FhirPatchUtil
import io.onfhir.client.util.FhirResourceMutator.FhirMutableResource
import io.onfhir.path.FhirPathEvaluator
import org.json4s.JsonAST.JValue

/**
 * Utility class to make it easy to update FHIR resource content by using FHIR Patch like operations
 */
object FhirResourceMutator {
  implicit val ic = scala.language.implicitConversions

  class FhirMutableResource(var resource:Resource) {

    /**
     * Add a FHIR element under the given path (FHIR Patch add logic)
     * @param fhirPath  FHIR path to the parent element to make the addition (If no element exist in this path, no addition is done)
     * @param name      Name of the element (If element exists and it is repetitive, the value is appended to the end of the array)
     * @param value     Value of the element
     * @param isArray   If the element to add should be an array (needed when the element does not exist  and if true a array with given single value is created)
     * @return
     */
    def addElement(fhirPath:String, name:String, value:JValue, isArray:Boolean = false):FhirMutableResource = {
      resource =
        parsePath(fhirPath)
          .map(parsedPath => FhirPatchUtil.applyAddPatch(resource, parsedPath, name, value, isArray))
          .getOrElse(resource)
      this
    }

    /**
     * See addElement(..), only difference is that if given FHIR path expression does not evaluate to any element this one throws exception
     * @param fhirPath  FHIR path to the parent element to make the addition
     * @param name      Name of the element (If element exists and it is repetitive, the value is appended to the end of the array)
     * @param value     Value of the element
     * @param isArray   If the element to add should be an array (needed when the element does not exist  and if true a array with given single value is created)
     * @return
    */
    @throws[IllegalArgumentException]
    def addElementOrThrowExc(fhirPath:String, name:String, value:JValue, isArray:Boolean = false):FhirMutableResource = {
      resource = parsePath(fhirPath) match {
        case None => throw new IllegalArgumentException(s"Given FHIR path $fhirPath does not indicate any element for the given resource!")
        case Some(parsedPath) => FhirPatchUtil.applyAddPatch(resource, parsedPath, name, value, isArray)
      }
      this
    }

    /**
     * Add a FHIR element to the root
     * @param name      Name of the element
     * @param value     Value of the element
     * @param isArray   If the element to add should be an array (needed when the element does not exist  and if true a array with given single value is created)
     * @return
     */
    def addRootElement(name:String, value:JValue, isArray:Boolean = false):FhirMutableResource = {
      resource = FhirPatchUtil.applyAddPatch(resource, Nil, name, value, isArray)
      this
    }

    /**
     * Insert a FHIR element to given index
     * @param fhirPath  FHIR path to the repetitive element to insert the value
     * @param index     Zero based index to insert the element
     * @param value     JSON Value to insert
     * @throws IndexOutOfBoundsException if given index is out of bounds according to the target array element
     * @throws IllegalArgumentException if insert is called on a non repetitive element
     * @return
     */
    @throws[IndexOutOfBoundsException]
    @throws[IllegalArgumentException]
    def insertElement(fhirPath:String, index:Int, value:JValue):FhirMutableResource = {
      resource =
        parsePath(fhirPath)
          .map(parsedPath => FhirPatchUtil.applyInsertPatch(resource, parsedPath, index, value))
          .getOrElse(resource)
      this
    }

    /**
     * See insertElement(...),  only difference is that if given FHIR path expression does not evaluate to any element this one throws exception
     * @param fhirPath  FHIR path to the repetitive element to insert the value
     * @param index     Zero based index to insert the element
     * @param value     JSON Value to insert
     * @return
     */
    @throws[IndexOutOfBoundsException]
    @throws[IllegalArgumentException]
    def insertElementOrThrowExc(fhirPath:String, index:Int, value:JValue):FhirMutableResource = {
      resource = parsePath(fhirPath) match {
        case None => throw new IllegalArgumentException(s"Given FHIR path $fhirPath does not indicate any element for the given resource!")
        case Some(parsedPath) => FhirPatchUtil.applyInsertPatch(resource, parsedPath, index, value)
      }
      this
    }

    /***
     * Delete the element indicated by path (if element not exist, no change is done)
     * @param fhirPath  Fhir path to the element or item in an repetitive element
     * @return
     */
    def deleteElement(fhirPath:String):FhirMutableResource = {
      resource =
        parsePath(fhirPath)
          .map(parsedPath => FhirPatchUtil.applyDeletePatch(resource, parsedPath))
          .getOrElse(resource)
      this
    }

    /**
     * See deleteElement(...), only difference is that if given FHIR path expression does not evaluate to any element this one throws exception
     * @param fhirPath
     * @return
     */
    @throws[IllegalArgumentException]
    def deleteElementOrThrowExc(fhirPath:String):FhirMutableResource = {
      resource = parsePath(fhirPath) match {
        case None => throw new IllegalArgumentException(s"Given FHIR path $fhirPath does not indicate any element for the given resource!")
        case Some(parsedPath) => FhirPatchUtil.applyDeletePatch(resource, parsedPath)
      }
      this
    }

    /**
     * Replace the element indicated by path
     * @param fhirPath  Fhir path to the element or item in an repetitive element
     * @param value     JSON Value to replace by
     * @return
     */
    def replaceElement(fhirPath:String, value:JValue):FhirMutableResource = {
      resource =
        parsePath(fhirPath)
          .map(parsedPath =>
            FhirPatchUtil.applyReplacePatch(resource, parsedPath, value)
          ).getOrElse(resource)
      this
    }

    /**
     * See replaceElement(...), only difference is that if given FHIR path expression does not evaluate to any element this one throws exception
     * @param fhirPath
     * @param value
     * @return
     */
    @throws[IllegalArgumentException]
    def replaceElementOrThrowExc(fhirPath:String, value:JValue):FhirMutableResource = {
      resource = parsePath(fhirPath) match {
        case None => throw new IllegalArgumentException(s"Given FHIR path $fhirPath does not indicate any element for the given resource!")
        case Some(parsedPath) => FhirPatchUtil.applyReplacePatch(resource, parsedPath, value)
      }
      this
    }

    /**
     * Move an item in a repetitive element from given source index to destination index
     * @param fhirPath      Fhir path to the element
     * @param source        Zero based source index
     * @param destination   Zero based destination index
     * @throws IndexOutOfBoundsException if given source or destination index is out of bounds according to the target array element
     * @throws IllegalArgumentException if path does not indicate a repetitive element
     * @return
     */
    @throws[IndexOutOfBoundsException]
    @throws[IllegalArgumentException]
    def moveElement(fhirPath:String, source:Int, destination:Int):FhirMutableResource = {
      resource = parsePath(fhirPath).map(parsedPath =>
        FhirPatchUtil.applyMovePatch(resource, parsedPath, source, destination)
      ).getOrElse(resource)
      this
    }

    /**
     * See moveElement(...), only difference is that if given FHIR path expression does not evaluate to any element this one throws exception
     * @param fhirPath      Fhir path to the element
     * @param source        Zero based source index
     * @param destination   Zero based destination index
     * @throws IndexOutOfBoundsException if given source or destination index is out of bounds according to the target array element
     * @throws IllegalArgumentException if path does not indicate a repetitive element
     * @return
     */
    @throws[IndexOutOfBoundsException]
    @throws[IllegalArgumentException]
    def moveElementOrThrowExc(fhirPath:String, source:Int, destination:Int):FhirMutableResource = {
      resource = parsePath(fhirPath) match {
        case None => throw new IllegalArgumentException(s"Given FHIR path $fhirPath does not indicate any element for the given resource!")
        case Some(parsedPath) => FhirPatchUtil.applyMovePatch(resource, parsedPath, source, destination)
      }
      this
    }

    /**
     * Utility method to parse the given FHIR path
     * @param fhirPath  FHIR Path  path expression
     * @return
     */
    private def parsePath(fhirPath:String):Option[Seq[(String, Option[Int])]] = {
      FhirPathEvaluator().evaluateToFindPaths(fhirPath, resource).headOption match {
        case Some(Seq(("", None))) => Some(Nil)
        case Some(oth) => Some(oth)
        case None => None
      }
    }
  }

  /**
   * Implicit conversion
   * @param resource
   * @return
   */
  implicit def convertToFhirMutableResource(resource: Resource):FhirMutableResource = new FhirMutableResource(resource)

  implicit def convertToResource(fhirMutableResource: FhirMutableResource):Resource = fhirMutableResource.resource

}
