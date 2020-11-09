package io.onfhir.api.util

import io.onfhir.api.Resource
import org.json4s.JsonAST.{JArray, JObject, JValue}
import org.json4s.JsonDSL._

object FhirPatchUtil {

  type FhirPatchOp = (JObject, (String, Option[Int])) => JObject

  /**
   * Supplementary method to handle updating the data by using the delegated operation while finding the path
   * @param input           Resource to update
   * @param path            Path that patch is defined
   * @param jsonOperation   Patch Operation method to handle the update
   * @return
   */
  private def runPatchOperation(input:Resource, path:Seq[(String, Option[Int])], jsonOperation:FhirPatchOp):Resource = {
    if(path.length == 1) {
      jsonOperation(input, path.head)
    } else {
      JObject(
        input.obj.map {
          case (el, value) if el == path.head._1 && value.isInstanceOf[JObject] =>
            path.head._1 -> runPatchOperation(value.asInstanceOf[JObject], path.tail, jsonOperation) //Continue from the element
          case (el, value) if el == path.head._1 && value.isInstanceOf[JArray] =>
            val arrIndex = path.head._2.getOrElse(0)
            path.head._1 ->
              JArray(
                value.asInstanceOf[JArray]
                  .arr.zipWithIndex
                  .map(v =>
                    if(v._2 == arrIndex)
                      runPatchOperation(v._1.asInstanceOf[JObject], path.tail, jsonOperation)
                    else
                      v._1
                  )
              )
          case oth => oth
        }
      )
    }
  }

  /**
   * FHIR patch add
   * Add an element to the given path with the given name
   * @param resource        FHIR Resource to update
   * @param path            Path to the parent element to add the element
   * @param name            Name of the element
   * @param value           JSON Value to add
   * @param isTargetArray   If the element to add is an array
   * @return
   */
  def applyAddPatch(resource: Resource, path:Seq[(String, Option[Int])], name:String, value:JValue, isTargetArray:Boolean = false):Resource = {
    runPatchOperation(resource, path :+ (name -> None), addOperation(value, isTargetArray))
  }

  /**
   * Delegated FHIR patch add operation
   * @param value           JSON value to add
   * @param isTargetArray   If the target path for element to add is an array
   * @param resource        Input resource to add the element
   * @param lastPath        Last element of the path to add the element
   * @return
   */
  private def addOperation(value:JValue, isTargetArray:Boolean)(resource: Resource, lastPath:(String, Option[Int])):Resource = {
    resource.obj.find(_._1 == lastPath._1).map(_._2) match {
      case Some(JArray(arr)) =>
        resource.obj.filterNot(_._1 == lastPath._1) :+
          lastPath._1 -> JArray(arr :+ value) // add the element to end of array
      case Some(_) =>
        //Replace the value if exist, if it is not array
        resource.obj.filterNot(_._1 == lastPath._1) :+
          lastPath._1 -> value
      case None =>
        //If the element does not exist
        if(isTargetArray)
          resource ~ (lastPath._1 -> JArray(List(value)))
        else
          resource ~ (lastPath._1 -> value)
    }
  }

  /**
   * FHIR Patch insert
   * @param resource  FHIR Resource to update
   * @param path      Path to the element to insert value
   * @param index     Index to insert the element within array
   * @param value     JSON value to insert
   * @throws IndexOutOfBoundsException if given index is out of bounds according to the target array element
   * @throws IllegalArgumentException if insert is called on a non repetitive element
   * @return
   */
  @throws[IndexOutOfBoundsException]
  @throws[IllegalArgumentException]
  def applyInsertPatch(resource: Resource, path:Seq[(String, Option[Int])], index:Int, value:JValue):Resource = {
    if(path.last._2.nonEmpty) {
      throw new IllegalArgumentException(s"Invalid FHIR Path Patch operation 'insert', operation 'insert' can only be used on repetitive elements")
    }
    runPatchOperation(resource, path, insertOperation(index, value))
  }
  /**
   * Delegated FHIR patch insert operation
   * @param index     Index to insert the element
   * @param value     JSON value to insert
   * @param resource  Input FHIR resource
   * @param lastPath  Last path element to insert
   * @return
   */
  private def insertOperation(index:Int, value:JValue)(resource: Resource, lastPath:(String, Option[Int])):Resource = {
    JObject(
      resource.obj.map {
        //If the target is an array do the operation
        case (lastPath._1, JArray(arr)) =>
          if(index > arr.size) {
            throw new IndexOutOfBoundsException(s"Invalid FHIR Path Patch operation 'insert', given index $index is greater than the size of array!")
          }
          lastPath._1 -> JArray((arr.slice(0,index) :+ value) ++ arr.drop(index))
        //Otherwise throw exception
        case (lastPath._1, _) =>
          throw new IllegalArgumentException(s"Invalid FHIR Path Patch operation, operation 'insert' can only be used on repetitive elements!")
        //If there is no such field, return resource as it is
        case oth => oth
      }
    )
  }

  /**
   * FHIR patch delete
   * Delete the element indicated by the path
   * @param path      Parsed path to the element to delete
   * @param resource  Input FHIR resource
   * @return
   */
  def applyDeletePatch(resource: Resource, path:Seq[(String, Option[Int])]):Resource = {
    if(path.isEmpty)
      resource
    else
      runPatchOperation(resource, path, deleteOperation)
  }

  /**
   * Delegated FHIR patch delete operation
   * @param resource Input FHIR resource
   * @param lastPath Last path element to delete
   * @return
   */
  private def deleteOperation(resource: Resource, lastPath:(String, Option[Int])):Resource = {
    if(lastPath._2.isEmpty) //if it is not an an array, just remove the element
      JObject(resource.obj.filterNot(_._1 == lastPath._1))
    else {
      //Otherwise,it should be an array, find the element ad remove it
      val arr = resource.obj.find(_._1 == lastPath._1).get._2.asInstanceOf[JArray].arr
      JObject(
        if(arr.size == 1) //if the array will be empty, remove it
          resource.obj.filterNot(_._1 == lastPath._1)
        else {
          val farr = JArray(arr.slice(0, lastPath._2.get) ++ arr.drop(lastPath._2.get + 1))
          resource.obj.filterNot(_._1 == lastPath._1) :+ lastPath._1 -> farr
        }
      )
    }
  }

  /**
   * FHIR Patch replace
   * @param resource    Resource to update
   * @param path        Path to the element to replace
   * @param value       Value to put for replacement
   * @throws IllegalArgumentException if path indicates a repetitive element but actual element is not
   * @return
   */
  @throws[IllegalArgumentException]
  def applyReplacePatch(resource: Resource, path:Seq[(String, Option[Int])], value:JValue):Resource = {
    runPatchOperation(resource, path, replaceOperation(value))
  }

  /**
   * Delegated FHIR patch replace operation
   * @param value     Value to put as replacement
   * @param resource  Input FHIR resource
   * @param lastPath  Last element path to replace
   * @throws IllegalArgumentException if path indicates a repetitive element but actual element is not
   * @return
   */
  @throws[IllegalArgumentException]
  def replaceOperation(value:JValue)(resource: Resource, lastPath:(String, Option[Int])):Resource = {
    JObject(resource.obj.map {
      case (lastPath._1, JArray(arr)) if lastPath._2.isDefined =>
        val replacedInd = lastPath._2.get
        lastPath._1 -> JArray((arr.slice(0, replacedInd) :+ value) ++ arr.drop(replacedInd +1))
      //Replacing the array
      case (lastPath._1, JArray(_)) if lastPath._2.isEmpty =>
        lastPath._1 -> JArray(List(value))
      //Replacing a single Json object or value
      case (lastPath._1, _) =>
        if(lastPath._2.isEmpty)
          lastPath._1 -> value
        else
          throw new IllegalArgumentException("Invalid FHIR Path Patch operation 'replace'! Path indicates a repetitive element although the target element is not!")
      case oth => oth
    })
  }

  /**
   * FHIR patch move operation
   * @param resource    Resource to update
   * @param path        Path to the parent repetitive element
   * @param source      Source index to move
   * @param destination Destination index
   * @throws IndexOutOfBoundsException if given source or destionation is out of bounds according to the target array element
   * @throws IllegalArgumentException if path does not indicate a repetitive element
   * @return
   */
  @throws[IndexOutOfBoundsException]
  @throws[IllegalArgumentException]
  def applyMovePatch(resource: Resource, path:Seq[(String, Option[Int])], source:Int, destination:Int):Resource = {
    runPatchOperation(resource, path, moveOperation(source,destination))
  }

  /**
   * Delegated FHIR patch move operation
   * @param source
   * @param destination
   * @param resource
   * @param lastPath
   * @return
   */
  def moveOperation(source:Int, destination:Int)(resource: Resource, lastPath:(String, Option[Int])):Resource = {
    JObject(resource.obj.map {
      case (lastPath._1, JArray(arr)) =>
        if(source > arr.size) {
          throw new IndexOutOfBoundsException(s"Invalid FHIR Path Patch operation 'move', given source $source is greater than the size of array ${arr.size}!")
        }
        if(destination > arr.size) {
          throw new IndexOutOfBoundsException(s"Invalid FHIR Path Patch operation 'move', given destination $destination is greater than the size of array ${arr.size}!")
        }

        val elem = arr.apply(source)
        val farr =
          if(destination <= source)
            (
              arr.slice(0, destination) :+ elem //get the part before destination and add the element
              ) ++ arr.slice(destination, source) ++ //add the part between the destionation and source
              arr.drop(source+1) //get list after the destination
          else
            (
              arr.slice(0, source) ++ //Get the list before the element to move
                arr.slice(source+1, destination+1) :+ elem //Get the middle part between source and destination and add the element to the end
              ) ++ arr.drop(destination+1)  //Get list after the destination

        lastPath._1 -> JArray(farr)

      case (lastPath._1, _) =>
        throw new IllegalArgumentException(s"Invalid FHIR Path Patch operation 'move', the given path should indicate a repetitive element!")

      case oth => oth
    })
  }


}
