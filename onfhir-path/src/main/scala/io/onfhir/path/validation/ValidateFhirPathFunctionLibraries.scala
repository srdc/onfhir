package io.onfhir.path.validation

import io.onfhir.path.AbstractFhirPathFunctionLibrary
import io.onfhir.path.annotation.FhirPathFunction
import org.reflections.Reflections

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

object ValidateFhirPathFunctionLibraries {

  /**
   * Validates that each FhirPath function defined in a FhirPath function library is annotated with FhirPathFunction annotation.
   * It finds the classes which extend AbstractFhirPathFunctionLibrary (i.e. FhirPath function libraries) using reflection
   * on the given package list and terminates with an exception if there are some functions missing this annotation.
   * Otherwise, it terminates successfully.
   *
   * @param args the list of packages where FhirPath function libraries will be searched
   * @throws Exception when a FhirPath function library has some functions missing FhirPathFunction annotation
   * */
  def main(args: Array[String]): Unit = {
    args.foreach(p => {
      val reflections = new Reflections(p)
      // traverse the each library
      reflections.getSubTypesOf(classOf[AbstractFhirPathFunctionLibrary]).forEach(c => {
        if (currentMirror.classSymbol(c)
          .toType
          .decls // returns declared members of class
          .filter(method => method.isMethod && method.isPublic && !method.isConstructor && !method.asTerm.isAccessor) // returns the public methods by discarding the constructor and 'val' methods
          .exists(method => !method.annotations.exists(_.tree.tpe =:= typeOf[FhirPathFunction]))) { // throw an exception if there are some methods which does not have a FhirPathFunction annotation
          throw new Exception(s"There are some methods which are not annotated with FhirPathFunction in the library: ${c.getName}")
        }
      })
    })
  }
}
