package io.onfhir.mapping.model

case class ConceptMapping(equivalence:String, target:Option[String], display:Option[String], comment:Option[String])

case class ConceptMapGroup(source:Option[String], target:Option[String], sourceVersion:Option[String], targetVersion:Option[String], element:Map[String, ConceptMapping])

case class ConceptMap(url:String, conceptMapGroups:Seq[ConceptMapGroup])
