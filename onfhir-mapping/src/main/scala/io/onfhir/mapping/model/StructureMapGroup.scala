package io.onfhir.mapping.model


case class StructureMapGroup(name:String,
                             extend:Option[String],
                             sourceInputs:Seq[(String, Option[String])],
                             targetInputs:Seq[(String, Option[String])],
                             rules:Seq[StructureMapRule]
                            )

