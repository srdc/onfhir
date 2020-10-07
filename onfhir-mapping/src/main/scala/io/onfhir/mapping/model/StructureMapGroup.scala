package io.onfhir.mapping.model


case class StructureMapGroup(name:String,
                             extend:Option[String],
                             sourceInputs:Seq[(String, String)],
                             targetInputs:Seq[(String, String)],
                             rules:Seq[StructureMapRule]
                            )

