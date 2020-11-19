package io.onfhir.api.model

sealed trait IFhirTrigger extends InternalEntity

/**
 * A FHIR data trigger definition (represent partially FHIR TriggerDefinition)
 * @param rtype               Resource type to monitor
 * @param dataEvent           Data event to monitor (see io.onfhir.api.model.FhirTriggerEvents)
 * @param criteria            Parsed FHIR Query that defines criteria on the latest resource content
 * @param previousCriteria    Parsed FHIR Query that defines criteria on the previous resource content
 * @param bothRequired        If both criteria given, they should be satisfied both or one is enough
 */
case class FhirDataTrigger(rtype:String, dataEvent:String, criteria:Seq[Parameter], previousCriteria:Seq[Parameter] = Nil, bothRequired:Boolean = true) extends IFhirTrigger

/**
 * A FHIR Named-event based trigger definition
 * @param event
 */
case class FhirNamedEventTrigger(event:String) extends IFhirTrigger

//TODO Time based trigger

//TODO Pattern based trigger - complex event processing (not defined in standard)