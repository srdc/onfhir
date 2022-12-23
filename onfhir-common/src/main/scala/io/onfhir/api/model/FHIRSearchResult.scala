package io.onfhir.api.model

import io.onfhir.api.Resource

/**
 * Compact form of a FHIR search result set
 * @param total         Number of matching records
 * @param matches       Matching records for this page
 * @param includes      Included records for this page
 * @param offsetBefore  Offset values for searching before for offset based pagination
 * @param offsetAfter   Offset values for searching after for offset based pagination
 */
case class FHIRSearchResult(total:Long,
                            matches:Seq[Resource] = Nil,
                            includes:Seq[Resource] = Nil,
                            offsetBefore:Seq[String] = Nil,
                            offsetAfter:Seq[String] = Nil
                           )
