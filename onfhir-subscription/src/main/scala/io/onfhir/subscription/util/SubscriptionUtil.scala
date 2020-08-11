package io.onfhir.subscription.util

import io.onfhir.api.model.Parameter

import scala.util.hashing.MurmurHash3

object SubscriptionUtil {

  def getCriteriaHash(rtype:String, criteria:Seq[Parameter]):String = {
    val serializedQuery =
      criteria
        .sortBy(_.name) //Order based on search parameter name
        .map(p =>
          s"${p.name}${p.suffix}=${p.valuePrefixList.sortBy(_._2).map(v => v._1+v._2).mkString(",")}" //also sort values
        )
        .mkString("&")

    val serializedWholeQuery = s"$rtype?$serializedQuery"
    MurmurHash3.stringHash(serializedWholeQuery).toString
  }

}
