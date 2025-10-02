package io.onfhir.authz

/**
 * Authorization rule for FHIR actions
 *
 * e.g. Define a further constraint for resource creation
 * {
 *  "description": "User should include himself/herself in performer while creating or updating the Observation",
 *  "resourceType": [ "Observation" ],
 *  "fhirAction": ["create", "update"],*
 *  "contentConstraint": [
 *    "performer.reference.exists($this = %claims.fhirUser)"
 *  ]
 * }
 *
 * The following context parameters can be used within FHIR Path and search statements
 * - %claims : Authorization claims supplied in the authorization token (AuthzContext)
 *  e.g. %claims.sub --> OpenId Connect 'sub' claim (subject identifier for the end-user at the Issuer).
 *  e.g. %claims.fhirUser --> Smart-on-Fhir 'fhirUser' claim providing the end-user's corresponding FHIR entity e.g. Practitioner/15335
 * - %scopes: Authorization scopes provided for the user within authorization context
 *  e.g. %scopes.exists($this.startsWith('patient/Observation'))
 * - %request: Other FHIR request details i.e. resourceType, resourceId, queryParams, etc (i.e. FHIRRequest)
 *  e.g. %request.queryParams.contains('patient') --> If search is restricting for one or more patients
 *
 * @param resourceType      List of FHIR resource types that this rule is about e.g. Condition, Observation
 * @param fhirAction        List of FHIR actions that this rule allows (search-type, create, update, etc or FHIR operation name e.g. $lastn)
 * @param precondition      Precondition for this rule to apply (A FHIR Path expression to be executed on current authorization context)
 * @param searchConstraint  FHIR search statements to be satisfied
 *                          e.g. subject={{%claims.patient.select('Patient/' & $this).mkString(',')}} for FHIR search
 * @param contentConstraint FHIR Path statements to be satisfied for the supplied content for FHIR create, update,  or FHIR operation etc.
 *                          e.g. Observation.practitioner.reference = 'Practitioner/' & %claims.sub
 */
case class FhirAuthzConstraintRule(
                                    resourceType: Seq[String],
                                    description: Option[String] = None,
                                    fhirAction: Seq[String],
                                    precondition: Option[String] = None,
                                    searchConstraint: Option[Seq[String]] = None,
                                    contentConstraint: Option[Seq[String]] = None
                                  )
