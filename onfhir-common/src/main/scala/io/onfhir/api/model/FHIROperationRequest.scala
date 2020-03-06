package io.onfhir.api.model

/**
  * Created by tuncay on 10/3/2017.
  * FHIR Operation request
  */
class FHIROperationRequest(params:Seq[(String, FHIROperationParam)]) extends FHIRMultiOperationParam(params) {


}
