package io.onfhir.subscription.cache

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.{ORMap, ORMapKey, SelfUniqueAddress}
import akka.cluster.ddata.typed.scaladsl.Replicator._

import io.onfhir.subscription.config.SubscriptionConfig
import io.onfhir.subscription.model.{CriteriaSubscriptions, FhirSubscription}

import scala.collection.mutable

object AkkaBasedSubscriptionCache {

  /**
   * Internal commands to communicate with replicator
   */
  private sealed trait InternalCommand extends Command
  //Internal responses handled
  private case class InternalUpdateResponseForCriteriaSubscriptions(rsp: Replicator.UpdateResponse[ORMap[String, ORMap[String,CriteriaSubscriptions]]], replyTo:ActorRef[Boolean]) extends InternalCommand
  private case class InternalUpdateResponseForFhirSubscription(internalCommand:Option[InternalCommand], rsp:Replicator.UpdateResponse[ORMap[String, FhirSubscription]], replyTo:ActorRef[Boolean]) extends InternalCommand
  private case class InternalGetResponseForCriteriaSubscriptions(key:String, rsp: Replicator.GetResponse[ORMap[String, ORMap[String,CriteriaSubscriptions]]], replyTo: ActorRef[Seq[CriteriaSubscriptions]]) extends InternalCommand
  private case class InternalGetResponseForSubscription(sid:String, rsp:Replicator.GetResponse[ORMap[String, FhirSubscription]], replyTo:ActorRef[Option[FhirSubscription]]) extends InternalCommand
  private case class InternalOpResponse(result:Boolean, replyTo:ActorRef[Boolean]) extends InternalCommand
  //Internal commands
  private case class InternalAddCriteriaSubscription(fhirSubscription: FhirSubscription, replyTo:ActorRef[Boolean]) extends InternalCommand
  private case class InternalRemoveCriteriaSubscription(fhirSubscription: FhirSubscription, replyTo:ActorRef[Boolean]) extends InternalCommand
  private case class InternalRemoveSubscription(fhirSubscription: FhirSubscription, replyTo:ActorRef[Boolean]) extends InternalCommand

  /**
   * Actor Behavior
   * @param subscriptionConfig
   * @param fhirSearchParameterCache
   * @return
   */
  def apply(subscriptionConfig: SubscriptionConfig,
            fhirSearchParameterCache: FhirSearchParameterCache
           ): Behavior[Command] = {
    Behaviors.setup { ctx =>
      implicit val node: SelfUniqueAddress = DistributedData(ctx.system).selfUniqueAddress

      // adapter that turns the response messages from the replicator into our own protocol
      DistributedData.withReplicatorMessageAdapter[Command, ORMap[String, ORMap[String, CriteriaSubscriptions]]] { replicatorAdapterForCriteriaSubscriptions =>
        DistributedData.withReplicatorMessageAdapter[Command, ORMap[String, FhirSubscription]] { replicatorAdapterForFhirSubscriptions =>

          Behaviors.receiveMessage[Command] {
            //case GetSubscriptionDimensions(rtype, replyTo) =>
            //  replyTo.tell(getSubscriptionDimensions(subscriptionConfig, fhirSearchParameterCache, rtype))
            //  Behaviors.same

            /**
             * Retrieving subscriptions (criteria and subscription ids) for a resource type
             */
            case GetCriteriaSubscriptions(rtype, dimensionParamAndValue, replyTo) =>
              val key = getCriteriaSubscriptionKey(rtype, dimensionParamAndValue)
              replicatorAdapterForCriteriaSubscriptions.askGet(
                askReplyTo => Replicator.Get(key, Replicator.ReadLocal, askReplyTo),
                value => InternalGetResponseForCriteriaSubscriptions(key._id, value, replyTo))
              Behaviors.same

            /**
             * Retrieving details of a specific subscription
             */
            case GetSubscription(id, replyTo) =>
              val key = getSubscriptionKey(id)
              replicatorAdapterForFhirSubscriptions.askGet(
                askReplyTo => Replicator.Get(key, Replicator.ReadLocal, askReplyTo),
                value => InternalGetResponseForSubscription(id, value, replyTo))
              Behaviors.same

            /**
             * New subscription
             */
            case AddOrUpdateSubscription(fhirSubscription, replyTo) =>
              //Write the subscription
              val key = getSubscriptionKey(fhirSubscription.id)

              //If subscription status is active (all apart from web-socket)
              val innerCommand =
                if(fhirSubscription.status > 0)
                  Some(InternalAddCriteriaSubscription(fhirSubscription, replyTo))
                else
                  None

              replicatorAdapterForFhirSubscriptions.askUpdate(
                askReplyTo => Replicator.Update(key, ORMap.empty[String, FhirSubscription], WriteLocal, askReplyTo)(_ :+ (fhirSubscription.id -> fhirSubscription)),
                rsp => InternalUpdateResponseForFhirSubscription(innerCommand, rsp, replyTo)
              )
              Behaviors.same

            /**
             * Activate an requested and waiting subscription
             */
            case ActivateSubscription(id, replyTo) =>
              val key = getSubscriptionKey(id)
              replicatorAdapterForFhirSubscriptions.askGet(
                askReplyTo => Replicator.Get(key, Replicator.ReadLocal, askReplyTo),
                {
                  case rsp@GetSuccess(_) =>
                    rsp.dataValue.get(key.id) match {
                      case None => InternalOpResponse(false, replyTo)
                      case Some(fs) => InternalAddCriteriaSubscription(fs, replyTo)
                    }
                  case _ => InternalOpResponse(false, replyTo)
                }
              )
              Behaviors.same

            /**
             * Deactivate a subscription (remove the criteria)
             */
            case DeactivateSubscription(id, replyTo) =>
              val key = getSubscriptionKey(id)
              replicatorAdapterForFhirSubscriptions.askGet(
                askReplyTo => Replicator.Get(key, Replicator.ReadLocal, askReplyTo),
                {
                  case rsp@GetSuccess(_) =>
                    rsp.dataValue.get(key.id) match {
                      case None => InternalOpResponse(false, replyTo)
                      case Some(fs) => InternalRemoveCriteriaSubscription(fs, replyTo)
                    }
                  case _ => InternalOpResponse(false, replyTo)
                }
              )
              Behaviors.same

            /**
             * Completely delete the subscrription
              */
            case RemoveSubscription(id, replyTo) =>
              val key = getSubscriptionKey(id)
              replicatorAdapterForFhirSubscriptions.askGet(
                askReplyTo => Replicator.Get(key, Replicator.ReadLocal, askReplyTo),
                {
                  case rsp@GetSuccess(_) =>
                    rsp.dataValue.get(key.id) match {
                      case None => InternalOpResponse(false, replyTo)
                      case Some(fs) => InternalRemoveSubscription(fs, replyTo)
                    }
                  case _ => InternalOpResponse(false, replyTo)
                }
              )
              Behaviors.same
            /**
             * Handle internal commands
             */
            case internal: InternalCommand =>
              internal match {
                /**
                 *  Handle replicator responses
                 */

                //Update of criteria list
                case InternalUpdateResponseForCriteriaSubscriptions(rsp, replyTo) =>
                  rsp match {
                    case Replicator.UpdateSuccess(_) => replyTo.tell(true)
                    case Replicator.UpdateFailure(_) =>
                      ctx.log.warn("Problem while updating criteria subscriptions !")
                      replyTo.tell(false)
                  }

                  Behaviors.same // ok

                //Update of fhir subscriptions
                case InternalUpdateResponseForFhirSubscription(internalCommand, rsp @ Replicator.UpdateSuccess(_), replyTo) =>
                  rsp match {
                    case  Replicator.UpdateSuccess(_) =>
                      internalCommand match {
                        case None => replyTo.tell(true)
                        case Some(c) => ctx.self.tell(c)
                      }
                  }
                  Behaviors.same

                //Response to retrieving criteria
                case InternalGetResponseForCriteriaSubscriptions(key, rsp, replyTo) =>
                  rsp match {
                    case s @ Replicator.GetSuccess(d) =>
                      val value = s.dataValue.get(key).map(_.entries.values).getOrElse(Nil).toSeq
                      replyTo ! value
                    case _ =>
                      ctx.log.warn("Problem while retrieving data from Akka Distributed data for key {}", key)
                      //We don't care errors for now
                      replyTo ! Nil
                  }

                  Behaviors.same

                //Response to retrieving subscription
                case InternalGetResponseForSubscription(key, rsp, replyTo) =>
                  rsp match {
                    case s @ Replicator.GetSuccess(_) =>
                      val value = s.dataValue.get(key)
                      replyTo ! value
                    case _ =>
                      ctx.log.warn("Problem while retrieving data from Akka Distributed data for key {}", key)
                      replyTo ! None
                  }
                  Behaviors.same

                //Generic boolean response
                case InternalOpResponse(r, replyTo) =>
                  replyTo ! r
                  Behaviors.same

                //Internal request to add the subscription to criteria list
                case InternalAddCriteriaSubscription(fhirSubscription, replyTo) =>
                  //TODO dimensions are not implemented
                  val key = getCriteriaSubscriptionKey(fhirSubscription.rtype, None)

                  replicatorAdapterForCriteriaSubscriptions.askUpdate(
                    askReplyTo =>
                      Replicator.Update(key, ORMap.empty[String, ORMap[String, CriteriaSubscriptions]], WriteLocal, askReplyTo)
                      ( l =>
                        l.get(key.id) match {
                          case None =>
                            l :+ (key.id -> (
                              ORMap.create[String, CriteriaSubscriptions]() :+
                                fhirSubscription.getCriteriaHash -> CriteriaSubscriptions(fhirSubscription.rtype, fhirSubscription.criteria, mutable.HashSet(fhirSubscription.id))
                              ))
                          case Some(csForResourceType) =>
                            l :+ (key.id ->
                              (csForResourceType.get(fhirSubscription.getCriteriaHash) match {
                              case None => csForResourceType :+ (fhirSubscription.getCriteriaHash -> CriteriaSubscriptions(fhirSubscription.rtype, fhirSubscription.criteria, mutable.HashSet(fhirSubscription.id)))
                              case Some(cs) =>
                                cs.subscriptionIds.add(fhirSubscription.id)
                                csForResourceType
                            }))
                        }
                      ),
                    rsp => InternalUpdateResponseForCriteriaSubscriptions(rsp, replyTo)
                  )
                  Behaviors.same

                //Internal request to remove the subscription from criteria list
                case InternalRemoveCriteriaSubscription(fhirSubscription, replyTo) =>
                  val key = getCriteriaSubscriptionKey(fhirSubscription.rtype, None)

                  replicatorAdapterForCriteriaSubscriptions.askUpdate(
                    askReplyTo =>
                      Replicator.Update(key, ORMap.empty[String, ORMap[String, CriteriaSubscriptions]], WriteLocal, askReplyTo)
                      ( l =>
                        l.get(key.id) match {
                          case None => l
                          case Some(csForResourceType) =>
                            val temp = csForResourceType.get(fhirSubscription.getCriteriaHash)
                            temp.foreach(cs => cs.subscriptionIds.remove(fhirSubscription.id))
                            l :+ (
                              key.id ->
                                (
                                  if(temp.exists(_.subscriptionIds.isEmpty))
                                    csForResourceType.remove(fhirSubscription.getCriteriaHash)
                                  else
                                    csForResourceType
                                  )
                              )
                        }
                      ),
                    rsp => InternalUpdateResponseForCriteriaSubscriptions(rsp, replyTo)
                  )
                  Behaviors.same

                //Internal request to remove the subscription completely
                case InternalRemoveSubscription(fhirSubscription, replyTo) =>
                  val key = getSubscriptionKey(fhirSubscription.id)
                  replicatorAdapterForFhirSubscriptions.askDelete(askReplyTo =>
                    Replicator.Delete(key, WriteLocal, askReplyTo),
                    {
                      case rsp @ DeleteSuccess(_) => InternalRemoveCriteriaSubscription(fhirSubscription, replyTo)
                      case _ => InternalOpResponse(false,replyTo)
                    })
                  Behaviors.same
              }
          }
        }
      }
      Behaviors.same
    }
  }


  private def getCriteriaSubscriptionKey(rtype:String, dimensionParamAndValue:Option[(String, String)]):ORMapKey[String, ORMap[String, CriteriaSubscriptions]] = {
    ORMapKey[String, ORMap[String, CriteriaSubscriptions]](s"$rtype${dimensionParamAndValue.map(d=> s":${d._1}:${d._2}").getOrElse("")}")
  }


  private def getSubscriptionKey(id:String):ORMapKey[String, FhirSubscription] = {
    ORMapKey[String, FhirSubscription](s"subscription:$id")
  }




}
