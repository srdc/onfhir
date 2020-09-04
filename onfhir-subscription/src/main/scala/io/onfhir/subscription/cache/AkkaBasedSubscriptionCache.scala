package io.onfhir.subscription.cache

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.{ORMap, ORMapKey, ORSet, PNCounter, SelfUniqueAddress}
import akka.cluster.ddata.typed.scaladsl.Replicator._
import io.onfhir.api.{SubscriptionChannelTypes, SubscriptionStatusCodes}
import io.onfhir.subscription.config.SubscriptionConfig
import io.onfhir.subscription.model.{CriteriaSubscriptions, DDFhirSubscription}
import scala.concurrent.duration._
import scala.collection.mutable

object AkkaBasedSubscriptionCache {

  /**
   * Internal commands to communicate with replicator
   */
  private sealed trait InternalCommand extends Command
  //Internal responses handled
  private case class InternalUpdateResponseForCriteriaSubscriptions(rsp: Replicator.UpdateResponse[ORMap[String, ORMap[String,CriteriaSubscriptions]]], replyTo:ActorRef[Response]) extends InternalCommand
  private case class InternalUpdateResponseForFhirSubscription(internalCommand:Option[InternalCommand], rsp:Replicator.UpdateResponse[ORMap[String, DDFhirSubscription]], replyTo:ActorRef[Response]) extends InternalCommand
  private case class InternalGetResponseForCriteriaSubscriptions(key:String, rsp: Replicator.GetResponse[ORMap[String, ORMap[String,CriteriaSubscriptions]]], replyTo: ActorRef[Response]) extends InternalCommand
  private case class InternalGetResponseForSubscription(sid:String, rsp:Replicator.GetResponse[ORMap[String, DDFhirSubscription]], replyTo:ActorRef[Response]) extends InternalCommand
  private case class InternalOpResponse(result:Boolean, replyTo:ActorRef[Response]) extends InternalCommand
  //Internal commands
  private case class InternalAddCriteriaSubscription(fhirSubscription: DDFhirSubscription, replyTo:ActorRef[Response]) extends InternalCommand
  private case class InternalRemoveCriteriaSubscription(fhirSubscription: DDFhirSubscription, replyTo:ActorRef[Response]) extends InternalCommand
  private case class InternalRemoveSubscription(fhirSubscription: DDFhirSubscription, replyTo:ActorRef[Response]) extends InternalCommand


  def apply(subscriptionConfig: SubscriptionConfig): Behavior[Command] = running(subscriptionConfig)

  /**
   * Actor Behavior
   * @param subscriptionConfig
   * @return
   */
  def running(subscriptionConfig: SubscriptionConfig): Behavior[Command] = {
    Behaviors.setup { ctx =>
      implicit val node: SelfUniqueAddress = DistributedData(ctx.system).selfUniqueAddress

      // adapter that turns the response messages from the replicator into our own protocol
      DistributedData.withReplicatorMessageAdapter[Command, ORMap[String, ORMap[String, CriteriaSubscriptions]]] { replicatorAdapterForCriteriaSubscriptions =>
        DistributedData.withReplicatorMessageAdapter[Command, ORMap[String, DDFhirSubscription]] { replicatorAdapterForFhirSubscriptions =>

          Behaviors.receiveMessage[Command] {
            /**
             * Retrieving subscriptions (criteria and subscription ids) for a resource type
             */
            case GetCriteriaSubscriptions(rtype, dimensionParamAndValue, replyTo) =>
              val key = getCriteriaSubscriptionKey(rtype.capitalize, dimensionParamAndValue)
              replicatorAdapterForCriteriaSubscriptions.askGet(
                askReplyTo => Replicator.Get(key, Replicator.ReadLocal, askReplyTo),
                value => InternalGetResponseForCriteriaSubscriptions(rtype.capitalize, value, replyTo))
              running(subscriptionConfig)

            /**
             * Retrieving details of a specific subscription
             */
            case GetSubscription(id, replyTo) =>
              val key = getSubscriptionKey(id)
              replicatorAdapterForFhirSubscriptions.askGet(
                askReplyTo => Replicator.Get(key, Replicator.ReadLocal, askReplyTo),
                value => InternalGetResponseForSubscription(id, value, replyTo))
              running(subscriptionConfig)

            /**
             * New subscription
             */
            case AddOrUpdateSubscription(fhirSubscription, replyTo) =>
              //Write the subscription
              val key = getSubscriptionKey(fhirSubscription.id)

              val statusCounter:PNCounter = PNCounter.empty :+ 1
              val ddFhirSubscription = DDFhirSubscription(fhirSubscription.id, fhirSubscription.rtype, fhirSubscription.channel, fhirSubscription.criteria, statusCounter, fhirSubscription.expiration )


              val innerCommand = Some(InternalAddCriteriaSubscription(ddFhirSubscription, replyTo))

              replicatorAdapterForFhirSubscriptions.askUpdate(
                askReplyTo => Replicator.Update(key, ORMap.empty[String, DDFhirSubscription], WriteMajority(60 seconds), askReplyTo)(_ :+ (fhirSubscription.id -> ddFhirSubscription)),
                rsp => InternalUpdateResponseForFhirSubscription(innerCommand, rsp, replyTo)
              )
              running(subscriptionConfig)

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
                      case Some(fs) if(fs.channel.channelType == SubscriptionChannelTypes.WebSocket) => InternalAddCriteriaSubscription(fs, replyTo)
                      case _ => InternalOpResponse(false, replyTo)
                    }
                  case _ => InternalOpResponse(false, replyTo)
                }
              )
              running(subscriptionConfig)

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
              running(subscriptionConfig)

            case SetSubscriptionStatus(id, status, replyTo) =>
              val key = getSubscriptionKey(id)
              replicatorAdapterForFhirSubscriptions.askUpdate(
                askReplyTo => Replicator.Update(key, ORMap.empty[String, DDFhirSubscription], WriteMajority(60 seconds), askReplyTo)(s => {
                  s.get(id) match {
                    case None => s
                    case Some(ddfs) =>
                      val result = s :+ ddfs.id -> ddfs.copy(status = ddfs.status.increment(status - ddfs.status.getValue.intValue()))
                      result
                  }
                }),
                {
                  case rsp@UpdateSuccess(_) =>InternalOpResponse(true, replyTo)
                  case _ => InternalOpResponse(false, replyTo)
                }
              )
              running(subscriptionConfig)
            /**
             * Completely delete the subscrription
              */
            case RemoveSubscription(id, replyTo) =>
              val key = getSubscriptionKey(id)

              replicatorAdapterForFhirSubscriptions.askGet(
                askReplyTo => Replicator.Get(key, Replicator.ReadLocal, askReplyTo),
                {
                  case rsp@GetSuccess(_) =>
                    rsp.dataValue.get(id) match {
                      case None => InternalOpResponse(false, replyTo)
                      case Some(fs) => InternalRemoveSubscription(fs, replyTo)
                    }
                  case _ => InternalOpResponse(false, replyTo)
                }
              )
              running(subscriptionConfig)
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
                    case Replicator.UpdateSuccess(_) => replyTo.tell(UpdateSubscriptionResponse(true))
                    case Replicator.UpdateFailure(_) =>
                      ctx.log.warn("Problem while updating criteria subscriptions !")
                      replyTo.tell(UpdateSubscriptionResponse(false))
                  }

                  running(subscriptionConfig)// ok

                //Update of fhir subscriptions
                case InternalUpdateResponseForFhirSubscription(internalCommand, rsp, replyTo) =>
                  rsp match {
                    case  Replicator.UpdateSuccess(_) =>
                      internalCommand match {
                        case None => replyTo.tell(UpdateSubscriptionResponse(true))
                        case Some(c) => ctx.self.tell(c)
                      }
                    case Replicator.UpdateFailure(ex) =>
                      ctx.log.warn("Problem while updating FHIR subscription details", ex)
                      replyTo.tell(UpdateSubscriptionResponse(false))
                  }
                  running(subscriptionConfig)

                //Response to retrieving criteria
                case InternalGetResponseForCriteriaSubscriptions(rtype, rsp, replyTo) =>
                  rsp match {
                    case s @ Replicator.GetSuccess(d) =>
                      val value = s.dataValue.get(rtype).map(_.entries.values).getOrElse(Nil).toSeq
                      ctx.log.debug(s"${value.length} subscription criteria found for resource type $rtype!")
                      replyTo ! GetCriteriaSubscriptionsResponse(value)
                    case m @ Replicator.NotFound(key) =>
                      ctx.log.debug(s"No subsscription for resource type $rtype and key $key")
                      //We don't care errors for now
                      replyTo ! GetCriteriaSubscriptionsResponse(Nil)
                    case f @Replicator.GetFailure(ex) =>
                      ctx.log.error("Problem while retrieving data from Akka Distributed data for key $key",ex)
                      //We don't care errors for now
                      replyTo ! GetCriteriaSubscriptionsResponse(Nil)
                  }

                  running(subscriptionConfig)

                //Response to retrieving subscription
                case InternalGetResponseForSubscription(key, rsp, replyTo) =>
                  rsp match {
                    case s @ Replicator.GetSuccess(_) =>
                      val value = s.dataValue.get(key)
                      replyTo ! GetSubscriptionResponse(value)
                    case nf @ Replicator.NotFound(key) =>
                      ctx.log.warn(s"Key $key not found in Akka Distributed data ")
                      replyTo ! GetSubscriptionResponse(None)
                    case _ =>
                      ctx.log.warn("Problem while retrieving data from Akka Distributed data for key {}", key)
                      replyTo ! GetSubscriptionResponse(None)
                  }
                  running(subscriptionConfig)

                //Generic boolean response
                case InternalOpResponse(r, replyTo) =>
                  replyTo ! UpdateSubscriptionResponse(r)
                  Behaviors.same

                //Internal request to add the subscription to criteria list
                case InternalAddCriteriaSubscription(fhirSubscription, replyTo) =>
                  //TODO dimensions are not implemented
                  val key = getCriteriaSubscriptionKey(fhirSubscription.rtype, None)

                  replicatorAdapterForCriteriaSubscriptions.askUpdate(
                    askReplyTo =>
                      Replicator.Update(key, ORMap.empty[String, ORMap[String, CriteriaSubscriptions]], WriteAll(60 seconds), askReplyTo)
                      ( l =>
                        l.get(fhirSubscription.rtype) match {
                          case None =>
                            l :+ (fhirSubscription.rtype -> (
                              ORMap.create[String, CriteriaSubscriptions]() :+
                                fhirSubscription.getCriteriaHash -> CriteriaSubscriptions(fhirSubscription.rtype, fhirSubscription.criteria, ORSet.empty[String].:+(fhirSubscription.id))
                              ))
                          case Some(csForResourceType) =>
                            l :+ (fhirSubscription.rtype ->
                              (csForResourceType.get(fhirSubscription.getCriteriaHash) match {
                                case None =>
                                  csForResourceType :+ (fhirSubscription.getCriteriaHash -> CriteriaSubscriptions(fhirSubscription.rtype, fhirSubscription.criteria, ORSet.empty[String].:+(fhirSubscription.id)))
                                case Some(cs) =>
                                  csForResourceType :+ fhirSubscription.getCriteriaHash -> cs.copy(subscriptionIds = cs.subscriptionIds.add(node, fhirSubscription.id))
                            }))
                        }
                      ),
                    rsp => InternalUpdateResponseForCriteriaSubscriptions(rsp, replyTo)
                  )
                  running(subscriptionConfig)

                //Internal request to remove the subscription from criteria list
                case InternalRemoveCriteriaSubscription(fhirSubscription, replyTo) =>
                  val key = getCriteriaSubscriptionKey(fhirSubscription.rtype, None)

                  replicatorAdapterForCriteriaSubscriptions.askUpdate(
                    askReplyTo =>
                      Replicator.Update(key, ORMap.empty[String, ORMap[String, CriteriaSubscriptions]], WriteAll(60 seconds), askReplyTo)
                      ( l =>
                        l.get(fhirSubscription.rtype) match {
                          case None => l
                          case Some(csForResourceType) =>
                            val temp =
                              csForResourceType
                                .get(fhirSubscription.getCriteriaHash)
                                .map(cs => cs.copy(subscriptionIds = cs.subscriptionIds.remove(fhirSubscription.id)))
                            l :+ (
                              fhirSubscription.rtype ->
                                (
                                  if(temp.exists(_.subscriptionIds.isEmpty))
                                    csForResourceType.remove(fhirSubscription.getCriteriaHash)
                                  else if(temp.nonEmpty)
                                    csForResourceType.:+(fhirSubscription.getCriteriaHash, temp.get)
                                  else
                                    csForResourceType
                                  )
                              )
                        }
                      ),
                    rsp => InternalUpdateResponseForCriteriaSubscriptions(rsp, replyTo)
                  )
                  running(subscriptionConfig)

                //Internal request to remove the subscription completely
                case InternalRemoveSubscription(fhirSubscription, replyTo) =>
                  val key = getSubscriptionKey(fhirSubscription.id)

                  replicatorAdapterForFhirSubscriptions.askUpdate(askReplyTo =>
                    Replicator.Update(key, ORMap.empty[String, DDFhirSubscription], WriteAll(60 seconds), askReplyTo)(s => s.remove(fhirSubscription.id)), {
                    case rsp@UpdateSuccess(_) => InternalRemoveCriteriaSubscription(fhirSubscription, replyTo)
                    case _ =>
                      InternalOpResponse(false,replyTo)
                  })

                  running(subscriptionConfig)
              }
          }
        }
      }
    }
  }


  private def getCriteriaSubscriptionKey(rtype:String, dimensionParamAndValue:Option[(String, String)]):ORMapKey[String, ORMap[String, CriteriaSubscriptions]] = {
    ORMapKey[String, ORMap[String, CriteriaSubscriptions]](s"criteria:$rtype${dimensionParamAndValue.map(d=> s":${d._1}:${d._2}").getOrElse("")}")
  }


  private def getSubscriptionKey(id:String):ORMapKey[String, DDFhirSubscription] = {
    ORMapKey[String, DDFhirSubscription](s"subscription:$id")
  }




}
