package io.onfhir.subscription

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.cluster.sharding.external.ExternalShardAllocationStrategy
import akka.cluster.sharding.typed.scaladsl.{ClusterSharding, Entity, EntityTypeKey}
import akka.kafka.cluster.sharding.KafkaClusterSharding
import io.onfhir.config.SearchParameterConf
import io.onfhir.subscription.cache.DistributedSearchParameterConfCache.GetSearchParameterConfs
import io.onfhir.subscription.cache.{DistributedSearchParameterConfCache, FhirSearchParameterCache, GetCriteriaSubscriptions, GetCriteriaSubscriptionsResponse}
import io.onfhir.subscription.config.SubscriptionConfig
import io.onfhir.subscription.model.{CborSerializable, ConsumerGroupIds, CriteriaSubscriptions}
import io.onfhir.subscription.util.MultipleQueryResourceChecker
import org.json4s.JsonAST.JObject

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
 * @author Tuncay Namlı
 * Evaluator for subscriptions with the given stream
 */
object SubscriptionEvaluator {

  sealed trait Command extends CborSerializable{
    val rtype:String
    val rid:String
    def getEntityId = rtype + ":" + rid
  }

  /**
   * Command to retrieve subscription ids for satisfied subscriptions for this resource
   * @param rtype               FHIR resource type
   * @param rid                 FHIR resource id
   * @param resourceContent     FHIR resource content in json format
   * @param replyTo             Actor to send the reply
   */
  case class GetSatisfiedSubscriptionsForResource(rtype:String, rid:String, resourceContent:String, replyTo: ActorRef[Seq[String]]) extends Command


  case class AdaptedGetCriterionSubscriptionsResponse(rtype:String,  rid:String, resourceContent:String, criteriaSubscriptions:Seq[CriteriaSubscriptions], replyTo: ActorRef[Seq[String]]) extends Command
  case class FailureInCriterionSubscriptionsResponse(rtype:String,  rid:String,  replyTo: ActorRef[Seq[String]]) extends Command

  case class CalculateAndSendSatisfiedSubscriptions(rtype:String, rid:String, searchParameters:Map[String, SearchParameterConf], resource:JObject, criteriaSubscriptions:Seq[CriteriaSubscriptions], replyTo: ActorRef[Seq[String]]) extends Command

  /**
   * Initialization of this actor
   * @param system                Typed actor system
   * @param subscriptionConfig    Configuration
   * @return
   */
  def init(system: ActorSystem[_], fhirSearchParameterCache: ActorRef[DistributedSearchParameterConfCache.Command], subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command], subscriptionConfig: SubscriptionConfig): Future[ActorRef[Command]] = {
    import system.executionContext
    KafkaClusterSharding(subscriptionConfig.system).messageExtractorNoEnvelope(
      timeout = 10.seconds,
      topic = subscriptionConfig.kafkaFhirTopic,
      entityIdExtractor = (msg: Command) => msg.getEntityId,
      settings = subscriptionConfig.kafkaConsumerSettings(ConsumerGroupIds.kafkaConsumerGroupIdForResources)
    ).map(messageExtractor => {
      system.log.info("Message extractor created. Initializing sharding for group {}", ConsumerGroupIds.kafkaConsumerGroupIdForResources)
      ClusterSharding(system).init(
        Entity(subscriptionConfig.entityTypeKeyForResources)(createBehavior = _ => SubscriptionEvaluator(fhirSearchParameterCache, subscriptionCache, subscriptionConfig))
          .withAllocationStrategy(new ExternalShardAllocationStrategy(system, subscriptionConfig.entityTypeKeyForResources.name))
          .withMessageExtractor(messageExtractor))
    })
  }

  def apply(fhirSearchParameterCache: ActorRef[DistributedSearchParameterConfCache.Command], subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command], subscriptionConfig: SubscriptionConfig): Behavior[Command] =
    running(fhirSearchParameterCache, subscriptionCache, subscriptionConfig)

  private def running(fhirSearchParameterCache: ActorRef[DistributedSearchParameterConfCache.Command], subscriptionCache:ActorRef[io.onfhir.subscription.cache.Command], subscriptionConfig: SubscriptionConfig): Behavior[Command] = {
    Behaviors.setup { ctx =>
      implicit val responseTimeout = subscriptionConfig.processorAskTimeout
      Behaviors.receiveMessage[Command] {
        case GetSatisfiedSubscriptionsForResource(rtype, rid, resource, ack) =>
          ctx.log.debug(s"Processing subscriptions for resource type $rtype with id $rid...")

          ctx.ask[io.onfhir.subscription.cache.Command, io.onfhir.subscription.cache.Response](subscriptionCache, rt => GetCriteriaSubscriptions(rtype, None, rt)) {
            case Success(GetCriteriaSubscriptionsResponse(criteriaSubscriptions)) =>
              AdaptedGetCriterionSubscriptionsResponse(rtype, rid, resource, criteriaSubscriptions, ack)
            case Failure(ex) =>
              ctx.log.error(s"Problem while retrieving subscribed criteria for resource type $rtype!", ex)
              FailureInCriterionSubscriptionsResponse(rtype, rid, ack)
          }
          Behaviors.same

        case AdaptedGetCriterionSubscriptionsResponse(rtype, rid, resourceContent, criteriaSubscriptions, ack) =>
          //If there is no subscription
          if(criteriaSubscriptions.isEmpty) {
            ctx.log.debug(s"No subscription for resource type $rtype, resource $rid is evaluated...")
            ack.tell(Nil)
          } else {
            import io.onfhir.util.JsonFormatter._
            val resource = resourceContent.parseJson

            val allMentionedParameters:Set[String] = criteriaSubscriptions.flatMap(_.criteria.map(_.name)).toSet
            ctx.ask[DistributedSearchParameterConfCache.Command, DistributedSearchParameterConfCache.Response](fhirSearchParameterCache, rt => GetSearchParameterConfs(rtype, allMentionedParameters, rt)) {
              case Success(DistributedSearchParameterConfCache.GetSearchParamConfsResponse(foundSearchParamConfs)) =>
                CalculateAndSendSatisfiedSubscriptions(rtype, rid, foundSearchParamConfs, resource, criteriaSubscriptions, ack)
              case Failure(ex) => FailureInCriterionSubscriptionsResponse(rtype, rid, ack)
            }
          }

        Behaviors.same

        case CalculateAndSendSatisfiedSubscriptions(rtype, rid, searchParameters, resource, criteriaSubscriptions, replyTo) =>
          val resourceChecker = new MultipleQueryResourceChecker(rtype, searchParameters)
          //Find the satisfied criteria
          val satisfiedCriteria = resourceChecker.filterSatisfiedCriteriaForResource(criteriaSubscriptions.map(_.criteria), resource)
          val satisfiedSubscriptionIds =
            criteriaSubscriptions
              .map(_.subscriptionIds)
              .zip(satisfiedCriteria) //zip with results
              .filter(_._2) //only get the satisfied ones
              .flatMap(_._1.elements) //Get the ids

          ctx.log.debug(s"Resource $rid is evaluated successfully, satisfied subscriptions: (${satisfiedSubscriptionIds.mkString(",")})...")

          replyTo.tell(satisfiedSubscriptionIds)
          Behaviors.same
        case FailureInCriterionSubscriptionsResponse(rtype, rid, ack) =>
          ctx.log.warn(s"Problem while retrieving subscription criteria for $rtype, skipping resource $rid for evaluations!")
          ack.tell(Nil)
          Behaviors.same
      }
    }
  }
}
