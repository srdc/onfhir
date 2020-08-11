package io.onfhir.subscription

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.cluster.sharding.external.ExternalShardAllocationStrategy
import akka.cluster.sharding.typed.scaladsl.{ClusterSharding, Entity, EntityTypeKey}
import akka.kafka.cluster.sharding.KafkaClusterSharding
import io.onfhir.api.Resource
import io.onfhir.subscription.config.SubscriptionConfig

import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * @author Tuncay Namlı
 * Evaluator for subscriptions with the given stream
 */
object SubscriptionEvaluator {

  sealed trait Command {
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

  /**
   * Initialization of this actor
   * @param system                Typed actor system
   * @param subscriptionConfig    Configuration
   * @return
   */
  def init(system: ActorSystem[_], subscriptionConfig: SubscriptionConfig): Future[ActorRef[Command]] = {
    import system.executionContext
    KafkaClusterSharding(subscriptionConfig.system).messageExtractorNoEnvelope(
      timeout = 10.seconds,
      topic = subscriptionConfig.kafkaFhirTopic,
      entityIdExtractor = (msg: Command) => msg.getEntityId,
      settings = subscriptionConfig.kafkaConsumerSettings
    ).map(messageExtractor => {
      system.log.info("Message extractor created. Initializing sharding for group {}", subscriptionConfig.kafkaConsumerGroupId)
      ClusterSharding(system).init(
        Entity(subscriptionConfig.entityTypeKey)(createBehavior = _ => SubscriptionEvaluator())
          .withAllocationStrategy(new ExternalShardAllocationStrategy(system, subscriptionConfig.entityTypeKey.name))
          .withMessageExtractor(messageExtractor))
    })
  }

  def apply(): Behavior[Command] = running()

  private def running(): Behavior[Command] = {
    Behaviors.setup { ctx =>
      Behaviors.receiveMessage[Command] {
        case GetSatisfiedSubscriptionsForResource(rtype, rid, resource, ack) =>
          ctx.log.info("Processing subscriptions for resource type {} with id {}...", rtype,rid)
          val sids = getSatisfiedSubscriptions(rtype, rid, resource)
          ack.tell(sids)
          running()
      }
    }
  }


  private def getSatisfiedSubscriptions(rtype:String, rid:String, resource: String):Seq[String] = {
    //TODO implement the logic
    Nil
  }

}
