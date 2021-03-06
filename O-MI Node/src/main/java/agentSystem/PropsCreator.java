package agentSystem;
import com.typesafe.config.Config;
import akka.actor.Props;
import akka.actor.ActorRef;

public interface PropsCreator{
  public Props props( Config config, ActorRef requestHandler, ActorRef dbHandler );
}
