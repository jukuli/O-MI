package agentSystem;

import scala.concurrent.Future;
import scala.concurrent.ExecutionContext;
import types.omi.WriteRequest;
import types.omi.ReadRequest;
import types.omi.CallRequest;
import types.omi.ResponseRequest;


public interface ResponsibleInternalAgent extends InternalAgent{
  /**
   * Method to be called when a WriteRequest  is received.
   */
  public Future<ResponseRequest> handleWrite(WriteRequest write);
  //public Future<ResponseRequest>  handleRead(ReadRequest read);
  public Future<ResponseRequest> handleCall(CallRequest call);

}
