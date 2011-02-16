
package be.ugent.zeus.urenloop.score.api;

import javax.ws.rs.FormParam;
import javax.ws.rs.Path;

/**
 *
 * @author Thomas Meire
 */
@Path("/0.1/")
public class SCoreAPI {

  @Path("/rounds/increase/")
  public void increaseRound(@FormParam("mac") String macAddress) {

  }
}
