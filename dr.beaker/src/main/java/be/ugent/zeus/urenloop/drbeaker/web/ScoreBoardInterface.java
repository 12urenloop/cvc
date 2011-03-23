
package be.ugent.zeus.urenloop.drbeaker.web;

import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import com.sun.jersey.api.view.Viewable;
import java.net.URI;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

/**
 *
 * @author Thomas Meire
 */
@Path("/")
public class ScoreBoardInterface {

  private TeamManager teamManager = TeamManager.lookup();

  @GET
  @Path("/")
  public Viewable index () {
    return new Viewable("/index.jsp", teamManager.get("score"));
  }

  @GET
  @Path("/login")
  public Response login() {
    return Response.seeOther(URI.create("/")).build();
  }
}
