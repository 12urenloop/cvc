
package be.ugent.zeus.urenloop.drbeaker.web;

import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import com.sun.jersey.api.view.Viewable;
import javax.ws.rs.GET;
import javax.ws.rs.Path;

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
    return new Viewable("/index.jsp", teamManager.getByScore());
  }
}
