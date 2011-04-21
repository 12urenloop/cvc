package be.ugent.zeus.urenloop.drbeaker.api;

import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

/**
*
* @author Thomas Meire
*/
@Path("/api/teams/")
@Produces(MediaType.APPLICATION_JSON)
public class TeamAPI {

  private TeamManager teamManager = TeamManager.lookup();

  @GET
  @Path("/")
  public List<Team> list (@QueryParam("order") String sortOrder) {
    return teamManager.get(sortOrder);
  }
}