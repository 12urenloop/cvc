
package be.ugent.zeus.urenloop.drbeaker.api;

import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import com.sun.jersey.api.core.HttpResponseContext;
import java.util.List;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 *
 * @author Thomas Meire
 */
@Path("/api/teams/")
@Produces(MediaType.APPLICATION_JSON)
public class TeamAPI {

  private TeamManager teamManager = TeamManager.lookup();

  @POST
  @Path("/")
  public Team add (@Context HttpResponseContext response, @FormParam("name") String name) {
    Team team = new Team();
    team.setName(name);

    teamManager.add(team);

    // HTTP status 'created'
    response.setStatus(201);
    return team;
  }

  @GET
  @Path("/")
  public List<Team> list () {
    return teamManager.get();
  }

  @GET
  @Path("/{id}")
  public Response get (@PathParam("id") long id) {
    Team t = teamManager.get(id);
    if (t != null) {
      return Response.ok(t).build();
    } else {
      return Response.status(404).build();
    }
  }

  @DELETE
  @Path("/{id}")
  public Response delete (@PathParam("id") long id) {
    Team team = teamManager.get(id);
    teamManager.delete(team);
    // HTTP status 'deleted'
    return Response.status(204).build();
  }

  @PUT
  @Path("/{id}/score/increase")
  public Response increaseScore(@PathParam("id") long id) {
    Team team = teamManager.get(id);
    teamManager.addTeamLap(team);
    return Response.ok().build();
  }
}
