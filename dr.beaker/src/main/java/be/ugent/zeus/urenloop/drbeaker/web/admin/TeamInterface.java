package be.ugent.zeus.urenloop.drbeaker.web.admin;

import be.ugent.zeus.urenloop.drbeaker.StickManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import com.sun.jersey.api.view.Viewable;
import java.net.URI;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

/**
 *
 * @author Thomas Meire
 */
@Path("/admin/teams")
public class TeamInterface {

  private TeamManager teamManager = TeamManager.lookup();

  private StickManager stickManager = StickManager.lookup();

  @GET
  @Path("/")
  public Viewable showTeams() {
    return new Viewable("/admin/teams.jsp", teamManager.getAll());
  }

  @POST
  @Path("/add")
  public Response addNewTeam(@FormParam("teamname") String name) {
    Team team = new Team();
    team.setName(name);

    teamManager.add(team);

    return Response.seeOther(URI.create("/admin/teams/")).build();
  }

  @POST
  @Path("/delete")
  public Response removeTeam (@FormParam("teamname") String name) {
    teamManager.removeByName(name);
    return Response.seeOther(URI.create("/admin/teams")).build();
  }
  
  @POST
  @Path("/stick")
  public Response assignStick(@FormParam("team") long teamID, @FormParam("stick") long stickID, @FormParam("confirmation") @DefaultValue(value="false") boolean confirmation) {

    if (!confirmation) {
      Stick stick = stickManager.get(stickID);
      Team team = teamManager.get(teamID);
      return Response.ok(new Viewable("/admin/confirmation.jsp", new Object[]{team, stick})).build();
    }
    
    teamManager.assign(teamID, stickID);

    return Response.seeOther(URI.create("/admin/teams/")).build();
  }
}
