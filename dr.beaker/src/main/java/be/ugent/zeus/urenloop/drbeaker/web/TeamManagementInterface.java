package be.ugent.zeus.urenloop.drbeaker.web;

import be.ugent.zeus.urenloop.drbeaker.AuthenticationManager;
import be.ugent.zeus.urenloop.drbeaker.StickManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import com.sun.jersey.api.view.Viewable;
import java.net.URI;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;

/**
 *
 * @author Thomas Meire
 */
@Path("/manage/team")
public class TeamManagementInterface {

  private TeamManager teamManager = TeamManager.lookup();

  private StickManager stickManager = StickManager.lookup();

  private AuthenticationManager authManager = AuthenticationManager.lookup();

  @GET
  @Path("/")
  public Viewable index() {
    return new Viewable("/team/index.jsp", teamManager.get());
  }

  @POST
  @Path("/add")
  public Response addNewTeam(@FormParam("teamname") String name) {
    Team team = new Team();
    team.setName(name);

    teamManager.add(team);

    return Response.seeOther(URI.create("/manage/team/")).build();
  }

  @POST
  @Path("/stick")
  public Response assignStick(@FormParam("team") long teamID, @FormParam("stick") long stickID) {
    Team team = teamManager.get(teamID);
    Stick stick = stickManager.get(stickID);
    System.err.println(teamID + " - " + stickID);
    teamManager.assign(team, stick);

    return Response.seeOther(URI.create("/manage/team/")).build();
  }

  @POST
  @Path("/bonus")
  public Response addBonusForTeam(@Context HttpServletRequest request,
          @FormParam("team") long id, @FormParam("bonus") int bonus,
          @FormParam("reason") String reason) {

    Team team = teamManager.get(id);

    User user = null;
    if (request != null && request.getUserPrincipal() != null) {
      user = authManager.getUser(request.getUserPrincipal().getName());
    }

    teamManager.addTeamBonus(user, team, bonus, reason);
    return Response.seeOther(URI.create("/")).build();
  }
}
