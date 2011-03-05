package be.ugent.zeus.urenloop.drbeaker.web;

import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import com.sun.jersey.api.view.Viewable;
import java.net.URI;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

/**
 *
 * @author Thomas Meire
 */
@Path("/manage/team")
public class TeamManagementInterface {

  private TeamManager teamManager = TeamManager.lookup();

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
  @Path("/bonus")
  public Response addBonusForTeam(@FormParam("team") long id, @FormParam("bonus") int bonus) {
    teamManager.addTeamBonus(id, bonus);
    return Response.seeOther(URI.create("/")).build();
  }
}
