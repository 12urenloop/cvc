package be.ugent.zeus.urenloop.drbeaker.web;

import be.ugent.zeus.urenloop.drbeaker.AuthenticationManager;
import be.ugent.zeus.urenloop.drbeaker.ScoreManager;
import be.ugent.zeus.urenloop.drbeaker.StickManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Group;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import com.sun.jersey.api.view.Viewable;
import java.net.URI;
import java.util.List;
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
@Path("/admin")
public class AdminInterface {

  private AuthenticationManager authManager = AuthenticationManager.lookup();

  private TeamManager teamManager = TeamManager.lookup();

  private StickManager stickManager = StickManager.lookup();
  private ScoreManager scoreManager = ScoreManager.lookup();

  @Context
  private HttpServletRequest request;

  @GET
  @Path("/")
  public Response index() {
    return Response.seeOther(URI.create("/admin/bonus")).build();
  }

  @GET
  @Path("/history")
  public Viewable showGlobalScoreHistory() {
    return new Viewable("/admin/history.jsp", teamManager.getHistory());
  }

  @GET
  @Path("/bonus")
  public Viewable showBonusses() {
    return new Viewable("/admin/bonusses.jsp", teamManager.get());
  }

  @POST
  @Path("/bonus/add")
  public Response addBonus(@FormParam("team") Long id, @FormParam("bonus") int bonus, @FormParam("reason") String reason) {
    Team team = teamManager.get(id);
    User user = authManager.getUser(request.getUserPrincipal().getName());
    scoreManager.addBonus(user, team, bonus, reason);

    return Response.seeOther(URI.create("/admin/bonus")).build();
  }

  @POST
  @Path("/bonusses/add")
  public Response addBonusses(@FormParam("teams") List<Long> ids, @FormParam("bonus") int bonus, @FormParam("reason") String reason) {
    User user = authManager.getUser(request.getUserPrincipal().getName());

    for (Long id : ids) {
      scoreManager.addBonus(user, teamManager.get(id), bonus, reason);
    }
    return Response.seeOther(URI.create("/admin/bonus")).build();
  }

  @GET
  @Path("/users")
  public Viewable showUsers() {
    return new Viewable("/admin/index.jsp", new Object[]{authManager.getUsers(), authManager.getGroups()});
  }

  @POST
  @Path("/users/add")
  public Response addUser(@FormParam("username") String username,
          @FormParam("password") String password,
          @FormParam("group") String groupName) throws Exception {

    if (username == null || password == null) {
      // TODO: handle this gracefully
      throw new Exception("Username and password shouldn't be null.");
    }

    User user = authManager.getUser(username);
    if (user != null) {
      // TODO: handle this gracefully
      throw new Exception("User " + username + " already existed.");
    }

    user = new User();
    user.setUsername(username);
    user.setPassword(password);
    authManager.add(user);

    Group group = authManager.getGroup(groupName);
    if (group != null) {
      authManager.addUserToGroup(user, group);
    }
    return Response.seeOther(URI.create("/admin/")).build();
  }

  @GET
  @Path("/teams")
  public Viewable showTeams() {
    return new Viewable("/admin/teams.jsp", teamManager.get());
  }

  @GET
  @Path("/console")
  public Viewable showManualConsole() {
    return new Viewable("/admin/console.jsp", new Object[]{scoreManager, teamManager.get()});
  }

  @POST
  @Path("/console")
  public Viewable addLapViaConsole(@FormParam("team") long id) {
    Team team = teamManager.get(id);
    scoreManager.addLap("console", team, 0, null);
    return new Viewable("/admin/console.jsp", new Object[]{scoreManager, teamManager.get()});
  }

  @GET
  @Path("/system")
  public Viewable showCounterSwitch() {
    return new Viewable("/admin/system.jsp", null);
  }

  @POST
  @Path("/system")
  public Response showCounterSwitch(@FormParam("name") String name) {
    scoreManager.setCurrentSource(name);
    return Response.seeOther(URI.create("/admin/system")).build();
  }

  @GET
  @Path("/sticks")
  public Viewable showSticks() {
    return new Viewable("/admin/sticks.jsp", stickManager.get());
  }
}
