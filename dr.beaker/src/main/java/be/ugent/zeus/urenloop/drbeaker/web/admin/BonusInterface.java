
package be.ugent.zeus.urenloop.drbeaker.web.admin;

import be.ugent.zeus.urenloop.drbeaker.AuthenticationManager;
import be.ugent.zeus.urenloop.drbeaker.ScoreManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
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
@Path("/admin/bonus")
public class BonusInterface {

  private AuthenticationManager authManager = AuthenticationManager.lookup();
  private TeamManager teamManager = TeamManager.lookup();
  private ScoreManager scoreManager = ScoreManager.lookup();

  @Context
  private HttpServletRequest request;

  @GET
  @Path("/")
  public Viewable showBonusses() {
    return new Viewable("/admin/bonusses.jsp", teamManager.get());
  }

  @POST
  @Path("/add")
  public Response addBonus(@FormParam("team") Long id, @FormParam("bonus") int bonus, @FormParam("reason") String reason) {
    Team team = teamManager.get(id);
    User user = authManager.getUser(request.getUserPrincipal().getName());
    scoreManager.addBonus(user, team, bonus, reason);

    return Response.seeOther(URI.create("/admin/bonus")).build();
  }

  @POST
  @Path("/multi-add")
  public Response addBonusses(@FormParam("teams") List<Long> ids, @FormParam("bonus") int bonus, @FormParam("reason") String reason) {
    User user = authManager.getUser(request.getUserPrincipal().getName());

    for (Long id : ids) {
      scoreManager.addBonus(user, teamManager.get(id), bonus, reason);
    }
    return Response.seeOther(URI.create("/admin/bonus")).build();
  }
}
