/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package be.ugent.zeus.urenloop.drbeaker.web.admin;

import be.ugent.zeus.urenloop.drbeaker.AuthenticationManager;
import be.ugent.zeus.urenloop.drbeaker.ScoreManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import com.sun.jersey.api.view.Viewable;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.core.Context;

/**
 *
 * @author Thomas Meire
 */
@Path("/admin/console")
public class ConsoleInterface {

  private TeamManager teamManager = TeamManager.lookup();

  private ScoreManager scoreManager = ScoreManager.lookup();
  
  private AuthenticationManager authenticationManager = AuthenticationManager.lookup();

  @Context
  private HttpServletRequest request;

  @GET
  public Viewable showManualConsole() {
    return new Viewable("/admin/console.jsp", new Object[]{scoreManager, teamManager.getAll()});
  }

  @POST
  public Viewable addLapViaConsole(@FormParam("team") long id) {
    User user = authenticationManager.getUser(request.getUserPrincipal().getName());
    Team team = teamManager.get(id);
    scoreManager.addLapFromConsole(user, team);
    return new Viewable("/admin/console.jsp", new Object[]{scoreManager, teamManager.getAll()});
  }
}
