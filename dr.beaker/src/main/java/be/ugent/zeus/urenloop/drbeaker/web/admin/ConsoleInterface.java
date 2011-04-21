/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package be.ugent.zeus.urenloop.drbeaker.web.admin;

import be.ugent.zeus.urenloop.drbeaker.ScoreManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import com.sun.jersey.api.view.Viewable;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;

/**
 *
 * @author Thomas Meire
 */
@Path("/admin/console")
public class ConsoleInterface {

  private TeamManager teamManager = TeamManager.lookup();

  private ScoreManager scoreManager = ScoreManager.lookup();

  @GET
  public Viewable showManualConsole() {
    return new Viewable("/admin/console.jsp", new Object[]{scoreManager, teamManager.get()});
  }

  @POST
  public Viewable addLapViaConsole(@FormParam("team") long id) {
    Team team = teamManager.get(id);
    scoreManager.addLap("console", team, 0, null);
    return new Viewable("/admin/console.jsp", new Object[]{scoreManager, teamManager.get()});
  }
}
