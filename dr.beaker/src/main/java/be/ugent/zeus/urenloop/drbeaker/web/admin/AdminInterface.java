package be.ugent.zeus.urenloop.drbeaker.web.admin;

import be.ugent.zeus.urenloop.drbeaker.ScoreManager;
import be.ugent.zeus.urenloop.drbeaker.StickManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.HistoryEntry;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import com.sun.jersey.api.view.Viewable;
import java.net.URI;
import java.util.List;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

/**
 *
 * @author Thomas Meire
 */
@Path("/admin")
public class AdminInterface {

  private StickManager stickManager = StickManager.lookup();
  private ScoreManager scoreManager = ScoreManager.lookup();
  private TeamManager teamManager = TeamManager.lookup();

  @GET
  @Path("/")
  public Response index() {
    return Response.seeOther(URI.create("/admin/bonus")).build();
  }

  @GET
  @Path("/history")
  public Viewable showGlobalScoreHistory(@QueryParam("teamname") String name) {
    List<HistoryEntry> history = null;
    if (name != null && !name.trim().equals("")) {
      Team team = teamManager.get(name);
      if (team != null) {
        history = team.getHistory();
      }
    }
    if (history == null) {
      history = scoreManager.getHistory();
    }
    return new Viewable("/admin/history.jsp", history);
  }

  @GET
  @Path("/sticks")
  public Viewable showSticks() {
    return new Viewable("/admin/sticks.jsp", stickManager.get());
  }

  @POST
  @Path("/sticks")
  public Viewable showSticks(@FormParam("mac") String mac) {
    Stick stick = new Stick();
    stick.setMac(mac);
    stickManager.add(stick);

    return new Viewable("/admin/sticks.jsp", stickManager.get());
  }
}
