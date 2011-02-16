
package be.ugent.zeus.urenloop.score.api;

import be.ugent.zeus.urenloop.score.TeamManager;
import be.ugent.zeus.urenloop.score.db.Team;
import javax.ws.rs.FormParam;
import javax.ws.rs.Path;

/**
 *
 * @author Thomas Meire
 */
@Path("/0.1/")
public class SCoreAPI {

  private TeamManager teamManager = TeamManager.lookup();

  @Path("/rounds/increase/")
  public void increaseRound(@FormParam("mac") String macAddress) {
    Team team = teamManager.getTeam(macAddress);
    team.increaseScore();
  }
}
