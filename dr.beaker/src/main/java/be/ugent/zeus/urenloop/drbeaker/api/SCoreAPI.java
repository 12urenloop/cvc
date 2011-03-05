package be.ugent.zeus.urenloop.drbeaker.api;

import be.ugent.zeus.urenloop.drbeaker.AuthenticationManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Group;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;

/**
 *
 * @author Thomas Meire
 */
@Path("/api/0.1/")
public class SCoreAPI {

  private TeamManager teamManager = TeamManager.lookup();

  private AuthenticationManager authManager = AuthenticationManager.lookup();

  @PUT
  @Path("/laps/increase/")
  public void addLap(@FormParam("mac") String macAddress) {
    Team team = teamManager.get(macAddress);
    team.increaseScore();
  }

  @GET
  @Path("/bootstrap")
  public String bootstrap() {
    Group admins = new Group();
    admins.setName("administrator");

    Group moderators = new Group();
    moderators.setName("moderator");

    authManager.add(admins);
    authManager.add(moderators);

    User user = new User();
    user.setUsername("admin");
    user.setPassword("admin");

    authManager.add(user);
    authManager.addUserToGroup(user, admins);

    Team t1 = new Team();
    t1.setName("WiNA");
    t1.setScore(4);
    teamManager.add(t1);

    Team t2 = new Team();
    t2.setName("VTK");
    t2.setScore(2);
    teamManager.add(t2);

    Team t3 = new Team();
    t3.setName("HILOK");
    t3.setScore(3);
    teamManager.add(t3);

    Team t4 = new Team();
    t4.setName("SK");
    t4.setScore(1);
    teamManager.add(t4);

    Team t5 = new Team();
    t5.setName("Zeus WPI");
    t5.setScore(42);
    teamManager.add(t5);

    return "OK";
  }
}
