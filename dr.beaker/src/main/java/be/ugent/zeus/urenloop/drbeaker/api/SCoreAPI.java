package be.ugent.zeus.urenloop.drbeaker.api;

import be.ugent.zeus.urenloop.drbeaker.AuthenticationManager;
import be.ugent.zeus.urenloop.drbeaker.StickManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Group;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

/**
 *
 * @author Thomas Meire
 */
@Path("/api/0.1/")
public class SCoreAPI {

  private static final Logger logger = Logger.getLogger("12UL");

  private static final String[] macs = {"12:21:30:10:34", "12:49:49:28:37"};

  private TeamManager teamManager = TeamManager.lookup();

  private AuthenticationManager authManager = AuthenticationManager.lookup();

  private StickManager stickManager = StickManager.lookup();

  @PUT
  @Path("/{mac}/laps/increase/")
  public boolean addLap(@PathParam("mac") String macAddress,
          @FormParam("speed") double speed,
          @FormParam("suspicious") boolean suspicious) {
    logger.log(Level.SEVERE, "mac: {0}, speed: {1}, suspicious: {2}", new Object[]{macAddress, speed, suspicious});

    Stick stick = stickManager.get(macAddress);

    Team team = stick.getTeam();
    if (!suspicious) {
      teamManager.addTeamLap(team);
    } else {
      teamManager.addTeamLap(team);
    }
    return true;
  }

  @GET
  @Path("/bootstrap")
  public String bootstrap() {
    // add all sticks to the system
    Stick stick;
    for (String mac : macs) {
      stick = new Stick();
      stick.setMac(mac);
      stickManager.add(stick);
    }

    // add the user groups
    Group admins = new Group();
    admins.setName("administrator");

    Group moderators = new Group();
    moderators.setName("moderator");

    authManager.add(admins);
    authManager.add(moderators);

    // add an admin user
    User user = new User();
    user.setUsername("admin");
    user.setPassword("admin");

    authManager.add(user);
    authManager.addUserToGroup(user, admins);

    // add various dummy teams
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
