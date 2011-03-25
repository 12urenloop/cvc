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
import javax.ws.rs.core.Response;

/**
 *
 * @author Thomas Meire
 */
@Path("/api/0.1/")
public class SCoreAPI {

  private static final Logger logger = Logger.getLogger("12UL");

  private static final String[] macs = {"00:00:00:00:00:01", "00:00:00:00:00:02", "00:00:00:00:00:03", "00:00:00:00:00:04"};

  private TeamManager teamManager = TeamManager.lookup();

  private AuthenticationManager authManager = AuthenticationManager.lookup();

  private StickManager stickManager = StickManager.lookup();

  @PUT
  @Path("/{mac}/laps/increase/")
  public Response addLap(@PathParam("mac") String macAddress,
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
    return Response.ok().build();
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
    User user1 = new User();
    user1.setUsername("admin");
    user1.setPassword("admin");

    authManager.add(user1);
    authManager.addUserToGroup(user1, admins);

    User user2 = new User();
    user2.setUsername("mod");
    user2.setPassword("mod");

    authManager.add(user2);
    authManager.addUserToGroup(user2, moderators);

    // add various dummy teams
    Team t1 = new Team();
    t1.setName("bulbasaur");
    teamManager.add(t1);

    teamManager.assign(t1, stickManager.get(1));

    Team t2 = new Team();
    t2.setName("machop");
    teamManager.add(t2);

    teamManager.assign(t2, stickManager.get(2));

    Team t3 = new Team();
    t3.setName("mankey");
    teamManager.add(t3);

    teamManager.assign(t3, stickManager.get(3));

    Team t4 = new Team();
    t4.setName("charmander");
    teamManager.add(t4);

    teamManager.assign(t4, stickManager.get(4));

    return "OK";
  }
}
