package be.ugent.zeus.urenloop.drbeaker.api;

import be.ugent.zeus.urenloop.drbeaker.AuthenticationManager;
import be.ugent.zeus.urenloop.drbeaker.ScoreManager;
import be.ugent.zeus.urenloop.drbeaker.StickManager;
import be.ugent.zeus.urenloop.drbeaker.TeamManager;
import be.ugent.zeus.urenloop.drbeaker.db.Group;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

/**
 *
 * @author Thomas Meire
 */
@Path("/api/0.1/")
public class SCoreAPI {

  private static final Logger logger = Logger.getLogger("12UL");

  private TeamManager teams = TeamManager.lookup();

  private AuthenticationManager authManager = AuthenticationManager.lookup();

  private StickManager stickManager = StickManager.lookup();

  private ScoreManager scoreManager = ScoreManager.lookup();

  private Response error(String message) {
    return Response.status(Status.INTERNAL_SERVER_ERROR).entity(message).build();
  }

  @PUT
  @Path("/{mac}/laps/increase/")
  public Response addLap(@Context HttpServletRequest request,
          @PathParam("mac") String macAddress,
          @FormParam("speed") double speed,
          @FormParam("warning") List<String> warnings) {

    String source = request.getRemoteAddr();

    logger.log(Level.INFO, "mac: {0}, speed: " + speed + ", warnings: {2}, origin: {3}", new Object[]{macAddress, speed, warnings, source});

    try {
      scoreManager.addLap(source, macAddress, speed, warnings);
    } catch (Exception e) {
      return error(e.getMessage());
    }
    return Response.status(Status.NO_CONTENT).build();
  }
  private static final String[] macs = {"00:00:00:00:00:01", "00:00:00:00:00:02", "00:00:00:00:00:03", "00:00:00:00:00:04"};

  @GET
  @Path("/bootstrap")
  public String bootstrap() {
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

    // add test machine @ zeus as current source
    scoreManager.setCurrentSource("10.1.2.43");

    if (false) {
      // add all sticks to the system
      Stick stick;
      for (String mac : macs) {
        stick = new Stick();
        stick.setMac(mac);
        stickManager.add(stick);
      }

      // add various dummy teams
      Team t1 = new Team();
      t1.setName("bulbasaur");
      teams.add(t1);

      teams.assign(t1, stickManager.get(1));

      Team t2 = new Team();
      t2.setName("machop");
      teams.add(t2);

      teams.assign(t2, stickManager.get(2));

      Team t3 = new Team();
      t3.setName("mankey");
      teams.add(t3);

      teams.assign(t3, stickManager.get(3));

      Team t4 = new Team();
      t4.setName("charmander");
      teams.add(t4);

      teams.assign(t4, stickManager.get(4));
    }
    return "OK";
  }
}
