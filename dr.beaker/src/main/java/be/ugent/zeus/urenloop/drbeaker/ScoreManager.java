
package be.ugent.zeus.urenloop.drbeaker;

import be.ugent.zeus.urenloop.drbeaker.db.HistoryEntry;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;

/**
 *
 * @author Thomas Meire
 */
@Stateless
public class ScoreManager {

  private static final Logger logger = Logger.getLogger("12UL");

  @PersistenceContext(unitName = "scorePU")
  private EntityManager em;

  @EJB
  private StickManager stickManager;
  
  @EJB
  private TeamManager teamManager;
  
  private String source;

  public String getSource () {
    if (source == null) {
      // Use the system property as default source. If no property is set, use the console as default.
      source = System.getProperty("counter-source", "console");
    }
    return source;
  }

  public void setCurrentSource (String source) {
    System.setProperty("counter-source", source);
    this.source = source;
  }

  public void addLap (String source, String macAddress, double speed, List<String> warnings) throws Exception {
    // fetch the stick for the mac address
    Stick stick = stickManager.get(macAddress);
    if (stick == null) {
      // TODO: store this somewhere in the database
      logger.log(Level.WARNING, "The stick {0} was not known to the system!", macAddress);
      throw new Exception("The mac address is not known.");
    }
    // fetch the team for the mac address
    Team team = stick.getTeam();
    if (team == null) {
      // TODO: store this somewhere in the database
      logger.log(Level.WARNING, "The stick {0} was not assigned to a team!", stick.getId());
      throw new Exception("The mac address was not assigned to a team.");
    }

    addLap(source, team, speed, warnings);
  }

  public void addLap (String source, Team team, double speed, List<String> warnings) {
    // only accept from the current source
    if (!source.equals(getSource())) {
      logger.log(Level.WARNING, "Expected counting source {0}, got {1}", new Object[]{getSource(), source});
      em.persist(new HistoryEntry(null, team, 0, 0.0, "Rejected a lap for team " + team.getName() + " from wrong counter (" + source + ")"));
      return;
    }

    if (warnings == null) {
      warnings = new ArrayList<String>();
    }

    logger.log(Level.INFO, "Adding a lap for team {0} (speed: {1}, {2} warnings) from source {3}.",new Object[]{team.getName(), speed, warnings.size(), source});

    // update the score + average speed
    team.update(1);
    if (speed != 0) {
      team.updateAverageSpeed(speed);
      logger.log(Level.INFO, "New average speed {0}", new Object[]{team.getAverageSpeed()});
    }

    // store a message for the warning log
    if (!warnings.isEmpty()) {
    }

    em.merge(team);
    em.persist(new HistoryEntry(null, team, 1, speed, "Completed a lap from " + source + "."));
  }
  
  public void addLapFromConsole (User user, Team team) {
    if (!source.equals("console")) {
      em.persist(new HistoryEntry(user, team, 1, 0, "Rejected a lap from console (current source: " + source + ")"));
      return;
    }

    logger.log(Level.INFO, "Adding a lap for team {0} from source console.",new Object[]{team.getName()});

    team.update(1);
    em.merge(team);

    em.persist(new HistoryEntry(user, team, 1, 0, "Completed a lap from console."));
  }

  public void addBonus (User user, Team team, int amount, String reason) {
    logger.log(Level.INFO, "Adding a bonus of {0} for team {1} by user {2} (reason: {3})",new Object[]{amount, team.getName(), user.getUsername(), reason});

    team.update(amount);

    em.merge(team);
    em.persist(new HistoryEntry(user, team, amount, 0, reason));
  }

  public List<HistoryEntry> getHistory() {
    return getHistory(0);
  }

  /**
   * @param since the timestamp in milliseconds
   * @return 
   */
  public List<HistoryEntry> getHistory(long since) {
    TypedQuery query = em.createNamedQuery("History.allSinceTime", HistoryEntry.class);

    query.setParameter("since", new Date(since));
    return query.getResultList();
  }

  public void reset() {
    for (HistoryEntry entry : getHistory(0)) {
      em.remove(entry);
    }
    
    for (Team team : teamManager.getAll()) {
      team.reset();
      em.merge(team);
    }
  }

  public static ScoreManager lookup() {
    try {
      Context c = new InitialContext();
      return (ScoreManager) c.lookup("java:module/ScoreManager!be.ugent.zeus.urenloop.drbeaker.ScoreManager");
    } catch (NamingException ne) {
      throw new RuntimeException(ne);
    }
  }
}
