
package be.ugent.zeus.urenloop.drbeaker;

import be.ugent.zeus.urenloop.drbeaker.db.HistoryEntry;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.Stateless;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 *
 * @author Thomas Meire
 */
@Stateless
public class ScoreManager {

  private static final Logger logger = Logger.getLogger("12UL");

  @PersistenceContext(unitName = "scorePU")
  private EntityManager em;

  private String source;

  public String getSource () {
    return source;
  }

  public void setCurrentSource (String source) {
    this.source = source;
  }

  public void addLap (String source, Team team, double speed, List<String> warnings) {
    // only accept from the current source
    if (!source.equals(this.source)) {
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
    }

    // store a message for the warning log
    if (!warnings.isEmpty()) {
    }

    em.merge(team);
    em.persist(new HistoryEntry(null, team, 1, "Completed a lap from " + source + "."));
  }

  public void addBonus (User user, Team team, int amount, String reason) {
    logger.log(Level.INFO, "Adding a bonus of {0} for team {1} by user {2} (reason: {3})",new Object[]{amount, team.getName(), user.getUsername(), reason});

    team.update(amount);

    em.merge(team);
    em.persist(new HistoryEntry(user, team, amount, reason));
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
