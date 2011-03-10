package be.ugent.zeus.urenloop.drbeaker;

import be.ugent.zeus.urenloop.drbeaker.db.HistoryEntry;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
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
public class TeamManager {

  private static final Logger logger = Logger.getLogger("12UL");

  @PersistenceContext(unitName = "scorePU")
  private EntityManager em;

  public void add(Team team) {
    em.persist(team);
  }

  public void update(Team team) {
    em.merge(team);
  }

  public void delete(Team team) {
    em.remove(team);
  }

  public void addTeamBonus(User user, Team team, int bonus, String reason) {
    if (team != null) {
      team.increaseScore(bonus);

      // Add new history entry
      HistoryEntry entry = new HistoryEntry(user, team, bonus, reason);
      em.persist(entry);

      em.merge(team);
    } else {
      logger.log(Level.WARNING, "Trying to do add a bonus to a null team!");
    }
  }

  public void addTeamLap(Team team) {
    if (team != null) {
      team.increaseScore();

      // Add new history entry
      HistoryEntry entry = new HistoryEntry(null, team, 1, "Lap completed");
      em.persist(entry);

      em.merge(team);
    } else {
      logger.log(Level.WARNING, "Trying to do add a lap to a null team!");
    }
  }

  public void assign(Team team, Stick stick) {
    if (team == null) {
      throw new RuntimeException("No team found!");
    }

    if (stick == null && team != null) {
      team.setStick(null);
      em.merge(team);
    } else {
      if (stick.getTeam() != null) {
        stick.getTeam().setStick(null);
        em.merge(stick.getTeam());
      }
      team.setStick(stick);
      stick.setTeam(team);
      em.merge(team);
      em.merge(stick);
    }
  }

  public Team get(Long pk) {
    return em.find(Team.class, pk);
  }

  public List<Team> get() {
    TypedQuery query = em.createNamedQuery("Team.all", Team.class);
    return query.getResultList();
  }

  public List<Team> getByScore() {
    TypedQuery query = em.createNamedQuery("Team.allByScore", Team.class);
    return query.getResultList();
  }

  public List<HistoryEntry> getHistory() {
    TypedQuery query = em.createNamedQuery("History.all", HistoryEntry.class);
    return query.getResultList();
  }

  public static TeamManager lookup() {
    try {
      Context c = new InitialContext();
      return (TeamManager) c.lookup("java:module/TeamManager!be.ugent.zeus.urenloop.drbeaker.TeamManager");
    } catch (NamingException ne) {
      throw new RuntimeException(ne);
    }
  }
}
