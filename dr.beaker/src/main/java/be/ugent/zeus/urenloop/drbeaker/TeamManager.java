package be.ugent.zeus.urenloop.drbeaker;

import be.ugent.zeus.urenloop.drbeaker.db.HistoryEntry;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
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
    return doQuery("Team.all");
  }

  public List<Team> get(String sortOrder) {
    String query;
    if ("score".equals(sortOrder)) {
      query = "Team.allByScore";
    } else {
      query = "Team.all";
    }
    return doQuery(query);
  }

  private List<Team> doQuery(String query) {
    TypedQuery q = em.createNamedQuery(query, Team.class);
    return q.getResultList();
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
