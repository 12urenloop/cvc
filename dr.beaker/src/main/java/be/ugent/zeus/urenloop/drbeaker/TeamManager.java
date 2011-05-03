package be.ugent.zeus.urenloop.drbeaker;

import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import be.ugent.zeus.urenloop.drbeaker.db.Team;
import java.util.List;
import java.util.logging.Logger;
import javax.ejb.Stateless;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
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
  
  public void assign(Long teamID, Long stickID) {
    Team team = get(teamID);
    Stick stick = em.find(Stick.class, stickID);

    assign(team, stick);
  }

  public void assign (Team team, Stick stick) {
    if (team == null) {
      throw new RuntimeException("No team found!");
    }

    if (team.getStick() != null) {
      team.getStick().setTeam(null);
    }    
    if (stick.getTeam() != null) {
      stick.getTeam().setStick(null);
    }
    team.setStick(stick);
    stick.setTeam(team);
    em.merge(team);
    em.merge(stick);
  }

  public void removeByName(String name) {
    TypedQuery<Team> q = em.createNamedQuery("Team.findByName", Team.class);
    q.setParameter("name", name);

    try {
      Team team = q.getSingleResult();
      em.remove(team);
    } catch (NoResultException e) {
    }
  }

  public Team get(Long pk) {
    return em.find(Team.class, pk);
  }

  public Team get(String name) {
    TypedQuery<Team> q = em.createNamedQuery("Team.findByName", Team.class);
    q.setParameter("name", name);

    try {
      return q.getSingleResult();
    } catch (NoResultException e) {
      return null;
    }
  }

  public List<Team> getAll() {
    return doQuery("Team.all");
  }

  public List<Team> getAll(String sortOrder) {
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

  public static TeamManager lookup() {
    try {
      Context c = new InitialContext();
      return (TeamManager) c.lookup("java:module/TeamManager!be.ugent.zeus.urenloop.drbeaker.TeamManager");
    } catch (NamingException ne) {
      throw new RuntimeException(ne);
    }
  }
}
