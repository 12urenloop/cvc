package be.ugent.zeus.urenloop.drbeaker;

import be.ugent.zeus.urenloop.drbeaker.db.Team;
import java.awt.image.BufferedImage;
import java.util.List;
import javax.ejb.Stateless;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.persistence.TypedQuery;

/**
 *
 * @author Thomas Meire
 */
@Stateless
public class TeamManager {

  @PersistenceContext(unitName = "scorePU")
  private EntityManager em;

  public void add (Team team) {
    em.persist(team);
  }

  public void addTeamBonus(long id, int bonus) {
    Team team = get(id);
    team.increaseScore(bonus);
    System.err.println(team.getScore());
    em.merge(team);
  }

  public Team get(Long pk) {
    return em.find(Team.class, pk);
  }

  public Team get(String mac) {
    Query query = em.createNamedQuery("Team.findByMac");
    query.setParameter("mac", mac);

    return (Team) query.getSingleResult();
  }

  public List<Team> get() {
    TypedQuery query = em.createNamedQuery("Team.all", Team.class);
    return query.getResultList();
  }

  public List<Team> getByScore() {
    TypedQuery query = em.createNamedQuery("Team.allByScore", Team.class);
    return query.getResultList();
  }

  public static TeamManager lookup () {
    try {
      Context c = new InitialContext();
      return (TeamManager) c.lookup("java:module/TeamManager!be.ugent.zeus.urenloop.drbeaker.TeamManager");
    } catch (NamingException ne) {
      throw new RuntimeException(ne);
    }
  }
}
