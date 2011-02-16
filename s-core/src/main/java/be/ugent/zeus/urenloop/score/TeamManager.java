package be.ugent.zeus.urenloop.score;

import be.ugent.zeus.urenloop.score.db.Team;
import javax.ejb.Stateless;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceUnit;
import javax.persistence.Query;

/**
 *
 * @author Thomas Meire
 */
@Stateless
public class TeamManager {

  @PersistenceUnit(name = "scorePU")
  private EntityManager em;

  public Team getTeam(Long pk) {
    return em.find(Team.class, pk);
  }

  public Team getTeam(String mac) {
    Query query = em.createNamedQuery("Team.findByMac");
    query.setParameter("mac", mac);

    return (Team) query.getSingleResult();
  }

  public static TeamManager lookup () {
    try {
      Context c = new InitialContext();
      return (TeamManager) c.lookup("java:module/TeamManager!be.ugent.zeus.urenloop.score.TeamManager");
    } catch (NamingException ne) {
      throw new RuntimeException(ne);
    }
  }
}
