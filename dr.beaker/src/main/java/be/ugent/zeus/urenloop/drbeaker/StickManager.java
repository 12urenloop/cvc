
package be.ugent.zeus.urenloop.drbeaker;

import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import java.util.List;
import java.util.logging.Level;
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
public class StickManager {

  private static final Logger logger = Logger.getLogger("12UL");

  @PersistenceContext(unitName = "scorePU")
  private EntityManager em;

  public void add (Stick stick) {
    em.persist(stick);
    logger.log(Level.FINE, "Added stick with mac {0}", stick.getMac());
  }

  public List<Stick> get() {
    return em.createNamedQuery("Stick.findAll", Stick.class).getResultList();
  }

  public Stick get (long id) {
    return em.find(Stick.class, id);
  }

  public Stick get (String mac) {
    TypedQuery<Stick> query = em.createNamedQuery("Stick.findByMac", Stick.class);
    query.setParameter("mac", mac);
    try {
      return query.getSingleResult();
    } catch (NoResultException nre) {
      return null;
    }
  }

  public void delete(Stick stick) {
    em.remove(stick);
  }

  public static StickManager lookup() {
    try {
      Context c = new InitialContext();
      return (StickManager) c.lookup("java:module/StickManager!be.ugent.zeus.urenloop.drbeaker.StickManager");
    } catch (NamingException ne) {
      throw new RuntimeException(ne);
    }
  }
}
