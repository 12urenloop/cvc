
package be.ugent.zeus.urenloop.drbeaker;

import be.ugent.zeus.urenloop.drbeaker.db.Group;
import be.ugent.zeus.urenloop.drbeaker.db.User;
import java.util.List;

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
public class AuthenticationManager {

  @PersistenceContext(unitName = "scorePU")
  private EntityManager em;

  public void add (User user) {
    em.persist(user);
  }

  public void add (Group group) {
    em.persist(group);
  }

  public void addUserToGroup(User user, Group group) {
    group.add(user);
    user.addToGroup(group);
    em.merge(group);
  }

  public User getUser(String username) {
    return em.find(User.class, username);
  }

  public Group getGroup(String name) {
    return em.find(Group.class, name);
  }

  public List<User> getUsers() {
    TypedQuery query = em.createNamedQuery("User.all", User.class);
    return query.getResultList();
  }

  public List<Group> getGroups() {
    TypedQuery query = em.createNamedQuery("Group.all", Group.class);
    return query.getResultList();
  }

  public static AuthenticationManager lookup () {
    try {
      Context c = new InitialContext();
      return (AuthenticationManager) c.lookup("java:module/AuthenticationManager!be.ugent.zeus.urenloop.drbeaker.AuthenticationManager");
    } catch (NamingException ne) {
      throw new RuntimeException(ne);
    }
  }

  public void merge(Group admins) {
    em.merge(admins);
  }
}
