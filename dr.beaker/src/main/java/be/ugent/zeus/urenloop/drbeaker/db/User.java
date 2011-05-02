
package be.ugent.zeus.urenloop.drbeaker.db;

import java.io.Serializable;
import java.security.MessageDigest;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import sun.misc.BASE64Encoder;

/**
 *
 * @author Thomas Meire
 */
@Entity
@Table(name = "USERS")
@NamedQueries(
  @NamedQuery(name="User.all", query="SELECT u from User u")
)
@XmlRootElement
public class User implements Serializable {

  private static final Logger logger = Logger.getLogger("12UL");

  private static final long serialVersionUID = 1L;

  @Id
  private String username;

  private String password;

  @ManyToMany(mappedBy="users")
  private Set<Group> groups;

  @OneToMany(mappedBy = "user")
  private List<HistoryEntry> actions;

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public void setPassword(String password) {
    try {
      byte[] digest = MessageDigest.getInstance("SHA1").digest(password.getBytes());
      this.password = (new BASE64Encoder()).encode(digest);
    } catch (Exception e) {
      logger.log(Level.SEVERE, "Password encryption for user {0} failed, continuing with empty username!", username);
      e.printStackTrace();
    }
  }

  public void addToGroup(Group group) {
    getGroups().add(group);
    group.getUsers().add(this);
  }

  public void setGroups(Set<Group> groups) {
    this.groups = groups;
  }
  
  @XmlTransient
  public Set<Group> getGroups() {
    return groups;
  }

  @XmlTransient
  public List<HistoryEntry> getActions() {
    return actions;
  }

  public void setActions(List<HistoryEntry> actions) {
    this.actions = actions;
  }

  @Override
  public String toString() {
    return "be.ugent.zeus.urenloop.score.db.User[id=" + username + "]";
  }
}
