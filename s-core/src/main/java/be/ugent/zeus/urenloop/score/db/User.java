
package be.ugent.zeus.urenloop.score.db;

import java.io.Serializable;
import java.security.MessageDigest;
import java.util.Set;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
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
public class User implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  private String username;

  private String password;

  @ManyToMany(mappedBy="users")
  private Set<Group> groups;

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
      e.printStackTrace();
    }
  }

  public void addToGroup(Group group) {
    Set<Group> groups = getGroups();
    groups.add(group);


    group.getUsers().add(this);
  }

  public void setGroups(Set<Group> groups) {
    this.groups = groups;
  }

  public Set<Group> getGroups() {
    return groups;
  }

  @Override
  public String toString() {
    return "be.ugent.zeus.urenloop.score.db.User[id=" + username + "]";
  }
}
