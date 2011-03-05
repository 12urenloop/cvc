package be.ugent.zeus.urenloop.drbeaker.db;

import java.io.Serializable;
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

/**
 *
 * @author Thomas Meire
 */
@Entity()
@Table(name="GROUPS")
@NamedQueries(
  @NamedQuery(name="Group.all", query="SELECT g from Group g")
)
public class Group implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  private String name;

  @ManyToMany(cascade=CascadeType.ALL)
  @JoinTable(name = "USERS_GROUPS",
    joinColumns = {@JoinColumn(name = "NAME")},
    inverseJoinColumns = {@JoinColumn(name = "USERNAME")}
  )
  private Set<User> users;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Set<User> getUsers () {
    return users;
  }

  public void setUsers (Set<User> users) {
    this.users = users;
  }

  public void add (User user) {
    users.add(user);
    user.getGroups().add(this);
  }

  @Override
  public String toString() {
    return "be.ugent.zeus.urenloop.score.db.Group[id=" + name + "]";
  }
}
