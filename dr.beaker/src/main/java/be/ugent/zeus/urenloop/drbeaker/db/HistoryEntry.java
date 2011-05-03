
package be.ugent.zeus.urenloop.drbeaker.db;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Temporal;
import javax.xml.bind.annotation.XmlRootElement;

/**
 *
 * @author Thomas Meire
 */
@Entity
@NamedQueries({
  @NamedQuery(name="History.all", query="select h from HistoryEntry h order by h.date desc"),
  @NamedQuery(name="History.allSinceTime", query="select h from HistoryEntry h where h.date >= :since order by h.date desc"),
  @NamedQuery(name="History.findByTeam", query="select h from HistoryEntry h where h.team=:team order by h.date desc"),
  @NamedQuery(name="History.findByUser", query="select h from HistoryEntry h where h.user=:user order by h.date desc")
})
@XmlRootElement
public class HistoryEntry implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  @ManyToOne
  private User user;

  @ManyToOne
  private Team team;

  private int amount;

  private double speed;

  private String reason;

  @Temporal(javax.persistence.TemporalType.TIMESTAMP)
  private Date date;

  public HistoryEntry() {
  }

  public HistoryEntry(User user, Team team, int amount, double speed, String reason) {
    this.user = user;
    this.team = team;
    this.amount = amount;
    this.reason = reason;
    this.date = new Date();
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public int getAmount() {
    return amount;
  }

  public void setAmount(int amount) {
    this.amount = amount;
  }

  public void setSpeed (double speed) {
    this.speed = speed;
  }

  public double getSpeed () {
    return speed;
  }
  
  public String getReason() {
    return reason;
  }

  public void setReason(String reason) {
    this.reason = reason;
  }

  public Team getTeam() {
    return team;
  }

  public void setTeam(Team team) {
    this.team = team;
  }

  public Date getDate() {
    return date;
  }

  public void setDate(Date date) {
    this.date = date;
  }

  public User getUser() {
    return user;
  }

  public void setUser(User user) {
    this.user = user;
  }

  @Override
  public int hashCode() {
    int hash = 0;
    hash += (id != null ? id.hashCode() : 0);
    return hash;
  }

  @Override
  public boolean equals(Object object) {
    // TODO: Warning - this method won't work in the case the id fields are not set
    if (!(object instanceof HistoryEntry)) {
      return false;
    }
    HistoryEntry other = (HistoryEntry) object;
    if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "be.ugent.zeus.urenloop.drbeaker.db.TeamHistoryEntry[id=" + id + "]";
  }
}
