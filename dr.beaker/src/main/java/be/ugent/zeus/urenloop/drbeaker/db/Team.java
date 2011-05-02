package be.ugent.zeus.urenloop.drbeaker.db;

import java.io.Serializable;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

/**
 *
 * @author Thomas Meire
 */
@Entity
@NamedQueries({
  @NamedQuery(name = "Team.all", query = "SELECT t from Team t"),
  @NamedQuery(name = "Team.allByName", query = "SELECT t from Team t order by t.name desc"),
  @NamedQuery(name = "Team.allByScore", query = "SELECT t from Team t order by t.score desc, t.name desc"),
  @NamedQuery(name = "Team.findByName", query = "SELECT t from Team t where t.name = :name")
})
@XmlRootElement
public class Team implements Serializable {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  @Column(unique=true, nullable=false)
  private String name;

  private int score;

  private double speed;

  private transient Queue<Double> lapspeeds = new LinkedList<Double>();

  @OneToOne
  private Stick stick;

  @XmlTransient
  @OneToMany(mappedBy = "team")
  private List<HistoryEntry> history;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public int getScore() {
    return score;
  }

  public void setScore(int score) {
    this.score = score;
  }

  public void update(int amount) {
    score += amount;
    if (score < 0) {
      score = 0;
    }
  }

  public double getAverageSpeed () {
    return speed;
  }

  private double calculateMovingAverageSpeed () {
    double tmp = 0.;
    for (Double d : lapspeeds) {
      tmp += d;
    }
    return tmp / lapspeeds.size();
  }

  public void updateAverageSpeed(double speed) {
    if (lapspeeds.size() >= 5) {
      lapspeeds.poll();
    }
    lapspeeds.offer(speed);

    this.speed = calculateMovingAverageSpeed();
  }

  public Stick getStick() {
    return stick;
  }

  public void setStick(Stick stick) {
    this.stick = stick;
  }

  public List<HistoryEntry> getHistory() {
    return history;
  }

  public void setHistory(List<HistoryEntry> history) {
    this.history = history;
  }

  public void reset() {
    speed = 0;
    score = 0;
    lapspeeds.clear();
  }
}
