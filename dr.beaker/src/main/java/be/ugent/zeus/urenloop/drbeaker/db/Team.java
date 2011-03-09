package be.ugent.zeus.urenloop.drbeaker.db;

import java.io.Serializable;
import java.util.List;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

/**
 *
 * @author Thomas Meire
 */
@Entity
@NamedQueries({
  @NamedQuery(name = "Team.all", query = "SELECT t from Team t order by t.name asc"),
  @NamedQuery(name = "Team.allByScore", query = "SELECT t from Team t order by t.score desc")
})
public class Team implements Serializable {
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  private String name;

  private int score;

  @OneToOne
  private Stick stick;

  @OneToMany(mappedBy = "team")
  private List<HistoryEntry> history;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public void setName (String name) {
    this.name = name;
  }

  public String getName () {
    return name;
  }

  public int getScore() {
    return score;
  }

  public void setScore(int score) {
    this.score = score;
  }

  public void increaseScore() {
    score++;
  }

  public void increaseScore(int bonus) {
    System.err.println("BONUS: " + bonus);
    score += bonus;
    if (score < 0) {
      score = 0;
    }
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
}
