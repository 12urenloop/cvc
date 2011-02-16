package be.ugent.zeus.urenloop.score.db;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;

/**
 *
 * @author Thomas Meire
 */
@Entity
@NamedQueries(
@NamedQuery(name = "Team.findByMac", query = "SELECT t from Team t where t.macAddress = :mac"))
public class Team implements Serializable {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  private String macAddress;

  private int score;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getMacAddress() {
    return macAddress;
  }

  public void setMacAddress(String mac) {
    this.macAddress = mac;
  }

  public void increaseScore() {
    score++;
  }

  public int getScore() {
    return score;
  }

  public void setScore(int score) {
    this.score = score;
  }
}
