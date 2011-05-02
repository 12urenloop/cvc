package be.ugent.zeus.urenloop.drbeaker.api;

import be.ugent.zeus.urenloop.drbeaker.ScoreManager;
import be.ugent.zeus.urenloop.drbeaker.db.HistoryEntry;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

/**
 *
 * @author Thomas Meire
 */
@Path("/api/history")
@Produces(MediaType.APPLICATION_JSON)
public class HistoryAPI {

  private ScoreManager scoreManager = ScoreManager.lookup();

  @GET
  public List<HistoryEntry> getHistoryEntries(@QueryParam("since") Long since) {
    if (since == null) {
      since = 0l;
    }
    return scoreManager.getHistory(since * 1000);
  }
}
