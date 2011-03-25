
package be.ugent.zeus.urenloop.drbeaker.api;

import be.ugent.zeus.urenloop.drbeaker.StickManager;
import be.ugent.zeus.urenloop.drbeaker.db.Stick;
import java.util.List;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

/**
 *
 * @author Thomas Meire
 */
@Path("api/sticks")
public class StickAPI {

  private StickManager stickManager = StickManager.lookup();

  @POST
  @Path("/")
  public long add (@FormParam("mac") String mac) {
    Stick stick = new Stick();
    stick.setMac(mac);

    stickManager.add(stick);
    return stick.getId();
  }

  @GET
  @Path("/")
  public List<Stick> get () {
    return stickManager.get();
  }

  @GET
  @Path("/{id}")
  public Stick get (@PathParam("id") long ID) {
    return stickManager.get(ID);
  }

  @DELETE
  @Path("/{id}")
  public void delete (@PathParam("id") long ID) {
    Stick stick = stickManager.get(ID);
    stickManager.delete(stick);
  }
}
