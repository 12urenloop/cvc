
package be.ugent.zeus.urenloop.drbeaker.web.admin;

import be.ugent.zeus.urenloop.drbeaker.ScoreManager;
import com.sun.jersey.api.view.Viewable;
import java.net.URI;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

/**
 *
 * @author Thomas Meire
 */
@Path("/admin/system")
public class SystemInterface {

  private ScoreManager scoreManager = ScoreManager.lookup();

  @GET
  public Viewable showCounterSwitch() {
    return new Viewable("/admin/system.jsp", null);
  }

  @POST
  public Response showCounterSwitch(@FormParam("name") String name) {
    scoreManager.setCurrentSource(name);
    return Response.seeOther(URI.create("/admin/system")).build();
  }
}
