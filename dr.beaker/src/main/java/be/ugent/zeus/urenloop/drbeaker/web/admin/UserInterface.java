
package be.ugent.zeus.urenloop.drbeaker.web.admin;

import be.ugent.zeus.urenloop.drbeaker.AuthenticationManager;
import be.ugent.zeus.urenloop.drbeaker.db.Group;
import be.ugent.zeus.urenloop.drbeaker.db.User;
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
@Path("/admin/users/")
public class UserInterface {
  
  private AuthenticationManager authManager = AuthenticationManager.lookup();

  @GET
  @Path("/")
  public Viewable showUsers() {
    return new Viewable("/admin/index.jsp", new Object[]{authManager.getUsers(), authManager.getGroups()});
  }

  @POST
  @Path("/add")
  public Response addUser(@FormParam("username") String username,
          @FormParam("password") String password,
          @FormParam("group") String groupName) throws Exception {

    if (username == null || password == null) {
      // TODO: handle this gracefully
      throw new Exception("Username and password shouldn't be null.");
    }

    User user = authManager.getUser(username);
    if (user != null) {
      // TODO: handle this gracefully
      throw new Exception("User " + username + " already existed.");
    }

    user = new User();
    user.setUsername(username);
    user.setPassword(password);
    authManager.add(user);

    Group group = authManager.getGroup(groupName);
    if (group != null) {
      authManager.addUserToGroup(user, group);
    }
    return Response.seeOther(URI.create("/admin/")).build();
  }
}
