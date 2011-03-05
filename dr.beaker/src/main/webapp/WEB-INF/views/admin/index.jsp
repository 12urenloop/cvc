<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<c:set var="contextroot" value="${pageContext.request.contextPath}" />

<ul:template title="Administration interface">
  <ul:main title="Administration interface">
    <h3>Available Users</h3>
    <ul>
      <c:forEach items="${it[0]}" var="user">
        <li><strong>${user.username}</strong><br>
          (Groups:
        <c:forEach items="${user.groups}" var="group">
          ${group.name}
        </c:forEach>)
        </li>
      </c:forEach>
    </ul>
    <hr>
    <ul>
      <c:forEach items="${it[1]}" var="group">
        <li><strong>${group.name}</strong>
        <c:forEach items="${group.users}" var="user">
          ${user.username}
        </c:forEach>
        </li>
      </c:forEach>
    </ul>
  </ul:main>

  <ul:side>
    <h3>Add a new user</h3>
    <form action="${contextroot}/admin/users/add" method="POST">
      <label for="username">Username:</label>
      <input type="text" name="username" id="username"><br>
      <label for="password">Password:</label>
      <input type="password" name="password" id="password"><br>

      Choose the group for this user:<br>
      <input type="radio" name="group" id="administrator" value="administrator">
      <label for="administrator">Administrator</label><br>
      <input type="radio" name="group" id="moderator" value="moderator">
      <label for="moderator">Moderator</label><br>

      <input type="submit" value="Add user">
    </form>
  </ul:side>
</ul:template>