<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Administration interface" tab="admin" subtab="users" cols="2">
  <ul:maincol>
    <div class="section">
      <div class="section-title">Gebruikersoverzicht</div>
      <div class="section-content">
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
      </div>
    </div>
  </ul:maincol>
  <ul:sidecol>
    <ul:section title="Gebruiker toevoegen">
      <form action="<c:url value="/admin/users/add" />" method="POST">
        <label for="username">Gebruikersnaam:</label>
        <input type="text" name="username" id="username"><br>
        <label for="password">Paswoord:</label>
        <input type="password" name="password" id="password"><br>

        <label for="group">Gebruikersgroep:</label><br>
        <input type="radio" name="group" id="administrator" value="administrator">
        <label for="administrator">Administrator</label><br>
        <input type="radio" name="group" id="moderator" value="moderator">
        <label for="moderator">Moderator</label><br>

        <input type="submit" value="Toevoegen">
      </form>
    </ul:section>
    <ul:section title="Gebruiker verwijderen">
      <form action="<c:url value="/admin/users/delete"/>" method="POST">
        <label for="username">Gebruikersnaam:</label><input type="text" name="username"><br>
        <input type="submit" value="Verwijderen" />
      </form>
    </ul:section>
  </ul:sidecol>
</ul:page>