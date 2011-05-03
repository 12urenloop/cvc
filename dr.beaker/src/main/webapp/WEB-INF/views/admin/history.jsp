
<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c"   uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Score log" tab="admin" subtab="history">
  <ul:section title="Score log">
    <script>
      $(function () { setTimeout(location.reload, 10000); });
    </script>
    <form method="GET">
      <label for="teamname">Team</label><input type="text" name="teamname"><input type="submit" value="Filter">
    </form>
    <table>
      <thead>
        <tr>
          <th>Tijd</th>
          <th>Team</th>
          <th>Gebruikersnaam</th>
          <th>Punten</th>
          <th>Snelheid</th>
          <th>Reden</th>
        </tr>
      </thead>
      <tbody>
        <c:forEach items="${it}" var="history">
          <tr>
            <td><fmt:formatDate value="${history.date}" type="time" /></td>
            <td>${history.team.name}</td>
            <td>${history.user.username}</td>
            <td>${history.amount}</td>
            <td>${history.speed}</td>
            <td>${history.reason}</td>
          </tr>
        </c:forEach>
      </tbody>
    </table>
  </ul:section>
</ul:page>