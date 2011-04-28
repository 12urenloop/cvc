
<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c"   uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Score log" tab="admin" subtab="history">
  <ul:section title="Score log">
    <script>
      $(function () { setTimeout(location.reload, 5000); });
    </script>
    <table>
      <thead>
        <tr>
          <th>Tijd</th>
          <th>Team</th>
          <th>Gebruikersnaam</th>
          <th>Punten</th>
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
            <td>${history.reason}</td>
          </tr>
        </c:forEach>
      </tbody>
    </table>
  </ul:section>
</ul:page>