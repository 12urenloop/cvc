<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<c:set var="contextroot" value="${pageContext.request.contextPath}" />

<ul:template title="Team Overview" tab="teams">
  <ul:main title="Teams in contest">
    <table class="scoreboard">
      <thead>
        <tr>
          <th>Team name</th>
          <th style="text-align: right;">Stick</th>
        </tr>
      </thead>
      <c:forEach items="${it}" var="team">
        <tr>
          <td>${team.name}</td>
          <td style="text-align: right;">
            <c:choose>
              <c:when test="${team.stick != null}">
                <strong>${team.stick.id}</strong> <small>(${team.stick.mac})</small>
              </c:when>
              <c:otherwise>
                <span style="color: red;">No stick assigned!</span>
              </c:otherwise>
            </c:choose>
          </td>
        </tr>
      </c:forEach>
    </table>
  </ul:main>
  <ul:side>
    <h3>Create new team</h3>
    <form method="POST" action="<c:url value="/manage/team/add" />" class="form">
      <label for="teamname">Name:</label>
      <input type="text" name="teamname" id="teamname"><br>
      <input type="submit" value="Create team">
    </form>

    <h3>Assign stick to team</h3>
    <form method="POST" action="<c:url value="/manage/team/stick/" />" class="form">
      <label for="team">Team</label>
      <select name="team">
        <c:forEach items="${it}" var="team">
          <option value="${team.id}">${team.name}</option>
        </c:forEach>
      </select><br>
      <%-- TODO: perhaps just show a select from 1 to MAX_STICK_ID? --%>
      <label for="stick">Stick id</label>
      <input type="text" name="stick" size="3" style="text-align: right"><br>
      <input type="submit" value="Assign stick">
    </form>
  </ul:side>
</ul:template>