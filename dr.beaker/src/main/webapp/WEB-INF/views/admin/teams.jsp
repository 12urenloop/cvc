<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Team Overview" tab="admin" subtab="teams" cols="2">
  <ul:maincol>
    <ul:section title="Teams in wedstrijd">
      <table class="scoreboard" id="scoreboard">
        <thead>
          <tr>
            <th>Team naam</th>
            <th style="text-align: right;">Stok</th>
          </tr>
        </thead>
        <tbody>
          <c:forEach items="${it}" var="team">
            <tr>
              <td>${team.name}</td>
              <td style="text-align: right;">
                <c:choose>
                  <c:when test="${team.stick != null}">
                    <strong>${team.stick.id}</strong> <small>(${team.stick.mac})</small>
                  </c:when>
                  <c:otherwise>
                    <span style="color: red;">Geen stok toegekend!</span>
                  </c:otherwise>
                </c:choose>
              </td>
            </tr>
          </c:forEach>
        </tbody>
      </table>
    </ul:section>
  </ul:maincol>
  <ul:sidecol>
    <ul:section title="Nieuw team aanmaken">
      <form method="POST" action="<c:url value="/admin/teams/add" />">
        <label for="teamname">Naam</label>
        <input type="text" name="teamname" id="teamname"><br>
        <input type="submit" value="Team aanmaken">
      </form>
    </ul:section>

    <ul:section title="Stok toekennen aan team">
      <form method="POST" action="<c:url value="/admin/teams/stick/" />" class="form">
        <label for="team">Team</label>
        <select id="team-selector" name="team">
          <c:forEach items="${it}" var="team">
            <option value="${team.id}">${team.name}</option>
          </c:forEach>
        </select><br>
        <%-- TODO: perhaps just show a select from 1 to MAX_STICK_ID? --%>
        <label for="stick">Stok id</label>
        <input type="text" name="stick" size="3" style="text-align: right"><br>
        <input type="submit" value="Stok toekennen">
      </form>
    </ul:section>
  </ul:sidecol>
</ul:page>