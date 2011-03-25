<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Stokken overzicht" tab="admin" subtab="sticks" cols="2">
  <ul:maincol>
    <ul:section title="Beschibare loopstokken">
      <table class="scoreboard" id="scoreboard">
        <thead>
          <tr>
            <th>ID</th>
            <th>MAC adres</th>
            <th style="text-align: center;">Team</th>
          </tr>
        </thead>
        <tbody>
          <c:forEach items="${it}" var="stick">
            <tr>
              <td>${stick.id}</td>
              <td>${stick.mac}</td>
              <td style="text-align: center;">
                ${(stick.team != null) ? stick.team.name : ''}
              </td>
            </tr>
          </c:forEach>
        </tbody>
      </table>
    </ul:section>
  </ul:maincol>
  <ul:sidecol>
    <h3>Nieuwe stok toevoegen</h3>
    <form id="add-team-form" method="POST" action="<c:url value="/manage/team/add" />" class="form">
      <label for="mac">Mac adres:</label>
      <input type="text" name="mac" id="mac"><br>
      <input type="submit" value="Toevoegen">
    </form>
  </ul:sidecol>
</ul:page>