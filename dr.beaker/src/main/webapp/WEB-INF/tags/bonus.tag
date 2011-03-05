<%@tag description="put the tag description here" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%@attribute name="teams" type="java.util.List<be.ugent.zeus.urenloop.drbeaker.db.Team>" rtexprvalue="true" %>


<% if(request.isUserInRole("Moderator")) { %>
  <h3>Bonuspunten toekennen</h3>
  <form action="${pageContext.request.contextPath}/manage/team/bonus/" method="POST">
    <label for="team">Team:</label>
    <select name="team">
      <c:forEach items="${teams}" var="team">
        <option value="${team.id}">${team.name}</option>
      </c:forEach>
    </select><br>
    <label for="bonus">Aantal punten:</label>
    <input type="text" name="bonus" size="2" style="text-align: right;"><br>
    <input type="submit" value="Bonus toekennen">
  </form>
<% } %>
