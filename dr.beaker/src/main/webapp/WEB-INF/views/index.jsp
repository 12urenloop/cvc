<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:template title="12 Urenloop - Scorebord" tab="index">
  <ul:main title="Scorebord">
    <script type="text/javascript" src="<c:url value="/javascript/scoreboard.js"/>"></script>
    <a id="autorefresh-toggle" href="#"> Toggle autorefresh</a>
    <table class="scoreboard" id="scoreboard">
      <tbody>
      <c:forEach items="${it}" var="team">
        <tr>
          <td class="name">${team.name}</td>
          <td class="score">${team.score}</td>
        </tr>
      </c:forEach>
      </tbody>
    </table>
  </ul:main>
  <ul:side>
    <ul:bonus teams="${it}"/>
  </ul:side>
</ul:template>