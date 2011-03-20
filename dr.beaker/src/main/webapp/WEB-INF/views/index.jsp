<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="12 Urenloop - Scorebord" tab="index">
  <ul:onecol title="Scorebord">
    <script type="text/javascript" src="<c:url value="/javascript/scoreboard.js"/>"></script>
    <span style="width: 100%; display: inline-block; text-align: right;"><a id="autorefresh-toggle" href="#"> Toggle autorefresh</a></span>
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
  </ul:onecol>
</ul:page>