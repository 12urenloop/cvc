<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Manuele console" tab="admin" subtab="console">
  <ul:section title="Manuele console">
    <c:choose>
      <c:when test="${it[0].source == 'console'}">
    <form action="<c:url  value="/admin/console" />" method="POST">
      <c:forEach items="${it[1]}" var="team">
        <button style="height: 60px; margin: 10px;" type="submit" value="${team.id}" name="team">
          <span style="font-size: 25px; font-weight: bold;">${team.id}</span><br> ${team.name}
        </button>
      </c:forEach>
    </form>
      </c:when>
      <c:otherwise>
        De manuele console is momenteel niet beschikbaar. Indien je die wil gebruiken moet je die activeren in het <a href="<c:url value="/admin/system" />">systeem beheer</a>.
      </c:otherwise>
    </c:choose>
  </ul:section>
</ul:page>