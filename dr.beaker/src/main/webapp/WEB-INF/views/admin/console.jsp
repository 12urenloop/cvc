<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Manuele console" tab="admin" subtab="console">
  <ul:section title="Manuele console">
    <form action="<c:url  value="/admin/console" />" method="POST">
      <c:forEach items="${it}" var="team">
        <button style="height: 60px; margin: 10px;" type="submit" value="${team.id}" name="team">
          <span style="font-size: 25px; font-weight: bold;">${team.id}</span><br> ${team.name}
        </button>
      </c:forEach>
    </form>
  </ul:section>
</ul:page>