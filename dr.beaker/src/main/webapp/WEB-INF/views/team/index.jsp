<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<c:set var="contextroot" value="${pageContext.request.contextPath}" />

<ul:template title="Team Overview">
  <ul:main title="Teams in contest">
    <ul>
    <c:forEach items="${it}" var="team">
      <li>${team.name}</li>
    </c:forEach>
    </ul>
  </ul:main>
    <ul:side>
      <h3>Create new team</h3>
      <form method="POST" action="${contextroot}/manage/team/add" class="form">
        <label for="teamname">Name:</label>
        <input type="text" name="teamname" id="teamname">
        <input type="submit" value="Create team">
      </form>
    </ul:side>
</ul:template>