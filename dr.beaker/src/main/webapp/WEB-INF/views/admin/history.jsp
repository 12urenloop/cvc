<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Score log" tab="admin" subtab="history">
  <ul:section title="Score log">
    <table>
      <c:forEach items="${it}" var="history">
        <tr>
          <td>${history.team.name}</td>
          <td>${history.user.username}</td>
          <td>${history.amount}</td>
          <td>${history.reason}</td>
        </tr>
      </c:forEach>
    </table>
  </ul:section>
</ul:page>