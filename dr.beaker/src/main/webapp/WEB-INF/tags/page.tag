<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
  "http://www.w3.org/TR/html4/loose.dtd">
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%@attribute name="title"%>
<%@attribute name="tab" %>
<%@attribute name="subtab" %>
<%@attribute name="cols" %>
<html>
  <head>
    <title>${title}</title>
    <link rel="stylesheet" type="text/css" href="<c:url value="/style/style.css"/>">
    <script src="<c:url value="/javascript/jquery.js" />" language="javascript"></script>
  </head>
  <body>
    <div id="site-wrapper">
      <div id="header">
        <div id="top">
          <div class="left" id="logo">
            <a href="<c:url value="/" />"><img src="<c:url value="/style/images/logo.gif"/>" alt="12 Urenloop" /></a>
          </div>
          <div  class="left navigation" id="main-nav">
            <ul class="tabbed">
              <li <c:if test="${tab == 'index'}">class="current-tab"</c:if>><a href="<c:url value="/" />">Scorebord</a></li>
              <% if (request.isUserInRole("Moderator")) {%>
              <li <c:if test="${tab == 'admin'}">class="current-tab"</c:if>><a href="<c:url value="/admin" />">Beheer</a></li>
              <% }%>
              <% if (request.getUserPrincipal() == null) {%>
              <li><a href="<c:url value="/login" />">Login</a></li>
              <% }%>
            </ul>
          </div>
          <div class="clearer">&nbsp;</div>
        </div>
        <c:if test="${tab == 'admin'}">
          <div class="navigation" id="sub-nav">
            <ul class="tabbed">
              <li <c:if test="${subtab == 'speclaps'}">class="current-tab"</c:if>><a href="<c:url value="/admin/laps/special"/>">Speciale rondes</a></li>
              <li <c:if test="${subtab == 'teams'}">class="current-tab"</c:if>><a href="<c:url value="/admin/teams" />">Teams</a></li>
              <li <c:if test="${subtab == 'sticks'}">class="current-tab"</c:if>><a href="<c:url value="/admin/sticks" />">Stokken</a></li>
              <% if (request.isUserInRole("Administrator")) {%>
              <li <c:if test="${subtab == 'users'}">class="current-tab"</c:if>><a href="<c:url value="/admin/users" />">Gebruikers</a></li>
              <li <c:if test="${subtab == 'history'}">class="current-tab"</c:if>><a href="<c:url value="/admin/history" />">Geschiedenis</a></li>
              <% }%>
            </ul>
            <div class="clearer">&nbsp;</div>
          </div>
        </c:if>
        <div class="clearer">&nbsp;</div>
      </div>
      <c:choose>
        <c:when test="${cols == 2}">
          <div class="main" id="main-two-columns">
            <jsp:doBody />
          </div>
        </c:when>
        <c:otherwise>
          <div class="main">
            <jsp:doBody />
          </div>
        </c:otherwise>
      </c:choose>
      <div id="footer">
        <p>Deze software werd ontwikkeld door <a href="http://zeus.ugent.be">Zeus WPI</a>, de werkgroep informatica.</p>
      </div>
    </div>
  </body>
</html>