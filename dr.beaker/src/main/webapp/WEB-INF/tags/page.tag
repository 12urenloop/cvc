<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
  "http://www.w3.org/TR/html4/loose.dtd">
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%@attribute name="title"%>
<%@attribute name="tab" %>
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
              <li class="current-tab"><a href="index.html">Speciale rondes</a></li>
              <% if (request.isUserInRole("Administrator")) {%>
              <li><a href="style-demo.html">Teams</a></li>
              <li><a href="archives.html">Stokken</a></li>
              <li><a href="comments.html">Gebruikers</a></li>
              <% }%>
            </ul>
            <div class="clearer">&nbsp;</div>

          </div>
        </c:if>
        <div class="clearer">&nbsp;</div>
      </div>
      <jsp:doBody />
      <div id="footer">
        <p>Deze software werd ontwikkeld door <a href="http://zeus.ugent.be">Zeus WPI</a>, de werkgroep informatica.</p>
      </div>
    </div>
  </body>
</html>