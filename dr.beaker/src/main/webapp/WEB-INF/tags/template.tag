<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
  "http://www.w3.org/TR/html4/loose.dtd">

<%@tag description="Global page template" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%@attribute name="title"%>
<%@attribute name="tab" %>

<c:set var="contextroot" value="${pageContext.request.contextPath}" />

<html>
  <head>
    <title>${title}</title>
    <link rel="stylesheet" type="text/css" href="${contextroot}/style/style.css" />
  </head>
  <body>
    <div id="site-wrapper">
      <div id="header">
        <div id="top">
          <div class="left" id="logo">
            <a href="${contextroot}"><img src="${contextroot}/style/images/logo.gif" alt="" /></a>
          </div>
          <div class="left navigation" id="main-nav">
            <ul class="tabbed">
              <li <c:if test="${tab == 'index'}">class="current-tab"</c:if>><a href="${contextroot}">Scorebord</a></li>
              <% if(request.isUserInRole("Moderator")) { %>
              <li <c:if test="${tab == 'teams'}">class="current-tab"</c:if>><a href="${contextroot}/manage/team">Teams</a></li>
              <% } %>
              <% if(request.isUserInRole("Administrator")) { %>
              <li <c:if test="${tab == 'admin'}">class="current-tab"</c:if>><a href="${contextroot}/admin">Beheer</a></li>
              <% } %>
            </ul>
          </div>
          <div class="clearer">&nbsp;</div>
        </div>
        <div class="clearer">&nbsp;</div>
      </div>
      <div id="splash">
        <jsp:doBody />
      </div>
      <div id="footer">
        <div class="left" id="footer-left">
          <p>Deze software werd ontwikkeld door <a href="http://zeus.ugent.be">Zeus WPI</a>,
            de werkgroep informatica.</p>
        </div>
      </div>
    </div>
  </body>
</html>