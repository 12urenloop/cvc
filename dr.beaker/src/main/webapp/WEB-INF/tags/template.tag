<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
  "http://www.w3.org/TR/html4/loose.dtd">

<%@tag description="Global page template" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%@attribute name="title"%>
<%-- @attribute name="message"%> --%>

<c:set var="contextroot" value="${pageContext.request.contextPath}" />

<html>
  <head>
    <title>${title}</title>
    <link rel="stylesheet" type="text/css" href="${contextroot}/style/style.css" />
  </head>
  <body>
    <div id="header-wrapper">
      <div id="header">
        <h1><a>12 Urenloop</a></h1>
      </div>
    </div>
    <div id="main-wrapper">
      <div id="main" class="cols">
        <jsp:doBody />
      </div>
    </div>
    <div id="footer">
      <p><a href="http://fkgent.be">FaculteitenKonvent Gent</a> &bull;
        De Therminal, Hoveniersberg 24 - 9000 Gent, Belgium &bull;
        +32.9.264.70.93 &bull; info@fkserv.ugent.be</p>
      <p>Deze software werd ontwikkeld door <a href="http://zeus.ugent.be">Zeus WPI</a>,
        de werkgroep informatica.</p>
    </div>
  </body>
</html>

