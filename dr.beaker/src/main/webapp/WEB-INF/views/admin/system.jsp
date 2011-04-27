<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Systeem beheer" tab="admin" subtab="system">
  <ul:section title="Beschikbare tellers">
    Wissel tussen verschillende telsystemen of de manuele telconsole. De naam van
    het telsysteem is ofwel het IP adres van het systeem of het woord 'console'
    om de manuele telconsole te activeren.<br>
    <strong>Momenteel staat hier geen validatie op. Maak dus geen fouten!</strong><br><br>
    Huidige counter: ${it}

    <form method="post" action="">
      <label for="name">Telsysteem</label>
      <input type="text" name="name" id="name" title="IP address or 'console'"><br>
      <input type="submit" value="Wisselen">
    </form>
  </ul:section>
  <ul:section title="Mass bootstrap field">
    Paste a space-seperated list of "stick id - mac - teamname" to mass insert.
    <form action="<c:url value="/admin/system/bootstrap" />" method="post">
      <textarea name="data"></textarea>
      <input type="submit" value="bootstrap">
    </form>
  </ul:section>
</ul:page>