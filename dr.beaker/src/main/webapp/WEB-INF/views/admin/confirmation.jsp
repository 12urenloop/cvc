<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<c:set var="team" value="${it[0]}" />
<c:set var="stick" value="${it[1]}" />

<ul:page title="Stok bevestiging" tab="admin" subtab="bonusses">
  <ul:section title="Stok bevestiging">
    <form method="POST">
      Ben je zeker dat je de stok <strong>${stick.id}</strong> wil toekennen aan <strong>${team.name}</strong> in plaats van het huidige team <strong>${stick.team.name}</strong> ?<br>
      <input type="hidden" value="${stick.id}" name="stick">
      <input type="hidden" value="${team.id}" name="team">
      <input type="hidden" value="true" name="confirmation">
      <input type="submit" value="Ja, ik ben zeker">
    </form>
  </ul:section>
</ul:page>