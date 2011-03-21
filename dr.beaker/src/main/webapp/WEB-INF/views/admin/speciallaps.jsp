<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Speciale rondes" tab="admin" subtab="speclaps" cols="1">
  <div class="section-title">Speciale rondes</div>
  <div class="section-content">
    <form method="POST" action="">
      Bonus voor elk deelnemend team: <input type="text" name="bonus">
    </form>
  </div>
</ul:page>