<%@page contentType="text/html" pageEncoding="UTF-8"%>

<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="ul" tagdir="/WEB-INF/tags" %>

<ul:page title="Bonus management" tab="admin" subtab="bonusses">
  <script>

    function submit (event) {
      event.preventDefault();
      $.ajax({
        type : "POST",
        url : $(this).attr('action'),
        data : $(this).serialize(),
        success : function (response) {
          var message = $('#message');
          message.text(response);
          message.show();
          setTimeout(function() {message.slideUp(500);}, 1000);
        }
      });
    }
    
    $(function(){
      $("#single-bonus").submit(submit);
      $("#multi-bonus").submit(submit);      
    });
  </script>
  <div class="success" id="message" style="display:none;"></div>
  <ul:section title="Bonus voor een enkel team">
    <form id="single-bonus" action="<c:url value="/admin/bonus/add" />" method="POST">
      <label for="reason">Reden</label>
      <input type="text" id="reason" name="reason"><br>
      <label for="team">Team</label>
      <select id="team" name="team">
        <c:forEach items="${it}" var="team">
          <option value="${team.id}">${team.name}</option>
        </c:forEach>
      </select><br>
      <label for="bonus">Bonus</label>
      <input type="text" id="bonus" name="bonus" size="2" maxlength="2"><br>
      <input type="submit" value="Toekennen">
    </form>
  </ul:section>
  <ul:section title="Bonus voor meerdere teams">
    <form id="multi-bonus" action="<c:url value="/admin/bonus/multi-add" />" method="POST">
      <label for="bonusses">Bonus</label>
      <input type="text" id="bonusses" name="bonus" size="2" maxlength="2" value="5"><br>
      <label for="reason">Reden</label>
      <input type="text" id="reason" name="reason" size="50" value="Deelname aan een speciale ronde"><br>
      <label for="team">Teams</label><br>
      <c:forEach items="${it}" var="team">
        <input type="checkbox" name="teams" id="teams[${team.id}]" value="${team.id}">
        <label style="width: 200px; vertical-align: top; padding-left: 5px;" for="teams[${team.id}]">${team.name}</label>
      </c:forEach>
      <br>
      <input type="submit" value="Toekennen">
    </form>
  </ul:section>
</ul:page>