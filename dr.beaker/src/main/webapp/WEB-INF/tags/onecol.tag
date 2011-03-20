
<%@tag description="put the tag description here" pageEncoding="UTF-8"%>

<%@attribute name="title" %>

<div class="main">
  <div class="section">
    <div class="section-title">${title}</div>
    <div class="section-content"><jsp:doBody /></div>
  </div>
</div>