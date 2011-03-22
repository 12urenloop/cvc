/**
 * based upon code from http://www.html5laboratory.com/creating-a-bar-chart-with-canvas.php  by Ian Devlin
 * adapted by Jens Timmerman
 * This code is free to use, modify and distribute
 * No Warranty whatsoever 
 */ 
function drawBarChart(context, data, startX,startY, chartWidth, chartHeight, markDataIncrementsIn,border,textsize,colors) {
  // Draw the x and y axes
  //context.lineWidth = "1.0";
  //drawLine(context, startX, startY, startX, 30); 
  //drawLine(context, startX, startY, 570, startY);			
  context.lineWidth = "0.0";
  context.font = "20px";
  var maxValue = 0;
  for (var i=0; i < data.length; i++) {
    var values = data[i].split(",");
    var height = parseInt(values[1]);
    if (parseInt(height) > parseInt(maxValue)) maxValue = height;
  }
  //border should also scale barwidth
  barWidth = (chartWidth-border*2)/data.length;
  for (var i=0; i < data.length; i++) {
    // Extract the data
    var values = data[i].split(",");
    var name = values[0];
    var height = parseInt(values[1]);
	
    // Write the data to the chart
    // leave 10% gap between bars
    context.fillStyle = colors[i];
    var scaledheight=height/(maxValue)*(chartHeight-border*2);
    drawRectangle(context,startX +border + (i * barWidth) + i,startY-scaledheight-border,barWidth*.9,scaledheight-textsize,true);
	
    // Add the column title to the x-axis
    context.textAlign = "left";
    context.fillStyle = "#000";
    context.fillText(name, startX +border+ (i * barWidth) + i, startY-border , 200);		
  }
  // Add some data markers to the y-axis
  var numMarkers = Math.ceil(maxValue / markDataIncrementsIn);
  context.textAlign = "right";
  context.fillStyle = "#000";
  var markerValue = 0;
  for (var i=0; i < numMarkers; i++) {		
    context.fillText(markerValue, (startX+border-textsize), (startY-border - markerValue/(maxValue)*(chartHeight-border*2)), 50);
    markerValue += markDataIncrementsIn;
  }
}


// drawLine - draws a line on a canvas context from the start point to the end point 
function drawLine(contextO, startx, starty, endx, endy) {
  contextO.beginPath();
  contextO.moveTo(startx, starty);
  contextO.lineTo(endx, endy);
  contextO.closePath();
  contextO.stroke();
}

// drawRectangle - draws a rectangle on a canvas context using the dimensions specified
function drawRectangle(contextO, x, y, w, h, fill) {			
  contextO.beginPath();
  contextO.rect(x, y, w, h);
  contextO.closePath();
  contextO.stroke();
  if (fill) contextO.fill();
}
