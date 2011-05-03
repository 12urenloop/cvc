/**
 * based upon code from http://www.html5laboratory.com/creating-a-bar-chart-with-canvas.php  by Ian Devlin
 * adapted by Jens Timmerman
 * This code is free to use, modify and distribute
 * No Warranty whatsoever 
 */ 
function drawBarChart(context, labels,data, startX,startY, chartHeight, chartWidth, markDataIncrementsIn,border,textsize,colors) {
  // Draw the x and y axes
  //context.lineWidth = "1.0";
  startX = border;
  //drawLine(context, startX, startY, startX, 30); 
  //drawLine(context, startX, startY, 570, startY);			
  context.lineWidth = "0.0";
  context.font = "20px";
  var maxValue = 0;

  for (var i=0; i < data.length; i++) {
    var height = parseInt(data[i]);
    if (parseInt(height) > parseInt(maxValue)) maxValue = height;
  }
  //border should also scale barwidth + 1extra bar for markerValues
  barWidth = (chartWidth-border*2)/(data.length+1);
  for (var i=0; i < data.length; i++) {
    // Extract the data
    var name = labels[i];
    var height = parseInt(data[i]);
    //alert(values);

    // Write the data to the chart
    // leave 10% gap between bars
    context.fillStyle = colors[i];
    var scaledheight=height/(maxValue)*(chartHeight-border*2);
    drawRectangle(context,border + ((i+1) * barWidth) + i,border,barWidth*.9,scaledheight-textsize,true);
    //add black border around bar	
    context.strokeStyle = "#000";
    drawRectangle(context,border + ((i+1) * barWidth) + i,border,barWidth*.9,scaledheight-textsize,false);

   context.font= barWidth*.5 +  "pt Arial";
	
    // Add the column title to the x-axis
    context.textAlign = "left";
    context.fillStyle = "#000";
    context.fillText(name,1.2* border,2*border+ ((i+1) * barWidth) + i,chartHeight);		
  }
  // Add some data markers to the y-axis
  var numMarkers = 10;
  var markDataIncrementsIn =Math.round( maxValue /numMarkers);
  context.textAlign = "right";
  context.fillStyle = "#000";
  var markerValue = 0;
  for (var i=0; i < numMarkers; i++) {		
    context.fillText(markerValue, (border + markerValue/(maxValue)*(chartHeight-border*2)),(border+barWidth*.8), barWidth*.9);
    markerValue += markDataIncrementsIn;
  }
}


// drawLine - draws a line on a canvas context from the start point to the end point 
function drawLine(contextO, starty, startx, endy, endx) {
  contextO.beginPath();
  contextO.moveTo(startx, starty);
  contextO.lineTo(endx, endy);
  contextO.closePath();
  contextO.stroke();
}

// drawRectangle - draws a rectangle on a canvas context using the dimensions specified
function drawRectangle(contextO, y, x, h, w, fill) {			
  contextO.beginPath();
  contextO.rect(x, y, w, h);
  contextO.closePath();
  contextO.stroke();
  if (fill){
     contextO.fill();
  }
}
