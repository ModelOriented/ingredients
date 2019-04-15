var variableNames = options.variableNames, n = options.n;
var yMax = options.yMax, yMin = options.yMin;
var size = options.size, alpha = options.alpha;
var onlyNumerical = options.onlyNumerical, m = options.facetNcol;
var chartTitle = options.chartTitle;
var showObservations = options.showObservations, showRugs = options.showRugs;

var plotHeight, plotWidth;
var margin = {top: 98, right: 30, bottom: 50, left: 65, inner: 70, inner2: 0};
var labelsMargin = options.labelsMargin;

if (m != 1) { 
  if (onlyNumerical === true){ 
    margin.inner2 = 25;
  } else { 
    margin.inner2 = labelsMargin;
    margin.left = labelsMargin;
  }
}

var w = width - margin.left - margin.right;
var h = height - margin.top - margin.bottom;

var plotTop = margin.top, plotLeft = margin.left;

if (options.scalePlot === true) {
  plotWidth = (w-(m-1)*margin.inner2)/m;
  plotHeight = 2*plotWidth/3;
} else {
  plotHeight = 280;
  plotWidth = 420;
}

var colors = getColors(3, "point");
var pointColor = colors[0];
var lineColor = colors[1];
var greyColor = colors[2];

ceterisParibus(data);

svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

function ceterisParibus(data){
  var profData = data[0], minMaxData = data[1], obsData = data[2];
  
  if (onlyNumerical) {
    for (var i=0; i<n; i++){
      var variableName = variableNames[i];
      numericalPlot(variableName, profData[variableName],
      minMaxData[variableName], obsData, i+1);
    }
  } else {
    for (var i=0; i<n; i++){
      var variableName = variableNames[i];
      categoricalPlot(variableName, profData[variableName], obsData, i+1);
    }
  }  
}

function numericalPlot(variableName, lData, mData, pData, i) {
  
  var x = d3.scaleLinear()
            .range([plotLeft + 10, plotLeft + plotWidth - 10])
            .domain([mData[1], mData[0]]);

  var y = d3.scaleLinear()
            .range([plotTop + plotHeight, plotTop])
            .domain([yMin, yMax]);
  
  var line = d3.line()
               .x(function(d) { return x(d.xhat); })
               .y(function(d) { return y(d.yhat); })
               .curve(d3.curveMonotoneX); 
  
  if (i==1) {
      svg.append("text")
          .attr("class", "bigTitle")
          .attr("x", plotLeft)
          .attr("y", plotTop - 60)
          .text(chartTitle);
  }
    
  svg.append("text")
      .attr("class","smallTitle")
      .attr("x", plotLeft)
      .attr("y", plotTop - 15)
      .text(variableName);
  
  // find 5 nice ticks with max and min - do better than d3
  var domain = x.domain();
  var tickValues = d3.ticks(domain[0], domain[1],5);

  switch (tickValues.length){
    case 3:
      tickValues.unshift(domain[0]);
      tickValues.push(domain[1]);
      break;

    case 4:
      if(Math.abs(domain[0] - tickValues[0]) < Math.abs(domain[1] - tickValues[3])){
        tickValues.shift();
        tickValues.unshift(domain[0]);
        tickValues.push(domain[1]);
      } else {
        tickValues.pop();
        tickValues.push(domain[1]);
        tickValues.unshift(domain[0]);
      }
      break;

    case 5:
      tickValues.pop();
      tickValues.shift();
      tickValues.push(domain[1]);
      tickValues.unshift(domain[0]);
      break;

    case 6:
      if(Math.abs(domain[0] - tickValues[0]) < Math.abs(domain[1] - tickValues[3])){
        tickValues.pop();
        tickValues.shift();
        tickValues.shift();
        tickValues.push(domain[1]);
        tickValues.unshift(domain[0]);
      } else {
        tickValues.pop();
        tickValues.pop();
        tickValues.shift();
        tickValues.push(domain[1]);
        tickValues.unshift(domain[0]);
      }
      break;

    case 7:
      tickValues.pop();
      tickValues.pop();
      tickValues.shift();
      tickValues.shift();
      tickValues.push(domain[1]);
      tickValues.unshift(domain[0]);
  }
   
  var xAxis = d3.axisBottom(x)
              .tickValues(tickValues)
              .tickSizeInner(0)
              .tickPadding(15);

  xAxis = svg.append("g")
              .attr("class", "axisLabel")
              .attr("transform", "translate(0,"+ (plotTop + plotHeight) + ")")
              .call(xAxis);

  var yGrid = svg.append("g")
             .attr("class", "grid")
             .attr("transform", "translate(" + plotLeft + ",0)")
             .call(d3.axisLeft(y)
                    .ticks(10)
                    .tickSize(-plotWidth)
                    .tickFormat("")
            ).call(g => g.select(".domain").remove());
            
  if (i%2 === 1) {
      var yAxis = d3.axisLeft(y)
              .ticks(5)
              .tickSize(0);

      yAxis = svg.append("g")
              .attr("class", "axisLabel")
              .attr("transform","translate(" + plotLeft + ",0)")
              .call(yAxis)
              .call(g => g.select(".domain").remove());
    }
  
  Object.keys(lData).forEach(function(key) {
      svg.append("path")
        .data([lData[key]])
        .attr("class", "line " + variableName)
        .attr("id", variableName)
        .attr("d", line)
        .style("fill", "none")
        .style("stroke", lineColor)
        .style("opacity", alpha)
        .style("stroke-width", size); 
  });

  var tool_tip = d3.tip()
        .attr("class", "d3-tip")
        .offset([-8, 0])
        .html(function(d) { return pTooltipHtml(d); });
  svg.append("g").call(tool_tip);
  
  if (showObservations === true) {
      svg
        .selectAll()
        .data(pData)
        .enter()
        .append("circle")
        .attr("class", "dot " + variableName)
        .attr("id", variableName)
        .attr("cx", d => x(d[variableName]))
        .attr("cy", d => y(d.yhat))
        .attr("r", 3)
        .style("stroke-width", 15)
        .style("stroke", "red")
        .style("stroke-opacity", 0)        
        .style("fill", pointColor)
        .on('mouseover', tool_tip.show)
        .on('mouseout', tool_tip.hide);
    }
  
  if (i==n){
    svg.append("text")
          .attr("class", "axisTitle")
          .attr("transform", "rotate(-90)")
          .attr("y", 15)
          .attr("x", -(margin.top + plotTop + plotHeight)/2)
          .attr("text-anchor", "middle")
          .text("prediction");
  }
  
  if (i%m !== 0){
    plotLeft += (margin.inner2 + plotWidth);
  }
  if (i%m === 0){
    plotLeft -= (margin.inner2 + (m-1)*plotWidth);
    plotTop += (margin.inner + plotHeight);
  }
}

function categoricalPlot(variableName, bData, lData, i){
  
  var x = d3.scaleLinear()
        .range([plotLeft,  plotLeft + plotWidth])
        .domain([yMin, yMax]);
  
  if (i > n - m) {
   var xAxis = d3.axisBottom(x)
                .ticks(5)
                .tickSize(0);

   xAxis = svg.append("g")
          .attr("class", "axisLabel")
          .attr("transform", "translate(0," + (plotTop + plotHeight) + ")")
          .call(xAxis)
          .call(g => g.select(".domain").remove()); 
  }
  
  var y = d3.scaleBand()
        .rangeRound([plotTop + plotHeight, plotTop])
        .padding(0.33)
        .domain(bData.map(function (d) {
             return d.xhat;
        }));
        
  var xGrid = svg.append("g")
         .attr("class", "grid")
         .attr("transform", "translate(0," + (plotTop + plotHeight) + ")")
         .call(d3.axisBottom(x)
                .ticks(10)
                .tickSize(-plotHeight)
                .tickFormat("")
        ).call(g => g.select(".domain").remove());

  var yGrid = svg.append("g")
         .attr("class", "grid")
         .attr("transform", "translate(" + plotLeft + ",0)")
         .call(d3.axisLeft(y)
                .tickSize(-plotWidth)
                .tickFormat("")
        ).call(g => g.select(".domain").remove());

  var yAxis = d3.axisLeft(y)
        .tickSize(0);

  yAxis = svg.append("g")
        .attr("class", "axisLabel")
        .attr("transform","translate(" + (plotLeft-8) + ",0)")
        .call(yAxis)
        .call(g => g.select(".domain").remove());
  
  svg.append("text")
        .attr("x", plotLeft)
        .attr("y", plotTop - 15)
        .attr("class", "smallTitle")
        .text(variableName + " = " + lData[0][variableName]);
        
  if (i == 1){
    svg.append("text")
          .attr("x", plotLeft)
          .attr("y", plotTop - 60)
          .attr("class", "bigTitle")
          .text(chartTitle);
  }
  
  var bars = svg.selectAll()
        .data(bData)
        .enter()
        .append("g");

  var fullModel = lData[0].yhat;

  var tool_tip = d3.tip()
        .attr("class", "d3-tip")
        .offset([-8, 0])
        .html(function(d) { return bTooltipHtml(d, lData[0]); });
  svg.append("g").call(tool_tip);
  
  bars.append("rect")
        .attr("class", variableName)
        .attr("fill", lineColor)
        .attr("y", function (d) {
            return y(d.xhat);
        })
        .attr("height", y.bandwidth())
        .attr("x", function (d) {
          // start ploting the bar left to full model line
          if (x(d.yhat) < x(fullModel)) {
            return x(d.yhat);
          } else {
            return x(fullModel);
          }
        })
        .attr("width", function (d) {
            return  Math.abs(x(d.yhat) - x(fullModel));
        })
        .on('mouseover', tool_tip.show)
        .on('mouseout', tool_tip.hide);

  var minimumY = Number.MAX_VALUE;
  var maximumY = Number.MIN_VALUE;
  bars.selectAll(".".concat(variableName)).each(function() {
      if(+this.getAttribute('y') < minimumY) {
        minimumY = +this.getAttribute('y');
      }
      if(+this.getAttribute('y') > maximumY) {
        maximumY = +this.getAttribute('y');
      }
    });

  svg.append("line")
        .attr("class", "interceptLine")
        .attr("x1", x(fullModel))
        .attr("y1", minimumY)
        .attr("x2", x(fullModel))
        .attr("y2", maximumY + y.bandwidth());
  
  if (i==n) {
    svg.append("text")
        .attr("transform",
              "translate(" + (margin.left + m*plotWidth + (m-1)*margin.inner2 + margin.right)/2 + " ," +
                             (plotTop + plotHeight + 45) + ")")
        .attr("class", "axisTitle")
        .text("prediction");  
  }
        
  if (i%m !== 0){
    plotLeft += (margin.inner2 + plotWidth);
  }
  if (i%m === 0){
    plotLeft -= (m-1)*(margin.inner2+plotWidth);
    plotTop += (margin.inner + plotHeight);
  } 
}

function pTooltipHtml(d){
  
  var temp = "";
  for (var [k, v] of Object.entries(d)) {
    if (k === "yhat") {
      k = "prediction";
      temp += "<center>" +  k + ": " + v + "</br>";
      temp += "</br>";
    } else{
      temp += "<center>" +  k + ": " + v + "</br>";
    }
  }
  return temp;
}

function bTooltipHtml(d, addData) {
      
  var temp = "<center>";
  for (var [k, v] of Object.entries(addData)) {
    if (k === d.vname){
      temp +=  k + ": " + "<font color = \"red\">"  +  d.xhat + "</br></font>";
    } else if (k === "yhat") {
      temp += "prediction:</br>";
      temp += "- before" + ": " + v + "</br>";
      temp += "- after" + ": " + "<font color = \"red\">" + d.yhat + "</br></font>";
      temp += "</br>";
    } else {
      temp += k + ": " + v + "</br>";
    }
  }
  temp += "</center>";
  return temp;
}

