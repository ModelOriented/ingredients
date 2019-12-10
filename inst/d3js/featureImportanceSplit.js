var xMax = options.xmax,
    xMin = 0;
var n = options.n;
var m = typeof(options.m) === "number" ? [options.m] : options.m;
var showBoxplots = options.showBoxplots,
    barWidth = options.barWidth,
    boxWidth = 2*barWidth/3,
    scaleHeight = options.scaleHeight,
    chartTitle = options.chartTitle;

var maxLength = calculateTextWidth(data[1]) + 15;

var margin = {top: 78, right: 30, bottom: 50, left: maxLength, inner: 42},
    h = height - margin.top - margin.bottom,
    plotTop = margin.top,
    plotHeightArr = [],
    plotWidth = 420*1.2;

for (e of m) {
  plotHeightArr.push(e*barWidth + (e+1)*barWidth/2);
}

if (scaleHeight === true) {
  if (h > d3.sum(plotHeightArr) + (n-1)*margin.inner) {
    var temp = h - d3.sum(plotHeightArr) - (n-1)*margin.inner;
    plotTop += temp/2;
  }
}

var colors = d3.scaleOrdinal()
                .domain(data[1])
                .range(getColors(d3.max(m), "bar"));

featureImportanceSplit(data);

svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

// plot function
function featureImportanceSplit(data) {
  var fiData = data[0];
  var i = 1;

  for (var j in fiData) {
    var labelName = j;
    var labelData = fiData[j];
    singlePlot(labelName, labelData, i);
    i += 1;
  }
}

function singlePlot(labelName, labelData, i) {

  let plotHeight = plotHeightArr[i-1];

  var x = d3.scaleLinear()
      .range([margin.left, margin.left + plotWidth])
      .domain([xMin, xMax]);

  if (i == n){
    // xaxis
      svg.append("text")
        .attr("transform",
              "translate(" + (margin.left + plotWidth/2) + " ," +
                             (plotTop + plotHeight + 45) + ")")
        .attr("class", "axisTitle")
        .text("Drop-out loss");

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
      .rangeRound([plotTop, plotTop + plotHeight])
      .padding(0.33)
      .domain(labelData.map(d => d.label));

  // grid
  var xGrid = svg.append("g")
       .attr("class", "grid")
       .attr("transform", "translate(0," + (plotTop + plotHeight) + ")")
       .call(d3.axisBottom(x)
              .ticks(10)
              .tickSize(-plotHeight)
              .tickFormat("")
      ).call(g => g.select(".domain").remove());

  // effort to make grid endings clean
  let str = xGrid.select('.tick:last-child').attr('transform');
  var yGridEnd = str.substring(str.indexOf("(")+1, str.indexOf(","));

  var yGrid = svg.append("g")
       .attr("class", "grid")
       .attr("transform", "translate(" + margin.left + ",0)")
       .call(d3.axisLeft(y)
              .tickSize(-(yGridEnd-margin.left))
              .tickFormat("")
      ).call(g => g.select(".domain").remove());

  // yaxis
  var yAxis = d3.axisLeft(y)
      .tickSize(0);

  yAxis = svg.append("g")
      .attr("class", "axisLabel")
      .attr("transform","translate(" + (margin.left - 10) + ",0)")
      .call(yAxis)
      .call(g => g.select(".domain").remove());

  // wrap y label text
  yAxis.selectAll("text").call(wrapText, margin.left - 10);

  if (i == 1) {
    // title
      svg.append("text")
        .attr("x", margin.left)
        .attr("y", plotTop - 40)
        .attr("class", "bigTitle")
        .text(chartTitle);
  }

  // labelname
  svg.append("text")
      .attr("x", margin.left )
      .attr("y", plotTop - 15)
      .attr("class", "smallTitle")
      .text(labelName);

  // tooltip
  var tool_tip = d3.tip()
        .attr("class", "d3-tip")
        .html(d => staticTooltipHtml(labelName, d));

  svg.call(tool_tip);

  // bars
  var bars = svg.selectAll()
      .data(labelData)
      .enter()
      .append("g");

  bars.append("rect")
      .attr("class", labelName.replace(/\s/g,''))
      .attr("fill", d => colors(d.label))
      .attr("y", d => y(d.label))
      .attr("height", y.bandwidth())
      .attr("x", x(0))
      .attr("width", d => x(d.dropout_loss)-x(0))
      .on('mouseover', tool_tip.show)
      .on('mouseout', tool_tip.hide);

  if (showBoxplots) {
    // Show the main horizontal line
    svg
      .selectAll()
      .data(labelData)
      .enter()
      .append("line")
      .attr("class", "interceptLine")
      .attr("x1", d => x(d.min))
      .attr("x2", d => x(d.max))
      .attr("y1", d => y(d.label) + y.bandwidth()/2)
      .attr("y2", d => y(d.label) + y.bandwidth()/2);

    // rectangle for the main box
    svg
      .selectAll()
      .data(labelData)
      .enter()
      .append("rect")
      .attr("x", d => x(d.q1))
      .attr("y", d => y(d.label) + boxWidth/4)
      .attr("height", boxWidth)
      .attr("width", d => x(d.q3) - x(d.q1))
      .style("fill", "#371ea3");
  }

  plotTop += (margin.inner + plotHeight);
}

function staticTooltipHtml(labelName, d) {
    let sign;
    d.dropout_loss > d.full_model ? sign = "+" : sign = "";
    var temp ="Model: " + d.label
      + "</br>" +
      "Model loss after feature " + labelName
      + "</br>" +
      " is permuted: " +  Math.round(d.dropout_loss * 1000)/1000
      + "</br>" +
      "Drop-out loss change: "  + sign + Math.round((d.dropout_loss - d.full_model) * 1000)/1000 ;
    return temp;
}
