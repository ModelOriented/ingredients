var xMax = options.xmax,
    xMin = 0;
var n = options.n;
var m = options.m;
var barWidth = options.barWidth,
    chartTitle = options.chartTitle;

var maxLength = calculateTextWidth(data[1]) + 15;

var margin = {top: 78, right: 30, bottom: 50, left: maxLength, inner: 42},
    h = height - margin.top - margin.bottom,
    plotTop = margin.top,
    plotHeight = m*barWidth + (m+1)*barWidth/2,
    plotWidth = 420*1.2;

if (options.scaleHeight === true) {
  if (h > n*plotHeight + (n-1)*margin.inner) {
    var temp = h - n*plotHeight - (n-1)*margin.inner;
    plotTop += temp/2;
  }
}

var colors = getColors(m, "bar");

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
      .rangeRound([(plotTop + plotHeight), plotTop])
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
  var yGridEnd = str.substring(str.indexOf("(")+1,str.indexOf(","));

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
      .attr("fill", (d, i) => colors[m-i-1])
      .attr("y", d => y(d.label))
      .attr("height", y.bandwidth())
      .attr("x", x(0))
      .attr("width", d => x(d.dropout_loss)-x(0))
      .on('mouseover', tool_tip.show)
      .on('mouseout', tool_tip.hide);

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
