// load all data
var variableNames = options.variableNames, n = options.n, labelCount = options.c;
var yMax = options.yMax, yMin = options.yMin, yMean = options.yMean;
var size = options.size, alpha = options.alpha, color = options.color;
var onlyNumerical = options.onlyNumerical, m = options.facetNcol;
var chartTitle = options.chartTitle, labelNames = options.labelNames;

var plotHeight, plotWidth;
var margin = {top: 78, right: 30, bottom: 50, left: 65, inner: 70, inner2: 0};
var labelMargin = options.labelMargin;

m = d3.min([n,m]);

if (m != 1) {
  if (onlyNumerical === true) {
    margin.inner2 = 25;
  } else {
    margin.inner2 = labelMargin;
    margin.left = labelMargin;
  }
} else if (onlyNumerical === false) {
  margin.left = labelMargin;
}

var w = width - margin.left - margin.right;

var plotTop = margin.top, plotLeft = margin.left;

if (options.scalePlot === true) {
  plotWidth = (w-(m-1)*margin.inner2)/m;
  plotHeight = 2*plotWidth/3;
} else {
  plotHeight = 280;
  plotWidth = 420;
}

var tColors = getColors(3, "point");
var dbColor = tColors[0];
var lbColor = color;
var colors = getColors(labelCount, "line");

// plot
aggregatedProfiles(data);

// change font
svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

function aggregatedProfiles(data) {
  var profData = data[0], minMaxData = data[1];

  // lines or bars?
  if (onlyNumerical) {
    for (let i=0; i<n; i++) {
      let variableName = variableNames[i];
      numericalPlot(variableName, profData[variableName], minMaxData[variableName], i+1);
    }
  } else {
    for (let i=0; i<n; i++) {
      let variableName = variableNames[i];
      categoricalPlot(variableName, profData[variableName], i+1);
    }
  }
}

function numericalPlot(variableName, lData, mData, i) {

  var x = d3.scaleLinear()
            .range([plotLeft + 10, plotLeft + plotWidth - 10])
            .domain([mData[1], mData[0]]);

  var y = d3.scaleLinear()
            .range([plotTop + plotHeight, plotTop])
            .domain([yMin, yMax]);

  var line = d3.line()
               .x(d => x(d.xhat))
               .y(d => y(d.yhat))
               .curve(d3.curveMonotoneX);

  if (i == 1) {
      svg.append("text")
          .attr("class", "bigTitle")
          .attr("x", plotLeft)
          .attr("y", plotTop - 40)
          .text(chartTitle);

      if (labelCount > 1) {
        // add legend
        var tempW = -20+14;

        var legend = svg.selectAll(".legend")
              .data(labelNames)
              .enter()
              .append("g")
              .attr("class", "legend")
              .attr("transform", function(d, i) {
                let temp = getTextWidth(d, 11, "Fira Sans, sans-serif");
                tempW = tempW + temp + 20;
                return "translate(" + (margin.left+(m*plotWidth) + (m-1)*margin.inner2 - tempW) +
                    "," + (margin.top - 50) + ")";
              });

        legend.append("text")
              .attr("dy", ".6em")
              .attr("class", "legendLabel")
              .text(d => d)
              .attr("x", 14);

        legend.append("rect")
                .attr("width", 8)
                .attr("height", 8)
                .attr("class", "legendBox");

        legend.append("circle")
                .attr("class", "legendDot")
                .attr("cx", 4)
                .attr("cy", 4)
                .attr("r", 2.5)
                .style("fill", (d, i) => colors[i]);
      }
  }

  svg.append("text")
      .attr("class","smallTitle")
      .attr("x", plotLeft)
      .attr("y", plotTop - 15)
      .text(variableName);

  // find 5 nice ticks with max and min - do better than d3
  var tickValues = getTickValues(x.domain());

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

  if (i == 1 || i%m == 1) {
      var yAxis = d3.axisLeft(y)
              .ticks(5)
              .tickSize(0);

      yAxis = svg.append("g")
              .attr("class", "axisLabel")
              .attr("transform","translate(" + plotLeft + ",0)")
              .call(yAxis)
              .call(g => g.select(".domain").remove());
    }

  // make tooltip
  var tool_tip = d3.tip()
            .attr("class", "d3-tip")
            .html(d => staticTooltipHtml(d, variableName));

  svg.call(tool_tip);

  // function to find nearest point on the line
  var bisectXhat = d3.bisector(d => d.xhat).right;

  // tooltip appear with info nearest to mouseover
  function appear(data) {
    var x0 = x.invert(d3.mouse(d3.event.currentTarget)[0]),
        i = bisectXhat(data, x0),
        d0 = data[i - 1],
        d1 = data[i],
        d = x0 - d0.xhat > d1.xhat - x0 ? d1 : d0;

    tool_tip.show(d);
  }

  // add lines
  Object.keys(lData).forEach(function(key,j) {
      svg.append("path")
        .data([lData[key]])
        .attr("class", "line " + variableName)
        .attr("d", line)
        .style("fill", "none")
        .style("stroke", colors[j])
        .style("opacity", alpha)
        .style("stroke-width", size)
        .on('mouseover', function(d) {

          d3.select(this)
            .style("stroke-width", size*1.5);

          // make line and points appear on top
          this.parentNode.appendChild(this);

          // show changed tooltip
          appear(d);
        })
        .on('mouseout', function(d){

          d3.select(this)
            .style("stroke-width", size);

          // hide changed tooltip
          tool_tip.hide(d);
        });
  });

  if (i == n) {
    svg.append("text")
          .attr("class", "axisTitle")
          .attr("transform", "rotate(-90)")
          .attr("y", 15)
          .attr("x", -(margin.bottom + plotTop + plotHeight)/2)
          .attr("text-anchor", "middle")
          .text("average prediction");
  }

  if (i%m !== 0) {
    plotLeft += (margin.inner2 + plotWidth);
  }
  if (i%m === 0) {
    plotLeft -= (m-1)*(margin.inner2+plotWidth);
    plotTop += (margin.inner + plotHeight);
  }
}

function categoricalPlot(variableName, bData, i) {

  var x = d3.scaleLinear()
        .range([plotLeft, plotLeft + plotWidth])
        .domain([yMin, yMax]);

  // plot one xAxis per facet column
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
        .rangeRound([plotTop, plotTop + plotHeight])
        .padding(0.33)
        .domain(bData.map(d => d.xhat));

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
        .attr("transform","translate(" + (plotLeft - 8) + ",0)")
        .call(yAxis)
        .call(g => g.select(".domain").remove());

  // wrap y label text
  yAxis.selectAll("text").call(wrapText, margin.left - 10);

  svg.append("text")
        .attr("x", plotLeft)
        .attr("y", plotTop - 15)
        .attr("class", "smallTitle")
        .text(variableName);

  if (i == 1) {
    svg.append("text")
          .attr("x", plotLeft)
          .attr("y", plotTop - 40)
          .attr("class", "bigTitle")
          .text(chartTitle);
  }

  var bars = svg.selectAll()
        .data(bData)
        .enter()
        .append("g");

  var fullModel = yMean;

  // make tooltip
  var tool_tip = d3.tip()
        .attr("class", "d3-tip")
        .html(d => staticTooltipHtml(d, variableName));

  svg.call(tool_tip);

  // add bars
  bars.append("rect")
        .attr("class", variableName)
        .attr("fill", lbColor)
        .attr("y", d => y(d.xhat))
        .attr("height", y.bandwidth())
        .attr("x", function(d) {
          // start ploting the bar left to full model line
          if (x(d.yhat) < x(fullModel)) {
            return x(d.yhat);
          } else {
            return x(fullModel);
          }
        })
        .attr("width", d => Math.abs(x(d.yhat) - x(fullModel)))
        .on('mouseover', tool_tip.show)
        .on('mouseout', tool_tip.hide);

  // add intercept line
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

  if (i == n) {
    svg.append("text")
        .attr("transform",
              "translate(" + (margin.left + m*plotWidth + (m-1)*margin.inner2 + margin.right)/2 + " ," +
                             (plotTop + plotHeight + 45) + ")")
        .attr("class", "axisTitle")
        .attr("text-anchor", "middle")
        .text("average prediction");
  }

  if (i%m !== 0){
    plotLeft += (margin.inner2 + plotWidth);
  }
  if (i%m === 0){
    plotLeft -= (m-1)*(margin.inner2+plotWidth);
    plotTop += (margin.inner + plotHeight);
  }
}

function staticTooltipHtml(d, variableName){
  // function formats tooltip text
  var temp = "";
  for (var [k, v] of Object.entries(d)) {
    switch (k) {
      case "xhat":
        temp += "<center>" +  variableName  + ": " + v + "</br>";
        break;
      case "yhat":
        temp += "<center>" +  "average prediction"  + ": " + v + "</br>";
        break;
      case "vname":
        break;
      default:
        temp += "<center>" +  k  + ": " + v + "</br>";
        break;
    }
  }

  temp += onlyNumerical ? "" : "<center>" +  "mean observation prediction:"
                                + "</br>" + yMean + "</br>";
  return temp;
}
