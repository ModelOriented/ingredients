// load all data
var variableNames = options.variableNames, n = options.n;
var yMax = options.yMax, yMin = options.yMin;
var size = options.size, alpha = options.alpha, color = options.color;
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
var lineColor = color; //colors[1];
var greyColor = colors[2];

// plot
ceterisParibus(data);

// change font
svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

function ceterisParibus(data){
  var profData = data[0], minMaxData = data[1], obsData = data[2];

  // lines or bars?
  if (onlyNumerical) {
    for (let i=0; i<n; i++){
      let variableName = variableNames[i];
      numericalPlot(variableName, profData[variableName],
      minMaxData[variableName], obsData, i+1);
    }
  } else {
    for (let i=0; i<n; i++){
      let variableName = variableNames[i];
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

  // make tooltip
  var tool_tip = d3.tip()
            .attr("class", "tooltip")
            .offset([-8, 0])
            .html(function(d, addData) {
              if(addData !== undefined){
                return changedTooltipHtml(d, addData);
              } else {
                return staticTooltipHtml(d);
              }
            });
  svg.call(tool_tip);

  // function to find nearest point on the line
  var bisectXhat = d3.bisector(d => d.xhat).right;

  // tooltip appear with info nearest to mouseover
  function appear(data){
    var x0 = x.invert(d3.mouse(d3.event.currentTarget)[0]),
        i = bisectXhat(data, x0),
        d0 = data[i - 1],
        d1 = data[i],
        d = x0 - d0.xhat > d1.xhat - x0 ? d1 : d0;
    let temp = pData.find(el => el["observation.id"] === d.id);
    tool_tip.show(d, temp);
  }

  // add lines
  Object.keys(lData).forEach(function(key) {
      svg.append("path")
        .data([lData[key]])
        .attr("class", "line " + variableName)
        .attr("d", line)
        .style("fill", "none")
        .style("stroke", lineColor)
        .style("opacity", alpha)
        .style("stroke-width", size)
        .on('mouseover', function(d){

          d3.select(this)
            .style("stroke", pointColor)
            .style("stroke-width", size*1.5);

          // make line and points appear on top
          this.parentNode.appendChild(this);
          d3.select(this.parentNode).selectAll(".point").each(function() {
                           this.parentNode.appendChild(this);
                      });

          // show changed tooltip
          appear(d);
        })
        .on('mouseout', function(d){

          d3.select(this)
            .style("stroke", lineColor)
            .style("stroke-width", size);

          // hide changed tooltip
          tool_tip.hide(d);
        });
  });

  if (showObservations === true) {

      // add points
      svg.selectAll()
            .data(pData)
            .enter()
            .append("circle")
            .attr("class", "point")
            .attr("id", d => d["observation.id"])
            .attr("cx", d => x(d[variableName]))
            .attr("cy", d => y(d.yhat))
            .attr("r", 3)
            .style("stroke-width", 15)
            .style("stroke", "red")
            .style("stroke-opacity", 0)
            .style("fill", pointColor)
            .on('mouseover', function(d) {
              tool_tip.show(d);
          		d3.select(this)
          			.attr("r", 6);
          	})
            .on('mouseout', function(d) {
              tool_tip.hide(d);
          		d3.select(this)
          			.attr("r", 3);
          	});
    }

  if (showRugs === true) {

    // add rugs
    svg
      .selectAll()
      .data(pData)
      .enter()
      .append("line")
      .attr("class", "rugLine")
      .style("stroke", "red")
      .style("stroke-width", 2)
      .attr("x1", d => x(d[variableName]))
      .attr("y1", plotTop + plotHeight)
      .attr("x2", d => x(d[variableName]))
      .attr("y2", plotTop + plotHeight - 10);
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

  // make tooltip
  var tool_tip = d3.tip()
        .attr("class", "tooltip")
        .offset([-8, 0])
        .html(function(d) { return changedTooltipHtml(d, lData[0]); });
  svg.call(tool_tip);

  // add bars
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

function staticTooltipHtml(d, addData){
  // function formats tooltip text
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

function changedTooltipHtml(d, addData) {
  // function formats tooltip text with update in red
  var temp = "<center>";
  for (var [k, v] of Object.entries(addData)) {
    if (k === "yhat") {
      temp += "prediction:</br>";
      temp += "- before" + ": " + v + "</br>";
      temp += "- after" + ": " + "<font color = \"red\">" + d.yhat + "</br></font>";
      temp += "</br>";
    } else if (k === d.vname) {
      temp +=  k + ": " + "<font color = \"red\">"  +  d.xhat + "</br></font>";
    } else {
      temp += k + ": " + v + "</br>";
    }
  }
  temp += "</center>";
  return temp;
}
