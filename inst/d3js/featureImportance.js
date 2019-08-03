var minValue = options.xmin;
var maxValue = options.xmax;
var n = options.n;
var m = options.m;
var barWidth = options.barWidth,
    chartTitle = options.chartTitle;

var maxLength = calculateTextWidth(data[1])+15;

var margin = {top: 98, right: 30, bottom: 71, left: maxLength, inner: 42},
    w = width - margin.left - margin.right,
    h = height - margin.top - margin.bottom,
    plotTop = margin.top,
    plotHeight = m*barWidth + (m+1)*barWidth/2;

if (options.scaleHeight === true) {
  if (h > n*plotHeight + (n-1)*margin.inner) {
    var temp = h - n*plotHeight - (n-1)*margin.inner;
    plotTop += temp/2;
  }
}

var colors = getColors(n, "bar");

featureImportance(data[0]);

svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

// plot function
function featureImportance(data) {
    var i = 1;

    for (var j in data) {
      var modelName = j;
      var modelData = data[j];
      singlePlot(modelName, modelData, i);
      i += 1;
    }
}

function singlePlot(modelName, modelData, i) {

  var x = d3.scaleLinear()
      .range([margin.left, margin.left + w])
      .domain([minValue, maxValue]);

  if (i == n){
    // xaxis
      svg.append("text")
        .attr("transform",
              "translate(" + (width/2) + " ," +
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
      .rangeRound([plotTop + plotHeight, plotTop])
      .padding(0.33)
      .domain(modelData.map(function (d) {
           return d.variable;
      }));

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
  let str = xGrid.select('.tick:first-child').attr('transform');
  var yGridStart = str.substring(str.indexOf("(")+1,str.indexOf(","));
  str = xGrid.select('.tick:last-child').attr('transform');
  var yGridEnd = str.substring(str.indexOf("(")+1,str.indexOf(","));

  var yGrid = svg.append("g")
       .attr("class", "grid")
       .attr("transform", "translate(" + yGridStart + ",0)")
       .call(d3.axisLeft(y)
              .tickSize(-(yGridEnd-yGridStart))
              .tickFormat("")
      ).call(g => g.select(".domain").remove());

  // yaxis
  var yAxis = d3.axisLeft(y)
      .tickSize(0);

  yAxis = svg.append("g")
      .attr("class", "axisLabel")
      .attr("transform","translate(" + (yGridStart-10) + ",0)")
      .call(yAxis)
      .call(g => g.select(".domain").remove());

  if (i == 1){
    // title
      svg.append("text")
        .attr("x", yGridStart)
        .attr("y", plotTop - 60)
        .attr("class", "bigTitle")
        .text(chartTitle);
  }

  // modelname
  svg.append("text")
      .attr("x", yGridStart)
      .attr("y", plotTop - 15)
      .attr("class", "smallTitle")
      .text(modelName);

  // tooltip
  var tool_tip = d3.tip()
        .attr("class", "tooltip")
        .offset([-8, 0])
        .html(d => staticTooltipHtml(modelName,d));
  svg.call(tool_tip);

  // bars
  var bars = svg.selectAll()
      .data(modelData)
      .enter()
      .append("g");

  // find full model dropout_loss value
  var fullModel = modelData[0].full_model;

  bars.append("rect")
      .attr("class", modelName.replace(/\s/g,''))
      .attr("fill", colors[i-1])
      .attr("y", d => y(d.variable))
      .attr("height", y.bandwidth())
      .attr("x", function (d) {
        // start ploting the bar left to full model line
        if (x(d.dropout_loss) < x(fullModel)) {
          return x(d.dropout_loss);
        } else {
          return x(fullModel);
        }
      })
      .attr("width", d => Math.abs(x(d.dropout_loss) - x(fullModel)))
      .on('mouseover', tool_tip.show)
      .on('mouseout', tool_tip.hide);

  // make line next to bars
  var minimumY = Number.MAX_VALUE;
  var maximumY = Number.MIN_VALUE;
  bars.selectAll(".".concat(modelName.replace(/\s/g,''))).each(function() {
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

  plotTop += (margin.inner + plotHeight);
}

function staticTooltipHtml(modelName, d){
    let sign;
    if (d.dropout_loss > d.full_model) sign = "+"; else sign = "";
    var temp ="Model: " + modelName
      + "</br>" +
      "Model loss after feature " + d.variable
      + "</br>" +
      " is permuted: " +  Math.round(d.dropout_loss * 1000)/1000
      + "</br>" +
      "Drop-out loss change: "  + sign + Math.round((d.dropout_loss - d.full_model) * 1000)/1000 ;
    return temp;
}
