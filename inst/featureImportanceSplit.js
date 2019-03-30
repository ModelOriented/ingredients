var maxValue = options.xmax;
var n = options.n;
var m = options.m;
var barWidth = options.barWidth;

var margin = {top: 98, right: 30, bottom: 71, left: 80, inner: 42};
var w = width - margin.left - margin.right;
var h = height - margin.top- margin.bottom;
var labelsMargin = margin.left - 8;
var plotTop = margin.top;
var plotHeight = m*barWidth + (m+1)*barWidth/2;
var plotBottom = margin.top + plotHeight;

if (options.scaleHeight === true) {
  if (h > n*plotHeight + (n-1)*margin.inner) {
    var temp = h - n*plotHeight - (n-1)*margin.inner;
    plotTop += temp/2;
    plotBottom += temp/2;
  }
}

var colors = getColors(m, "bar");

featureImportance(data);

// css
var tooltip = d3.select("body").append("div")
          .attr("class", "tooltip")
          .style("position", "absolute")
          .style("text-align", "center")
          .style("width", "300px")
          .style("height", "80px")
          .style("padding", "2px")
          .style("color", "white")
          .style("background", "#371ea3")
          .style("opacity", "1")
          .style("border", "0px")
          .style("border-radius", "8px")
          .style("pointer-events", "none")
          .style("visibility", "hidden");

svg.selectAll("text")
  .style('font-family', 'Fira Sans, sans-serif');

// plot function
function featureImportance(data) {
    var i = 1;

    for (var j in data) {
      var labelName = j;
      var labelData = data[j];
      singleFeatureImportance(labelName, labelData, i);
      i += 1;
    }
}

function singleFeatureImportance(labelName, labelData, i) {

    var x = d3.scaleLinear()
        .range([margin.left, w + margin.left])
        .domain([0, maxValue]);

    if (i != 1){
      plotTop += margin.inner + plotHeight;
      plotBottom += margin.inner + plotHeight;
    }

    if (i == 1){
      // title
        svg.append("text")
          .attr("x", margin.left )
          .attr("y", plotTop - 60)
          .attr("class", "bigTitle")
          .text("Feature importance");
    }

    if (i == n){
      // xaxis
        svg.append("text")
          .attr("transform",
                "translate(" + (width/2) + " ," +
                               (plotBottom + 45) + ")")
          .attr("class", "axisTitle")
          .text("Drop-out loss");

        var xAxis = d3.axisBottom(x)
                    .ticks(5)
                    .tickSize(0);

        xAxis = svg.append("g")
          .attr("class", "xAxis")
          .attr("transform", "translate(0," + plotBottom + ")")
          .attr("class", "axisLabel")
          .call(xAxis)
          .call(g => g.select(".domain").remove());
    }

    var y = d3.scaleBand()
        .rangeRound([plotBottom, plotTop])
        .padding(0.33)
        .domain(labelData.map(function (d) {
             return d.label;
        }));

    // labelname
    svg.append("text")
        .attr("x", margin.left )
        .attr("y", plotTop - 15)
        .attr("class", "smallTitle")
        .text(labelName);

    // grid
    var xGrid = svg.append("g")
         .attr("class", "grid")
         .attr("transform", "translate(0," + plotBottom + ")")
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
        .attr("transform","translate(" + labelsMargin + ",0)")
        .call(yAxis)
        .call(g => g.select(".domain").remove());

    // bars
    var bars = svg.selectAll()
        .data(labelData)
        .enter()
        .append("g");

    bars.append("rect")
        .attr("class", labelName)
        .attr("fill", function (d, i) {
          return colors[m-i-1];
        })
        .attr("y", function (d) {
            return y(d.label);
        })
        .attr("height", y.bandwidth())
        .attr("x", x(0))
        .attr("width", function (d) {
            return x(d.dropout_loss)-x(0);
        });

    var tooltipString;

    // tooltip functions
    bars.on("mouseover", function(){
            tooltip.style("visibility", "visible");
        })
        .on("mousemove", function(d) {
            tooltip .html( tooltipHtml(labelName, d) )
                .style("left", (d3.event.pageX) + "px")
                .style("top", (d3.event.pageY - 28) + "px");
        })
        .on("mouseout", function(d) {
            tooltip.style("visibility", "hidden");
        });
}

function tooltipHtml(labelName, d){
    let sign;
    if (d.dropout_loss > d.full_model) sign = "+"; else sign = "";
    var temp ="Model: " + d.label
      + "</br>" +
      "Model loss after feature " + labelName
      + "</br>" +
      " is permuted: " +  Math.round(d.dropout_loss * 1000)/1000
      + "</br>" +
      "Drop-out loss change: "  + sign + Math.round((d.dropout_loss - d.full_model) * 1000)/1000 ;
    return temp;
}
