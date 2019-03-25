var minValue = options.xmin;
var maxValue = options.xmax;
var n = options.n;
var m = options.m;
var ticksMargin = options.ticksMargin;

var plotHeight;
var margin = {top: 98, right: 30, bottom: 71, left: 80, inner: 42};
var labelsMargin = margin.left - 8;
var ticksMargin;
var w = width - margin.left - margin.right;
var h = height - margin.top- margin.bottom;
var labelsMargin = margin.left - 6;
var plotTop = margin.top;

if (options.scaleHeight === true) {
  plotHeight = (h-(n-1)*margin.inner)/n;
} else {
  plotHeight = 2*10 + m*12 + (m-1)*6;
}

var plotBottom = margin.top + plotHeight;

featureImportance(data);

// css
var tooltip = d3.select("body").append("div")
          .attr("class", "tooltip")
          .style("position", "absolute")
          .style("text-align", "center")
          .style("width", "100px")
          .style("height", "20px")
          .style("padding", "2px")
          .style("color", "white")
          .style("background", "#371ea3")
          .style("opacity", "0.5")
          .style("border", "0px")
          .style("border-radius", "8px")
          .style("pointer-events", "none")
          .style("visibility", "hidden");

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

    var colors = getColors(m, "bar");

    var x = d3.scaleLinear()
        .range([margin.left, w + margin.left])
        .domain([0, maxValue + ticksMargin]);

    if (i != 1){
      plotTop += margin.inner + plotHeight;
      plotBottom += margin.inner + plotHeight;
    }

    if (i == 1){
      // title
        svg.append("text")
          .attr("x", margin.left )
          .attr("y", 42)
          .attr("class", "bigTitle")
          .text("Feature importance");
    }

    if (i == n){
      // xaxis
        svg.append("text")
          .attr("transform",
                "translate(" + (w/2) + " ," +
                               (margin.top + n*(margin.inner+plotHeight)) + ")")
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
        .padding(0.35)
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

    var yGrid = svg.append("g")
         .attr("class", "grid")
         .attr("transform", "translate(" + margin.left + ",0)")
         .call(d3.axisLeft(y)
                .tickSize(-w)
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

    // tooltip functions
    bars.on("mouseover", function(){
            tooltip.style("visibility", "visible");
        })
        .on("mousemove", function(d) {
            tooltip .html( Math.round(d.dropout_loss * 100)/100)
                .style("left", (d3.event.pageX) + "px")
                .style("top", (d3.event.pageY - 28) + "px");
        })
        .on("mouseout", function(d) {
            tooltip.style("visibility", "hidden");
        });
}

