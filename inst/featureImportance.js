var minValue = options.xmin;
var maxValue = options.xmax;
var n = options.n;
var m = options.m;
var ticksMargin = options.ticksMargin;

var plotHeight;
var margin = {top: 98, right: 30, bottom: 71, left: 80, inner: 42};
var ticksMargin;
var w = width - margin.left - margin.right;
var h = height - margin.top- margin.bottom;
var labelsMargin = margin.left - 8;
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
          .style("height", "38px")
          .style("padding", "2px")
          .style("color", "white")
          .style("background", "#371ea3")
          .style("opacity", "0.5")
          .style("border", "0px")
          .style("border-radius", "8px")
          .style("pointer-events", "none")
          .style("visibility", "hidden");

svg.selectAll(".fullModel").style("font-weight", "700");

// plot function
function featureImportance(data) {
    var i = 1;

    for (var j in data) {
      var modelName = j;
      var modelData = data[j];
      singleFeatureImportance(modelName, modelData, i);
      i += 1;
    }
}

function singleFeatureImportance(modelName, modelData, i) {

    var colors = getColors(n, "bar");

    var x = d3.scaleLinear()
        .range([margin.left, w + margin.left])
        .domain([minValue - ticksMargin, maxValue + ticksMargin]);

    if (i != 1){
      plotTop += margin.inner + plotHeight;
      plotBottom += margin.inner + plotHeight;
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
          .attr("class", "axisLabel")
          .attr("transform", "translate(0," + plotBottom + ")")
          .call(xAxis)
          .call(g => g.select(".domain").remove());
    }

    var y = d3.scaleBand()
        .rangeRound([plotBottom, plotTop])
        .padding(0.35)
        .domain(modelData.map(function (d) {
             return d.variable;
        }));

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
        .attr("transform","translate(" + (yGridStart-8) + ",0)")
        .call(yAxis)
        .call(g => g.select(".domain").remove());

    // tag full model to later make it bold
    yAxis.selectAll("text").each(function() {
        if(this.textContent === "_full_model_") {
          this.classList.add("fullModel");
        }
      });

    // modelname
    svg.append("text")
        .attr("x", yGridStart)
        .attr("y", plotTop - 15)
        .attr("class", "smallTitle")
        .text(modelName);

    if (i == 1){
      // title
        svg.append("text")
          .attr("x", yGridStart)
          .attr("y", 42)
          .attr("class", "bigTitle")
          .text("Feature importance");
    }

    // bars
    var bars = svg.selectAll()
        .data(modelData)
        .enter()
        .append("g");

    // find full model dropout_loss value
    var fullModel;
    modelData.forEach((item) => {
      if(item.variable==="_full_model_") fullModel = item.dropout_loss;
      });

    bars.append("rect")
        .attr("class", modelName)
        .attr("fill", colors[i-1])
        .attr("y", function (d) {
            return y(d.variable);
        })
        .attr("height", y.bandwidth())
        .attr("x", function (d) {
          // start ploting the bar left to full model line
          if (x(d.dropout_loss) < x(fullModel)) {
            return x(d.dropout_loss);
          } else {
            return x(fullModel);
          }
        })
        .attr("width", function (d) {
            return  Math.abs(x(d.dropout_loss) - x(fullModel));
        });

    // tooltip functions
    bars.on("mouseover", function(){
            tooltip.style("visibility", "visible");
        })
        .on("mousemove", function(d) {
          if (d.dropout_loss > fullModel) {
             tooltip .html(
                Math.round(d.dropout_loss * 100)/100
                + "</br>" + "+"
                + Math.round((d.dropout_loss-fullModel) * 100)/100
              )
                .style("left", (d3.event.pageX) + "px")
                .style("top", (d3.event.pageY - 28) + "px");
          } else {
             tooltip .html(
              Math.round(d.dropout_loss * 100)/100
              + "</br>"
              + Math.round((d.dropout_loss-fullModel) * 100)/100
            )
                .style("left", (d3.event.pageX) + "px")
                .style("top", (d3.event.pageY - 28) + "px");
          }
        })
        .on("mouseout", function(d) {
            tooltip.style("visibility", "hidden");
        });

    // make line next to bars
    var minimumY = Number.MAX_VALUE;
    var maximumY = Number.MIN_VALUE;
    bars.selectAll(".".concat(modelName)).each(function() {
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
}
