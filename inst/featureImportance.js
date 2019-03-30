var minValue = options.xmin;
var maxValue = options.xmax;
var n = options.n;
var m = options.m;
var barWidth = options.barWidth;

var margin = {top: 98, right: 30, bottom: 71, left: 120, inner: 42};
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

var colors = getColors(n, "bar");

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
      var modelName = j;
      var modelData = data[j];
      singleFeatureImportance(modelName, modelData, i);
      i += 1;
    }
}

function singleFeatureImportance(modelName, modelData, i) {

    var x = d3.scaleLinear()
        .range([margin.left, w + margin.left])
        .domain([minValue, maxValue]);

    if (i != 1){
      plotTop += margin.inner + plotHeight;
      plotBottom += margin.inner + plotHeight;
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
          .attr("class", "axisLabel")
          .attr("transform", "translate(0," + plotBottom + ")")
          .call(xAxis)
          .call(g => g.select(".domain").remove());
    }

    var y = d3.scaleBand()
        .rangeRound([plotBottom, plotTop])
        .padding(0.33)
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
          .attr("y", plotTop - 60)
          .attr("class", "bigTitle")
          .text("Feature importance");
    }

    // bars
    var bars = svg.selectAll()
        .data(modelData)
        .enter()
        .append("g");

    // find full model dropout_loss value
    var fullModel = modelData[0].full_model;

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
             tooltip .html( tooltipHtml(modelName,d,"+") )
                .style("left", (d3.event.pageX) + "px")
                .style("top", (d3.event.pageY - 28) + "px");
          } else {
             tooltip .html(tooltipHtml(modelName,d,""))
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

function tooltipHtml(modelName, d, sign){
    var temp ="Model: " + modelName
      + "</br>" +
      "Model loss after feature " + d.variable
      + "</br>" +
      " is permuted: " +  Math.round(d.dropout_loss * 1000)/1000
      + "</br>" +
      "Drop-out loss change: "  + sign + Math.round((d.dropout_loss - d.full_model) * 1000)/1000 ;
    return temp;
}
