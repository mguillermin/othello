var othello = function(){
    var w = 400;
    var h = 400;
    var padding = 10;
    var radius = 20;

    var dataset = [
        [3,3,"White"],
        [3,4,"Black"],
        [4,3,"Black"],
        [4,4,"White"]
    ]

    var xScale = d3.scale.linear()
        .domain([0,8])
        .range([padding, w]);
    var yScale = d3.scale.linear()
        .domain([0,8])
        .range([padding, h]);

    var svg = d3.select("body").append("svg")
        .attr("class", "board")
        .attr("width", w)
        .attr("height", h);

    svg.selectAll("circle")
        .data(dataset)
        .enter()
        .append("circle")
        .attr("cx", function(d) {
            return xScale(d[1]) + radius;
        })
        .attr("cy", function(d) {
            return yScale(d[0]) + radius;
        })
        .attr("r", radius)
        .attr("class", function(d){
            return d[2].toLowerCase();
        });
}