var othello = (function(){
    var my = {};
    var w = 400;
    var h = 400;
    var padding = 10;
    var radius = 20;
    var svg;
    var onPlayCb;

    /**
     * Complete a dataset with entries for all 8x8 places
     * If a place is not set in the dataset a "None" value will be inserted
     * @param dataset
     * @returns {Array}
     */
    var completeDataset = function(dataset) {
        var extractValue = function(i, j, dataset) {
            for (var item in dataset) {
                if (dataset[item][0] == i && dataset[item][1] == j) {
                    return dataset[item][2];
                }
            }
            return "None";
        }
        var result = [];
        for (var i = 0; i <= 7; i++) {
            for (var j = 0; j <= 7; j++) {
                result.push([i,j,extractValue(i,j,dataset)]);
            }
        }
        return result;
    }

    var xScale = d3.scale.linear()
        .domain([0,8])
        .range([padding, w]);
    var yScale = d3.scale.linear()
        .domain([0,8])
        .range([padding, h]);

    var init = function(dataset, playCb) {
        onPlayCb = playCb;
        svg = d3.select("body").append("svg")
            .attr("class", "board")
            .attr("width", w)
            .attr("height", h);
        updateData(dataset);
    }

    var updateData = function(dataset) {
        var circles = svg.selectAll("circle")
            .data(completeDataset(dataset), function(d) { return d[0] + "_" + d[1]; });

        circles.enter()
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
            })
            .on("click", function(d) {
                if (d[2] == "None") {
                    console.log(d);
                    onPlayCb(d);
                }
            });

        circles
            .attr("class", function(d){
                return d[2].toLowerCase();
            });

        circles.exit().remove();
    }


    my.w = w;
    my.h = h;
    my.svg = svg;
    my.init = init;
    my.updateData = updateData;

    return my;
}());