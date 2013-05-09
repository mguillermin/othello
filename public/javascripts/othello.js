var othello = (function(){
    var my = {};
    var w = 400;
    var h = 400;
    var radius = 20;
    var possibleRadius = 5;
    var svg;
    var onPlayCallback;

    /**
     * Complete a dataset with entries for all 8x8 places
     * If a place is not set in the dataset a "None" value will be inserted
     * @param dataset
     * @returns {Array}
     */
    var completeDataset = function(dataset, possibleMoves) {
        var extractValue = function(i, j, dataset, possibleMoves) {
            for (var idx in dataset) {
                if (dataset[idx][0] == i && dataset[idx][1] == j) {
                    return dataset[idx][2];
                }
            }
            for (var idx in possibleMoves) {
                if (possibleMoves[idx][0] == i && possibleMoves[idx][1] == j) {
                    return "Possible";
                }
            }
            return "None";
        }
        var result = [];
        for (var i = 0; i <= 7; i++) {
            for (var j = 0; j <= 7; j++) {
                result.push([i,j,extractValue(i,j,dataset, possibleMoves)]);
            }
        }
        return result;
    }

    var xScale = d3.scale.linear()
        .domain([0,8])
        .range([0, w]);
    var yScale = d3.scale.linear()
        .domain([0,8])
        .range([0, h]);

    var init = function(dataset, possibleMoves, playCb) {
        onPlayCallback = playCb;
        svg = d3.select("body").append("svg")
            .attr("class", "board")
            .attr("width", w)
            .attr("height", h);
        for (var i = 0; i <= 8; i++) {
            svg.append("line").attr("x1", xScale(i)).attr("x2", xScale(i))
                .attr("y1", yScale(0)).attr("y2", yScale(8))
                .attr("class", "grid");
            svg.append("line").attr("x1", xScale(0)).attr("x2", xScale(8))
                .attr("y1", yScale(i)).attr("y2", yScale(i))
                .attr("class", "grid");
        }
        updateData(dataset, possibleMoves);
    }

    var updateData = function(dataset, possibleMoves) {
        var circles = svg.selectAll("circle")
            .data(completeDataset(dataset, possibleMoves), function(d) { return d[0] + "_" + d[1]; });

        circles.enter()
            .append("circle")
            .attr("cx", function(d) {
                // middle of the grid square
                return (xScale(d[1]) + xScale(d[1] + 1)) / 2;
            })
            .attr("cy", function(d) {
                // middle of the grid square
                return (yScale(d[0]) + yScale(d[0] + 1)) / 2;
            })
            .attr("r", function(d) {
                if (d[2] == "Possible") {
                    return possibleRadius;
                } else {
                    return radius;
                }
            })
            .attr("class", function(d){
                return d[2].toLowerCase();
            })
            .on("click", function(d) {
                if (d[2] == "Possible") {
                    onPlayCallback(d);
                }
            });

        circles
            .attr("class", function(d){
                return d[2].toLowerCase();
            })
            .attr("r", function(d) {
                if (d[2] == "Possible") {
                    return possibleRadius;
                } else {
                    return radius;
                }
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