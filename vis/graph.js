var updater;
var time = 0;
var backwards = false;
var speed = 1;
$("#start").click(function () {
    speed = 1;
    if (updater == undefined) {
        updater = setInterval(function () {
            backwards = false;
            $("#time").text(++time);
            //nodes[0].mode = "leader";
            nodes[0].timeout = ((nodes[0].timeout + 5) % 100);
            //links[0].message.progress = ((0.1 + parseFloat(links[0].message.progress)) % 1).toFixed(2);
            refresh();

            //force.start();

        }, 1000);
    }
});
$("#pause").click(function () {
    clearInterval(updater);
    updater = null;
});
$("#back").click(function () {
    backwards = true;
    $("#time").text(--time);
    nodes[0].timeout = ((nodes[0].timeout - 5) % 100);
    refresh();
    //links[0].message.progress = ((parseFloat(links[0].message.progress) - 0.1) % 1).toFixed(2);
    //force.start();

});
$("#forward").click(function () {
    backwards = false;
    $("#time").text(++time);
    nodes[0].timeout = ((nodes[0].timeout + 5) % 100);
    refresh();
    //links[0].message.progress = ((0.1 + parseFloat(links[0].message.progress)) % 1).toFixed(2);
    //force.start();
});

$("#fastforward").click(function () {
    clearInterval(updater);
    speed++;
    updater = setInterval(function () {
        backwards = false;
        $("#time").text(++time);
        //nodes[0].mode = "leader";
        nodes[0].timeout = ((nodes[0].timeout + 5) % 100);
        //links[0].message.progress = ((0.1 + parseFloat(links[0].message.progress)) % 1).toFixed(2);
        refresh();

        //force.start();

    }, 1000 / speed);
    //links[0].message.progress = ((0.1 + parseFloat(links[0].message.progress)) % 1).toFixed(2);
    //force.start();
});



var timeoutArc = d3.svg.arc()
    .innerRadius(10)
    .outerRadius(12)
    .startAngle(0)
    .endAngle(function (d) {
        return (d.timeout * 2 * Math.PI / 100); //convert percentage into radians
    });


var numNodes;
var messages;
//message format {source:, dest:, sent:, arrives:,info:,RPC:}
var nodeModeChanges;
//{node:, newMode:,newTerm:,newInfo:,time:}
var timeouts;
//{node:,start:,expires:,type:}
var links = [];
var nodes = [];
var width = 1000,
    height = 600;
var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);
var force, link, node, timer;
d3.json("data.json", function (error, json) {
    if (error) { return console.warn(error); }
    numNodes = json.numNodes;
    messages = json.messages;
    nodeModeChanges = json.nodeModeChanges;
    timeouts = json.timeouts;



    // send requestVotes to all other nodes & send response
    /*    d3.range(0, numNodes).forEach(function (node) {
        if (node != 2) {
            var arrival = 7 + Math.floor(Math.random() * 4)
            messages.push({
                source: 2,
                dest: node,
                sent: 6,
                arrives: arrival,
                info: "requestVote for node 2<br>term:3",
                RPC: "requestVote"
            });
            messages.push({
                source: node,
                dest: 2,
                sent: arrival + 1,
                arrives: arrival + 2 + Math.floor(Math.random() * 4),
                info: "requestVote reply for node 2<br>term:3<br>source:" + node,
                RPC: "requestVote"
            });
        }
    });*/

//create fully connected graph
    d3.range(0, numNodes).forEach(
        function (node1) {
            d3.range(0, numNodes).forEach(
                function (node2) {
                    if (node1 != node2) {
                        links.push({
                            source: node1,
                            target: node2
                        });
                    }
                });

        });

//initialise each node
    d3.range(0, numNodes).forEach(function (d) {
        nodes.push({
            name: d,
            timeout: 50,
            term: 0,
            info: "some information about\nthis node"
        });
    });


    force = d3.layout.force()
        .nodes(d3.values(nodes))
        .links(links)
        .size([width, height])
        .linkDistance(300)
        .charge(-300)
        .friction(0.1)
        .on("tick", tick)
        .start();


    link = svg.selectAll(".link")
        .data(force.links())
        .enter().append("line")
        .attr("class", "link");

    node = svg.selectAll(".node")
        .data(force.nodes())
        .enter().append("g")
        .attr("class", "node")
        .on("mouseover", mouseover)
        .on("mouseout", mouseout)
        .call(force.drag);

    insertNewMessages();

    node.append("circle")
        .attr("r", 8)
        .attr("class", function (d) {
            return d.mode;
        })
        .append("svg:title")
        .text(function (d) {
            return d.info + "\nterm:" + d.term;
        });

    timer = node.append("path")
        .attr("d", timeoutArc);

    node.append("text")
        .attr("x", 12)
        .attr("dy", ".35em")
        .text(function (d) {
            return d.name;
        });

    refresh();

});

function tick() {
    link
        .attr("x1", function (d) {
            return d.source.x;
        })
        .attr("y1", function (d) {
            return d.source.y;
        })
        .attr("x2", function (d) {
            return d.target.x;
        })
        .attr("y2", function (d) {
            return d.target.y;
        });
    //update packet position along path

    svg.selectAll(".packet")
        .data(messages.filter(function (d) {
            return ((d.sent <= time) && (d.arrives >= time));
        }))
        .attr("x", function (d) {
            return ((nodes[d.dest].x - nodes[d.source].x) * ((time - d.sent) / (d.arrives - d.sent))) + nodes[d.source].x;
        })
        .attr("y", function (d) {
            return ((nodes[d.dest].y - nodes[d.source].y) * ((time - d.sent) / (d.arrives - d.sent))) + nodes[d.source].y;
        });
    //removeOldMessages();

    //update timeout position
    timer.attr("d", timeoutArc);

    //reposition nodes
    node
        .attr("transform", function (d) {
            return "translate(" + d.x + "," + d.y + ")";
        });
    force.nodes(d3.values(nodes));


}

function mouseover() {
    d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", 16);

}

function mouseout() {
    d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", 8);

}


var tooltip = d3.select("body")
    .append("div")
    .style("position", "absolute")
    .style("z-index", "10")
    .style("visibility", "hidden")
    .attr("class", "tooltip")
    .html("a simple tooltip");

function showPacketInfo(d) {
    //alert("info");
    d3.select(this).transition().attr("width", 15).attr("height", 15);
    tooltip.style("visibility", "visible");
    tooltip.style("top", (event.pageY - 10) + "px").style("left", (event.pageX + 10) + "px");
    tooltip.html(d.info);
}

function movePacketInfo() {
    tooltip.style("top", (event.pageY - 10) + "px").style("left", (event.pageX + 10) + "px");
}

function hidePacketInfo() {
    d3.select(this).transition().attr("width", 5).attr("height", 5);
    tooltip.style("visibility", "hidden");
}


function insertNewMessages() {
    svg.selectAll(".packet")
        .data(messages.filter(function (d) {
            return ((d.sent <= time) && (d.arrives >= time));
        }))
        .enter().append("rect")
        .attr("x", function (d) {
            return ((nodes[d.dest].x - nodes[d.source].x) * ((time - d.sent) / (d.arrives - d.sent))) + nodes[d.source].x;
        })
        .attr("y", function (d) {
            return ((nodes[d.dest].y - nodes[d.source].y) * ((time - d.sent) / (d.arrives - d.sent))) + nodes[d.source].y;
        })
    /*        .attr("dy", ".35em")
        .attr("dx", ".35em")
        .text(function (d) {
            return ((time - d.sent) / (d.arrives - d.sent));
        })*/
        .attr("width", 5)
        .attr("height", 5)
        .attr("class", function (d) {
            return "packet " + d.RPC;
        })
        .on("mouseover", showPacketInfo)
        .on("mouseout", hidePacketInfo)
        .on("mousemove", movePacketInfo);

}

function calcPreviousX(d) {
    var prevTime;
    if (backwards) {
        prevTime = time + 1;
    } else {
        prevTime = time - 1;
    }
    if (prevTime <= d.sent) {
        return nodes[d.source].x;
    }
    if (prevTime >= d.arrives) {
        return nodes[d.dest].x;
    }

    return ((nodes[d.dest].x - nodes[d.source].x) * ((prevTime - d.sent) / (d.arrives - d.sent))) + nodes[d.source].x;
}

function calcPreviousY(d) {
    var prevTime;
    if (backwards) {
        prevTime = time + 1;
    } else {
        prevTime = time - 1;
    }
    if (prevTime <= d.sent) {
        return nodes[d.source].y;
    }
    if (prevTime >= d.arrives) {
        return nodes[d.dest].y;
    }

    return ((nodes[d.dest].y - nodes[d.source].y) * ((prevTime - d.sent) / (d.arrives - d.sent))) + nodes[d.source].y;
}

function updateMessages() {
    svg.selectAll(".packet")
        .data(messages.filter(function (d) {
            return ((d.sent <= time) && (d.arrives >= time));
        }))
        //set to old position then transisition to new
        .attr("x", calcPreviousX)
        .attr("y", calcPreviousY)
        .transition()
        .attr("x", function (d) {
            return ((nodes[d.dest].x - nodes[d.source].x) * ((time - d.sent) / (d.arrives - d.sent))) + nodes[d.source].x;
        })
        .attr("y", function (d) {
            return ((nodes[d.dest].y - nodes[d.source].y) * ((time - d.sent) / (d.arrives - d.sent))) + nodes[d.source].y;
        })
        .attr("class", function (d) {
            return "packet " + d.RPC;
        });

}

function removeOldMessages() {
    svg.selectAll(".packet").data(messages).exit().remove();

    svg.selectAll(".packet")
        .data(messages.filter(function (d) {
            return ((d.sent <= time) && (d.arrives >= time));
        })).exit().remove();
}

function updateNodeMode() {
    nodeModeChanges.forEach(function (d) {
        if (d.time == time) {
            if (d.newMode) {
                nodes[d.node].mode = d.newMode;
            }
            if (d.newTerm) {
                nodes[d.node].term = d.newTerm;
            }
            if (d.newInfo) {
                nodes[d.node].info = d.newInfo;
            }

        }
    });
    node.selectAll("circle").attr("class", function (d) {
        return d.mode;
    })
        .select("title")
        .text(function (d) {
            return d.info + "\nterm:" + d.term;
        });
}

function updateTimeouts() {
    timeouts.forEach(function (d) {
        if (d.start <= time && d.expires >= time) {
            nodes[d.node].timeout = ((time - d.start) / (d.expires - d.start) * 100);
        }
    });

}

function refresh() {
    updateTimeouts();
    timer.transition().attr("d", timeoutArc);
    removeOldMessages();
    insertNewMessages();
    updateMessages();
    updateNodeMode();
}



var legendText = ["Leader", "Candidate", "Follower", "AppendEntries", "RequestVote"];

// add legend   
var legend = svg.append("g")
    .attr("class", "legend")
//.attr("x", w - 65)
//.attr("y", 50)
    .attr("height", 100)
    .attr("width", 100)
    .attr('transform', 'translate(-20,50)');

//Follower
legend.append("circle")
    .attr("r", 8)
    .attr("transform", "translate(900,20)")
    .attr("class", "follower");

legend.append("text")
    .attr("x", 910)
    .attr("y", 23)
    .text("Follower");

//Leader
legend.append("circle")
    .attr("r", 8)
    .attr("transform", "translate(900,40)")
    .attr("class", "leader");

legend.append("text")
    .attr("x", 910)
    .attr("y", 43)
    .text("Leader");

//Candidate
legend.append("circle")
    .attr("r", 8)
    .attr("transform", "translate(900,60)")
    .attr("class", "candidate");

legend.append("text")
    .attr("x", 910)
    .attr("y", 63)
    .text("Candidate");

// AppendEntries
legend.append("rect")
    .attr("x", 900)
    .attr("y", 80)
    .attr("width", 5)
    .attr("height", 5)
    .attr("class", "appendEntries");

legend.append("text")
    .attr("x", 910)
    .attr("y", 83)
    .text("appendEntries");

// RequestVote
legend.append("rect")
    .attr("x", 900)
    .attr("y", 100)
    .attr("width", 5)
    .attr("height", 5)
    .attr("class", "requestVote");

legend.append("text")
    .attr("x", 910)
    .attr("y", 103)
    .text("requestVote");