// Global variables
var currentLayer = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');
var open = false;

function parseServerMessage(message) {

    var now = new Date(),
        now = now.getHours()+':'+now.getMinutes()+':'+now.getSeconds();
    $("#debug").prepend("<p><strong>["+now+"]</strong> Drawing "+message.type+"</p>")

    layerList[currentLayer] = new Kinetic.Layer();
    var figure;

    switch (message.type) {
        case "line":
            figure = drawLine(message.start, message.end);
            break;
        case "polygon":
            figure = drawPolygon();
            break;
        case "circle":
            figure = drawCircle(message.data.x, message.data.y, message.data.radius);
            break;
        default:
            window.alert("Unrecognized JSON message received from server.");
            figure = null;
    }
    layerList[currentLayer].add(figure);
    stage.add(layerList[currentLayer]);
    layerList[currentLayer].on('click', function(event) {
        window.alert("Clicked on " + event.targetNode.getClassName() + " on layer " + layerList.indexOf(event.targetNode.getLayer()));
    });
    currentLayer++;
}


function drawLine(begin, end) {
    figure = new Kinetic.Line({
        points: [begin, end],
        stroke: "blue",
        strokeWidth: 8
    });
    return figure;
}

function drawPolygon() {
    figure = new Kinetic.Polygon({
        points: [73, 192, 73, 160, 340, 23, 500, 109, 499, 139, 342, 93],
        fill: '#00D2FF',
        stroke: 'black',
        strokeWidth: 1
    });
    return figure;
}

function drawCircle(_x, _y, _radius) {
    figure = new Kinetic.Circle({
        radius: _radius,
        fill: '#FFD200',
        stroke: 'black',
        strokeWidth: 1,
        x: _x,
        y: _y
    });
    return figure;
}


$(document).ready(function() {

    var width = 900; // defined here because the contaier also needs these proportions 
    var height = 600;

    $( "#wrapper" ).css( "min-width", width+"px" );
    $( "#wrapper" ).css( "height", $( window ).height()+"px" );

    $( window ).resize(function() { 
        $( "#wrapper" ).css( "height", $( window ).height()+"px" );
    });

    $( "#canvas" ).css( "width", width+"px" );
    $( "#canvas" ).css( "height", height+"px" );
    $( "#canvas" ).css( "margin-top", "-"+height/2+"px" );
    $( "#canvas" ).css( "margin-left", "-"+(width + $( "#debug" ).width())/2+"px" );

    stage = new Kinetic.Stage({
        container: 'canvas',
        width: width,
        height: height
    });

    // When the connection is open, send some data to the server
    connection.onopen = function () {
    };

    // Log errors
    connection.onerror = function (error) {
      console.log('WebSocket Error ');
      console.log(error);
    };

    // Log messages from the server
    connection.onmessage = function (e) {
        console.log("test"+e.data);
        parseServerMessage(jQuery.parseJSON(e.data));
    };

    window.setInterval(function(){
        connection.send("");
    }, 2000);

});

