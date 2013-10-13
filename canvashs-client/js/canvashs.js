// Global variables
var currentLayer = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');
var open = false;

function parseServerMessage(message) {
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
            figure = drawCircle();
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

function drawCircle() {
    figure = new Kinetic.Circle({
        radius: 100,
        fill: '#FFD200',
        stroke: 'black',
        strokeWidth: 1,
        x: 300,
        y: 400
    });
    return figure;
}


$(document).ready(function() {

    stage = new Kinetic.Stage({
        container: 'container',
        width: 900,
        height: 600
    });

    var layer = new Kinetic.Layer();
    var rectangle = new Kinetic.Rect({
        x: 0,
        y: 0,
        width: stage.getWidth(),
        height: stage.getHeight(),
        fill: 'lightgrey',
        stroke: 'grey',
        strokeWidth: 2
    });

    // add the shape to the layer
    layer.add(rectangle);
    // add the layer to the stage
    stage.add(layer);




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

});
