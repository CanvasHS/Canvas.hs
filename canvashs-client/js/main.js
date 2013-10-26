// Global variables
var currentLayer = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');
var open = false;

function parseFigureMessage(message) {
    layerList[currentLayer] = new Kinetic.Layer();
    var figure = makeFigure(message);

    var now = new Date(),
        now = now.getHours()+':'+now.getMinutes()+':'+now.getSeconds();
    $("#debug").prepend("<p><strong>["+now+"]</strong> Drawing "+message.type+"</p>")

    layerList[currentLayer].add(figure);
    stage.add(layerList[currentLayer]);

    // Click event used for debugging is added below
    layerList[currentLayer].on('click', function(event) {
        window.alert("Clicked on " + event.targetNode.getClassName() + " on layer " + layerList.indexOf(event.targetNode.getLayer()));
    });
    currentLayer++;
}

function makeFigure(message) {
    var figure;
    switch (message.type) {
        case "line":
            figure = drawLine(message.data);
            break;
        case "polygon":
            figure = drawPolygon(message.data);
            break;
        case "circle":
            figure = drawCircle(message.data);
            break;
        case "rect":
            figure = drawRect(message.data);
            break;
        case "text":
            figure = drawText(message.data);
            break;
        case "container":
            figure = drawGroup(message.data);
            message.children.forEach(function(child) {
                figure.add(makeFigure(child));
            });
            break;
        default:
            window.alert("Unrecognized JSON message received from server.");
            figure = null;
    }
<<<<<<< HEAD
    layerList[currentLayer].add(figure);
    stage.add(layerList[currentLayer]);
    layerList[currentLayer].on('click', function(event) {
        window.alert("Clicked on " + event.targetNode.getClassName() + " on layer " + layerList.indexOf(event.targetNode.getLayer()) + "\nSend to server as generic click (see console)");
        /*msg = '{"event":"mouseclick", "data":["id":"TESTID", "x":10, "y":10]}';
        TEMPsendMessageToServer(msg);*/
    });
    currentLayer++;
=======
    return figure;
>>>>>>> origin/dev_martijn
}

/*
* TEMPORARY FUNCTION FOR DEBUG PURPOSES 
* ONLY USE WHILE PROPER FUNCTION IS NOT YET IN PLACE
*/
function TEMPsendMessageToServer(msg){
    /*console.log("Sending message to server using TEMP function:");
    console.log(msg);
    connection.send(msg);*/
}


function drawLine(data) {
    return new Kinetic.Line(data);
}
function drawPolygon(data) {
    return new Kinetic.Polygon(data);
}
function drawCircle(data) {
    return new Kinetic.Circle(data);
}
function drawRect(data) {
    return new Kinetic.Rect(data);
}
function drawText(data) {
    return new Kinetic.Text(data);
}
function drawGroup(data) {
    return new Kinetic.Group(data);
}


$(document).ready(function() {

    var width = 900; // defined here because the container also needs these proportions 
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
        width: 900,
        height: 600
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
        console.log("received raw data:");
        console.log(e.data);
        parseServerMessage(jQuery.parseJSON(e.data));
    };

    window.setInterval(function(){
        connection.send("");
    }, 2000);

//    for(var n = 0; n < message.objects.length; n++) {
//        parseFigureMessage(message.objects[n]);
//    }
});

