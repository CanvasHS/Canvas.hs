// Global variables
var currentLayer = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');
var open = false;

// Event handlers
function connectionDataRecieved(event) {

    // Clear screen
    layerList[currentLayer].destroyChildren();

    var dataObject = jQuery.parseJSON(event.data);

    var shape = parseShapeData(dataObject);

    // Draw on current layer
    layerList[currentLayer].add(shape);
    layerList[currentLayer].batchDraw();
}

function connectionError(error) {

    printDebugMessage("WebSocket Error " + error);
}

function parseShapeData(data) {

    var shape = kineticShapeFromData(data);
    enableEventHandlers(shape, data);

    return shape;
}

/*
 * Sending Input events
 */

function enableEventHandlers(figure, message) {   
    if(message.eventData != undefined && message.eventData != null) {
        if(message.eventData.listen.indexOf("mouseclick") != -1) {
            figure.on('click', clickEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mousedown") != -1) {
            figure.on('mousedown', mouseDownEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mouseup") != -1) {
            figure.on('mouseup', mouseUpEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mouseover") != -1) {
            figure.on('mouseover', mouseOverEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mousemove") != -1) {
            figure.on('mousemove', mouseMoveEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mouseout") != -1) {
            figure.on('mouseout', mouseOutEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mousedrag") != -1) {
            figure.on('mousedrag', mouseDragEventHandler.bind(undefined, message.eventData.eventId));
        }
    }
}

function clickEventHandler(id, event) { mouseEvent("mouseclick", id, event); }
function mouseDownEventHandler(id, event) { mouseEvent("mousedown", id, event); }
function mouseUpEventHandler(id, event) { mouseEvent("mouseup", id, event); }
function mouseOverEventHandler(id, event) { mouseEvent("mouseover", id, event); }
function mouseOutEventHandler(id, event) { mouseEvent("mouseout", id, event); }
function mouseMoveEventHandler(id, event) { mouseEvent("mousemove", id, event); }
function mouseDragEventHandler(id, event) { mouseEvent("mousedrag", id, event); }
function mouseEvent(eventName, id, event) {
    // Compensate for the position of the canvas
    var canvasPos = $("#canvas").position();
    console.log(event);
    connection.send(JSON.stringify({
        "event":eventName,
        "data":{
            "id": id,
            "x": event.pageX-canvasPos.left+575,
            "y": event.pageY-canvasPos.top+300
        }
    }));
}

/*
 * Drawing figures
 */ 

function kineticShapeFromData(message) {

    var shape = null;
    var data = message.data;

    // both fill and stroke are in the form {"r": 255, "g": 255, "b": 255, ("a": 1.0)?}
    if(data["fill"]){
        data["fill"] = rgbaDictToColor(data["fill"]);
    }

    if(data["stroke"]){
        data["stroke"] = rgbaDictToColor(data["stroke"]);
    }

    switch (message.type) {
        case "line":
            shape = new Kinetic.Line(data);
            break;
        case "polygon":
            shape = new Kinetic.Polygon(data);
            break;
        case "circle":
            shape = new Kinetic.Circle(data);
            break;
        case "rect":
            shape = new Kinetic.Rect(data);
            break;
        case "text":
            shape = new Kinetic.Text(data);
            break;
        case "container":

            data.clip = [0, 0, data.width, data.height];
            shape = new Kinetic.Group(data);
            message.children.forEach(function(child) {
                shape.add(parseShapeData(child));
            });
            break;
        default:
            printDebugMessage("Unrecognized JSON message received from server.",2);
    }
    return shape;
}

function rgbaDictToColor(dict){
    var res = "";
    if(dict["a"]){
        res = "rgba({0},{1},{2},{3})".format(dict["r"], dict["g"], dict["b"], dict["a"]);
    } else {
        res = "rgb({0},{1},{2})".format(dict["r"], dict["g"], dict["b"]);
    }
    return res;
}

/*
 * Visible debugger
 * 
 * Type 0 = message
 * Type 1 = warning
 * Type 2 = error
 */

function printDebugMessage(message, type) {

    if(type == 1)
    {
        console.warn(message);
    }
    else if(type == 2)
    {
        console.error(message);
    }

    var now = new Date(),
        now = now.getHours()+':'+now.getMinutes()+':'+now.getSeconds();
    $("#debug").prepend("<p><strong>["+now+"]</strong> "+message+"</p>")
}

/*
 * Canvast setup
 */

function initCanvas(width, height) {

    var canvas = $('#canvas');
    var wrapper = $('#wrapper');

    canvas.css( "width", width+"px" );
    canvas.css( "height", height+"px" );
    canvas.css( "margin-top", "-"+height/2+"px" );
    canvas.css( "margin-left", "-"+(width + $( "#debug" ).width())/2+"px" );

    wrapper.css( "min-width", width+"px" );
    wrapper.css( "height", $( window ).height()+"px" );

    // $( window ).resize(function() { 
    //     wrapper.css( "height", $( window ).height()+"px" );
    // });

    stage = new Kinetic.Stage({
        container: 'canvas',
        width: width, // Default width and height
        height: height // Can be changed with the resize canvas function
    });

    // Create new layer to draw on
    newDefaultLayer();
}


function newDefaultLayer() {
    currentLayer++;
    layerList[currentLayer] = new Kinetic.Layer();
    stage.add(layerList[currentLayer]);
}

function sendKeyEvent(eventName, event) {
    event.preventDefault();
    var key = String.fromCharCode(event.keyCode);
    connection.send(JSON.stringify({
        "event":eventName,
        "data":{
            "key": key,
            "control": event.ctrlKey || event.metaKey,
            "alt": event.altKey,
            "shift": event.shiftKey
        }
    }));
}

/*
 * On document ready
 */

$(document).ready(function() {

    var width = 900; // defined here because the container also needs these proportions 
    var height = 600;

    // Init canvas
    initCanvas(width,height);

    // When the connection is open, send some data to the server
    connection.onopen = function () {
    };

    // Log errors
    connection.onerror = connectionError;

    // Log messages from the server
    connection.onmessage = connectionDataRecieved;

    // Begin to listen for keys
    window.addEventListener('keydown', function(e) {
        sendKeyEvent("keydown", e);
    });
    
    window.addEventListener('keyup', function(e) {
        sendKeyEvent("keyup", e);
    });
});
