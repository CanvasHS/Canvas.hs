// Global variables
var topLayerIdx = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');

// Event handlers
function connectionDataRecieved(event) {

    // Clear screen
    layerList[topLayerIdx].destroyChildren();

    var dataObject = jQuery.parseJSON(event.data);

    var shape = parseShapeData(dataObject);

    // Draw on current layer
    layerList[topLayerIdx].add(shape);
    layerList[topLayerIdx].batchDraw();
}

function connectionError(error) {

    printDebugMessage("WebSocket Error " + error);
}

function parseShapeData(data) {

    var shape = shapeFromData(data);
    enableEventHandlers(shape, data);

    return shape;
}

/*
 * Sending Input events
 */
var control = false;
var shift = false;
var alt = false;
var superKey = false;

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

function keyEvent(event,key) {
    connection.send(JSON.stringify({
        "event":event,
        "data":{
            "key": key,
            "control": control,
            "alt": alt,
            "shift": shift,
            "super": superKey
        }
    }));
}

/*
 * Drawing figures
 */ 

function shapeFromData(message) {



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
            printDebugMessage("Drawing line (" + data.id + ") with points: " + data.points,0);
            break;
        case "polygon":
            shape = new Kinetic.Polygon(data);
            printDebugMessage("Drawing polygon (" + data.id + ")",0);
            break;
        case "circle":
            shape = new Kinetic.Circle(data);
            printDebugMessage("Drawing circle (" + data.id + ") with x:"+data.x+" y:"+data.y+" and radius:"+data.radius,0);
            break;
        case "rect":
            shape = new Kinetic.Rect(data);
            printDebugMessage("Drawing rectangle (" + data.id + ") with x:"+data.x+" y:"+data.y+" width:"+data.width+" height:"+data.height,0);
            break;
        case "text":
            shape = new Kinetic.Text(data);
            printDebugMessage("Drawing text (" + data.id + ") with x:"+data.x+" y:"+data.y+" text:"+data.text,0);
            break;
        case "container":

            data.clip = [0, 0, data.width, data.height];
            shape = new Kinetic.Group(data);
            message.children.forEach(function(child) {
                shape.add(parseShapeData(child));
            });

            printDebugMessage("Drawing container (" + data.id + ")",0);
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

    topLayerIdx++;
    layerList[topLayerIdx] = new Kinetic.Layer();
    stage.add(layerList[topLayerIdx]);
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
        printDebugMessage("Connection opened",0);
    };
    // Log errors
    connection.onerror = connectionError;
    // Callback for recieving data
    connection.onmessage = connectionDataRecieved;

    // Begin to listen for keys
    window.addEventListener('keydown', function(e) {
        e.preventDefault();
        if (e.keyCode === 16) {
            shift = true;
        } else if (e.keyCode === 17) {
            control = true;
        } else if (e.keyCode === 18) {
            alt = true;
        } else if (e.keyCode === 91) {
            superKey = true;
        } else {
            var key = String.fromCharCode(e.keyCode);
            keyEvent("keydown", key);
//            console.log("Keydown: " + key + "\r\n shift: " + shift
//                    + "\r\n control: " + control + "\r\n alt: " + alt + "\r\n super: " + superKey);
        }
    });
    
    window.addEventListener('keyup', function(e) {
        e.preventDefault();
        if (e.keyCode === 16) {
            shift = false;
        } else if (e.keyCode === 17) {
            control = false;
        } else if (e.keyCode === 18) {
            alt = false;
        } else if (e.keyCode === 91) {
            superKey = false;
        } else {
            var key = String.fromCharCode(e.keyCode);
            keyEvent("keyup", key);
//            console.log("Keyup: " + key);
        }
    });
});
