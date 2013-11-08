// Global variables
var currentLayer = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');
var open = false;

// Event handlers
function parseMessage(event) {
    console.log("received raw data:");
    console.log(event.data);
    layerList[currentLayer].destroyChildren();
    placeFigure(parseFigureMessage(jQuery.parseJSON(event.data)));
    layerList[currentLayer].batchDraw();
}
function connectionError(error) {
    console.log('WebSocket Error ');
    console.log(error);
}
function parseFigureMessage(message) {
    var figure = makeFigure(message);
    console.log(message);
    parseEventData(figure, message);
    return figure;
}
function newDefaultLayer() {
    currentLayer++;
    layerList[currentLayer] = new Kinetic.Layer();
    stage.add(layerList[currentLayer]);
}
function parseEventData(figure, message) {   
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
function placeFigure(figure) {
    layerList[currentLayer].add(figure);
    debugMessage("Drawing "+figure.className);
}
function makeFigure(message) {
    var figure;

    data = message.data;

    // both fill and stroke are in the form {"r": 255, "g": 255, "b": 255, ("a": 1.0)?}
    if(data["fill"]){
        data["fill"] = rgbaDictToColor(data["fill"]);
    }

    if(data["stroke"]){
        data["stroke"] = rgbaDictToColor(data["stroke"]);
    }

    switch (message.type) {
        case "line":
            figure = drawLine(data);
            break;
        case "polygon":
            figure = drawPolygon(data);
            break;
        case "circle":
            figure = drawCircle(data);
            break;
        case "rect":
            figure = drawRect(data);
            break;
        case "text":
            figure = drawText(data);
            break;
        case "container":
            figure = drawGroup(data);
            message.children.forEach(function(child) {
                figure.add(parseFigureMessage(child));
            });
            break;
        default:
            window.alert("Unrecognized JSON message received from server.");
            figure = null;
    }
    return figure;
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
    data.clip = [0, 0, data.width, data.height];
    return new Kinetic.Group(data);
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
function debugMessage(message) {

    var now = new Date(),
        now = now.getHours()+':'+now.getMinutes()+':'+now.getSeconds();
    $("#debug").prepend("<p><strong>["+now+"]</strong> "+message+"</p>")
}
function initCanvas(canvas, width, height) {

    canvas.css( "width", width+"px" );
    canvas.css( "height", height+"px" );
    canvas.css( "margin-top", "-"+height/2+"px" );
    canvas.css( "margin-left", "-"+(width + $( "#debug" ).width())/2+"px" );
    stage = new Kinetic.Stage({
        container: 'canvas',
        width: width,
        height: height
    });
    // Create new layer to draw on
    newDefaultLayer();
}

var control = false;
var shift = false;
var alt = false;
var superKey = false;
        
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

$(document).ready(function() {

    var width = 900; // defined here because the container also needs these proportions 
    var height = 600;

    // Init canvas
    initCanvas($('#canvas'), width, height);

    $( "#wrapper" ).css( "min-width", width+"px" );
    $( "#wrapper" ).css( "height", $( window ).height()+"px" );

    $( window ).resize(function() { 
        $( "#wrapper" ).css( "height", $( window ).height()+"px" );
    });

    // When the connection is open, send some data to the server
    connection.onopen = function () {
    };

    // Log errors
    connection.onerror = connectionError;

    // Log messages from the server
    connection.onmessage = parseMessage;

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
