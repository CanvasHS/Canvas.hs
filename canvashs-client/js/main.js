// Global variables
var topLayerIdx = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');

var generadedShapeIdIdx = 0;
var debugTween;
var debugAnnimatingShapes = [];
var debugCanClose = true;

// Event handlers
function connectionDataReceived(event) {

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

function enableEventHandlers(shape, message) {   
    if(message.eventData != undefined && message.eventData != null) {
        if(message.eventData.listen.indexOf("mouseclick") != -1) {
            shape.on('click', clickEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mousedown") != -1) {
            shape.on('mousedown', mouseDownEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mouseup") != -1) {
            shape.on('mouseup', mouseUpEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mouseover") != -1) {
            shape.on('mouseover', mouseOverEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mousemove") != -1) {
            shape.on('mousemove', mouseMoveEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mouseout") != -1) {
            shape.on('mouseout', mouseOutEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mousedrag") != -1) {
            shape.on('mousedown', mouseDragStartEventHandler.bind(undefined, message.eventData.eventId));
            shape.on('mouseup', mouseDragEndEventHandler.bind(undefined, message.eventData.eventId));
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
function mouseDragStartEventHandler(id, event) {
    shape.on('mousemove', mouseDragEventHandler.bind(undefined, message.eventData.eventId));
}
function mouseDragEndEventHandler(id, event) {
    shape.off('mousemove', mouseDragEventHandler.bind(undefined, message.eventData.eventId));
}
function mouseEvent(eventName, id, event) {
    // Compensate for the position of the canvas
    var canvasPos = $("#canvas").position();
    var x = event.pageX-canvasPos.left+575; // Fix me
    var y = event.pageY-canvasPos.top+300; // Fix me

    printDebugMessage("Event "+eventName+" on <a data-sid=\"" + id + "\" class=\"debugSelector\">" + id + "</a> (x:"+x+" y:"+y+")",0);

    connection.send(JSON.stringify({
        "event":eventName,
        "data":{
            "id": id,
            "x": x, 
            "y": y
        }
    }));
}

function sendKeyEvent(eventName, event) {
    
    var key = normalizeKeyCode(event);
    var ctrl = event.ctrlKey || event.metaKey;
    var alt = event.altKey;
    var shift = event.shiftKey;

    printDebugMessage("KeyEvent "+key+" "+eventName+" (ctrl:"+ctrl+" alt:"+alt+" shift:"+shift+")",0);

    event.preventDefault(); // Prevent browser default behavior to block ctrl-c for example

    connection.send(JSON.stringify({
        "event":eventName,
        "data":{
            "key": key,
            "control": ctrl,
            "alt": alt,
            "shift": shift
        }
    }));
}

function sendScrollEvent(deltaX,deltaY) {

    printDebugMessage("ScrollEvent (deltaX:"+deltaX+" deltaY:"+deltaY+")",0);

    connection.send(JSON.stringify({
        "event":"scroll",
        "data":{
            "xdelta": deltaX,
            "ydelta": deltaY
        }
    }));
}

/*
 * Drawing shapes
 */ 

function shapeFromData(message) {

    var shape = null;
    var debugMessage = "";
    var data = message.data;

    // both fill and stroke are in the form {"r": 255, "g": 255, "b": 255, ("a": 1.0)?}
    if(data["fill"]){
        data["fill"] = rgbaDictToColor(data["fill"]);
    }

    if(data["stroke"]){
        data["stroke"] = rgbaDictToColor(data["stroke"]);
    }
    // Debug message
    if(!data["id"]) {
        data["id"] = "sid" + generadedShapeIdIdx;
        generadedShapeIdIdx++;
    }

    debugMessage = "Drawing <a data-sid=\"" + data["id"] + "\" class=\"debugSelector\">" + message.type + " (" + data["id"] + ")</a> ";

    // Init shape based on type
    switch (message.type) {
        case "line":
            shape = new Kinetic.Line(data);
            debugMessage += "with points: " + data.points;
            break;
        case "polygon":
            data.draggable = true;
            shape = new Kinetic.Polygon(data);
            shape.on('dragstart', mouseDragStartEventHandler.bind(undefined, "DragStartTestPolygon"));
            shape.on('dragend', mouseDragEndEventHandler.bind(undefined, "DragEndTestPolygon"));
            break;
        case "circle":
            shape = new Kinetic.Circle(data);
            debugMessage += "width x:"+data.x+" y:"+data.y+" and radius:"+data.radius;
            break;
        case "rect":
            shape = new Kinetic.Rect(data);
            debugMessage += "width x:"+data.x+" y:"+data.y+" width:"+data.width+" height:"+data.height;
            break;
        case "text":
            shape = new Kinetic.Text(data);
            debugMessage += "width x:"+data.x+" y:"+data.y+" text:"+data.text;
            break;
        case "container":

            data.clip = [0, 0, data.width, data.height];
            shape = new Kinetic.Group(data);
            message.children.forEach(function(child) {
                shape.add(parseShapeData(child));
            });
            break;
        default:
            debugMessage = null;
            printDebugMessage("Unrecognized JSON message received from server.",2);
    }

    if(debugMessage)
        printDebugMessage(debugMessage,0);

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

    var now = new Date(),
        now = now.getHours()+':'+now.getMinutes()+':'+now.getSeconds();

    if(type == 1)
    {
        console.warn(message);
    }
    else if(type == 2)
    {
        console.error(message);
    }
    else
    {
        console.log(message);
    }

    
    $("#debug").prepend("<p><strong>["+now+"]</strong> "+message+"</p>")
}

/*
 * Canvas setup
 */

function initCanvas(container, width, height) {
    // Only init canvas when there is a container
    // Container is provided for testing purposes and extesibility
    if(container.exists()) {
        container.css( "width", width+"px" );
        container.css( "height", height+"px" );
        container.css( "margin-top", "-"+height/2+"px" );
        container.css( "margin-left", "-"+width/2+"px" );


        stage = new Kinetic.Stage({
            container: container.attr('id'),
            width: width, // Default width and height
            height: height // Can be changed with the resize canvas function
        });

        // Create new layer to draw on
        newDefaultLayer();

    }
}
function initWrapper(wrapper, width, height) {

    wrapper.css( "min-width", width+"px" );
    wrapper.css( "height", $( window ).height()+"px" );
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
    initCanvas($('#canvas'),width,height);
    initWrapper($('#wrapper'),width,height);

    // Start listening for scroll events using mousewheel.js
    $('#canvas').mousewheel(function(event) {
        sendScrollEvent(event.deltaX,event.deltaY);
        return false; // Prevent browser default behavior
    });

    // For flashing certain shapes while debugging
    $("#debug").delegate( ".debugSelector", "click", function() {
        debugCanClose = false;
        var sid = $(this).data("sid"); // The shape id in an extended data attribute

        var shape = stage.find('#' + sid)[0];
        var alreadyAnnimatingIdx = $.inArray(shape, debugAnnimatingShapes);

        if(alreadyAnnimatingIdx == -1)
        { 
            debugAnnimatingShapes.push(shape);

            var fill = shape.getFill();

            shape.setFill("red");
            layerList[topLayerIdx].draw();

            var interval = setInterval(function(){

                shape.setFill(fill);
                layerList[topLayerIdx].draw();


                clearInterval(interval);
                debugAnnimatingShapes.splice( alreadyAnnimatingIdx ,1 );
                debugCanClose = true;
            },250);
        }

    });

    // For hiding the debug console
    $("#debug").click(function(){
        if(debugCanClose) {
            if($(this).width() == 20)
            {
                $(this).animate({
                    width: 350
                },300);
            }
            else
            {
                $(this).animate({
                    width: 20
                },300);
            }
        }
    });

    // When the connection is open, send some data to the server
    connection.onopen = function () {
        printDebugMessage("Connection opened",0);
    };
    // Log errors
    connection.onerror = connectionError;
    // Callback for recieving data
    connection.onmessage = connectionDataReceived;

    // Begin to listen for keys
    window.addEventListener('keydown', sendKeyEvent.bind(this,'keydown'));
    
    window.addEventListener('keyup', sendKeyEvent.bind(this,'keyup'));
});
