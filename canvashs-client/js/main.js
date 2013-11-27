// Global variables
var topLayerIdx = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');
var canvas = undefined;

var generadedShapeIdIdx = 0;
var debugOn = false;
var debugTween;
var debugAnnimatingShapes = [];
var debugCanClose = true;

/**
 * Handles data received from the websocket connection.
 * @param {type} event
 * @returns {undefined}
 */
function connectionDataReceived(event) {

    // Reset mousedrag
    mouseDragFound = false;
    
    // Clear screen
    layerList[topLayerIdx].destroyChildren();

    var dataObject = jQuery.parseJSON(event.data);

    var shape = parseShapeData(dataObject);

    // Disable mousedrag if event is no longer attached to the shape
    if(!mouseDragFound && mouseDragId!=undefined) {
        mouseDragEndEventHandler(mouseDragId,undefined);
    }

    // Draw on current layer
    layerList[topLayerIdx].add(shape);
    layerList[topLayerIdx].batchDraw();
}

/**
 * Prints a message to the console when a connection error occurs.
 * @param {type} error
 * @returns {undefined}
 */
function connectionError(error) {

    printDebugMessage("WebSocket Error " + error);
}

function parseShapeData(data) {

    var shape = shapeFromData(data);
    enableEventHandlers(shape, data);

    return shape;
}

/**
 * Adds event handlers for events the server is interested in.
 * @param {type} shape The shape on which the event handler is set.
 * @param {type} message The message from the server.
 * @returns {undefined}
 */
function enableEventHandlers(shape, message) {   
    if(message.eventData != undefined && message.eventData != null) {
        if(message.eventData.listen.indexOf("mouseclick") != -1) {
            shape.on('click', clickEventHandler.bind(undefined, message.eventData.eventId));
        }
        if(message.eventData.listen.indexOf("mousedoubleclick") != -1) {
            shape.on('dblclick', doubleClickEventHandler.bind(undefined, message.eventData.eventId));
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
            mouseDragFound = mouseDragFound || message.eventData["eventId"]==mouseDragId; // Used to disable mousedrag when no longer this event is requested
            shape.on('mousedown', mouseDragStartEventHandler.bind(undefined, message.eventData.eventId));
        }
    }
}

function clickEventHandler(id, event) { mouseEvent("mouseclick", id, event); }
function doubleClickEventHandler(id, event) { mouseEvent("mousedoubleclick", id, event); }
function mouseDownEventHandler(id, event) { mouseEvent("mousedown", id, event); }
function mouseUpEventHandler(id, event) { mouseEvent("mouseup", id, event); }
function mouseOverEventHandler(id, event) { mouseEvent("mouseover", id, event); }
function mouseOutEventHandler(id, event) { mouseEvent("mouseout", id, event); }
function mouseMoveEventHandler(id, event) { mouseEvent("mousemove", id, event); }
var mouseDragId = undefined;
var mouseDragEndHandler = undefined;
var mouseDragHandler = undefined;
var dragEventRateLimiter = undefined;
var mouseDragFound = true;
var enableDragHandler = true;
var prevMousePosX = 0;
var prevMousePosY = 0;
var mouseMoveRateLimit = 90; // The mousemove interval limit
/**
 * Starts the mouse drag event handler.
 * @param {type} id The id of the shape the event handler will listen on.
 * @param {type} event
 * @returns {undefined}
 */
function mouseDragStartEventHandler(id, event) {
    mouseDragEndHandler = mouseDragEndEventHandler.bind(undefined, id);
    mouseDragHandler = mouseDragEventHandler.bind(undefined, id);
    mouseDragId = id; // Used to disable mousedrag when mousedrag is no longer attached to the shape
    // Compensate for the position of the canvas
    var canvasPos = $("#canvas").position();
    prevMousePosX = event.pageX-canvasPos.left+450;
    prevMousePosY = event.pageY-canvasPos.top+300;
    
    canvas.on('mouseout', mouseDragEndHandler);
    canvas.on('mouseup', mouseDragEndHandler);
    canvas.on('mousemove', mouseDragHandler);
    // Limit mousemoves from firing by a specified interval
    dragEventRateLimiter = true;
    dragEventRateLimiter = window.setInterval(function(){
        enableDragHandler = true;
    }, mouseMoveRateLimit);
    // Cancel event bubbling
    event.cancelBubble = true;
}
/**
 * Handles mouse drag events.
 * @param {type} id The id of the shape the event handler listens on.
 * @param {type} event
 * @returns {undefined}
 */
function mouseDragEndEventHandler(id, event) {
    canvas.off('mouseout', mouseDragEndHandler);
    canvas.off('mouseup', mouseDragEndHandler);
    canvas.off('mousemove', mouseDragHandler);
    mouseDragId = undefined;
    mouseDragEndHandler = undefined;
    mouseDragHandler = undefined;
    if(dragEventRateLimiter != undefined) {
        clearInterval(dragEventRateLimiter);
        dragEventRateLimiter = undefined;
    }
    // Cancel event bubbling
    event.cancelBubble = true;
}
/**
 * Sends information belonging to a mouse drag event.
 * @param {type} id The id of the shape the drag occurs on.
 * @param {type} event
 * @returns {undefined}
 */
function mouseDragEventHandler(id, event) {
    if (enableDragHandler) {
        // Compensate for the position of the canvas
        var canvasPos = $("#canvas").position();
        var x1 = prevMousePosX; // Fix me
        var x2 = event.pageX-canvasPos.left+450;
        prevMousePosX = x2;
        var y1 = prevMousePosY; // Fix me
        var y2 = event.pageY-canvasPos.top+300;
        prevMousePosY = y2;

        printDebugMessage("Event mousedrag on <a data-sid=\"" + id + "\" class=\"debugSelector\">" + id + "</a> from (x1:"+x1+" y1:"+y1+") to (x2:"+x2+" y2:"+y2+")" ,0);

        connection.send(JSON.stringify({
            "event":"mousedrag",
            "data":{
                "id1": id,
                "x1": x1, 
                "y1": y1,
                "id2": id,
                "x2": x2, 
                "y2": y2
            }
        }));
        // Wait for the next interval before firing again
        enableDragHandler = false;
    }
    // Cancel event bubbling
    event.cancelBubble = true;
}
/**
 * Sends information about mouse events.
 * @param {type} eventName The name of the event as send to the server.
 * @param {type} id The id of the shape as send to the server.
 * @param {type} event
 * @returns {undefined}
 */
function mouseEvent(eventName, id, event) {
    // Compensate for the position of the canvas
    var canvasPos = $("#canvas").position();
    var x = event.pageX-canvasPos.left+450; // Fix me
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
    // Cancel event bubbling
    event.cancelBubble = true;
}
/**
 * Sends information about key events to the server.
 * @param {String} eventName The name of the key event as send to the server.
 * @param {type} event
 * @returns {undefined}
 */
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
/**
 * Sends a scroll event to the server.
 * @param {Number} deltaX How far is scrolled in horizontal direction.
 * @param {Number} deltaY How far is scrolled in vertical direction.
 * @returns {undefined}
 */
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

/**
 * Draws shapes from a JSON message.
 * @param {type} message The message out of which a shape is constructed.
 * @returns {Kinetic.Group|shapeFromData.shape|Kinetic.Circle|Kinetic.Line|Kinetic.Polygon|Kinetic.Text|Kinetic.Rect}
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
    if(!data["id"] && debugOn) {
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
            shape = new Kinetic.Polygon(data);
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
    if(dict["a"] != undefined){
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

/**
 * Prints debug messages to the console if debug is enabled.
 * @param {type} message The message that is printed to the console.
 * @param {Number} type The severity of the debug message.
 * @returns {undefined}
 */
function printDebugMessage(message, type) {
    if(debugOn) {
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

        
        $("#debug").prepend("<p><strong>["+now+"]</strong> "+message+"</p>");

    }
}

/**
 * Hides or unhides the debug console.
 * @returns {undefined}
 */
var hideDebugConsole = function(){
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
    };
/**
 * Flashes a shape when it is selected in the debugger.
 */
var debugSelector = function () {
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
};
/**
 * Initializes debug capabilities.
 * @returns {undefined}
 */
function initDebug() {
    // Enable debug
    debugOn = true;
    // Debug selector for flashing shapes
    $("#debug").delegate( ".debugSelector", "click", debugSelector);
    // For hiding the debug console
    $("#debug").click(hideDebugConsole);
    $("#debug").show();
}
/**
 * Disables debug capabilities.
 * @returns {undefined}
 */
function debugOff() {
    debugOn = false;
    // Remove debug selector
    $("#debug").undelegate( ".debugSelector", "click", debugSelector);
    // Remove click event for hiding the console
    $("#debug").off('click',hideDebugConsole);
    // Clear debug screen and hide
    $("#debug").hide().html("");
}


/**
 * Initializes the canvas.
 * @param {type} container
 * @param {Number} width The width of the canvas.
 * @param {Number} height The height of the canvas.
 * @returns {undefined}
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

/**
 * Makes a new layer to draw on.
 */
function newDefaultLayer() {
    topLayerIdx++;
    layerList[topLayerIdx] = new Kinetic.Layer();
    stage.add(layerList[topLayerIdx]);
}

/**
 * On document ready
 */
$(document).ready(function() {

    var width = 900; // defined here because the container also needs these proportions 
    var height = 600;

    // Init canvas
    initCanvas($('#canvas'),width,height);
    initWrapper($('#wrapper'),width,height);

    canvas = $("#canvas canvas");

    // Start listening for scroll events using mousewheel.js
    canvas.mousewheel(function(event) {
        sendScrollEvent(event.deltaX,event.deltaY);
        return false; // Prevent browser default behavior
    });

    
    if(debugOn) {
        initDebug();
    }
    else {
        debugOff();
    }

    

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
