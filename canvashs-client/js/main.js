// Global variables
var topLayerIdx = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');
var canvasConstWidth = 900; // Do not change after starting
var canvasConstHeight = 600;

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

function connectionClosed(error) {
    printDebugMessage("Connection closed " + error);
    console.log(error);
    $("#control-wrapper").addClass('display');
    $("#control-window").addClass('display');
    $("#control-window").html("<div class=\"control-content\"><p><strong>Connection lost</strong><br /><!--Retrying in 3... <a>reconnect</a>--></p></div>");
}
function fullScreen() {
    if (document.documentElement.requestFullscreen) {
      document.documentElement.requestFullscreen();
    } else if (document.documentElement.mozRequestFullScreen) {
      document.documentElement.mozRequestFullScreen();
    } else if (document.documentElement.webkitRequestFullscreen) {
      document.documentElement.webkitRequestFullscreen();
    }
}
function fullWindow(container) {
    $("body").addClass('fullwindow');
    setFluidProportions($("#canvas,#canvas div"));
    $(window).resize(resize);
    resize(); // Resize to set correct offset
}
function fullWindowOff(container) {
    $("body").removeClass('fullwindow');
    // Animate the kinetic container
    $("#canvas div").animate({
        width: canvasConstWidth+'px',
        height: canvasConstHeight+'px'},300);
    setFixedProportions($("#canvas"), canvasConstWidth, canvasConstHeight);
    resize(); // Resizes the canvas
}
function setFluidProportions(container) {
    // Animate to fluid width and height
    container.animate({
        top: "0",
        left: "0",
        width: '100%',
        height: '100%',
        marginTop: "0px",
        marginLeft: "0px"
    },{duration: 300,step:resize});
}
function setFixedProportions(container,width,height) {
    // Animate to fixed width and height
    container.animate({
        top: "50%",
        left: "50%",
        width: width+'px',
        height: height+'px',
        marginTop: "-"+height/2+"px",
        marginLeft: "-"+width/2+"px"
    },{duration: 300,step:resize});
}
function resetOffset() {
    // Reset offset of the current layer, this makes sure the layer is in the center of the screen
    if(layerList[topLayerIdx]!=undefined) {
        layerList[topLayerIdx].setPosition(
            Math.round(($("#canvas canvas").attr("width")*1-canvasConstWidth)/2),
            Math.round(($("#canvas canvas").attr("height")*1-canvasConstHeight)/2)
        );

        stage.batchDraw(); // Redraw Canvas
    }
    
}
function resize(event) {
    $("#canvas canvas").attr("width",$("#canvas").outerWidth());
    $("#canvas canvas").attr("height",$("#canvas").outerHeight());
    $("#canvas canvas").css("width",$("#canvas").outerWidth()+"px");
    $("#canvas canvas").css("height",$("#canvas").outerHeight()+"px");
    //Reset offset
    resetOffset();
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
            shape.on('mousedrag', mouseDragEventHandler.bind(undefined, message.eventData.eventId));
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
    console.log(event);
    connection.send(JSON.stringify({
        "event":eventName,
        "data":{
            "id": id,
            "x": realX(event.pageX),
            "y": realY(event.pageY)
        }
    }));
}
function sendKeyEvent(eventName, event) {
    console.log(event);
    event.preventDefault();
    connection.send(JSON.stringify({
        "event":eventName,
        "data":{
            "key": normalizeKeyCode(event),
            "control": event.ctrlKey || event.metaKey,
            "alt": event.altKey,
            "shift": event.shiftKey
        }
    }));
}
function realX(x) {
    var canvasPos = $("#canvas").position();
    return x-canvasPos.left-parseInt($("#canvas").css("margin-left"))-layerList[topLayerIdx].getX();
}
function realY(y) {
    var canvasPos = $("#canvas").position();
    return y-canvasPos.top-parseInt($("#canvas").css("margin-top"))-layerList[topLayerIdx].getY();
}

/*
 * Drawing shapes
 */ 

function shapeFromData(message) {



    var shape = null;
    var dataMessage = "";
    var data = message.data;

    // both fill and stroke are in the form {"r": 255, "g": 255, "b": 255, ("a": 1.0)?}
    if(data["fill"]){
        data["fill"] = rgbaDictToColor(data["fill"]);
    }

    if(data["stroke"]){
        data["stroke"] = rgbaDictToColor(data["stroke"]);
    }
    // Debug message
    var debugMessage = "Drawing " + message.type;
    if(data.id)
        dataMessage += "(" + data.id + ")"
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
 * Canvas setup
 */

function initCanvas(container, width, height) {
    // Only init canvas when there is a container
    // Container is provided for testing purposes and extesibility
    if(container.exists()) {
        setFixedProportions(container,width,height);


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
    wrapper.css( "min-height", height+"px" );
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

    var width = canvasConstWidth; // defined here because the container also needs these proportions 
    var height = canvasConstHeight;

    // Init canvas
    initCanvas($('#canvas'),width,height);
    initWrapper($('#wrapper'),width,height);

    $("#debug").mouseup(function(){

        if($(this).width() == 10)
        {
            $(this).animate({
                width: 350
            },300);
        }
        else
        {
            $(this).animate({
                width: 10
            },300);
        }
    });

    // When the connection is open, send some data to the server
    connection.onopen = function () {
        printDebugMessage("Connection opened",0);
    };
    // Log errors
    connection.onerror = connectionError;
    // Indicate disconnected connection
    connection.onclose = connectionClosed;
    // Callback for recieving data
    connection.onmessage = connectionDataReceived;

    // Begin to listen for keys
    window.addEventListener('keydown', sendKeyEvent.bind(this,'keydown'));
    
    window.addEventListener('keyup', sendKeyEvent.bind(this,'keyup'));
});
