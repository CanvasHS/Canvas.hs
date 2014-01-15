// Global variables
var topLayerIdx = 0;
var layerList = new Array();
var stage = undefined;
var connection = new WebSocket('ws://localhost:8080');

var canvasWindowWidth = 900;
var canvasWindowHeight = 600;

// Event handlers
var canvas = undefined;

var generadedShapeIdIdx = 0;
var debugOn = true;
var debugAnnimatingShapes = [];
var debugCanClose = true;

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
 * Handles data received from the websocket connection.
 * @param {type} event
 * @returns {undefined}
 */
function connectionDataReceived(event) {

    // Reset mousedrag
    mouseDragFound = false;

    var dataObject = jQuery.parseJSON(event.data);

    // handle the shape data
    if(hasProperty(dataObject,"shape") && dataObject.shape != undefined) {
        var shape = parseShapeData(dataObject.shape);

        // Clear screen
        layerList[topLayerIdx].destroyChildren();

        // Disable mousedrag if event is no longer attached to the shape
        if(!mouseDragFound && mouseDragId!=undefined) {
            mouseDragEndEventHandler(mouseDragId,undefined);
        }

        // Draw on current layer
        layerList[topLayerIdx].add(shape);
        layerList[topLayerIdx].batchDraw();
    }
    else {
        printDebugMessage("No shape data recieved",1);
    }

    if(hasProperty(dataObject,"actions") && dataObject.actions != undefined && 
        Object.prototype.toString.call( dataObject.actions ) === '[object Array]' ) {
        
        var actions = dataObject.actions;

        for (var i = 0; i < actions.length; i++) {
            parseActionData(actions[i]);
        }
    }
    else {
        printDebugMessage("No actions recieved",0);
    }
}

/**
 * Prints a message to the console when a connection error occurs.
 * @param {type} error
 * @returns {undefined}
 */
function connectionError(error) {
    printDebugMessage("WebSocket Error " + error,2);
}

function connectionClosed(error) {
    printDebugMessage("Connection closed " + error,0);

    openControlWindow("Connection lost");
}
/**
 * Opens a control element with a title and a message
 * @param {type} title The title of the message (required)
 * @param {type} message The contents of the message (optional)
 * @returns {undefined}
 */
function openControlWindow(title, message) {
    // Opens the control interface element
    message = message == undefined ? "" : message;
    $("#control-wrapper").addClass('display');
    $("#control-window").addClass('display');
    $("#control-window").html("<div class=\"control-content\"><p><strong>"+title+"</strong><br />"+message+"</p></div>");   
}
/**
 * Closes the control element
 * @returns {undefined}
 */
function closeControlWindow() {
    // Closes the control interface element
    $("#control-wrapper").removeClass('display');
    $("#control-window").removeClass('display');
}

/**
 * Type is an enumeration where 0 is FizedSize 1 is FullWindow and 2 is FullScreen.
 * Width and height are required with FixedSize and are ignored with the other types.
 * @param {type} displayType
 * @param {type} attempt
 * @returns {undefined}
 */
function setWindowDisplayType(displayType, attempt)
{
    attempt = attempt != undefined ? attempt : 1;

    switch (displayType) {
        case 0: // FixedSize

            window.fullScreenApi.cancelFullScreen(document.getElementById('wrapper'));

            $("body").removeClass('fullscreen');
            $("body").removeClass('fullwindow');

            // Animate the kinetic container
            $("#canvas div").animate({
                width: canvasWindowWidth+'px',
                height: canvasWindowHeight+'px'},300);
            setFixedProportions($("#canvas"), canvasWindowWidth, canvasWindowHeight);
            resizeCanvas(); // Resizes the canvas
        break;
        case 1: // FullWindow

            window.fullScreenApi.cancelFullScreen(document.getElementById('wrapper'));

            $("body").addClass('fullwindow');
            $("body").removeClass('fullscreen');

            closeControlWindow();

            setFluidProportions($("#canvas,#canvas div"));
            $(window).resize(resizeCanvas);
            resizeCanvas(); // Resizes the canvas
        break;
        case 2: // FullScreen

            window.fullScreenApi.requestFullScreen(document.getElementById('wrapper'));

            $("body").addClass('fullscreen');
            $("body").removeClass('fullwindow');
            
            closeControlWindow();

            setFluidProportions($("#canvas,#canvas div"));
            $(window).resize(resizeCanvas);
            resizeCanvas(); // Resizes the canvas
            // If this did not result in a full screen window then request it from the user.
            if(window.fullScreenApi.isFullScreen() == false) {
                if(attempt > 2) {
                    // Show a message if it was not possible to switch to full screen
                    openControlWindow("Failed to switch to fullscreen"); 
                    setTimeout(closeControlWindow, 2400);   
                }
                else {
                    setTimeout(requestFullscreen.bind(undefined, attempt+1), 100*attempt);
                }
            }
        break;
        default:
            printDebugMessage("Window display type not supported ("+displayType+")",0);
    };
}

/**
 * Asks the user to go fullscreen.
 * @param {type} attempt
 * @returns {undefined}
 */
function requestFullscreen(attempt) {
    if(window.fullScreenApi.isFullScreen() == false) {
        openControlWindow("Switch to fullscreen?","<a href=\"#\" id=\"switchToFullscreen\">Yes</a> - <a href=\"#\" id=\"switchToFullwindow\">No</a>");    
        $("#switchToFullscreen").off('click');
        $("#switchToFullwindow").off('click');
        $("#switchToFullscreen").click(setWindowDisplayType.bind(undefined, 2, attempt));
        $("#switchToFullwindow").click(setWindowDisplayType.bind(undefined, 1, attempt));
    }
}

/**
 * Opens a prompt to ask if a file should be uploaded. When clicked on "Yes" a file selecion browser will be opened.
 * @returns {undefined}
 */
function requestUpload() {
    
    openControlWindow("Upload a file?","<a href=\"#\" id=\"acceptPrompt\">Yes</a> - <a href=\"#\" id=\"closePrompt\">No</a>");    
    $("#acceptPrompt").off('click');
    $("#closePrompt").off('click');
    $("#acceptPrompt").click(promptFileBrowser.bind(undefined));
    $("#closePrompt").click(closeControlWindow.bind(undefined));
}

/**
 * Opens a file browser in which you can select a file. This function should be called directly through user input
 * and not through a websocket for example. Browsers have built in protection to prevent this, the promt will not show.
 * @returns {undefined}
 */
function promptFileBrowser() {

    $('#fileUpload').trigger('click');
    $('#fileUpload').change(function() {
        if ( this.files && this.files[0] ) {
            var FR= new FileReader();
            FR.onload = function(e) {
                printDebugMessage("Uploading file: "+e.target.result,0);

                //e.target.result contains the base64 filecontents, but with a mimetype prepended (which we don't want)
                parts = e.target.result.split("base64,");
                mimetype = parts[0]; //not used
                contents = parts[1];
                
                connection.send(JSON.stringify({
                    "event":"upload",
                    "data":{
                        "filecontents": contents
                    }
                }));
            };       
            FR.readAsDataURL( this.files[0] );
        }
    });

    closeControlWindow();
}

/**
 * Sets fluid proportions for the container.
 * @param {type} container
 * @returns {undefined}
 */
function setFluidProportions(container) {
    // Animate to fluid width and height
    container.animate({
        top: "0",
        left: "0",
        width: '100%',
        height: '100%',
        marginTop: "0px",
        marginLeft: "0px"
    },{duration: 300,step:resizeCanvas});

}

/**
 * Sets fixed proportions for the container.
 * @param {type} container
 * @param {type} width The new fixed width of the container.
 * @param {type} height The new fixed height of the container.
 * @returns {undefined}
 */
function setFixedProportions(container,width,height) {
    // Animate to fixed width and height
    container.animate({
        top: "50%",
        left: "50%",
        width: width+'px',
        height: height+'px',
        marginTop: "-"+height/2+"px",
        marginLeft: "-"+width/2+"px"
    },{duration: 300,step:resizeCanvas});
}

/**
 * Resizes the canvas.
 * @param {type} event
 * @returns {undefined}
 */
function resizeCanvas(event) {  

    $("#canvas canvas").css( "width", $("#canvas").width()+"px" );
    $("#canvas canvas").css( "height", $("#canvas").height()+"px" );

    if(stage != undefined) {
        stage.setSize($("#canvas").width(),$("#canvas").height());
        stage.batchDraw(); // Redraw Canvas
    }
}

/**
 * Parses shape data and returns an object kinetic accepts. Also coupling to event handlers is done.
 * @param {type} shape data
 * @returns {Kinetic.Group|shapeFromData.shape|Kinetic.Circle|Kinetic.Line|Kinetic.Polygon|Kinetic.Text|Kinetic.Rect}
 */
function parseShapeData(data) {

    var shape = shapeFromData(data);
    enableEventHandlers(shape, data);

    return shape;
}

/**
 * Parses action data and execute the actions.
 * @param {type} data
 * @returns {undefined}
 */
function parseActionData(data) {
    if(hasProperty(data,"action") && data.action != undefined &&
       hasProperty(data,"data") && data.data != undefined) {

        // Parse certain properties depending on type of action
        var actionProperties = data.data;

        switch (data.action) {
            case "windowdisplaytype": // To chacnge the display type
                if(hasProperty(actionProperties,"type") && actionProperties.type != undefined) {
                    
                    // First set the global width and height var's if action contains them
                    if(hasProperty(actionProperties,"width") && actionProperties.width != undefined)
                        canvasWindowWidth = actionProperties.width;

                    if(hasProperty(actionProperties,"height") && actionProperties.height != undefined)
                        canvasWindowHeight = actionProperties.height;

                    setWindowDisplayType(actionProperties.type);
                }
                else {
                    printDebugMessage("Window Display Type action recieved without type",2);
                }

            break;
            case "debugger": // To enable or disable the debugger
                if(hasProperty(actionProperties,"enabled") && actionProperties.enabled != undefined) {

                    if(actionProperties.enabled)
                        initDebug();
                    else
                        debugOff();
                }
                else {
                    printDebugMessage("Debugger action recieved without enabled",2);
                }
            break;
            case "requestupload":

                if(hasProperty(actionProperties,"multiple") && actionProperties.multiple != undefined) {

                    if(actionProperties.multiple)
                         $('#fileUpload').prop('multiple', true);
                    else
                         $('#fileUpload').removeProp('multiple');

                    requestUpload();
                }
                else {
                    printDebugMessage("Request upload action recieved without multiple attribute",2);
                }
            
            break;
            case "download":


                if(hasProperty(actionProperties,"filecontents") && actionProperties.filecontents != undefined) {

                    document.location = 'data:Application/octet-stream,' +
                         encodeURIComponent(atob(actionProperties.filecontents));
                }
                else {
                    printDebugMessage("Download action recieved without file data",2);
                }
                

            break;
            case "prompt":

                if(hasProperty(actionProperties,"message") && actionProperties.message != undefined) {

                    var result;
                    if(hasProperty(actionProperties,"placeholder"))
                        result = prompt(actionProperties.message,actionProperties.placeholder);
                    else
                        result = prompt(actionProperties.message,"");

                    if(result != undefined)
                    {
                        connection.send(JSON.stringify({
                            "event":"prompt",
                            "data":{
                                "value": result
                            }
                        }));
                    }
                }
                else {
                    printDebugMessage("Prompt action recieved without file data",2);
                }
                

            break;
            case "acceptfiledragndrop":

            break;
            default:
                printDebugMessage("Unkown action type: "+data.action,1);
        }
    }
    else {
        printDebugMessage("Error parsing action data",2);
    }
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
function mouseOutEventHandler(id, event) { 
    // Needed, otherwhise redraw fires mouseOut event
    if(event.targetNode.getParent() != undefined) {
        mouseEvent("mouseout", id, event); 
    } 
}
function mouseMoveEventHandler(id, event) { mouseEvent("mousemove", id, event); }

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
    prevMousePosX = realX(event.pageX);
    prevMousePosY = realY(event.pageY);
    
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
        var x1 = prevMousePosX; // Fix me
        var x2 = realX(event.pageX);
        prevMousePosX = x2;
        var y1 = prevMousePosY; // Fix me
        var y2 = realY(event.pageY);
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
    x = realX(event.pageX);
    y = realY(event.pageY);
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

function realX(x) {
    var canvasPos = $("#canvas").position();
    return x-canvasPos.left-parseInt($("#canvas").css("margin-left"));
}

function realY(y) {
    var canvasPos = $("#canvas").position();
    return y-canvasPos.top-parseInt($("#canvas").css("margin-top"));
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
 * Sends a window resize event to the server.
 * @param {Number} width How wide the window has become.
 * @param {Number} height How high the window has become.
 * @returns {undefined}
 */
function sendWindowResizeEvent(width,height) {

    printDebugMessage("Window Resize (width:"+width+" height:"+height+")",0);

    connection.send(JSON.stringify({
        "event":"resizewindow",
        "data":{
            "width": width,
            "height": height
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
        case "arc":
            shape = new CanvasHs.Arc(data);
            debugMessage += "x:"+data.x+" y:"+data.y;
            break;
        case "text":
            var fontStyle = new String();
            if(data.bold) {
                fontStyle = "bold";
                delete data.bold;
            }
            if(data.italic) {
                fontStyle += " italic";
                delete data.italic;
            }

            data.fontStyle = fontStyle;
            shape = new Kinetic.Text(data);

            // As haskell has no idea about textsizes this code wil fix align
            // it will make sure that the offset is set at the middle/end of the
            // text.
            // It does keep align set, that way kinetic knows what to do with
            // multiline strings.
            var align = data["align"];

            if(align != undefined){
                var offsetX = shape.getOffsetX();
                var width = shape.getWidth();

                console.log(width);
                
                if(align == 'center'){
                    offsetX = offsetX + (width / 2);
                } else if(align == 'right') {
                    offsetX = offsetX + width;
                }

                shape.setOffsetX(offsetX);
            }

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

/**
 * Converts rgba values to colors.
 * @param {type} dict
 * @returns {String}
 */
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

    if(type == 1) {
        console.warn(message);
    }
    else if(type == 2) {
        console.error(message);
    }
    else if(debugOn) {
        console.log(message);
    }

    if(debugOn) {
        var now = new Date(),
            now = now.getHours()+':'+now.getMinutes()+':'+now.getSeconds();

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
 * @returns {undefined}
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

/**
 * Makes a new layer to draw on.
 * @returns {undefined}
 */
function newDefaultLayer() {
    topLayerIdx++;
    layerList[topLayerIdx] = new Kinetic.Layer();
    stage.add(layerList[topLayerIdx]);
}

/**
 * On document ready
 */
$(document).ready(function () {

    // Init canvas
    initCanvas($('#canvas'),canvasWindowWidth,canvasWindowHeight);
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
    // Indicate disconnected connection
    connection.onclose = connectionClosed;
    // Callback for recieving data
    connection.onmessage = connectionDataReceived;

    // Begin to listen for keys
    window.addEventListener('keydown', sendKeyEvent.bind(this,'keydown'));
    window.addEventListener('keyup', sendKeyEvent.bind(this,'keyup'));

    $( window ).resize(function() {
        sendWindowResizeEvent($(window).width(),$(window).height());
    });

});
