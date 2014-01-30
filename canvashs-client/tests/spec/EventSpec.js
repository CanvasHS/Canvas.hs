describe("Click events", function() {
	it("no event handlers", function() {
		// Parse circle
		var circle = parseShapeData({"type": "circle","data": {"id": "circle_a","x": 1,"y": 10,"radius": 10,"stroke": {"r": 255,"g": 255,"b": 255,"a": 1 },"strokeWidth": 2,"fill": {"r":255,"g":0,"b":0,"a":1},"scaleX": 1.5}});
    expect(circle.eventListeners).toBeDefined();
    expect(circle.eventListeners.mousedown).toBeUndefined();
    expect(circle.eventListeners.click).toBeUndefined();
    expect(circle.eventListeners.mouseup).toBeUndefined();
    expect(circle.eventListeners.dblclick).toBeUndefined();
    expect(circle.eventListeners.mouseover).toBeUndefined();
    expect(circle.eventListeners.mouseout).toBeUndefined();
	});
	it("creating mouse event handlers", function() {
		// Parse circle
		var message = {"type": "circle", "data": {"id": "circle_nr_1", "x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"listen" : ["mousedown","mouseclick","mouseup","mousedoubleclick","mouseover", "mouseout"]}};
		var circle = parseShapeData(message);
		enableEventHandlers(circle, message);
    expect(circle.eventListeners).toBeDefined();
    // mousedown
    expect(circle.eventListeners.mousedown).toBeDefined();
    expect(circle.eventListeners.mousedown[0]).toBeDefined();
    expect(circle.eventListeners.mousedown[0].handler).toBeDefined();
    // mouseclick
    expect(circle.eventListeners.click).toBeDefined();
    expect(circle.eventListeners.click[0]).toBeDefined();
    expect(circle.eventListeners.click[0].handler).toBeDefined();
    // mouseup
    expect(circle.eventListeners.mouseup).toBeDefined();
    expect(circle.eventListeners.mouseup[0]).toBeDefined();
    expect(circle.eventListeners.mouseup[0].handler).toBeDefined();
    // mousedoubleclick
    expect(circle.eventListeners.dblclick).toBeDefined();
    expect(circle.eventListeners.dblclick[0]).toBeDefined();
    expect(circle.eventListeners.dblclick[0].handler).toBeDefined();
    // mouseover
    expect(circle.eventListeners.mouseover).toBeDefined();
    expect(circle.eventListeners.mouseover[0]).toBeDefined();
    expect(circle.eventListeners.mouseover[0].handler).toBeDefined();
    // mouseout
    expect(circle.eventListeners.mouseout).toBeDefined();
    expect(circle.eventListeners.mouseout[0]).toBeDefined();
    expect(circle.eventListeners.mouseout[0].handler).toBeDefined();
	});
  it("creating mousedrag event handler (special case compared to other mouse event handlers)", function() {
    // Parse circle
    var message = {"type": "circle", "data": {"id": "circle_nr_1", "x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"listen" : ["mousedrag"]}};
    var circle = parseShapeData(message);
    enableEventHandlers(circle, message);
    expect(circle.eventListeners).toBeDefined();
    // mousedown made because of mousedrag
    expect(circle.eventListeners.mousedown).toBeDefined();
    expect(circle.eventListeners.mousedown[0]).toBeDefined();
    expect(circle.eventListeners.mousedown[0].handler).toBeDefined();
  });
});

describe("Draw elements to test event handling", function() {
  var flag;
  var canvas;
  beforeEach(function(){
    // Make jasmine spies
    spyOn(connection, 'send');
    spyOn(window, 'realX').andCallFake(function(x) {
      return x + 100;
    });
    spyOn(window, 'realY').andCallFake(function(y) {
      return y + 50;
    });

    // Setup canvas of Canvas.Hs
    canvas_wrapper = document.createElement("div");
    canvas_wrapper.id='canvas';
    $(document.body).append(canvas_wrapper);
    initCanvas($(canvas_wrapper), 900, 600); // Init canvas
    canvas = $(canvas_wrapper).find('canvas')[0]; // Get canvas element
    var layer = new Kinetic.Layer();
  });
    it("testing message after mousedown event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mousedown"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("mousedown");
      event.pageX = 20;
      event.pageY = 200;
      stage.find('Circle')[0].fire('mousedown', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mousedown","data":{"id":"circle_nr_2","x":120,"y":250}}');
    });
    it("testing message after mouseclick event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mouseclick"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("click");
      event.pageX = 20;
      event.pageY = 200;
      stage.find('Circle')[0].fire('click', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mouseclick","data":{"id":"circle_nr_2","x":120,"y":250}}');
    });
    it("testing message after mouseup event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mouseup"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("mouseup");
      event.pageX = 20;
      event.pageY = 200;
      stage.find('Circle')[0].fire('mouseup', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mouseup","data":{"id":"circle_nr_2","x":120,"y":250}}');
    });
    it("testing message after mousedoubleclick event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mousedoubleclick"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("dblclick");
      event.pageX = 20;
      event.pageY = 200;
      stage.find('Circle')[0].fire('dblclick', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mousedoubleclick","data":{"id":"circle_nr_2","x":120,"y":250}}');
    });
    it("testing message after mouseover event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mouseover"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("mouseover");
      event.pageX = 20;
      event.pageY = 200;
      stage.find('Circle')[0].fire('mouseover', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mouseover","data":{"id":"circle_nr_2","x":120,"y":250}}');
    });
    it("testing message after mouseout event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mouseout"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("mouseout");
      event.pageX = 20;
      event.pageY = 200;
      event.targetNode = stage.find('Circle')[0];
      stage.find('Circle')[0].fire('mouseout', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mouseout","data":{"id":"circle_nr_2","x":120,"y":250}}');
    });
    it("testing message after mousedrag event", function() {
      spyOn(window, 'mouseDragStartEventHandler').andCallThrough();
      spyOn(window, 'mouseDragEventHandler').andCallThrough();
      spyOn(window, 'mouseDragEndEventHandler').andCallThrough();

      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mousedrag"]}}}'});
      stage.batchDraw();
      stage.draw();

      var mousedownEvent = jQuery.Event( "mousedown" );
      mousedownEvent.pageX = 20;
      mousedownEvent.pageY = 20;
      expect(window.mouseDragStartEventHandler.callCount).toBe(0);
      expect(window.mouseDragEndEventHandler.callCount).toBe(0);
      expect(connection.send.callCount).toBe(0);
      stage.find('Circle')[0].fire('mousedown', mousedownEvent);
      expect(window.mouseDragStartEventHandler).toHaveBeenCalled();
      expect(window.mouseDragEndEventHandler.callCount).toBe(0);

      var event = jQuery.Event( "mousemove" );
      event.pageX = 100;
      event.pageY = 200;
      expect(window.mouseDragEventHandler.callCount).toBe(0);
      $(canvas).trigger(event);
      expect(window.mouseDragEventHandler).toHaveBeenCalled();
      expect(connection.send).toHaveBeenCalledWith('{"event":"mousedrag","data":{"id1":"circle_nr_2","x1":120,"y1":70,"id2":"circle_nr_2","x2":200,"y2":250}}');

      expect(window.mouseDragEndEventHandler.callCount).toBe(0);
      $(canvas).trigger('mouseup');
      expect(window.mouseDragEndEventHandler).toHaveBeenCalled();
    });
  afterEach(function(){
    // Remove the canvas elements used for testing and reset variables
    $(canvas_wrapper).remove();
    canvas = null;
    canvas_wrapper = null;
    // Reset variables of Canvas.hs
    var topLayerIdx = 0;
    var layerList = new Array();
    var stage = undefined;
    var connection = new WebSocket('ws://localhost:8080');
    var open = false;
  });
});