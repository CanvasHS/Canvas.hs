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
    // Setup canvas of Canvas.Hs
    canvas_wrapper = document.createElement("div");
    canvas_wrapper.id='canvas';
    $(document.body).append(canvas_wrapper);
    initCanvas($(canvas_wrapper), 900, 600); // Init canvas
    canvas = $(canvas_wrapper); // Get canvas element
    var layer = new Kinetic.Layer();
  });
    it("testing message after mousedown event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mousedown"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Create spy for mouseEvent method
      spyOn(connection, 'send');
      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("mousedown");
      event.pageX = 20;
      event.pageY = 126;
      stage.find('Circle')[0].fire('mousedown', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mousedown","data":{"id":"circle_nr_2","x":20,"y":20}}');
    });
    it("testing message after mouseclick event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mouseclick"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Create spy for mouseEvent method
      spyOn(connection, 'send');
      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("click");
      event.pageX = 20;
      event.pageY = 126;
      stage.find('Circle')[0].fire('click', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mouseclick","data":{"id":"circle_nr_2","x":20,"y":20}}');
    });
    it("testing message after mouseup event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mouseup"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Create spy for mouseEvent method
      spyOn(connection, 'send');
      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("mouseup");
      event.pageX = 20;
      event.pageY = 126;
      stage.find('Circle')[0].fire('mouseup', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mouseup","data":{"id":"circle_nr_2","x":20,"y":20}}');
    });
    it("testing message after mousedoubleclick event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mousedoubleclick"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Create spy for mouseEvent method
      spyOn(connection, 'send');
      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("dblclick");
      event.pageX = 20;
      event.pageY = 126;
      stage.find('Circle')[0].fire('dblclick', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mousedoubleclick","data":{"id":"circle_nr_2","x":20,"y":20}}');
    });
    it("testing message after mouseover event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mouseover"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Create spy for mouseEvent method
      spyOn(connection, 'send');
      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("mouseover");
      event.pageX = 20;
      event.pageY = 126;
      stage.find('Circle')[0].fire('mouseover', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mouseover","data":{"id":"circle_nr_2","x":20,"y":20}}');
    });
    it("testing message after mouseout event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mouseout"]}}}'});
      stage.batchDraw();
      stage.draw();

      // Create spy for mouseEvent method
      spyOn(connection, 'send');
      // Test if mouseEvent method is called after 
      // mouse event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      var event = new Event("mouseout");
      event.pageX = 20;
      event.pageY = 126;
      event.targetNode = stage.find('Circle')[0];
      stage.find('Circle')[0].fire('mouseout', event);
      expect(connection.send).toHaveBeenCalledWith('{"event":"mouseout","data":{"id":"circle_nr_2","x":20,"y":20}}');
    });
    // it("testing message after mousedrag event", function() {
    //   // Draw in the Canvas.hs canvas
    //   connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"eventId": "circle_nr_2", "listen" : ["mousedrag"]}}}'});
    //   stage.batchDraw();
    //   stage.draw();

    //   var clickSpy = jasmine.createSpy();
    //   // Create spy for mouseEvent method
    //   spyOn(connection, 'send');
    //   // Test if mouseEvent method is called after 
    //   // click event on circle but not before.
    //   expect(connection.send.callCount).toBe(0);
    //   stage.find('Circle')[0].fire('mousedown');

    //   var event = jQuery.Event( "mousedrag" );
    //   event.pageX = 100;
    //   event.pageY = 200;
    //   canvas.trigger(event);
    //   expect(connection.send).toHaveBeenCalled();

      // expect(canvas.eventListeners).toBeDefined();
      // // mouseout made because of mousedrag
      // expect(canvas.eventListeners.mouseout).toBeDefined();
      // expect(canvas.eventListeners.mouseout[0]).toBeDefined();
      // expect(canvas.eventListeners.mouseout[0].handler).toBeDefined();
      // // mouseup made because of mousedrag
      // expect(canvas.eventListeners.mouseup).toBeDefined();
      // expect(canvas.eventListeners.mouseup[0]).toBeDefined();
      // expect(canvas.eventListeners.mouseup[0].handler).toBeDefined();
      // // mousemove made because of mousedrag
      // expect(canvas.eventListeners.mousemove).toBeDefined();
      // expect(canvas.eventListeners.mousemove[0]).toBeDefined();
      // expect(canvas.eventListeners.mousemove[0].handler).toBeDefined();
    // });
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