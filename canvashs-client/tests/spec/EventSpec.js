describe("Click events", function() {
	it("no event handlers", function() {
		// Parse circle
		var circle = parseShapeData({"type": "circle","data": {"id": "circle_a","x": 1,"y": 10,"radius": 10,"stroke": {"r": 255,"g": 255,"b": 255,"a": 1 },"strokeWidth": 2,"fill": {"r":255,"g":0,"b":0,"a":1},"scaleX": 1.5}});
    expect(circle.eventListeners).toBeDefined();
    expect(circle.eventListeners.click).toBeUndefined();
	});
	it("creating mouseclick event handler", function() {
		// Parse circle
		var message = {"type": "circle", "data": {"id": "circle_nr_1", "x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"listen" : ["mousedown","mouseclick","mouseup","mousedoubleclick","mousedrag","mouseover", "mouseout"]}};
		var circle = parseShapeData(message);
		enableEventHandlers(circle, message);
    expect(circle.eventListeners).toBeDefined();
    expect(circle.eventListeners.click).toBeDefined();
    expect(circle.eventListeners.click[0]).toBeDefined();
    expect(circle.eventListeners.click[0].handler).toBeDefined();
	});
  it("testing handling of mouseclick event", function() {
    // Parse circle
    var message = {"type": "circle", "data": {"id": "circle_nr_1", "x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"listen" : ["mousedown","mouseclick","mouseup","mousedoubleclick","mousedrag","mouseover", "mouseout"]}};
    var circle = parseShapeData(message);
    enableEventHandlers(circle, message);

    var clickSpy = jasmine.createSpy();
    // Create spy for mouseEvent method
    spyOn(window, 'mouseEvent');

    // Test if mouseEvent method is called after 
    // click event on circle but not before.
    expect(window.mouseEvent.callCount).toBe(0);
    circle.fire('click');
    expect(window.mouseEvent).toHaveBeenCalled();
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
    it("testing message after mouseclick event", function() {
      // Draw in the Canvas.hs canvas
      connectionDataReceived({"data":'{"shape":{"type": "circle", "data": {"id": "circle_nr_2", "x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"listen" : ["mousedown","mouseclick","mouseup","mousedoubleclick","mousedrag","mouseover", "mouseout"]}}}'});
      stage.batchDraw();
      stage.draw();

      var clickSpy = jasmine.createSpy();
      // Create spy for mouseEvent method
      spyOn(connection, 'send');
      // Test if mouseEvent method is called after 
      // click event on circle but not before.
      expect(connection.send.callCount).toBe(0);
      stage.find('#circle_nr_2')[0].fire('click');
      expect(connection.send).toHaveBeenCalled();
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