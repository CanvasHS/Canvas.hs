describe("Parse elements", function() {
  	it("parses a rectangle", function() {
  		// Parse rectangle and check output
  		var rectangle = parseFigureMessage({"type": "rect","data": {"id": "rect_b","x": 10,"y": 10,"width": 10,"height": 20,"stroke": {"r": 255,"g": 255,"b": 255,"a": 1 },"strokeWidth": 2,"fill": {"r":255,"g":0,"b":0,"a":1},"scaleX": 1.5}});
  		expect(rectangle).toBeDefined();
  		expect(rectangle.attrs.height).toBeDefined();
  		expect(rectangle.attrs.width).toBeDefined();
  		expect(rectangle.attrs.height).toBe(20);
  		expect(rectangle.attrs.width).toBe(10);
  		expect(rectangle.className).toBe("Rect");
  	});
});
describe("Draw elements", function() {
	var flag;
	var canvas;
	var canvas_compare;
	var compare_stage;
	beforeEach(function(){
		// Required to run imageDiff
    	this.addMatchers(imagediff.jasmine);
    	// Setup canvas of Canvas.Hs
	    canvas_wrapper = document.createElement("div");
	    canvas_wrapper.id='canvas';
	    $(document.body).append(canvas_wrapper);
	  	initCanvas($(canvas_wrapper), 900, 600); // Init canvas
	    canvas = $(canvas_wrapper).find('canvas')[0]; // Get canvas element
	    // Setup kinetic canvas to compare against
	    canvas_compare_wrapper = document.createElement("div");
	    canvas_compare_wrapper.id='canvas_compare';
	    $(document.body).append(canvas_compare_wrapper);
  		compare_stage = new Kinetic.Stage({
	        container: 'canvas_compare',
	        width: 900,
	        height: 600
	    });
	 	var layer = new Kinetic.Layer();
	 	compare_stage.add(layer);
	    canvas_compare = $(canvas_compare_wrapper).find('canvas')[0]; // Get canvas element
	});
  	it("draws a rectangle", function() {
  		runs(function() {
  			// Draw in the Canvas.hs canvas
	  		parseMessage({"data":'{"type": "rect","data": {"id": "rect_b","x": 10,"y": 12,"width": 10,"height": 20,"stroke": {"r":255,"g":255,"b":255,"a":1},"strokeWidth": 2,"fill": {"r":255,"g":0,"b":0,"a":1},"scaleX": 1.5}}'});
	 		stage.batchDraw();
	 		stage.draw();
	 	});
  		runs(function() {
  			// Draw in the comparison Canvas
  			layer = compare_stage.getChildren().toArray()[0];
	 		var rect = new Kinetic.Rect({
		        x: 10,
		        y: 12,
		        width: 10,
		        height: 20,
		        fill: 'rgba(255,0,0,1)',
		        stroke: 'rgba(255,255,255,1)',
		        scaleX: 1.5,
		        strokeWidth: 2
		      });
	 		layer.add(rect);
	 		compare_stage.batchDraw();
	 	});
	    runs(function() {
	    	// Compare the results
	    	expect(canvas).toImageDiffEqual(canvas_compare);
  		});
  	});
  	it("draws a polygon", function() {
  		runs(function() {
  			// Draw in the Canvas.hs canvas
	  		parseMessage({"data":'{    "type": "polygon",    "data": {        "id": "polygon_nr_23",        "points": [73, 192, 73, 160, 340, 23, 500, 109, 499, 139, 342, 93],        "stroke": {"r":255,"g":255,"b":255,"a":1},        "strokeWidth": 2,        "fill": {"r":255,"g":100,"b":100,"a":1},        "rotationDeg": 40    }}'});
	 		stage.batchDraw();
	 		stage.draw();
	 	});
  		runs(function() {
  			// Draw in the comparison Canvas
  			layer = compare_stage.getChildren().toArray()[0];
	 		var poly = new Kinetic.Polygon({
		        points: [73, 192, 73, 160, 340, 23, 500, 109, 499, 139, 342, 93],
		        fill: 'rgba(255,100,100,1)',
		        stroke: 'rgba(255,255,255,1)',
		        rotationDeg: 40,
		        strokeWidth: 2
		      });
	 		layer.add(poly);
	 		compare_stage.batchDraw();
	 	});
	    runs(function() {
	    	// Compare the results
	    	expect(canvas).toImageDiffEqual(canvas_compare);
  		});
  	});
  	it("draws a circle", function() {
  		runs(function() {
  			// Draw in the Canvas.hs canvas
	  		parseMessage({data:'{    "type": "circle",    "data": {        "id": "circle_nr_1",        "x": 20,        "y": 20,        "radius": 5,        "stroke": {"r":225,"g":255,"b":215,"a":1},        "strokeWidth": 2,        "fill": {"r":255,"g":0,"b":0,"a":0}    }}'});
	 		stage.batchDraw();
	 		stage.draw();
	 	});
  		runs(function() {
  			// Draw in the comparison Canvas
  			layer = compare_stage.getChildren().toArray()[0];
	 		var circle = new Kinetic.Circle({
		        x: 20,
		        y: 20,
		        radius: 5,
		        stroke: 'rgba(225,255,215,1)',
		        strokeWidth: 2,
		        fill: 'rgba(255,0,0,1)'
		      });
	 		layer.add(circle);
	 		compare_stage.batchDraw();
	 	});
	    runs(function() {
	    	// Compare the results
	    	expect(canvas).toImageDiffEqual(canvas_compare);
  		});
  	});
  	it("draws a text", function() {
  		runs(function() {
  			// Draw in the Canvas.hs canvas
	  		parseMessage({data:'{    "type": "text",    "data": {        "id": "text",        "x": 25,        "y": 20,        "text": "Simple Text",        "fontSize": 30,        "fontFamily": "Helvetica",        "fill": {"r":200,"g":10,"b":10,"a":0.8}    }}'});
	 		stage.batchDraw();
	 		stage.draw();
	 	});
  		runs(function() {
  			// Draw in the comparison Canvas
  			layer = compare_stage.getChildren().toArray()[0];
	 		var text = new Kinetic.Text({
		        x: 25,
		        y: 20,
		        text: "Simple Text",
		        fontSize: 30,
		        fontFamily: "Helvetica",
		        fill: 'rgba(200,10,10,0.8)'
		      });
	 		layer.add(text);
	 		compare_stage.batchDraw();
	 	});
	    runs(function() {
	    	// Compare the results
	    	expect(canvas).toImageDiffEqual(canvas_compare);
  		});
  	});
	afterEach(function(){
		// Remove the canvas elements used for testing and reset variables
		$(canvas_wrapper).remove();
		$(canvas_compare_wrapper).remove();
		canvas = null;
		canvas_wrapper = null;
		canvas_compare = null;
		canvas_wrapper_compare = null;
		compare_stage = null;
		// Reset variables of Canvas.hs
		var currentLayer = 0;
		var layerList = new Array();
		var stage = undefined;
		var connection = new WebSocket('ws://localhost:8080');
		var open = false;
	});
});