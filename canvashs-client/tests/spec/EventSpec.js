describe("Click events", function() {
	it("no click event", function() {
		// Parse circle and check output
		var circle = parseShapeData({"type": "circle","data": {"id": "circle_a","x": 1,"y": 10,"radius": 10,"stroke": {"r": 255,"g": 255,"b": 255,"a": 1 },"strokeWidth": 2,"fill": {"r":255,"g":0,"b":0,"a":1},"scaleX": 1.5}});
    expect(circle.eventListeners).toBeDefined();
    expect(circle.eventListeners.click).toBeUndefined();
	});
	it("mouseclick", function() {
		// Parse circle and check output
		var message = {"type": "circle", "data": {"id": "circle_nr_1", "x": 20, "y": 20, "radius": 5, "stroke": {"r": 255,"g": 255,"b": 255,"a": 1 }, "strokeWidth": 2, "fill": {"r":255,"g":0,"b":0,"a":1}}, "eventData": {"listen" : ["mousedown","mouseclick","mouseup","mousedoubleclick","mousedrag","mouseover", "mouseout"]}};
		var circle = parseShapeData(message);
		enableEventHandlers(circle, message);
    expect(circle.eventListeners).toBeDefined();
    expect(circle.eventListeners.click).toBeDefined();
    expect(circle.eventListeners.click[0]).toBeDefined();
    expect(circle.eventListeners.click[0].handler).toBeDefined();
	});
});