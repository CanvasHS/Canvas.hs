describe("Tests function", function() {

});
describe("Parses actions", function() {
});
describe("Execute actions", function() {
    var flag;
    var canvas;
    var canvas_compare;
    var compare_stage;
    beforeEach(function() {
        // Required to run imageDiff
        this.addMatchers(imagediff.jasmine);
        // Setup canvas of Canvas.Hs
        canvas_wrapper = document.createElement("div");
        canvas_wrapper.id = 'canvasWrap';
        $(document.body).append(canvas_wrapper);
        initCanvas($(canvas_wrapper), 900, 600); // Init canvas
        canvas = $(canvas_wrapper).find('canvas')[0]; // Get canvas element
        // Setup kinetic canvas to compare against
        canvas_compare_wrapper = document.createElement("div");
        canvas_compare_wrapper.id = 'canvas_compare';
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
    it("parse no actions", function() {
        runs(function() {
            // Draw in the Canvas.hs canvas
            spyOn(window, 'printDebugMessage').andCallThrough();
            connectionDataReceived({"data": '{}'});
            stage.batchDraw();
            stage.draw();
        });
        runs(function() {
            // See if function was not called
            expect(window.printDebugMessage).toHaveBeenCalled();
            expect(window.printDebugMessage).toHaveBeenCalledWith("No actions received",0);
        });
    });
    afterEach(function() {
        // Remove the canvas elements used for testing and reset variables
        $(canvas_wrapper).remove();
        $(canvas_compare_wrapper).remove();
        canvas = null;
        canvas_wrapper = null;
        canvas_compare = null;
        canvas_wrapper_compare = null;
        compare_stage = null;
        // Reset variables of Canvas.hs
        var topLayerIdx = 0;
        var layerList = new Array();
        var stage = undefined;
        var connection = new WebSocket('ws://localhost:8080');
        var open = false;
    });
});