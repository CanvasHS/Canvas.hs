describe("Tests function", function() {

});
describe("Test action functions", function() {
    var flag;
    var canvas;
    beforeEach(function() {
        // Required to run imageDiff
        this.addMatchers(imagediff.jasmine);
        // Setup canvas of Canvas.Hs
        canvas_wrapper = document.createElement("div");
        canvas_wrapper.id = 'canvasWrap';
        $(document.body).append(canvas_wrapper);
        initCanvas($(canvas_wrapper), 900, 600); // Init canvas
        canvas = $(canvas_wrapper).find('canvas')[0]; // Get canvas element
        $(canvas_wrapper).after('<div id="control-wrapper"><div id="control-window"></div></div>');
    });
    it("set fixed proportions", function() {
        runs(function() {
            // Set fixed proportions on the Canvas
            setFixedProportions($(canvas_wrapper), 500, 400);
        });
        runs(function() {
            // See if fixed properties were set
            expect($(canvas_wrapper).css('height')).toBe('400px');
            expect($(canvas_wrapper).css('width')).toBe('500px');
            console.log("end");
        });
    });
    it("set fluid proportions", function() {
        runs(function() {
            // Set fluid proportions on the Canvas
            setFluidProportions($(canvas_wrapper));
        });
        runs(function() {
            // See if fluid properties were set
            expect($(canvas_wrapper).css('marginTop')).toBe('0px');
            expect($(canvas_wrapper).css('marginLeft')).toBe('0px');
            expect($(canvas_wrapper).css('width')).toBe($(canvas_wrapper).parent().width()+"px");
        });
    });
    it("set window display type fixed", function() {
        runs(function() {
            // Create spy on function
            spyOn(window, 'setFixedProportions').andCallThrough();
            // Set display type
            setWindowDisplayType(0,0);
        });
        runs(function() {
            // See if function for fluid properties was called
            expect(window.setFixedProportions).toHaveBeenCalled();
            // See if the proportions match
            expect($(canvas_wrapper).css('height')).toBe(canvasWindowHeight+'px');
            expect($(canvas_wrapper).css('width')).toBe(canvasWindowWidth+'px');

        });
    });
    it("set window display type fluid", function() {
        runs(function() {
            // Create spy on function
            spyOn(window, 'setFluidProportions').andCallThrough();
            // Set display type
            setWindowDisplayType(1,0);
        });
        runs(function() {
            // See if function for fluid properties was called
            expect(window.setFluidProportions).toHaveBeenCalled();
            // See if the proportions match
            expect($(canvas_wrapper).css('marginTop')).toBe('0px');
            expect($(canvas_wrapper).css('marginLeft')).toBe('0px');
            expect($(canvas_wrapper).css('width')).toBe($(canvas_wrapper).parent().width()+"px");

        });
    });
    it("open control window", function() {
        runs(function() {
            // Open control window
            openControlWindow("Test title","Test message");
        });
        runs(function() {
            // See if control window was opened
            expect($(canvas_wrapper).next('#control-wrapper').hasClass('display')).toBe(true);
            expect($(canvas_wrapper).next('#control-wrapper').hasClass('display')).toBe(true);
            expect($(canvas_wrapper).next('#control-wrapper').find("strong").text()).toBe("Test title");

        });
    });
    it("close control window", function() {
        runs(function() {
            // Open and close control window
            openControlWindow("Test title","Test message");
            closeControlWindow();
        });
        runs(function() {
            // See if it was hidden
            expect($(canvas_wrapper).next('#control-wrapper').hasClass('display')).not.toBe(true);
            expect($(canvas_wrapper).next('#control-wrapper').hasClass('display')).not.toBe(true);
            expect($(canvas_wrapper).next('#control-wrapper').find("strong").text()).toBe("Test title");

        });
    });
    afterEach(function() {
        // Remove the canvas elements used for testing and reset variables
        $(canvas_wrapper).next('#control-wrapper').remove();
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
        // Use mock clock
        jasmine.Clock.useMock();;
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
            // See if function was called
            expect(window.printDebugMessage).toHaveBeenCalled();
            expect(window.printDebugMessage).toHaveBeenCalledWith("No actions received",0);
        });
    });
    it("parse fixedsize action", function() {
        runs(function() {
            // Create spy on function
            spyOn(window, 'setFixedProportions').andCallThrough();
            connectionDataReceived({"data": '{"actions": [' +
                '{' +
                    '"action":"windowdisplaytype",' +
                    '"data":{' +
                        '"type": 0,' +
                        '"width": 920,' +
                        '"height": 1337' +
                    '}' +
                '}' +
            ']}'});
            
        });
        runs(function() {
            // See if function for fluid properties was called
            expect(window.setFixedProportions).toHaveBeenCalled();
            expect(window.setFixedProportions).toHaveBeenCalledWith(jasmine.any(Object), 920,1337);

        });
    });
    it("parse fluidsize action", function() {
        runs(function() {
            // Create spy on function
            spyOn(window, 'setFluidProportions').andCallThrough();
            connectionDataReceived({"data": '{"actions": [' +
                '{' +
                    '"action":"windowdisplaytype",' +
                    '"data":{' +
                        '"type": 1' +
                    '}' +
                '}' +
            ']}'});
            
        });
        runs(function() {
            // See if function for fluid properties was called
            expect(window.setFluidProportions).toHaveBeenCalled();

        });
    });
    it("parse fullscreen action", function() {
        runs(function() {
            // Create spy on function
            spyOn(window, 'requestFullscreen').andCallThrough();
            spyOn(window, 'setFluidProportions').andCallThrough();
            spyOn(window, 'resizeCanvas').andCallThrough();

            connectionDataReceived({"data": '{"actions": [' +
                '{' +
                    '"action":"windowdisplaytype",' +
                    '"data":{' +
                        '"type": 2' +
                    '}' +
                '}' +
            ']}'});
            jasmine.Clock.tick(1000);
        });
        runs(function() {
            // See if function for fluid properties was called
            expect(window.setFluidProportions).toHaveBeenCalled();
            expect(window.resizeCanvas).toHaveBeenCalled();
            // See if function for fullscreen was called
            expect(window.requestFullscreen).toHaveBeenCalled();

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