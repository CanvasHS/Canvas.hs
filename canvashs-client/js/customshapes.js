// As this is a custom shape, we will define our own namespace
var CanvasHs = {}

// Make new scope
(function() {
    CanvasHs.Arc  = function(config) {
        this.__init(config);
    }

    CanvasHs.Arc.prototype = {
        __init: function(config) {
            Kinetic.Wedge.call(this, config);
            this.className = 'Arc';
            this.setDrawFunc(this.draw);
        },
        draw: function(context) {
            context.beginPath();
            context.arc(0,0, this.getRadius(), 0, this.getAngle(), this.getClockwise());
            context.closePath();
            context.fillStrokeShape(this);
        }

    }

    Kinetic.Util.extend(CanvasHs.Arc, Kinetic.Wedge);

})();
