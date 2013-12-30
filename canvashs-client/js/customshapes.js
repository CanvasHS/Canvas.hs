// As this is a custom shape, we will define our own namespace
var CanvasHs = {};

// Make new scope
(function() {
    CanvasHs.Arc  = function(config) {
        this.___init(config);
    }

    CanvasHs.Arc.prototype = {
        ___init: function(config) {
            Kinetic.Shape.call(this, config);
            this.className = 'Arc';
            this.setDrawFunc(this.draw);
        },
        draw: function(context) {
            context.beginPath();
            console.log(this.getRadius()+" "+this.getAngle());
            // this is a hackfix, but it works..
            context.arc(0,0, this.getRadius(), 0, -1 *this.getAngle(), true);
            context.strokeShape(this);
        }

    }

    Kinetic.Util.extend(CanvasHs.Arc, Kinetic.Shape);

    Kinetic.Factory.addGetterSetter(CanvasHs.Arc, 'radius', 0);

    Kinetic.Factory.addRotationGetterSetter(CanvasHs.Arc, 'angle', 0);

})();
