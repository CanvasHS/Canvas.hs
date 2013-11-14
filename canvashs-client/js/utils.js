// Extends jquery with exists method
$.fn.exists = function () {
    return this.length !== 0;
}

// Adds string format
String.prototype.format = function() {
    var s = this,
        i = arguments.length;

    while (i--) {
        s = s.replace(new RegExp('\\{' + i + '\\}', 'gm'), arguments[i]);
    }
    return s;
};
// Avoid `console` errors in browsers that lack a console.
(function() {
    var method;
    var noop = function () {};
    var methods = [
        'assert', 'clear', 'count', 'debug', 'dir', 'dirxml', 'error',
        'exception', 'group', 'groupCollapsed', 'groupEnd', 'info', 'log',
        'markTimeline', 'profile', 'profileEnd', 'table', 'time', 'timeEnd',
        'timeStamp', 'trace', 'warn'
    ];
    var length = methods.length;
    var console = (window.console = window.console || {});

    while (length--) {
        method = methods[length];

        // Only stub undefined methods.
        if (!console[method]) {
            console[method] = noop;
        }
    }
}());

// Place any jQuery/helper plugins in here.




var _to_ascii = {
    '188': '44',
    '109': '45',
    '190': '46',
    '191': '47',
    '192': '96',
    '220': '92',
    '222': '39',
    '221': '93',
    '219': '91',
    '173': '45',
    '187': '61', //IE Key codes
    '186': '59', //IE Key codes
    '189': '45'  //IE Key codes
}

var shiftUps = {
    "96": "~",
    "49": "!",
    "50": "@",
    "51": "#",
    "52": "$",
    "53": "%",
    "54": "^",
    "55": "&",
    "56": "*",
    "57": "(",
    "48": ")",
    "45": "_",
    "61": "+",
    "91": "{",
    "93": "}",
    "92": "|",
    "59": ":",
    "39": "\"",
    "44": "<",
    "46": ">",
    "47": "?"
};
var keynames = {
    '8': "backspace",
    '9': "tab",
    '13': "enter",
    '16': "shift",
    '17': "control",
    '18': "alt",
    '20': "caps lock",
    '27': "esc",
    '32': "space",
    '33': "page-up",
    '34': "page-down",
    '35': "end",
    '36': "home",
    '37': "left-arrow",
    '38': "up-arrow",
    '39': "right-arrow",
    '40': "down-arrow",
    '45': "insert",
    '46': "delete",
    '112': "F1",
    '113': "F2",
    '114': "F3",
    '115': "F4",
    '116': "F5",
    '117': "F6",
    '118': "F7",
    '119': "F8",
    '120': "F9",
    '121': "F10",
    '122': "F11",
    '144': "num lock"
};

function normalizeKeyCode(e) {
    var c = e.which;
    //normalize keyCode 
    if (_to_ascii.hasOwnProperty(c)) {
        c = _to_ascii[c];
    }

    if (!e.shiftKey && (c >= 65 && c <= 90)) {
        c = String.fromCharCode(c + 32);
    } else if (e.shiftKey && shiftUps.hasOwnProperty(c)) {
        //get shifted keyCode value
        c = shiftUps[c];
    } else {
        c = keyToString(c);
    }
    return c;
}  

function keyToString(keyCode) {
    var key = keynames[keyCode];
    return key == undefined ? String.fromCharCode(keyCode) : key;
}