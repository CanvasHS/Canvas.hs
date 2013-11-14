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
    key = "";
    switch(keyCode)
    {
    case 8:
        key = "backspace";
        break;
    case 9:
        key = "tab";
        break;
    case 13:
        key = "enter";
        break;
    case 16:
        key = "shift";
        break;
    case 17:
        key = "control";
        break;
    case 18:
        key = "alt";
        break;
    case 20:
        key = "caps lock";
        break;
    case 27:
        key = "esc";
        break;
    case 32:
        key = "space";
        break;
    case 33:
        key = "page-up";
        break;
    case 34:
        key = "page-down";
        break;
    case 35:
        key = "end";
        break;
    case 36:
        key = "home";
        break;
    case 37:
        key = "left-arrow";
        break;
    case 38:
        key = "up-arrow";
        break;
    case 39:
        key = "right-arrow";
        break;
    case 40:
        key = "down-arrow";
        break;
    case 45:
        key = "insert";
        break;
    case 46:
        key = "delete";
        break;
    case 112:
        key = "F1";
        break;
    case 113:
        key = "F2";
        break;
    case 114:
        key = "F3";
        break;
    case 115:
        key = "F4";
        break;
    case 116:
        key = "F5";
        break;
    case 117:
        key = "F6";
        break;
    case 118:
        key = "F7";
        break;
    case 119:
        key = "F8";
        break;
    case 120:
        key = "F9";
        break;
    case 121:
        key = "F10";
        break;
    case 122:
        key = "F11";
        break;
    case 144:
        key = "num lock";
        break;
    default:
        key = String.fromCharCode(keyCode);
    }
    return key;
}