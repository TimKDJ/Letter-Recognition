$(document).ready(function () {
    var coordinates = [];
    var mouseDown = false;
    var allowDraw = true;

    window.ondragstart = function() { 
        return false; 
    };
    
    $('#pad').delegate('td', 'mousedown', function() {
        if (allowDraw) {
            mouseDown = true;
            markCell($(this));
            setCoordinates($(this));
        }
    }).delegate('td', 'mouseenter', function() {
	if (mouseDown && allowDraw) {
            markCell($(this));
            setCoordinates($(this));
	}
    });
    $('html').bind('mouseup', function() {
	mouseDown = false;
    });

    markCell = function(el) {
        el.attr('class', 'selected');
    };
    
    setCoordinates = function(el) {
        var row = el.parent().index();
        var col = el.index();
        if (coordinates.indexOf(row + '+' + col) < 0) {
            coordinates.push(row + '+' + col);
        }
    };

    $('#continue').on('click', function() {
        if (coordinates.length > 0) {
            Shiny.onInputChange('coordinates', coordinates);
            allowDraw = false;
        }
    });
    
    $('#reset').on('click', function() {
        allowDraw = true;
        coordinates = [];
        $('td').each(function() {
            var cellClass = $(this).attr('class');
            if (cellClass == 'selected') {
                $(this).attr('class', 'unselected');
            }
        });
    });

});