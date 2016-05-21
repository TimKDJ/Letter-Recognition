$(document).ready(function () {
    var coordinates = [];
    var mouseDown = false;
    var allowDraw = true;
    
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
    
    $('#continue').on('click', function() {
        if (coordinates.length > 0 && allowDraw == true) {
            allowDraw = false;
            Shiny.onInputChange('coordinates', coordinates);
            $('#prediction').show('slow');
        }
    });
    
    $('#reset').on('click', function() {
        if (coordinates.length > 0) {
            allowDraw = true;
            unmarkCells();
            coordinates = [];
            $('#prediction').hide('slow');
        }
    });
    
    $('.action-button').mouseup(function(){
        $(this).blur();
    });

    markCell = function(el) {
        el.attr('class', 'selected');
    };
    
    unmarkCells = function() {
        $('td').each(function() {
            var cellClass = $(this).attr('class');
            if (cellClass == 'selected') {
                $(this).attr('class', 'unselected');
            }
        });
    };
    
    setCoordinates = function(el) {
        var row = el.parent().index();
        var col = el.index();
        if (coordinates.indexOf(row + '+' + col) < 0) {
            coordinates.push(row + '+' + col);
        }
    };
    
    window.ondragstart = function() { 
        return false; 
    };

});