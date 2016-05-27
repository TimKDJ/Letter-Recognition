$(document).ready(function () {  // let the page load before running the code
    
    var coordinates = [];  // holds the coordinates of marked pad cells as row+column (e.g. 7+5)
    var mouseDown = false;  // is set to true after mousedown on the pad occurs
    var allowDraw = true;  // gets disabled after prediction is shown


    $('#pad').delegate('td', 'mousedown', function () {  // binds mousedown to td elements of the pad and marks cells
        if (allowDraw) {  
            mouseDown = true;
            markCell($(this));  // change css style of td
            setCoordinates($(this));  // add row and column to coordinates
        }
    }).delegate('td', 'mouseenter', function () {  // binds mouseenter to td elements and marks cells
        if (mouseDown && allowDraw) {  // if mouse is down and movement occurs, draw
            markCell($(this));
            setCoordinates($(this));
        }
    });


    $('html').bind('mouseup', function () {  // binds mouseup, revert the mousedown state disallow drawing
        mouseDown = false;
    });


    $('#continue').on('click', function () {  // reacts to click of continue button
        if (coordinates.length > 0 && allowDraw == true) {  // ensure that the user drew something and the button wasn't clicked already
            allowDraw = false;
            Shiny.onInputChange('coordinates', coordinates);  // feed the coordinates to the R server
            $('#prediction').show('slow');  // show the prediction generated by R
        }
    });


    $('#reset').on('click', function () {  // reacts to click of reset button
        if (coordinates.length > 0) {  // make sure the user drew something
            allowDraw = true;
            unmarkCells();  // revert the color changes
            coordinates = [];  // empty the coordinates
            $('#prediction').hide('slow');  // hide the prediction given by R
        }
    });


    $('.action-button').mouseup(function () {  // after a button is clicked, change the button layout back to normal
        $(this).blur();
    });


    markCell = function (el) {  // changes the class to 'selected' and thereby the color of a td element
        el.attr('class', 'selected');
    };


    unmarkCells = function () {  // removes all 'selected' classes and reverts it back to 'unselected'
        $('td').each(function () {
            var cellClass = $(this).attr('class');
            if (cellClass == 'selected') {
                $(this).attr('class', 'unselected');
            }
        });
    };


    setCoordinates = function (el) {  // obtains row and column index and adds it to an array
        var row = el.parent().index();  // index of tr equals the row
        var col = el.index();  // index of td equals the column
        if (coordinates.indexOf(row + '+' + col) < 0) {  // make sure the coordinate is not yet in the array
            coordinates.push(row + '+' + col);  // append it to the array
        }
    };


    window.ondragstart = function () {  // this prevents dragging of previously colored elements and thereby making the drawing more smooth
        return false;
    };

});