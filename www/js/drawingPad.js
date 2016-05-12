$(document).ready(function () {
    var coordinates = [];
    setCoordinates = function (rows, cols) {
        if (coordinates.indexOf(rows + '+' + cols) < 0) {
            coordinates.push(rows + '+' + cols);
        }
    };

    $('#pencil').click(function () {
        Shiny.onInputChange('coordinates', coordinates);
    });

    $(document).mousedown(function () {
        $('.cell').bind('mouseover', function () {
            var row = $(this).parent().index();
            var col = $(this).index(); 
            setCoordinates(row, col);
            $(this).css({
                background: '#0072C6'
            });
        });
    });
    $(document).mouseup(function () {
        $('.cell').unbind('mouseover');
    });
    $('.cell').mousedown(function () {
        var row = $(this).parent().index();
        var col = $(this).index();
        setCoordinates(row, col);
        $(this).css({
            background: '#0072C6'
        });
    });
});