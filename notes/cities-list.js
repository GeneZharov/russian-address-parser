function citiesList() {
    'use strict';

    // Сохрани локально страницу:
    //    https://ru.wikipedia.org/wiki/Список_городов_России
    // Добавь в неё:
    //    <script src="cities-list.js"></script>
    //    <script>citiesList()</script>

    var cells;
    cells = document.querySelectorAll(
        '#mw-content-text table td:nth-child(2) > *'
    );
    cells = Array.prototype.slice.call( cells, 0 ); // To real array

    var cities = cells
        .map( function (cell) {
            return cell.innerHTML.toLowerCase();
        } )
        //.sort();

    var result = cities
        .map( function (city) { return '"' + city + '"'; } )
        .join(', ');

    document.body.innerHTML = result;

}
