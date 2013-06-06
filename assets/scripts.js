$(function () {
    $.ajax({
        url: "/data",
        dataType: "json",
        success: function (data) {
            makeChartWithData(data);
            fillRatingsTable(data);
            $('.pages-toggler-wrapper').show();
        },
        error: function () {
            alert("Непонятная ошибка загрузки данных!");
        }
    });
    $('.pages-toggler-wrapper').on('click', '.pages-toggler', function (event) {
        $('.page').fadeOut('fast');
        var target_selector = '#' + $(this).data('target');
        $(target_selector).fadeIn('fast');
    });
});


function fillRatingsTable(data) {
    var table = $('#meanvalues-list');
    var table_contents = '<tbody>';
    
    for (var i = 0; i < data.length; ++i) {
        var hero_name = data[i].name;
        var hero_mean_rating = data[i].mean;
        var hero_corrected_mean_rating = data[i]["mean-last"];
        table_contents += '<tr><td>' + hero_name + '</td><td>' + hero_mean_rating + '</td><td>' + hero_corrected_mean_rating + '</td></tr>';
    }
    
    table_contents += '</tbody>';

    table.append(table_contents);
    table.dataTable({
        iDisplayLength: 60,
        aaSorting: [[ 2, "desc" ]]
    });
}


function makeChartWithData (data) {
    new Highcharts.Chart({
        chart:
        {
            renderTo:'container',
            marginBottom:25,
            zoomType: 'xy',
            ignoreHiddenSeries: false,
            reflow: false,
            events: 
            {
                load: selectAllSeries
            }
        },
        title: 
        {
            text:'Кто дно?'
        },
        subtitle: 
        {
            text:'Стырено с: http://ru.playpw.com/ratings.html'
        },
        xAxis:
        {
            title:
            {
                text: 'Место'
            },
            tickInterval: 2
        },
        yAxis:
        {
            title: 
            {
                text:'Рейтинг'
            },
            plotLines:
            [{
                value:0,
                width:1,
                color:'#808080'
            }]
        },
        plotOptions: 
        {
            line: 
            {
                showCheckbox: true,
                events:
                {
                    click: filterSingleSerie
                }
            },
            series: 
            {
                events: 
                {
                    checkboxClick: toggleSingleSerie,
                    legendItemClick: filterSingleSerie
                }
            }
        },
        tooltip: 
        {
            shared: false,
            formatter: function () 
            {
                return '<b>' + this.series.name + '</b><br/>'
                    + this.x + ' место: ' + this.y + ' очков.';
            }
        },
        legend: 
        {
            enabled: true,
            layout:'vertical',
            align:'right',
            verticalAlign:'top',
            y: 100,
        },
        series: data
    });

    function filterSingleSerie(event) {
        var chosenSerie = this.index;
        var series = this.chart.series;
        
        if (this.visible && this.hasOwnProperty('onlyItShown') && this.onlyItShown) {
            showAllSeries(series);
        } else {
            hideAllSeriesExceptOne(series, chosenSerie);
        }
        
        return false;

        function showAllSeries(series) {
            for (var i = 0; i < series.length; ++i) {
                series[i].onlyItShown = false;
                series[i].show();
                series[i].select(true);
            }
        }

        function hideAllSeriesExceptOne(series, chosenSerieIndex) {
            for (var i = 0; i < series.length; ++i) {
                if (series[i].index == chosenSerieIndex) {
                    series[i].onlyItShown = true;
                    series[i].show();
                    series[i].select(true);
                } else {
                    series[i].onlyItShown = false;
                    series[i].hide();
                    series[i].select(false);
                }
            }
        }
    }

    function toggleSingleSerie(event) {
        if (this.visible) {
            this.hide();
            this.select(false);
        } else {
            this.show();
            this.select(true);
        }
        return false;
    }

    function selectAllSeries(event) {
        for (var i = 0; i < this.series.length; ++i) {
            this.series[i].select(true);
        }
    }
}
