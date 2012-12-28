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
    table.dataTable();
}


function makeChartWithData (data) {
    new Highcharts.Chart({
	chart:{
	    renderTo:'container',
	    marginBottom:25,
	    zoomType: 'xy',
            ignoreHiddenSeries: false,
            reflow: false
	},
	title:{
	    text:'Кто дно?'
	},
	subtitle:{
	    text:'Source: http://ru.playpw.com/ratings.html'
	},
	yAxis:{
	    title:{
		text:'Rating'
	    },
	    plotLines:[{
		value:0,
		width:1,
		color:'#808080'
	    }]
	},
	tooltip:{
            shared: false,
	    formatter: function () {
		return '<b>' + this.series.name + '</b><br/>'
		    + this.x + ' место: ' + this.y + ' очков.';
	    }
	},
	legend:{
            enabled: true,
	    layout:'vertical',
	    align:'right',
	    verticalAlign:'top',
	    y: 100,
	},
	series: data
    });
}
