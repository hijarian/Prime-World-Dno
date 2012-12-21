$(function () {
    $.ajax({
        url: "/data",
        dataType: "json",
        success: function (data) {
            makeChartWithData(data);
        },
        error: function () {
            alert("Непонятная ошибка загрузки данных!");
        }
    });
});

function makeChartWithData (data) {
    new Highcharts.Chart({
	chart:{
	    renderTo:'container',
	    type:'line',
	    marginRight:130,
	    marginBottom:25,
	    zoomType: 'xy'
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
	    formatter: function () {
		return '<b>' + this.series.name + '</b><br/>'
		    + this.x + ' место: ' + this.y + ' очков.';
	    }
	},
	legend:{
	    layout:'vertical',
	    align:'right',
	    verticalAlign:'top',
	    y: 100
	},
	series: data
    });
}
