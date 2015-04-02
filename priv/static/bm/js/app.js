$(function () {
$( document ).ready(function() {
    var socket = new WebSocket('ws://localhost:8080/ws');

    socket.onopen = function(event) {
      console.log("Open!");
      socket.send("{}");
    };
    var hchart;
    Highcharts.setOptions({
        global: {
            useUTC: false
        }
    });

    $('#container').highcharts({
        chart: {
            type: 'spline',
            animation: Highcharts.svg, // don't animate in old IE
            marginRight: 10,
            events: {
                load: function () {
                    hchart = this;
                }
            }
        },
        title: {
            text: 'Live random data'
        },
        xAxis: {
            type: 'datetime',
            tickPixelInterval: 1000
        },
        yAxis: {
            title: {
                text: 'Value'
            },
            plotLines: [{
                value: 0,
                width: 1,
                color: '#808080'
            }]
        },
        tooltip: {
            formatter: function () {
                return '<b>' + this.series.name + '</b><br/>' +
                    Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', this.x) + '<br/>' +
                    Highcharts.numberFormat(this.y, 2);
            }
        },
        legend: {
            enabled: false
        },
        exporting: {
            enabled: false
        },
        series: [{
            name: 'Live Temperature Data',
            data: []
        }]
    });

    // Handle messages sent by the server.
    socket.onmessage = function(event) {
      var message = JSON.parse(event.data);
      console.log(message.data);
    //// prepend("<p>" + message + "</p>");
      var point = [new Date().getTime(), parseFloat(message.data) ];
      var shift = hchart.series[0].data.length > 10;
      console.log(shift);
      hchart.series[0].addPoint(point, true, shift);
    };

})});
