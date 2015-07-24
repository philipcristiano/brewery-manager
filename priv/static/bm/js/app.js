$(function () {
$( document ).ready(function() {
    var socket = new WebSocket('ws://localhost:8080/ws');
    var series = [];

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

    $('#container').highcharts('StockChart', {
        rangeSelector: {
            enabled: true,
            inputDateFormat: '%Y-%m-%d',
            buttons: [{
                type: 'minute',
                count: 1,
                text: '1m'
            }, {
                type: 'minute',
                count: 3,
                text: '5m'
            }, {
                type: 'minute',
                count: 6,
                text: '15m'
            }, {
                type: 'minute',
                count: 60,
                text: '60m'
            }, {
                type: 'day',
                count: 1,
                text: '1d'
            }, {
                type: 'all',
                text: 'All'
            }],
        },
        chart: {
            type: 'spline',
            events: {
                load: function () {
                    hchart = this;
                }
            },
        },
        title: {
            text: 'Live random data'
        },
        xAxis: {
            type: 'datetime'
            //tickPixelInterval: 100
        },
        yAxis: {
            title: {
                text: 'Temperature'
            }
        },
        series: [{
            name: 'Live Temperature Data',
            data: [],
            turboThreshold: 0,
        }]
    });

    // Handle messages sent by the server.
    socket.onmessage = function(event) {
      var message = JSON.parse(event.data);
      console.log(message.data);
      var num = message.data.value;
      console.log(num);

      if (!isNaN(num)) {
          console.log('adding');
          var point = {x: new Date().getTime(), y: parseFloat(num)};
          var shift = hchart.series[0].data.length >= 10;
          //hchart.series[0].addPoint(point, true, false);
          var series = hchart.series[0];
          var x = (new Date()).getTime(); // current time
          series.addPoint(point, true, shift, true);
          hchart.redraw();
      }
    };

})});
