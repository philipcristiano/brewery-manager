$(function () {
$( document ).ready(function() {
    var socket = new WebSocket('ws://localhost:8080/ws');
    var series = [];

    socket.onopen = function(event) {
      console.log("Open!");
      socket.send("{}");
    };

    var devices = [];
    rivets.bind($('#devices'), {devices: devices});
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
      if (message.type === 'event') {
          var num = message.data.value;
          var point = {x: new Date().getTime(), y: parseFloat(num)};
          var series = hchart.series[0];
          var x = (new Date()).getTime(); // current time
          series.addPoint(point, true, false, true);
          hchart.redraw();
      }
      if (message.type === 'groups') {
          console.log(message.data);
          devices.push({'id': message.data.bm_devices, 'enabled': false});
      }
    };

})});
