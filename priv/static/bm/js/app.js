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

    $('#container').highcharts({
        chart: {
            type: 'spline',
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
            type: 'datetime'
            //tickPixelInterval: 100
        },
        yAxis: {
            title: {
                text: 'Temperature'
            }
            //plotLines: [{
            //    value: 0,
            //    width: 1,
            //    color: '#808080'
            //}]
        },
        //tooltip: {
        //    formatter: function () {
        //        return '<b>' + this.series.name + '</b><br/>' +
        //            Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', this.x) + '<br/>' +
        //            Highcharts.numberFormat(this.y, 2);
        //    }
        //},
        //legend: {
        //    enabled: false
        //},
        //exporting: {
        //    enabled: false
        //},
        rangeSelector: {
            selected: 1,
        },
        series: [{
            name: 'Live Temperature Data',
            data: [],
        }]
    });

    // Handle messages sent by the server.
    socket.onmessage = function(event) {
      var message = JSON.parse(event.data);
      console.log(message.data);
      var num = parseFloat(message.data);
      if (!isNaN(num)) {
          var point = {x: new Date().getTime(), y: parseFloat(message.data)};
          var shift = hchart.series[0].data.length >= 1000;
          //hchart.series[0].addPoint(point, true, false);
          var series = hchart.series[0];
          var x = (new Date()).getTime(); // current time
          series.addPoint(point, true, shift, true);
          hchart.redraw();
      }
    };

})});
