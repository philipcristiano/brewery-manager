$(function () {
$( document ).ready(function() {
    var socket = new WSWrapper('ws://localhost:8080/ws');

    var series = [];
    var devices = [];
    var device_map = {};
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
                count: 5,
                text: '5m'
            }, {
                type: 'minute',
                count: 15,
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
    });

    // Handle messages sent by the server.
    socket.setHandler('event', function(message) {
        var num = message.data.value;
        var point = {x: new Date().getTime(), y: parseFloat(num)};
        device_map[message.data.device].addPoint(message.data.sensor, point);
        hchart.redraw();
    });
    socket.setHandler('settable', function(message) {
        device_map[message.data.device].addSettable(message.data.group,
                                                    message.data.parameter,
                                                    message.data.value);
    });
    socket.setHandler('groups', function(message) {
        var num_device_ids = message.data.length;
        for (var i=0; i < num_device_ids; i++) {
            device_id = message.data[i];
            if (device_map[device_id] === undefined) {
                device = new Device(device_id, socket, hchart);
                device_map[device_id] = device;
                devices.push(device);
            }
        }
    });

})});


function Device(id, socket, chart) {
    var device = this;
    this.id = id;
    this.enabled = false;
    this.series = undefined;
    this.sensors = [];
    this.sensor_map = {}
    this.settables = [];
    this.settable_map = {}

    this.enable_changed = function() {
        socket.group_membership(id, device.enabled);
    };

    this.addPoint = function(sensor_id, point) {
        if (device.sensor_map[sensor_id] === undefined) {
            var sensor = new Sensor(device, sensor_id, socket, chart);
            device.sensor_map[sensor_id] = sensor;
            device.sensors.push(sensor);
        }
        device.sensor_map[sensor_id].addPoint(point);
    }

    this.addSettable = function(group, parameter, value) {
        var id = group + parameter;
        if (device.settable_map[id] === undefined) {
            var settable = new Settable(device, group, parameter, socket, chart);
            device.settable_map[id] = settable;
            device.settables.push(settable);
        }
        device.settable_map[id].currentValue(value);
    }

}

function Sensor(device, id, socket, chart) {
    var sensor = this;
    this.device = device;
    this.id = id
    this.series = undefined;
    this.last_value = undefined;

    this.addPoint = function(point) {
        sensor.last_value = point.y;
        if (sensor.series === undefined) {
            sensor.series = chart.addSeries({name: this.device.id + ' - ' + sensor.id,
                                             turboThreshold: 0});
        }
        sensor.series.addPoint(point, true, false, true);

    }
}

function Settable(device, group, parameter, socket, chart) {
    var settable = this;
    this.id = device.id + '-' + group + "-" + parameter;
    this.device = device;
    this.group = group;
    this.parameter = parameter;
    this.socket = socket;
    this.chart = chart;

    this.currentValue = function(value) {
        settable.value = value;
    }

    this.set = function() {
        var val = $('#' + settable.id).val()
        console.log(val);
        socket.send(JSON.stringify({"type": "set",
                                    "device": settable.device.id,
                                    "group": settable.group,
                                    "parameter": settable.parameter,
                                    "value": val}));
    }
}

function WSWrapper(URL) {
    var wsw = this;
    this.url = URL;
    this.sock = undefined;
    this.handlers = {};
    this.groups = {};

    this.send = function(data) {
        wsw.sock.send(data);
    }

    this.connect = function() {
        wsw.sock = new WebSocket(URL);
        wsw.sock.onopen = function(event) {
          console.log("Open!");
          for (device_id in wsw.groups) {
              wsw.send_group_membership(device_id, wsw.groups[device_id])
          }
        }
        wsw.sock.onclose = function(event) {
          console.log("Close :(");
          window.setTimeout(wsw.connect, 1000);
        }
        wsw.sock.onmessage = function(event) {
            var message = JSON.parse(event.data);
            var type = message.type;
            var handler = wsw.handlers[type];
            if (handler === undefined) {
                console.log("No handler for ", type);
            } else {
                handler(message);
            }
        }

    }

    this.group_membership = function(device_id, enabled) {
        wsw.groups[device_id] = enabled;
        wsw.send_group_membership(device_id, enabled);
    }

    this.send_group_membership = function(device_id, enabled) {
        var repr = [{"id": device_id, "enabled": enabled}];
        wsw.sock.send(JSON.stringify({"type": "membership", "data": repr}));
    }

    this.setHandler = function(type, handler) {
        wsw.handlers[type] = handler;
    }

    this.connect();
}
