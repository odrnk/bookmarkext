var exampleSocket = new WebSocket("ws://localhost:18080");

exampleSocket.onopen = function (event) {
    var data = {
        "jsonrpc": "2.0",
        "method": "Add",
        "params": {
          "x": 123,
          "y": 34
        },
        "id": 11
    };
    exampleSocket.send(JSON.stringify(data)); 
};

exampleSocket.onmessage = function (event) {
    log(event.data);
}

var log = (function () {
    var num = 1;
    return function (msg) { 
        document.body.innerHTML += num + ". " + msg + '<br>';
        num++;
    }
})();
