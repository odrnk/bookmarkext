var exampleSocket = new WebSocket("ws://localhost:18080");

exampleSocket.onopen = function (event) {
    var data = {
        "jsonrpc": "2.0",
        "method": "GetRootTags",
        "params": {
          "skip": 0,
          "take": 50,
          "sort": [
            { "name": "asc"},
            { "date_added": "desc" }
          ]
        },
        "id": 10
    };
    exampleSocket.send(JSON.stringify(data)); 
};

exampleSocket.onmessage = function (event) {
    try {
        var tmp = JSON.parse(event.data);
    } catch(e) {
        log(event.data);
        return;
    }
    var res = JSON.stringify(tmp, null, 4);
    log(res);
}

var log = (function () {
    var num = 1;
    return function (msg) { 
        document.body.innerHTML += num + ". <pre>" + msg + '</pre><br>';
        num++;
    }
})();
