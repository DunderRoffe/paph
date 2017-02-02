var services = angular.module('Services', ['ngResource']);

services.factory('EXAMPLE', function($http) {
});

services.factory('SocketService', ['$q', '$rootScope', function($q, $rootScope) {
  return function(listener, url) {
    var Service = {};
    var ws = new WebSocket("ws://" + url +"/socket/");
    
    ws.onopen = function(){  
        console.log("Socket has been opened!");  
    };
    
    ws.onmessage = function(message) {
      var jsonMessage = JSON.parse(message.data);
      console.log("Received data from websocket: ", jsonMessage);
      listener(jsonMessage);
    };

    Service.sendRequest = function(request) {
      ws.send(JSON.stringify(request));
    }

    return Service;
  }
}])
