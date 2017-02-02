var frameCtrls = angular.module('FrameCtrls', []);

frameCtrls.controller('FrameCtrl', function($scope, SocketService) {
  $scope.connected = false;
  $scope.pool = [];
  $scope.available = [];
  var socket = SocketService(function(data) {

    if ($scope.connected) {
      // TODO: HERE BE CODE FOR HANDLING INCOMMING MESSAGES WHILE CONNECTED
    } else {
      // Handshake
      switch (data.tag) {
        case 'Available':
          $scope.available = data.ids;
          break;
        case 'Connect':
          $scope.connected = true;
          $scope.available = [];
          break;
      };

      $scope.$digest();
    }
  }, 'localhost:9160');

  $scope.connect = function(id) {
      socket.sendRequest({"tag":"Connect", "id": id});
  }
  $scope.plz = function() {
      socket.sendRequest({"tag":"PLZ", "contents": []});
  }
  $scope.disconnect = function() {
      socket.sendRequest({"tag":"Disconnect", "contents": []});
      $scope.connected = false;
  }

//------------------------------------------------------------------------------

  var frames = [];
  frames[0] = {'type' : 'slp', 'head' : 'GORGEN', 'class': 'fx', 'stats' : {'sty' : 3, 'kyl' : 2, 'skp' : 2, 'kns' : 4}}
  frames[1] = {'type' : 'rp' , 'head' : 'LOJKA', 'class': 'zs', 'stats' : {'sty' : 2, 'kyl' : 4, 'skp' : 4, 'kns' : 2}}
  frames[2] = {'type' : 'info', 'head' : 'RANDOM INFO RUTA', 'text' : 'This is some descriptive text'}
  frames[3] = {'type' : 'info', 'head' : 'MER INFO-RUTA', 'text' : 'This is some BLAAAAHAHAHHAHAHAHHAHRG'}

  $scope.frames = frames;

  $scope.getPartial = function(frame) {
    return ("partials/" + frame.type + ".html");
  };

  $scope.prettifyHeader = function(frame) {
    var str = frame.head;

    switch(frame.type) {
      case 'rp':   str += ' (RP)';  break;
      case 'slp':  str += ' (SLP)'; break;
    }

    return str;

  }

  $scope.prettifyClass = function(token) {
    switch(token) {
      case 'kr': return ' Krossare';    break;
      case 'zs': return ' Zonstrykare'; break;
      case 'ss': return ' Skrotskalle'; break;
      case 'fx': return ' Fixare';      break;
      case 'sl': return ' Slav';        break;
      case 'bo': return ' Boss';        break;
      case 'kk': return ' Krönikör';    break;
    }
  }

  $scope.closeFrame = function(index) {
    $scope.frames.splice(index, 1);
  };
});
