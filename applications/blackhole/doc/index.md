/*
Section: Blackhole
Title: Blackhole
Language: en-US
*/

# Blackhole *Realtime HTTP Websocket Events*

## Example Client
~~~
<html>
  <head>
    <script src="//cdnjs.cloudflare.com/ajax/libs/socket.io/0.9.6/socket.io.min.js"></script>
  </head>
  <body>
    <div id="content"></div>
    <script>
      var socket = io.connect('http://192.168.56.111:5555');
      socket.emit("subscribe", { account_id: "4b31dd1d32ce6d249897c06332375d65", auth_token: "7b70f69a2a4976d80bfa0382894d1553", binding: "call.CHANNEL_CREATE.*"});
      socket.emit("subscribe", { account_id: "4b31dd1d32ce6d249897c06332375d65", auth_token: "7b70f69a2a4976d80bfa0382894d1553", binding: "call.CHANNEL_ANSWER.*"});
      socket.emit("subscribe", { account_id: "4b31dd1d32ce6d249897c06332375d65", auth_token: "7b70f69a2a4976d80bfa0382894d1553", binding: "call.CHANNEL_DESTROY.*"});
      socket.emit("subscribe", { account_id: "4b31dd1d32ce6d249897c06332375d65", auth_token: "7b70f69a2a4976d80bfa0382894d1553", binding: "conference.event.*"});

      socket.on("participants_event", function (data) {
        console.log(data);
      });
      socket.on("CHANNEL_CREATE", function (data) {
        console.log(data); // data = EventJObj
      });
      socket.on("CHANNEL_ANSWER", function (data) {
        console.log(data);
      });
      socket.on("CHANNEL_DESTROY", function (data) {
        console.log(data);
      });
    </script>
  </body>
</html>
~~~

