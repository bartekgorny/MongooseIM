initialised = false

function push(event, payload) {
    m = JSON.stringify({ event : event, payload : payload})
    if(event != "heartbeat") console.log(`--->: ${m}`);
    socket.send(m)
}

const SOCKET_STATES = {connecting: 0, open: 1, closing: 2, closed: 3}

function sendHeartbeat(socket) {
  if(!isConnected(socket)){
      initialise()
      return }
    if(socket.pendingHeartbeatRef){
      socket.pendingHeartbeatRef = null
      console.log("heartbeat timeout. Attempting to re-establish connection")
      clearInterval(socket.heartbeatTimer)
      abnormalClose(socket, "heartbeat timeout")
      initialise()
      return
    }
    socket.pendingHeartbeatRef = true
    push("heartbeat", {})
  }

function isConnected(socket){ return connectionState(socket) === "open" }

function abnormalClose(socket, reason){
    socket.closeWasClean = false
    socket.close(1000, reason)
}


function  connectionState(socket){
    switch(socket.readyState){
      case SOCKET_STATES.connecting: return "connecting"
      case SOCKET_STATES.open:       return "open"
      case SOCKET_STATES.closing:    return "closing"
      default:                       return "closed"
    }
  }


function initialise() {
    // TODO it survives server restart, but does not start properly if loaded for the first time when server is down
    socket = new WebSocket("ws://localhost:5111/ws-traffic")
    console.log(socket)
    socket.onopen = function(e) {
      console.log("[open] Connection established");
      socket.pendingHeartbeatRef = null
      clearInterval(socket.heartbeatTimer)
      socket.heartbeatTimer = setInterval(() => sendHeartbeat(socket), 2000)
      if(initialised) {
          app.ports.incPort.send({"event":"reinitialise", "payload":{}})
      }else{
          start_app()
          initialised = true
      }

    };
    socket.onmessage = function(event) {
        socket.pendingHeartbeatRef = null
        e = JSON.parse(event.data)
        if(e.event != "heartbeat_ok"){
            console.log(`    <---: ${event.data}`);
            handle_event(e)
        }
    };
}

initialise()

function start_app() {
    app = Elm.Traffic.init({ node: document.getElementById("session-elm-container")})
    app.ports.outPort.subscribe(function(data){
        push(data[0], data[1])
    })
}

function handle_event(e){
  app.ports.incPort.send(e)
}
