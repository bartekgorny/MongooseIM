
let socket = new WebSocket("ws://localhost:5111/ws-traffic")

function push(event, payload) {
    m = JSON.stringify({ event : event, payload : payload})
    console.log(`--->: ${m}`);
    socket.send(m)
}

console.log(socket)
socket.onopen = function(e) {
  console.log("[open] Connection established");
};
socket.onmessage = function(event) {
    console.log(`    <---: ${event.data}`);
    e = JSON.parse(event.data)
    console.log(e)
};
let app = Elm.Traffic.init({ node: document.getElementById("session-elm-container")})
app.ports.testPort.subscribe(function(ts){
    push("test", ts)
})
