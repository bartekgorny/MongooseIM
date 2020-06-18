
function open_websocket() {
    socket = new WebSocket("ws://localhost:5111/ws-traffic")
    return socket
}