var e = Elm.fullscreen(Elm.Main);

e.recv("loopOut", function(event){
    e.send("input", event.value);
})

//var socket = new WebSocket("ws://localhost:3000");

//socket.onopen = function(event){
    //socket.send("asdf");
    
    //e.recv("output", function(cmd){
        ////alert("HEYO");
        //socket.send('{"ObjId":1,"Goal":{"GoTo":{"ToMap":[100.0,100.0]}},"Player":"James"}')
    //});
//};

//socket.onmessage = function(event){
    //e.send("input", event.data);
//}; 

//socket.onclose = function(event){

//}
