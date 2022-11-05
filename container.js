function Container (childrenArraymap) {
this.reset = reset;
this.completed___Q = completed___Q;
this.handle = handle;
this.step = step;
this.setChildren = setChildren;
this.setRoutings = setRoutings;
this.outputs = [];
this.acceptInput = acceptInput;
this.mergeOutputs = mergeOutputs;
this.route = route;
this.runToCompletion = runToCompletion;
this.children = childrenArray;
this.set__parent__of__children (this);
this.routingMap = map;
}
;
function set__parent__of__children () {
function js (self) {
    self.children.forEach (child => {
        child.container = self;
    });
  }
}
function reset () {
this.children.forAll (child => {
child.reset ();
});
}
;
function completed___Q () {
function js (self) {
    var done = true;
    self.children.forEach (child => {
        done = done & child.completed___Q ();
    });
    return done;
  }
}
function handle (message) {
this.reset ();
;
this;
{
completed;
{
die;
'internal error: container.handle called on completed container';}
completed;
{
}}
this.acceptInput (message);
;
function send (portdata) {
this.outputs.push (new Message ('port', data, this, message));
}
this.runToCompletion (send);
this.outputs.push (new Message ('out', null, this, message));
}
function acceptInput (message) {
;
this.outputs;
this.routingMap.forAll (connection => {
;
connection;
{
down;
{
{
var childOutputs = connection.receiver.handle (new Message ('connection.port', message.data, this, message)sendProcedure);
this.mergeOutputs (childOutputs);
}}
passthrough;
{
{
var childOutputs = [new Message ('connection.port', message.data, this, message)];
this.mergeOutputs (childOutputs);
}}
across;
up;
{
}}
});
}
function mergeOutputs (childOutputs) {
function js (childOutputs) {
    var clonedOuts = Array.from (self.outputs);
    var childOuts = childOutputs.reverse ();
    childOuts.forEach (cout => {
        clonedOutputs.push (cout);
    });
    return clonedOutputs;
  }
}
function runToCompletion (sendProcedure) {
         while (! this.completed ()) {
             this.step (sendProcedure)
             this.route (sendProcedure)
         }
     
}
function step (sendProcedure) {
this.children.forAll (child => {
child.step (sendProcedure);
});
}
function route (sendProcedure) {
this.routingMap.forAll (connection => {
synonym;
deferred__message__stack;
[];
{
;
connection.direction;
{
across;
up;
{
synonym;
output__message;
connection.sender.outputs;
{
deferred__message__stack.push (???ListOfObjects{targetmessage}[{connection.receivermessage}]);}}
down;
passThrough;
{
}}}
});
deferred__message__stack.forAll (pair => {
pair.target.handle (pair.messagesendProcedure);
});
}
