function Container (<<childrenArraymap>>) {
this.reset = reset;
this.???jsmangle(completed ?) = ???jsmangle(completed ?);
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
this.???jsmangle(set parent of children) (this);
this.routingMap = map;
}
;
function ???jsmangle(set parent of children) () {
function js (self) {
    self.children.forEach (child => {
        child.container = self;
    });
  }
}
function reset (<<>>) {
this.children.forAll (child => {
child.reset ();
});
}
;
function ???jsmangle(completed ?) () {
function js (self) {
    var done = true;
    self.children.forEach (child => {
        done = done & child.jsmangle ('❲completed ?❳') ();
    });
    return done;
  }
}
function handle (<<message>>) {
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
function send (<<portdata>>) {
this.outputs.push (new Message ('port', data, this, message));
}
this.runToCompletion (send);
this.outputs.push (new Message ('out', ϕ, this, message));
}
function acceptInput (<<message>>) {
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
function mergeOutputs (<<childOutputs>>) {
function js (childOutputs) {
    var clonedOuts = Array.from (self.outputs);
    var childOuts = childOutputs.reverse ();
    childOuts.forEach (cout => {
        clonedOutputs.push (cout);
    });
    return clonedOutputs;
  }
}
function runToCompletion (<<sendProcedure>>) {
         while (! this.completed ()) {
             this.step (sendProcedure)
             this.route (sendProcedure)
         }
     
}
function step (<<sendProcedure>>) {
this.children.forAll (child => {
child.step (sendProcedure);
});
}
function route (<<sendProcedure>>) {
this.routingMap.forAll (connection => {
synonym;
???jsmangle(deferred message stack);
[];
{
;
connection.direction;
{
across;
up;
{
synonym;
???jsmangle(output message);
connection.sender.outputs;
{
???jsmangle(deferred message stack).push (???ListOfObjects{targetmessage}[{connection.receivermessage}]);}}
down;
passThrough;
{
}}}
});
???jsmangle(deferred message stack).forAll (pair => {
pair.target.handle (pair.messagesendProcedure);
});
}
