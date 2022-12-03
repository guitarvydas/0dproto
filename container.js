def tag ⟨# Direction downupacrosspass__through ⟩
def tag ⟨# Input in ⟩
def tag ⟨# Output out ⟩
def dtype ⟨ Sender componentport ⟩
def dtype ⟨ Receiver componentport ⟩
def dtype ⟨ MessageWithoutDebug portdata ⟩
def dtype ⟨ Message portdataorigintrail ⟩
def dtype ⟨ Connection directionsenderreceiver ⟩
def tag ⟨# YesNo yesno ⟩
def dtype ⟨ Data Value ⟩
def dtype Value = ⟨#YesNo⟩ | * 
function Container (childrenArray, map, ) 
{this.reset ≣ reset  this.completed___Q ≣ completed___Q  this.handle ≣ handle  this.step ≣ step  this.setChildren ≣ setChildren  this.setRoutings ≣ setRoutings  this.clear__outputs  this.acceptInput ≣ acceptInput  this.mergeOutputs ≣ mergeOutputs  this.route ≣ route  this.runToCompletion ≣ runToCompletion  this.children ≣ childrenArray  this.set__parent__of__children(this , )  this.routingMap ≣ map  }}
function clear__outputs  {langjs(self , )«
    self.outputs = [];
  » 
}
function set__parent__of__children  {langjs(self , )«
    self.children.forEach (child => {
        child.container = self;
    });
  » 
}
function reset () {this.children .forAll (child => {child.reset 
}); 
}
function completed___Q  {langjs(self , )«
    var done = true;
    self.children.forEach (child => {
        done = done & child.completed___Q ();
    });
    return done;
  » 
}
function handle (message, ) {this.reset #? this { #? this completed {die 'internal error: container.handle called on completed container' 
}#? this armed {
} } this.acceptInput(message , ) 
function send (port, data, ) {this.outputs.push(⟨Messageport data this message ⟩ , ) 
} this.runToCompletion(send , ) this.outputs.push(⟨Message⟨#Outputout ⟩ null this message ⟩ , ) 
}
function acceptInput (message, ) {this.clear__outputs this.routingMap .forAll (connection => {#? ⟨#Direction⟩ connection { #? ⟨#Direction⟩ ⟨#Directiondown ⟩ {
{
var childOutputs = connection.receiver.handle(⟨Messageconnection.port message.data this message ⟩ , sendProcedure , ) ; 
{this.mergeOutputs(childOutputs , ) 
} 
}#? ⟨#Direction⟩ ⟨#Directionpass__through ⟩ {
{
var childOutputs = [⟨Message⟩ ⟨Messageconnection.port message.data this message ⟩  ] ; 
{this.mergeOutputs(childOutputs , ) 
} 
}#? ⟨#Direction⟩ ⟨#Directionacross ⟩ {
}#? ⟨#Direction⟩ ⟨#Directionup ⟩ {
} } 
}); 
}
function mergeOutputs (childOutputs, ) {langjs(childOutputs , )“
    var clonedOuts = Array.from (self.outputs);
    var childOuts = childOutputs.reverse ();
    childOuts.forEach (cout => {
        clonedOutputs.push (cout);
    });
    return clonedOutputs;
  ” 
}
function runToCompletion (sendProcedure, ) {langjs«
         while (! «this.completed ») {
             «this.step(sendProcedure , ) »
             «this.route(sendProcedure , ) »
         }
     » langcl«
         (loop while (not «this.completed »)
             do (progn «this.step(sendProcedure , ) »
                       «this.route(sendProcedure , ) »))
     » 
}
function step (sendProcedure, ) {this.children .forAll (child => {child.step(sendProcedure , ) 
}); 
}
function route (sendProcedure, ) {
{
var deferred__message__stack = [⟨Message⟩  ] ; 
{this.routingMap .forAll (connection => {#? ⟨#Direction⟩ connection.direction { #? ⟨#Direction⟩ ⟨#Directionacross ⟩ {
{
var output__message = connection.sender.outputs ; 
{deferred__message__stack.push(⟨Destconnection.receiver connection.port output__message ⟩ , ) 
} 
}#? ⟨#Direction⟩ ⟨#Directionup ⟩ {
{
var output__message = connection.sender.outputs ; 
{deferred__message__stack.push(⟨Destconnection.receiver connection.port output__message ⟩ , ) 
} 
}#? ⟨#Direction⟩ ⟨#Directiondown ⟩ {
}#? ⟨#Direction⟩ ⟨#Directionpass__through ⟩ {
} } 
}); deferred__message__stack .forAll (dest => {
{
var m = ⟨Messagedest.port dest.message.data this dest.message ⟩ ; 
{dest.target.handle(m , sendProcedure , ) 
} 
}); 
} 
}
