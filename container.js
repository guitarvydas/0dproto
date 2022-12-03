def tag ⟨# Direction downupacross❲pass through❳ ⟩
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
function Container (childrenArray, map, ) {armed completed } 
{this.reset ≣ reset  this.❲completed ?❳ ≣ ❲completed ?❳  this.handle ≣ handle  this.step ≣ step  this.setChildren ≣ setChildren  this.setRoutings ≣ setRoutings  this.❲clear outputs❳  this.acceptInput ≣ acceptInput  this.mergeOutputs ≣ mergeOutputs  this.route ≣ route  this.runToCompletion ≣ runToCompletion  this.children ≣ childrenArray  this.❲set parent of children❳(this )  this.routingMap ≣ map  }
function ❲clear outputs❳  
{langjs(self )«
    self.outputs = [];
  » }
function ❲set parent of children❳  
{langjs(self )«
    self.children.forEach (child => {
        child.container = self;
    });
  » }
function reset () 
{∀ this.children  child 
{child.reset } }
function ❲completed ?❳  
{langjs(self )«
    var done = true;
    self.children.forEach (child => {
        done = done & child.completed___Q ();
    });
    return done;
  » }
function handle (message, ) 
{this.reset #? this { #? this completed 
{die 'internal error: container.handle called on completed container' }#? this armed 
{} } this.acceptInput(message ) 
function send (port, data, ) 
{this.outputs.push(⟨Messageport data this message ⟩ ) } this.runToCompletion(send ) this.outputs.push(⟨Message⟨#Outputout ⟩ null this message ⟩ ) }
function acceptInput (message, ) 
{this.❲clear outputs❳ ∀ this.routingMap  connection 
{#? ⟨#Direction⟩ connection { #? ⟨#Direction⟩ ⟨#Directiondown ⟩ 
{synonym childOutputs ≣ connection.receiver.handle(⟨Messageconnection.port message.data this message ⟩ sendProcedure )  
{this.mergeOutputs(childOutputs ) } }#? ⟨#Direction⟩ ⟨#Direction❲pass through❳ ⟩ 
{synonym childOutputs ≣ [⟨Message⟩ ⟨Messageconnection.port message.data this message ⟩  ]  
{this.mergeOutputs(childOutputs ) } }#? ⟨#Direction⟩ ⟨#Directionacross ⟩ 
{}#? ⟨#Direction⟩ ⟨#Directionup ⟩ 
{} } } }
function mergeOutputs (childOutputs, ) 
{langjs(childOutputs )“
    var clonedOuts = Array.from (self.outputs);
    var childOuts = childOutputs.reverse ();
    childOuts.forEach (cout => {
        clonedOutputs.push (cout);
    });
    return clonedOutputs;
  ” }
function runToCompletion (sendProcedure, ) 
{langjs«
         while (! «this.completed ») {
             «this.step(sendProcedure ) »
             «this.route(sendProcedure ) »
         }
     » langcl«
         (loop while (not «this.completed »)
             do (progn «this.step(sendProcedure ) »
                       «this.route(sendProcedure ) »))
     » }
function step (sendProcedure, ) 
{∀ this.children  child 
{child.step(sendProcedure ) } }
function route (sendProcedure, ) 
{synonym ❲deferred message stack❳ ≣ [⟨Message⟩  ]  
{∀ this.routingMap  connection 
{#? ⟨#Direction⟩ connection.direction { #? ⟨#Direction⟩ ⟨#Directionacross ⟩ 
{synonym ❲output message❳ ≣ connection.sender.outputs  
{❲deferred message stack❳.push(⟨Destconnection.receiver connection.port ❲output message❳ ⟩ ) } }#? ⟨#Direction⟩ ⟨#Directionup ⟩ 
{synonym ❲output message❳ ≣ connection.sender.outputs  
{❲deferred message stack❳.push(⟨Destconnection.receiver connection.port ❲output message❳ ⟩ ) } }#? ⟨#Direction⟩ ⟨#Directiondown ⟩ 
{}#? ⟨#Direction⟩ ⟨#Direction❲pass through❳ ⟩ 
{} } } ∀ ❲deferred message stack❳  dest 
{synonym m ≣ ⟨Messagedest.port dest.message.data this dest.message ⟩  
{dest.target.handle(m sendProcedure ) } } } }
