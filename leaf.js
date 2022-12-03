
function Leaf (func) 
{
this.func = func ; 
this.reset = reset ; 
this.completed___Q = completed___Q ; 
this.handle = handle ; 
this.step = step ; this.clear__outputs  this.reset  
this = armed ;  }}
function clear__outputs  {langjs(self )«
    self.outputs = [];
  » 
}
function handle (message, sendFunction) {#?this { #? this armed {
function send (port, data, trace) {this.outputs.push(⟨Messageport data this trace ⟩ ) 
} 
{
var val = this.func(message , send ) ; 
{this.outputs.push(⟨Message⟨#Outputout ⟩ val this message ⟩ ) 
this = completed ; 
} 
}#? this completed {.die 'Leaf not armed' 
} } 
}
function step (sendFunction) {#?this { #? this completed {
}#? this armed {.die 'internal error: Leaf/step called on armed leaf' 
} } 
}
function reset  {
this = armed ; 
}
function completed___Q () {#? this { #? this armed {⟨#YesNono ⟩ 
}#? this completed {⟨#YesNoyes ⟩ 
} } 
}
