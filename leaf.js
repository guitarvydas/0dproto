
function Leaf (func) 
{
this.func = func ; 
this.reset = reset ; 
this.completed___Q = completed___Q ; 
this.handle = handle ; 
this.step = step ; this.clear__outputs  this.reset  
this = armed ;  }}
function clear__outputs () {langjs(self )«
    self.outputs = [];
  » 
}
function handle (message, sendFunction) {{
if (this._state === 'armed') {
function send (port, data, trace) {this.outputs.push(⟨Messageport data this trace ⟩ ) 
} 
{
var val = this.func(message , send ) ; 
{this.outputs.push(⟨Message'out ' val this message ⟩ ) 
this = completed ; 
} 
}
if (this._state === 'completed') {.die 'Leaf not armed' 
}} 
}
function step (sendFunction) {{
if (this._state === 'completed') {
}
if (this._state === 'armed') {.die 'internal error: Leaf/step called on armed leaf' 
}} 
}
function reset () {
this = armed ; 
}
function completed___Q () {{
if (this._state === 'armed') {'no ' 
}
if (this._state === 'completed') {'yes ' 
}} 
}
