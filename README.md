Note: 
- .0d is meant for machine readability / completeness (a wall of details)
- for a human readable version, see .u0d (end-User 0d) (details elided, expect the computer to infer the details)

primtives:
- pure function
  - all functions implicitly return the value of the last expression
- procedure (impure function)
- string - 'hello world'
- define dtype - `#def dtype ⟨...⟩` - type name is the fist item in ..., the rest of the items are names
- define tag - `#def tag ⟨#...⟩` - type name is the fist item in ..., the rest of the items are names
- define compound dtype `#def dtype Name = «...» | ⟨...⟩ | ...`, ex.`dtype Value = «Boolean» | *` - name of dtype is Value, it can be either a Boolean tag OR anything
- block `{ ... }`
- synonym - create a scope, bind a name to an expression within the scope
- Lists
  - [* ...] List of anything
  - [⟨...⟩ ... ] List of dtype (dtype given as first name in list)
  - [⟨#...⟩ ... ] List of tag (tag type given as first name in list) (similar to `enum` but requires type name)
- Empty Lists
  - [*] Empty List that allows Anything to be inserted as items
  - [⟨...⟩] Empty List of dtype
  - [⟨#...⟩] Empty List of tag 
- construct dtype `⟨...⟩` - dtype name is the first item, the rest are expressions
- construct tag `⟨#...⟩` - tag name is the first item, the rest are tag field names (corresponding to the tag type)
- query dtype `#?✦`
- query tag `#?#`
- query component state
- reference a function `λ`
- reference a procedure `ž`
- reference an attribute `x.y`
- call a method (extract an attribute and call it; in JS `x.y()`, in 0d `x/y`)
- call a builtin procedure `/name`
- parameter list `( ... )` or nothing
- instantiate a Component `↵` (with or without a parameter list)
- reference a parameter `𝜌`
- reference state of a component `✦`
- reference self `š`
- reference a temp variable `𝜏`
- verbatim `« ... »` where `...` can be any character except `«` and `»` or, recursively another verbatim (matched `«` and `»`) (use`\«` and `\»` for unmatched brackets)

names
- JS-like names (no spaces, must begin with a letter or _, rest of characters are alnums or _)
- compound name ❲...❳ where ... is any character except `❲` or `❳` (use `\❲` `\❳`), e.g. ❲completed ?❳ ; wart - it is difficult to give sensible error messages when a bracket is missing
- `š` - self
- keyword name `#` `JS-like name` (no spaces)
- `ϕ` null

operators
- forall `∀`
- procedure snippet `#lang (js|cl|???) verbatim`
- expression snippet `##lang (js|cl|???) verbatim`

keywords
- `#function`
- `#procedure`
- `#internal`
- `#lang`
- `##lang`
- `#? #`
- `#? ✦`
- `#def`

spaces
- file global
- function scope
- procedure scope
- temporary
- synonym scope


