- a Function returns a value
- a Leaf returns an Alist of output messages
- a Container returns an Alist

- a Component returns an Alist of output messages
- a Component was active iff
  1. lookup ('out', outputs) != null, or,
  2. outputs.length > 1

- a Leaf calls its function and puts the result in its own outputs['out']
  - the Leaf merges all other outputs sen(d)t by the function into its own outputs[]
  - the Leaf returns its own outputs[]

- a Container always produces outputs['out']==null
  - the Container merges all other outputs sen(d)t by the function into its own outputs[]
  - the Container returns its own outputs[]

- a Component creates an empty outputs[] Alist
  - the Component passes its own outputs[] Alist to its children for filling-in

- a Container runs its children until there is no change to its outputs
  - a Container runs each child and merges the child's outputs[] into its own outputs[]
  - a Container continues to run its children until its own outputs[] is not different from the previous version of its own outputs[]


LeafX
  func
    send ('string2', 'world');
    return 'hello';
  r = 'hello', Leaf.values = [['string2', 'world']];
  return [['out', 'hello'], ['string2', 'world']];

ContainerY
  children <- [LeafX];
  connections <- [ [[ children [0], 'string2'] -> [self, 'say']] ];
  return [['out', null], ['say', 'world']]

ContainerZ
  children <- [LeafX];
  connections <- [
    [[ children [0], 'string2'] -> [self, 'say2']] 
    [[ children [0], 'out'] -> [self, 'say1']] 
  ];
  return [['out', null], ['say2', 'world'], ['say1', 'hello']]
