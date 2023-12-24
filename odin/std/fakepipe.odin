package std

import "core:fmt"
import zd "../../engine/0d"
import reg "../../engine/registry0d"

initialize :: proc(r : ^reg.Component_Registry) {
    reg.add_leaf (r, reg.Leaf_Template { name = "fakepipename", instantiate = fakepipename_instantiate })

}

///////

fakepipename_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    instance_name := gensym ("fakepipename")
    return zd.make_leaf (instance_name, owner, nil, fakepipename_handle)
}

fakepipename_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    @(static) rand := 0
    rand += 1 // not very random, but good enough - 'rand' must be unique within a single run
    zd.send_string (eh, "output", fmt.aprintf ("/tmp/fakepipename%d", rand), msg)
}

