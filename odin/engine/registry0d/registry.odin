package registry0d

import "core:fmt"
import "core:os"
import "core:log"
import "core:encoding/json" 
import "core:path/filepath"

import zd "../0d"
import "../../../ir"

Registry_Stats :: struct {
    nleaves : int,
    ncontainers : int,
    ninstances : int
}

Component_Registry :: struct {
    templates: map[string]Template,
    stats : Registry_Stats,
}

Template_Kind :: enum {Leaf, Container}

Container_Template :: struct {
    name: string,
    decl: ir.Container_Decl,
}

Leaf_Template :: struct {
    name: string,
    instantiate: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh,
}

Leaf_Instantiator :: Leaf_Template

Template :: union {
    Leaf_Template,
    Container_Template,
}


    
json2internal :: proc (container_xml : string) -> ^[]ir.Container_Decl {
    fname := fmt.aprintf ("%v.json", filepath.base (container_xml))
    read_fd, read_errnum := os.open (path=fname, flags=os.O_RDONLY)
    fmt.assertf (read_errnum == 0, "read open error on %v, err=%v\n", fname, read_errnum)
    data, success := os.read_entire_file_from_handle (read_fd)
    fmt.assertf (success, "read error on file %s errno=%v", fname, os.get_last_error ())
    s := transmute(string)data
    decls := new ([]ir.Container_Decl)
    unmarshal_err := json.unmarshal_string (s, decls)
    fmt.assertf (unmarshal_err == nil || unmarshal_err == .None, "failure converting from JSON to internal format %v\n", unmarshal_err)
    os.close (read_fd)
    return decls
}

delete_decls :: proc (^[]ir.Container_Decl) {
    // TBD
}

make_component_registry :: proc(leaves: []Leaf_Template, containers: ^[]ir.Container_Decl) -> ^Component_Registry {

    reg :=  new (Component_Registry)

    for leaf_template in leaves {
	add_leaf (reg, leaf_template)
    }

    for decl in containers {
        container_template := Container_Template {
	    name=decl.name,
            decl = decl,
        }
	fmt.assertf (!(decl.name in reg.templates), "component \"%v\" already declared", decl.name)
        reg.templates[decl.name] = container_template
	reg.stats.ncontainers += 1
    }

    return reg
}

add_leaf :: proc (r : ^Component_Registry, leaf_template : Leaf_Template) {
	fmt.assertf (!(leaf_template.name in r.templates), "Leaf \"%v\" already declared", leaf_template.name)
        r.templates[leaf_template.name] = leaf_template
	r.stats.nleaves += 1
}

get_component_instance :: proc(reg: ^Component_Registry, name: string, owner : ^zd.Eh) -> (instance: ^zd.Eh, ok: bool) {
    descriptor: Template
    descriptor, ok = reg.templates[name]
    component_name := fmt.aprintf ("%s.%s", owner.name, name)
    if ok {
        switch template in descriptor {
        case Leaf_Template:
            instance = template.instantiate(component_name, owner)
        case Container_Template:
            instance = container_instantiator(reg, owner, template.decl, component_name)
        }
	reg.stats.ninstances += 1
    }
    return instance, ok
}

container_instantiator :: proc(reg: ^Component_Registry, owner : ^zd.Eh, decl: ir.Container_Decl, container_name : string) -> ^zd.Eh {

    container := zd.make_container(container_name, owner)

    children := make([dynamic]^zd.Eh)

    // this map is temporarily used to ensure connector pointers into the child array
    // line up to the same instances
    child_id_map := make(map[int]^zd.Eh)
    defer delete(child_id_map)

    // collect children
    {
        for child_decl in decl.children {
            child_instance, ok := get_component_instance(reg, child_decl.name, container)
            //fmt.assertf (ok, "\n*** Error: Can't find component %v\n", child_decl.name)
	    if !ok {
		fmt.printf ("\n*** Error: Can't find component %v\n", child_decl.name)
		os.exit (1)
	    }
            append(&children, child_instance)
            child_id_map[child_decl.id] = child_instance
        }
        container.children = children[:]
    }

    // setup connections
    {
        connectors := make([dynamic]zd.Connector)

        for c in decl.connections {
            connector: zd.Connector

            target_component: ^zd.Eh
            target_ok := false

            source_component: ^zd.Eh
            source_ok := false


            switch c.dir {
            case .Down:
                connector.direction = .Down
                connector.sender = {
		    "",
                    nil,
                    c.source_port,
                }
                source_ok = true

                target_component, target_ok = child_id_map[c.target.id]
		if !target_ok {
		    fmt.printf ("internal error: .Down connection target not ok %v\n", c.target)
		    os.exit (1)
		}
                connector.receiver = {
		    target_component.name,
                    &target_component.input,
                    c.target_port,
                }
            case .Across:
                connector.direction = .Across
                source_component, source_ok = child_id_map[c.source.id]
                target_component, target_ok = child_id_map[c.target.id]

		if !source_ok {
		    fmt.printf ("internal error: .Across connection source not ok %v\n", c.source)
		    os.exit (1)
		}
		if !target_ok {
		    fmt.printf ("internal error: .Across connection target not ok %v\n", c.target)
		    os.exit (1)
		}

                connector.sender = {
		    source_component.name,
                    source_component,
                    c.source_port,
                }

                connector.receiver = {
		    target_component.name,
                    &target_component.input,
                    c.target_port,
                }
            case .Up:
                connector.direction = .Up
                source_component, source_ok = child_id_map[c.source.id]
		if !source_ok {
		    fmt.printf ("internal error: .Up connection source not ok %v\n", c.source)
		    os.exit (1)
		}
                connector.sender = {
		    source_component.name,
                    source_component,
                    c.source_port,
                }

                connector.receiver = {
		    "",
                    &container.output,
                    c.target_port,
                }
                target_ok = true
            case .Through:
                connector.direction = .Through
                connector.sender = {
		    "",
                    nil,
                    c.source_port,
                }
                source_ok = true

                connector.receiver = {
		    "",
                    &container.output,
                    c.target_port,
                }
                target_ok = true
            }

            if source_ok && target_ok {
                append(&connectors, connector)
            } else if source_ok {              
	      fmt.println ("no target", c)
            } else {              
	      fmt.println ("no source", c)
	    }
        }

        container.connections = connectors[:]
    }

    return container
}

append_leaf :: proc (template_map: ^[dynamic]Leaf_Instantiator, template: Leaf_Template) {
    append (template_map, template)
}

dump_registry:: proc (reg : Component_Registry) {
  fmt.println ()
  fmt.println ("*** PALETTE ***")
  for c in reg.templates {
    fmt.println(c);
  }
  fmt.println ("***************")
  fmt.println ()
}

print_stats :: proc (reg: ^Component_Registry) {
    fmt.printf ("registry statistics: %v\n", reg.stats)
}
