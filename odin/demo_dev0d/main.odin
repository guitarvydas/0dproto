package kinopio2md

import "core:fmt"
import "core:log"
import "core:runtime"
import "core:strings"
import "core:slice"
import "core:os"
import "core:unicode/utf8"

import      "../das2json"
import reg  "../registry0d"
import zd   "../0d"
import leaf "../leaf0d"
import      "../debug"

main :: proc() {
    diagram_name, main_container_name := parse_command_line_args ()
    palette := initialize_component_palette (diagram_name)

    // set this to only track handlers in Components
    //log_level := zd.log_light_handlers // set this to only track handlers in Components
    //log_level := zd.log_full_handlers // set this to only track handlers, in full glory, in Components
    log_level := zd.log_all // set this to track everything, equivalent to runtime.Logger_Level.Debug
    // log_level := runtime.Logger_Level.Info
    fmt.printf ("\n*** starting logger level %v ***\n", log_level)
    context.logger = log.create_console_logger(
	lowest=cast(runtime.Logger_Level)log_level,
        opt={.Level, .Time, .Terminal_Color},
    )

    run (&palette, main_container_name, diagram_name, start_function)
}




start_function :: proc (main_container : ^zd.Eh) {
    b := zd.new_datum_bang ()
    msg := zd.make_message("input", b, zd.make_cause (main_container, nil) )
    main_container.handler(main_container, msg)
}


////////
project_specific_components :: proc (leaves: ^[dynamic]reg.Leaf_Template) {
    append(leaves, reg.Leaf_Template { name = "?", instantiate = leaf.probe_instantiate })
    append(leaves, reg.Leaf_Template { name = "trash", instantiate = leaf.trash_instantiate })
}


run :: proc (r : ^reg.Component_Registry, main_container_name : string, diagram_source_file : string, injectfn : #type proc (^zd.Eh)) {
    pregistry := r
    // get entrypoint container
    main_container, ok := reg.get_component_instance(pregistry, "", main_container_name, owner=nil)
    fmt.assertf(
        ok,
        "Couldn't find main container with page name %s in file %s (check tab names, or disable compression?)\n",
        main_container_name,
        diagram_source_file,
    )

    dump_hierarchy (main_container)

    injectfn (main_container)

    dump_outputs (main_container)
    dump_stats (pregistry)

    print_error_maybe (main_container)
    print_output (main_container)
    fmt.println("\n\n--- done ---")
}


print_output :: proc (main_container : ^zd.Eh) {
    fmt.println("\n\n--- RESULT ---")
    fmt.printf ("...all processes (ps) ...\n")
    zd.print_specific_output (main_container, "ps")
    fmt.printf ("...processes ...\n")
    zd.print_specific_output (main_container, "grepped")
    fmt.printf ("...number of processes ...\n")
    zd.print_specific_output (main_container, "processes")
}
print_error_maybe :: proc (main_container : ^zd.Eh) {
    error_port := "error"
    dont_care, found := zd.fetch_first_output (main_container, error_port)
    if found {
	fmt.println("\n\n--- !!! ERRORS !!! ---")
	zd.print_specific_output (main_container, error_port)
    }
}

parse_command_line_args :: proc () -> (diagram_source_file, main_container_name: string) {
    diagram_source_file = slice.get(os.args, 1) or_else "<specify on command line>"
    main_container_name = slice.get(os.args, 2) or_else "main"
    
    if !os.exists(diagram_source_file) {
        fmt.println("Source diagram file", diagram_source_file, "does not exist.")
        os.exit(1)
    }
    return diagram_source_file, main_container_name
}

initialize_component_palette :: proc (diagram_source_file: string) -> (palette: reg.Component_Registry) {
    leaves := make([dynamic]reg.Leaf_Instantiator)

    // set up shell leaves
    leaf.collect_process_leaves(diagram_source_file, &leaves)

    // export native leaves
    reg.append_leaf (&leaves, reg.Leaf_Instantiator {
        name = "stdout",
        instantiate = leaf.stdout_instantiate,
    })

    project_specific_components (&leaves)

    das2json.drawio2json (diagram_source_file)
    containers := reg.json2internal (diagram_source_file)
    palette = reg.make_component_registry(leaves[:], containers)
    return palette
}


// debugging helpers

dump_hierarchy :: proc (main_container : ^zd.Eh) {
    fmt.println("\n\n--- Hierarchy ---")
    debug.log_hierarchy (main_container)
}

dump_outputs :: proc (main_container : ^zd.Eh) {
    fmt.println("\n\n--- Outputs ---")
    zd.print_output_list(main_container)
}

dump_stats :: proc (pregistry : ^reg.Component_Registry) {
    reg.print_stats (pregistry)
}

