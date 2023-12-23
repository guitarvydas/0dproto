package lib
import "core:os"
import "core:fmt"
import "core:slice"

import reg  "../registry0d"
import zd   "../0d"

parse_command_line_args :: proc (default_diagram_source_file, default_main_container_name : string) -> (diagram_source_file, main_container_name: string) {
    diagram_source_file = slice.get(os.args, 1) or_else default_diagram_source_file
    main_container_name = slice.get(os.args, 2) or_else default_main_container_name
    
    if !os.exists(diagram_source_file) {
        fmt.println("Source diagram file", diagram_source_file, "does not exist.")
        os.exit(1)
    }
    return diagram_source_file, main_container_name
}

run_demo :: proc (r : ^reg.Component_Registry, main_container_name : string, diagram_source_file : string, injectfn : #type proc (^zd.Eh)) {
    pregistry := r
    // get entrypoint container
    main_container, ok := reg.get_component_instance(pregistry, "", main_container_name, owner=nil)
    fmt.assertf(
        ok,
        "Couldn't find main container with page name %s in file %s (check tab names, or disable compression?)\n",
        main_container_name,
        diagram_source_file,
    )
    injectfn (main_container)
    print_error_maybe (main_container)
    print_output (main_container)
    fmt.println("\n\n--- done ---")
}


print_output :: proc (main_container : ^zd.Eh) {
    fmt.println("\n\n--- RESULT ---")
    fmt.printf ("... response ... \n")
    zd.print_specific_output (main_container, "output")
}
print_error_maybe :: proc (main_container : ^zd.Eh) {
    error_port := "error"
    dont_care, found := zd.fetch_first_output (main_container, error_port)
    if found {
	fmt.println("\n\n--- !!! ERRORS !!! ---")
	zd.print_specific_output (main_container, error_port)
    }
}


initialize_component_palette :: proc (diagram_source_file: string, project_specific_components : #type proc (^[dynamic]reg.Leaf_Template)) -> (palette: reg.Component_Registry) {
    leaves := make([dynamic]reg.Leaf_Instantiator)

    // set up shell leaves
    collect_process_leaves(diagram_source_file, &leaves)

    // export native leaves
    reg.append_leaf (&leaves, reg.Leaf_Instantiator {
        name = "stdout",
        instantiate = stdout_instantiate,
    })

    project_specific_components (&leaves)

    containers := reg.json2internal (diagram_source_file)
    palette = reg.make_component_registry(leaves[:], containers)
    return palette
}


