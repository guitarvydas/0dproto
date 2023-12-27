package syntax

import "../../ir"

// Collects all declarations on the passed page, using the semantics outlined below.
container_decl_from_page :: proc(page: Page) -> ir.Container_Decl {
    decl: ir.Container_Decl
    decl.name = page.name

    decl.children = collect_children(page.cells)

    connections := make([dynamic]ir.Connect_Decl)
    collect_down_decls(page.cells, &connections)
    collect_across_decls(page.cells, &connections)
    collect_up_decls(page.cells, &connections)
    collect_through_decls(page.cells, &connections)
    decl.connections = connections[:]

    return decl
}

// Semantics for detecting container children:
//
// All elements that are rects, and marked as a container.
collect_children :: proc(cells: []Cell) -> []ir.Elem_Reference {
    children := make([dynamic]ir.Elem_Reference)

    for cell in cells {
        if cell.type == .Rect && .Container in cell.flags {
            ref := ir.Elem_Reference{cell.value, cell.id}
            append(&children, ref)
        }
    }

    return children[:]
}

// Semantics for detecting "Up" decls.
//
// An element with a parent, connected to a rhombus (arrow towards the rhombus)
// Where a "parent" is a "rect marked as a container"
collect_up_decls :: proc(cells: []Cell, decls: ^[dynamic]ir.Connect_Decl) {
    for cell in cells {
        if cell.type != .Arrow do continue

        decl: ir.Connect_Decl
        decl.dir = .Up

        target_rhombus := cells[cell.target]
        if target_rhombus.type != .Rhombus do continue

        // NOTE(z64): right now, i allow this to be any shape... might be ok?
        source_cell := cells[cell.source]

        decl.source_port = source_cell.value
        decl.target_port = target_rhombus.value

        parent_rect := cells[source_cell.parent]
        if !(parent_rect.type == .Rect && .Container in parent_rect.flags) {
            continue
        }

        decl.source = {parent_rect.value, parent_rect.id}

        append(decls, decl)
    }
}

// Semantics for detecting "Across" decls:
//
// An element with a parent connected to another element with a parent
// Where a "parent" is a "rect marked as a container"
collect_across_decls :: proc(cells: []Cell, decls: ^[dynamic]ir.Connect_Decl) {
    for cell in cells {
        if cell.type != .Arrow do continue

        decl: ir.Connect_Decl
        decl.dir = .Across

        source_port := cells[cell.source]
        target_port := cells[cell.target]

        decl.source_port = source_port.value
        decl.target_port = target_port.value

        source_rect := cells[source_port.parent]
        target_rect := cells[target_port.parent]
        if !(source_rect.type == .Rect && .Container in source_rect.flags) {
            continue
        }
        if !(target_rect.type == .Rect && .Container in target_rect.flags) {
            continue
        }

        decl.source = {source_rect.value, source_rect.id}
        decl.target = {target_rect.value, target_rect.id}

        append(decls, decl)
    }
}

// Semantics for detecting "Down" decls:
//
// Rhombus connected to an element that has a parent (arrow away from the rhombus)
// Where a "parent" is a "rect marked as a container"
collect_down_decls :: proc(cells: []Cell, decls: ^[dynamic]ir.Connect_Decl) {
    for cell in cells {
        if cell.type != .Arrow do continue

        decl: ir.Connect_Decl
        decl.dir = .Down

        source_rhombus := cells[cell.source]
        if source_rhombus.type != .Rhombus do continue

        // NOTE(z64): right now, i allow this to be any shape... might be ok?
        target_cell := cells[cell.target]

        decl.source_port = source_rhombus.value
        decl.target_port = target_cell.value

        parent_rect := cells[target_cell.parent]
        if parent_rect.type != .Rect && .Container in parent_rect.flags {
            continue
        }

        decl.target = {parent_rect.value, parent_rect.id}

        append(decls, decl)
    }
}

// Semantics for detecting "Through" decls:
//
// Two rhombuses connected by an arrow.
collect_through_decls :: proc(cells: []Cell, decls: ^[dynamic]ir.Connect_Decl) {
    for cell in cells {
        if cell.type != .Arrow do continue

        decl: ir.Connect_Decl
        decl.dir = .Through

        source_rhombus := cells[cell.source]
        target_rhombus := cells[cell.target]
        if source_rhombus.type != .Rhombus do continue
        if target_rhombus.type != .Rhombus do continue

        decl.source_port = source_rhombus.value
        decl.target_port = target_rhombus.value

        append(decls, decl)
    }
}
