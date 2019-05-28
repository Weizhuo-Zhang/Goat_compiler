    call proc_main
    halt
proc_main:
    push_stack_frame 2
  # init variables
    int_const r0, 0
  # initialise variable a
    store 0, r0
    store 1, r0
  # Assign statement for variable a
    int_const r0, 42
  # Generate Array a
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable a
  # Generate Array a
    int_const r0, 1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
  # Generate Array a
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Generate Array a
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 2
    return
