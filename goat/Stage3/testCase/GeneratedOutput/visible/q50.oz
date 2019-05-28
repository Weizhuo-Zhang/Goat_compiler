    call proc_main
    halt
proc_incr:
    push_stack_frame 1
  # init parameters
  # initialise parameters n
    store 0, r0
  # Assign statement for variable n
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    pop_stack_frame 1
    return
proc_main:
    push_stack_frame 1
  # init variables
    int_const r0, 0
  # initialise variable k
    store 0, r0
  # Assign statement for variable k
    int_const r0, 41
    store 0, r0
    load r0, 0
    load_address r0, 0
    call proc_incr
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 1
    return
