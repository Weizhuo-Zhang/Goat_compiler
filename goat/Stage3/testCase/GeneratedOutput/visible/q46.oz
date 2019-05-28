    call proc_main
    halt
proc_f:
    push_stack_frame 1
  # init parameters
  # initialise parameters x
    store 0, r0
  # Assign statement for variable x
    load r0, 0
    int_const r1, 1
    int_to_real r1, r1
    add_real r0, r0, r1
    store 0, r0
    load r0, 0
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 1
    return
proc_main:
    push_stack_frame 1
  # init variables
    int_const r0, 0
  # initialise variable n
    store 0, r0
  # Assign statement for variable n
    int_const r0, 41
    int_to_real r0, r0
    store 0, r0
    load r0, 0
    load r0, 0
    call proc_f
    load r0, 0
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 1
    return
