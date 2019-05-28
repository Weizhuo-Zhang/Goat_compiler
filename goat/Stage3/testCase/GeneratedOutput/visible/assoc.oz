    call proc_main
    halt
proc_main:
    push_stack_frame 1
  # init variables
    int_const r0, 0
  # initialise variable n
    store 0, r0
  # Assign statement for variable n
    int_const r0, 4
    int_const r1, 7
    add_int r0, r0, r1
    int_const r1, 9
    neg_int r1, r1
    int_const r2, 1
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    int_const r1, 6
    sub_int r0, r0, r1
    store 0, r0
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 1
    return
