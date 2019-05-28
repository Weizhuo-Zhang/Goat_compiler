    call proc_main
    halt
proc_main:
    push_stack_frame 2
  # init variables
    int_const r0, 0
  # init variable: x
    store 0, r0
  # initialise variable y
    store 1, r0
    string_const r0, "Integer, please: "
    call_builtin print_string
    call_builtin read_int
    store 0, r0
  # Assign statement for variable y
    load r0, 0
    int_to_real r0, r0
    store 1, r0
    load r0, 0
    load r1, 1
    int_to_real r0, r0
    add_real r0, r0, r1
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 2
    return
