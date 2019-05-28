    call proc_main
    halt
proc_main:
    push_stack_frame 9
  # init variables
    int_const r0, 0
  # init variable: b
    store 0, r0
    store 1, r0
    store 2, r0
    store 3, r0
  # init variable: n
    store 4, r0
  # init variable: v
    store 5, r0
    store 6, r0
    store 7, r0
  # initialise variable y
    store 8, r0
  # Assign statement for variable v
    int_const r0, 2
  # Generate Array v
    int_const r1, 1
    load_address r2, 5
    sub_offset r1, r2, r1
    store_indirect r1, r0
    string_const r0, "Provide an integer: "
    call_builtin print_string
    call_builtin read_int
    store 5, r0
  # Generate Array v
    int_const r0, 1
    load_address r1, 5
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Generate Array v
    int_const r0, 0
    load_address r1, 5
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
  # Generate Matrix b
    int_const r0, 0
    int_const r1, 2
    mul_int r0, r0, r1
    int_const r1, 0
    sub_int r0, r1, r0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    not r0, r0
    branch_on_true r0, main_8_a
    branch_uncond main_8_b
main_8_a:
  # Generate Matrix b
    int_const r0, 0
    int_const r1, 2
    mul_int r0, r0, r1
    int_const r1, 1
    sub_int r0, r1, r0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
  # Generate Matrix b
    int_const r1, 1
    int_const r2, 2
    mul_int r1, r1, r2
    int_const r2, 0
    sub_int r1, r2, r1
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    or r0, r0, r1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
main_8_b:
    pop_stack_frame 9
    return
