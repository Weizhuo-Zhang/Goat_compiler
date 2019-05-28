    call proc_main
    halt
proc_main:
    push_stack_frame 8
  # init variables
    int_const r0, 0
  # init variable: a
    store 0, r0
    store 1, r0
    store 2, r0
  # init variable: b
    store 3, r0
    store 4, r0
    store 5, r0
  # init variable: i
    store 6, r0
  # initialise variable result
    store 7, r0
  # Assign statement for variable a
    int_const r0, 5
  # Generate Array a
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable a
    int_const r0, 7
  # Generate Array a
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable a
    int_const r0, 9
  # Generate Array a
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable b
    int_const r0, 6
  # Generate Array b
    int_const r1, 0
    load_address r2, 3
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable b
    int_const r0, 8
  # Generate Array b
    int_const r1, 1
    load_address r2, 3
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable b
    int_const r0, 10
  # Generate Array b
    int_const r1, 2
    load_address r2, 3
    sub_offset r1, r2, r1
    store_indirect r1, r0
main_7_a:
    load r0, 6
    int_const r1, 3
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_7_b
    branch_uncond main_7_c
main_7_b:
  # Assign statement for variable result
    load r0, 7
  # Generate Array a
    load r1, 6
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
  # Generate Array b
    load r2, 6
    load_address r3, 3
    sub_offset r2, r3, r2
    load_indirect r2, r2
    mul_int r1, r1, r2
    add_int r0, r0, r1
    store 7, r0
  # Assign statement for variable i
    load r0, 6
    int_const r1, 1
    add_int r0, r0, r1
    store 6, r0
    branch_uncond main_7_a
main_7_c:
    load r0, 7
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 8
    return
