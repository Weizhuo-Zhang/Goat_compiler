    call proc_main
    halt
proc_main:
    push_stack_frame 5
  # init variables
    int_const r0, 0
  # initialise variable a
    store 0, r0
    store 1, r0
    store 2, r0
    store 3, r0
    store 4, r0
  # Assign statement for variable a
    int_const r0, 1
  # Generate Array a
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable a
    int_const r0, 1
  # Generate Array a
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable a
    int_const r0, 0
  # Generate Array a
  # Generate Array a
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Generate Array a
  # Generate Array a
    int_const r0, 1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 0
    cmp_ne_int r0, r0, r1
    branch_on_true r0, main_4_a
    branch_uncond main_4_b
main_4_a:
    string_const r0, "What the?\n"
    call_builtin print_string
main_4_b:
    pop_stack_frame 5
    return
