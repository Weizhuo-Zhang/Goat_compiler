    call proc_main
    halt
proc_main:
    push_stack_frame 111
  # init variables
    int_const r0, 0
  # init variable: i
    store 0, r0
  # init variable: j
    store 1, r0
  # init variable: k
    store 2, r0
  # init variable: m
    store 3, r0
    store 4, r0
    store 5, r0
    store 6, r0
    store 7, r0
    store 8, r0
    store 9, r0
    store 10, r0
    store 11, r0
    store 12, r0
    store 13, r0
    store 14, r0
    store 15, r0
    store 16, r0
    store 17, r0
    store 18, r0
    store 19, r0
    store 20, r0
    store 21, r0
    store 22, r0
    store 23, r0
    store 24, r0
    store 25, r0
    store 26, r0
    store 27, r0
    store 28, r0
    store 29, r0
    store 30, r0
    store 31, r0
    store 32, r0
    store 33, r0
    store 34, r0
    store 35, r0
    store 36, r0
    store 37, r0
    store 38, r0
  # init variable: n
    store 39, r0
    store 40, r0
    store 41, r0
    store 42, r0
    store 43, r0
    store 44, r0
    store 45, r0
    store 46, r0
    store 47, r0
    store 48, r0
    store 49, r0
    store 50, r0
    store 51, r0
    store 52, r0
    store 53, r0
    store 54, r0
    store 55, r0
    store 56, r0
    store 57, r0
    store 58, r0
    store 59, r0
    store 60, r0
    store 61, r0
    store 62, r0
    store 63, r0
    store 64, r0
    store 65, r0
    store 66, r0
    store 67, r0
    store 68, r0
    store 69, r0
    store 70, r0
    store 71, r0
    store 72, r0
    store 73, r0
    store 74, r0
  # initialise variable r
    store 75, r0
    store 76, r0
    store 77, r0
    store 78, r0
    store 79, r0
    store 80, r0
    store 81, r0
    store 82, r0
    store 83, r0
    store 84, r0
    store 85, r0
    store 86, r0
    store 87, r0
    store 88, r0
    store 89, r0
    store 90, r0
    store 91, r0
    store 92, r0
    store 93, r0
    store 94, r0
    store 95, r0
    store 96, r0
    store 97, r0
    store 98, r0
    store 99, r0
    store 100, r0
    store 101, r0
    store 102, r0
    store 103, r0
    store 104, r0
    store 105, r0
    store 106, r0
    store 107, r0
    store 108, r0
    store 109, r0
    store 110, r0
  # Assign statement for variable k
    int_const r0, 0
    store 2, r0
  # Assign statement for variable i
    int_const r0, 0
    store 0, r0
main_3_a:
    load r0, 0
    int_const r1, 6
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_3_b
    branch_uncond main_3_c
main_3_b:
  # Assign statement for variable j
    int_const r0, 0
    store 1, r0
main_3_3_2_a:
    load r0, 1
    int_const r1, 6
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_3_3_2_b
    branch_uncond main_3_3_2_c
main_3_3_2_b:
  # Assign statement for variable k
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
  # Assign statement for variable m
    load r0, 2
  # Generate Matrix m
    load r1, 0
    int_const r2, 6
    mul_int r1, r1, r2
    load r2, 1
    sub_int r1, r2, r1
    load_address r2, 3
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable n
    load r0, 2
  # Generate Matrix n
    int_const r1, 5
    load r2, 1
    sub_int r1, r1, r2
    int_const r2, 6
    mul_int r1, r1, r2
    int_const r2, 5
    load r3, 0
    sub_int r2, r2, r3
    sub_int r1, r2, r1
    load_address r2, 39
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable j
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    branch_uncond main_3_3_2_a
main_3_3_2_c:
  # Assign statement for variable i
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond main_3_a
main_3_c:
  # Assign statement for variable i
    int_const r0, 0
    store 0, r0
main_5_a:
    load r0, 0
    int_const r1, 6
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_5_b
    branch_uncond main_5_c
main_5_b:
  # Assign statement for variable j
    int_const r0, 0
    store 1, r0
main_5_3_2_a:
    load r0, 1
    int_const r1, 6
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_5_3_2_b
    branch_uncond main_5_3_2_c
main_5_3_2_b:
  # Assign statement for variable k
    int_const r0, 0
    store 2, r0
main_5_3_2_3_2_a:
    load r0, 2
    int_const r1, 6
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_5_3_2_3_2_b
    branch_uncond main_5_3_2_3_2_c
main_5_3_2_3_2_b:
  # Assign statement for variable r
  # Generate Matrix r
    load r0, 0
    int_const r1, 6
    mul_int r0, r0, r1
    load r1, 1
    sub_int r0, r1, r0
    load_address r1, 75
    sub_offset r0, r1, r0
    load_indirect r0, r0
  # Generate Matrix m
    load r1, 0
    int_const r2, 6
    mul_int r1, r1, r2
    load r2, 2
    sub_int r1, r2, r1
    load_address r2, 3
    sub_offset r1, r2, r1
    load_indirect r1, r1
  # Generate Matrix n
    load r2, 2
    int_const r3, 6
    mul_int r2, r2, r3
    load r3, 1
    sub_int r2, r3, r2
    load_address r3, 39
    sub_offset r2, r3, r2
    load_indirect r2, r2
    mul_int r1, r1, r2
    add_int r0, r0, r1
  # Generate Matrix r
    load r1, 0
    int_const r2, 6
    mul_int r1, r1, r2
    load r2, 1
    sub_int r1, r2, r1
    load_address r2, 75
    sub_offset r1, r2, r1
    store_indirect r1, r0
  # Assign statement for variable k
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
    branch_uncond main_5_3_2_3_2_a
main_5_3_2_3_2_c:
  # Assign statement for variable j
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    branch_uncond main_5_3_2_a
main_5_3_2_c:
  # Assign statement for variable i
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond main_5_a
main_5_c:
  # Assign statement for variable i
    int_const r0, 0
    store 0, r0
main_7_a:
    load r0, 0
    int_const r1, 6
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_7_b
    branch_uncond main_7_c
main_7_b:
  # Assign statement for variable j
    int_const r0, 0
    store 1, r0
main_7_3_2_a:
    load r0, 1
    int_const r1, 6
    cmp_lt_int r0, r0, r1
    branch_on_true r0, main_7_3_2_b
    branch_uncond main_7_3_2_c
main_7_3_2_b:
  # Generate Matrix r
    load r0, 0
    int_const r1, 6
    mul_int r0, r0, r1
    load r1, 1
    sub_int r0, r1, r0
    load_address r1, 75
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "  "
    call_builtin print_string
  # Assign statement for variable j
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    branch_uncond main_7_3_2_a
main_7_3_2_c:
    string_const r0, "\n"
    call_builtin print_string
  # Assign statement for variable i
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond main_7_a
main_7_c:
    pop_stack_frame 111
    return
