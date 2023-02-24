INCLUDE ../test/assert.p4

.( TIME&DATE MS ) test_group
t{ : tw_secs TIME&DATE DROP DROP DROP DROP 60 * + ; -> }t
t{ tw_secs 1000 MS tw_secs SWAP - 1 3 WITHIN -> TRUE }t
test_group_end

[DEFINED] epoch-seconds [IF]
.( epoch-seconds MS ) test_group
t{ epoch-seconds 1000 MS epoch-seconds SWAP - 1 3 WITHIN -> TRUE }t
test_group_end
[THEN]

.( +FIELD ) test_group
t{ 0 0 +FIELD tv_field_0 CONSTANT tv_struct_0 -> }t
t{ tv_struct_0 -> 0 }t
t{ 0 tv_field_0 -> 0 }t
t{ 0 3 +FIELD tv_field_a CONSTANT tv_struct_1 -> }t
t{ tv_struct_1 0 tv_field_a  -> 3 0 }t
t{ 0 1 +FIELD tv_field_b 2 +FIELD tv_field_c 3 +FIELD tv_field_d CONSTANT tv_struct_2 -> }t
t{ tv_struct_2 0 tv_field_b 0 tv_field_c 0 tv_field_d -> 6 0 1 3 }t
test_group_end

.( FIELD: ) test_group
t{ 0 FIELD: tv_field_0 CONSTANT tv_struct_0 -> }t
t{ tv_struct_0 -> 1 CELLS }t
t{ 0 tv_field_0 -> 0 }t
t{ 0 FIELD: tv_field_a CONSTANT tv_struct_1 -> }t
t{ tv_struct_1 0 tv_field_a  -> 1 CELLS 0 }t
t{ 0 FIELD: tv_field_b FIELD: tv_field_c CONSTANT tv_struct_2 -> }t
t{ tv_struct_2 0 tv_field_b 0 tv_field_c -> 2 CELLS 0 1 CELLS }t
t{ 0 1 +FIELD tv_field_d FIELD: tv_field_e CONSTANT tv_struct_3 -> }t
t{ tv_struct_3 0 tv_field_d 0 tv_field_e -> 2 CELLS 0 1 CELLS }t
test_group_end

.( CFIELD: ) test_group
t{ 0 CFIELD: tv_field_0 CONSTANT tv_struct_0 -> }t
t{ tv_struct_0 -> 1 CHARS }t
t{ 0 tv_field_0 -> 0 }t
t{ 0 CFIELD: tv_field_a CFIELD: tv_field_b CFIELD: tv_field_c CONSTANT tv_struct_1 -> }t
t{ tv_struct_1 0 tv_field_a 0 tv_field_b 0 tv_field_c -> 3 CHARS 0 1 CHARS 2 CHARS }t
t{ 0 CFIELD: tv_field_d FIELD: tv_field_e CFIELD: tv_field_f CONSTANT tv_struct_2 -> }t
t{ tv_struct_2 0 tv_field_d 0 tv_field_e 0 tv_field_f -> 2 CELLS CHAR+ 0 1 CELLS 2 CELLS }t
test_group_end

.( BEGIN-STRUCTURE END-STRUCTURE ) test_group
t{ BEGIN-STRUCTURE tv_struct_0 END-STRUCTURE -> }t
t{ tv_struct_0 -> 0 }t
t{ BEGIN-STRUCTURE tv_struct_1 FIELD: tv_field_a FIELD: tv_field_b END-STRUCTURE -> }t
t{ tv_struct_1 0 tv_field_a 0 tv_field_b -> 2 CELLS 0 1 CELLS }t
test_group_end
