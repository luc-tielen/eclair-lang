declare external ccc ptr @malloc(i32)

declare external ccc void @free(ptr)

declare external ccc void @llvm.memset.p0i8.i64(ptr, i8, i64, i1)

declare external ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr, ptr, i64, i1)

declare external ccc i32 @memcmp(ptr, ptr, i64)

declare external ccc ptr @mmap(ptr, i64, i32, i32, i32, i32)

declare external ccc i32 @munmap(ptr, i64)

%node_data_t_0 = type {ptr, i16, i16, i1}

%node_t_0 = type {%node_data_t_0, [20 x [3 x i32]]}

%inner_node_t_0 = type {%node_t_0, [21 x ptr]}

%btree_iterator_t_0 = type {ptr, i16}

%btree_t_0 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_0(i32 %lhs_0, i32 %rhs_0) {
start:
  %0 = icmp ult i32 %lhs_0, %rhs_0
  br i1 %0, label %if_0, label %end_if_0
if_0:
  ret i8 -1
end_if_0:
  %1 = icmp ugt i32 %lhs_0, %rhs_0
  %2 = select i1 %1, i8 1, i8 0
  ret i8 %2
}

define external ccc i8 @eclair_btree_value_compare_values_0(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [3 x i32], ptr %lhs_0, i32 0, i32 0
  %1 = getelementptr [3 x i32], ptr %rhs_0, i32 0, i32 0
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_0(i32 %2, i32 %3)
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %comparison_2, label %end_0
comparison_1:
  %6 = getelementptr [3 x i32], ptr %lhs_0, i32 0, i32 1
  %7 = getelementptr [3 x i32], ptr %rhs_0, i32 0, i32 1
  %8 = load i32, ptr %6
  %9 = load i32, ptr %7
  %10 = call ccc i8 @eclair_btree_value_compare_0(i32 %8, i32 %9)
  %11 = icmp eq i8 %10, 0
  br i1 %11, label %comparison_2, label %end_0
comparison_2:
  %12 = getelementptr [3 x i32], ptr %lhs_0, i32 0, i32 2
  %13 = getelementptr [3 x i32], ptr %rhs_0, i32 0, i32 2
  %14 = load i32, ptr %12
  %15 = load i32, ptr %13
  %16 = call ccc i8 @eclair_btree_value_compare_0(i32 %14, i32 %15)
  br label %end_0
end_0:
  %17 = phi i8 [%4, %comparison_0], [%10, %comparison_1], [%16, %comparison_2]
  ret i8 %17
}

define external ccc ptr @eclair_btree_node_new_0(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 424, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_0, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_0, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_0, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_0, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_0, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_0, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 168, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_0(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_0(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_0(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_0(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_0(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_0(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_0, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_0, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_0, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_0, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_0(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_0, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_0(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_0, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_0, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_0, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_0, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_0, ptr %19, i32 0, i32 0, i32 2
  %23 = load i16, ptr %22
  %24 = icmp ne i16 %23, 0
  br i1 %24, label %if_1, label %end_if_0
if_1:
  ret void
end_if_0:
  br label %leaf.next_0
end_if_1:
  br label %leaf.next_0
leaf.next_0:
  %25 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_0, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_0, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_0, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_0, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_0, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_0(ptr %val_0, ptr %current_0, ptr %end_0) {
start:
  %stack.ptr_0 = alloca ptr
  store ptr %current_0, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %0 = load ptr, ptr %stack.ptr_0
  %1 = icmp ne ptr %0, %end_0
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = load ptr, ptr %stack.ptr_0
  %3 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %2, ptr %val_0)
  %4 = icmp ne i8 %3, -1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [3 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc ptr @eclair_btree_linear_search_upper_bound_0(ptr %val_0, ptr %current_0, ptr %end_0) {
start:
  %stack.ptr_0 = alloca ptr
  store ptr %current_0, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %0 = load ptr, ptr %stack.ptr_0
  %1 = icmp ne ptr %0, %end_0
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = load ptr, ptr %stack.ptr_0
  %3 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %2, ptr %val_0)
  %4 = icmp eq i8 %3, 1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [3 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc void @eclair_btree_init_empty_0(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_0(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__0(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_0(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_0(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_0(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_0(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_0(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_0() {
start:
  %0 = mul i16 3, 20
  %1 = udiv i16 %0, 4
  %2 = sub i16 20, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_0(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_0()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_0(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 20
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [3 x i32], ptr %8
  %10 = getelementptr %node_t_0, ptr %4, i32 0, i32 1, i16 %7
  store [3 x i32] %9, ptr %10
  %11 = add i16 1, %7
  store i16 %11, ptr %stack.ptr_0
  %12 = add i16 1, %5
  br label %for_begin_0
for_end_0:
  %13 = icmp eq i1 %3, 1
  br i1 %13, label %if_0, label %end_if_0
if_0:
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_1
for_begin_1:
  %14 = phi i16 [%1, %if_0], [%23, %for_body_1]
  %15 = icmp ule i16 %14, 20
  br i1 %15, label %for_body_1, label %for_end_1
for_body_1:
  %16 = load i16, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_0, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_0, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_0, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 20, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_0, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_0(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_0(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_0(i1 1)
  %6 = getelementptr %node_t_0, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [3 x i32], ptr %7
  %9 = getelementptr %node_t_0, ptr %5, i32 0, i32 1, i16 0
  store [3 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_0, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_0, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_0, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_0, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_0(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_0(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 20
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_0(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_0, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_0(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp sge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [3 x i32], ptr %26
  %28 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %24
  store [3 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_0, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [3 x i32], ptr %key_0
  %39 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %21
  store [3 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_0, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_0, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_0(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_0, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 20, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_0, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [3 x i32], ptr %19
  %21 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_0, ptr %9, i32 0, i32 1, i16 %22
  store [3 x i32] %20, ptr %23
  %24 = sub i16 %14, 1
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [0, %if_0], [%32, %for_body_0]
  %26 = icmp ult i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %22, 1
  %28 = add i16 %25, %27
  %29 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [3 x i32], ptr %29
  %31 = getelementptr %node_t_0, ptr %9, i32 0, i32 1, i16 %28
  store [3 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [3 x i32], ptr %33
  store [3 x i32] %34, ptr %19
  %35 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [3 x i32], ptr %41
  %43 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 1, i16 %38
  store [3 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_0, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_0, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_0, ptr %65, i32 0, i32 0, i32 1
  store i16 %63, ptr %67
  %68 = add i16 1, %58
  br label %for_begin_3
for_end_3:
  %69 = sub i16 %36, %14
  %70 = add i16 1, %69
  br label %for_begin_4
for_begin_4:
  %71 = phi i16 [0, %for_end_3], [%80, %for_body_4]
  %72 = icmp ult i16 %71, %70
  br i1 %72, label %for_body_4, label %for_end_4
for_body_4:
  %73 = add i16 %71, %14
  %74 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_0, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_0(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_0(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_0(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_0(i1 0)
  %2 = getelementptr %node_t_0, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [3 x i32], ptr %val_0
  %4 = getelementptr %node_t_0, ptr %1, i32 0, i32 1, i16 0
  store [3 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_0, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_0, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_0(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 12
  %23 = icmp ne ptr %17, %16
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_0, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_0, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_0, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_0(ptr %val_0, ptr %30, ptr %31)
  %33 = ptrtoint ptr %32 to i64
  %34 = ptrtoint ptr %30 to i64
  %35 = sub i64 %33, %34
  %36 = trunc i64 %35 to i16
  %37 = udiv i16 %36, 12
  store i16 %37, ptr %stack.ptr_1
  %38 = icmp ne ptr %32, %30
  br i1 %38, label %if_1, label %end_if_1
if_1:
  %39 = getelementptr [3 x i32], ptr %32, i32 -1
  %40 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 20
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_0(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_0, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_0, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_0, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [3 x i32], ptr %66
  %68 = getelementptr %node_t_0, ptr %59, i32 0, i32 1, i16 %63
  store [3 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [3 x i32], ptr %val_0
  %71 = getelementptr %node_t_0, ptr %59, i32 0, i32 1, i16 %60
  store [3 x i32] %70, ptr %71
  %72 = getelementptr %node_t_0, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__0(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_0(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_0(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_0(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_0(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_0, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_0, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_0(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_0(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_0(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_0, i32 1
  call ccc void @eclair_btree_find_0(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_0(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_0(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_0(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_0(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_0, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_0, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_0, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_0(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 12
  %14 = icmp ult ptr %8, %7
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_0(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_0, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_0(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_0, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_0(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_0(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_0(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_0(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_0, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_0, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_0, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_0(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 12
  %14 = getelementptr %node_t_0, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_0, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_0, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_0, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_0, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_0(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_0(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_0(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_0, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_0(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_0(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_0(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_0(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_0, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_0, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_0, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_0(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 12
  %14 = getelementptr %node_t_0, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_0, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_0, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_0, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_0, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_0(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_0(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_0, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_0(ptr %node_0) {
start:
  %0 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_0(ptr %8)
  br label %end_if_0
end_if_0:
  %10 = add i16 1, %5
  br label %for_begin_0
for_end_0:
  br label %end_if_1
end_if_1:
  call ccc void @free(ptr %node_0)
  ret void
}

define external ccc void @eclair_btree_clear_0(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_0(ptr %1)
  %3 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_0(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_0, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_0, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_0, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_0, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_0, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_0, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_0, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_0, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

%node_data_t_1 = type {ptr, i16, i16, i1}

%node_t_1 = type {%node_data_t_1, [30 x [2 x i32]]}

%inner_node_t_1 = type {%node_t_1, [31 x ptr]}

%btree_iterator_t_1 = type {ptr, i16}

%btree_t_1 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_1(i32 %lhs_0, i32 %rhs_0) {
start:
  %0 = icmp ult i32 %lhs_0, %rhs_0
  br i1 %0, label %if_0, label %end_if_0
if_0:
  ret i8 -1
end_if_0:
  %1 = icmp ugt i32 %lhs_0, %rhs_0
  %2 = select i1 %1, i8 1, i8 0
  ret i8 %2
}

define external ccc i8 @eclair_btree_value_compare_values_1(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [2 x i32], ptr %lhs_0, i32 0, i32 1
  %1 = getelementptr [2 x i32], ptr %rhs_0, i32 0, i32 1
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_1(i32 %2, i32 %3)
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %comparison_1, label %end_0
comparison_1:
  %6 = getelementptr [2 x i32], ptr %lhs_0, i32 0, i32 0
  %7 = getelementptr [2 x i32], ptr %rhs_0, i32 0, i32 0
  %8 = load i32, ptr %6
  %9 = load i32, ptr %7
  %10 = call ccc i8 @eclair_btree_value_compare_1(i32 %8, i32 %9)
  br label %end_0
end_0:
  %11 = phi i8 [%4, %comparison_0], [%10, %comparison_1]
  ret i8 %11
}

define external ccc ptr @eclair_btree_node_new_1(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 504, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_1, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_1, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_1, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_1, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_1, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_1, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 248, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_1(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_1(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_1(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_1(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_1(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_1(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_1, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_1, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_1, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_1, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_1(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_1, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_1(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_1, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_1, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_1, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_1, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_1, ptr %19, i32 0, i32 0, i32 2
  %23 = load i16, ptr %22
  %24 = icmp ne i16 %23, 0
  br i1 %24, label %if_1, label %end_if_0
if_1:
  ret void
end_if_0:
  br label %leaf.next_0
end_if_1:
  br label %leaf.next_0
leaf.next_0:
  %25 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_1, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_1, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_1, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_1, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_1, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_1(ptr %val_0, ptr %current_0, ptr %end_0) {
start:
  %stack.ptr_0 = alloca ptr
  store ptr %current_0, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %0 = load ptr, ptr %stack.ptr_0
  %1 = icmp ne ptr %0, %end_0
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = load ptr, ptr %stack.ptr_0
  %3 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %2, ptr %val_0)
  %4 = icmp ne i8 %3, -1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [2 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc ptr @eclair_btree_linear_search_upper_bound_1(ptr %val_0, ptr %current_0, ptr %end_0) {
start:
  %stack.ptr_0 = alloca ptr
  store ptr %current_0, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %0 = load ptr, ptr %stack.ptr_0
  %1 = icmp ne ptr %0, %end_0
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = load ptr, ptr %stack.ptr_0
  %3 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %2, ptr %val_0)
  %4 = icmp eq i8 %3, 1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [2 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc void @eclair_btree_init_empty_1(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_1(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__1(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_1(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_1(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_1(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_1(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_1(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_1() {
start:
  %0 = mul i16 3, 30
  %1 = udiv i16 %0, 4
  %2 = sub i16 30, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_1(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_1()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_1(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 30
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [2 x i32], ptr %8
  %10 = getelementptr %node_t_1, ptr %4, i32 0, i32 1, i16 %7
  store [2 x i32] %9, ptr %10
  %11 = add i16 1, %7
  store i16 %11, ptr %stack.ptr_0
  %12 = add i16 1, %5
  br label %for_begin_0
for_end_0:
  %13 = icmp eq i1 %3, 1
  br i1 %13, label %if_0, label %end_if_0
if_0:
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_1
for_begin_1:
  %14 = phi i16 [%1, %if_0], [%23, %for_body_1]
  %15 = icmp ule i16 %14, 30
  br i1 %15, label %for_body_1, label %for_end_1
for_body_1:
  %16 = load i16, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_1, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_1, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_1, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 30, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_1, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_1(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_1(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_1(i1 1)
  %6 = getelementptr %node_t_1, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [2 x i32], ptr %7
  %9 = getelementptr %node_t_1, ptr %5, i32 0, i32 1, i16 0
  store [2 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_1, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_1, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_1, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_1, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_1(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_1(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 30
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_1(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_1, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_1(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp sge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [2 x i32], ptr %26
  %28 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %24
  store [2 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_1, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [2 x i32], ptr %key_0
  %39 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %21
  store [2 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_1, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_1, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_1(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_1, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 30, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_1, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [2 x i32], ptr %19
  %21 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_1, ptr %9, i32 0, i32 1, i16 %22
  store [2 x i32] %20, ptr %23
  %24 = sub i16 %14, 1
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [0, %if_0], [%32, %for_body_0]
  %26 = icmp ult i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %22, 1
  %28 = add i16 %25, %27
  %29 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [2 x i32], ptr %29
  %31 = getelementptr %node_t_1, ptr %9, i32 0, i32 1, i16 %28
  store [2 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [2 x i32], ptr %33
  store [2 x i32] %34, ptr %19
  %35 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [2 x i32], ptr %41
  %43 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 1, i16 %38
  store [2 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_1, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_1, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_1, ptr %65, i32 0, i32 0, i32 1
  store i16 %63, ptr %67
  %68 = add i16 1, %58
  br label %for_begin_3
for_end_3:
  %69 = sub i16 %36, %14
  %70 = add i16 1, %69
  br label %for_begin_4
for_begin_4:
  %71 = phi i16 [0, %for_end_3], [%80, %for_body_4]
  %72 = icmp ult i16 %71, %70
  br i1 %72, label %for_body_4, label %for_end_4
for_body_4:
  %73 = add i16 %71, %14
  %74 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_1, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_1(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_1(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_1(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_1(i1 0)
  %2 = getelementptr %node_t_1, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [2 x i32], ptr %val_0
  %4 = getelementptr %node_t_1, ptr %1, i32 0, i32 1, i16 0
  store [2 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_1, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_1, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_1(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 8
  %23 = icmp ne ptr %17, %16
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_1, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_1, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_1, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_1(ptr %val_0, ptr %30, ptr %31)
  %33 = ptrtoint ptr %32 to i64
  %34 = ptrtoint ptr %30 to i64
  %35 = sub i64 %33, %34
  %36 = trunc i64 %35 to i16
  %37 = udiv i16 %36, 8
  store i16 %37, ptr %stack.ptr_1
  %38 = icmp ne ptr %32, %30
  br i1 %38, label %if_1, label %end_if_1
if_1:
  %39 = getelementptr [2 x i32], ptr %32, i32 -1
  %40 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 30
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_1(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_1, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_1, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_1, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [2 x i32], ptr %66
  %68 = getelementptr %node_t_1, ptr %59, i32 0, i32 1, i16 %63
  store [2 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [2 x i32], ptr %val_0
  %71 = getelementptr %node_t_1, ptr %59, i32 0, i32 1, i16 %60
  store [2 x i32] %70, ptr %71
  %72 = getelementptr %node_t_1, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__1(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_1(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_1(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_1(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_1(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_1, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_1, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_1(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_1(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_1(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_1, i32 1
  call ccc void @eclair_btree_find_1(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_1(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_1(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_1(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_1(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_1, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_1, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_1, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_1(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = icmp ult ptr %8, %7
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_1(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_1, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_1(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_1, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_1(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_1(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_1(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_1(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_1, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_1, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_1, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_1(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = getelementptr %node_t_1, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_1, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_1, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_1, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_1, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_1(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_1(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_1(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_1, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_1(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_1(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_1(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_1(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_1, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_1, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_1, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_1(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = getelementptr %node_t_1, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_1, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_1, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_1, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_1, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_1(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_1(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_1, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_1(ptr %node_0) {
start:
  %0 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_1(ptr %8)
  br label %end_if_0
end_if_0:
  %10 = add i16 1, %5
  br label %for_begin_0
for_end_0:
  br label %end_if_1
end_if_1:
  call ccc void @free(ptr %node_0)
  ret void
}

define external ccc void @eclair_btree_clear_1(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_1(ptr %1)
  %3 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_1(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_1, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_1, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_1, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_1, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_1, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_1, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_1, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_1, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

@specialize_debug_info.btree__2__1_0__256__linear = global i32 1

@specialize_debug_info.btree__3__0_1_2__256__linear = global i32 0

%symbol_t = type <{i32, ptr}>

define external ccc void @eclair_symbol_init(ptr %symbol_0, i32 %size_0, ptr %data_0) {
start:
  %0 = getelementptr %symbol_t, ptr %symbol_0, i32 0, i32 0
  store i32 %size_0, ptr %0
  %1 = getelementptr %symbol_t, ptr %symbol_0, i32 0, i32 1
  store ptr %data_0, ptr %1
  ret void
}

define external ccc void @eclair_symbol_destroy(ptr %symbol_0) {
start:
  %0 = getelementptr %symbol_t, ptr %symbol_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  call ccc void @free(ptr %1)
  ret void
}

define external ccc i1 @eclair_symbol_is_equal(ptr %symbol1_0, ptr %symbol2_0) {
start:
  %0 = getelementptr %symbol_t, ptr %symbol1_0, i32 0, i32 0
  %1 = load i32, ptr %0
  %2 = getelementptr %symbol_t, ptr %symbol2_0, i32 0, i32 0
  %3 = load i32, ptr %2
  %4 = icmp ne i32 %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %symbol_t, ptr %symbol1_0, i32 0, i32 1
  %6 = load ptr, ptr %5
  %7 = getelementptr %symbol_t, ptr %symbol2_0, i32 0, i32 1
  %8 = load ptr, ptr %7
  %9 = zext i32 %1 to i64
  %10 = call ccc i32 @memcmp(ptr %6, ptr %8, i64 %9)
  %11 = icmp eq i32 %10, 0
  ret i1 %11
}

%vector_t_symbol = type {ptr, ptr, i32}

define external ccc void @eclair_vector_init_symbol(ptr %vec_0) {
start:
  %0 = call ccc ptr @malloc(i32 192)
  %1 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 0
  store ptr %0, ptr %1
  %2 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 1
  store ptr %0, ptr %2
  %3 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 2
  store i32 16, ptr %3
  ret void
}

define external ccc void @eclair_vector_destroy_symbol(ptr %vec_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  store ptr %1, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %2 = load ptr, ptr %stack.ptr_0
  %3 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 1
  %4 = load ptr, ptr %3
  %5 = icmp ne ptr %2, %4
  br i1 %5, label %while_body_0, label %while_end_0
while_body_0:
  %6 = load ptr, ptr %stack.ptr_0
  call ccc void @eclair_symbol_destroy(ptr %6)
  %7 = getelementptr %symbol_t, ptr %6, i32 1
  store ptr %7, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %8 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  call ccc void @free(ptr %9)
  ret void
}

define external ccc i32 @eclair_vector_size_symbol(ptr %vec_0) {
start:
  %0 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 1
  %3 = load ptr, ptr %2
  %4 = ptrtoint ptr %3 to i64
  %5 = ptrtoint ptr %1 to i64
  %6 = sub i64 %4, %5
  %7 = trunc i64 %6 to i32
  %8 = udiv i32 %7, 12
  ret i32 %8
}

define external ccc void @eclair_vector_grow_symbol(ptr %vec_0) {
start:
  %0 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 2
  %1 = load i32, ptr %0
  %2 = mul i32 %1, 12
  %3 = zext i32 %2 to i64
  %4 = mul i32 %1, 2
  %5 = mul i32 %4, 12
  %6 = call ccc ptr @malloc(i32 %5)
  %7 = getelementptr %symbol_t, ptr %6, i32 %1
  %8 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %6, ptr %9, i64 %3, i1 0)
  call ccc void @free(ptr %9)
  %10 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 0
  store ptr %6, ptr %10
  %11 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 1
  store ptr %7, ptr %11
  %12 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 2
  store i32 %4, ptr %12
  ret void
}

define external ccc i32 @eclair_vector_push_symbol(ptr %vec_0, ptr %elem_0) {
start:
  %0 = call ccc i32 @eclair_vector_size_symbol(ptr %vec_0)
  %1 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 2
  %2 = load i32, ptr %1
  %3 = icmp eq i32 %0, %2
  br i1 %3, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_vector_grow_symbol(ptr %vec_0)
  br label %end_if_0
end_if_0:
  %4 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 1
  %5 = load ptr, ptr %4
  %6 = load %symbol_t, ptr %elem_0
  store %symbol_t %6, ptr %5
  %7 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 1
  %8 = load ptr, ptr %7
  %9 = getelementptr %symbol_t, ptr %8, i32 1
  store ptr %9, ptr %7
  ret i32 %0
}

define external ccc ptr @eclair_vector_get_value_symbol(ptr %vec_0, i32 %idx_0) {
start:
  %0 = getelementptr %vector_t_symbol, ptr %vec_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %symbol_t, ptr %1, i32 %idx_0
  ret ptr %2
}

%entry_t = type {%symbol_t, i32}

%vector_t_entry = type {ptr, ptr, i32}

define external ccc void @eclair_vector_init_entry(ptr %vec_0) {
start:
  %0 = call ccc ptr @malloc(i32 256)
  %1 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 0
  store ptr %0, ptr %1
  %2 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 1
  store ptr %0, ptr %2
  %3 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 2
  store i32 16, ptr %3
  ret void
}

define external ccc void @eclair_vector_destroy_entry(ptr %vec_0) {
start:
  %0 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  call ccc void @free(ptr %1)
  ret void
}

define external ccc i32 @eclair_vector_size_entry(ptr %vec_0) {
start:
  %0 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 1
  %3 = load ptr, ptr %2
  %4 = ptrtoint ptr %3 to i64
  %5 = ptrtoint ptr %1 to i64
  %6 = sub i64 %4, %5
  %7 = trunc i64 %6 to i32
  %8 = udiv i32 %7, 16
  ret i32 %8
}

define external ccc void @eclair_vector_grow_entry(ptr %vec_0) {
start:
  %0 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 2
  %1 = load i32, ptr %0
  %2 = mul i32 %1, 16
  %3 = zext i32 %2 to i64
  %4 = mul i32 %1, 2
  %5 = mul i32 %4, 16
  %6 = call ccc ptr @malloc(i32 %5)
  %7 = getelementptr %entry_t, ptr %6, i32 %1
  %8 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %6, ptr %9, i64 %3, i1 0)
  call ccc void @free(ptr %9)
  %10 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 0
  store ptr %6, ptr %10
  %11 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 1
  store ptr %7, ptr %11
  %12 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 2
  store i32 %4, ptr %12
  ret void
}

define external ccc i32 @eclair_vector_push_entry(ptr %vec_0, ptr %elem_0) {
start:
  %0 = call ccc i32 @eclair_vector_size_entry(ptr %vec_0)
  %1 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 2
  %2 = load i32, ptr %1
  %3 = icmp eq i32 %0, %2
  br i1 %3, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_vector_grow_entry(ptr %vec_0)
  br label %end_if_0
end_if_0:
  %4 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 1
  %5 = load ptr, ptr %4
  %6 = load %entry_t, ptr %elem_0
  store %entry_t %6, ptr %5
  %7 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 1
  %8 = load ptr, ptr %7
  %9 = getelementptr %entry_t, ptr %8, i32 1
  store ptr %9, ptr %7
  ret i32 %0
}

define external ccc ptr @eclair_vector_get_value_entry(ptr %vec_0, i32 %idx_0) {
start:
  %0 = getelementptr %vector_t_entry, ptr %vec_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %entry_t, ptr %1, i32 %idx_0
  ret ptr %2
}

%hashmap_t = type {[64 x %vector_t_entry]}

define external ccc i32 @eclair_symbol_hash(ptr %symbol_0) {
start:
  %stack.ptr_0 = alloca i32
  store i32 0, ptr %stack.ptr_0
  %0 = getelementptr %symbol_t, ptr %symbol_0, i32 0, i32 0
  %1 = load i32, ptr %0
  %2 = getelementptr %symbol_t, ptr %symbol_0, i32 0, i32 1
  %3 = load ptr, ptr %2
  br label %for_begin_0
for_begin_0:
  %4 = phi i32 [0, %start], [%12, %for_body_0]
  %5 = icmp ult i32 %4, %1
  br i1 %5, label %for_body_0, label %for_end_0
for_body_0:
  %6 = getelementptr i8, ptr %3, i32 %4
  %7 = load i8, ptr %6
  %8 = zext i8 %7 to i32
  %9 = load i32, ptr %stack.ptr_0
  %10 = mul i32 31, %9
  %11 = add i32 %8, %10
  store i32 %11, ptr %stack.ptr_0
  %12 = add i32 1, %4
  br label %for_begin_0
for_end_0:
  %13 = load i32, ptr %stack.ptr_0
  %14 = and i32 %13, 63
  ret i32 %14
}

define external ccc void @eclair_hashmap_init(ptr %hashmap_0) {
start:
  br label %for_begin_0
for_begin_0:
  %0 = phi i32 [0, %start], [%3, %for_body_0]
  %1 = icmp ult i32 %0, 64
  br i1 %1, label %for_body_0, label %for_end_0
for_body_0:
  %2 = getelementptr %hashmap_t, ptr %hashmap_0, i32 0, i32 0, i32 %0
  call ccc void @eclair_vector_init_entry(ptr %2)
  %3 = add i32 1, %0
  br label %for_begin_0
for_end_0:
  ret void
}

define external ccc void @eclair_hashmap_destroy(ptr %hashmap_0) {
start:
  br label %for_begin_0
for_begin_0:
  %0 = phi i32 [0, %start], [%3, %for_body_0]
  %1 = icmp ult i32 %0, 64
  br i1 %1, label %for_body_0, label %for_end_0
for_body_0:
  %2 = getelementptr %hashmap_t, ptr %hashmap_0, i32 0, i32 0, i32 %0
  call ccc void @eclair_vector_destroy_entry(ptr %2)
  %3 = add i32 1, %0
  br label %for_begin_0
for_end_0:
  ret void
}

define external ccc i32 @eclair_hashmap_get_or_put_value(ptr %hashmap_0, ptr %symbol_0, i32 %value_0) {
start:
  %stack.ptr_0 = alloca %entry_t
  %0 = call ccc i32 @eclair_symbol_hash(ptr %symbol_0)
  %1 = and i32 %0, 63
  %2 = getelementptr %hashmap_t, ptr %hashmap_0, i32 0, i32 0, i32 %1
  %3 = call ccc i32 @eclair_vector_size_entry(ptr %2)
  br label %for_begin_0
for_begin_0:
  %4 = phi i32 [0, %start], [%11, %end_if_0]
  %5 = icmp ult i32 %4, %3
  br i1 %5, label %for_body_0, label %for_end_0
for_body_0:
  %6 = call ccc ptr @eclair_vector_get_value_entry(ptr %2, i32 %4)
  %7 = getelementptr %entry_t, ptr %6, i32 0, i32 0
  %8 = call ccc i1 @eclair_symbol_is_equal(ptr %7, ptr %symbol_0)
  br i1 %8, label %if_0, label %end_if_0
if_0:
  %9 = getelementptr %entry_t, ptr %6, i32 0, i32 1
  %10 = load i32, ptr %9
  ret i32 %10
end_if_0:
  %11 = add i32 1, %4
  br label %for_begin_0
for_end_0:
  %12 = load %symbol_t, ptr %symbol_0
  %13 = getelementptr %entry_t, ptr %stack.ptr_0, i32 0, i32 0
  store %symbol_t %12, ptr %13
  %14 = getelementptr %entry_t, ptr %stack.ptr_0, i32 0, i32 1
  store i32 %value_0, ptr %14
  %15 = call ccc i32 @eclair_vector_push_entry(ptr %2, ptr %stack.ptr_0)
  ret i32 %value_0
}

define external ccc i32 @eclair_hashmap_lookup(ptr %hashmap_0, ptr %symbol_0) {
start:
  %0 = call ccc i32 @eclair_symbol_hash(ptr %symbol_0)
  %1 = and i32 %0, 63
  %2 = getelementptr %hashmap_t, ptr %hashmap_0, i32 0, i32 0, i32 %1
  %3 = call ccc i32 @eclair_vector_size_entry(ptr %2)
  br label %for_begin_0
for_begin_0:
  %4 = phi i32 [0, %start], [%11, %end_if_0]
  %5 = icmp ult i32 %4, %3
  br i1 %5, label %for_body_0, label %for_end_0
for_body_0:
  %6 = call ccc ptr @eclair_vector_get_value_entry(ptr %2, i32 %4)
  %7 = getelementptr %entry_t, ptr %6, i32 0, i32 0
  %8 = call ccc i1 @eclair_symbol_is_equal(ptr %7, ptr %symbol_0)
  br i1 %8, label %if_0, label %end_if_0
if_0:
  %9 = getelementptr %entry_t, ptr %6, i32 0, i32 1
  %10 = load i32, ptr %9
  ret i32 %10
end_if_0:
  %11 = add i32 1, %4
  br label %for_begin_0
for_end_0:
  ret i32 4294967295
}

define external ccc i1 @eclair_hashmap_contains(ptr %hashmap_0, ptr %symbol_0) {
start:
  %0 = call ccc i32 @eclair_symbol_hash(ptr %symbol_0)
  %1 = and i32 %0, 63
  %2 = getelementptr %hashmap_t, ptr %hashmap_0, i32 0, i32 0, i32 %1
  %3 = call ccc i32 @eclair_vector_size_entry(ptr %2)
  br label %for_begin_0
for_begin_0:
  %4 = phi i32 [0, %start], [%9, %end_if_0]
  %5 = icmp ult i32 %4, %3
  br i1 %5, label %for_body_0, label %for_end_0
for_body_0:
  %6 = call ccc ptr @eclair_vector_get_value_entry(ptr %2, i32 %4)
  %7 = getelementptr %entry_t, ptr %6, i32 0, i32 0
  %8 = call ccc i1 @eclair_symbol_is_equal(ptr %7, ptr %symbol_0)
  br i1 %8, label %if_0, label %end_if_0
if_0:
  ret i1 1
end_if_0:
  %9 = add i32 1, %4
  br label %for_begin_0
for_end_0:
  ret i1 0
}

%symbol_table = type {%vector_t_symbol, %hashmap_t}

define external ccc void @eclair_symbol_table_init(ptr %table_0) {
start:
  %0 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 0
  %1 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 1
  call ccc void @eclair_vector_init_symbol(ptr %0)
  call ccc void @eclair_hashmap_init(ptr %1)
  ret void
}

define external ccc void @eclair_symbol_table_destroy(ptr %table_0) {
start:
  %0 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 0
  %1 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 1
  call ccc void @eclair_vector_destroy_symbol(ptr %0)
  call ccc void @eclair_hashmap_destroy(ptr %1)
  ret void
}

define external ccc i32 @eclair_symbol_table_find_or_insert(ptr %table_0, ptr %symbol_0) {
start:
  %0 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 0
  %1 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 1
  %2 = call ccc i32 @eclair_vector_size_symbol(ptr %0)
  %3 = call ccc i32 @eclair_hashmap_get_or_put_value(ptr %1, ptr %symbol_0, i32 %2)
  %4 = icmp eq i32 %2, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  %5 = call ccc i32 @eclair_vector_push_symbol(ptr %0, ptr %symbol_0)
  br label %end_if_0
end_if_0:
  ret i32 %3
}

define external ccc i1 @eclair_symbol_table_contains_index(ptr %table_0, i32 %index_0) {
start:
  %0 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 0
  %1 = call ccc i32 @eclair_vector_size_symbol(ptr %0)
  %2 = icmp ult i32 %index_0, %1
  ret i1 %2
}

define external ccc i1 @eclair_symbol_table_contains_symbol(ptr %table_0, ptr %symbol_0) {
start:
  %0 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 1
  %1 = call ccc i1 @eclair_hashmap_contains(ptr %0, ptr %symbol_0)
  ret i1 %1
}

define external ccc i32 @eclair_symbol_table_lookup_index(ptr %table_0, ptr %symbol_0) {
start:
  %0 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 1
  %1 = call ccc i32 @eclair_hashmap_lookup(ptr %0, ptr %symbol_0)
  ret i32 %1
}

define external ccc ptr @eclair_symbol_table_lookup_symbol(ptr %table_0, i32 %index_0) {
start:
  %0 = getelementptr %symbol_table, ptr %table_0, i32 0, i32 0
  %1 = call ccc ptr @eclair_vector_get_value_symbol(ptr %0, i32 %index_0)
  ret ptr %1
}

%program = type {%symbol_table, %btree_t_0, %btree_t_1}

@string_literal_0 = global [13 x i8] [i8 100, i8 101, i8 99, i8 108, i8 97, i8 114, i8 101, i8 95, i8 116, i8 121, i8 112, i8 101, i8 0]

@string_literal_1 = global [24 x i8] [i8 99, i8 111, i8 110, i8 102, i8 108, i8 105, i8 99, i8 116, i8 105, i8 110, i8 103, i8 95, i8 100, i8 101, i8 102, i8 105, i8 110, i8 105, i8 116, i8 105, i8 111, i8 110, i8 115, i8 0]

define external ccc ptr @eclair_program_init() "wasm-export-name"="eclair_program_init" {
start:
  %stack.ptr_0 = alloca %symbol_t, i32 1
  %stack.ptr_1 = alloca %symbol_t, i32 1
  %0 = call ccc ptr @malloc(i32 1592)
  %1 = getelementptr %program, ptr %0, i32 0, i32 0
  call ccc void @eclair_symbol_table_init(ptr %1)
  %2 = getelementptr %program, ptr %0, i32 0, i32 1
  call ccc void @eclair_btree_init_empty_0(ptr %2)
  %3 = getelementptr %program, ptr %0, i32 0, i32 2
  call ccc void @eclair_btree_init_empty_1(ptr %3)
  %4 = getelementptr %program, ptr %0, i32 0, i32 0
  %5 = getelementptr inbounds [13 x i8], ptr @string_literal_0, i32 0, i32 0
  %6 = zext i32 12 to i64
  %7 = call ccc ptr @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %7, ptr %5, i64 %6, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_0, i32 12, ptr %7)
  %8 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %4, ptr %stack.ptr_0)
  %9 = getelementptr %program, ptr %0, i32 0, i32 0
  %10 = getelementptr inbounds [24 x i8], ptr @string_literal_1, i32 0, i32 0
  %11 = zext i32 23 to i64
  %12 = call ccc ptr @malloc(i32 23)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %12, ptr %10, i64 %11, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_1, i32 23, ptr %12)
  %13 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %9, ptr %stack.ptr_1)
  ret ptr %0
}

define external ccc void @eclair_program_destroy(ptr %arg_0) "wasm-export-name"="eclair_program_destroy" {
start:
  %0 = getelementptr %program, ptr %arg_0, i32 0, i32 0
  call ccc void @eclair_symbol_table_destroy(ptr %0)
  %1 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_destroy_0(ptr %1)
  %2 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_destroy_1(ptr %2)
  call ccc void @free(ptr %arg_0)
  ret void
}

define external ccc void @eclair_program_run(ptr %arg_0) "wasm-export-name"="eclair_program_run" {
start:
  %stack.ptr_0 = alloca [2 x i32], i32 1
  %stack.ptr_1 = alloca [2 x i32], i32 1
  %stack.ptr_2 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_3 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_4 = alloca [2 x i32], i32 1
  %stack.ptr_5 = alloca [2 x i32], i32 1
  %stack.ptr_6 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_7 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_8 = alloca [3 x i32], i32 1
  %0 = getelementptr [2 x i32], ptr %stack.ptr_0, i32 0, i32 0
  store i32 0, ptr %0
  %1 = getelementptr [2 x i32], ptr %stack.ptr_0, i32 0, i32 1
  store i32 0, ptr %1
  %2 = getelementptr [2 x i32], ptr %stack.ptr_1, i32 0, i32 0
  store i32 4294967295, ptr %2
  %3 = getelementptr [2 x i32], ptr %stack.ptr_1, i32 0, i32 1
  store i32 4294967295, ptr %3
  %4 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_1(ptr %4, ptr %stack.ptr_0, ptr %stack.ptr_2)
  %5 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_1(ptr %5, ptr %stack.ptr_1, ptr %stack.ptr_3)
  br label %loop_0
loop_0:
  %6 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_2, ptr %stack.ptr_3)
  br i1 %6, label %if_0, label %end_if_0
if_0:
  br label %range_query.end
end_if_0:
  %7 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_2)
  %8 = getelementptr [2 x i32], ptr %stack.ptr_4, i32 0, i32 0
  store i32 0, ptr %8
  %9 = getelementptr [2 x i32], ptr %stack.ptr_4, i32 0, i32 1
  %10 = getelementptr [2 x i32], ptr %7, i32 0, i32 1
  %11 = load i32, ptr %10
  store i32 %11, ptr %9
  %12 = getelementptr [2 x i32], ptr %stack.ptr_5, i32 0, i32 0
  store i32 4294967295, ptr %12
  %13 = getelementptr [2 x i32], ptr %stack.ptr_5, i32 0, i32 1
  %14 = getelementptr [2 x i32], ptr %7, i32 0, i32 1
  %15 = load i32, ptr %14
  store i32 %15, ptr %13
  %16 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_1(ptr %16, ptr %stack.ptr_4, ptr %stack.ptr_6)
  %17 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_1(ptr %17, ptr %stack.ptr_5, ptr %stack.ptr_7)
  br label %loop_1
loop_1:
  %18 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_6, ptr %stack.ptr_7)
  br i1 %18, label %if_1, label %end_if_1
if_1:
  br label %range_query.end_1
end_if_1:
  %19 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_6)
  %20 = getelementptr [2 x i32], ptr %7, i32 0, i32 0
  %21 = load i32, ptr %20
  %22 = getelementptr [2 x i32], ptr %19, i32 0, i32 0
  %23 = load i32, ptr %22
  %24 = icmp ult i32 %21, %23
  br i1 %24, label %if_2, label %end_if_2
if_2:
  %25 = getelementptr [3 x i32], ptr %stack.ptr_8, i32 0, i32 0
  %26 = getelementptr [2 x i32], ptr %7, i32 0, i32 0
  %27 = load i32, ptr %26
  store i32 %27, ptr %25
  %28 = getelementptr [3 x i32], ptr %stack.ptr_8, i32 0, i32 1
  %29 = getelementptr [2 x i32], ptr %19, i32 0, i32 0
  %30 = load i32, ptr %29
  store i32 %30, ptr %28
  %31 = getelementptr [3 x i32], ptr %stack.ptr_8, i32 0, i32 2
  %32 = getelementptr [2 x i32], ptr %7, i32 0, i32 1
  %33 = load i32, ptr %32
  store i32 %33, ptr %31
  %34 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  %35 = call ccc i1 @eclair_btree_insert_value_0(ptr %34, ptr %stack.ptr_8)
  br label %end_if_2
end_if_2:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_6)
  br label %loop_1
range_query.end_1:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_2)
  br label %loop_0
range_query.end:
  ret void
}

define external ccc void @eclair_add_facts(ptr %eclair_program_0, i32 %fact_type_0, ptr %memory_0, i32 %fact_count_0) "wasm-export-name"="eclair_add_facts" {
start:
  switch i32 %fact_type_0, label %switch.default_0 [i32 0, label %declare_type_0]
declare_type_0:
  %0 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 2
  br label %for_begin_0
for_begin_0:
  %1 = phi i32 [0, %declare_type_0], [%5, %for_body_0]
  %2 = icmp ult i32 %1, %fact_count_0
  br i1 %2, label %for_body_0, label %for_end_0
for_body_0:
  %3 = getelementptr [2 x i32], ptr %memory_0, i32 %1
  %4 = call ccc i1 @eclair_btree_insert_value_1(ptr %0, ptr %3)
  %5 = add i32 1, %1
  br label %for_begin_0
for_end_0:
  br label %end_0
switch.default_0:
  ret void
end_0:
  ret void
}

define external ccc void @eclair_add_fact(ptr %eclair_program_0, i32 %fact_type_0, ptr %memory_0) "wasm-export-name"="eclair_add_fact" {
start:
  call ccc void @eclair_add_facts(ptr %eclair_program_0, i32 %fact_type_0, ptr %memory_0, i32 1)
  ret void
}

define external ccc ptr @eclair_get_facts(ptr %eclair_program_0, i32 %fact_type_0) "wasm-export-name"="eclair_get_facts" {
start:
  %stack.ptr_0 = alloca i32, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_2 = alloca %btree_iterator_t_0, i32 1
  switch i32 %fact_type_0, label %switch.default_0 [i32 1, label %conflicting_definitions_0]
conflicting_definitions_0:
  %0 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 1
  %1 = call ccc i64 @eclair_btree_size_0(ptr %0)
  %2 = trunc i64 %1 to i32
  %3 = mul i32 %2, 12
  %4 = call ccc ptr @malloc(i32 %3)
  store i32 0, ptr %stack.ptr_0
  call ccc void @eclair_btree_begin_0(ptr %0, ptr %stack.ptr_1)
  call ccc void @eclair_btree_end_0(ptr %0, ptr %stack.ptr_2)
  br label %while_begin_0
while_begin_0:
  %5 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_1, ptr %stack.ptr_2)
  %6 = select i1 %5, i1 0, i1 1
  br i1 %6, label %while_body_0, label %while_end_0
while_body_0:
  %7 = load i32, ptr %stack.ptr_0
  %8 = getelementptr [3 x i32], ptr %4, i32 %7
  %9 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_1)
  %10 = getelementptr [3 x i32], ptr %9, i32 0
  %11 = load [3 x i32], ptr %10
  %12 = getelementptr [3 x i32], ptr %8, i32 0
  store [3 x i32] %11, ptr %12
  %13 = add i32 %7, 1
  store i32 %13, ptr %stack.ptr_0
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_1)
  br label %while_begin_0
while_end_0:
  ret ptr %4
switch.default_0:
  ret ptr zeroinitializer
}

define external ccc void @eclair_free_buffer(ptr %buffer_0) "wasm-export-name"="eclair_free_buffer" {
start:
  call ccc void @free(ptr %buffer_0)
  ret void
}

define external ccc i32 @eclair_fact_count(ptr %eclair_program_0, i32 %fact_type_0) "wasm-export-name"="eclair_fact_count" {
start:
  switch i32 %fact_type_0, label %switch.default_0 [i32 1, label %conflicting_definitions_0]
conflicting_definitions_0:
  %0 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 1
  %1 = call ccc i64 @eclair_btree_size_0(ptr %0)
  %2 = trunc i64 %1 to i32
  ret i32 %2
switch.default_0:
  ret i32 0
}

define external ccc i32 @eclair_encode_string(ptr %eclair_program_0, i32 %string_length_0, ptr %string_data_0) "wasm-export-name"="eclair_encode_string" {
start:
  %stack.ptr_0 = alloca %symbol_t, i32 1
  %0 = call ccc ptr @malloc(i32 %string_length_0)
  %1 = zext i32 %string_length_0 to i64
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %0, ptr %string_data_0, i64 %1, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_0, i32 %string_length_0, ptr %0)
  %2 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 0
  %3 = call ccc i32 @eclair_symbol_table_lookup_index(ptr %2, ptr %stack.ptr_0)
  %4 = icmp ne i32 %3, 4294967295
  br i1 %4, label %if_0, label %end_if_0
if_0:
  call ccc void @free(ptr %0)
  ret i32 %3
end_if_0:
  %5 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %2, ptr %stack.ptr_0)
  ret i32 %5
}

define external ccc ptr @eclair_decode_string(ptr %eclair_program_0, i32 %string_index_0) "wasm-export-name"="eclair_decode_string" {
start:
  %0 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 0
  %1 = call ccc i1 @eclair_symbol_table_contains_index(ptr %0, i32 %string_index_0)
  br i1 %1, label %if_0, label %end_if_0
if_0:
  %2 = call ccc ptr @eclair_symbol_table_lookup_symbol(ptr %0, i32 %string_index_0)
  ret ptr %2
end_if_0:
  ret ptr zeroinitializer
}
