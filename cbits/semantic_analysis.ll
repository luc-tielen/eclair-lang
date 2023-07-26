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
  %23 = icmp uge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
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
  %57 = getelementptr %inner_node_t_0, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_0, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_0, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_0, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_0, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_0, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
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
  %24 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_0, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_0, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_0, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_0(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 12
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [3 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 20
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_0, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_0(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_0, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_0, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_0, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_0, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [3 x i32], ptr %68
  %70 = getelementptr %node_t_0, ptr %61, i32 0, i32 1, i16 %65
  store [3 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [3 x i32], ptr %val_0
  %73 = getelementptr %node_t_0, ptr %61, i32 0, i32 1, i16 %62
  store [3 x i32] %72, ptr %73
  %74 = getelementptr %node_t_0, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
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
  %15 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_0(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_0, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_0(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_0, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
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
  %25 = call ccc i8 @eclair_btree_value_compare_values_0(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_0(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_0(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_0, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
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
  %0 = getelementptr [2 x i32], ptr %lhs_0, i32 0, i32 0
  %1 = getelementptr [2 x i32], ptr %rhs_0, i32 0, i32 0
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_1(i32 %2, i32 %3)
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %comparison_1, label %end_0
comparison_1:
  %6 = getelementptr [2 x i32], ptr %lhs_0, i32 0, i32 1
  %7 = getelementptr [2 x i32], ptr %rhs_0, i32 0, i32 1
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
  %23 = icmp uge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
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
  %57 = getelementptr %inner_node_t_1, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_1, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_1, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_1, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_1, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_1, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
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
  %24 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_1, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_1, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_1, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_1(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 8
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [2 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 30
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_1, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_1(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_1, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_1, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_1, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_1, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [2 x i32], ptr %68
  %70 = getelementptr %node_t_1, ptr %61, i32 0, i32 1, i16 %65
  store [2 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [2 x i32], ptr %val_0
  %73 = getelementptr %node_t_1, ptr %61, i32 0, i32 1, i16 %62
  store [2 x i32] %72, ptr %73
  %74 = getelementptr %node_t_1, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
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
  %15 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_1(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_1, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_1(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_1, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
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
  %25 = call ccc i8 @eclair_btree_value_compare_values_1(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_1(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_1(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_1, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
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

%node_data_t_2 = type {ptr, i16, i16, i1}

%node_t_2 = type {%node_data_t_2, [30 x [2 x i32]]}

%inner_node_t_2 = type {%node_t_2, [31 x ptr]}

%btree_iterator_t_2 = type {ptr, i16}

%btree_t_2 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_2(i32 %lhs_0, i32 %rhs_0) {
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

define external ccc i8 @eclair_btree_value_compare_values_2(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [2 x i32], ptr %lhs_0, i32 0, i32 1
  %1 = getelementptr [2 x i32], ptr %rhs_0, i32 0, i32 1
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_2(i32 %2, i32 %3)
  br label %end_0
end_0:
  %5 = phi i8 [%4, %comparison_0]
  ret i8 %5
}

define external ccc ptr @eclair_btree_node_new_2(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 504, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_2, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_2, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_2, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_2, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_2, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_2, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 248, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_2(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_2(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_2(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_2(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_2(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_2(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_2, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_2, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_2, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_2, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_2(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_2, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_2(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_2, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_2, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_2, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_2, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_2, ptr %19, i32 0, i32 0, i32 2
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
  %25 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_2, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_2, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_2, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_2, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_2, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_2(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %2, ptr %val_0)
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

define external ccc ptr @eclair_btree_linear_search_upper_bound_2(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %2, ptr %val_0)
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

define external ccc void @eclair_btree_init_empty_2(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_2(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__2(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_2(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_2(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_2(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_2(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_2(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_2() {
start:
  %0 = mul i16 3, 30
  %1 = udiv i16 %0, 4
  %2 = sub i16 30, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_2(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_2()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_2(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 30
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [2 x i32], ptr %8
  %10 = getelementptr %node_t_2, ptr %4, i32 0, i32 1, i16 %7
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
  %17 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_2, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_2, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_2, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 30, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_2, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_2(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_2(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_2(i1 1)
  %6 = getelementptr %node_t_2, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [2 x i32], ptr %7
  %9 = getelementptr %node_t_2, ptr %5, i32 0, i32 1, i16 0
  store [2 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_2, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_2, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_2, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_2, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_2(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_2(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 30
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_2(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_2, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_2(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp uge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [2 x i32], ptr %26
  %28 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %24
  store [2 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_2, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [2 x i32], ptr %key_0
  %39 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %21
  store [2 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_2, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_2, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_2(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_2, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 30, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_2, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [2 x i32], ptr %19
  %21 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_2, ptr %9, i32 0, i32 1, i16 %22
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
  %29 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [2 x i32], ptr %29
  %31 = getelementptr %node_t_2, ptr %9, i32 0, i32 1, i16 %28
  store [2 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [2 x i32], ptr %33
  store [2 x i32] %34, ptr %19
  %35 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [2 x i32], ptr %41
  %43 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 1, i16 %38
  store [2 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_2, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = getelementptr %inner_node_t_2, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_2, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_2, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_2, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_2(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_2(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_2(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_2(i1 0)
  %2 = getelementptr %node_t_2, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [2 x i32], ptr %val_0
  %4 = getelementptr %node_t_2, ptr %1, i32 0, i32 1, i16 0
  store [2 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_2, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_2, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_2(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 8
  %23 = icmp ne ptr %17, %16
  %24 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_2, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_2, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_2, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_2(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 8
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [2 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 30
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_2(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_2, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_2, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_2, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [2 x i32], ptr %68
  %70 = getelementptr %node_t_2, ptr %61, i32 0, i32 1, i16 %65
  store [2 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [2 x i32], ptr %val_0
  %73 = getelementptr %node_t_2, ptr %61, i32 0, i32 1, i16 %62
  store [2 x i32] %72, ptr %73
  %74 = getelementptr %node_t_2, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__2(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_2(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_2(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_2(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_2(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_2, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_2, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_2(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_2(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_2(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_2, i32 1
  call ccc void @eclair_btree_find_2(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_2(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_2(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_2(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_2(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_2, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_2, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_2, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_2(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = icmp ult ptr %8, %7
  %15 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_2(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_2, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_2(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_2, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_2(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_2(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_2(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_2(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_2, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_2, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_2, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_2(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = getelementptr %node_t_2, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_2, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_2, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_2, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_2, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_2(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  %25 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_2(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_2(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_2, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_2(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_2(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_2(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_2(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_2, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_2, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_2, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_2(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = getelementptr %node_t_2, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_2, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_2, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_2, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_2, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_2(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_2(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_2, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_2(ptr %node_0) {
start:
  %0 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_2(ptr %8)
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

define external ccc void @eclair_btree_clear_2(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_2(ptr %1)
  %3 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_2(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_2, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_2, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_2, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_2, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_2, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_2, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_2, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_2, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

%node_data_t_3 = type {ptr, i16, i16, i1}

%node_t_3 = type {%node_data_t_3, [15 x [4 x i32]]}

%inner_node_t_3 = type {%node_t_3, [16 x ptr]}

%btree_iterator_t_3 = type {ptr, i16}

%btree_t_3 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_3(i32 %lhs_0, i32 %rhs_0) {
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

define external ccc i8 @eclair_btree_value_compare_values_3(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 2
  %1 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 2
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_3(i32 %2, i32 %3)
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %comparison_3, label %end_0
comparison_1:
  %6 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 3
  %7 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 3
  %8 = load i32, ptr %6
  %9 = load i32, ptr %7
  %10 = call ccc i8 @eclair_btree_value_compare_3(i32 %8, i32 %9)
  %11 = icmp eq i8 %10, 0
  br i1 %11, label %comparison_3, label %end_0
comparison_2:
  %12 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 0
  %13 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 0
  %14 = load i32, ptr %12
  %15 = load i32, ptr %13
  %16 = call ccc i8 @eclair_btree_value_compare_3(i32 %14, i32 %15)
  %17 = icmp eq i8 %16, 0
  br i1 %17, label %comparison_3, label %end_0
comparison_3:
  %18 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 1
  %19 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 1
  %20 = load i32, ptr %18
  %21 = load i32, ptr %19
  %22 = call ccc i8 @eclair_btree_value_compare_3(i32 %20, i32 %21)
  br label %end_0
end_0:
  %23 = phi i8 [%4, %comparison_0], [%10, %comparison_1], [%16, %comparison_2], [%22, %comparison_3]
  ret i8 %23
}

define external ccc ptr @eclair_btree_node_new_3(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 384, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_3, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_3, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_3, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_3, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_3, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_3, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 128, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_3(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_3(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_3(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_3(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_3(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_3(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_3, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_3, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_3, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_3, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_3(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_3, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_3(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_3, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_3, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_3, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_3, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_3, ptr %19, i32 0, i32 0, i32 2
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
  %25 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_3, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_3, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_3, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_3, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_3, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_3(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %2, ptr %val_0)
  %4 = icmp ne i8 %3, -1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [4 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc ptr @eclair_btree_linear_search_upper_bound_3(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %2, ptr %val_0)
  %4 = icmp eq i8 %3, 1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [4 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc void @eclair_btree_init_empty_3(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_3(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__3(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_3(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_3(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_3(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_3(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_3(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_3() {
start:
  %0 = mul i16 3, 15
  %1 = udiv i16 %0, 4
  %2 = sub i16 15, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_3(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_3()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_3(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 15
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [4 x i32], ptr %8
  %10 = getelementptr %node_t_3, ptr %4, i32 0, i32 1, i16 %7
  store [4 x i32] %9, ptr %10
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
  %15 = icmp ule i16 %14, 15
  br i1 %15, label %for_body_1, label %for_end_1
for_body_1:
  %16 = load i16, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_3, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_3, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_3, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 15, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_3, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_3(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_3(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_3(i1 1)
  %6 = getelementptr %node_t_3, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [4 x i32], ptr %7
  %9 = getelementptr %node_t_3, ptr %5, i32 0, i32 1, i16 0
  store [4 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_3, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_3, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_3, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_3, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_3(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_3(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 15
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_3(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_3, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_3(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp uge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [4 x i32], ptr %26
  %28 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %24
  store [4 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_3, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [4 x i32], ptr %key_0
  %39 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %21
  store [4 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_3, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_3, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_3(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_3, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 15, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_3, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [4 x i32], ptr %19
  %21 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_3, ptr %9, i32 0, i32 1, i16 %22
  store [4 x i32] %20, ptr %23
  %24 = sub i16 %14, 1
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [0, %if_0], [%32, %for_body_0]
  %26 = icmp ult i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %22, 1
  %28 = add i16 %25, %27
  %29 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [4 x i32], ptr %29
  %31 = getelementptr %node_t_3, ptr %9, i32 0, i32 1, i16 %28
  store [4 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [4 x i32], ptr %33
  store [4 x i32] %34, ptr %19
  %35 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [4 x i32], ptr %41
  %43 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 1, i16 %38
  store [4 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_3, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = getelementptr %inner_node_t_3, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_3, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_3, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_3, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_3(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_3(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_3(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_3(i1 0)
  %2 = getelementptr %node_t_3, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [4 x i32], ptr %val_0
  %4 = getelementptr %node_t_3, ptr %1, i32 0, i32 1, i16 0
  store [4 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_3, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_3, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_3(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 16
  %23 = icmp ne ptr %17, %16
  %24 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_3, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_3, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_3, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_3(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 16
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [4 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 15
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_3(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_3, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_3, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_3, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [4 x i32], ptr %68
  %70 = getelementptr %node_t_3, ptr %61, i32 0, i32 1, i16 %65
  store [4 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [4 x i32], ptr %val_0
  %73 = getelementptr %node_t_3, ptr %61, i32 0, i32 1, i16 %62
  store [4 x i32] %72, ptr %73
  %74 = getelementptr %node_t_3, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__3(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_3(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_3(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_3(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_3(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_3, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_3, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_3(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_3(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_3(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_3, i32 1
  call ccc void @eclair_btree_find_3(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_3(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_3(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_3(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_3(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_3, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_3, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_3, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_3(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = icmp ult ptr %8, %7
  %15 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_3(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_3, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_3(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_3, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_3(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_3(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_3(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_3(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_3, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_3, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_3, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_3(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = getelementptr %node_t_3, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_3, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_3, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_3, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_3, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_3(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  %25 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_3(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_3(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_3, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_3(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_3(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_3(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_3(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_3, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_3, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_3, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_3(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = getelementptr %node_t_3, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_3, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_3, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_3, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_3, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_3(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_3(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_3, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_3(ptr %node_0) {
start:
  %0 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_3(ptr %8)
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

define external ccc void @eclair_btree_clear_3(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_3(ptr %1)
  %3 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_3(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_3, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_3, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_3, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_3, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_3, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_3, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_3, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_3, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

%node_data_t_4 = type {ptr, i16, i16, i1}

%node_t_4 = type {%node_data_t_4, [15 x [4 x i32]]}

%inner_node_t_4 = type {%node_t_4, [16 x ptr]}

%btree_iterator_t_4 = type {ptr, i16}

%btree_t_4 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_4(i32 %lhs_0, i32 %rhs_0) {
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

define external ccc i8 @eclair_btree_value_compare_values_4(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 0
  %1 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 0
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_4(i32 %2, i32 %3)
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %comparison_3, label %end_0
comparison_1:
  %6 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 1
  %7 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 1
  %8 = load i32, ptr %6
  %9 = load i32, ptr %7
  %10 = call ccc i8 @eclair_btree_value_compare_4(i32 %8, i32 %9)
  %11 = icmp eq i8 %10, 0
  br i1 %11, label %comparison_3, label %end_0
comparison_2:
  %12 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 2
  %13 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 2
  %14 = load i32, ptr %12
  %15 = load i32, ptr %13
  %16 = call ccc i8 @eclair_btree_value_compare_4(i32 %14, i32 %15)
  %17 = icmp eq i8 %16, 0
  br i1 %17, label %comparison_3, label %end_0
comparison_3:
  %18 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 3
  %19 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 3
  %20 = load i32, ptr %18
  %21 = load i32, ptr %19
  %22 = call ccc i8 @eclair_btree_value_compare_4(i32 %20, i32 %21)
  br label %end_0
end_0:
  %23 = phi i8 [%4, %comparison_0], [%10, %comparison_1], [%16, %comparison_2], [%22, %comparison_3]
  ret i8 %23
}

define external ccc ptr @eclair_btree_node_new_4(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 384, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_4, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_4, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_4, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_4, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_4, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_4, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 128, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_4(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_4(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_4(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_4(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_4(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_4(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_4, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_4, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_4, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_4, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_4(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_4, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_4(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_4, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_4, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_4, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_4, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_4, ptr %19, i32 0, i32 0, i32 2
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
  %25 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_4, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_4, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_4, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_4, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_4, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_4(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %2, ptr %val_0)
  %4 = icmp ne i8 %3, -1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [4 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc ptr @eclair_btree_linear_search_upper_bound_4(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %2, ptr %val_0)
  %4 = icmp eq i8 %3, 1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [4 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc void @eclair_btree_init_empty_4(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_4(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__4(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_4(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_4(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_4(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_4(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_4(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_4() {
start:
  %0 = mul i16 3, 15
  %1 = udiv i16 %0, 4
  %2 = sub i16 15, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_4(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_4()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_4(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 15
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [4 x i32], ptr %8
  %10 = getelementptr %node_t_4, ptr %4, i32 0, i32 1, i16 %7
  store [4 x i32] %9, ptr %10
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
  %15 = icmp ule i16 %14, 15
  br i1 %15, label %for_body_1, label %for_end_1
for_body_1:
  %16 = load i16, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_4, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_4, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_4, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 15, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_4, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_4(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_4(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_4(i1 1)
  %6 = getelementptr %node_t_4, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [4 x i32], ptr %7
  %9 = getelementptr %node_t_4, ptr %5, i32 0, i32 1, i16 0
  store [4 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_4, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_4, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_4, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_4, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_4(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_4(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 15
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_4(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_4, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_4(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp uge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [4 x i32], ptr %26
  %28 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %24
  store [4 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_4, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [4 x i32], ptr %key_0
  %39 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %21
  store [4 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_4, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_4, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_4(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_4, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 15, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_4, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [4 x i32], ptr %19
  %21 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_4, ptr %9, i32 0, i32 1, i16 %22
  store [4 x i32] %20, ptr %23
  %24 = sub i16 %14, 1
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [0, %if_0], [%32, %for_body_0]
  %26 = icmp ult i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %22, 1
  %28 = add i16 %25, %27
  %29 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [4 x i32], ptr %29
  %31 = getelementptr %node_t_4, ptr %9, i32 0, i32 1, i16 %28
  store [4 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [4 x i32], ptr %33
  store [4 x i32] %34, ptr %19
  %35 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [4 x i32], ptr %41
  %43 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 1, i16 %38
  store [4 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_4, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = getelementptr %inner_node_t_4, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_4, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_4, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_4, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_4(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_4(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_4(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_4(i1 0)
  %2 = getelementptr %node_t_4, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [4 x i32], ptr %val_0
  %4 = getelementptr %node_t_4, ptr %1, i32 0, i32 1, i16 0
  store [4 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_4, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_4, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_4(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 16
  %23 = icmp ne ptr %17, %16
  %24 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_4, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_4, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_4, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_4(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 16
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [4 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 15
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_4(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_4, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_4, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_4, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [4 x i32], ptr %68
  %70 = getelementptr %node_t_4, ptr %61, i32 0, i32 1, i16 %65
  store [4 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [4 x i32], ptr %val_0
  %73 = getelementptr %node_t_4, ptr %61, i32 0, i32 1, i16 %62
  store [4 x i32] %72, ptr %73
  %74 = getelementptr %node_t_4, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__4(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_4(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_4(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_4(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_4(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_4, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_4, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_4(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_4(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_4(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_4, i32 1
  call ccc void @eclair_btree_find_4(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_4(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_4(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_4(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_4(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_4, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_4, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_4, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_4(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = icmp ult ptr %8, %7
  %15 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_4(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_4, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_4(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_4, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_4(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_4(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_4(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_4(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_4, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_4, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_4, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_4(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = getelementptr %node_t_4, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_4, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_4, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_4, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_4, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_4(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  %25 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_4(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_4(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_4, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_4(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_4(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_4(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_4(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_4, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_4, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_4, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_4(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = getelementptr %node_t_4, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_4, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_4, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_4, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_4, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_4(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_4(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_4, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_4(ptr %node_0) {
start:
  %0 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_4(ptr %8)
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

define external ccc void @eclair_btree_clear_4(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_4(ptr %1)
  %3 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_4(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_4, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_4, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_4, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_4, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_4, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_4, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_4, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_4, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

%node_data_t_5 = type {ptr, i16, i16, i1}

%node_t_5 = type {%node_data_t_5, [15 x [4 x i32]]}

%inner_node_t_5 = type {%node_t_5, [16 x ptr]}

%btree_iterator_t_5 = type {ptr, i16}

%btree_t_5 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_5(i32 %lhs_0, i32 %rhs_0) {
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

define external ccc i8 @eclair_btree_value_compare_values_5(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [4 x i32], ptr %lhs_0, i32 0, i32 1
  %1 = getelementptr [4 x i32], ptr %rhs_0, i32 0, i32 1
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_5(i32 %2, i32 %3)
  br label %end_0
end_0:
  %5 = phi i8 [%4, %comparison_0]
  ret i8 %5
}

define external ccc ptr @eclair_btree_node_new_5(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 384, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_5, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_5, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_5, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_5, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_5, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_5, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 128, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_5(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_5(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_5(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_5(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_5(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_5(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_5, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_5, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_5, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_5, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_5(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_5, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_5(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_5, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_5, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_5, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_5, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_5, ptr %19, i32 0, i32 0, i32 2
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
  %25 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_5, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_5, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_5, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_5, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_5, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_5(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %2, ptr %val_0)
  %4 = icmp ne i8 %3, -1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [4 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc ptr @eclair_btree_linear_search_upper_bound_5(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %2, ptr %val_0)
  %4 = icmp eq i8 %3, 1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [4 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc void @eclair_btree_init_empty_5(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_5(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__5(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_5(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_5(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_5(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_5(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_5(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_5() {
start:
  %0 = mul i16 3, 15
  %1 = udiv i16 %0, 4
  %2 = sub i16 15, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_5(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_5()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_5(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 15
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [4 x i32], ptr %8
  %10 = getelementptr %node_t_5, ptr %4, i32 0, i32 1, i16 %7
  store [4 x i32] %9, ptr %10
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
  %15 = icmp ule i16 %14, 15
  br i1 %15, label %for_body_1, label %for_end_1
for_body_1:
  %16 = load i16, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_5, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_5, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_5, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 15, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_5, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_5(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_5(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_5(i1 1)
  %6 = getelementptr %node_t_5, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [4 x i32], ptr %7
  %9 = getelementptr %node_t_5, ptr %5, i32 0, i32 1, i16 0
  store [4 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_5, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_5, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_5, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_5, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_5(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_5(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 15
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_5(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_5, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_5(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp uge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [4 x i32], ptr %26
  %28 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %24
  store [4 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_5, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [4 x i32], ptr %key_0
  %39 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %21
  store [4 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_5, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_5, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_5(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_5, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 15, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_5, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [4 x i32], ptr %19
  %21 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_5, ptr %9, i32 0, i32 1, i16 %22
  store [4 x i32] %20, ptr %23
  %24 = sub i16 %14, 1
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [0, %if_0], [%32, %for_body_0]
  %26 = icmp ult i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %22, 1
  %28 = add i16 %25, %27
  %29 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [4 x i32], ptr %29
  %31 = getelementptr %node_t_5, ptr %9, i32 0, i32 1, i16 %28
  store [4 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [4 x i32], ptr %33
  store [4 x i32] %34, ptr %19
  %35 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [4 x i32], ptr %41
  %43 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 1, i16 %38
  store [4 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_5, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = getelementptr %inner_node_t_5, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_5, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_5, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_5, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_5(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_5(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_5(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_5(i1 0)
  %2 = getelementptr %node_t_5, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [4 x i32], ptr %val_0
  %4 = getelementptr %node_t_5, ptr %1, i32 0, i32 1, i16 0
  store [4 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_5, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_5, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_5(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 16
  %23 = icmp ne ptr %17, %16
  %24 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_5, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_5, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_5, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_5(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 16
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [4 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 15
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_5(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_5, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_5, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_5, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [4 x i32], ptr %68
  %70 = getelementptr %node_t_5, ptr %61, i32 0, i32 1, i16 %65
  store [4 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [4 x i32], ptr %val_0
  %73 = getelementptr %node_t_5, ptr %61, i32 0, i32 1, i16 %62
  store [4 x i32] %72, ptr %73
  %74 = getelementptr %node_t_5, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__5(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_5(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_5(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_5(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_5(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_5(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_5, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_5, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_5(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_5(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_5(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_5, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_5, i32 1
  call ccc void @eclair_btree_find_5(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_5(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_5(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_5(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_5(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_5(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_5, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_5, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_5, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_5(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = icmp ult ptr %8, %7
  %15 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_5(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_5, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_5(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_5, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_5(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_5, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_5(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_5(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_5(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_5, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_5, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_5, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_5(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = getelementptr %node_t_5, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_5, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_5, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_5, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_5, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_5(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  %25 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_5(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_5(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_5, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_5(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_5, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_5(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_5(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_5(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_5, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_5, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_5, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_5(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 16
  %14 = getelementptr %node_t_5, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_5, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_5, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_5, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_5, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_5(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_5(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_5, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_5(ptr %node_0) {
start:
  %0 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_5(ptr %8)
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

define external ccc void @eclair_btree_clear_5(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_5(ptr %1)
  %3 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_5(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_5, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_5, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_5, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_5, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_5, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_5, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_5, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_5, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

%node_data_t_6 = type {ptr, i16, i16, i1}

%node_t_6 = type {%node_data_t_6, [60 x [1 x i32]]}

%inner_node_t_6 = type {%node_t_6, [61 x ptr]}

%btree_iterator_t_6 = type {ptr, i16}

%btree_t_6 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_6(i32 %lhs_0, i32 %rhs_0) {
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

define external ccc i8 @eclair_btree_value_compare_values_6(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [1 x i32], ptr %lhs_0, i32 0, i32 0
  %1 = getelementptr [1 x i32], ptr %rhs_0, i32 0, i32 0
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_6(i32 %2, i32 %3)
  br label %end_0
end_0:
  %5 = phi i8 [%4, %comparison_0]
  ret i8 %5
}

define external ccc ptr @eclair_btree_node_new_6(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 744, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_6, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_6, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_6, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_6, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_6, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_6, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 488, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_6(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_6(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_6(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_6(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_6(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_6(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_6, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_6, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_6, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_6, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_6(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_6, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_6(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_6, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_6, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_6, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_6, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_6, ptr %19, i32 0, i32 0, i32 2
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
  %25 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_6, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_6, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_6, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_6, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_6, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_6(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %2, ptr %val_0)
  %4 = icmp ne i8 %3, -1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [1 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc ptr @eclair_btree_linear_search_upper_bound_6(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %2, ptr %val_0)
  %4 = icmp eq i8 %3, 1
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret ptr %2
end_if_0:
  %5 = getelementptr [1 x i32], ptr %2, i32 1
  store ptr %5, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  ret ptr %end_0
}

define external ccc void @eclair_btree_init_empty_6(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_6(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__6(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_6(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_6(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_6(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_6(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_6(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_6() {
start:
  %0 = mul i16 3, 60
  %1 = udiv i16 %0, 4
  %2 = sub i16 60, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_6(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_6()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_6(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 60
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [1 x i32], ptr %8
  %10 = getelementptr %node_t_6, ptr %4, i32 0, i32 1, i16 %7
  store [1 x i32] %9, ptr %10
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
  %15 = icmp ule i16 %14, 60
  br i1 %15, label %for_body_1, label %for_end_1
for_body_1:
  %16 = load i16, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_6, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_6, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_6, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 60, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_6, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_6(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_6(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_6(i1 1)
  %6 = getelementptr %node_t_6, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [1 x i32], ptr %7
  %9 = getelementptr %node_t_6, ptr %5, i32 0, i32 1, i16 0
  store [1 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_6, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_6, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_6, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_6, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_6(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_6(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 60
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_6(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_6, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_6(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp uge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [1 x i32], ptr %26
  %28 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %24
  store [1 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_6, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [1 x i32], ptr %key_0
  %39 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %21
  store [1 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_6, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_6, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_6(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_6, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 60, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_6, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [1 x i32], ptr %19
  %21 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_6, ptr %9, i32 0, i32 1, i16 %22
  store [1 x i32] %20, ptr %23
  %24 = sub i16 %14, 1
  br label %for_begin_0
for_begin_0:
  %25 = phi i16 [0, %if_0], [%32, %for_body_0]
  %26 = icmp ult i16 %25, %24
  br i1 %26, label %for_body_0, label %for_end_0
for_body_0:
  %27 = add i16 %22, 1
  %28 = add i16 %25, %27
  %29 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [1 x i32], ptr %29
  %31 = getelementptr %node_t_6, ptr %9, i32 0, i32 1, i16 %28
  store [1 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [1 x i32], ptr %33
  store [1 x i32] %34, ptr %19
  %35 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [1 x i32], ptr %41
  %43 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 1, i16 %38
  store [1 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_6, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = getelementptr %inner_node_t_6, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_6, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_6, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_6, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_6(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_6(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_6(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_6(i1 0)
  %2 = getelementptr %node_t_6, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [1 x i32], ptr %val_0
  %4 = getelementptr %node_t_6, ptr %1, i32 0, i32 1, i16 0
  store [1 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_6, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_6, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_6(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 4
  %23 = icmp ne ptr %17, %16
  %24 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_6, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_6, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_6, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_6(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 4
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [1 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 60
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_6(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_6, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_6, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_6, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [1 x i32], ptr %68
  %70 = getelementptr %node_t_6, ptr %61, i32 0, i32 1, i16 %65
  store [1 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [1 x i32], ptr %val_0
  %73 = getelementptr %node_t_6, ptr %61, i32 0, i32 1, i16 %62
  store [1 x i32] %72, ptr %73
  %74 = getelementptr %node_t_6, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__6(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_6(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_6(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_6(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_6(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_6, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_6, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_6(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_6(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_6(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_6, i32 1
  call ccc void @eclair_btree_find_6(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_6(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_6(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_6(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_6(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_6, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_6, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_6, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_6(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 4
  %14 = icmp ult ptr %8, %7
  %15 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_6(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_6, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_6(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_6, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_6(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_6(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_6(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_6(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_6, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_6, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_6, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_6(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 4
  %14 = getelementptr %node_t_6, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_6, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_6, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_6, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_6, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_6(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  %25 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_6(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_6(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_6, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_6(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_6(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_6(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_6(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_6, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_6, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_6, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_6(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 4
  %14 = getelementptr %node_t_6, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_6, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_6, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_6, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_6, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_6(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_6(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_6, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_6(ptr %node_0) {
start:
  %0 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_6(ptr %8)
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

define external ccc void @eclair_btree_clear_6(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_6(ptr %1)
  %3 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_6(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_6, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_6, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_6, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_6, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_6, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_6, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_6, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_6, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

%node_data_t_7 = type {ptr, i16, i16, i1}

%node_t_7 = type {%node_data_t_7, [30 x [2 x i32]]}

%inner_node_t_7 = type {%node_t_7, [31 x ptr]}

%btree_iterator_t_7 = type {ptr, i16}

%btree_t_7 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_7(i32 %lhs_0, i32 %rhs_0) {
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

define external ccc i8 @eclair_btree_value_compare_values_7(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [2 x i32], ptr %lhs_0, i32 0, i32 1
  %1 = getelementptr [2 x i32], ptr %rhs_0, i32 0, i32 1
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_7(i32 %2, i32 %3)
  %5 = icmp eq i8 %4, 0
  br i1 %5, label %comparison_1, label %end_0
comparison_1:
  %6 = getelementptr [2 x i32], ptr %lhs_0, i32 0, i32 0
  %7 = getelementptr [2 x i32], ptr %rhs_0, i32 0, i32 0
  %8 = load i32, ptr %6
  %9 = load i32, ptr %7
  %10 = call ccc i8 @eclair_btree_value_compare_7(i32 %8, i32 %9)
  br label %end_0
end_0:
  %11 = phi i8 [%4, %comparison_0], [%10, %comparison_1]
  ret i8 %11
}

define external ccc ptr @eclair_btree_node_new_7(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 504, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_7, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_7, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_7, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_7, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_7, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_7, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 248, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_7(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_7(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_7(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_7(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_7(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_7(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_7, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_7, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_7, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_7, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_7(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_7, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_7(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_7, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_7, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_7, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_7, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_7, ptr %19, i32 0, i32 0, i32 2
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
  %25 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_7, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_7, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_7, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_7, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_7, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_7(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %2, ptr %val_0)
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

define external ccc ptr @eclair_btree_linear_search_upper_bound_7(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %2, ptr %val_0)
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

define external ccc void @eclair_btree_init_empty_7(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_7(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__7(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_7(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_7(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_7(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_7(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_7(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_7() {
start:
  %0 = mul i16 3, 30
  %1 = udiv i16 %0, 4
  %2 = sub i16 30, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_7(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_7()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_7(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 30
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [2 x i32], ptr %8
  %10 = getelementptr %node_t_7, ptr %4, i32 0, i32 1, i16 %7
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
  %17 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_7, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_7, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_7, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 30, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_7, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_7(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_7(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_7(i1 1)
  %6 = getelementptr %node_t_7, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [2 x i32], ptr %7
  %9 = getelementptr %node_t_7, ptr %5, i32 0, i32 1, i16 0
  store [2 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_7, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_7, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_7, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_7, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_7(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_7(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 30
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_7(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_7, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_7(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp uge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [2 x i32], ptr %26
  %28 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %24
  store [2 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_7, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [2 x i32], ptr %key_0
  %39 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %21
  store [2 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_7, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_7, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_7(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_7, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 30, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_7, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [2 x i32], ptr %19
  %21 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_7, ptr %9, i32 0, i32 1, i16 %22
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
  %29 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [2 x i32], ptr %29
  %31 = getelementptr %node_t_7, ptr %9, i32 0, i32 1, i16 %28
  store [2 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [2 x i32], ptr %33
  store [2 x i32] %34, ptr %19
  %35 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [2 x i32], ptr %41
  %43 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 1, i16 %38
  store [2 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_7, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = getelementptr %inner_node_t_7, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_7, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_7, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_7, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_7(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_7(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_7(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_7(i1 0)
  %2 = getelementptr %node_t_7, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [2 x i32], ptr %val_0
  %4 = getelementptr %node_t_7, ptr %1, i32 0, i32 1, i16 0
  store [2 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_7, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_7, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_7(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 8
  %23 = icmp ne ptr %17, %16
  %24 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_7, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_7, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_7, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_7(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 8
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [2 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 30
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_7(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_7, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_7, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_7, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [2 x i32], ptr %68
  %70 = getelementptr %node_t_7, ptr %61, i32 0, i32 1, i16 %65
  store [2 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [2 x i32], ptr %val_0
  %73 = getelementptr %node_t_7, ptr %61, i32 0, i32 1, i16 %62
  store [2 x i32] %72, ptr %73
  %74 = getelementptr %node_t_7, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__7(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_7(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_7(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_7(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_7(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_7, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_7, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_7(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_7(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_7(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_7, i32 1
  call ccc void @eclair_btree_find_7(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_7(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_7(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_7(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_7(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_7, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_7, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_7, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_7(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = icmp ult ptr %8, %7
  %15 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_7(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_7, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_7(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_7, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_7(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_7(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_7(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_7(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_7, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_7, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_7, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_7(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = getelementptr %node_t_7, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_7, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_7, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_7, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_7, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_7(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  %25 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_7(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_7(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_7, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_7(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_7(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_7(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_7(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_7, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_7, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_7, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_7(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 8
  %14 = getelementptr %node_t_7, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_7, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_7, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_7, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_7, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_7(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_7(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_7, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_7(ptr %node_0) {
start:
  %0 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_7(ptr %8)
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

define external ccc void @eclair_btree_clear_7(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_7(ptr %1)
  %3 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_7(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_7, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_7, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_7, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_7, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_7, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_7, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_7, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_7, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

%node_data_t_8 = type {ptr, i16, i16, i1}

%node_t_8 = type {%node_data_t_8, [20 x [3 x i32]]}

%inner_node_t_8 = type {%node_t_8, [21 x ptr]}

%btree_iterator_t_8 = type {ptr, i16}

%btree_t_8 = type {ptr, ptr}

define external ccc i8 @eclair_btree_value_compare_8(i32 %lhs_0, i32 %rhs_0) {
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

define external ccc i8 @eclair_btree_value_compare_values_8(ptr %lhs_0, ptr %rhs_0) {
start:
  br label %comparison_0
comparison_0:
  %0 = getelementptr [3 x i32], ptr %lhs_0, i32 0, i32 2
  %1 = getelementptr [3 x i32], ptr %rhs_0, i32 0, i32 2
  %2 = load i32, ptr %0
  %3 = load i32, ptr %1
  %4 = call ccc i8 @eclair_btree_value_compare_8(i32 %2, i32 %3)
  br label %end_0
end_0:
  %5 = phi i8 [%4, %comparison_0]
  ret i8 %5
}

define external ccc ptr @eclair_btree_node_new_8(i1 %type_0) {
start:
  %0 = select i1 %type_0, i32 424, i32 256
  %1 = call ccc ptr @malloc(i32 %0)
  %2 = getelementptr %node_t_8, ptr %1, i32 0, i32 0, i32 0
  store ptr zeroinitializer, ptr %2
  %3 = getelementptr %node_t_8, ptr %1, i32 0, i32 0, i32 1
  store i16 0, ptr %3
  %4 = getelementptr %node_t_8, ptr %1, i32 0, i32 0, i32 2
  store i16 0, ptr %4
  %5 = getelementptr %node_t_8, ptr %1, i32 0, i32 0, i32 3
  store i1 %type_0, ptr %5
  %6 = getelementptr %node_t_8, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %6, i8 0, i64 240, i1 0)
  %7 = icmp eq i1 %type_0, 1
  br i1 %7, label %if_0, label %end_if_0
if_0:
  %8 = getelementptr %inner_node_t_8, ptr %1, i32 0, i32 1
  call ccc void @llvm.memset.p0i8.i64(ptr %8, i8 0, i64 168, i1 0)
  br label %end_if_0
end_if_0:
  ret ptr %1
}

define external ccc i64 @eclair_btree_node_count_entries_8(ptr %node_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 0
  %5 = zext i16 %1 to i64
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i64 %5
end_if_0:
  store i64 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  br label %for_begin_0
for_begin_0:
  %8 = phi i16 [0, %end_if_0], [%15, %for_body_0]
  %9 = icmp ule i16 %8, %7
  br i1 %9, label %for_body_0, label %for_end_0
for_body_0:
  %10 = load i64, ptr %stack.ptr_0
  %11 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %8
  %12 = load ptr, ptr %11
  %13 = call ccc i64 @eclair_btree_node_count_entries_8(ptr %12)
  %14 = add i64 %10, %13
  store i64 %14, ptr %stack.ptr_0
  %15 = add i16 1, %8
  br label %for_begin_0
for_end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc void @eclair_btree_iterator_init_8(ptr %iter_0, ptr %cur_0, i16 %pos_0) {
start:
  %0 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  store ptr %cur_0, ptr %0
  %1 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 1
  store i16 %pos_0, ptr %1
  ret void
}

define external ccc void @eclair_btree_iterator_end_init_8(ptr %iter_0) {
start:
  call ccc void @eclair_btree_iterator_init_8(ptr %iter_0, ptr zeroinitializer, i16 0)
  ret void
}

define external ccc i1 @eclair_btree_iterator_is_equal_8(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_iterator_t_8, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_8, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = icmp ne ptr %1, %3
  br i1 %4, label %if_0, label %end_if_0
if_0:
  ret i1 0
end_if_0:
  %5 = getelementptr %btree_iterator_t_8, ptr %lhs_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = getelementptr %btree_iterator_t_8, ptr %rhs_0, i32 0, i32 1
  %8 = load i16, ptr %7
  %9 = icmp eq i16 %6, %8
  ret i1 %9
}

define external ccc ptr @eclair_btree_iterator_current_8(ptr %iter_0) {
start:
  %0 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 1
  %1 = load i16, ptr %0
  %2 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %node_t_8, ptr %3, i32 0, i32 1, i16 %1
  ret ptr %4
}

define external ccc void @eclair_btree_iterator_next_8(ptr %iter_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_8, ptr %1, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = icmp eq i1 %3, 1
  br i1 %4, label %if_0, label %end_if_1
if_0:
  %5 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 1
  %6 = load i16, ptr %5
  %7 = add i16 1, %6
  %8 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  %9 = load ptr, ptr %8
  %10 = getelementptr %inner_node_t_8, ptr %9, i32 0, i32 1, i16 %7
  %11 = load ptr, ptr %10
  store ptr %11, ptr %stack.ptr_0
  br label %while_begin_0
while_begin_0:
  %12 = load ptr, ptr %stack.ptr_0
  %13 = getelementptr %node_t_8, ptr %12, i32 0, i32 0, i32 3
  %14 = load i1, ptr %13
  %15 = icmp eq i1 %14, 1
  br i1 %15, label %while_body_0, label %while_end_0
while_body_0:
  %16 = load ptr, ptr %stack.ptr_0
  %17 = getelementptr %inner_node_t_8, ptr %16, i32 0, i32 1, i16 0
  %18 = load ptr, ptr %17
  store ptr %18, ptr %stack.ptr_0
  br label %while_begin_0
while_end_0:
  %19 = load ptr, ptr %stack.ptr_0
  %20 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 1
  store i16 0, ptr %21
  %22 = getelementptr %node_t_8, ptr %19, i32 0, i32 0, i32 2
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
  %25 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 1
  %26 = load i16, ptr %25
  %27 = add i16 1, %26
  store i16 %27, ptr %25
  %28 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 1
  %29 = load i16, ptr %28
  %30 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  %31 = load ptr, ptr %30
  %32 = getelementptr %node_t_8, ptr %31, i32 0, i32 0, i32 2
  %33 = load i16, ptr %32
  %34 = icmp ult i16 %29, %33
  br i1 %34, label %if_2, label %end_if_2
if_2:
  ret void
end_if_2:
  br label %while_begin_1
while_begin_1:
  %35 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  %36 = load ptr, ptr %35
  %37 = icmp eq ptr %36, zeroinitializer
  br i1 %37, label %leaf.no_parent_0, label %leaf.has_parent_0
leaf.no_parent_0:
  br label %loop.condition.end_0
leaf.has_parent_0:
  %38 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 1
  %39 = load i16, ptr %38
  %40 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  %41 = load ptr, ptr %40
  %42 = getelementptr %node_t_8, ptr %41, i32 0, i32 0, i32 2
  %43 = load i16, ptr %42
  %44 = icmp eq i16 %39, %43
  br label %loop.condition.end_0
loop.condition.end_0:
  %45 = phi i1 [0, %leaf.no_parent_0], [%44, %leaf.has_parent_0]
  br i1 %45, label %while_body_1, label %while_end_1
while_body_1:
  %46 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  %47 = load ptr, ptr %46
  %48 = getelementptr %node_t_8, ptr %47, i32 0, i32 0, i32 1
  %49 = load i16, ptr %48
  %50 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 1
  store i16 %49, ptr %50
  %51 = getelementptr %node_t_8, ptr %47, i32 0, i32 0, i32 0
  %52 = load ptr, ptr %51
  %53 = getelementptr %btree_iterator_t_8, ptr %iter_0, i32 0, i32 0
  store ptr %52, ptr %53
  br label %while_begin_1
while_end_1:
  ret void
}

define external ccc ptr @eclair_btree_linear_search_lower_bound_8(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %2, ptr %val_0)
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

define external ccc ptr @eclair_btree_linear_search_upper_bound_8(ptr %val_0, ptr %current_0, ptr %end_0) {
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
  %3 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %2, ptr %val_0)
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

define external ccc void @eclair_btree_init_empty_8(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %0
  %1 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %1
  ret void
}

define external ccc void @eclair_btree_init_8(ptr %tree_0, ptr %start_0, ptr %end_0) {
start:
  call ccc void @eclair_btree_insert_range__8(ptr %tree_0, ptr %start_0, ptr %end_0)
  ret void
}

define external ccc void @eclair_btree_destroy_8(ptr %tree_0) {
start:
  call ccc void @eclair_btree_clear_8(ptr %tree_0)
  ret void
}

define external ccc i1 @eclair_btree_is_empty_8(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  ret i1 %2
}

define external ccc i64 @eclair_btree_size_8(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  br i1 %2, label %null_0, label %not_null_0
null_0:
  ret i64 0
not_null_0:
  %3 = call ccc i64 @eclair_btree_node_count_entries_8(ptr %1)
  ret i64 %3
}

define external ccc i16 @eclair_btree_node_split_point_8() {
start:
  %0 = mul i16 3, 20
  %1 = udiv i16 %0, 4
  %2 = sub i16 20, 2
  %3 = icmp ult i16 %1, %2
  %4 = select i1 %3, i16 %1, i16 %2
  ret i16 %4
}

define external ccc void @eclair_btree_node_split_8(ptr %node_0, ptr %root_0) {
start:
  %stack.ptr_0 = alloca i16
  %0 = call ccc i16 @eclair_btree_node_split_point_8()
  %1 = add i16 1, %0
  %2 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 3
  %3 = load i1, ptr %2
  %4 = call ccc ptr @eclair_btree_node_new_8(i1 %3)
  store i16 0, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [%1, %start], [%12, %for_body_0]
  %6 = icmp ult i16 %5, 20
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = load i16, ptr %stack.ptr_0
  %8 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %5
  %9 = load [3 x i32], ptr %8
  %10 = getelementptr %node_t_8, ptr %4, i32 0, i32 1, i16 %7
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
  %17 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %14
  %18 = load ptr, ptr %17
  %19 = getelementptr %node_t_8, ptr %18, i32 0, i32 0, i32 0
  store ptr %4, ptr %19
  %20 = getelementptr %node_t_8, ptr %18, i32 0, i32 0, i32 1
  store i16 %16, ptr %20
  %21 = getelementptr %inner_node_t_8, ptr %4, i32 0, i32 1, i16 %16
  store ptr %18, ptr %21
  %22 = add i16 1, %16
  store i16 %22, ptr %stack.ptr_0
  %23 = add i16 1, %14
  br label %for_begin_1
for_end_1:
  br label %end_if_0
end_if_0:
  %24 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  store i16 %0, ptr %24
  %25 = sub i16 20, %0
  %26 = sub i16 %25, 1
  %27 = getelementptr %node_t_8, ptr %4, i32 0, i32 0, i32 2
  store i16 %26, ptr %27
  call ccc void @eclair_btree_node_grow_parent_8(ptr %node_0, ptr %root_0, ptr %4)
  ret void
}

define external ccc void @eclair_btree_node_grow_parent_8(ptr %node_0, ptr %root_0, ptr %sibling_0) {
start:
  %0 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp eq ptr %1, zeroinitializer
  %3 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br i1 %2, label %create_new_root_0, label %insert_new_node_in_parent_0
create_new_root_0:
  %5 = call ccc ptr @eclair_btree_node_new_8(i1 1)
  %6 = getelementptr %node_t_8, ptr %5, i32 0, i32 0, i32 2
  store i16 1, ptr %6
  %7 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %4
  %8 = load [3 x i32], ptr %7
  %9 = getelementptr %node_t_8, ptr %5, i32 0, i32 1, i16 0
  store [3 x i32] %8, ptr %9
  %10 = getelementptr %inner_node_t_8, ptr %5, i32 0, i32 1, i16 0
  store ptr %node_0, ptr %10
  %11 = getelementptr %inner_node_t_8, ptr %5, i32 0, i32 1, i16 1
  store ptr %sibling_0, ptr %11
  %12 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %12
  %13 = getelementptr %node_t_8, ptr %sibling_0, i32 0, i32 0, i32 0
  store ptr %5, ptr %13
  %14 = getelementptr %node_t_8, ptr %sibling_0, i32 0, i32 0, i32 1
  store i16 1, ptr %14
  store ptr %5, ptr %root_0
  ret void
insert_new_node_in_parent_0:
  %15 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 1
  %16 = load i16, ptr %15
  %17 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %4
  call ccc void @eclair_btree_node_insert_inner_8(ptr %1, ptr %root_0, i16 %16, ptr %node_0, ptr %17, ptr %sibling_0)
  ret void
}

define external ccc void @eclair_btree_node_insert_inner_8(ptr %node_0, ptr %root_0, i16 %pos_0, ptr %predecessor_0, ptr %key_0, ptr %new_node_0) {
start:
  %stack.ptr_0 = alloca i16
  store i16 %pos_0, ptr %stack.ptr_0
  %0 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %1 = load i16, ptr %0
  %2 = icmp uge i16 %1, 20
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = load i16, ptr %stack.ptr_0
  %4 = call ccc i16 @eclair_btree_node_rebalance_or_split_8(ptr %node_0, ptr %root_0, i16 %pos_0)
  %5 = sub i16 %3, %4
  store i16 %5, ptr %stack.ptr_0
  %6 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %7 = load i16, ptr %6
  %8 = icmp ugt i16 %5, %7
  br i1 %8, label %if_1, label %end_if_0
if_1:
  %9 = sub i16 %5, %7
  %10 = sub i16 %9, 1
  store i16 %10, ptr %stack.ptr_0
  %11 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 0
  %12 = load ptr, ptr %11
  %13 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 1
  %14 = load i16, ptr %13
  %15 = add i16 1, %14
  %16 = getelementptr %inner_node_t_8, ptr %12, i32 0, i32 1, i16 %15
  %17 = load ptr, ptr %16
  call ccc void @eclair_btree_node_insert_inner_8(ptr %17, ptr %root_0, i16 %10, ptr %predecessor_0, ptr %key_0, ptr %new_node_0)
  ret void
end_if_0:
  br label %end_if_1
end_if_1:
  %18 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %19 = load i16, ptr %18
  %20 = sub i16 %19, 1
  %21 = load i16, ptr %stack.ptr_0
  br label %for_begin_0
for_begin_0:
  %22 = phi i16 [%20, %end_if_1], [%37, %for_body_0]
  %23 = icmp uge i16 %22, %21
  br i1 %23, label %for_body_0, label %for_end_0
for_body_0:
  %24 = add i16 %22, 1
  %25 = add i16 %22, 2
  %26 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %22
  %27 = load [3 x i32], ptr %26
  %28 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %24
  store [3 x i32] %27, ptr %28
  %29 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %24
  %30 = load ptr, ptr %29
  %31 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %25
  store ptr %30, ptr %31
  %32 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %25
  %33 = load ptr, ptr %32
  %34 = getelementptr %node_t_8, ptr %33, i32 0, i32 0, i32 1
  %35 = load i16, ptr %34
  %36 = add i16 1, %35
  store i16 %36, ptr %34
  %37 = sub i16 %22, 1
  br label %for_begin_0
for_end_0:
  %38 = load [3 x i32], ptr %key_0
  %39 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %21
  store [3 x i32] %38, ptr %39
  %40 = add i16 %21, 1
  %41 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %40
  store ptr %new_node_0, ptr %41
  %42 = getelementptr %node_t_8, ptr %new_node_0, i32 0, i32 0, i32 0
  store ptr %node_0, ptr %42
  %43 = getelementptr %node_t_8, ptr %new_node_0, i32 0, i32 0, i32 1
  store i16 %40, ptr %43
  %44 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %45 = load i16, ptr %44
  %46 = add i16 1, %45
  store i16 %46, ptr %44
  ret void
}

define external ccc i16 @eclair_btree_node_rebalance_or_split_8(ptr %node_0, ptr %root_0, i16 %idx_0) {
start:
  %0 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 1
  %3 = load i16, ptr %2
  %4 = icmp ne ptr %1, zeroinitializer
  %5 = icmp ugt i16 %3, 0
  %6 = and i1 %4, %5
  br i1 %6, label %rebalance_0, label %split_0
rebalance_0:
  %7 = sub i16 %3, 1
  %8 = getelementptr %inner_node_t_8, ptr %1, i32 0, i32 1, i16 %7
  %9 = load ptr, ptr %8
  %10 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %11 = load i16, ptr %10
  %12 = sub i16 20, %11
  %13 = icmp slt i16 %12, %idx_0
  %14 = select i1 %13, i16 %12, i16 %idx_0
  %15 = icmp ugt i16 %14, 0
  br i1 %15, label %if_0, label %end_if_1
if_0:
  %16 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 1
  %17 = load i16, ptr %16
  %18 = sub i16 %17, 1
  %19 = getelementptr %inner_node_t_8, ptr %1, i32 0, i32 0, i32 1, i16 %18
  %20 = load [3 x i32], ptr %19
  %21 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %22 = load i16, ptr %21
  %23 = getelementptr %node_t_8, ptr %9, i32 0, i32 1, i16 %22
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
  %29 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %25
  %30 = load [3 x i32], ptr %29
  %31 = getelementptr %node_t_8, ptr %9, i32 0, i32 1, i16 %28
  store [3 x i32] %30, ptr %31
  %32 = add i16 1, %25
  br label %for_begin_0
for_end_0:
  %33 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %24
  %34 = load [3 x i32], ptr %33
  store [3 x i32] %34, ptr %19
  %35 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %36 = load i16, ptr %35
  %37 = sub i16 %36, %14
  br label %for_begin_1
for_begin_1:
  %38 = phi i16 [0, %for_end_0], [%44, %for_body_1]
  %39 = icmp ult i16 %38, %37
  br i1 %39, label %for_body_1, label %for_end_1
for_body_1:
  %40 = add i16 %38, %14
  %41 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %40
  %42 = load [3 x i32], ptr %41
  %43 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 1, i16 %38
  store [3 x i32] %42, ptr %43
  %44 = add i16 1, %38
  br label %for_begin_1
for_end_1:
  %45 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 3
  %46 = load i1, ptr %45
  %47 = icmp eq i1 %46, 1
  br i1 %47, label %if_1, label %end_if_0
if_1:
  br label %for_begin_2
for_begin_2:
  %48 = phi i16 [0, %if_1], [%61, %for_body_2]
  %49 = icmp ult i16 %48, %14
  br i1 %49, label %for_body_2, label %for_end_2
for_body_2:
  %50 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %51 = load i16, ptr %50
  %52 = add i16 %51, 1
  %53 = add i16 %48, %52
  %54 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %48
  %55 = load ptr, ptr %54
  %56 = getelementptr %inner_node_t_8, ptr %9, i32 0, i32 1, i16 %53
  store ptr %55, ptr %56
  %57 = getelementptr %inner_node_t_8, ptr %9, i32 0, i32 1, i16 %53
  %58 = load ptr, ptr %57
  %59 = getelementptr %node_t_8, ptr %58, i32 0, i32 0, i32 0
  store ptr %9, ptr %59
  %60 = getelementptr %node_t_8, ptr %58, i32 0, i32 0, i32 1
  store i16 %53, ptr %60
  %61 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  %62 = sub i16 %36, %14
  %63 = add i16 1, %62
  br label %for_begin_3
for_begin_3:
  %64 = phi i16 [0, %for_end_2], [%73, %for_body_3]
  %65 = icmp ult i16 %64, %63
  br i1 %65, label %for_body_3, label %for_end_3
for_body_3:
  %66 = add i16 %64, %14
  %67 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %66
  %68 = load ptr, ptr %67
  %69 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %64
  store ptr %68, ptr %69
  %70 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %64
  %71 = load ptr, ptr %70
  %72 = getelementptr %node_t_8, ptr %71, i32 0, i32 0, i32 1
  store i16 %64, ptr %72
  %73 = add i16 1, %64
  br label %for_begin_3
for_end_3:
  br label %end_if_0
end_if_0:
  %74 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 %75, %14
  store i16 %76, ptr %74
  %77 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %78 = load i16, ptr %77
  %79 = sub i16 %78, %14
  store i16 %79, ptr %77
  ret i16 %14
end_if_1:
  br label %split_0
split_0:
  call ccc void @eclair_btree_node_split_8(ptr %node_0, ptr %root_0)
  ret i16 0
}

define external ccc i1 @eclair_btree_insert_value_8(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca ptr
  %stack.ptr_1 = alloca i16
  %0 = call ccc i1 @eclair_btree_is_empty_8(ptr %tree_0)
  br i1 %0, label %empty_0, label %non_empty_0
empty_0:
  %1 = call ccc ptr @eclair_btree_node_new_8(i1 0)
  %2 = getelementptr %node_t_8, ptr %1, i32 0, i32 0, i32 2
  store i16 1, ptr %2
  %3 = load [3 x i32], ptr %val_0
  %4 = getelementptr %node_t_8, ptr %1, i32 0, i32 1, i16 0
  store [3 x i32] %3, ptr %4
  %5 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 1
  store ptr %1, ptr %6
  br label %inserted_new_value_0
non_empty_0:
  %7 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %8 = load ptr, ptr %7
  store ptr %8, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %9 = load ptr, ptr %stack.ptr_0
  %10 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 3
  %11 = load i1, ptr %10
  %12 = icmp eq i1 %11, 1
  br i1 %12, label %inner_0, label %leaf_0
inner_0:
  %13 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %14 = load i16, ptr %13
  %15 = getelementptr %node_t_8, ptr %9, i32 0, i32 1, i16 0
  %16 = getelementptr %node_t_8, ptr %9, i32 0, i32 1, i16 %14
  %17 = call ccc ptr @eclair_btree_linear_search_lower_bound_8(ptr %val_0, ptr %15, ptr %16)
  %18 = ptrtoint ptr %17 to i64
  %19 = ptrtoint ptr %15 to i64
  %20 = sub i64 %18, %19
  %21 = trunc i64 %20 to i16
  %22 = udiv i16 %21, 12
  %23 = icmp ne ptr %17, %16
  %24 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  %26 = and i1 %23, %25
  br i1 %26, label %no_insert_0, label %inner_continue_insert_0
inner_continue_insert_0:
  %27 = getelementptr %inner_node_t_8, ptr %9, i32 0, i32 1, i16 %22
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %29 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %30 = load i16, ptr %29
  %31 = getelementptr %node_t_8, ptr %9, i32 0, i32 1, i16 0
  %32 = getelementptr %node_t_8, ptr %9, i32 0, i32 1, i16 %30
  %33 = call ccc ptr @eclair_btree_linear_search_upper_bound_8(ptr %val_0, ptr %31, ptr %32)
  %34 = ptrtoint ptr %33 to i64
  %35 = ptrtoint ptr %31 to i64
  %36 = sub i64 %34, %35
  %37 = trunc i64 %36 to i16
  %38 = udiv i16 %37, 12
  store i16 %38, ptr %stack.ptr_1
  %39 = icmp ne ptr %33, %31
  %40 = getelementptr [3 x i32], ptr %33, i32 -1
  %41 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %40, ptr %val_0)
  %42 = icmp eq i8 0, %41
  %43 = and i1 %39, %42
  br i1 %43, label %no_insert_0, label %leaf_continue_insert_0
leaf_continue_insert_0:
  %44 = icmp uge i16 %30, 20
  br i1 %44, label %split_0, label %no_split_0
split_0:
  %45 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %46 = load i16, ptr %stack.ptr_1
  %47 = call ccc i16 @eclair_btree_node_rebalance_or_split_8(ptr %9, ptr %45, i16 %46)
  %48 = sub i16 %46, %47
  store i16 %48, ptr %stack.ptr_1
  %49 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %50 = load i16, ptr %49
  %51 = icmp ugt i16 %48, %50
  br i1 %51, label %if_0, label %end_if_0
if_0:
  %52 = add i16 %50, 1
  %53 = sub i16 %48, %52
  store i16 %53, ptr %stack.ptr_1
  %54 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 0
  %55 = load ptr, ptr %54
  %56 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 1
  %57 = load i16, ptr %56
  %58 = add i16 1, %57
  %59 = getelementptr %inner_node_t_8, ptr %55, i32 0, i32 1, i16 %58
  %60 = load ptr, ptr %59
  store ptr %60, ptr %stack.ptr_0
  br label %end_if_0
end_if_0:
  br label %no_split_0
no_split_0:
  %61 = load ptr, ptr %stack.ptr_0
  %62 = load i16, ptr %stack.ptr_1
  %63 = getelementptr %node_t_8, ptr %61, i32 0, i32 0, i32 2
  %64 = load i16, ptr %63
  br label %for_begin_0
for_begin_0:
  %65 = phi i16 [%64, %no_split_0], [%71, %for_body_0]
  %66 = icmp ugt i16 %65, %62
  br i1 %66, label %for_body_0, label %for_end_0
for_body_0:
  %67 = sub i16 %65, 1
  %68 = getelementptr %node_t_8, ptr %61, i32 0, i32 1, i16 %67
  %69 = load [3 x i32], ptr %68
  %70 = getelementptr %node_t_8, ptr %61, i32 0, i32 1, i16 %65
  store [3 x i32] %69, ptr %70
  %71 = sub i16 %65, 1
  br label %for_begin_0
for_end_0:
  %72 = load [3 x i32], ptr %val_0
  %73 = getelementptr %node_t_8, ptr %61, i32 0, i32 1, i16 %62
  store [3 x i32] %72, ptr %73
  %74 = getelementptr %node_t_8, ptr %61, i32 0, i32 0, i32 2
  %75 = load i16, ptr %74
  %76 = add i16 1, %75
  store i16 %76, ptr %74
  br label %inserted_new_value_0
no_insert_0:
  ret i1 0
inserted_new_value_0:
  ret i1 1
}

define external ccc void @eclair_btree_insert_range__8(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_8(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_8(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_8(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_8(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_begin_8(ptr %tree_0, ptr %result_0) {
start:
  %0 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 1
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_iterator_t_8, ptr %result_0, i32 0, i32 0
  store ptr %1, ptr %2
  %3 = getelementptr %btree_iterator_t_8, ptr %result_0, i32 0, i32 1
  store i16 0, ptr %3
  ret void
}

define external ccc void @eclair_btree_end_8(ptr %tree_0, ptr %result_0) {
start:
  call ccc void @eclair_btree_iterator_end_init_8(ptr %result_0)
  ret void
}

define external ccc i1 @eclair_btree_contains_8(ptr %tree_0, ptr %val_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_1 = alloca %btree_iterator_t_8, i32 1
  call ccc void @eclair_btree_find_8(ptr %tree_0, ptr %val_0, ptr %stack.ptr_0)
  call ccc void @eclair_btree_end_8(ptr %tree_0, ptr %stack.ptr_1)
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_8(ptr %stack.ptr_0, ptr %stack.ptr_1)
  %1 = select i1 %0, i1 0, i1 1
  ret i1 %1
}

define external ccc void @eclair_btree_find_8(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_8(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_8(ptr %result_0)
  ret void
end_if_0:
  %1 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_0
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_0
  %4 = getelementptr %node_t_8, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_8, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_8, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_8(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 12
  %14 = icmp ult ptr %8, %7
  %15 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  %17 = and i1 %14, %16
  br i1 %17, label %if_1, label %end_if_1
if_1:
  call ccc void @eclair_btree_iterator_init_8(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %18 = getelementptr %node_t_8, ptr %3, i32 0, i32 0, i32 3
  %19 = load i1, ptr %18
  %20 = icmp eq i1 %19, 0
  br i1 %20, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_end_init_8(ptr %result_0)
  ret void
end_if_2:
  %21 = getelementptr %inner_node_t_8, ptr %3, i32 0, i32 1, i16 %13
  %22 = load ptr, ptr %21
  store ptr %22, ptr %stack.ptr_0
  br label %loop_0
}

define external ccc void @eclair_btree_lower_bound_8(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_8(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_8(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_8(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_8, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_8, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_8, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_lower_bound_8(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 12
  %14 = getelementptr %node_t_8, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_8, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_8, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_8, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_8, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_8(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  %25 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  %27 = and i1 %24, %26
  br i1 %27, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_8(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br i1 %24, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_init_8(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_3
end_if_3:
  %28 = getelementptr %inner_node_t_8, ptr %3, i32 0, i32 1, i16 %13
  %29 = load ptr, ptr %28
  store ptr %29, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_upper_bound_8(ptr %tree_0, ptr %val_0, ptr %result_0) {
start:
  %stack.ptr_0 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_1 = alloca ptr
  %0 = call ccc i1 @eclair_btree_is_empty_8(ptr %tree_0)
  br i1 %0, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_iterator_end_init_8(ptr %result_0)
  ret void
end_if_0:
  call ccc void @eclair_btree_iterator_end_init_8(ptr %stack.ptr_0)
  %1 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %2 = load ptr, ptr %1
  store ptr %2, ptr %stack.ptr_1
  br label %loop_0
loop_0:
  %3 = load ptr, ptr %stack.ptr_1
  %4 = getelementptr %node_t_8, ptr %3, i32 0, i32 0, i32 2
  %5 = load i16, ptr %4
  %6 = getelementptr %node_t_8, ptr %3, i32 0, i32 1, i16 0
  %7 = getelementptr %node_t_8, ptr %3, i32 0, i32 1, i16 %5
  %8 = call ccc ptr @eclair_btree_linear_search_upper_bound_8(ptr %val_0, ptr %6, ptr %7)
  %9 = ptrtoint ptr %8 to i64
  %10 = ptrtoint ptr %6 to i64
  %11 = sub i64 %9, %10
  %12 = trunc i64 %11 to i16
  %13 = udiv i16 %12, 12
  %14 = getelementptr %node_t_8, ptr %3, i32 0, i32 0, i32 3
  %15 = load i1, ptr %14
  %16 = icmp eq i1 %15, 0
  br i1 %16, label %if_1, label %end_if_1
if_1:
  %17 = icmp eq ptr %8, %7
  br i1 %17, label %handle_last_0, label %handle_not_last_0
handle_last_0:
  %18 = getelementptr %btree_iterator_t_8, ptr %stack.ptr_0, i32 0, i32 0
  %19 = load ptr, ptr %18
  %20 = getelementptr %btree_iterator_t_8, ptr %result_0, i32 0, i32 0
  store ptr %19, ptr %20
  %21 = getelementptr %btree_iterator_t_8, ptr %stack.ptr_0, i32 0, i32 1
  %22 = load i16, ptr %21
  %23 = getelementptr %btree_iterator_t_8, ptr %result_0, i32 0, i32 1
  store i16 %22, ptr %23
  ret void
handle_not_last_0:
  call ccc void @eclair_btree_iterator_init_8(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  %24 = icmp ne ptr %8, %7
  br i1 %24, label %if_2, label %end_if_2
if_2:
  call ccc void @eclair_btree_iterator_init_8(ptr %result_0, ptr %3, i16 %13)
  br label %end_if_2
end_if_2:
  %25 = getelementptr %inner_node_t_8, ptr %3, i32 0, i32 1, i16 %13
  %26 = load ptr, ptr %25
  store ptr %26, ptr %stack.ptr_1
  br label %loop_0
}

define external ccc void @eclair_btree_node_delete_8(ptr %node_0) {
start:
  %0 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 3
  %1 = load i1, ptr %0
  %2 = icmp eq i1 %1, 1
  br i1 %2, label %if_0, label %end_if_1
if_0:
  %3 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %4 = load i16, ptr %3
  br label %for_begin_0
for_begin_0:
  %5 = phi i16 [0, %if_0], [%10, %end_if_0]
  %6 = icmp ule i16 %5, %4
  br i1 %6, label %for_body_0, label %for_end_0
for_body_0:
  %7 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %5
  %8 = load ptr, ptr %7
  %9 = icmp ne ptr %8, zeroinitializer
  br i1 %9, label %if_1, label %end_if_0
if_1:
  call ccc void @eclair_btree_node_delete_8(ptr %8)
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

define external ccc void @eclair_btree_clear_8(ptr %tree_0) {
start:
  %0 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = icmp ne ptr %1, zeroinitializer
  br i1 %2, label %if_0, label %end_if_0
if_0:
  call ccc void @eclair_btree_node_delete_8(ptr %1)
  %3 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  store ptr zeroinitializer, ptr %3
  %4 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 1
  store ptr zeroinitializer, ptr %4
  br label %end_if_0
end_if_0:
  ret void
}

define external ccc void @eclair_btree_swap_8(ptr %lhs_0, ptr %rhs_0) {
start:
  %0 = getelementptr %btree_t_8, ptr %lhs_0, i32 0, i32 0
  %1 = load ptr, ptr %0
  %2 = getelementptr %btree_t_8, ptr %rhs_0, i32 0, i32 0
  %3 = load ptr, ptr %2
  %4 = getelementptr %btree_t_8, ptr %lhs_0, i32 0, i32 0
  store ptr %3, ptr %4
  %5 = getelementptr %btree_t_8, ptr %rhs_0, i32 0, i32 0
  store ptr %1, ptr %5
  %6 = getelementptr %btree_t_8, ptr %lhs_0, i32 0, i32 1
  %7 = load ptr, ptr %6
  %8 = getelementptr %btree_t_8, ptr %rhs_0, i32 0, i32 1
  %9 = load ptr, ptr %8
  %10 = getelementptr %btree_t_8, ptr %lhs_0, i32 0, i32 1
  store ptr %9, ptr %10
  %11 = getelementptr %btree_t_8, ptr %rhs_0, i32 0, i32 1
  store ptr %7, ptr %11
  ret void
}

@specialize_debug_info.btree__1__0__256__linear = global i32 6

@specialize_debug_info.btree__2__0_1__256__linear = global i32 1

@specialize_debug_info.btree__2__1__256__linear = global i32 2

@specialize_debug_info.btree__2__1_0__256__linear = global i32 7

@specialize_debug_info.btree__3__0_1_2__256__linear = global i32 0

@specialize_debug_info.btree__3__2__256__linear = global i32 8

@specialize_debug_info.btree__4__0_1_2_3__256__linear = global i32 4

@specialize_debug_info.btree__4__1__256__linear = global i32 5

@specialize_debug_info.btree__4__2_3_0_1__256__linear = global i32 3

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

%program = type {%symbol_table, %btree_t_0, %btree_t_0, %btree_t_1, %btree_t_2, %btree_t_0, %btree_t_3, %btree_t_0, %btree_t_0, %btree_t_4, %btree_t_5, %btree_t_6, %btree_t_6, %btree_t_1, %btree_t_1, %btree_t_2, %btree_t_6, %btree_t_1, %btree_t_6, %btree_t_0, %btree_t_1, %btree_t_6, %btree_t_1, %btree_t_1, %btree_t_7, %btree_t_0, %btree_t_0, %btree_t_1, %btree_t_1, %btree_t_6, %btree_t_6, %btree_t_6, %btree_t_6, %btree_t_6, %btree_t_1, %btree_t_1, %btree_t_1, %btree_t_6, %btree_t_6, %btree_t_1, %btree_t_1, %btree_t_6, %btree_t_1, %btree_t_6, %btree_t_0, %btree_t_1, %btree_t_6, %btree_t_6, %btree_t_0, %btree_t_1, %btree_t_1, %btree_t_2, %btree_t_0, %btree_t_0, %btree_t_8, %btree_t_0, %btree_t_1, %btree_t_2, %btree_t_6, %btree_t_1, %btree_t_2, %btree_t_6, %btree_t_1, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_1, %btree_t_2, %btree_t_6, %btree_t_1, %btree_t_1, %btree_t_0, %btree_t_0, %btree_t_0}

@string_literal_0 = global [11 x i8] [i8 108, i8 105, i8 116, i8 95, i8 110, i8 117, i8 109, i8 98, i8 101, i8 114, i8 0]

@string_literal_1 = global [11 x i8] [i8 108, i8 105, i8 116, i8 95, i8 115, i8 116, i8 114, i8 105, i8 110, i8 103, i8 0]

@string_literal_2 = global [9 x i8] [i8 118, i8 97, i8 114, i8 105, i8 97, i8 98, i8 108, i8 101, i8 0]

@string_literal_3 = global [11 x i8] [i8 99, i8 111, i8 110, i8 115, i8 116, i8 114, i8 97, i8 105, i8 110, i8 116, i8 0]

@string_literal_4 = global [6 x i8] [i8 98, i8 105, i8 110, i8 111, i8 112, i8 0]

@string_literal_5 = global [5 x i8] [i8 97, i8 116, i8 111, i8 109, i8 0]

@string_literal_6 = global [9 x i8] [i8 97, i8 116, i8 111, i8 109, i8 95, i8 97, i8 114, i8 103, i8 0]

@string_literal_7 = global [5 x i8] [i8 114, i8 117, i8 108, i8 101, i8 0]

@string_literal_8 = global [9 x i8] [i8 114, i8 117, i8 108, i8 101, i8 95, i8 97, i8 114, i8 103, i8 0]

@string_literal_9 = global [12 x i8] [i8 114, i8 117, i8 108, i8 101, i8 95, i8 99, i8 108, i8 97, i8 117, i8 115, i8 101, i8 0]

@string_literal_10 = global [9 x i8] [i8 110, i8 101, i8 103, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_11 = global [15 x i8] [i8 105, i8 110, i8 112, i8 117, i8 116, i8 95, i8 114, i8 101, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_12 = global [16 x i8] [i8 111, i8 117, i8 116, i8 112, i8 117, i8 116, i8 95, i8 114, i8 101, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_13 = global [18 x i8] [i8 105, i8 110, i8 116, i8 101, i8 114, i8 110, i8 97, i8 108, i8 95, i8 114, i8 101, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_14 = global [18 x i8] [i8 101, i8 120, i8 116, i8 101, i8 114, i8 110, i8 95, i8 100, i8 101, i8 102, i8 105, i8 110, i8 105, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_15 = global [13 x i8] [i8 100, i8 101, i8 99, i8 108, i8 97, i8 114, i8 101, i8 95, i8 116, i8 121, i8 112, i8 101, i8 0]

@string_literal_16 = global [19 x i8] [i8 109, i8 111, i8 100, i8 117, i8 108, i8 101, i8 95, i8 100, i8 101, i8 99, i8 108, i8 97, i8 114, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_17 = global [13 x i8] [i8 115, i8 99, i8 111, i8 112, i8 101, i8 100, i8 95, i8 118, i8 97, i8 108, i8 117, i8 101, i8 0]

@string_literal_18 = global [14 x i8] [i8 114, i8 101, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 95, i8 97, i8 116, i8 111, i8 109, i8 0]

@string_literal_19 = global [12 x i8] [i8 101, i8 120, i8 116, i8 101, i8 114, i8 110, i8 95, i8 97, i8 116, i8 111, i8 109, i8 0]

@string_literal_20 = global [14 x i8] [i8 103, i8 114, i8 111, i8 117, i8 110, i8 100, i8 101, i8 100, i8 95, i8 110, i8 111, i8 100, i8 101, i8 0]

@string_literal_21 = global [7 x i8] [i8 97, i8 115, i8 115, i8 105, i8 103, i8 110, i8 0]

@string_literal_22 = global [14 x i8] [i8 105, i8 110, i8 101, i8 113, i8 117, i8 97, i8 108, i8 105, i8 116, i8 121, i8 95, i8 111, i8 112, i8 0]

@string_literal_23 = global [20 x i8] [i8 104, i8 97, i8 115, i8 95, i8 111, i8 117, i8 116, i8 112, i8 117, i8 116, i8 95, i8 114, i8 101, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_24 = global [22 x i8] [i8 108, i8 105, i8 116, i8 101, i8 114, i8 97, i8 108, i8 95, i8 99, i8 111, i8 110, i8 116, i8 114, i8 97, i8 100, i8 105, i8 99, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_25 = global [9 x i8] [i8 119, i8 105, i8 108, i8 100, i8 99, i8 97, i8 114, i8 100, i8 0]

@string_literal_26 = global [14 x i8] [i8 114, i8 117, i8 108, i8 101, i8 95, i8 104, i8 101, i8 97, i8 100, i8 95, i8 118, i8 97, i8 114, i8 0]

@string_literal_27 = global [6 x i8] [i8 97, i8 108, i8 105, i8 97, i8 115, i8 0]

@string_literal_28 = global [10 x i8] [i8 112, i8 111, i8 105, i8 110, i8 116, i8 115, i8 95, i8 116, i8 111, i8 0]

@string_literal_29 = global [11 x i8] [i8 100, i8 101, i8 112, i8 101, i8 110, i8 100, i8 115, i8 95, i8 111, i8 110, i8 0]

@string_literal_30 = global [22 x i8] [i8 116, i8 114, i8 97, i8 110, i8 115, i8 105, i8 116, i8 105, i8 118, i8 101, i8 95, i8 100, i8 101, i8 112, i8 101, i8 110, i8 100, i8 115, i8 95, i8 111, i8 110, i8 0]

@string_literal_31 = global [7 x i8] [i8 115, i8 111, i8 117, i8 114, i8 99, i8 101, i8 0]

@string_literal_32 = global [16 x i8] [i8 104, i8 97, i8 115, i8 95, i8 100, i8 101, i8 102, i8 105, i8 110, i8 105, i8 116, i8 105, i8 111, i8 110, i8 115, i8 0]

@string_literal_33 = global [10 x i8] [i8 108, i8 105, i8 118, i8 101, i8 95, i8 114, i8 117, i8 108, i8 101, i8 0]

@string_literal_34 = global [17 x i8] [i8 100, i8 101, i8 112, i8 101, i8 110, i8 100, i8 101, i8 110, i8 99, i8 121, i8 95, i8 99, i8 121, i8 99, i8 108, i8 101, i8 0]

@string_literal_35 = global [11 x i8] [i8 114, i8 117, i8 108, i8 101, i8 95, i8 115, i8 99, i8 111, i8 112, i8 101, i8 0]

@string_literal_36 = global [21 x i8] [i8 99, i8 111, i8 110, i8 115, i8 116, i8 114, i8 97, i8 105, i8 110, i8 101, i8 100, i8 95, i8 114, i8 117, i8 108, i8 101, i8 95, i8 118, i8 97, i8 114, i8 0]

@string_literal_37 = global [18 x i8] [i8 103, i8 114, i8 111, i8 117, i8 110, i8 100, i8 101, i8 100, i8 95, i8 118, i8 97, i8 114, i8 105, i8 97, i8 98, i8 108, i8 101, i8 0]

@string_literal_38 = global [20 x i8] [i8 117, i8 110, i8 103, i8 114, i8 111, i8 117, i8 110, i8 100, i8 101, i8 100, i8 95, i8 118, i8 97, i8 114, i8 105, i8 97, i8 98, i8 108, i8 101, i8 0]

@string_literal_39 = global [25 x i8] [i8 117, i8 110, i8 103, i8 114, i8 111, i8 117, i8 110, i8 100, i8 101, i8 100, i8 95, i8 101, i8 120, i8 116, i8 101, i8 114, i8 110, i8 97, i8 108, i8 95, i8 97, i8 116, i8 111, i8 109, i8 0]

@string_literal_40 = global [17 x i8] [i8 119, i8 105, i8 108, i8 100, i8 99, i8 97, i8 114, i8 100, i8 95, i8 105, i8 110, i8 95, i8 102, i8 97, i8 99, i8 116, i8 0]

@string_literal_41 = global [19 x i8] [i8 119, i8 105, i8 108, i8 100, i8 99, i8 97, i8 114, i8 100, i8 95, i8 105, i8 110, i8 95, i8 101, i8 120, i8 116, i8 101, i8 114, i8 110, i8 0]

@string_literal_42 = global [22 x i8] [i8 119, i8 105, i8 108, i8 100, i8 99, i8 97, i8 114, i8 100, i8 95, i8 105, i8 110, i8 95, i8 114, i8 117, i8 108, i8 101, i8 95, i8 104, i8 101, i8 97, i8 100, i8 0]

@string_literal_43 = global [23 x i8] [i8 119, i8 105, i8 108, i8 100, i8 99, i8 97, i8 114, i8 100, i8 95, i8 105, i8 110, i8 95, i8 99, i8 111, i8 110, i8 115, i8 116, i8 114, i8 97, i8 105, i8 110, i8 116, i8 0]

@string_literal_44 = global [18 x i8] [i8 119, i8 105, i8 108, i8 100, i8 99, i8 97, i8 114, i8 100, i8 95, i8 105, i8 110, i8 95, i8 98, i8 105, i8 110, i8 111, i8 112, i8 0]

@string_literal_45 = global [23 x i8] [i8 117, i8 110, i8 99, i8 111, i8 110, i8 115, i8 116, i8 114, i8 97, i8 105, i8 110, i8 101, i8 100, i8 95, i8 114, i8 117, i8 108, i8 101, i8 95, i8 118, i8 97, i8 114, i8 0]

@string_literal_46 = global [24 x i8] [i8 114, i8 117, i8 108, i8 101, i8 95, i8 119, i8 105, i8 116, i8 104, i8 95, i8 99, i8 111, i8 110, i8 116, i8 114, i8 97, i8 100, i8 105, i8 99, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_47 = global [10 x i8] [i8 100, i8 101, i8 97, i8 100, i8 95, i8 99, i8 111, i8 100, i8 101, i8 0]

@string_literal_48 = global [19 x i8] [i8 110, i8 111, i8 95, i8 111, i8 117, i8 116, i8 112, i8 117, i8 116, i8 95, i8 114, i8 101, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_49 = global [23 x i8] [i8 100, i8 101, i8 97, i8 100, i8 95, i8 105, i8 110, i8 116, i8 101, i8 114, i8 110, i8 97, i8 108, i8 95, i8 114, i8 101, i8 108, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_50 = global [24 x i8] [i8 99, i8 111, i8 110, i8 102, i8 108, i8 105, i8 99, i8 116, i8 105, i8 110, i8 103, i8 95, i8 100, i8 101, i8 102, i8 105, i8 110, i8 105, i8 116, i8 105, i8 111, i8 110, i8 115, i8 0]

@string_literal_51 = global [20 x i8] [i8 101, i8 120, i8 116, i8 101, i8 114, i8 110, i8 95, i8 117, i8 115, i8 101, i8 100, i8 95, i8 97, i8 115, i8 95, i8 102, i8 97, i8 99, i8 116, i8 0]

@string_literal_52 = global [20 x i8] [i8 101, i8 120, i8 116, i8 101, i8 114, i8 110, i8 95, i8 117, i8 115, i8 101, i8 100, i8 95, i8 97, i8 115, i8 95, i8 114, i8 117, i8 108, i8 101, i8 0]

@string_literal_53 = global [16 x i8] [i8 99, i8 121, i8 99, i8 108, i8 105, i8 99, i8 95, i8 110, i8 101, i8 103, i8 97, i8 116, i8 105, i8 111, i8 110, i8 0]

@string_literal_54 = global [2 x i8] [i8 95, i8 0]

@string_literal_55 = global [3 x i8] [i8 33, i8 61, i8 0]

@string_literal_56 = global [2 x i8] [i8 60, i8 0]

@string_literal_57 = global [3 x i8] [i8 60, i8 61, i8 0]

@string_literal_58 = global [2 x i8] [i8 62, i8 0]

@string_literal_59 = global [3 x i8] [i8 62, i8 61, i8 0]

@string_literal_60 = global [2 x i8] [i8 61, i8 0]

define external ccc ptr @eclair_program_init() "wasm-export-name"="eclair_program_init" {
start:
  %stack.ptr_0 = alloca %symbol_t, i32 1
  %stack.ptr_1 = alloca %symbol_t, i32 1
  %stack.ptr_2 = alloca %symbol_t, i32 1
  %stack.ptr_3 = alloca %symbol_t, i32 1
  %stack.ptr_4 = alloca %symbol_t, i32 1
  %stack.ptr_5 = alloca %symbol_t, i32 1
  %stack.ptr_6 = alloca %symbol_t, i32 1
  %stack.ptr_7 = alloca %symbol_t, i32 1
  %stack.ptr_8 = alloca %symbol_t, i32 1
  %stack.ptr_9 = alloca %symbol_t, i32 1
  %stack.ptr_10 = alloca %symbol_t, i32 1
  %stack.ptr_11 = alloca %symbol_t, i32 1
  %stack.ptr_12 = alloca %symbol_t, i32 1
  %stack.ptr_13 = alloca %symbol_t, i32 1
  %stack.ptr_14 = alloca %symbol_t, i32 1
  %stack.ptr_15 = alloca %symbol_t, i32 1
  %stack.ptr_16 = alloca %symbol_t, i32 1
  %stack.ptr_17 = alloca %symbol_t, i32 1
  %stack.ptr_18 = alloca %symbol_t, i32 1
  %stack.ptr_19 = alloca %symbol_t, i32 1
  %stack.ptr_20 = alloca %symbol_t, i32 1
  %stack.ptr_21 = alloca %symbol_t, i32 1
  %stack.ptr_22 = alloca %symbol_t, i32 1
  %stack.ptr_23 = alloca %symbol_t, i32 1
  %stack.ptr_24 = alloca %symbol_t, i32 1
  %stack.ptr_25 = alloca %symbol_t, i32 1
  %stack.ptr_26 = alloca %symbol_t, i32 1
  %stack.ptr_27 = alloca %symbol_t, i32 1
  %stack.ptr_28 = alloca %symbol_t, i32 1
  %stack.ptr_29 = alloca %symbol_t, i32 1
  %stack.ptr_30 = alloca %symbol_t, i32 1
  %stack.ptr_31 = alloca %symbol_t, i32 1
  %stack.ptr_32 = alloca %symbol_t, i32 1
  %stack.ptr_33 = alloca %symbol_t, i32 1
  %stack.ptr_34 = alloca %symbol_t, i32 1
  %stack.ptr_35 = alloca %symbol_t, i32 1
  %stack.ptr_36 = alloca %symbol_t, i32 1
  %stack.ptr_37 = alloca %symbol_t, i32 1
  %stack.ptr_38 = alloca %symbol_t, i32 1
  %stack.ptr_39 = alloca %symbol_t, i32 1
  %stack.ptr_40 = alloca %symbol_t, i32 1
  %stack.ptr_41 = alloca %symbol_t, i32 1
  %stack.ptr_42 = alloca %symbol_t, i32 1
  %stack.ptr_43 = alloca %symbol_t, i32 1
  %stack.ptr_44 = alloca %symbol_t, i32 1
  %stack.ptr_45 = alloca %symbol_t, i32 1
  %stack.ptr_46 = alloca %symbol_t, i32 1
  %stack.ptr_47 = alloca %symbol_t, i32 1
  %stack.ptr_48 = alloca %symbol_t, i32 1
  %stack.ptr_49 = alloca %symbol_t, i32 1
  %stack.ptr_50 = alloca %symbol_t, i32 1
  %stack.ptr_51 = alloca %symbol_t, i32 1
  %stack.ptr_52 = alloca %symbol_t, i32 1
  %stack.ptr_53 = alloca %symbol_t, i32 1
  %stack.ptr_54 = alloca %symbol_t, i32 1
  %stack.ptr_55 = alloca %symbol_t, i32 1
  %stack.ptr_56 = alloca %symbol_t, i32 1
  %stack.ptr_57 = alloca %symbol_t, i32 1
  %stack.ptr_58 = alloca %symbol_t, i32 1
  %stack.ptr_59 = alloca %symbol_t, i32 1
  %stack.ptr_60 = alloca %symbol_t, i32 1
  %0 = call ccc ptr @malloc(i32 2728)
  %1 = getelementptr %program, ptr %0, i32 0, i32 0
  call ccc void @eclair_symbol_table_init(ptr %1)
  %2 = getelementptr %program, ptr %0, i32 0, i32 1
  call ccc void @eclair_btree_init_empty_0(ptr %2)
  %3 = getelementptr %program, ptr %0, i32 0, i32 2
  call ccc void @eclair_btree_init_empty_0(ptr %3)
  %4 = getelementptr %program, ptr %0, i32 0, i32 3
  call ccc void @eclair_btree_init_empty_1(ptr %4)
  %5 = getelementptr %program, ptr %0, i32 0, i32 4
  call ccc void @eclair_btree_init_empty_2(ptr %5)
  %6 = getelementptr %program, ptr %0, i32 0, i32 5
  call ccc void @eclair_btree_init_empty_0(ptr %6)
  %7 = getelementptr %program, ptr %0, i32 0, i32 6
  call ccc void @eclair_btree_init_empty_3(ptr %7)
  %8 = getelementptr %program, ptr %0, i32 0, i32 7
  call ccc void @eclair_btree_init_empty_0(ptr %8)
  %9 = getelementptr %program, ptr %0, i32 0, i32 8
  call ccc void @eclair_btree_init_empty_0(ptr %9)
  %10 = getelementptr %program, ptr %0, i32 0, i32 9
  call ccc void @eclair_btree_init_empty_4(ptr %10)
  %11 = getelementptr %program, ptr %0, i32 0, i32 10
  call ccc void @eclair_btree_init_empty_5(ptr %11)
  %12 = getelementptr %program, ptr %0, i32 0, i32 11
  call ccc void @eclair_btree_init_empty_6(ptr %12)
  %13 = getelementptr %program, ptr %0, i32 0, i32 12
  call ccc void @eclair_btree_init_empty_6(ptr %13)
  %14 = getelementptr %program, ptr %0, i32 0, i32 13
  call ccc void @eclair_btree_init_empty_1(ptr %14)
  %15 = getelementptr %program, ptr %0, i32 0, i32 14
  call ccc void @eclair_btree_init_empty_1(ptr %15)
  %16 = getelementptr %program, ptr %0, i32 0, i32 15
  call ccc void @eclair_btree_init_empty_2(ptr %16)
  %17 = getelementptr %program, ptr %0, i32 0, i32 16
  call ccc void @eclair_btree_init_empty_6(ptr %17)
  %18 = getelementptr %program, ptr %0, i32 0, i32 17
  call ccc void @eclair_btree_init_empty_1(ptr %18)
  %19 = getelementptr %program, ptr %0, i32 0, i32 18
  call ccc void @eclair_btree_init_empty_6(ptr %19)
  %20 = getelementptr %program, ptr %0, i32 0, i32 19
  call ccc void @eclair_btree_init_empty_0(ptr %20)
  %21 = getelementptr %program, ptr %0, i32 0, i32 20
  call ccc void @eclair_btree_init_empty_1(ptr %21)
  %22 = getelementptr %program, ptr %0, i32 0, i32 21
  call ccc void @eclair_btree_init_empty_6(ptr %22)
  %23 = getelementptr %program, ptr %0, i32 0, i32 22
  call ccc void @eclair_btree_init_empty_1(ptr %23)
  %24 = getelementptr %program, ptr %0, i32 0, i32 23
  call ccc void @eclair_btree_init_empty_1(ptr %24)
  %25 = getelementptr %program, ptr %0, i32 0, i32 24
  call ccc void @eclair_btree_init_empty_7(ptr %25)
  %26 = getelementptr %program, ptr %0, i32 0, i32 25
  call ccc void @eclair_btree_init_empty_0(ptr %26)
  %27 = getelementptr %program, ptr %0, i32 0, i32 26
  call ccc void @eclair_btree_init_empty_0(ptr %27)
  %28 = getelementptr %program, ptr %0, i32 0, i32 27
  call ccc void @eclair_btree_init_empty_1(ptr %28)
  %29 = getelementptr %program, ptr %0, i32 0, i32 28
  call ccc void @eclair_btree_init_empty_1(ptr %29)
  %30 = getelementptr %program, ptr %0, i32 0, i32 29
  call ccc void @eclair_btree_init_empty_6(ptr %30)
  %31 = getelementptr %program, ptr %0, i32 0, i32 30
  call ccc void @eclair_btree_init_empty_6(ptr %31)
  %32 = getelementptr %program, ptr %0, i32 0, i32 31
  call ccc void @eclair_btree_init_empty_6(ptr %32)
  %33 = getelementptr %program, ptr %0, i32 0, i32 32
  call ccc void @eclair_btree_init_empty_6(ptr %33)
  %34 = getelementptr %program, ptr %0, i32 0, i32 33
  call ccc void @eclair_btree_init_empty_6(ptr %34)
  %35 = getelementptr %program, ptr %0, i32 0, i32 34
  call ccc void @eclair_btree_init_empty_1(ptr %35)
  %36 = getelementptr %program, ptr %0, i32 0, i32 35
  call ccc void @eclair_btree_init_empty_1(ptr %36)
  %37 = getelementptr %program, ptr %0, i32 0, i32 36
  call ccc void @eclair_btree_init_empty_1(ptr %37)
  %38 = getelementptr %program, ptr %0, i32 0, i32 37
  call ccc void @eclair_btree_init_empty_6(ptr %38)
  %39 = getelementptr %program, ptr %0, i32 0, i32 38
  call ccc void @eclair_btree_init_empty_6(ptr %39)
  %40 = getelementptr %program, ptr %0, i32 0, i32 39
  call ccc void @eclair_btree_init_empty_1(ptr %40)
  %41 = getelementptr %program, ptr %0, i32 0, i32 40
  call ccc void @eclair_btree_init_empty_1(ptr %41)
  %42 = getelementptr %program, ptr %0, i32 0, i32 41
  call ccc void @eclair_btree_init_empty_6(ptr %42)
  %43 = getelementptr %program, ptr %0, i32 0, i32 42
  call ccc void @eclair_btree_init_empty_1(ptr %43)
  %44 = getelementptr %program, ptr %0, i32 0, i32 43
  call ccc void @eclair_btree_init_empty_6(ptr %44)
  %45 = getelementptr %program, ptr %0, i32 0, i32 44
  call ccc void @eclair_btree_init_empty_0(ptr %45)
  %46 = getelementptr %program, ptr %0, i32 0, i32 45
  call ccc void @eclair_btree_init_empty_1(ptr %46)
  %47 = getelementptr %program, ptr %0, i32 0, i32 46
  call ccc void @eclair_btree_init_empty_6(ptr %47)
  %48 = getelementptr %program, ptr %0, i32 0, i32 47
  call ccc void @eclair_btree_init_empty_6(ptr %48)
  %49 = getelementptr %program, ptr %0, i32 0, i32 48
  call ccc void @eclair_btree_init_empty_0(ptr %49)
  %50 = getelementptr %program, ptr %0, i32 0, i32 49
  call ccc void @eclair_btree_init_empty_1(ptr %50)
  %51 = getelementptr %program, ptr %0, i32 0, i32 50
  call ccc void @eclair_btree_init_empty_1(ptr %51)
  %52 = getelementptr %program, ptr %0, i32 0, i32 51
  call ccc void @eclair_btree_init_empty_2(ptr %52)
  %53 = getelementptr %program, ptr %0, i32 0, i32 52
  call ccc void @eclair_btree_init_empty_0(ptr %53)
  %54 = getelementptr %program, ptr %0, i32 0, i32 53
  call ccc void @eclair_btree_init_empty_0(ptr %54)
  %55 = getelementptr %program, ptr %0, i32 0, i32 54
  call ccc void @eclair_btree_init_empty_8(ptr %55)
  %56 = getelementptr %program, ptr %0, i32 0, i32 55
  call ccc void @eclair_btree_init_empty_0(ptr %56)
  %57 = getelementptr %program, ptr %0, i32 0, i32 56
  call ccc void @eclair_btree_init_empty_1(ptr %57)
  %58 = getelementptr %program, ptr %0, i32 0, i32 57
  call ccc void @eclair_btree_init_empty_2(ptr %58)
  %59 = getelementptr %program, ptr %0, i32 0, i32 58
  call ccc void @eclair_btree_init_empty_6(ptr %59)
  %60 = getelementptr %program, ptr %0, i32 0, i32 59
  call ccc void @eclair_btree_init_empty_1(ptr %60)
  %61 = getelementptr %program, ptr %0, i32 0, i32 60
  call ccc void @eclair_btree_init_empty_2(ptr %61)
  %62 = getelementptr %program, ptr %0, i32 0, i32 61
  call ccc void @eclair_btree_init_empty_6(ptr %62)
  %63 = getelementptr %program, ptr %0, i32 0, i32 62
  call ccc void @eclair_btree_init_empty_1(ptr %63)
  %64 = getelementptr %program, ptr %0, i32 0, i32 63
  call ccc void @eclair_btree_init_empty_0(ptr %64)
  %65 = getelementptr %program, ptr %0, i32 0, i32 64
  call ccc void @eclair_btree_init_empty_0(ptr %65)
  %66 = getelementptr %program, ptr %0, i32 0, i32 65
  call ccc void @eclair_btree_init_empty_0(ptr %66)
  %67 = getelementptr %program, ptr %0, i32 0, i32 66
  call ccc void @eclair_btree_init_empty_1(ptr %67)
  %68 = getelementptr %program, ptr %0, i32 0, i32 67
  call ccc void @eclair_btree_init_empty_2(ptr %68)
  %69 = getelementptr %program, ptr %0, i32 0, i32 68
  call ccc void @eclair_btree_init_empty_6(ptr %69)
  %70 = getelementptr %program, ptr %0, i32 0, i32 69
  call ccc void @eclair_btree_init_empty_1(ptr %70)
  %71 = getelementptr %program, ptr %0, i32 0, i32 70
  call ccc void @eclair_btree_init_empty_1(ptr %71)
  %72 = getelementptr %program, ptr %0, i32 0, i32 71
  call ccc void @eclair_btree_init_empty_0(ptr %72)
  %73 = getelementptr %program, ptr %0, i32 0, i32 72
  call ccc void @eclair_btree_init_empty_0(ptr %73)
  %74 = getelementptr %program, ptr %0, i32 0, i32 73
  call ccc void @eclair_btree_init_empty_0(ptr %74)
  %75 = getelementptr %program, ptr %0, i32 0, i32 0
  %76 = getelementptr inbounds [11 x i8], ptr @string_literal_0, i32 0, i32 0
  %77 = zext i32 10 to i64
  %78 = call ccc ptr @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %78, ptr %76, i64 %77, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_0, i32 10, ptr %78)
  %79 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %75, ptr %stack.ptr_0)
  %80 = getelementptr %program, ptr %0, i32 0, i32 0
  %81 = getelementptr inbounds [11 x i8], ptr @string_literal_1, i32 0, i32 0
  %82 = zext i32 10 to i64
  %83 = call ccc ptr @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %83, ptr %81, i64 %82, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_1, i32 10, ptr %83)
  %84 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %80, ptr %stack.ptr_1)
  %85 = getelementptr %program, ptr %0, i32 0, i32 0
  %86 = getelementptr inbounds [9 x i8], ptr @string_literal_2, i32 0, i32 0
  %87 = zext i32 8 to i64
  %88 = call ccc ptr @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %88, ptr %86, i64 %87, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_2, i32 8, ptr %88)
  %89 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %85, ptr %stack.ptr_2)
  %90 = getelementptr %program, ptr %0, i32 0, i32 0
  %91 = getelementptr inbounds [11 x i8], ptr @string_literal_3, i32 0, i32 0
  %92 = zext i32 10 to i64
  %93 = call ccc ptr @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %93, ptr %91, i64 %92, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_3, i32 10, ptr %93)
  %94 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %90, ptr %stack.ptr_3)
  %95 = getelementptr %program, ptr %0, i32 0, i32 0
  %96 = getelementptr inbounds [6 x i8], ptr @string_literal_4, i32 0, i32 0
  %97 = zext i32 5 to i64
  %98 = call ccc ptr @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %98, ptr %96, i64 %97, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_4, i32 5, ptr %98)
  %99 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %95, ptr %stack.ptr_4)
  %100 = getelementptr %program, ptr %0, i32 0, i32 0
  %101 = getelementptr inbounds [5 x i8], ptr @string_literal_5, i32 0, i32 0
  %102 = zext i32 4 to i64
  %103 = call ccc ptr @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %103, ptr %101, i64 %102, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_5, i32 4, ptr %103)
  %104 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %100, ptr %stack.ptr_5)
  %105 = getelementptr %program, ptr %0, i32 0, i32 0
  %106 = getelementptr inbounds [9 x i8], ptr @string_literal_6, i32 0, i32 0
  %107 = zext i32 8 to i64
  %108 = call ccc ptr @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %108, ptr %106, i64 %107, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_6, i32 8, ptr %108)
  %109 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %105, ptr %stack.ptr_6)
  %110 = getelementptr %program, ptr %0, i32 0, i32 0
  %111 = getelementptr inbounds [5 x i8], ptr @string_literal_7, i32 0, i32 0
  %112 = zext i32 4 to i64
  %113 = call ccc ptr @malloc(i32 4)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %113, ptr %111, i64 %112, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_7, i32 4, ptr %113)
  %114 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %110, ptr %stack.ptr_7)
  %115 = getelementptr %program, ptr %0, i32 0, i32 0
  %116 = getelementptr inbounds [9 x i8], ptr @string_literal_8, i32 0, i32 0
  %117 = zext i32 8 to i64
  %118 = call ccc ptr @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %118, ptr %116, i64 %117, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_8, i32 8, ptr %118)
  %119 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %115, ptr %stack.ptr_8)
  %120 = getelementptr %program, ptr %0, i32 0, i32 0
  %121 = getelementptr inbounds [12 x i8], ptr @string_literal_9, i32 0, i32 0
  %122 = zext i32 11 to i64
  %123 = call ccc ptr @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %123, ptr %121, i64 %122, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_9, i32 11, ptr %123)
  %124 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %120, ptr %stack.ptr_9)
  %125 = getelementptr %program, ptr %0, i32 0, i32 0
  %126 = getelementptr inbounds [9 x i8], ptr @string_literal_10, i32 0, i32 0
  %127 = zext i32 8 to i64
  %128 = call ccc ptr @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %128, ptr %126, i64 %127, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_10, i32 8, ptr %128)
  %129 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %125, ptr %stack.ptr_10)
  %130 = getelementptr %program, ptr %0, i32 0, i32 0
  %131 = getelementptr inbounds [15 x i8], ptr @string_literal_11, i32 0, i32 0
  %132 = zext i32 14 to i64
  %133 = call ccc ptr @malloc(i32 14)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %133, ptr %131, i64 %132, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_11, i32 14, ptr %133)
  %134 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %130, ptr %stack.ptr_11)
  %135 = getelementptr %program, ptr %0, i32 0, i32 0
  %136 = getelementptr inbounds [16 x i8], ptr @string_literal_12, i32 0, i32 0
  %137 = zext i32 15 to i64
  %138 = call ccc ptr @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %138, ptr %136, i64 %137, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_12, i32 15, ptr %138)
  %139 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %135, ptr %stack.ptr_12)
  %140 = getelementptr %program, ptr %0, i32 0, i32 0
  %141 = getelementptr inbounds [18 x i8], ptr @string_literal_13, i32 0, i32 0
  %142 = zext i32 17 to i64
  %143 = call ccc ptr @malloc(i32 17)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %143, ptr %141, i64 %142, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_13, i32 17, ptr %143)
  %144 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %140, ptr %stack.ptr_13)
  %145 = getelementptr %program, ptr %0, i32 0, i32 0
  %146 = getelementptr inbounds [18 x i8], ptr @string_literal_14, i32 0, i32 0
  %147 = zext i32 17 to i64
  %148 = call ccc ptr @malloc(i32 17)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %148, ptr %146, i64 %147, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_14, i32 17, ptr %148)
  %149 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %145, ptr %stack.ptr_14)
  %150 = getelementptr %program, ptr %0, i32 0, i32 0
  %151 = getelementptr inbounds [13 x i8], ptr @string_literal_15, i32 0, i32 0
  %152 = zext i32 12 to i64
  %153 = call ccc ptr @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %153, ptr %151, i64 %152, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_15, i32 12, ptr %153)
  %154 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %150, ptr %stack.ptr_15)
  %155 = getelementptr %program, ptr %0, i32 0, i32 0
  %156 = getelementptr inbounds [19 x i8], ptr @string_literal_16, i32 0, i32 0
  %157 = zext i32 18 to i64
  %158 = call ccc ptr @malloc(i32 18)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %158, ptr %156, i64 %157, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_16, i32 18, ptr %158)
  %159 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %155, ptr %stack.ptr_16)
  %160 = getelementptr %program, ptr %0, i32 0, i32 0
  %161 = getelementptr inbounds [13 x i8], ptr @string_literal_17, i32 0, i32 0
  %162 = zext i32 12 to i64
  %163 = call ccc ptr @malloc(i32 12)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %163, ptr %161, i64 %162, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_17, i32 12, ptr %163)
  %164 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %160, ptr %stack.ptr_17)
  %165 = getelementptr %program, ptr %0, i32 0, i32 0
  %166 = getelementptr inbounds [14 x i8], ptr @string_literal_18, i32 0, i32 0
  %167 = zext i32 13 to i64
  %168 = call ccc ptr @malloc(i32 13)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %168, ptr %166, i64 %167, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_18, i32 13, ptr %168)
  %169 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %165, ptr %stack.ptr_18)
  %170 = getelementptr %program, ptr %0, i32 0, i32 0
  %171 = getelementptr inbounds [12 x i8], ptr @string_literal_19, i32 0, i32 0
  %172 = zext i32 11 to i64
  %173 = call ccc ptr @malloc(i32 11)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %173, ptr %171, i64 %172, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_19, i32 11, ptr %173)
  %174 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %170, ptr %stack.ptr_19)
  %175 = getelementptr %program, ptr %0, i32 0, i32 0
  %176 = getelementptr inbounds [14 x i8], ptr @string_literal_20, i32 0, i32 0
  %177 = zext i32 13 to i64
  %178 = call ccc ptr @malloc(i32 13)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %178, ptr %176, i64 %177, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_20, i32 13, ptr %178)
  %179 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %175, ptr %stack.ptr_20)
  %180 = getelementptr %program, ptr %0, i32 0, i32 0
  %181 = getelementptr inbounds [7 x i8], ptr @string_literal_21, i32 0, i32 0
  %182 = zext i32 6 to i64
  %183 = call ccc ptr @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %183, ptr %181, i64 %182, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_21, i32 6, ptr %183)
  %184 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %180, ptr %stack.ptr_21)
  %185 = getelementptr %program, ptr %0, i32 0, i32 0
  %186 = getelementptr inbounds [14 x i8], ptr @string_literal_22, i32 0, i32 0
  %187 = zext i32 13 to i64
  %188 = call ccc ptr @malloc(i32 13)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %188, ptr %186, i64 %187, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_22, i32 13, ptr %188)
  %189 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %185, ptr %stack.ptr_22)
  %190 = getelementptr %program, ptr %0, i32 0, i32 0
  %191 = getelementptr inbounds [20 x i8], ptr @string_literal_23, i32 0, i32 0
  %192 = zext i32 19 to i64
  %193 = call ccc ptr @malloc(i32 19)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %193, ptr %191, i64 %192, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_23, i32 19, ptr %193)
  %194 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %190, ptr %stack.ptr_23)
  %195 = getelementptr %program, ptr %0, i32 0, i32 0
  %196 = getelementptr inbounds [22 x i8], ptr @string_literal_24, i32 0, i32 0
  %197 = zext i32 21 to i64
  %198 = call ccc ptr @malloc(i32 21)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %198, ptr %196, i64 %197, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_24, i32 21, ptr %198)
  %199 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %195, ptr %stack.ptr_24)
  %200 = getelementptr %program, ptr %0, i32 0, i32 0
  %201 = getelementptr inbounds [9 x i8], ptr @string_literal_25, i32 0, i32 0
  %202 = zext i32 8 to i64
  %203 = call ccc ptr @malloc(i32 8)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %203, ptr %201, i64 %202, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_25, i32 8, ptr %203)
  %204 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %200, ptr %stack.ptr_25)
  %205 = getelementptr %program, ptr %0, i32 0, i32 0
  %206 = getelementptr inbounds [14 x i8], ptr @string_literal_26, i32 0, i32 0
  %207 = zext i32 13 to i64
  %208 = call ccc ptr @malloc(i32 13)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %208, ptr %206, i64 %207, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_26, i32 13, ptr %208)
  %209 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %205, ptr %stack.ptr_26)
  %210 = getelementptr %program, ptr %0, i32 0, i32 0
  %211 = getelementptr inbounds [6 x i8], ptr @string_literal_27, i32 0, i32 0
  %212 = zext i32 5 to i64
  %213 = call ccc ptr @malloc(i32 5)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %213, ptr %211, i64 %212, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_27, i32 5, ptr %213)
  %214 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %210, ptr %stack.ptr_27)
  %215 = getelementptr %program, ptr %0, i32 0, i32 0
  %216 = getelementptr inbounds [10 x i8], ptr @string_literal_28, i32 0, i32 0
  %217 = zext i32 9 to i64
  %218 = call ccc ptr @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %218, ptr %216, i64 %217, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_28, i32 9, ptr %218)
  %219 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %215, ptr %stack.ptr_28)
  %220 = getelementptr %program, ptr %0, i32 0, i32 0
  %221 = getelementptr inbounds [11 x i8], ptr @string_literal_29, i32 0, i32 0
  %222 = zext i32 10 to i64
  %223 = call ccc ptr @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %223, ptr %221, i64 %222, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_29, i32 10, ptr %223)
  %224 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %220, ptr %stack.ptr_29)
  %225 = getelementptr %program, ptr %0, i32 0, i32 0
  %226 = getelementptr inbounds [22 x i8], ptr @string_literal_30, i32 0, i32 0
  %227 = zext i32 21 to i64
  %228 = call ccc ptr @malloc(i32 21)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %228, ptr %226, i64 %227, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_30, i32 21, ptr %228)
  %229 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %225, ptr %stack.ptr_30)
  %230 = getelementptr %program, ptr %0, i32 0, i32 0
  %231 = getelementptr inbounds [7 x i8], ptr @string_literal_31, i32 0, i32 0
  %232 = zext i32 6 to i64
  %233 = call ccc ptr @malloc(i32 6)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %233, ptr %231, i64 %232, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_31, i32 6, ptr %233)
  %234 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %230, ptr %stack.ptr_31)
  %235 = getelementptr %program, ptr %0, i32 0, i32 0
  %236 = getelementptr inbounds [16 x i8], ptr @string_literal_32, i32 0, i32 0
  %237 = zext i32 15 to i64
  %238 = call ccc ptr @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %238, ptr %236, i64 %237, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_32, i32 15, ptr %238)
  %239 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %235, ptr %stack.ptr_32)
  %240 = getelementptr %program, ptr %0, i32 0, i32 0
  %241 = getelementptr inbounds [10 x i8], ptr @string_literal_33, i32 0, i32 0
  %242 = zext i32 9 to i64
  %243 = call ccc ptr @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %243, ptr %241, i64 %242, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_33, i32 9, ptr %243)
  %244 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %240, ptr %stack.ptr_33)
  %245 = getelementptr %program, ptr %0, i32 0, i32 0
  %246 = getelementptr inbounds [17 x i8], ptr @string_literal_34, i32 0, i32 0
  %247 = zext i32 16 to i64
  %248 = call ccc ptr @malloc(i32 16)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %248, ptr %246, i64 %247, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_34, i32 16, ptr %248)
  %249 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %245, ptr %stack.ptr_34)
  %250 = getelementptr %program, ptr %0, i32 0, i32 0
  %251 = getelementptr inbounds [11 x i8], ptr @string_literal_35, i32 0, i32 0
  %252 = zext i32 10 to i64
  %253 = call ccc ptr @malloc(i32 10)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %253, ptr %251, i64 %252, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_35, i32 10, ptr %253)
  %254 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %250, ptr %stack.ptr_35)
  %255 = getelementptr %program, ptr %0, i32 0, i32 0
  %256 = getelementptr inbounds [21 x i8], ptr @string_literal_36, i32 0, i32 0
  %257 = zext i32 20 to i64
  %258 = call ccc ptr @malloc(i32 20)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %258, ptr %256, i64 %257, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_36, i32 20, ptr %258)
  %259 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %255, ptr %stack.ptr_36)
  %260 = getelementptr %program, ptr %0, i32 0, i32 0
  %261 = getelementptr inbounds [18 x i8], ptr @string_literal_37, i32 0, i32 0
  %262 = zext i32 17 to i64
  %263 = call ccc ptr @malloc(i32 17)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %263, ptr %261, i64 %262, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_37, i32 17, ptr %263)
  %264 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %260, ptr %stack.ptr_37)
  %265 = getelementptr %program, ptr %0, i32 0, i32 0
  %266 = getelementptr inbounds [20 x i8], ptr @string_literal_38, i32 0, i32 0
  %267 = zext i32 19 to i64
  %268 = call ccc ptr @malloc(i32 19)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %268, ptr %266, i64 %267, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_38, i32 19, ptr %268)
  %269 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %265, ptr %stack.ptr_38)
  %270 = getelementptr %program, ptr %0, i32 0, i32 0
  %271 = getelementptr inbounds [25 x i8], ptr @string_literal_39, i32 0, i32 0
  %272 = zext i32 24 to i64
  %273 = call ccc ptr @malloc(i32 24)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %273, ptr %271, i64 %272, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_39, i32 24, ptr %273)
  %274 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %270, ptr %stack.ptr_39)
  %275 = getelementptr %program, ptr %0, i32 0, i32 0
  %276 = getelementptr inbounds [17 x i8], ptr @string_literal_40, i32 0, i32 0
  %277 = zext i32 16 to i64
  %278 = call ccc ptr @malloc(i32 16)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %278, ptr %276, i64 %277, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_40, i32 16, ptr %278)
  %279 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %275, ptr %stack.ptr_40)
  %280 = getelementptr %program, ptr %0, i32 0, i32 0
  %281 = getelementptr inbounds [19 x i8], ptr @string_literal_41, i32 0, i32 0
  %282 = zext i32 18 to i64
  %283 = call ccc ptr @malloc(i32 18)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %283, ptr %281, i64 %282, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_41, i32 18, ptr %283)
  %284 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %280, ptr %stack.ptr_41)
  %285 = getelementptr %program, ptr %0, i32 0, i32 0
  %286 = getelementptr inbounds [22 x i8], ptr @string_literal_42, i32 0, i32 0
  %287 = zext i32 21 to i64
  %288 = call ccc ptr @malloc(i32 21)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %288, ptr %286, i64 %287, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_42, i32 21, ptr %288)
  %289 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %285, ptr %stack.ptr_42)
  %290 = getelementptr %program, ptr %0, i32 0, i32 0
  %291 = getelementptr inbounds [23 x i8], ptr @string_literal_43, i32 0, i32 0
  %292 = zext i32 22 to i64
  %293 = call ccc ptr @malloc(i32 22)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %293, ptr %291, i64 %292, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_43, i32 22, ptr %293)
  %294 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %290, ptr %stack.ptr_43)
  %295 = getelementptr %program, ptr %0, i32 0, i32 0
  %296 = getelementptr inbounds [18 x i8], ptr @string_literal_44, i32 0, i32 0
  %297 = zext i32 17 to i64
  %298 = call ccc ptr @malloc(i32 17)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %298, ptr %296, i64 %297, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_44, i32 17, ptr %298)
  %299 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %295, ptr %stack.ptr_44)
  %300 = getelementptr %program, ptr %0, i32 0, i32 0
  %301 = getelementptr inbounds [23 x i8], ptr @string_literal_45, i32 0, i32 0
  %302 = zext i32 22 to i64
  %303 = call ccc ptr @malloc(i32 22)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %303, ptr %301, i64 %302, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_45, i32 22, ptr %303)
  %304 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %300, ptr %stack.ptr_45)
  %305 = getelementptr %program, ptr %0, i32 0, i32 0
  %306 = getelementptr inbounds [24 x i8], ptr @string_literal_46, i32 0, i32 0
  %307 = zext i32 23 to i64
  %308 = call ccc ptr @malloc(i32 23)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %308, ptr %306, i64 %307, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_46, i32 23, ptr %308)
  %309 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %305, ptr %stack.ptr_46)
  %310 = getelementptr %program, ptr %0, i32 0, i32 0
  %311 = getelementptr inbounds [10 x i8], ptr @string_literal_47, i32 0, i32 0
  %312 = zext i32 9 to i64
  %313 = call ccc ptr @malloc(i32 9)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %313, ptr %311, i64 %312, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_47, i32 9, ptr %313)
  %314 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %310, ptr %stack.ptr_47)
  %315 = getelementptr %program, ptr %0, i32 0, i32 0
  %316 = getelementptr inbounds [19 x i8], ptr @string_literal_48, i32 0, i32 0
  %317 = zext i32 18 to i64
  %318 = call ccc ptr @malloc(i32 18)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %318, ptr %316, i64 %317, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_48, i32 18, ptr %318)
  %319 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %315, ptr %stack.ptr_48)
  %320 = getelementptr %program, ptr %0, i32 0, i32 0
  %321 = getelementptr inbounds [23 x i8], ptr @string_literal_49, i32 0, i32 0
  %322 = zext i32 22 to i64
  %323 = call ccc ptr @malloc(i32 22)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %323, ptr %321, i64 %322, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_49, i32 22, ptr %323)
  %324 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %320, ptr %stack.ptr_49)
  %325 = getelementptr %program, ptr %0, i32 0, i32 0
  %326 = getelementptr inbounds [24 x i8], ptr @string_literal_50, i32 0, i32 0
  %327 = zext i32 23 to i64
  %328 = call ccc ptr @malloc(i32 23)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %328, ptr %326, i64 %327, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_50, i32 23, ptr %328)
  %329 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %325, ptr %stack.ptr_50)
  %330 = getelementptr %program, ptr %0, i32 0, i32 0
  %331 = getelementptr inbounds [20 x i8], ptr @string_literal_51, i32 0, i32 0
  %332 = zext i32 19 to i64
  %333 = call ccc ptr @malloc(i32 19)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %333, ptr %331, i64 %332, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_51, i32 19, ptr %333)
  %334 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %330, ptr %stack.ptr_51)
  %335 = getelementptr %program, ptr %0, i32 0, i32 0
  %336 = getelementptr inbounds [20 x i8], ptr @string_literal_52, i32 0, i32 0
  %337 = zext i32 19 to i64
  %338 = call ccc ptr @malloc(i32 19)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %338, ptr %336, i64 %337, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_52, i32 19, ptr %338)
  %339 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %335, ptr %stack.ptr_52)
  %340 = getelementptr %program, ptr %0, i32 0, i32 0
  %341 = getelementptr inbounds [16 x i8], ptr @string_literal_53, i32 0, i32 0
  %342 = zext i32 15 to i64
  %343 = call ccc ptr @malloc(i32 15)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %343, ptr %341, i64 %342, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_53, i32 15, ptr %343)
  %344 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %340, ptr %stack.ptr_53)
  %345 = getelementptr %program, ptr %0, i32 0, i32 0
  %346 = getelementptr inbounds [2 x i8], ptr @string_literal_54, i32 0, i32 0
  %347 = zext i32 1 to i64
  %348 = call ccc ptr @malloc(i32 1)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %348, ptr %346, i64 %347, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_54, i32 1, ptr %348)
  %349 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %345, ptr %stack.ptr_54)
  %350 = getelementptr %program, ptr %0, i32 0, i32 0
  %351 = getelementptr inbounds [3 x i8], ptr @string_literal_55, i32 0, i32 0
  %352 = zext i32 2 to i64
  %353 = call ccc ptr @malloc(i32 2)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %353, ptr %351, i64 %352, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_55, i32 2, ptr %353)
  %354 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %350, ptr %stack.ptr_55)
  %355 = getelementptr %program, ptr %0, i32 0, i32 0
  %356 = getelementptr inbounds [2 x i8], ptr @string_literal_56, i32 0, i32 0
  %357 = zext i32 1 to i64
  %358 = call ccc ptr @malloc(i32 1)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %358, ptr %356, i64 %357, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_56, i32 1, ptr %358)
  %359 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %355, ptr %stack.ptr_56)
  %360 = getelementptr %program, ptr %0, i32 0, i32 0
  %361 = getelementptr inbounds [3 x i8], ptr @string_literal_57, i32 0, i32 0
  %362 = zext i32 2 to i64
  %363 = call ccc ptr @malloc(i32 2)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %363, ptr %361, i64 %362, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_57, i32 2, ptr %363)
  %364 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %360, ptr %stack.ptr_57)
  %365 = getelementptr %program, ptr %0, i32 0, i32 0
  %366 = getelementptr inbounds [2 x i8], ptr @string_literal_58, i32 0, i32 0
  %367 = zext i32 1 to i64
  %368 = call ccc ptr @malloc(i32 1)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %368, ptr %366, i64 %367, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_58, i32 1, ptr %368)
  %369 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %365, ptr %stack.ptr_58)
  %370 = getelementptr %program, ptr %0, i32 0, i32 0
  %371 = getelementptr inbounds [3 x i8], ptr @string_literal_59, i32 0, i32 0
  %372 = zext i32 2 to i64
  %373 = call ccc ptr @malloc(i32 2)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %373, ptr %371, i64 %372, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_59, i32 2, ptr %373)
  %374 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %370, ptr %stack.ptr_59)
  %375 = getelementptr %program, ptr %0, i32 0, i32 0
  %376 = getelementptr inbounds [2 x i8], ptr @string_literal_60, i32 0, i32 0
  %377 = zext i32 1 to i64
  %378 = call ccc ptr @malloc(i32 1)
  call ccc void @llvm.memcpy.p0i8.p0i8.i64(ptr %378, ptr %376, i64 %377, i1 0)
  call ccc void @eclair_symbol_init(ptr %stack.ptr_60, i32 1, ptr %378)
  %379 = call ccc i32 @eclair_symbol_table_find_or_insert(ptr %375, ptr %stack.ptr_60)
  ret ptr %0
}

define external ccc void @eclair_program_destroy(ptr %arg_0) "wasm-export-name"="eclair_program_destroy" {
start:
  %0 = getelementptr %program, ptr %arg_0, i32 0, i32 0
  call ccc void @eclair_symbol_table_destroy(ptr %0)
  %1 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_destroy_0(ptr %1)
  %2 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_destroy_0(ptr %2)
  %3 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_destroy_1(ptr %3)
  %4 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_destroy_2(ptr %4)
  %5 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_destroy_0(ptr %5)
  %6 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_destroy_3(ptr %6)
  %7 = getelementptr %program, ptr %arg_0, i32 0, i32 7
  call ccc void @eclair_btree_destroy_0(ptr %7)
  %8 = getelementptr %program, ptr %arg_0, i32 0, i32 8
  call ccc void @eclair_btree_destroy_0(ptr %8)
  %9 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_destroy_4(ptr %9)
  %10 = getelementptr %program, ptr %arg_0, i32 0, i32 10
  call ccc void @eclair_btree_destroy_5(ptr %10)
  %11 = getelementptr %program, ptr %arg_0, i32 0, i32 11
  call ccc void @eclair_btree_destroy_6(ptr %11)
  %12 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_destroy_6(ptr %12)
  %13 = getelementptr %program, ptr %arg_0, i32 0, i32 13
  call ccc void @eclair_btree_destroy_1(ptr %13)
  %14 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_destroy_1(ptr %14)
  %15 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_destroy_2(ptr %15)
  %16 = getelementptr %program, ptr %arg_0, i32 0, i32 16
  call ccc void @eclair_btree_destroy_6(ptr %16)
  %17 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_destroy_1(ptr %17)
  %18 = getelementptr %program, ptr %arg_0, i32 0, i32 18
  call ccc void @eclair_btree_destroy_6(ptr %18)
  %19 = getelementptr %program, ptr %arg_0, i32 0, i32 19
  call ccc void @eclair_btree_destroy_0(ptr %19)
  %20 = getelementptr %program, ptr %arg_0, i32 0, i32 20
  call ccc void @eclair_btree_destroy_1(ptr %20)
  %21 = getelementptr %program, ptr %arg_0, i32 0, i32 21
  call ccc void @eclair_btree_destroy_6(ptr %21)
  %22 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_destroy_1(ptr %22)
  %23 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_destroy_1(ptr %23)
  %24 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_destroy_7(ptr %24)
  %25 = getelementptr %program, ptr %arg_0, i32 0, i32 25
  call ccc void @eclair_btree_destroy_0(ptr %25)
  %26 = getelementptr %program, ptr %arg_0, i32 0, i32 26
  call ccc void @eclair_btree_destroy_0(ptr %26)
  %27 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_destroy_1(ptr %27)
  %28 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  call ccc void @eclair_btree_destroy_1(ptr %28)
  %29 = getelementptr %program, ptr %arg_0, i32 0, i32 29
  call ccc void @eclair_btree_destroy_6(ptr %29)
  %30 = getelementptr %program, ptr %arg_0, i32 0, i32 30
  call ccc void @eclair_btree_destroy_6(ptr %30)
  %31 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_destroy_6(ptr %31)
  %32 = getelementptr %program, ptr %arg_0, i32 0, i32 32
  call ccc void @eclair_btree_destroy_6(ptr %32)
  %33 = getelementptr %program, ptr %arg_0, i32 0, i32 33
  call ccc void @eclair_btree_destroy_6(ptr %33)
  %34 = getelementptr %program, ptr %arg_0, i32 0, i32 34
  call ccc void @eclair_btree_destroy_1(ptr %34)
  %35 = getelementptr %program, ptr %arg_0, i32 0, i32 35
  call ccc void @eclair_btree_destroy_1(ptr %35)
  %36 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_destroy_1(ptr %36)
  %37 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_destroy_6(ptr %37)
  %38 = getelementptr %program, ptr %arg_0, i32 0, i32 38
  call ccc void @eclair_btree_destroy_6(ptr %38)
  %39 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_destroy_1(ptr %39)
  %40 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_destroy_1(ptr %40)
  %41 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  call ccc void @eclair_btree_destroy_6(ptr %41)
  %42 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  call ccc void @eclair_btree_destroy_1(ptr %42)
  %43 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  call ccc void @eclair_btree_destroy_6(ptr %43)
  %44 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  call ccc void @eclair_btree_destroy_0(ptr %44)
  %45 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  call ccc void @eclair_btree_destroy_1(ptr %45)
  %46 = getelementptr %program, ptr %arg_0, i32 0, i32 46
  call ccc void @eclair_btree_destroy_6(ptr %46)
  %47 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_destroy_6(ptr %47)
  %48 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_destroy_0(ptr %48)
  %49 = getelementptr %program, ptr %arg_0, i32 0, i32 49
  call ccc void @eclair_btree_destroy_1(ptr %49)
  %50 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_destroy_1(ptr %50)
  %51 = getelementptr %program, ptr %arg_0, i32 0, i32 51
  call ccc void @eclair_btree_destroy_2(ptr %51)
  %52 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_destroy_0(ptr %52)
  %53 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_destroy_0(ptr %53)
  %54 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_destroy_8(ptr %54)
  %55 = getelementptr %program, ptr %arg_0, i32 0, i32 55
  call ccc void @eclair_btree_destroy_0(ptr %55)
  %56 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_destroy_1(ptr %56)
  %57 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_destroy_2(ptr %57)
  %58 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  call ccc void @eclair_btree_destroy_6(ptr %58)
  %59 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_destroy_1(ptr %59)
  %60 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_destroy_2(ptr %60)
  %61 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_destroy_6(ptr %61)
  %62 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_destroy_1(ptr %62)
  %63 = getelementptr %program, ptr %arg_0, i32 0, i32 63
  call ccc void @eclair_btree_destroy_0(ptr %63)
  %64 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  call ccc void @eclair_btree_destroy_0(ptr %64)
  %65 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  call ccc void @eclair_btree_destroy_0(ptr %65)
  %66 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_destroy_1(ptr %66)
  %67 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_destroy_2(ptr %67)
  %68 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_destroy_6(ptr %68)
  %69 = getelementptr %program, ptr %arg_0, i32 0, i32 69
  call ccc void @eclair_btree_destroy_1(ptr %69)
  %70 = getelementptr %program, ptr %arg_0, i32 0, i32 70
  call ccc void @eclair_btree_destroy_1(ptr %70)
  %71 = getelementptr %program, ptr %arg_0, i32 0, i32 71
  call ccc void @eclair_btree_destroy_0(ptr %71)
  %72 = getelementptr %program, ptr %arg_0, i32 0, i32 72
  call ccc void @eclair_btree_destroy_0(ptr %72)
  %73 = getelementptr %program, ptr %arg_0, i32 0, i32 73
  call ccc void @eclair_btree_destroy_0(ptr %73)
  call ccc void @free(ptr %arg_0)
  ret void
}

define external ccc void @eclair_btree_insert_range_delta_points_to_points_to(ptr %tree_0, ptr %begin_0, ptr %end_0) {
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

define external ccc void @eclair_btree_insert_range_points_to_new_points_to(ptr %tree_0, ptr %begin_0, ptr %end_0) {
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

define external ccc void @eclair_btree_insert_range_delta_transitive_depends_on_transitive_depends_on(ptr %tree_0, ptr %begin_0, ptr %end_0) {
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

define external ccc void @eclair_btree_insert_range_transitive_depends_on_new_transitive_depends_on(ptr %tree_0, ptr %begin_0, ptr %end_0) {
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

define external ccc void @eclair_btree_insert_range_delta_live_rule_live_rule(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_6(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_6(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_6(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_insert_range_live_rule_new_live_rule(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_6(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_6(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_6(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_insert_range_delta_dead_code_dead_code(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_6(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_6(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_6(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_insert_range_dead_code_new_dead_code(ptr %tree_0, ptr %begin_0, ptr %end_0) {
start:
  br label %while_begin_0
while_begin_0:
  %0 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %begin_0, ptr %end_0)
  %1 = select i1 %0, i1 0, i1 1
  br i1 %1, label %while_body_0, label %while_end_0
while_body_0:
  %2 = call ccc ptr @eclair_btree_iterator_current_6(ptr %begin_0)
  %3 = call ccc i1 @eclair_btree_insert_value_6(ptr %tree_0, ptr %2)
  call ccc void @eclair_btree_iterator_next_6(ptr %begin_0)
  br label %while_begin_0
while_end_0:
  ret void
}

define external ccc void @eclair_btree_insert_range_delta_grounded_node_grounded_node(ptr %tree_0, ptr %begin_0, ptr %end_0) {
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

define external ccc void @eclair_btree_insert_range_grounded_node_new_grounded_node(ptr %tree_0, ptr %begin_0, ptr %end_0) {
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

define external ccc void @eclair_program_run(ptr %arg_0) "wasm-export-name"="eclair_program_run" {
start:
  %stack.ptr_0 = alloca [2 x i32], i32 1
  %stack.ptr_1 = alloca [2 x i32], i32 1
  %stack.ptr_2 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_3 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_4 = alloca [2 x i32], i32 1
  %stack.ptr_5 = alloca [2 x i32], i32 1
  %stack.ptr_6 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_7 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_8 = alloca [3 x i32], i32 1
  %stack.ptr_9 = alloca [2 x i32], i32 1
  %stack.ptr_10 = alloca [2 x i32], i32 1
  %stack.ptr_11 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_12 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_13 = alloca [2 x i32], i32 1
  %stack.ptr_14 = alloca [2 x i32], i32 1
  %stack.ptr_15 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_16 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_17 = alloca [2 x i32], i32 1
  %stack.ptr_18 = alloca [2 x i32], i32 1
  %stack.ptr_19 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_20 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_21 = alloca [3 x i32], i32 1
  %stack.ptr_22 = alloca [2 x i32], i32 1
  %stack.ptr_23 = alloca [2 x i32], i32 1
  %stack.ptr_24 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_25 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_26 = alloca [2 x i32], i32 1
  %stack.ptr_27 = alloca [2 x i32], i32 1
  %stack.ptr_28 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_29 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_30 = alloca [3 x i32], i32 1
  %stack.ptr_31 = alloca [2 x i32], i32 1
  %stack.ptr_32 = alloca [2 x i32], i32 1
  %stack.ptr_33 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_34 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_35 = alloca [2 x i32], i32 1
  %stack.ptr_36 = alloca [2 x i32], i32 1
  %stack.ptr_37 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_38 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_39 = alloca [3 x i32], i32 1
  %stack.ptr_40 = alloca [2 x i32], i32 1
  %stack.ptr_41 = alloca [2 x i32], i32 1
  %stack.ptr_42 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_43 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_44 = alloca [2 x i32], i32 1
  %stack.ptr_45 = alloca [2 x i32], i32 1
  %stack.ptr_46 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_47 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_48 = alloca [3 x i32], i32 1
  %stack.ptr_49 = alloca [2 x i32], i32 1
  %stack.ptr_50 = alloca [2 x i32], i32 1
  %stack.ptr_51 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_52 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_53 = alloca [2 x i32], i32 1
  %stack.ptr_54 = alloca [2 x i32], i32 1
  %stack.ptr_55 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_56 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_57 = alloca [3 x i32], i32 1
  %stack.ptr_58 = alloca [2 x i32], i32 1
  %stack.ptr_59 = alloca [2 x i32], i32 1
  %stack.ptr_60 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_61 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_62 = alloca [2 x i32], i32 1
  %stack.ptr_63 = alloca [2 x i32], i32 1
  %stack.ptr_64 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_65 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_66 = alloca [2 x i32], i32 1
  %stack.ptr_67 = alloca [2 x i32], i32 1
  %stack.ptr_68 = alloca [2 x i32], i32 1
  %stack.ptr_69 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_70 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_71 = alloca [2 x i32], i32 1
  %stack.ptr_72 = alloca [2 x i32], i32 1
  %stack.ptr_73 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_74 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_75 = alloca [2 x i32], i32 1
  %stack.ptr_76 = alloca [3 x i32], i32 1
  %stack.ptr_77 = alloca [3 x i32], i32 1
  %stack.ptr_78 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_79 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_80 = alloca [2 x i32], i32 1
  %stack.ptr_81 = alloca [2 x i32], i32 1
  %stack.ptr_82 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_83 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_84 = alloca [3 x i32], i32 1
  %stack.ptr_85 = alloca [2 x i32], i32 1
  %stack.ptr_86 = alloca [2 x i32], i32 1
  %stack.ptr_87 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_88 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_89 = alloca [2 x i32], i32 1
  %stack.ptr_90 = alloca [2 x i32], i32 1
  %stack.ptr_91 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_92 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_93 = alloca [2 x i32], i32 1
  %stack.ptr_94 = alloca [2 x i32], i32 1
  %stack.ptr_95 = alloca [2 x i32], i32 1
  %stack.ptr_96 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_97 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_98 = alloca [2 x i32], i32 1
  %stack.ptr_99 = alloca [2 x i32], i32 1
  %stack.ptr_100 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_101 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_102 = alloca [2 x i32], i32 1
  %stack.ptr_103 = alloca [2 x i32], i32 1
  %stack.ptr_104 = alloca [2 x i32], i32 1
  %stack.ptr_105 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_106 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_107 = alloca [2 x i32], i32 1
  %stack.ptr_108 = alloca [2 x i32], i32 1
  %stack.ptr_109 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_110 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_111 = alloca [2 x i32], i32 1
  %stack.ptr_112 = alloca [2 x i32], i32 1
  %stack.ptr_113 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_114 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_115 = alloca [2 x i32], i32 1
  %stack.ptr_116 = alloca [2 x i32], i32 1
  %stack.ptr_117 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_118 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_119 = alloca [3 x i32], i32 1
  %stack.ptr_120 = alloca [4 x i32], i32 1
  %stack.ptr_121 = alloca [4 x i32], i32 1
  %stack.ptr_122 = alloca %btree_iterator_t_5, i32 1
  %stack.ptr_123 = alloca %btree_iterator_t_5, i32 1
  %stack.ptr_124 = alloca [3 x i32], i32 1
  %stack.ptr_125 = alloca [3 x i32], i32 1
  %stack.ptr_126 = alloca [3 x i32], i32 1
  %stack.ptr_127 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_128 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_129 = alloca [3 x i32], i32 1
  %stack.ptr_130 = alloca [3 x i32], i32 1
  %stack.ptr_131 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_132 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_133 = alloca [3 x i32], i32 1
  %stack.ptr_134 = alloca [3 x i32], i32 1
  %stack.ptr_135 = alloca [3 x i32], i32 1
  %stack.ptr_136 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_137 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_138 = alloca [3 x i32], i32 1
  %stack.ptr_139 = alloca [3 x i32], i32 1
  %stack.ptr_140 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_141 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_142 = alloca [3 x i32], i32 1
  %stack.ptr_143 = alloca [3 x i32], i32 1
  %stack.ptr_144 = alloca [3 x i32], i32 1
  %stack.ptr_145 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_146 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_147 = alloca [2 x i32], i32 1
  %stack.ptr_148 = alloca [2 x i32], i32 1
  %stack.ptr_149 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_150 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_151 = alloca [3 x i32], i32 1
  %stack.ptr_152 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_153 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_154 = alloca [3 x i32], i32 1
  %stack.ptr_155 = alloca [3 x i32], i32 1
  %stack.ptr_156 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_157 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_158 = alloca [3 x i32], i32 1
  %stack.ptr_159 = alloca [2 x i32], i32 1
  %stack.ptr_160 = alloca [2 x i32], i32 1
  %stack.ptr_161 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_162 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_163 = alloca [2 x i32], i32 1
  %stack.ptr_164 = alloca [2 x i32], i32 1
  %stack.ptr_165 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_166 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_167 = alloca [3 x i32], i32 1
  %stack.ptr_168 = alloca [3 x i32], i32 1
  %stack.ptr_169 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_170 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_171 = alloca [3 x i32], i32 1
  %stack.ptr_172 = alloca [3 x i32], i32 1
  %stack.ptr_173 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_174 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_175 = alloca [3 x i32], i32 1
  %stack.ptr_176 = alloca [3 x i32], i32 1
  %stack.ptr_177 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_178 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_179 = alloca [3 x i32], i32 1
  %stack.ptr_180 = alloca [3 x i32], i32 1
  %stack.ptr_181 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_182 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_183 = alloca [2 x i32], i32 1
  %stack.ptr_184 = alloca [2 x i32], i32 1
  %stack.ptr_185 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_186 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_187 = alloca [1 x i32], i32 1
  %stack.ptr_188 = alloca [3 x i32], i32 1
  %stack.ptr_189 = alloca [3 x i32], i32 1
  %stack.ptr_190 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_191 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_192 = alloca [3 x i32], i32 1
  %stack.ptr_193 = alloca [3 x i32], i32 1
  %stack.ptr_194 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_195 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_196 = alloca [2 x i32], i32 1
  %stack.ptr_197 = alloca [2 x i32], i32 1
  %stack.ptr_198 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_199 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_200 = alloca [1 x i32], i32 1
  %stack.ptr_201 = alloca [2 x i32], i32 1
  %stack.ptr_202 = alloca [2 x i32], i32 1
  %stack.ptr_203 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_204 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_205 = alloca [1 x i32], i32 1
  %stack.ptr_206 = alloca [3 x i32], i32 1
  %stack.ptr_207 = alloca [3 x i32], i32 1
  %stack.ptr_208 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_209 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_210 = alloca [1 x i32], i32 1
  %stack.ptr_211 = alloca [1 x i32], i32 1
  %stack.ptr_212 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_213 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_214 = alloca [3 x i32], i32 1
  %stack.ptr_215 = alloca [2 x i32], i32 1
  %stack.ptr_216 = alloca [2 x i32], i32 1
  %stack.ptr_217 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_218 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_219 = alloca [3 x i32], i32 1
  %stack.ptr_220 = alloca [3 x i32], i32 1
  %stack.ptr_221 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_222 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_223 = alloca [1 x i32], i32 1
  %stack.ptr_224 = alloca [1 x i32], i32 1
  %stack.ptr_225 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_226 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_227 = alloca [3 x i32], i32 1
  %stack.ptr_228 = alloca [4 x i32], i32 1
  %stack.ptr_229 = alloca [4 x i32], i32 1
  %stack.ptr_230 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_231 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_232 = alloca [1 x i32], i32 1
  %stack.ptr_233 = alloca [1 x i32], i32 1
  %stack.ptr_234 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_235 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_236 = alloca [2 x i32], i32 1
  %stack.ptr_237 = alloca [4 x i32], i32 1
  %stack.ptr_238 = alloca [4 x i32], i32 1
  %stack.ptr_239 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_240 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_241 = alloca [1 x i32], i32 1
  %stack.ptr_242 = alloca [1 x i32], i32 1
  %stack.ptr_243 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_244 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_245 = alloca [2 x i32], i32 1
  %stack.ptr_246 = alloca [4 x i32], i32 1
  %stack.ptr_247 = alloca [4 x i32], i32 1
  %stack.ptr_248 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_249 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_250 = alloca [1 x i32], i32 1
  %stack.ptr_251 = alloca [1 x i32], i32 1
  %stack.ptr_252 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_253 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_254 = alloca [2 x i32], i32 1
  %stack.ptr_255 = alloca [4 x i32], i32 1
  %stack.ptr_256 = alloca [4 x i32], i32 1
  %stack.ptr_257 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_258 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_259 = alloca [1 x i32], i32 1
  %stack.ptr_260 = alloca [1 x i32], i32 1
  %stack.ptr_261 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_262 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_263 = alloca [2 x i32], i32 1
  %stack.ptr_264 = alloca [1 x i32], i32 1
  %stack.ptr_265 = alloca [1 x i32], i32 1
  %stack.ptr_266 = alloca [1 x i32], i32 1
  %stack.ptr_267 = alloca [1 x i32], i32 1
  %stack.ptr_268 = alloca [1 x i32], i32 1
  %stack.ptr_269 = alloca [3 x i32], i32 1
  %stack.ptr_270 = alloca [3 x i32], i32 1
  %stack.ptr_271 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_272 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_273 = alloca [2 x i32], i32 1
  %stack.ptr_274 = alloca [2 x i32], i32 1
  %stack.ptr_275 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_276 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_277 = alloca [2 x i32], i32 1
  %stack.ptr_278 = alloca [2 x i32], i32 1
  %stack.ptr_279 = alloca [2 x i32], i32 1
  %stack.ptr_280 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_281 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_282 = alloca [2 x i32], i32 1
  %stack.ptr_283 = alloca [2 x i32], i32 1
  %stack.ptr_284 = alloca [2 x i32], i32 1
  %stack.ptr_285 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_286 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_287 = alloca [2 x i32], i32 1
  %stack.ptr_288 = alloca [2 x i32], i32 1
  %stack.ptr_289 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_290 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_291 = alloca [2 x i32], i32 1
  %stack.ptr_292 = alloca [2 x i32], i32 1
  %stack.ptr_293 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_294 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_295 = alloca [2 x i32], i32 1
  %stack.ptr_296 = alloca [2 x i32], i32 1
  %stack.ptr_297 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_298 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_299 = alloca [2 x i32], i32 1
  %stack.ptr_300 = alloca [2 x i32], i32 1
  %stack.ptr_301 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_302 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_303 = alloca [2 x i32], i32 1
  %stack.ptr_304 = alloca [2 x i32], i32 1
  %stack.ptr_305 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_306 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_307 = alloca [3 x i32], i32 1
  %stack.ptr_308 = alloca [2 x i32], i32 1
  %stack.ptr_309 = alloca [2 x i32], i32 1
  %stack.ptr_310 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_311 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_312 = alloca [2 x i32], i32 1
  %stack.ptr_313 = alloca [2 x i32], i32 1
  %stack.ptr_314 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_315 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_316 = alloca [2 x i32], i32 1
  %stack.ptr_317 = alloca [2 x i32], i32 1
  %stack.ptr_318 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_319 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_320 = alloca [3 x i32], i32 1
  %stack.ptr_321 = alloca [3 x i32], i32 1
  %stack.ptr_322 = alloca [2 x i32], i32 1
  %stack.ptr_323 = alloca [2 x i32], i32 1
  %stack.ptr_324 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_325 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_326 = alloca [2 x i32], i32 1
  %stack.ptr_327 = alloca [2 x i32], i32 1
  %stack.ptr_328 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_329 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_330 = alloca [2 x i32], i32 1
  %stack.ptr_331 = alloca [2 x i32], i32 1
  %stack.ptr_332 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_333 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_334 = alloca [2 x i32], i32 1
  %stack.ptr_335 = alloca [2 x i32], i32 1
  %stack.ptr_336 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_337 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_338 = alloca [3 x i32], i32 1
  %stack.ptr_339 = alloca [2 x i32], i32 1
  %stack.ptr_340 = alloca [2 x i32], i32 1
  %stack.ptr_341 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_342 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_343 = alloca [2 x i32], i32 1
  %stack.ptr_344 = alloca [2 x i32], i32 1
  %stack.ptr_345 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_346 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_347 = alloca [1 x i32], i32 1
  %stack.ptr_348 = alloca [1 x i32], i32 1
  %stack.ptr_349 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_350 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_351 = alloca [1 x i32], i32 1
  %stack.ptr_352 = alloca [2 x i32], i32 1
  %stack.ptr_353 = alloca [2 x i32], i32 1
  %stack.ptr_354 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_355 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_356 = alloca [1 x i32], i32 1
  %stack.ptr_357 = alloca [1 x i32], i32 1
  %stack.ptr_358 = alloca [2 x i32], i32 1
  %stack.ptr_359 = alloca [2 x i32], i32 1
  %stack.ptr_360 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_361 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_362 = alloca [2 x i32], i32 1
  %stack.ptr_363 = alloca [2 x i32], i32 1
  %stack.ptr_364 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_365 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_366 = alloca [1 x i32], i32 1
  %stack.ptr_367 = alloca [2 x i32], i32 1
  %stack.ptr_368 = alloca [2 x i32], i32 1
  %stack.ptr_369 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_370 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_371 = alloca [2 x i32], i32 1
  %stack.ptr_372 = alloca [2 x i32], i32 1
  %stack.ptr_373 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_374 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_375 = alloca [1 x i32], i32 1
  %stack.ptr_376 = alloca [1 x i32], i32 1
  %stack.ptr_377 = alloca [1 x i32], i32 1
  %stack.ptr_378 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_379 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_380 = alloca [1 x i32], i32 1
  %stack.ptr_381 = alloca [2 x i32], i32 1
  %stack.ptr_382 = alloca [2 x i32], i32 1
  %stack.ptr_383 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_384 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_385 = alloca [2 x i32], i32 1
  %stack.ptr_386 = alloca [1 x i32], i32 1
  %stack.ptr_387 = alloca [1 x i32], i32 1
  %stack.ptr_388 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_389 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_390 = alloca [1 x i32], i32 1
  %stack.ptr_391 = alloca [2 x i32], i32 1
  %stack.ptr_392 = alloca [2 x i32], i32 1
  %stack.ptr_393 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_394 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_395 = alloca [2 x i32], i32 1
  %stack.ptr_396 = alloca [2 x i32], i32 1
  %stack.ptr_397 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_398 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_399 = alloca [1 x i32], i32 1
  %stack.ptr_400 = alloca [1 x i32], i32 1
  %stack.ptr_401 = alloca [1 x i32], i32 1
  %stack.ptr_402 = alloca [1 x i32], i32 1
  %stack.ptr_403 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_404 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_405 = alloca [1 x i32], i32 1
  %stack.ptr_406 = alloca [1 x i32], i32 1
  %stack.ptr_407 = alloca [1 x i32], i32 1
  %stack.ptr_408 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_409 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_410 = alloca [1 x i32], i32 1
  %stack.ptr_411 = alloca [1 x i32], i32 1
  %stack.ptr_412 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_413 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_414 = alloca [1 x i32], i32 1
  %stack.ptr_415 = alloca [2 x i32], i32 1
  %stack.ptr_416 = alloca [2 x i32], i32 1
  %stack.ptr_417 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_418 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_419 = alloca [3 x i32], i32 1
  %stack.ptr_420 = alloca [3 x i32], i32 1
  %stack.ptr_421 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_422 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_423 = alloca [2 x i32], i32 1
  %stack.ptr_424 = alloca [2 x i32], i32 1
  %stack.ptr_425 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_426 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_427 = alloca [2 x i32], i32 1
  %stack.ptr_428 = alloca [2 x i32], i32 1
  %stack.ptr_429 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_430 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_431 = alloca [2 x i32], i32 1
  %stack.ptr_432 = alloca [2 x i32], i32 1
  %stack.ptr_433 = alloca [2 x i32], i32 1
  %stack.ptr_434 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_435 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_436 = alloca [3 x i32], i32 1
  %stack.ptr_437 = alloca [3 x i32], i32 1
  %stack.ptr_438 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_439 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_440 = alloca [2 x i32], i32 1
  %stack.ptr_441 = alloca [2 x i32], i32 1
  %stack.ptr_442 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_443 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_444 = alloca [2 x i32], i32 1
  %stack.ptr_445 = alloca [2 x i32], i32 1
  %stack.ptr_446 = alloca [2 x i32], i32 1
  %stack.ptr_447 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_448 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_449 = alloca [2 x i32], i32 1
  %stack.ptr_450 = alloca [2 x i32], i32 1
  %stack.ptr_451 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_452 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_453 = alloca [2 x i32], i32 1
  %stack.ptr_454 = alloca [2 x i32], i32 1
  %stack.ptr_455 = alloca [2 x i32], i32 1
  %stack.ptr_456 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_457 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_458 = alloca [3 x i32], i32 1
  %stack.ptr_459 = alloca [3 x i32], i32 1
  %stack.ptr_460 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_461 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_462 = alloca [3 x i32], i32 1
  %stack.ptr_463 = alloca [3 x i32], i32 1
  %stack.ptr_464 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_465 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_466 = alloca [2 x i32], i32 1
  %stack.ptr_467 = alloca [2 x i32], i32 1
  %stack.ptr_468 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_469 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_470 = alloca [2 x i32], i32 1
  %stack.ptr_471 = alloca [2 x i32], i32 1
  %stack.ptr_472 = alloca [2 x i32], i32 1
  %stack.ptr_473 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_474 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_475 = alloca [3 x i32], i32 1
  %stack.ptr_476 = alloca [3 x i32], i32 1
  %stack.ptr_477 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_478 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_479 = alloca [3 x i32], i32 1
  %stack.ptr_480 = alloca [3 x i32], i32 1
  %stack.ptr_481 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_482 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_483 = alloca [2 x i32], i32 1
  %stack.ptr_484 = alloca [2 x i32], i32 1
  %stack.ptr_485 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_486 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_487 = alloca [2 x i32], i32 1
  %stack.ptr_488 = alloca [2 x i32], i32 1
  %stack.ptr_489 = alloca [2 x i32], i32 1
  %stack.ptr_490 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_491 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_492 = alloca [2 x i32], i32 1
  %stack.ptr_493 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_494 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_495 = alloca [2 x i32], i32 1
  %stack.ptr_496 = alloca [2 x i32], i32 1
  %stack.ptr_497 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_498 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_499 = alloca [2 x i32], i32 1
  %stack.ptr_500 = alloca [2 x i32], i32 1
  %stack.ptr_501 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_502 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_503 = alloca [2 x i32], i32 1
  %stack.ptr_504 = alloca [2 x i32], i32 1
  %stack.ptr_505 = alloca [2 x i32], i32 1
  %stack.ptr_506 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_507 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_508 = alloca [2 x i32], i32 1
  %stack.ptr_509 = alloca [2 x i32], i32 1
  %stack.ptr_510 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_511 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_512 = alloca [1 x i32], i32 1
  %stack.ptr_513 = alloca [1 x i32], i32 1
  %stack.ptr_514 = alloca [1 x i32], i32 1
  %stack.ptr_515 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_516 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_517 = alloca [2 x i32], i32 1
  %stack.ptr_518 = alloca [2 x i32], i32 1
  %stack.ptr_519 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_520 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_521 = alloca [2 x i32], i32 1
  %stack.ptr_522 = alloca [2 x i32], i32 1
  %stack.ptr_523 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_524 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_525 = alloca [3 x i32], i32 1
  %stack.ptr_526 = alloca [3 x i32], i32 1
  %stack.ptr_527 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_528 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_529 = alloca [2 x i32], i32 1
  %stack.ptr_530 = alloca [2 x i32], i32 1
  %stack.ptr_531 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_532 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_533 = alloca [2 x i32], i32 1
  %stack.ptr_534 = alloca [2 x i32], i32 1
  %stack.ptr_535 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_536 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_537 = alloca [2 x i32], i32 1
  %stack.ptr_538 = alloca [2 x i32], i32 1
  %stack.ptr_539 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_540 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_541 = alloca [1 x i32], i32 1
  %stack.ptr_542 = alloca [1 x i32], i32 1
  %stack.ptr_543 = alloca [1 x i32], i32 1
  %stack.ptr_544 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_545 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_546 = alloca [2 x i32], i32 1
  %stack.ptr_547 = alloca [2 x i32], i32 1
  %stack.ptr_548 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_549 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_550 = alloca [1 x i32], i32 1
  %stack.ptr_551 = alloca [1 x i32], i32 1
  %stack.ptr_552 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_553 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_554 = alloca [1 x i32], i32 1
  %stack.ptr_555 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_556 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_557 = alloca [2 x i32], i32 1
  %stack.ptr_558 = alloca [2 x i32], i32 1
  %stack.ptr_559 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_560 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_561 = alloca [1 x i32], i32 1
  %stack.ptr_562 = alloca [1 x i32], i32 1
  %stack.ptr_563 = alloca [1 x i32], i32 1
  %stack.ptr_564 = alloca [1 x i32], i32 1
  %stack.ptr_565 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_566 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_567 = alloca [1 x i32], i32 1
  %stack.ptr_568 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_569 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_570 = alloca [2 x i32], i32 1
  %stack.ptr_571 = alloca [2 x i32], i32 1
  %stack.ptr_572 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_573 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_574 = alloca [1 x i32], i32 1
  %stack.ptr_575 = alloca [1 x i32], i32 1
  %stack.ptr_576 = alloca [2 x i32], i32 1
  %stack.ptr_577 = alloca [2 x i32], i32 1
  %stack.ptr_578 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_579 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_580 = alloca [1 x i32], i32 1
  %stack.ptr_581 = alloca [1 x i32], i32 1
  %stack.ptr_582 = alloca [2 x i32], i32 1
  %stack.ptr_583 = alloca [2 x i32], i32 1
  %stack.ptr_584 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_585 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_586 = alloca [1 x i32], i32 1
  %stack.ptr_587 = alloca [1 x i32], i32 1
  %stack.ptr_588 = alloca [2 x i32], i32 1
  %stack.ptr_589 = alloca [2 x i32], i32 1
  %stack.ptr_590 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_591 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_592 = alloca [1 x i32], i32 1
  %stack.ptr_593 = alloca [1 x i32], i32 1
  %stack.ptr_594 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_595 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_596 = alloca [3 x i32], i32 1
  %stack.ptr_597 = alloca [3 x i32], i32 1
  %stack.ptr_598 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_599 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_600 = alloca [1 x i32], i32 1
  %stack.ptr_601 = alloca [1 x i32], i32 1
  %stack.ptr_602 = alloca [1 x i32], i32 1
  %stack.ptr_603 = alloca [1 x i32], i32 1
  %stack.ptr_604 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_605 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_606 = alloca [1 x i32], i32 1
  %stack.ptr_607 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_608 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_609 = alloca [2 x i32], i32 1
  %stack.ptr_610 = alloca [2 x i32], i32 1
  %stack.ptr_611 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_612 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_613 = alloca [2 x i32], i32 1
  %stack.ptr_614 = alloca [2 x i32], i32 1
  %stack.ptr_615 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_616 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_617 = alloca [2 x i32], i32 1
  %stack.ptr_618 = alloca [2 x i32], i32 1
  %stack.ptr_619 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_620 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_621 = alloca [3 x i32], i32 1
  %stack.ptr_622 = alloca [3 x i32], i32 1
  %stack.ptr_623 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_624 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_625 = alloca [1 x i32], i32 1
  %stack.ptr_626 = alloca [1 x i32], i32 1
  %stack.ptr_627 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_628 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_629 = alloca [3 x i32], i32 1
  %stack.ptr_630 = alloca [2 x i32], i32 1
  %stack.ptr_631 = alloca [2 x i32], i32 1
  %stack.ptr_632 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_633 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_634 = alloca [2 x i32], i32 1
  %stack.ptr_635 = alloca [2 x i32], i32 1
  %stack.ptr_636 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_637 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_638 = alloca [2 x i32], i32 1
  %stack.ptr_639 = alloca [3 x i32], i32 1
  %stack.ptr_640 = alloca [3 x i32], i32 1
  %stack.ptr_641 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_642 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_643 = alloca [2 x i32], i32 1
  %stack.ptr_644 = alloca [2 x i32], i32 1
  %stack.ptr_645 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_646 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_647 = alloca [3 x i32], i32 1
  %stack.ptr_648 = alloca [3 x i32], i32 1
  %stack.ptr_649 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_650 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_651 = alloca [2 x i32], i32 1
  %stack.ptr_652 = alloca [2 x i32], i32 1
  %stack.ptr_653 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_654 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_655 = alloca [2 x i32], i32 1
  %stack.ptr_656 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_657 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_658 = alloca [3 x i32], i32 1
  %stack.ptr_659 = alloca [3 x i32], i32 1
  %stack.ptr_660 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_661 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_662 = alloca [3 x i32], i32 1
  %stack.ptr_663 = alloca [3 x i32], i32 1
  %stack.ptr_664 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_665 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_666 = alloca [2 x i32], i32 1
  %stack.ptr_667 = alloca [2 x i32], i32 1
  %stack.ptr_668 = alloca [2 x i32], i32 1
  %stack.ptr_669 = alloca [2 x i32], i32 1
  %stack.ptr_670 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_671 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_672 = alloca [2 x i32], i32 1
  %stack.ptr_673 = alloca [2 x i32], i32 1
  %stack.ptr_674 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_675 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_676 = alloca [2 x i32], i32 1
  %stack.ptr_677 = alloca [3 x i32], i32 1
  %stack.ptr_678 = alloca [3 x i32], i32 1
  %stack.ptr_679 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_680 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_681 = alloca [3 x i32], i32 1
  %stack.ptr_682 = alloca [3 x i32], i32 1
  %stack.ptr_683 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_684 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_685 = alloca [2 x i32], i32 1
  %stack.ptr_686 = alloca [2 x i32], i32 1
  %stack.ptr_687 = alloca [2 x i32], i32 1
  %stack.ptr_688 = alloca [2 x i32], i32 1
  %stack.ptr_689 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_690 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_691 = alloca [2 x i32], i32 1
  %stack.ptr_692 = alloca [2 x i32], i32 1
  %stack.ptr_693 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_694 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_695 = alloca [2 x i32], i32 1
  %stack.ptr_696 = alloca [2 x i32], i32 1
  %stack.ptr_697 = alloca [2 x i32], i32 1
  %stack.ptr_698 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_699 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_700 = alloca [2 x i32], i32 1
  %stack.ptr_701 = alloca [2 x i32], i32 1
  %stack.ptr_702 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_703 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_704 = alloca [2 x i32], i32 1
  %stack.ptr_705 = alloca [4 x i32], i32 1
  %stack.ptr_706 = alloca [4 x i32], i32 1
  %stack.ptr_707 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_708 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_709 = alloca [2 x i32], i32 1
  %stack.ptr_710 = alloca [2 x i32], i32 1
  %stack.ptr_711 = alloca [2 x i32], i32 1
  %stack.ptr_712 = alloca [2 x i32], i32 1
  %stack.ptr_713 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_714 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_715 = alloca [3 x i32], i32 1
  %stack.ptr_716 = alloca [3 x i32], i32 1
  %stack.ptr_717 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_718 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_719 = alloca [2 x i32], i32 1
  %stack.ptr_720 = alloca [2 x i32], i32 1
  %stack.ptr_721 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_722 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_723 = alloca [2 x i32], i32 1
  %stack.ptr_724 = alloca [2 x i32], i32 1
  %stack.ptr_725 = alloca [2 x i32], i32 1
  %stack.ptr_726 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_727 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_728 = alloca [2 x i32], i32 1
  %stack.ptr_729 = alloca [2 x i32], i32 1
  %stack.ptr_730 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_731 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_732 = alloca [2 x i32], i32 1
  %stack.ptr_733 = alloca [2 x i32], i32 1
  %stack.ptr_734 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_735 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_736 = alloca [2 x i32], i32 1
  %stack.ptr_737 = alloca [2 x i32], i32 1
  %stack.ptr_738 = alloca [2 x i32], i32 1
  %stack.ptr_739 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_740 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_741 = alloca [2 x i32], i32 1
  %stack.ptr_742 = alloca [2 x i32], i32 1
  %stack.ptr_743 = alloca [2 x i32], i32 1
  %stack.ptr_744 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_745 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_746 = alloca [2 x i32], i32 1
  %stack.ptr_747 = alloca [2 x i32], i32 1
  %stack.ptr_748 = alloca [2 x i32], i32 1
  %stack.ptr_749 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_750 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_751 = alloca [2 x i32], i32 1
  %stack.ptr_752 = alloca [2 x i32], i32 1
  %stack.ptr_753 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_754 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_755 = alloca [2 x i32], i32 1
  %stack.ptr_756 = alloca [2 x i32], i32 1
  %stack.ptr_757 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_758 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_759 = alloca [2 x i32], i32 1
  %stack.ptr_760 = alloca [2 x i32], i32 1
  %stack.ptr_761 = alloca [2 x i32], i32 1
  %stack.ptr_762 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_763 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_764 = alloca [2 x i32], i32 1
  %stack.ptr_765 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_766 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_767 = alloca [2 x i32], i32 1
  %stack.ptr_768 = alloca [2 x i32], i32 1
  %stack.ptr_769 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_770 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_771 = alloca [3 x i32], i32 1
  %stack.ptr_772 = alloca [3 x i32], i32 1
  %stack.ptr_773 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_774 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_775 = alloca [2 x i32], i32 1
  %stack.ptr_776 = alloca [2 x i32], i32 1
  %stack.ptr_777 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_778 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_779 = alloca [2 x i32], i32 1
  %stack.ptr_780 = alloca [2 x i32], i32 1
  %stack.ptr_781 = alloca [2 x i32], i32 1
  %stack.ptr_782 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_783 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_784 = alloca [3 x i32], i32 1
  %stack.ptr_785 = alloca [2 x i32], i32 1
  %stack.ptr_786 = alloca [2 x i32], i32 1
  %stack.ptr_787 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_788 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_789 = alloca [3 x i32], i32 1
  %stack.ptr_790 = alloca [3 x i32], i32 1
  %stack.ptr_791 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_792 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_793 = alloca [2 x i32], i32 1
  %stack.ptr_794 = alloca [2 x i32], i32 1
  %stack.ptr_795 = alloca [2 x i32], i32 1
  %stack.ptr_796 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_797 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_798 = alloca [3 x i32], i32 1
  %stack.ptr_799 = alloca [3 x i32], i32 1
  %stack.ptr_800 = alloca [3 x i32], i32 1
  %stack.ptr_801 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_802 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_803 = alloca [2 x i32], i32 1
  %stack.ptr_804 = alloca [2 x i32], i32 1
  %stack.ptr_805 = alloca [2 x i32], i32 1
  %stack.ptr_806 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_807 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_808 = alloca [2 x i32], i32 1
  %stack.ptr_809 = alloca [2 x i32], i32 1
  %stack.ptr_810 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_811 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_812 = alloca [3 x i32], i32 1
  %stack.ptr_813 = alloca [3 x i32], i32 1
  %stack.ptr_814 = alloca [3 x i32], i32 1
  %stack.ptr_815 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_816 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_817 = alloca [4 x i32], i32 1
  %stack.ptr_818 = alloca [4 x i32], i32 1
  %stack.ptr_819 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_820 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_821 = alloca [2 x i32], i32 1
  %stack.ptr_822 = alloca [1 x i32], i32 1
  %stack.ptr_823 = alloca [1 x i32], i32 1
  %stack.ptr_824 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_825 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_826 = alloca [2 x i32], i32 1
  %stack.ptr_827 = alloca [2 x i32], i32 1
  %stack.ptr_828 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_829 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_830 = alloca [2 x i32], i32 1
  %stack.ptr_831 = alloca [2 x i32], i32 1
  %stack.ptr_832 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_833 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_834 = alloca [3 x i32], i32 1
  %stack.ptr_835 = alloca [3 x i32], i32 1
  %stack.ptr_836 = alloca [3 x i32], i32 1
  %stack.ptr_837 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_838 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_839 = alloca [4 x i32], i32 1
  %stack.ptr_840 = alloca [4 x i32], i32 1
  %stack.ptr_841 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_842 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_843 = alloca [2 x i32], i32 1
  %stack.ptr_844 = alloca [1 x i32], i32 1
  %stack.ptr_845 = alloca [1 x i32], i32 1
  %stack.ptr_846 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_847 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_848 = alloca [2 x i32], i32 1
  %stack.ptr_849 = alloca [2 x i32], i32 1
  %stack.ptr_850 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_851 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_852 = alloca [2 x i32], i32 1
  %stack.ptr_853 = alloca [2 x i32], i32 1
  %stack.ptr_854 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_855 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_856 = alloca [3 x i32], i32 1
  %stack.ptr_857 = alloca [4 x i32], i32 1
  %stack.ptr_858 = alloca [4 x i32], i32 1
  %stack.ptr_859 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_860 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_861 = alloca [2 x i32], i32 1
  %stack.ptr_862 = alloca [2 x i32], i32 1
  %stack.ptr_863 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_864 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_865 = alloca [2 x i32], i32 1
  %stack.ptr_866 = alloca [2 x i32], i32 1
  %stack.ptr_867 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_868 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_869 = alloca [2 x i32], i32 1
  %stack.ptr_870 = alloca [3 x i32], i32 1
  %stack.ptr_871 = alloca [4 x i32], i32 1
  %stack.ptr_872 = alloca [4 x i32], i32 1
  %stack.ptr_873 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_874 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_875 = alloca [2 x i32], i32 1
  %stack.ptr_876 = alloca [2 x i32], i32 1
  %stack.ptr_877 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_878 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_879 = alloca [2 x i32], i32 1
  %stack.ptr_880 = alloca [2 x i32], i32 1
  %stack.ptr_881 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_882 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_883 = alloca [2 x i32], i32 1
  %stack.ptr_884 = alloca [3 x i32], i32 1
  %stack.ptr_885 = alloca [2 x i32], i32 1
  %stack.ptr_886 = alloca [2 x i32], i32 1
  %stack.ptr_887 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_888 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_889 = alloca [2 x i32], i32 1
  %stack.ptr_890 = alloca [2 x i32], i32 1
  %stack.ptr_891 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_892 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_893 = alloca [2 x i32], i32 1
  %stack.ptr_894 = alloca [3 x i32], i32 1
  %stack.ptr_895 = alloca [3 x i32], i32 1
  %stack.ptr_896 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_897 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_898 = alloca [2 x i32], i32 1
  %stack.ptr_899 = alloca [3 x i32], i32 1
  %stack.ptr_900 = alloca [3 x i32], i32 1
  %stack.ptr_901 = alloca [3 x i32], i32 1
  %stack.ptr_902 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_903 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_904 = alloca [4 x i32], i32 1
  %stack.ptr_905 = alloca [4 x i32], i32 1
  %stack.ptr_906 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_907 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_908 = alloca [2 x i32], i32 1
  %stack.ptr_909 = alloca [2 x i32], i32 1
  %stack.ptr_910 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_911 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_912 = alloca [2 x i32], i32 1
  %stack.ptr_913 = alloca [3 x i32], i32 1
  %stack.ptr_914 = alloca [3 x i32], i32 1
  %stack.ptr_915 = alloca [3 x i32], i32 1
  %stack.ptr_916 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_917 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_918 = alloca [4 x i32], i32 1
  %stack.ptr_919 = alloca [4 x i32], i32 1
  %stack.ptr_920 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_921 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_922 = alloca [2 x i32], i32 1
  %stack.ptr_923 = alloca [2 x i32], i32 1
  %stack.ptr_924 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_925 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_926 = alloca [2 x i32], i32 1
  %stack.ptr_927 = alloca [3 x i32], i32 1
  %stack.ptr_928 = alloca [4 x i32], i32 1
  %stack.ptr_929 = alloca [4 x i32], i32 1
  %stack.ptr_930 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_931 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_932 = alloca [2 x i32], i32 1
  %stack.ptr_933 = alloca [2 x i32], i32 1
  %stack.ptr_934 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_935 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_936 = alloca [2 x i32], i32 1
  %stack.ptr_937 = alloca [2 x i32], i32 1
  %stack.ptr_938 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_939 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_940 = alloca [2 x i32], i32 1
  %stack.ptr_941 = alloca [2 x i32], i32 1
  %stack.ptr_942 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_943 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_944 = alloca [2 x i32], i32 1
  %stack.ptr_945 = alloca [3 x i32], i32 1
  %stack.ptr_946 = alloca [4 x i32], i32 1
  %stack.ptr_947 = alloca [4 x i32], i32 1
  %stack.ptr_948 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_949 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_950 = alloca [2 x i32], i32 1
  %stack.ptr_951 = alloca [2 x i32], i32 1
  %stack.ptr_952 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_953 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_954 = alloca [2 x i32], i32 1
  %stack.ptr_955 = alloca [2 x i32], i32 1
  %stack.ptr_956 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_957 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_958 = alloca [2 x i32], i32 1
  %stack.ptr_959 = alloca [2 x i32], i32 1
  %stack.ptr_960 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_961 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_962 = alloca [2 x i32], i32 1
  %stack.ptr_963 = alloca [3 x i32], i32 1
  %0 = getelementptr [2 x i32], ptr %stack.ptr_0, i32 0, i32 0
  store i32 0, ptr %0
  %1 = getelementptr [2 x i32], ptr %stack.ptr_0, i32 0, i32 1
  store i32 0, ptr %1
  %2 = getelementptr [2 x i32], ptr %stack.ptr_1, i32 0, i32 0
  store i32 4294967295, ptr %2
  %3 = getelementptr [2 x i32], ptr %stack.ptr_1, i32 0, i32 1
  store i32 4294967295, ptr %3
  %4 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %4, ptr %stack.ptr_0, ptr %stack.ptr_2)
  %5 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %5, ptr %stack.ptr_1, ptr %stack.ptr_3)
  br label %loop_0
loop_0:
  %6 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_2, ptr %stack.ptr_3)
  br i1 %6, label %if_0, label %end_if_0
if_0:
  br label %range_query.end
end_if_0:
  %7 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_2)
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
  %16 = getelementptr %program, ptr %arg_0, i32 0, i32 51
  call ccc void @eclair_btree_lower_bound_2(ptr %16, ptr %stack.ptr_4, ptr %stack.ptr_6)
  %17 = getelementptr %program, ptr %arg_0, i32 0, i32 51
  call ccc void @eclair_btree_upper_bound_2(ptr %17, ptr %stack.ptr_5, ptr %stack.ptr_7)
  br label %loop_1
loop_1:
  %18 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_6, ptr %stack.ptr_7)
  br i1 %18, label %if_1, label %end_if_1
if_1:
  br label %range_query.end_1
end_if_1:
  %19 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_6)
  %20 = getelementptr [3 x i32], ptr %stack.ptr_8, i32 0, i32 0
  %21 = getelementptr [2 x i32], ptr %19, i32 0, i32 0
  %22 = load i32, ptr %21
  store i32 %22, ptr %20
  %23 = getelementptr [3 x i32], ptr %stack.ptr_8, i32 0, i32 1
  %24 = getelementptr [2 x i32], ptr %7, i32 0, i32 0
  %25 = load i32, ptr %24
  store i32 %25, ptr %23
  %26 = getelementptr [3 x i32], ptr %stack.ptr_8, i32 0, i32 2
  %27 = getelementptr [2 x i32], ptr %7, i32 0, i32 1
  %28 = load i32, ptr %27
  store i32 %28, ptr %26
  %29 = getelementptr %program, ptr %arg_0, i32 0, i32 26
  %30 = call ccc i1 @eclair_btree_insert_value_0(ptr %29, ptr %stack.ptr_8)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_6)
  br label %loop_1
range_query.end_1:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_2)
  br label %loop_0
range_query.end:
  %31 = getelementptr [2 x i32], ptr %stack.ptr_9, i32 0, i32 0
  store i32 0, ptr %31
  %32 = getelementptr [2 x i32], ptr %stack.ptr_9, i32 0, i32 1
  store i32 0, ptr %32
  %33 = getelementptr [2 x i32], ptr %stack.ptr_10, i32 0, i32 0
  store i32 4294967295, ptr %33
  %34 = getelementptr [2 x i32], ptr %stack.ptr_10, i32 0, i32 1
  store i32 4294967295, ptr %34
  %35 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %35, ptr %stack.ptr_9, ptr %stack.ptr_11)
  %36 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %36, ptr %stack.ptr_10, ptr %stack.ptr_12)
  br label %loop_2
loop_2:
  %37 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_11, ptr %stack.ptr_12)
  br i1 %37, label %if_2, label %end_if_2
if_2:
  br label %range_query.end_2
end_if_2:
  %38 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_11)
  %39 = getelementptr [2 x i32], ptr %stack.ptr_13, i32 0, i32 0
  store i32 0, ptr %39
  %40 = getelementptr [2 x i32], ptr %stack.ptr_13, i32 0, i32 1
  store i32 0, ptr %40
  %41 = getelementptr [2 x i32], ptr %stack.ptr_14, i32 0, i32 0
  store i32 4294967295, ptr %41
  %42 = getelementptr [2 x i32], ptr %stack.ptr_14, i32 0, i32 1
  store i32 4294967295, ptr %42
  %43 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %43, ptr %stack.ptr_13, ptr %stack.ptr_15)
  %44 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %44, ptr %stack.ptr_14, ptr %stack.ptr_16)
  br label %loop_3
loop_3:
  %45 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_15, ptr %stack.ptr_16)
  br i1 %45, label %if_3, label %end_if_3
if_3:
  br label %range_query.end_3
end_if_3:
  %46 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_15)
  %47 = getelementptr [2 x i32], ptr %stack.ptr_17, i32 0, i32 0
  %48 = getelementptr [2 x i32], ptr %46, i32 0, i32 1
  %49 = load i32, ptr %48
  store i32 %49, ptr %47
  %50 = getelementptr [2 x i32], ptr %stack.ptr_17, i32 0, i32 1
  %51 = getelementptr [2 x i32], ptr %38, i32 0, i32 1
  %52 = load i32, ptr %51
  store i32 %52, ptr %50
  %53 = getelementptr [2 x i32], ptr %stack.ptr_18, i32 0, i32 0
  %54 = getelementptr [2 x i32], ptr %46, i32 0, i32 1
  %55 = load i32, ptr %54
  store i32 %55, ptr %53
  %56 = getelementptr [2 x i32], ptr %stack.ptr_18, i32 0, i32 1
  %57 = getelementptr [2 x i32], ptr %38, i32 0, i32 1
  %58 = load i32, ptr %57
  store i32 %58, ptr %56
  %59 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %59, ptr %stack.ptr_17, ptr %stack.ptr_19)
  %60 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %60, ptr %stack.ptr_18, ptr %stack.ptr_20)
  br label %loop_4
loop_4:
  %61 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_19, ptr %stack.ptr_20)
  br i1 %61, label %if_4, label %end_if_4
if_4:
  br label %range_query.end_4
end_if_4:
  %62 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_19)
  %63 = getelementptr [3 x i32], ptr %stack.ptr_21, i32 0, i32 0
  %64 = getelementptr [2 x i32], ptr %46, i32 0, i32 1
  %65 = load i32, ptr %64
  store i32 %65, ptr %63
  %66 = getelementptr [3 x i32], ptr %stack.ptr_21, i32 0, i32 1
  %67 = getelementptr [2 x i32], ptr %38, i32 0, i32 0
  %68 = load i32, ptr %67
  store i32 %68, ptr %66
  %69 = getelementptr [3 x i32], ptr %stack.ptr_21, i32 0, i32 2
  %70 = getelementptr [2 x i32], ptr %38, i32 0, i32 1
  %71 = load i32, ptr %70
  store i32 %71, ptr %69
  %72 = getelementptr %program, ptr %arg_0, i32 0, i32 25
  %73 = call ccc i1 @eclair_btree_insert_value_0(ptr %72, ptr %stack.ptr_21)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_19)
  br label %loop_4
range_query.end_4:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_15)
  br label %loop_3
range_query.end_3:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_11)
  br label %loop_2
range_query.end_2:
  %74 = getelementptr [2 x i32], ptr %stack.ptr_22, i32 0, i32 0
  store i32 0, ptr %74
  %75 = getelementptr [2 x i32], ptr %stack.ptr_22, i32 0, i32 1
  store i32 0, ptr %75
  %76 = getelementptr [2 x i32], ptr %stack.ptr_23, i32 0, i32 0
  store i32 4294967295, ptr %76
  %77 = getelementptr [2 x i32], ptr %stack.ptr_23, i32 0, i32 1
  store i32 4294967295, ptr %77
  %78 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %78, ptr %stack.ptr_22, ptr %stack.ptr_24)
  %79 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %79, ptr %stack.ptr_23, ptr %stack.ptr_25)
  br label %loop_5
loop_5:
  %80 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_24, ptr %stack.ptr_25)
  br i1 %80, label %if_5, label %end_if_5
if_5:
  br label %range_query.end_5
end_if_5:
  %81 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_24)
  %82 = getelementptr [2 x i32], ptr %stack.ptr_26, i32 0, i32 0
  store i32 0, ptr %82
  %83 = getelementptr [2 x i32], ptr %stack.ptr_26, i32 0, i32 1
  %84 = getelementptr [2 x i32], ptr %81, i32 0, i32 1
  %85 = load i32, ptr %84
  store i32 %85, ptr %83
  %86 = getelementptr [2 x i32], ptr %stack.ptr_27, i32 0, i32 0
  store i32 4294967295, ptr %86
  %87 = getelementptr [2 x i32], ptr %stack.ptr_27, i32 0, i32 1
  %88 = getelementptr [2 x i32], ptr %81, i32 0, i32 1
  %89 = load i32, ptr %88
  store i32 %89, ptr %87
  %90 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_lower_bound_2(ptr %90, ptr %stack.ptr_26, ptr %stack.ptr_28)
  %91 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_upper_bound_2(ptr %91, ptr %stack.ptr_27, ptr %stack.ptr_29)
  br label %loop_6
loop_6:
  %92 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_28, ptr %stack.ptr_29)
  br i1 %92, label %if_6, label %end_if_6
if_6:
  br label %range_query.end_6
end_if_6:
  %93 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_28)
  %94 = getelementptr [2 x i32], ptr %81, i32 0, i32 0
  %95 = load i32, ptr %94
  %96 = getelementptr [2 x i32], ptr %93, i32 0, i32 0
  %97 = load i32, ptr %96
  %98 = icmp ult i32 %95, %97
  br i1 %98, label %if_7, label %end_if_7
if_7:
  %99 = getelementptr [3 x i32], ptr %stack.ptr_30, i32 0, i32 0
  %100 = getelementptr [2 x i32], ptr %81, i32 0, i32 0
  %101 = load i32, ptr %100
  store i32 %101, ptr %99
  %102 = getelementptr [3 x i32], ptr %stack.ptr_30, i32 0, i32 1
  %103 = getelementptr [2 x i32], ptr %93, i32 0, i32 0
  %104 = load i32, ptr %103
  store i32 %104, ptr %102
  %105 = getelementptr [3 x i32], ptr %stack.ptr_30, i32 0, i32 2
  %106 = getelementptr [2 x i32], ptr %81, i32 0, i32 1
  %107 = load i32, ptr %106
  store i32 %107, ptr %105
  %108 = getelementptr %program, ptr %arg_0, i32 0, i32 7
  %109 = call ccc i1 @eclair_btree_insert_value_0(ptr %108, ptr %stack.ptr_30)
  br label %end_if_7
end_if_7:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_28)
  br label %loop_6
range_query.end_6:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_24)
  br label %loop_5
range_query.end_5:
  %110 = getelementptr [2 x i32], ptr %stack.ptr_31, i32 0, i32 0
  store i32 0, ptr %110
  %111 = getelementptr [2 x i32], ptr %stack.ptr_31, i32 0, i32 1
  store i32 0, ptr %111
  %112 = getelementptr [2 x i32], ptr %stack.ptr_32, i32 0, i32 0
  store i32 4294967295, ptr %112
  %113 = getelementptr [2 x i32], ptr %stack.ptr_32, i32 0, i32 1
  store i32 4294967295, ptr %113
  %114 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_lower_bound_1(ptr %114, ptr %stack.ptr_31, ptr %stack.ptr_33)
  %115 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_upper_bound_1(ptr %115, ptr %stack.ptr_32, ptr %stack.ptr_34)
  br label %loop_7
loop_7:
  %116 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_33, ptr %stack.ptr_34)
  br i1 %116, label %if_8, label %end_if_8
if_8:
  br label %range_query.end_7
end_if_8:
  %117 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_33)
  %118 = getelementptr [2 x i32], ptr %stack.ptr_35, i32 0, i32 0
  store i32 0, ptr %118
  %119 = getelementptr [2 x i32], ptr %stack.ptr_35, i32 0, i32 1
  %120 = getelementptr [2 x i32], ptr %117, i32 0, i32 1
  %121 = load i32, ptr %120
  store i32 %121, ptr %119
  %122 = getelementptr [2 x i32], ptr %stack.ptr_36, i32 0, i32 0
  store i32 4294967295, ptr %122
  %123 = getelementptr [2 x i32], ptr %stack.ptr_36, i32 0, i32 1
  %124 = getelementptr [2 x i32], ptr %117, i32 0, i32 1
  %125 = load i32, ptr %124
  store i32 %125, ptr %123
  %126 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %126, ptr %stack.ptr_35, ptr %stack.ptr_37)
  %127 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %127, ptr %stack.ptr_36, ptr %stack.ptr_38)
  br label %loop_8
loop_8:
  %128 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_37, ptr %stack.ptr_38)
  br i1 %128, label %if_9, label %end_if_9
if_9:
  br label %range_query.end_8
end_if_9:
  %129 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_37)
  %130 = getelementptr [2 x i32], ptr %117, i32 0, i32 0
  %131 = load i32, ptr %130
  %132 = getelementptr [2 x i32], ptr %129, i32 0, i32 0
  %133 = load i32, ptr %132
  %134 = icmp ult i32 %131, %133
  br i1 %134, label %if_10, label %end_if_10
if_10:
  %135 = getelementptr [3 x i32], ptr %stack.ptr_39, i32 0, i32 0
  %136 = getelementptr [2 x i32], ptr %117, i32 0, i32 0
  %137 = load i32, ptr %136
  store i32 %137, ptr %135
  %138 = getelementptr [3 x i32], ptr %stack.ptr_39, i32 0, i32 1
  %139 = getelementptr [2 x i32], ptr %129, i32 0, i32 0
  %140 = load i32, ptr %139
  store i32 %140, ptr %138
  %141 = getelementptr [3 x i32], ptr %stack.ptr_39, i32 0, i32 2
  %142 = getelementptr [2 x i32], ptr %117, i32 0, i32 1
  %143 = load i32, ptr %142
  store i32 %143, ptr %141
  %144 = getelementptr %program, ptr %arg_0, i32 0, i32 7
  %145 = call ccc i1 @eclair_btree_insert_value_0(ptr %144, ptr %stack.ptr_39)
  br label %end_if_10
end_if_10:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_37)
  br label %loop_8
range_query.end_8:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_33)
  br label %loop_7
range_query.end_7:
  %146 = getelementptr [2 x i32], ptr %stack.ptr_40, i32 0, i32 0
  store i32 0, ptr %146
  %147 = getelementptr [2 x i32], ptr %stack.ptr_40, i32 0, i32 1
  store i32 0, ptr %147
  %148 = getelementptr [2 x i32], ptr %stack.ptr_41, i32 0, i32 0
  store i32 4294967295, ptr %148
  %149 = getelementptr [2 x i32], ptr %stack.ptr_41, i32 0, i32 1
  store i32 4294967295, ptr %149
  %150 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %150, ptr %stack.ptr_40, ptr %stack.ptr_42)
  %151 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %151, ptr %stack.ptr_41, ptr %stack.ptr_43)
  br label %loop_9
loop_9:
  %152 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_42, ptr %stack.ptr_43)
  br i1 %152, label %if_11, label %end_if_11
if_11:
  br label %range_query.end_9
end_if_11:
  %153 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_42)
  %154 = getelementptr [2 x i32], ptr %stack.ptr_44, i32 0, i32 0
  store i32 0, ptr %154
  %155 = getelementptr [2 x i32], ptr %stack.ptr_44, i32 0, i32 1
  %156 = getelementptr [2 x i32], ptr %153, i32 0, i32 1
  %157 = load i32, ptr %156
  store i32 %157, ptr %155
  %158 = getelementptr [2 x i32], ptr %stack.ptr_45, i32 0, i32 0
  store i32 4294967295, ptr %158
  %159 = getelementptr [2 x i32], ptr %stack.ptr_45, i32 0, i32 1
  %160 = getelementptr [2 x i32], ptr %153, i32 0, i32 1
  %161 = load i32, ptr %160
  store i32 %161, ptr %159
  %162 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %162, ptr %stack.ptr_44, ptr %stack.ptr_46)
  %163 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %163, ptr %stack.ptr_45, ptr %stack.ptr_47)
  br label %loop_10
loop_10:
  %164 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_46, ptr %stack.ptr_47)
  br i1 %164, label %if_12, label %end_if_12
if_12:
  br label %range_query.end_10
end_if_12:
  %165 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_46)
  %166 = getelementptr [2 x i32], ptr %153, i32 0, i32 0
  %167 = load i32, ptr %166
  %168 = getelementptr [2 x i32], ptr %165, i32 0, i32 0
  %169 = load i32, ptr %168
  %170 = icmp ult i32 %167, %169
  br i1 %170, label %if_13, label %end_if_13
if_13:
  %171 = getelementptr [3 x i32], ptr %stack.ptr_48, i32 0, i32 0
  %172 = getelementptr [2 x i32], ptr %153, i32 0, i32 0
  %173 = load i32, ptr %172
  store i32 %173, ptr %171
  %174 = getelementptr [3 x i32], ptr %stack.ptr_48, i32 0, i32 1
  %175 = getelementptr [2 x i32], ptr %165, i32 0, i32 0
  %176 = load i32, ptr %175
  store i32 %176, ptr %174
  %177 = getelementptr [3 x i32], ptr %stack.ptr_48, i32 0, i32 2
  %178 = getelementptr [2 x i32], ptr %153, i32 0, i32 1
  %179 = load i32, ptr %178
  store i32 %179, ptr %177
  %180 = getelementptr %program, ptr %arg_0, i32 0, i32 7
  %181 = call ccc i1 @eclair_btree_insert_value_0(ptr %180, ptr %stack.ptr_48)
  br label %end_if_13
end_if_13:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_46)
  br label %loop_10
range_query.end_10:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_42)
  br label %loop_9
range_query.end_9:
  %182 = getelementptr [2 x i32], ptr %stack.ptr_49, i32 0, i32 0
  store i32 0, ptr %182
  %183 = getelementptr [2 x i32], ptr %stack.ptr_49, i32 0, i32 1
  store i32 0, ptr %183
  %184 = getelementptr [2 x i32], ptr %stack.ptr_50, i32 0, i32 0
  store i32 4294967295, ptr %184
  %185 = getelementptr [2 x i32], ptr %stack.ptr_50, i32 0, i32 1
  store i32 4294967295, ptr %185
  %186 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_lower_bound_1(ptr %186, ptr %stack.ptr_49, ptr %stack.ptr_51)
  %187 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_upper_bound_1(ptr %187, ptr %stack.ptr_50, ptr %stack.ptr_52)
  br label %loop_11
loop_11:
  %188 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_51, ptr %stack.ptr_52)
  br i1 %188, label %if_14, label %end_if_14
if_14:
  br label %range_query.end_11
end_if_14:
  %189 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_51)
  %190 = getelementptr [2 x i32], ptr %stack.ptr_53, i32 0, i32 0
  store i32 0, ptr %190
  %191 = getelementptr [2 x i32], ptr %stack.ptr_53, i32 0, i32 1
  %192 = getelementptr [2 x i32], ptr %189, i32 0, i32 1
  %193 = load i32, ptr %192
  store i32 %193, ptr %191
  %194 = getelementptr [2 x i32], ptr %stack.ptr_54, i32 0, i32 0
  store i32 4294967295, ptr %194
  %195 = getelementptr [2 x i32], ptr %stack.ptr_54, i32 0, i32 1
  %196 = getelementptr [2 x i32], ptr %189, i32 0, i32 1
  %197 = load i32, ptr %196
  store i32 %197, ptr %195
  %198 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_lower_bound_2(ptr %198, ptr %stack.ptr_53, ptr %stack.ptr_55)
  %199 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_upper_bound_2(ptr %199, ptr %stack.ptr_54, ptr %stack.ptr_56)
  br label %loop_12
loop_12:
  %200 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_55, ptr %stack.ptr_56)
  br i1 %200, label %if_15, label %end_if_15
if_15:
  br label %range_query.end_12
end_if_15:
  %201 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_55)
  %202 = getelementptr [2 x i32], ptr %189, i32 0, i32 0
  %203 = load i32, ptr %202
  %204 = getelementptr [2 x i32], ptr %201, i32 0, i32 0
  %205 = load i32, ptr %204
  %206 = icmp ult i32 %203, %205
  br i1 %206, label %if_16, label %end_if_16
if_16:
  %207 = getelementptr [3 x i32], ptr %stack.ptr_57, i32 0, i32 0
  %208 = getelementptr [2 x i32], ptr %189, i32 0, i32 0
  %209 = load i32, ptr %208
  store i32 %209, ptr %207
  %210 = getelementptr [3 x i32], ptr %stack.ptr_57, i32 0, i32 1
  %211 = getelementptr [2 x i32], ptr %201, i32 0, i32 0
  %212 = load i32, ptr %211
  store i32 %212, ptr %210
  %213 = getelementptr [3 x i32], ptr %stack.ptr_57, i32 0, i32 2
  %214 = getelementptr [2 x i32], ptr %189, i32 0, i32 1
  %215 = load i32, ptr %214
  store i32 %215, ptr %213
  %216 = getelementptr %program, ptr %arg_0, i32 0, i32 7
  %217 = call ccc i1 @eclair_btree_insert_value_0(ptr %216, ptr %stack.ptr_57)
  br label %end_if_16
end_if_16:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_55)
  br label %loop_12
range_query.end_12:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_51)
  br label %loop_11
range_query.end_11:
  %218 = getelementptr [2 x i32], ptr %stack.ptr_58, i32 0, i32 0
  store i32 0, ptr %218
  %219 = getelementptr [2 x i32], ptr %stack.ptr_58, i32 0, i32 1
  store i32 0, ptr %219
  %220 = getelementptr [2 x i32], ptr %stack.ptr_59, i32 0, i32 0
  store i32 4294967295, ptr %220
  %221 = getelementptr [2 x i32], ptr %stack.ptr_59, i32 0, i32 1
  store i32 4294967295, ptr %221
  %222 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %222, ptr %stack.ptr_58, ptr %stack.ptr_60)
  %223 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %223, ptr %stack.ptr_59, ptr %stack.ptr_61)
  br label %loop_13
loop_13:
  %224 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_60, ptr %stack.ptr_61)
  br i1 %224, label %if_17, label %end_if_17
if_17:
  br label %range_query.end_13
end_if_17:
  %225 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_60)
  %226 = getelementptr [2 x i32], ptr %stack.ptr_62, i32 0, i32 0
  %227 = getelementptr [2 x i32], ptr %225, i32 0, i32 1
  %228 = load i32, ptr %227
  store i32 %228, ptr %226
  %229 = getelementptr [2 x i32], ptr %stack.ptr_62, i32 0, i32 1
  store i32 0, ptr %229
  %230 = getelementptr [2 x i32], ptr %stack.ptr_63, i32 0, i32 0
  %231 = getelementptr [2 x i32], ptr %225, i32 0, i32 1
  %232 = load i32, ptr %231
  store i32 %232, ptr %230
  %233 = getelementptr [2 x i32], ptr %stack.ptr_63, i32 0, i32 1
  store i32 4294967295, ptr %233
  %234 = getelementptr %program, ptr %arg_0, i32 0, i32 35
  call ccc void @eclair_btree_lower_bound_1(ptr %234, ptr %stack.ptr_62, ptr %stack.ptr_64)
  %235 = getelementptr %program, ptr %arg_0, i32 0, i32 35
  call ccc void @eclair_btree_upper_bound_1(ptr %235, ptr %stack.ptr_63, ptr %stack.ptr_65)
  br label %loop_14
loop_14:
  %236 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_64, ptr %stack.ptr_65)
  br i1 %236, label %if_18, label %end_if_18
if_18:
  br label %range_query.end_14
end_if_18:
  %237 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_64)
  %238 = getelementptr [2 x i32], ptr %stack.ptr_66, i32 0, i32 0
  %239 = getelementptr [2 x i32], ptr %225, i32 0, i32 0
  %240 = load i32, ptr %239
  store i32 %240, ptr %238
  %241 = getelementptr [2 x i32], ptr %stack.ptr_66, i32 0, i32 1
  %242 = getelementptr [2 x i32], ptr %225, i32 0, i32 1
  %243 = load i32, ptr %242
  store i32 %243, ptr %241
  %244 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %245 = call ccc i1 @eclair_btree_insert_value_1(ptr %244, ptr %stack.ptr_66)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_64)
  br label %loop_14
range_query.end_14:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_60)
  br label %loop_13
range_query.end_13:
  %246 = getelementptr [2 x i32], ptr %stack.ptr_67, i32 0, i32 0
  store i32 0, ptr %246
  %247 = getelementptr [2 x i32], ptr %stack.ptr_67, i32 0, i32 1
  store i32 0, ptr %247
  %248 = getelementptr [2 x i32], ptr %stack.ptr_68, i32 0, i32 0
  store i32 4294967295, ptr %248
  %249 = getelementptr [2 x i32], ptr %stack.ptr_68, i32 0, i32 1
  store i32 4294967295, ptr %249
  %250 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %250, ptr %stack.ptr_67, ptr %stack.ptr_69)
  %251 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %251, ptr %stack.ptr_68, ptr %stack.ptr_70)
  br label %loop_15
loop_15:
  %252 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_69, ptr %stack.ptr_70)
  br i1 %252, label %if_19, label %end_if_19
if_19:
  br label %range_query.end_15
end_if_19:
  %253 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_69)
  %254 = getelementptr [2 x i32], ptr %stack.ptr_71, i32 0, i32 0
  %255 = getelementptr [2 x i32], ptr %253, i32 0, i32 1
  %256 = load i32, ptr %255
  store i32 %256, ptr %254
  %257 = getelementptr [2 x i32], ptr %stack.ptr_71, i32 0, i32 1
  store i32 0, ptr %257
  %258 = getelementptr [2 x i32], ptr %stack.ptr_72, i32 0, i32 0
  %259 = getelementptr [2 x i32], ptr %253, i32 0, i32 1
  %260 = load i32, ptr %259
  store i32 %260, ptr %258
  %261 = getelementptr [2 x i32], ptr %stack.ptr_72, i32 0, i32 1
  store i32 4294967295, ptr %261
  %262 = getelementptr %program, ptr %arg_0, i32 0, i32 34
  call ccc void @eclair_btree_lower_bound_1(ptr %262, ptr %stack.ptr_71, ptr %stack.ptr_73)
  %263 = getelementptr %program, ptr %arg_0, i32 0, i32 34
  call ccc void @eclair_btree_upper_bound_1(ptr %263, ptr %stack.ptr_72, ptr %stack.ptr_74)
  br label %loop_16
loop_16:
  %264 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_73, ptr %stack.ptr_74)
  br i1 %264, label %if_20, label %end_if_20
if_20:
  br label %range_query.end_16
end_if_20:
  %265 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_73)
  %266 = getelementptr [2 x i32], ptr %stack.ptr_75, i32 0, i32 0
  %267 = getelementptr [2 x i32], ptr %253, i32 0, i32 0
  %268 = load i32, ptr %267
  store i32 %268, ptr %266
  %269 = getelementptr [2 x i32], ptr %stack.ptr_75, i32 0, i32 1
  %270 = getelementptr [2 x i32], ptr %253, i32 0, i32 1
  %271 = load i32, ptr %270
  store i32 %271, ptr %269
  %272 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %273 = call ccc i1 @eclair_btree_insert_value_1(ptr %272, ptr %stack.ptr_75)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_73)
  br label %loop_16
range_query.end_16:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_69)
  br label %loop_15
range_query.end_15:
  %274 = getelementptr [3 x i32], ptr %stack.ptr_76, i32 0, i32 0
  store i32 0, ptr %274
  %275 = getelementptr [3 x i32], ptr %stack.ptr_76, i32 0, i32 1
  store i32 0, ptr %275
  %276 = getelementptr [3 x i32], ptr %stack.ptr_76, i32 0, i32 2
  store i32 0, ptr %276
  %277 = getelementptr [3 x i32], ptr %stack.ptr_77, i32 0, i32 0
  store i32 4294967295, ptr %277
  %278 = getelementptr [3 x i32], ptr %stack.ptr_77, i32 0, i32 1
  store i32 4294967295, ptr %278
  %279 = getelementptr [3 x i32], ptr %stack.ptr_77, i32 0, i32 2
  store i32 4294967295, ptr %279
  %280 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_lower_bound_0(ptr %280, ptr %stack.ptr_76, ptr %stack.ptr_78)
  %281 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_upper_bound_0(ptr %281, ptr %stack.ptr_77, ptr %stack.ptr_79)
  br label %loop_17
loop_17:
  %282 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_78, ptr %stack.ptr_79)
  br i1 %282, label %if_21, label %end_if_21
if_21:
  br label %range_query.end_17
end_if_21:
  %283 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_78)
  %284 = getelementptr [2 x i32], ptr %stack.ptr_80, i32 0, i32 0
  %285 = getelementptr [3 x i32], ptr %283, i32 0, i32 2
  %286 = load i32, ptr %285
  store i32 %286, ptr %284
  %287 = getelementptr [2 x i32], ptr %stack.ptr_80, i32 0, i32 1
  store i32 0, ptr %287
  %288 = getelementptr [2 x i32], ptr %stack.ptr_81, i32 0, i32 0
  %289 = getelementptr [3 x i32], ptr %283, i32 0, i32 2
  %290 = load i32, ptr %289
  store i32 %290, ptr %288
  %291 = getelementptr [2 x i32], ptr %stack.ptr_81, i32 0, i32 1
  store i32 4294967295, ptr %291
  %292 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %292, ptr %stack.ptr_80, ptr %stack.ptr_82)
  %293 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %293, ptr %stack.ptr_81, ptr %stack.ptr_83)
  br label %loop_18
loop_18:
  %294 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_82, ptr %stack.ptr_83)
  br i1 %294, label %if_22, label %end_if_22
if_22:
  br label %range_query.end_18
end_if_22:
  %295 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_82)
  %296 = getelementptr [3 x i32], ptr %stack.ptr_84, i32 0, i32 0
  %297 = getelementptr [3 x i32], ptr %283, i32 0, i32 0
  %298 = load i32, ptr %297
  store i32 %298, ptr %296
  %299 = getelementptr [3 x i32], ptr %stack.ptr_84, i32 0, i32 1
  %300 = getelementptr [3 x i32], ptr %283, i32 0, i32 2
  %301 = load i32, ptr %300
  store i32 %301, ptr %299
  %302 = getelementptr [3 x i32], ptr %stack.ptr_84, i32 0, i32 2
  %303 = getelementptr [2 x i32], ptr %295, i32 0, i32 1
  %304 = load i32, ptr %303
  store i32 %304, ptr %302
  %305 = getelementptr %program, ptr %arg_0, i32 0, i32 55
  %306 = call ccc i1 @eclair_btree_insert_value_0(ptr %305, ptr %stack.ptr_84)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_82)
  br label %loop_18
range_query.end_18:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_78)
  br label %loop_17
range_query.end_17:
  %307 = getelementptr [2 x i32], ptr %stack.ptr_85, i32 0, i32 0
  store i32 0, ptr %307
  %308 = getelementptr [2 x i32], ptr %stack.ptr_85, i32 0, i32 1
  store i32 0, ptr %308
  %309 = getelementptr [2 x i32], ptr %stack.ptr_86, i32 0, i32 0
  store i32 4294967295, ptr %309
  %310 = getelementptr [2 x i32], ptr %stack.ptr_86, i32 0, i32 1
  store i32 4294967295, ptr %310
  %311 = getelementptr %program, ptr %arg_0, i32 0, i32 35
  call ccc void @eclair_btree_lower_bound_1(ptr %311, ptr %stack.ptr_85, ptr %stack.ptr_87)
  %312 = getelementptr %program, ptr %arg_0, i32 0, i32 35
  call ccc void @eclair_btree_upper_bound_1(ptr %312, ptr %stack.ptr_86, ptr %stack.ptr_88)
  br label %loop_19
loop_19:
  %313 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_87, ptr %stack.ptr_88)
  br i1 %313, label %if_23, label %end_if_23
if_23:
  br label %range_query.end_19
end_if_23:
  %314 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_87)
  %315 = getelementptr [2 x i32], ptr %stack.ptr_89, i32 0, i32 0
  store i32 0, ptr %315
  %316 = getelementptr [2 x i32], ptr %stack.ptr_89, i32 0, i32 1
  store i32 0, ptr %316
  %317 = getelementptr [2 x i32], ptr %stack.ptr_90, i32 0, i32 0
  store i32 4294967295, ptr %317
  %318 = getelementptr [2 x i32], ptr %stack.ptr_90, i32 0, i32 1
  store i32 4294967295, ptr %318
  %319 = getelementptr %program, ptr %arg_0, i32 0, i32 35
  call ccc void @eclair_btree_lower_bound_1(ptr %319, ptr %stack.ptr_89, ptr %stack.ptr_91)
  %320 = getelementptr %program, ptr %arg_0, i32 0, i32 35
  call ccc void @eclair_btree_upper_bound_1(ptr %320, ptr %stack.ptr_90, ptr %stack.ptr_92)
  br label %loop_20
loop_20:
  %321 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_91, ptr %stack.ptr_92)
  br i1 %321, label %if_24, label %end_if_24
if_24:
  br label %range_query.end_20
end_if_24:
  %322 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_91)
  %323 = getelementptr [2 x i32], ptr %314, i32 0, i32 1
  %324 = load i32, ptr %323
  %325 = getelementptr [2 x i32], ptr %322, i32 0, i32 1
  %326 = load i32, ptr %325
  %327 = icmp ne i32 %324, %326
  br i1 %327, label %if_25, label %end_if_25
if_25:
  %328 = getelementptr [2 x i32], ptr %stack.ptr_93, i32 0, i32 0
  %329 = getelementptr [2 x i32], ptr %314, i32 0, i32 0
  %330 = load i32, ptr %329
  store i32 %330, ptr %328
  %331 = getelementptr [2 x i32], ptr %stack.ptr_93, i32 0, i32 1
  %332 = getelementptr [2 x i32], ptr %322, i32 0, i32 0
  %333 = load i32, ptr %332
  store i32 %333, ptr %331
  %334 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  %335 = call ccc i1 @eclair_btree_insert_value_1(ptr %334, ptr %stack.ptr_93)
  br label %end_if_25
end_if_25:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_91)
  br label %loop_20
range_query.end_20:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_87)
  br label %loop_19
range_query.end_19:
  %336 = getelementptr [2 x i32], ptr %stack.ptr_94, i32 0, i32 0
  store i32 0, ptr %336
  %337 = getelementptr [2 x i32], ptr %stack.ptr_94, i32 0, i32 1
  store i32 0, ptr %337
  %338 = getelementptr [2 x i32], ptr %stack.ptr_95, i32 0, i32 0
  store i32 4294967295, ptr %338
  %339 = getelementptr [2 x i32], ptr %stack.ptr_95, i32 0, i32 1
  store i32 4294967295, ptr %339
  %340 = getelementptr %program, ptr %arg_0, i32 0, i32 34
  call ccc void @eclair_btree_lower_bound_1(ptr %340, ptr %stack.ptr_94, ptr %stack.ptr_96)
  %341 = getelementptr %program, ptr %arg_0, i32 0, i32 34
  call ccc void @eclair_btree_upper_bound_1(ptr %341, ptr %stack.ptr_95, ptr %stack.ptr_97)
  br label %loop_21
loop_21:
  %342 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_96, ptr %stack.ptr_97)
  br i1 %342, label %if_26, label %end_if_26
if_26:
  br label %range_query.end_21
end_if_26:
  %343 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_96)
  %344 = getelementptr [2 x i32], ptr %stack.ptr_98, i32 0, i32 0
  store i32 0, ptr %344
  %345 = getelementptr [2 x i32], ptr %stack.ptr_98, i32 0, i32 1
  store i32 0, ptr %345
  %346 = getelementptr [2 x i32], ptr %stack.ptr_99, i32 0, i32 0
  store i32 4294967295, ptr %346
  %347 = getelementptr [2 x i32], ptr %stack.ptr_99, i32 0, i32 1
  store i32 4294967295, ptr %347
  %348 = getelementptr %program, ptr %arg_0, i32 0, i32 34
  call ccc void @eclair_btree_lower_bound_1(ptr %348, ptr %stack.ptr_98, ptr %stack.ptr_100)
  %349 = getelementptr %program, ptr %arg_0, i32 0, i32 34
  call ccc void @eclair_btree_upper_bound_1(ptr %349, ptr %stack.ptr_99, ptr %stack.ptr_101)
  br label %loop_22
loop_22:
  %350 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_100, ptr %stack.ptr_101)
  br i1 %350, label %if_27, label %end_if_27
if_27:
  br label %range_query.end_22
end_if_27:
  %351 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_100)
  %352 = getelementptr [2 x i32], ptr %343, i32 0, i32 1
  %353 = load i32, ptr %352
  %354 = getelementptr [2 x i32], ptr %351, i32 0, i32 1
  %355 = load i32, ptr %354
  %356 = icmp ne i32 %353, %355
  br i1 %356, label %if_28, label %end_if_28
if_28:
  %357 = getelementptr [2 x i32], ptr %stack.ptr_102, i32 0, i32 0
  %358 = getelementptr [2 x i32], ptr %343, i32 0, i32 0
  %359 = load i32, ptr %358
  store i32 %359, ptr %357
  %360 = getelementptr [2 x i32], ptr %stack.ptr_102, i32 0, i32 1
  %361 = getelementptr [2 x i32], ptr %351, i32 0, i32 0
  %362 = load i32, ptr %361
  store i32 %362, ptr %360
  %363 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  %364 = call ccc i1 @eclair_btree_insert_value_1(ptr %363, ptr %stack.ptr_102)
  br label %end_if_28
end_if_28:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_100)
  br label %loop_22
range_query.end_22:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_96)
  br label %loop_21
range_query.end_21:
  %365 = getelementptr [2 x i32], ptr %stack.ptr_103, i32 0, i32 0
  store i32 0, ptr %365
  %366 = getelementptr [2 x i32], ptr %stack.ptr_103, i32 0, i32 1
  store i32 0, ptr %366
  %367 = getelementptr [2 x i32], ptr %stack.ptr_104, i32 0, i32 0
  store i32 4294967295, ptr %367
  %368 = getelementptr [2 x i32], ptr %stack.ptr_104, i32 0, i32 1
  store i32 4294967295, ptr %368
  %369 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %369, ptr %stack.ptr_103, ptr %stack.ptr_105)
  %370 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %370, ptr %stack.ptr_104, ptr %stack.ptr_106)
  br label %loop_23
loop_23:
  %371 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_105, ptr %stack.ptr_106)
  br i1 %371, label %if_29, label %end_if_29
if_29:
  br label %range_query.end_23
end_if_29:
  %372 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_105)
  %373 = getelementptr [2 x i32], ptr %stack.ptr_107, i32 0, i32 0
  %374 = getelementptr [2 x i32], ptr %372, i32 0, i32 1
  %375 = load i32, ptr %374
  store i32 %375, ptr %373
  %376 = getelementptr [2 x i32], ptr %stack.ptr_107, i32 0, i32 1
  store i32 0, ptr %376
  %377 = getelementptr [2 x i32], ptr %stack.ptr_108, i32 0, i32 0
  %378 = getelementptr [2 x i32], ptr %372, i32 0, i32 1
  %379 = load i32, ptr %378
  store i32 %379, ptr %377
  %380 = getelementptr [2 x i32], ptr %stack.ptr_108, i32 0, i32 1
  store i32 4294967295, ptr %380
  %381 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %381, ptr %stack.ptr_107, ptr %stack.ptr_109)
  %382 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %382, ptr %stack.ptr_108, ptr %stack.ptr_110)
  br label %loop_24
loop_24:
  %383 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_109, ptr %stack.ptr_110)
  br i1 %383, label %if_30, label %end_if_30
if_30:
  br label %range_query.end_24
end_if_30:
  %384 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_109)
  %385 = getelementptr [2 x i32], ptr %stack.ptr_111, i32 0, i32 0
  %386 = getelementptr [2 x i32], ptr %372, i32 0, i32 0
  %387 = load i32, ptr %386
  store i32 %387, ptr %385
  %388 = getelementptr [2 x i32], ptr %stack.ptr_111, i32 0, i32 1
  store i32 0, ptr %388
  %389 = getelementptr [2 x i32], ptr %stack.ptr_112, i32 0, i32 0
  %390 = getelementptr [2 x i32], ptr %372, i32 0, i32 0
  %391 = load i32, ptr %390
  store i32 %391, ptr %389
  %392 = getelementptr [2 x i32], ptr %stack.ptr_112, i32 0, i32 1
  store i32 4294967295, ptr %392
  %393 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %393, ptr %stack.ptr_111, ptr %stack.ptr_113)
  %394 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %394, ptr %stack.ptr_112, ptr %stack.ptr_114)
  br label %loop_25
loop_25:
  %395 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_113, ptr %stack.ptr_114)
  br i1 %395, label %if_31, label %end_if_31
if_31:
  br label %range_query.end_25
end_if_31:
  %396 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_113)
  %397 = getelementptr [2 x i32], ptr %372, i32 0, i32 1
  %398 = load i32, ptr %397
  %399 = getelementptr [2 x i32], ptr %396, i32 0, i32 1
  %400 = load i32, ptr %399
  %401 = icmp ne i32 %398, %400
  br i1 %401, label %if_32, label %end_if_33
if_32:
  %402 = getelementptr [2 x i32], ptr %stack.ptr_115, i32 0, i32 0
  %403 = getelementptr [2 x i32], ptr %396, i32 0, i32 1
  %404 = load i32, ptr %403
  store i32 %404, ptr %402
  %405 = getelementptr [2 x i32], ptr %stack.ptr_115, i32 0, i32 1
  %406 = getelementptr [2 x i32], ptr %384, i32 0, i32 1
  %407 = load i32, ptr %406
  store i32 %407, ptr %405
  %408 = getelementptr [2 x i32], ptr %stack.ptr_116, i32 0, i32 0
  %409 = getelementptr [2 x i32], ptr %396, i32 0, i32 1
  %410 = load i32, ptr %409
  store i32 %410, ptr %408
  %411 = getelementptr [2 x i32], ptr %stack.ptr_116, i32 0, i32 1
  %412 = getelementptr [2 x i32], ptr %384, i32 0, i32 1
  %413 = load i32, ptr %412
  store i32 %413, ptr %411
  %414 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %414, ptr %stack.ptr_115, ptr %stack.ptr_117)
  %415 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %415, ptr %stack.ptr_116, ptr %stack.ptr_118)
  br label %loop_26
loop_26:
  %416 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_117, ptr %stack.ptr_118)
  br i1 %416, label %if_33, label %end_if_32
if_33:
  br label %range_query.end_26
end_if_32:
  %417 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_117)
  %418 = getelementptr [3 x i32], ptr %stack.ptr_119, i32 0, i32 0
  %419 = getelementptr [2 x i32], ptr %372, i32 0, i32 0
  %420 = load i32, ptr %419
  store i32 %420, ptr %418
  %421 = getelementptr [3 x i32], ptr %stack.ptr_119, i32 0, i32 1
  %422 = getelementptr [2 x i32], ptr %372, i32 0, i32 1
  %423 = load i32, ptr %422
  store i32 %423, ptr %421
  %424 = getelementptr [3 x i32], ptr %stack.ptr_119, i32 0, i32 2
  %425 = getelementptr [2 x i32], ptr %396, i32 0, i32 1
  %426 = load i32, ptr %425
  store i32 %426, ptr %424
  %427 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  %428 = call ccc i1 @eclair_btree_insert_value_0(ptr %427, ptr %stack.ptr_119)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_117)
  br label %loop_26
range_query.end_26:
  br label %end_if_33
end_if_33:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_113)
  br label %loop_25
range_query.end_25:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_109)
  br label %loop_24
range_query.end_24:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_105)
  br label %loop_23
range_query.end_23:
  %429 = getelementptr [4 x i32], ptr %stack.ptr_120, i32 0, i32 0
  store i32 0, ptr %429
  %430 = getelementptr [4 x i32], ptr %stack.ptr_120, i32 0, i32 1
  store i32 60, ptr %430
  %431 = getelementptr [4 x i32], ptr %stack.ptr_120, i32 0, i32 2
  store i32 0, ptr %431
  %432 = getelementptr [4 x i32], ptr %stack.ptr_120, i32 0, i32 3
  store i32 0, ptr %432
  %433 = getelementptr [4 x i32], ptr %stack.ptr_121, i32 0, i32 0
  store i32 4294967295, ptr %433
  %434 = getelementptr [4 x i32], ptr %stack.ptr_121, i32 0, i32 1
  store i32 60, ptr %434
  %435 = getelementptr [4 x i32], ptr %stack.ptr_121, i32 0, i32 2
  store i32 4294967295, ptr %435
  %436 = getelementptr [4 x i32], ptr %stack.ptr_121, i32 0, i32 3
  store i32 4294967295, ptr %436
  %437 = getelementptr %program, ptr %arg_0, i32 0, i32 10
  call ccc void @eclair_btree_lower_bound_5(ptr %437, ptr %stack.ptr_120, ptr %stack.ptr_122)
  %438 = getelementptr %program, ptr %arg_0, i32 0, i32 10
  call ccc void @eclair_btree_upper_bound_5(ptr %438, ptr %stack.ptr_121, ptr %stack.ptr_123)
  br label %loop_27
loop_27:
  %439 = call ccc i1 @eclair_btree_iterator_is_equal_5(ptr %stack.ptr_122, ptr %stack.ptr_123)
  br i1 %439, label %if_34, label %end_if_34
if_34:
  br label %range_query.end_27
end_if_34:
  %440 = call ccc ptr @eclair_btree_iterator_current_5(ptr %stack.ptr_122)
  %441 = getelementptr [3 x i32], ptr %stack.ptr_124, i32 0, i32 0
  %442 = getelementptr [4 x i32], ptr %440, i32 0, i32 0
  %443 = load i32, ptr %442
  store i32 %443, ptr %441
  %444 = getelementptr [3 x i32], ptr %stack.ptr_124, i32 0, i32 1
  %445 = getelementptr [4 x i32], ptr %440, i32 0, i32 2
  %446 = load i32, ptr %445
  store i32 %446, ptr %444
  %447 = getelementptr [3 x i32], ptr %stack.ptr_124, i32 0, i32 2
  %448 = getelementptr [4 x i32], ptr %440, i32 0, i32 3
  %449 = load i32, ptr %448
  store i32 %449, ptr %447
  %450 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  %451 = call ccc i1 @eclair_btree_insert_value_0(ptr %450, ptr %stack.ptr_124)
  call ccc void @eclair_btree_iterator_next_5(ptr %stack.ptr_122)
  br label %loop_27
range_query.end_27:
  %452 = getelementptr [3 x i32], ptr %stack.ptr_125, i32 0, i32 0
  store i32 0, ptr %452
  %453 = getelementptr [3 x i32], ptr %stack.ptr_125, i32 0, i32 1
  store i32 0, ptr %453
  %454 = getelementptr [3 x i32], ptr %stack.ptr_125, i32 0, i32 2
  store i32 0, ptr %454
  %455 = getelementptr [3 x i32], ptr %stack.ptr_126, i32 0, i32 0
  store i32 4294967295, ptr %455
  %456 = getelementptr [3 x i32], ptr %stack.ptr_126, i32 0, i32 1
  store i32 4294967295, ptr %456
  %457 = getelementptr [3 x i32], ptr %stack.ptr_126, i32 0, i32 2
  store i32 4294967295, ptr %457
  %458 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %458, ptr %stack.ptr_125, ptr %stack.ptr_127)
  %459 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %459, ptr %stack.ptr_126, ptr %stack.ptr_128)
  br label %loop_28
loop_28:
  %460 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_127, ptr %stack.ptr_128)
  br i1 %460, label %if_35, label %end_if_35
if_35:
  br label %range_query.end_28
end_if_35:
  %461 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_127)
  %462 = getelementptr [3 x i32], ptr %stack.ptr_129, i32 0, i32 0
  %463 = getelementptr [3 x i32], ptr %461, i32 0, i32 2
  %464 = load i32, ptr %463
  store i32 %464, ptr %462
  %465 = getelementptr [3 x i32], ptr %stack.ptr_129, i32 0, i32 1
  store i32 0, ptr %465
  %466 = getelementptr [3 x i32], ptr %stack.ptr_129, i32 0, i32 2
  store i32 0, ptr %466
  %467 = getelementptr [3 x i32], ptr %stack.ptr_130, i32 0, i32 0
  %468 = getelementptr [3 x i32], ptr %461, i32 0, i32 2
  %469 = load i32, ptr %468
  store i32 %469, ptr %467
  %470 = getelementptr [3 x i32], ptr %stack.ptr_130, i32 0, i32 1
  store i32 4294967295, ptr %470
  %471 = getelementptr [3 x i32], ptr %stack.ptr_130, i32 0, i32 2
  store i32 4294967295, ptr %471
  %472 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %472, ptr %stack.ptr_129, ptr %stack.ptr_131)
  %473 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %473, ptr %stack.ptr_130, ptr %stack.ptr_132)
  br label %loop_29
loop_29:
  %474 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_131, ptr %stack.ptr_132)
  br i1 %474, label %if_36, label %end_if_36
if_36:
  br label %range_query.end_29
end_if_36:
  %475 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_131)
  %476 = getelementptr [3 x i32], ptr %stack.ptr_133, i32 0, i32 0
  %477 = getelementptr [3 x i32], ptr %461, i32 0, i32 0
  %478 = load i32, ptr %477
  store i32 %478, ptr %476
  %479 = getelementptr [3 x i32], ptr %stack.ptr_133, i32 0, i32 1
  %480 = getelementptr [3 x i32], ptr %475, i32 0, i32 1
  %481 = load i32, ptr %480
  store i32 %481, ptr %479
  %482 = getelementptr [3 x i32], ptr %stack.ptr_133, i32 0, i32 2
  %483 = getelementptr [3 x i32], ptr %475, i32 0, i32 2
  %484 = load i32, ptr %483
  store i32 %484, ptr %482
  %485 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  %486 = call ccc i1 @eclair_btree_insert_value_0(ptr %485, ptr %stack.ptr_133)
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_131)
  br label %loop_29
range_query.end_29:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_127)
  br label %loop_28
range_query.end_28:
  %487 = getelementptr [3 x i32], ptr %stack.ptr_134, i32 0, i32 0
  store i32 0, ptr %487
  %488 = getelementptr [3 x i32], ptr %stack.ptr_134, i32 0, i32 1
  store i32 0, ptr %488
  %489 = getelementptr [3 x i32], ptr %stack.ptr_134, i32 0, i32 2
  store i32 0, ptr %489
  %490 = getelementptr [3 x i32], ptr %stack.ptr_135, i32 0, i32 0
  store i32 4294967295, ptr %490
  %491 = getelementptr [3 x i32], ptr %stack.ptr_135, i32 0, i32 1
  store i32 4294967295, ptr %491
  %492 = getelementptr [3 x i32], ptr %stack.ptr_135, i32 0, i32 2
  store i32 4294967295, ptr %492
  %493 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %493, ptr %stack.ptr_134, ptr %stack.ptr_136)
  %494 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %494, ptr %stack.ptr_135, ptr %stack.ptr_137)
  br label %loop_30
loop_30:
  %495 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_136, ptr %stack.ptr_137)
  br i1 %495, label %if_37, label %end_if_37
if_37:
  br label %range_query.end_30
end_if_37:
  %496 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_136)
  %497 = getelementptr [3 x i32], ptr %stack.ptr_138, i32 0, i32 0
  %498 = getelementptr [3 x i32], ptr %496, i32 0, i32 2
  %499 = load i32, ptr %498
  store i32 %499, ptr %497
  %500 = getelementptr [3 x i32], ptr %stack.ptr_138, i32 0, i32 1
  store i32 0, ptr %500
  %501 = getelementptr [3 x i32], ptr %stack.ptr_138, i32 0, i32 2
  store i32 0, ptr %501
  %502 = getelementptr [3 x i32], ptr %stack.ptr_139, i32 0, i32 0
  %503 = getelementptr [3 x i32], ptr %496, i32 0, i32 2
  %504 = load i32, ptr %503
  store i32 %504, ptr %502
  %505 = getelementptr [3 x i32], ptr %stack.ptr_139, i32 0, i32 1
  store i32 4294967295, ptr %505
  %506 = getelementptr [3 x i32], ptr %stack.ptr_139, i32 0, i32 2
  store i32 4294967295, ptr %506
  %507 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %507, ptr %stack.ptr_138, ptr %stack.ptr_140)
  %508 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %508, ptr %stack.ptr_139, ptr %stack.ptr_141)
  br label %loop_31
loop_31:
  %509 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_140, ptr %stack.ptr_141)
  br i1 %509, label %if_38, label %end_if_38
if_38:
  br label %range_query.end_31
end_if_38:
  %510 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_140)
  %511 = getelementptr [3 x i32], ptr %stack.ptr_142, i32 0, i32 0
  %512 = getelementptr [3 x i32], ptr %496, i32 0, i32 0
  %513 = load i32, ptr %512
  store i32 %513, ptr %511
  %514 = getelementptr [3 x i32], ptr %stack.ptr_142, i32 0, i32 1
  %515 = getelementptr [3 x i32], ptr %510, i32 0, i32 2
  %516 = load i32, ptr %515
  store i32 %516, ptr %514
  %517 = getelementptr [3 x i32], ptr %stack.ptr_142, i32 0, i32 2
  %518 = getelementptr [3 x i32], ptr %510, i32 0, i32 1
  %519 = load i32, ptr %518
  store i32 %519, ptr %517
  %520 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  %521 = call ccc i1 @eclair_btree_insert_value_0(ptr %520, ptr %stack.ptr_142)
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_140)
  br label %loop_31
range_query.end_31:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_136)
  br label %loop_30
range_query.end_30:
  %522 = getelementptr [3 x i32], ptr %stack.ptr_143, i32 0, i32 0
  store i32 0, ptr %522
  %523 = getelementptr [3 x i32], ptr %stack.ptr_143, i32 0, i32 1
  store i32 0, ptr %523
  %524 = getelementptr [3 x i32], ptr %stack.ptr_143, i32 0, i32 2
  store i32 0, ptr %524
  %525 = getelementptr [3 x i32], ptr %stack.ptr_144, i32 0, i32 0
  store i32 4294967295, ptr %525
  %526 = getelementptr [3 x i32], ptr %stack.ptr_144, i32 0, i32 1
  store i32 4294967295, ptr %526
  %527 = getelementptr [3 x i32], ptr %stack.ptr_144, i32 0, i32 2
  store i32 4294967295, ptr %527
  %528 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_lower_bound_0(ptr %528, ptr %stack.ptr_143, ptr %stack.ptr_145)
  %529 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_upper_bound_0(ptr %529, ptr %stack.ptr_144, ptr %stack.ptr_146)
  br label %loop_32
loop_32:
  %530 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_145, ptr %stack.ptr_146)
  br i1 %530, label %if_39, label %end_if_39
if_39:
  br label %range_query.end_32
end_if_39:
  %531 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_145)
  %532 = getelementptr [2 x i32], ptr %stack.ptr_147, i32 0, i32 0
  %533 = getelementptr [3 x i32], ptr %531, i32 0, i32 1
  %534 = load i32, ptr %533
  store i32 %534, ptr %532
  %535 = getelementptr [2 x i32], ptr %stack.ptr_147, i32 0, i32 1
  store i32 0, ptr %535
  %536 = getelementptr [2 x i32], ptr %stack.ptr_148, i32 0, i32 0
  %537 = getelementptr [3 x i32], ptr %531, i32 0, i32 1
  %538 = load i32, ptr %537
  store i32 %538, ptr %536
  %539 = getelementptr [2 x i32], ptr %stack.ptr_148, i32 0, i32 1
  store i32 4294967295, ptr %539
  %540 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %540, ptr %stack.ptr_147, ptr %stack.ptr_149)
  %541 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %541, ptr %stack.ptr_148, ptr %stack.ptr_150)
  br label %loop_33
loop_33:
  %542 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_149, ptr %stack.ptr_150)
  br i1 %542, label %if_40, label %end_if_40
if_40:
  br label %range_query.end_33
end_if_40:
  %543 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_149)
  %544 = getelementptr [3 x i32], ptr %stack.ptr_151, i32 0, i32 0
  %545 = getelementptr [3 x i32], ptr %531, i32 0, i32 0
  %546 = load i32, ptr %545
  store i32 %546, ptr %544
  %547 = getelementptr [3 x i32], ptr %stack.ptr_151, i32 0, i32 1
  %548 = getelementptr [3 x i32], ptr %531, i32 0, i32 1
  %549 = load i32, ptr %548
  store i32 %549, ptr %547
  %550 = getelementptr [3 x i32], ptr %stack.ptr_151, i32 0, i32 2
  %551 = getelementptr [3 x i32], ptr %531, i32 0, i32 2
  %552 = load i32, ptr %551
  store i32 %552, ptr %550
  %553 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  %554 = call ccc i1 @eclair_btree_insert_value_0(ptr %553, ptr %stack.ptr_151)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_149)
  br label %loop_33
range_query.end_33:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_145)
  br label %loop_32
range_query.end_32:
  %555 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_begin_0(ptr %555, ptr %stack.ptr_152)
  %556 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_end_0(ptr %556, ptr %stack.ptr_153)
  %557 = getelementptr %program, ptr %arg_0, i32 0, i32 19
  call ccc void @eclair_btree_insert_range_delta_points_to_points_to(ptr %557, ptr %stack.ptr_152, ptr %stack.ptr_153)
  br label %loop_34
loop_34:
  %558 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  call ccc void @eclair_btree_clear_0(ptr %558)
  %559 = getelementptr [3 x i32], ptr %stack.ptr_154, i32 0, i32 0
  store i32 0, ptr %559
  %560 = getelementptr [3 x i32], ptr %stack.ptr_154, i32 0, i32 1
  store i32 0, ptr %560
  %561 = getelementptr [3 x i32], ptr %stack.ptr_154, i32 0, i32 2
  store i32 0, ptr %561
  %562 = getelementptr [3 x i32], ptr %stack.ptr_155, i32 0, i32 0
  store i32 4294967295, ptr %562
  %563 = getelementptr [3 x i32], ptr %stack.ptr_155, i32 0, i32 1
  store i32 4294967295, ptr %563
  %564 = getelementptr [3 x i32], ptr %stack.ptr_155, i32 0, i32 2
  store i32 4294967295, ptr %564
  %565 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_lower_bound_0(ptr %565, ptr %stack.ptr_154, ptr %stack.ptr_156)
  %566 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_upper_bound_0(ptr %566, ptr %stack.ptr_155, ptr %stack.ptr_157)
  br label %loop_35
loop_35:
  %567 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_156, ptr %stack.ptr_157)
  br i1 %567, label %if_41, label %end_if_41
if_41:
  br label %range_query.end_34
end_if_41:
  %568 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_156)
  %569 = getelementptr [3 x i32], ptr %stack.ptr_158, i32 0, i32 0
  %570 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %571 = load i32, ptr %570
  store i32 %571, ptr %569
  %572 = getelementptr [3 x i32], ptr %stack.ptr_158, i32 0, i32 1
  %573 = getelementptr [3 x i32], ptr %568, i32 0, i32 1
  %574 = load i32, ptr %573
  store i32 %574, ptr %572
  %575 = getelementptr [3 x i32], ptr %stack.ptr_158, i32 0, i32 2
  %576 = getelementptr [3 x i32], ptr %568, i32 0, i32 2
  %577 = load i32, ptr %576
  store i32 %577, ptr %575
  %578 = getelementptr %program, ptr %arg_0, i32 0, i32 19
  %579 = call ccc i1 @eclair_btree_contains_0(ptr %578, ptr %stack.ptr_158)
  %580 = select i1 %579, i1 0, i1 1
  br i1 %580, label %if_42, label %end_if_46
if_42:
  %581 = getelementptr [2 x i32], ptr %stack.ptr_159, i32 0, i32 0
  %582 = getelementptr [3 x i32], ptr %568, i32 0, i32 2
  %583 = load i32, ptr %582
  store i32 %583, ptr %581
  %584 = getelementptr [2 x i32], ptr %stack.ptr_159, i32 0, i32 1
  store i32 0, ptr %584
  %585 = getelementptr [2 x i32], ptr %stack.ptr_160, i32 0, i32 0
  %586 = getelementptr [3 x i32], ptr %568, i32 0, i32 2
  %587 = load i32, ptr %586
  store i32 %587, ptr %585
  %588 = getelementptr [2 x i32], ptr %stack.ptr_160, i32 0, i32 1
  store i32 4294967295, ptr %588
  %589 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %589, ptr %stack.ptr_159, ptr %stack.ptr_161)
  %590 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %590, ptr %stack.ptr_160, ptr %stack.ptr_162)
  br label %loop_36
loop_36:
  %591 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_161, ptr %stack.ptr_162)
  br i1 %591, label %if_43, label %end_if_42
if_43:
  br label %range_query.end_35
end_if_42:
  %592 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_161)
  %593 = getelementptr [2 x i32], ptr %stack.ptr_163, i32 0, i32 0
  store i32 0, ptr %593
  %594 = getelementptr [2 x i32], ptr %stack.ptr_163, i32 0, i32 1
  %595 = getelementptr [2 x i32], ptr %592, i32 0, i32 1
  %596 = load i32, ptr %595
  store i32 %596, ptr %594
  %597 = getelementptr [2 x i32], ptr %stack.ptr_164, i32 0, i32 0
  store i32 4294967295, ptr %597
  %598 = getelementptr [2 x i32], ptr %stack.ptr_164, i32 0, i32 1
  %599 = getelementptr [2 x i32], ptr %592, i32 0, i32 1
  %600 = load i32, ptr %599
  store i32 %600, ptr %598
  %601 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_lower_bound_2(ptr %601, ptr %stack.ptr_163, ptr %stack.ptr_165)
  %602 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_upper_bound_2(ptr %602, ptr %stack.ptr_164, ptr %stack.ptr_166)
  br label %loop_37
loop_37:
  %603 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_165, ptr %stack.ptr_166)
  br i1 %603, label %if_44, label %end_if_43
if_44:
  br label %range_query.end_36
end_if_43:
  %604 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_165)
  %605 = getelementptr [3 x i32], ptr %stack.ptr_167, i32 0, i32 0
  %606 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %607 = load i32, ptr %606
  store i32 %607, ptr %605
  %608 = getelementptr [3 x i32], ptr %stack.ptr_167, i32 0, i32 1
  %609 = getelementptr [2 x i32], ptr %604, i32 0, i32 0
  %610 = load i32, ptr %609
  store i32 %610, ptr %608
  %611 = getelementptr [3 x i32], ptr %stack.ptr_167, i32 0, i32 2
  store i32 0, ptr %611
  %612 = getelementptr [3 x i32], ptr %stack.ptr_168, i32 0, i32 0
  %613 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %614 = load i32, ptr %613
  store i32 %614, ptr %612
  %615 = getelementptr [3 x i32], ptr %stack.ptr_168, i32 0, i32 1
  %616 = getelementptr [2 x i32], ptr %604, i32 0, i32 0
  %617 = load i32, ptr %616
  store i32 %617, ptr %615
  %618 = getelementptr [3 x i32], ptr %stack.ptr_168, i32 0, i32 2
  store i32 4294967295, ptr %618
  %619 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_lower_bound_0(ptr %619, ptr %stack.ptr_167, ptr %stack.ptr_169)
  %620 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_upper_bound_0(ptr %620, ptr %stack.ptr_168, ptr %stack.ptr_170)
  br label %loop_38
loop_38:
  %621 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_169, ptr %stack.ptr_170)
  br i1 %621, label %if_45, label %end_if_44
if_45:
  br label %range_query.end_37
end_if_44:
  %622 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_169)
  %623 = getelementptr [3 x i32], ptr %stack.ptr_171, i32 0, i32 0
  %624 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %625 = load i32, ptr %624
  store i32 %625, ptr %623
  %626 = getelementptr [3 x i32], ptr %stack.ptr_171, i32 0, i32 1
  %627 = getelementptr [3 x i32], ptr %568, i32 0, i32 1
  %628 = load i32, ptr %627
  store i32 %628, ptr %626
  %629 = getelementptr [3 x i32], ptr %stack.ptr_171, i32 0, i32 2
  %630 = getelementptr [3 x i32], ptr %622, i32 0, i32 2
  %631 = load i32, ptr %630
  store i32 %631, ptr %629
  %632 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  %633 = call ccc i1 @eclair_btree_contains_0(ptr %632, ptr %stack.ptr_171)
  %634 = select i1 %633, i1 0, i1 1
  br i1 %634, label %if_46, label %end_if_45
if_46:
  %635 = getelementptr [3 x i32], ptr %stack.ptr_172, i32 0, i32 0
  %636 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %637 = load i32, ptr %636
  store i32 %637, ptr %635
  %638 = getelementptr [3 x i32], ptr %stack.ptr_172, i32 0, i32 1
  %639 = getelementptr [3 x i32], ptr %568, i32 0, i32 1
  %640 = load i32, ptr %639
  store i32 %640, ptr %638
  %641 = getelementptr [3 x i32], ptr %stack.ptr_172, i32 0, i32 2
  %642 = getelementptr [3 x i32], ptr %622, i32 0, i32 2
  %643 = load i32, ptr %642
  store i32 %643, ptr %641
  %644 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  %645 = call ccc i1 @eclair_btree_insert_value_0(ptr %644, ptr %stack.ptr_172)
  br label %end_if_45
end_if_45:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_169)
  br label %loop_38
range_query.end_37:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_165)
  br label %loop_37
range_query.end_36:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_161)
  br label %loop_36
range_query.end_35:
  br label %end_if_46
end_if_46:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_156)
  br label %loop_35
range_query.end_34:
  %646 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  %647 = call ccc i1 @eclair_btree_is_empty_0(ptr %646)
  br i1 %647, label %if_47, label %end_if_47
if_47:
  br label %loop.end
end_if_47:
  %648 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  call ccc void @eclair_btree_begin_0(ptr %648, ptr %stack.ptr_173)
  %649 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  call ccc void @eclair_btree_end_0(ptr %649, ptr %stack.ptr_174)
  %650 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_insert_range_points_to_new_points_to(ptr %650, ptr %stack.ptr_173, ptr %stack.ptr_174)
  %651 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  %652 = getelementptr %program, ptr %arg_0, i32 0, i32 19
  call ccc void @eclair_btree_swap_0(ptr %651, ptr %652)
  br label %loop_34
loop.end:
  %653 = getelementptr [3 x i32], ptr %stack.ptr_175, i32 0, i32 0
  store i32 0, ptr %653
  %654 = getelementptr [3 x i32], ptr %stack.ptr_175, i32 0, i32 1
  store i32 0, ptr %654
  %655 = getelementptr [3 x i32], ptr %stack.ptr_175, i32 0, i32 2
  store i32 0, ptr %655
  %656 = getelementptr [3 x i32], ptr %stack.ptr_176, i32 0, i32 0
  store i32 4294967295, ptr %656
  %657 = getelementptr [3 x i32], ptr %stack.ptr_176, i32 0, i32 1
  store i32 4294967295, ptr %657
  %658 = getelementptr [3 x i32], ptr %stack.ptr_176, i32 0, i32 2
  store i32 4294967295, ptr %658
  %659 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_lower_bound_0(ptr %659, ptr %stack.ptr_175, ptr %stack.ptr_177)
  %660 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_upper_bound_0(ptr %660, ptr %stack.ptr_176, ptr %stack.ptr_178)
  br label %loop_39
loop_39:
  %661 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_177, ptr %stack.ptr_178)
  br i1 %661, label %if_48, label %end_if_48
if_48:
  br label %range_query.end_38
end_if_48:
  %662 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_177)
  %663 = getelementptr [3 x i32], ptr %stack.ptr_179, i32 0, i32 0
  %664 = getelementptr [3 x i32], ptr %662, i32 0, i32 0
  %665 = load i32, ptr %664
  store i32 %665, ptr %663
  %666 = getelementptr [3 x i32], ptr %stack.ptr_179, i32 0, i32 1
  %667 = getelementptr [3 x i32], ptr %662, i32 0, i32 1
  %668 = load i32, ptr %667
  store i32 %668, ptr %666
  %669 = getelementptr [3 x i32], ptr %stack.ptr_179, i32 0, i32 2
  store i32 0, ptr %669
  %670 = getelementptr [3 x i32], ptr %stack.ptr_180, i32 0, i32 0
  %671 = getelementptr [3 x i32], ptr %662, i32 0, i32 0
  %672 = load i32, ptr %671
  store i32 %672, ptr %670
  %673 = getelementptr [3 x i32], ptr %stack.ptr_180, i32 0, i32 1
  %674 = getelementptr [3 x i32], ptr %662, i32 0, i32 1
  %675 = load i32, ptr %674
  store i32 %675, ptr %673
  %676 = getelementptr [3 x i32], ptr %stack.ptr_180, i32 0, i32 2
  store i32 4294967295, ptr %676
  %677 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_lower_bound_0(ptr %677, ptr %stack.ptr_179, ptr %stack.ptr_181)
  %678 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_upper_bound_0(ptr %678, ptr %stack.ptr_180, ptr %stack.ptr_182)
  br label %loop_40
loop_40:
  %679 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_181, ptr %stack.ptr_182)
  br i1 %679, label %if_49, label %end_if_49
if_49:
  br label %range_query.end_39
end_if_49:
  %680 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_181)
  %681 = getelementptr [2 x i32], ptr %stack.ptr_183, i32 0, i32 0
  %682 = getelementptr [3 x i32], ptr %662, i32 0, i32 2
  %683 = load i32, ptr %682
  store i32 %683, ptr %681
  %684 = getelementptr [2 x i32], ptr %stack.ptr_183, i32 0, i32 1
  %685 = getelementptr [3 x i32], ptr %680, i32 0, i32 2
  %686 = load i32, ptr %685
  store i32 %686, ptr %684
  %687 = getelementptr [2 x i32], ptr %stack.ptr_184, i32 0, i32 0
  %688 = getelementptr [3 x i32], ptr %662, i32 0, i32 2
  %689 = load i32, ptr %688
  store i32 %689, ptr %687
  %690 = getelementptr [2 x i32], ptr %stack.ptr_184, i32 0, i32 1
  %691 = getelementptr [3 x i32], ptr %680, i32 0, i32 2
  %692 = load i32, ptr %691
  store i32 %692, ptr %690
  %693 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_lower_bound_1(ptr %693, ptr %stack.ptr_183, ptr %stack.ptr_185)
  %694 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_upper_bound_1(ptr %694, ptr %stack.ptr_184, ptr %stack.ptr_186)
  br label %loop_41
loop_41:
  %695 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_185, ptr %stack.ptr_186)
  br i1 %695, label %if_50, label %end_if_50
if_50:
  br label %range_query.end_40
end_if_50:
  %696 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_185)
  %697 = getelementptr [1 x i32], ptr %stack.ptr_187, i32 0, i32 0
  %698 = getelementptr [3 x i32], ptr %662, i32 0, i32 0
  %699 = load i32, ptr %698
  store i32 %699, ptr %697
  %700 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  %701 = call ccc i1 @eclair_btree_insert_value_6(ptr %700, ptr %stack.ptr_187)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_185)
  br label %loop_41
range_query.end_40:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_181)
  br label %loop_40
range_query.end_39:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_177)
  br label %loop_39
range_query.end_38:
  %702 = getelementptr [3 x i32], ptr %stack.ptr_188, i32 0, i32 0
  store i32 0, ptr %702
  %703 = getelementptr [3 x i32], ptr %stack.ptr_188, i32 0, i32 1
  store i32 0, ptr %703
  %704 = getelementptr [3 x i32], ptr %stack.ptr_188, i32 0, i32 2
  store i32 0, ptr %704
  %705 = getelementptr [3 x i32], ptr %stack.ptr_189, i32 0, i32 0
  store i32 4294967295, ptr %705
  %706 = getelementptr [3 x i32], ptr %stack.ptr_189, i32 0, i32 1
  store i32 4294967295, ptr %706
  %707 = getelementptr [3 x i32], ptr %stack.ptr_189, i32 0, i32 2
  store i32 4294967295, ptr %707
  %708 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %708, ptr %stack.ptr_188, ptr %stack.ptr_190)
  %709 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %709, ptr %stack.ptr_189, ptr %stack.ptr_191)
  br label %loop_42
loop_42:
  %710 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_190, ptr %stack.ptr_191)
  br i1 %710, label %if_51, label %end_if_51
if_51:
  br label %range_query.end_41
end_if_51:
  %711 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_190)
  %712 = getelementptr [3 x i32], ptr %stack.ptr_192, i32 0, i32 0
  %713 = getelementptr [3 x i32], ptr %711, i32 0, i32 2
  %714 = load i32, ptr %713
  store i32 %714, ptr %712
  %715 = getelementptr [3 x i32], ptr %stack.ptr_192, i32 0, i32 1
  store i32 0, ptr %715
  %716 = getelementptr [3 x i32], ptr %stack.ptr_192, i32 0, i32 2
  store i32 0, ptr %716
  %717 = getelementptr [3 x i32], ptr %stack.ptr_193, i32 0, i32 0
  %718 = getelementptr [3 x i32], ptr %711, i32 0, i32 2
  %719 = load i32, ptr %718
  store i32 %719, ptr %717
  %720 = getelementptr [3 x i32], ptr %stack.ptr_193, i32 0, i32 1
  store i32 4294967295, ptr %720
  %721 = getelementptr [3 x i32], ptr %stack.ptr_193, i32 0, i32 2
  store i32 4294967295, ptr %721
  %722 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %722, ptr %stack.ptr_192, ptr %stack.ptr_194)
  %723 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %723, ptr %stack.ptr_193, ptr %stack.ptr_195)
  br label %loop_43
loop_43:
  %724 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_194, ptr %stack.ptr_195)
  br i1 %724, label %if_52, label %end_if_52
if_52:
  br label %range_query.end_42
end_if_52:
  %725 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_194)
  %726 = getelementptr [2 x i32], ptr %stack.ptr_196, i32 0, i32 0
  %727 = getelementptr [3 x i32], ptr %725, i32 0, i32 1
  %728 = load i32, ptr %727
  store i32 %728, ptr %726
  %729 = getelementptr [2 x i32], ptr %stack.ptr_196, i32 0, i32 1
  %730 = getelementptr [3 x i32], ptr %725, i32 0, i32 2
  %731 = load i32, ptr %730
  store i32 %731, ptr %729
  %732 = getelementptr [2 x i32], ptr %stack.ptr_197, i32 0, i32 0
  %733 = getelementptr [3 x i32], ptr %725, i32 0, i32 1
  %734 = load i32, ptr %733
  store i32 %734, ptr %732
  %735 = getelementptr [2 x i32], ptr %stack.ptr_197, i32 0, i32 1
  %736 = getelementptr [3 x i32], ptr %725, i32 0, i32 2
  %737 = load i32, ptr %736
  store i32 %737, ptr %735
  %738 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_lower_bound_1(ptr %738, ptr %stack.ptr_196, ptr %stack.ptr_198)
  %739 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_upper_bound_1(ptr %739, ptr %stack.ptr_197, ptr %stack.ptr_199)
  br label %loop_44
loop_44:
  %740 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_198, ptr %stack.ptr_199)
  br i1 %740, label %if_53, label %end_if_53
if_53:
  br label %range_query.end_43
end_if_53:
  %741 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_198)
  %742 = getelementptr [1 x i32], ptr %stack.ptr_200, i32 0, i32 0
  %743 = getelementptr [3 x i32], ptr %711, i32 0, i32 0
  %744 = load i32, ptr %743
  store i32 %744, ptr %742
  %745 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  %746 = call ccc i1 @eclair_btree_insert_value_6(ptr %745, ptr %stack.ptr_200)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_198)
  br label %loop_44
range_query.end_43:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_194)
  br label %loop_43
range_query.end_42:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_190)
  br label %loop_42
range_query.end_41:
  %747 = getelementptr [2 x i32], ptr %stack.ptr_201, i32 0, i32 0
  store i32 0, ptr %747
  %748 = getelementptr [2 x i32], ptr %stack.ptr_201, i32 0, i32 1
  store i32 54, ptr %748
  %749 = getelementptr [2 x i32], ptr %stack.ptr_202, i32 0, i32 0
  store i32 4294967295, ptr %749
  %750 = getelementptr [2 x i32], ptr %stack.ptr_202, i32 0, i32 1
  store i32 54, ptr %750
  %751 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_lower_bound_2(ptr %751, ptr %stack.ptr_201, ptr %stack.ptr_203)
  %752 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_upper_bound_2(ptr %752, ptr %stack.ptr_202, ptr %stack.ptr_204)
  br label %loop_45
loop_45:
  %753 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_203, ptr %stack.ptr_204)
  br i1 %753, label %if_54, label %end_if_54
if_54:
  br label %range_query.end_44
end_if_54:
  %754 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_203)
  %755 = getelementptr [1 x i32], ptr %stack.ptr_205, i32 0, i32 0
  %756 = getelementptr [2 x i32], ptr %754, i32 0, i32 0
  %757 = load i32, ptr %756
  store i32 %757, ptr %755
  %758 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  %759 = call ccc i1 @eclair_btree_insert_value_6(ptr %758, ptr %stack.ptr_205)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_203)
  br label %loop_45
range_query.end_44:
  %760 = getelementptr [3 x i32], ptr %stack.ptr_206, i32 0, i32 0
  store i32 0, ptr %760
  %761 = getelementptr [3 x i32], ptr %stack.ptr_206, i32 0, i32 1
  store i32 0, ptr %761
  %762 = getelementptr [3 x i32], ptr %stack.ptr_206, i32 0, i32 2
  store i32 0, ptr %762
  %763 = getelementptr [3 x i32], ptr %stack.ptr_207, i32 0, i32 0
  store i32 4294967295, ptr %763
  %764 = getelementptr [3 x i32], ptr %stack.ptr_207, i32 0, i32 1
  store i32 4294967295, ptr %764
  %765 = getelementptr [3 x i32], ptr %stack.ptr_207, i32 0, i32 2
  store i32 4294967295, ptr %765
  %766 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_lower_bound_0(ptr %766, ptr %stack.ptr_206, ptr %stack.ptr_208)
  %767 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_upper_bound_0(ptr %767, ptr %stack.ptr_207, ptr %stack.ptr_209)
  br label %loop_46
loop_46:
  %768 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_208, ptr %stack.ptr_209)
  br i1 %768, label %if_55, label %end_if_55
if_55:
  br label %range_query.end_45
end_if_55:
  %769 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_208)
  %770 = getelementptr [1 x i32], ptr %stack.ptr_210, i32 0, i32 0
  %771 = getelementptr [3 x i32], ptr %769, i32 0, i32 2
  %772 = load i32, ptr %771
  store i32 %772, ptr %770
  %773 = getelementptr [1 x i32], ptr %stack.ptr_211, i32 0, i32 0
  %774 = getelementptr [3 x i32], ptr %769, i32 0, i32 2
  %775 = load i32, ptr %774
  store i32 %775, ptr %773
  %776 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %776, ptr %stack.ptr_210, ptr %stack.ptr_212)
  %777 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %777, ptr %stack.ptr_211, ptr %stack.ptr_213)
  br label %loop_47
loop_47:
  %778 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_212, ptr %stack.ptr_213)
  br i1 %778, label %if_56, label %end_if_56
if_56:
  br label %range_query.end_46
end_if_56:
  %779 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_212)
  %780 = getelementptr [3 x i32], ptr %stack.ptr_214, i32 0, i32 0
  %781 = getelementptr [3 x i32], ptr %769, i32 0, i32 0
  %782 = load i32, ptr %781
  store i32 %782, ptr %780
  %783 = getelementptr [3 x i32], ptr %stack.ptr_214, i32 0, i32 1
  %784 = getelementptr [3 x i32], ptr %769, i32 0, i32 2
  %785 = load i32, ptr %784
  store i32 %785, ptr %783
  %786 = getelementptr [3 x i32], ptr %stack.ptr_214, i32 0, i32 2
  %787 = getelementptr [3 x i32], ptr %769, i32 0, i32 1
  %788 = load i32, ptr %787
  store i32 %788, ptr %786
  %789 = getelementptr %program, ptr %arg_0, i32 0, i32 73
  %790 = call ccc i1 @eclair_btree_insert_value_0(ptr %789, ptr %stack.ptr_214)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_212)
  br label %loop_47
range_query.end_46:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_208)
  br label %loop_46
range_query.end_45:
  %791 = getelementptr [2 x i32], ptr %stack.ptr_215, i32 0, i32 0
  store i32 0, ptr %791
  %792 = getelementptr [2 x i32], ptr %stack.ptr_215, i32 0, i32 1
  store i32 0, ptr %792
  %793 = getelementptr [2 x i32], ptr %stack.ptr_216, i32 0, i32 0
  store i32 4294967295, ptr %793
  %794 = getelementptr [2 x i32], ptr %stack.ptr_216, i32 0, i32 1
  store i32 4294967295, ptr %794
  %795 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %795, ptr %stack.ptr_215, ptr %stack.ptr_217)
  %796 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %796, ptr %stack.ptr_216, ptr %stack.ptr_218)
  br label %loop_48
loop_48:
  %797 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_217, ptr %stack.ptr_218)
  br i1 %797, label %if_57, label %end_if_57
if_57:
  br label %range_query.end_47
end_if_57:
  %798 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_217)
  %799 = getelementptr [3 x i32], ptr %stack.ptr_219, i32 0, i32 0
  %800 = getelementptr [2 x i32], ptr %798, i32 0, i32 1
  %801 = load i32, ptr %800
  store i32 %801, ptr %799
  %802 = getelementptr [3 x i32], ptr %stack.ptr_219, i32 0, i32 1
  store i32 0, ptr %802
  %803 = getelementptr [3 x i32], ptr %stack.ptr_219, i32 0, i32 2
  store i32 0, ptr %803
  %804 = getelementptr [3 x i32], ptr %stack.ptr_220, i32 0, i32 0
  %805 = getelementptr [2 x i32], ptr %798, i32 0, i32 1
  %806 = load i32, ptr %805
  store i32 %806, ptr %804
  %807 = getelementptr [3 x i32], ptr %stack.ptr_220, i32 0, i32 1
  store i32 4294967295, ptr %807
  %808 = getelementptr [3 x i32], ptr %stack.ptr_220, i32 0, i32 2
  store i32 4294967295, ptr %808
  %809 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_0(ptr %809, ptr %stack.ptr_219, ptr %stack.ptr_221)
  %810 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_0(ptr %810, ptr %stack.ptr_220, ptr %stack.ptr_222)
  br label %loop_49
loop_49:
  %811 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_221, ptr %stack.ptr_222)
  br i1 %811, label %if_58, label %end_if_58
if_58:
  br label %range_query.end_48
end_if_58:
  %812 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_221)
  %813 = getelementptr [1 x i32], ptr %stack.ptr_223, i32 0, i32 0
  %814 = getelementptr [3 x i32], ptr %812, i32 0, i32 2
  %815 = load i32, ptr %814
  store i32 %815, ptr %813
  %816 = getelementptr [1 x i32], ptr %stack.ptr_224, i32 0, i32 0
  %817 = getelementptr [3 x i32], ptr %812, i32 0, i32 2
  %818 = load i32, ptr %817
  store i32 %818, ptr %816
  %819 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %819, ptr %stack.ptr_223, ptr %stack.ptr_225)
  %820 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %820, ptr %stack.ptr_224, ptr %stack.ptr_226)
  br label %loop_50
loop_50:
  %821 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_225, ptr %stack.ptr_226)
  br i1 %821, label %if_59, label %end_if_59
if_59:
  br label %range_query.end_49
end_if_59:
  %822 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_225)
  %823 = getelementptr [3 x i32], ptr %stack.ptr_227, i32 0, i32 0
  %824 = getelementptr [2 x i32], ptr %798, i32 0, i32 1
  %825 = load i32, ptr %824
  store i32 %825, ptr %823
  %826 = getelementptr [3 x i32], ptr %stack.ptr_227, i32 0, i32 1
  %827 = getelementptr [3 x i32], ptr %812, i32 0, i32 2
  %828 = load i32, ptr %827
  store i32 %828, ptr %826
  %829 = getelementptr [3 x i32], ptr %stack.ptr_227, i32 0, i32 2
  %830 = getelementptr [3 x i32], ptr %812, i32 0, i32 1
  %831 = load i32, ptr %830
  store i32 %831, ptr %829
  %832 = getelementptr %program, ptr %arg_0, i32 0, i32 72
  %833 = call ccc i1 @eclair_btree_insert_value_0(ptr %832, ptr %stack.ptr_227)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_225)
  br label %loop_50
range_query.end_49:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_221)
  br label %loop_49
range_query.end_48:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_217)
  br label %loop_48
range_query.end_47:
  %834 = getelementptr [4 x i32], ptr %stack.ptr_228, i32 0, i32 0
  store i32 0, ptr %834
  %835 = getelementptr [4 x i32], ptr %stack.ptr_228, i32 0, i32 1
  store i32 0, ptr %835
  %836 = getelementptr [4 x i32], ptr %stack.ptr_228, i32 0, i32 2
  store i32 0, ptr %836
  %837 = getelementptr [4 x i32], ptr %stack.ptr_228, i32 0, i32 3
  store i32 0, ptr %837
  %838 = getelementptr [4 x i32], ptr %stack.ptr_229, i32 0, i32 0
  store i32 4294967295, ptr %838
  %839 = getelementptr [4 x i32], ptr %stack.ptr_229, i32 0, i32 1
  store i32 4294967295, ptr %839
  %840 = getelementptr [4 x i32], ptr %stack.ptr_229, i32 0, i32 2
  store i32 4294967295, ptr %840
  %841 = getelementptr [4 x i32], ptr %stack.ptr_229, i32 0, i32 3
  store i32 4294967295, ptr %841
  %842 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %842, ptr %stack.ptr_228, ptr %stack.ptr_230)
  %843 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %843, ptr %stack.ptr_229, ptr %stack.ptr_231)
  br label %loop_51
loop_51:
  %844 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_230, ptr %stack.ptr_231)
  br i1 %844, label %if_60, label %end_if_60
if_60:
  br label %range_query.end_50
end_if_60:
  %845 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_230)
  %846 = getelementptr [1 x i32], ptr %stack.ptr_232, i32 0, i32 0
  %847 = getelementptr [4 x i32], ptr %845, i32 0, i32 2
  %848 = load i32, ptr %847
  store i32 %848, ptr %846
  %849 = getelementptr [1 x i32], ptr %stack.ptr_233, i32 0, i32 0
  %850 = getelementptr [4 x i32], ptr %845, i32 0, i32 2
  %851 = load i32, ptr %850
  store i32 %851, ptr %849
  %852 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %852, ptr %stack.ptr_232, ptr %stack.ptr_234)
  %853 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %853, ptr %stack.ptr_233, ptr %stack.ptr_235)
  br label %loop_52
loop_52:
  %854 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_234, ptr %stack.ptr_235)
  br i1 %854, label %if_61, label %end_if_61
if_61:
  br label %range_query.end_51
end_if_61:
  %855 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_234)
  %856 = getelementptr [2 x i32], ptr %stack.ptr_236, i32 0, i32 0
  %857 = getelementptr [4 x i32], ptr %845, i32 0, i32 0
  %858 = load i32, ptr %857
  store i32 %858, ptr %856
  %859 = getelementptr [2 x i32], ptr %stack.ptr_236, i32 0, i32 1
  %860 = getelementptr [4 x i32], ptr %845, i32 0, i32 2
  %861 = load i32, ptr %860
  store i32 %861, ptr %859
  %862 = getelementptr %program, ptr %arg_0, i32 0, i32 70
  %863 = call ccc i1 @eclair_btree_insert_value_1(ptr %862, ptr %stack.ptr_236)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_234)
  br label %loop_52
range_query.end_51:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_230)
  br label %loop_51
range_query.end_50:
  %864 = getelementptr [4 x i32], ptr %stack.ptr_237, i32 0, i32 0
  store i32 0, ptr %864
  %865 = getelementptr [4 x i32], ptr %stack.ptr_237, i32 0, i32 1
  store i32 0, ptr %865
  %866 = getelementptr [4 x i32], ptr %stack.ptr_237, i32 0, i32 2
  store i32 0, ptr %866
  %867 = getelementptr [4 x i32], ptr %stack.ptr_237, i32 0, i32 3
  store i32 0, ptr %867
  %868 = getelementptr [4 x i32], ptr %stack.ptr_238, i32 0, i32 0
  store i32 4294967295, ptr %868
  %869 = getelementptr [4 x i32], ptr %stack.ptr_238, i32 0, i32 1
  store i32 4294967295, ptr %869
  %870 = getelementptr [4 x i32], ptr %stack.ptr_238, i32 0, i32 2
  store i32 4294967295, ptr %870
  %871 = getelementptr [4 x i32], ptr %stack.ptr_238, i32 0, i32 3
  store i32 4294967295, ptr %871
  %872 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %872, ptr %stack.ptr_237, ptr %stack.ptr_239)
  %873 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %873, ptr %stack.ptr_238, ptr %stack.ptr_240)
  br label %loop_53
loop_53:
  %874 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_239, ptr %stack.ptr_240)
  br i1 %874, label %if_62, label %end_if_62
if_62:
  br label %range_query.end_52
end_if_62:
  %875 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_239)
  %876 = getelementptr [1 x i32], ptr %stack.ptr_241, i32 0, i32 0
  %877 = getelementptr [4 x i32], ptr %875, i32 0, i32 3
  %878 = load i32, ptr %877
  store i32 %878, ptr %876
  %879 = getelementptr [1 x i32], ptr %stack.ptr_242, i32 0, i32 0
  %880 = getelementptr [4 x i32], ptr %875, i32 0, i32 3
  %881 = load i32, ptr %880
  store i32 %881, ptr %879
  %882 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %882, ptr %stack.ptr_241, ptr %stack.ptr_243)
  %883 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %883, ptr %stack.ptr_242, ptr %stack.ptr_244)
  br label %loop_54
loop_54:
  %884 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_243, ptr %stack.ptr_244)
  br i1 %884, label %if_63, label %end_if_63
if_63:
  br label %range_query.end_53
end_if_63:
  %885 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_243)
  %886 = getelementptr [2 x i32], ptr %stack.ptr_245, i32 0, i32 0
  %887 = getelementptr [4 x i32], ptr %875, i32 0, i32 0
  %888 = load i32, ptr %887
  store i32 %888, ptr %886
  %889 = getelementptr [2 x i32], ptr %stack.ptr_245, i32 0, i32 1
  %890 = getelementptr [4 x i32], ptr %875, i32 0, i32 3
  %891 = load i32, ptr %890
  store i32 %891, ptr %889
  %892 = getelementptr %program, ptr %arg_0, i32 0, i32 70
  %893 = call ccc i1 @eclair_btree_insert_value_1(ptr %892, ptr %stack.ptr_245)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_243)
  br label %loop_54
range_query.end_53:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_239)
  br label %loop_53
range_query.end_52:
  %894 = getelementptr [4 x i32], ptr %stack.ptr_246, i32 0, i32 0
  store i32 0, ptr %894
  %895 = getelementptr [4 x i32], ptr %stack.ptr_246, i32 0, i32 1
  store i32 0, ptr %895
  %896 = getelementptr [4 x i32], ptr %stack.ptr_246, i32 0, i32 2
  store i32 0, ptr %896
  %897 = getelementptr [4 x i32], ptr %stack.ptr_246, i32 0, i32 3
  store i32 0, ptr %897
  %898 = getelementptr [4 x i32], ptr %stack.ptr_247, i32 0, i32 0
  store i32 4294967295, ptr %898
  %899 = getelementptr [4 x i32], ptr %stack.ptr_247, i32 0, i32 1
  store i32 4294967295, ptr %899
  %900 = getelementptr [4 x i32], ptr %stack.ptr_247, i32 0, i32 2
  store i32 4294967295, ptr %900
  %901 = getelementptr [4 x i32], ptr %stack.ptr_247, i32 0, i32 3
  store i32 4294967295, ptr %901
  %902 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %902, ptr %stack.ptr_246, ptr %stack.ptr_248)
  %903 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %903, ptr %stack.ptr_247, ptr %stack.ptr_249)
  br label %loop_55
loop_55:
  %904 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_248, ptr %stack.ptr_249)
  br i1 %904, label %if_64, label %end_if_64
if_64:
  br label %range_query.end_54
end_if_64:
  %905 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_248)
  %906 = getelementptr [1 x i32], ptr %stack.ptr_250, i32 0, i32 0
  %907 = getelementptr [4 x i32], ptr %905, i32 0, i32 2
  %908 = load i32, ptr %907
  store i32 %908, ptr %906
  %909 = getelementptr [1 x i32], ptr %stack.ptr_251, i32 0, i32 0
  %910 = getelementptr [4 x i32], ptr %905, i32 0, i32 2
  %911 = load i32, ptr %910
  store i32 %911, ptr %909
  %912 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %912, ptr %stack.ptr_250, ptr %stack.ptr_252)
  %913 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %913, ptr %stack.ptr_251, ptr %stack.ptr_253)
  br label %loop_56
loop_56:
  %914 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_252, ptr %stack.ptr_253)
  br i1 %914, label %if_65, label %end_if_65
if_65:
  br label %range_query.end_55
end_if_65:
  %915 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_252)
  %916 = getelementptr [2 x i32], ptr %stack.ptr_254, i32 0, i32 0
  %917 = getelementptr [4 x i32], ptr %905, i32 0, i32 0
  %918 = load i32, ptr %917
  store i32 %918, ptr %916
  %919 = getelementptr [2 x i32], ptr %stack.ptr_254, i32 0, i32 1
  %920 = getelementptr [4 x i32], ptr %905, i32 0, i32 2
  %921 = load i32, ptr %920
  store i32 %921, ptr %919
  %922 = getelementptr %program, ptr %arg_0, i32 0, i32 69
  %923 = call ccc i1 @eclair_btree_insert_value_1(ptr %922, ptr %stack.ptr_254)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_252)
  br label %loop_56
range_query.end_55:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_248)
  br label %loop_55
range_query.end_54:
  %924 = getelementptr [4 x i32], ptr %stack.ptr_255, i32 0, i32 0
  store i32 0, ptr %924
  %925 = getelementptr [4 x i32], ptr %stack.ptr_255, i32 0, i32 1
  store i32 0, ptr %925
  %926 = getelementptr [4 x i32], ptr %stack.ptr_255, i32 0, i32 2
  store i32 0, ptr %926
  %927 = getelementptr [4 x i32], ptr %stack.ptr_255, i32 0, i32 3
  store i32 0, ptr %927
  %928 = getelementptr [4 x i32], ptr %stack.ptr_256, i32 0, i32 0
  store i32 4294967295, ptr %928
  %929 = getelementptr [4 x i32], ptr %stack.ptr_256, i32 0, i32 1
  store i32 4294967295, ptr %929
  %930 = getelementptr [4 x i32], ptr %stack.ptr_256, i32 0, i32 2
  store i32 4294967295, ptr %930
  %931 = getelementptr [4 x i32], ptr %stack.ptr_256, i32 0, i32 3
  store i32 4294967295, ptr %931
  %932 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %932, ptr %stack.ptr_255, ptr %stack.ptr_257)
  %933 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %933, ptr %stack.ptr_256, ptr %stack.ptr_258)
  br label %loop_57
loop_57:
  %934 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_257, ptr %stack.ptr_258)
  br i1 %934, label %if_66, label %end_if_66
if_66:
  br label %range_query.end_56
end_if_66:
  %935 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_257)
  %936 = getelementptr [1 x i32], ptr %stack.ptr_259, i32 0, i32 0
  %937 = getelementptr [4 x i32], ptr %935, i32 0, i32 3
  %938 = load i32, ptr %937
  store i32 %938, ptr %936
  %939 = getelementptr [1 x i32], ptr %stack.ptr_260, i32 0, i32 0
  %940 = getelementptr [4 x i32], ptr %935, i32 0, i32 3
  %941 = load i32, ptr %940
  store i32 %941, ptr %939
  %942 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %942, ptr %stack.ptr_259, ptr %stack.ptr_261)
  %943 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %943, ptr %stack.ptr_260, ptr %stack.ptr_262)
  br label %loop_58
loop_58:
  %944 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_261, ptr %stack.ptr_262)
  br i1 %944, label %if_67, label %end_if_67
if_67:
  br label %range_query.end_57
end_if_67:
  %945 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_261)
  %946 = getelementptr [2 x i32], ptr %stack.ptr_263, i32 0, i32 0
  %947 = getelementptr [4 x i32], ptr %935, i32 0, i32 0
  %948 = load i32, ptr %947
  store i32 %948, ptr %946
  %949 = getelementptr [2 x i32], ptr %stack.ptr_263, i32 0, i32 1
  %950 = getelementptr [4 x i32], ptr %935, i32 0, i32 3
  %951 = load i32, ptr %950
  store i32 %951, ptr %949
  %952 = getelementptr %program, ptr %arg_0, i32 0, i32 69
  %953 = call ccc i1 @eclair_btree_insert_value_1(ptr %952, ptr %stack.ptr_263)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_261)
  br label %loop_58
range_query.end_57:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_257)
  br label %loop_57
range_query.end_56:
  %954 = getelementptr [1 x i32], ptr %stack.ptr_264, i32 0, i32 0
  store i32 59, ptr %954
  %955 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %956 = call ccc i1 @eclair_btree_insert_value_6(ptr %955, ptr %stack.ptr_264)
  %957 = getelementptr [1 x i32], ptr %stack.ptr_265, i32 0, i32 0
  store i32 58, ptr %957
  %958 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %959 = call ccc i1 @eclair_btree_insert_value_6(ptr %958, ptr %stack.ptr_265)
  %960 = getelementptr [1 x i32], ptr %stack.ptr_266, i32 0, i32 0
  store i32 57, ptr %960
  %961 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %962 = call ccc i1 @eclair_btree_insert_value_6(ptr %961, ptr %stack.ptr_266)
  %963 = getelementptr [1 x i32], ptr %stack.ptr_267, i32 0, i32 0
  store i32 56, ptr %963
  %964 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %965 = call ccc i1 @eclair_btree_insert_value_6(ptr %964, ptr %stack.ptr_267)
  %966 = getelementptr [1 x i32], ptr %stack.ptr_268, i32 0, i32 0
  store i32 55, ptr %966
  %967 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %968 = call ccc i1 @eclair_btree_insert_value_6(ptr %967, ptr %stack.ptr_268)
  %969 = getelementptr [3 x i32], ptr %stack.ptr_269, i32 0, i32 0
  store i32 0, ptr %969
  %970 = getelementptr [3 x i32], ptr %stack.ptr_269, i32 0, i32 1
  store i32 0, ptr %970
  %971 = getelementptr [3 x i32], ptr %stack.ptr_269, i32 0, i32 2
  store i32 0, ptr %971
  %972 = getelementptr [3 x i32], ptr %stack.ptr_270, i32 0, i32 0
  store i32 4294967295, ptr %972
  %973 = getelementptr [3 x i32], ptr %stack.ptr_270, i32 0, i32 1
  store i32 4294967295, ptr %973
  %974 = getelementptr [3 x i32], ptr %stack.ptr_270, i32 0, i32 2
  store i32 4294967295, ptr %974
  %975 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %975, ptr %stack.ptr_269, ptr %stack.ptr_271)
  %976 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %976, ptr %stack.ptr_270, ptr %stack.ptr_272)
  br label %loop_59
loop_59:
  %977 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_271, ptr %stack.ptr_272)
  br i1 %977, label %if_68, label %end_if_68
if_68:
  br label %range_query.end_58
end_if_68:
  %978 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_271)
  %979 = getelementptr [2 x i32], ptr %stack.ptr_273, i32 0, i32 0
  %980 = getelementptr [3 x i32], ptr %978, i32 0, i32 2
  %981 = load i32, ptr %980
  store i32 %981, ptr %979
  %982 = getelementptr [2 x i32], ptr %stack.ptr_273, i32 0, i32 1
  store i32 0, ptr %982
  %983 = getelementptr [2 x i32], ptr %stack.ptr_274, i32 0, i32 0
  %984 = getelementptr [3 x i32], ptr %978, i32 0, i32 2
  %985 = load i32, ptr %984
  store i32 %985, ptr %983
  %986 = getelementptr [2 x i32], ptr %stack.ptr_274, i32 0, i32 1
  store i32 4294967295, ptr %986
  %987 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %987, ptr %stack.ptr_273, ptr %stack.ptr_275)
  %988 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %988, ptr %stack.ptr_274, ptr %stack.ptr_276)
  br label %loop_60
loop_60:
  %989 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_275, ptr %stack.ptr_276)
  br i1 %989, label %if_69, label %end_if_69
if_69:
  br label %range_query.end_59
end_if_69:
  %990 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_275)
  %991 = getelementptr [2 x i32], ptr %stack.ptr_277, i32 0, i32 0
  %992 = getelementptr [3 x i32], ptr %978, i32 0, i32 0
  %993 = load i32, ptr %992
  store i32 %993, ptr %991
  %994 = getelementptr [2 x i32], ptr %stack.ptr_277, i32 0, i32 1
  %995 = getelementptr [3 x i32], ptr %978, i32 0, i32 2
  %996 = load i32, ptr %995
  store i32 %996, ptr %994
  %997 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  %998 = call ccc i1 @eclair_btree_insert_value_1(ptr %997, ptr %stack.ptr_277)
  %999 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  %1000 = call ccc i1 @eclair_btree_insert_value_2(ptr %999, ptr %stack.ptr_277)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_275)
  br label %loop_60
range_query.end_59:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_271)
  br label %loop_59
range_query.end_58:
  %1001 = getelementptr [2 x i32], ptr %stack.ptr_278, i32 0, i32 0
  store i32 0, ptr %1001
  %1002 = getelementptr [2 x i32], ptr %stack.ptr_278, i32 0, i32 1
  store i32 0, ptr %1002
  %1003 = getelementptr [2 x i32], ptr %stack.ptr_279, i32 0, i32 0
  store i32 4294967295, ptr %1003
  %1004 = getelementptr [2 x i32], ptr %stack.ptr_279, i32 0, i32 1
  store i32 4294967295, ptr %1004
  %1005 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1005, ptr %stack.ptr_278, ptr %stack.ptr_280)
  %1006 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1006, ptr %stack.ptr_279, ptr %stack.ptr_281)
  br label %loop_61
loop_61:
  %1007 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_280, ptr %stack.ptr_281)
  br i1 %1007, label %if_70, label %end_if_70
if_70:
  br label %range_query.end_60
end_if_70:
  %1008 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_280)
  %1009 = getelementptr [2 x i32], ptr %stack.ptr_282, i32 0, i32 0
  %1010 = getelementptr [2 x i32], ptr %1008, i32 0, i32 0
  %1011 = load i32, ptr %1010
  store i32 %1011, ptr %1009
  %1012 = getelementptr [2 x i32], ptr %stack.ptr_282, i32 0, i32 1
  %1013 = getelementptr [2 x i32], ptr %1008, i32 0, i32 0
  %1014 = load i32, ptr %1013
  store i32 %1014, ptr %1012
  %1015 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  %1016 = call ccc i1 @eclair_btree_insert_value_1(ptr %1015, ptr %stack.ptr_282)
  %1017 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  %1018 = call ccc i1 @eclair_btree_insert_value_2(ptr %1017, ptr %stack.ptr_282)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_280)
  br label %loop_61
range_query.end_60:
  %1019 = getelementptr [2 x i32], ptr %stack.ptr_283, i32 0, i32 0
  store i32 0, ptr %1019
  %1020 = getelementptr [2 x i32], ptr %stack.ptr_283, i32 0, i32 1
  store i32 0, ptr %1020
  %1021 = getelementptr [2 x i32], ptr %stack.ptr_284, i32 0, i32 0
  store i32 4294967295, ptr %1021
  %1022 = getelementptr [2 x i32], ptr %stack.ptr_284, i32 0, i32 1
  store i32 4294967295, ptr %1022
  %1023 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_lower_bound_1(ptr %1023, ptr %stack.ptr_283, ptr %stack.ptr_285)
  %1024 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_upper_bound_1(ptr %1024, ptr %stack.ptr_284, ptr %stack.ptr_286)
  br label %loop_62
loop_62:
  %1025 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_285, ptr %stack.ptr_286)
  br i1 %1025, label %if_71, label %end_if_71
if_71:
  br label %range_query.end_61
end_if_71:
  %1026 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_285)
  %1027 = getelementptr [2 x i32], ptr %stack.ptr_287, i32 0, i32 0
  %1028 = getelementptr [2 x i32], ptr %1026, i32 0, i32 0
  %1029 = load i32, ptr %1028
  store i32 %1029, ptr %1027
  %1030 = getelementptr [2 x i32], ptr %stack.ptr_287, i32 0, i32 1
  store i32 0, ptr %1030
  %1031 = getelementptr [2 x i32], ptr %stack.ptr_288, i32 0, i32 0
  %1032 = getelementptr [2 x i32], ptr %1026, i32 0, i32 0
  %1033 = load i32, ptr %1032
  store i32 %1033, ptr %1031
  %1034 = getelementptr [2 x i32], ptr %stack.ptr_288, i32 0, i32 1
  store i32 4294967295, ptr %1034
  %1035 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_lower_bound_1(ptr %1035, ptr %stack.ptr_287, ptr %stack.ptr_289)
  %1036 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_upper_bound_1(ptr %1036, ptr %stack.ptr_288, ptr %stack.ptr_290)
  br label %loop_63
loop_63:
  %1037 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_289, ptr %stack.ptr_290)
  br i1 %1037, label %if_72, label %end_if_72
if_72:
  br label %range_query.end_62
end_if_72:
  %1038 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_289)
  %1039 = getelementptr [2 x i32], ptr %stack.ptr_291, i32 0, i32 0
  %1040 = getelementptr [2 x i32], ptr %1026, i32 0, i32 1
  %1041 = load i32, ptr %1040
  store i32 %1041, ptr %1039
  %1042 = getelementptr [2 x i32], ptr %stack.ptr_291, i32 0, i32 1
  store i32 0, ptr %1042
  %1043 = getelementptr [2 x i32], ptr %stack.ptr_292, i32 0, i32 0
  %1044 = getelementptr [2 x i32], ptr %1026, i32 0, i32 1
  %1045 = load i32, ptr %1044
  store i32 %1045, ptr %1043
  %1046 = getelementptr [2 x i32], ptr %stack.ptr_292, i32 0, i32 1
  store i32 4294967295, ptr %1046
  %1047 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %1047, ptr %stack.ptr_291, ptr %stack.ptr_293)
  %1048 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %1048, ptr %stack.ptr_292, ptr %stack.ptr_294)
  br label %loop_64
loop_64:
  %1049 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_293, ptr %stack.ptr_294)
  br i1 %1049, label %if_73, label %end_if_73
if_73:
  br label %range_query.end_63
end_if_73:
  %1050 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_293)
  %1051 = getelementptr [2 x i32], ptr %stack.ptr_295, i32 0, i32 0
  %1052 = getelementptr [2 x i32], ptr %1038, i32 0, i32 1
  %1053 = load i32, ptr %1052
  store i32 %1053, ptr %1051
  %1054 = getelementptr [2 x i32], ptr %stack.ptr_295, i32 0, i32 1
  store i32 0, ptr %1054
  %1055 = getelementptr [2 x i32], ptr %stack.ptr_296, i32 0, i32 0
  %1056 = getelementptr [2 x i32], ptr %1038, i32 0, i32 1
  %1057 = load i32, ptr %1056
  store i32 %1057, ptr %1055
  %1058 = getelementptr [2 x i32], ptr %stack.ptr_296, i32 0, i32 1
  store i32 4294967295, ptr %1058
  %1059 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %1059, ptr %stack.ptr_295, ptr %stack.ptr_297)
  %1060 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %1060, ptr %stack.ptr_296, ptr %stack.ptr_298)
  br label %loop_65
loop_65:
  %1061 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_297, ptr %stack.ptr_298)
  br i1 %1061, label %if_74, label %end_if_74
if_74:
  br label %range_query.end_64
end_if_74:
  %1062 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_297)
  %1063 = getelementptr [2 x i32], ptr %1050, i32 0, i32 1
  %1064 = load i32, ptr %1063
  %1065 = getelementptr [2 x i32], ptr %1062, i32 0, i32 1
  %1066 = load i32, ptr %1065
  %1067 = icmp ne i32 %1064, %1066
  br i1 %1067, label %if_75, label %end_if_77
if_75:
  %1068 = getelementptr [2 x i32], ptr %stack.ptr_299, i32 0, i32 0
  %1069 = getelementptr [2 x i32], ptr %1050, i32 0, i32 1
  %1070 = load i32, ptr %1069
  store i32 %1070, ptr %1068
  %1071 = getelementptr [2 x i32], ptr %stack.ptr_299, i32 0, i32 1
  store i32 0, ptr %1071
  %1072 = getelementptr [2 x i32], ptr %stack.ptr_300, i32 0, i32 0
  %1073 = getelementptr [2 x i32], ptr %1050, i32 0, i32 1
  %1074 = load i32, ptr %1073
  store i32 %1074, ptr %1072
  %1075 = getelementptr [2 x i32], ptr %stack.ptr_300, i32 0, i32 1
  store i32 4294967295, ptr %1075
  %1076 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %1076, ptr %stack.ptr_299, ptr %stack.ptr_301)
  %1077 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %1077, ptr %stack.ptr_300, ptr %stack.ptr_302)
  br label %loop_66
loop_66:
  %1078 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_301, ptr %stack.ptr_302)
  br i1 %1078, label %if_76, label %end_if_75
if_76:
  br label %range_query.end_65
end_if_75:
  %1079 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_301)
  %1080 = getelementptr [2 x i32], ptr %stack.ptr_303, i32 0, i32 0
  %1081 = getelementptr [2 x i32], ptr %1062, i32 0, i32 1
  %1082 = load i32, ptr %1081
  store i32 %1082, ptr %1080
  %1083 = getelementptr [2 x i32], ptr %stack.ptr_303, i32 0, i32 1
  %1084 = getelementptr [2 x i32], ptr %1079, i32 0, i32 1
  %1085 = load i32, ptr %1084
  store i32 %1085, ptr %1083
  %1086 = getelementptr [2 x i32], ptr %stack.ptr_304, i32 0, i32 0
  %1087 = getelementptr [2 x i32], ptr %1062, i32 0, i32 1
  %1088 = load i32, ptr %1087
  store i32 %1088, ptr %1086
  %1089 = getelementptr [2 x i32], ptr %stack.ptr_304, i32 0, i32 1
  %1090 = getelementptr [2 x i32], ptr %1079, i32 0, i32 1
  %1091 = load i32, ptr %1090
  store i32 %1091, ptr %1089
  %1092 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %1092, ptr %stack.ptr_303, ptr %stack.ptr_305)
  %1093 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %1093, ptr %stack.ptr_304, ptr %stack.ptr_306)
  br label %loop_67
loop_67:
  %1094 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_305, ptr %stack.ptr_306)
  br i1 %1094, label %if_77, label %end_if_76
if_77:
  br label %range_query.end_66
end_if_76:
  %1095 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_305)
  %1096 = getelementptr [3 x i32], ptr %stack.ptr_307, i32 0, i32 0
  %1097 = getelementptr [2 x i32], ptr %1026, i32 0, i32 0
  %1098 = load i32, ptr %1097
  store i32 %1098, ptr %1096
  %1099 = getelementptr [3 x i32], ptr %stack.ptr_307, i32 0, i32 1
  %1100 = getelementptr [2 x i32], ptr %1050, i32 0, i32 1
  %1101 = load i32, ptr %1100
  store i32 %1101, ptr %1099
  %1102 = getelementptr [3 x i32], ptr %stack.ptr_307, i32 0, i32 2
  %1103 = getelementptr [2 x i32], ptr %1079, i32 0, i32 1
  %1104 = load i32, ptr %1103
  store i32 %1104, ptr %1102
  %1105 = getelementptr %program, ptr %arg_0, i32 0, i32 8
  %1106 = call ccc i1 @eclair_btree_insert_value_0(ptr %1105, ptr %stack.ptr_307)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_305)
  br label %loop_67
range_query.end_66:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_301)
  br label %loop_66
range_query.end_65:
  br label %end_if_77
end_if_77:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_297)
  br label %loop_65
range_query.end_64:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_293)
  br label %loop_64
range_query.end_63:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_289)
  br label %loop_63
range_query.end_62:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_285)
  br label %loop_62
range_query.end_61:
  %1107 = getelementptr [2 x i32], ptr %stack.ptr_308, i32 0, i32 0
  store i32 0, ptr %1107
  %1108 = getelementptr [2 x i32], ptr %stack.ptr_308, i32 0, i32 1
  store i32 0, ptr %1108
  %1109 = getelementptr [2 x i32], ptr %stack.ptr_309, i32 0, i32 0
  store i32 4294967295, ptr %1109
  %1110 = getelementptr [2 x i32], ptr %stack.ptr_309, i32 0, i32 1
  store i32 4294967295, ptr %1110
  %1111 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_lower_bound_1(ptr %1111, ptr %stack.ptr_308, ptr %stack.ptr_310)
  %1112 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_upper_bound_1(ptr %1112, ptr %stack.ptr_309, ptr %stack.ptr_311)
  br label %loop_68
loop_68:
  %1113 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_310, ptr %stack.ptr_311)
  br i1 %1113, label %if_78, label %end_if_78
if_78:
  br label %range_query.end_67
end_if_78:
  %1114 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_310)
  %1115 = getelementptr [2 x i32], ptr %stack.ptr_312, i32 0, i32 0
  %1116 = getelementptr [2 x i32], ptr %1114, i32 0, i32 1
  %1117 = load i32, ptr %1116
  store i32 %1117, ptr %1115
  %1118 = getelementptr [2 x i32], ptr %stack.ptr_312, i32 0, i32 1
  store i32 0, ptr %1118
  %1119 = getelementptr [2 x i32], ptr %stack.ptr_313, i32 0, i32 0
  %1120 = getelementptr [2 x i32], ptr %1114, i32 0, i32 1
  %1121 = load i32, ptr %1120
  store i32 %1121, ptr %1119
  %1122 = getelementptr [2 x i32], ptr %stack.ptr_313, i32 0, i32 1
  store i32 4294967295, ptr %1122
  %1123 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %1123, ptr %stack.ptr_312, ptr %stack.ptr_314)
  %1124 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %1124, ptr %stack.ptr_313, ptr %stack.ptr_315)
  br label %loop_69
loop_69:
  %1125 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_314, ptr %stack.ptr_315)
  br i1 %1125, label %if_79, label %end_if_79
if_79:
  br label %range_query.end_68
end_if_79:
  %1126 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_314)
  %1127 = getelementptr [2 x i32], ptr %stack.ptr_316, i32 0, i32 0
  %1128 = getelementptr [2 x i32], ptr %1126, i32 0, i32 1
  %1129 = load i32, ptr %1128
  store i32 %1129, ptr %1127
  %1130 = getelementptr [2 x i32], ptr %stack.ptr_316, i32 0, i32 1
  store i32 0, ptr %1130
  %1131 = getelementptr [2 x i32], ptr %stack.ptr_317, i32 0, i32 0
  %1132 = getelementptr [2 x i32], ptr %1126, i32 0, i32 1
  %1133 = load i32, ptr %1132
  store i32 %1133, ptr %1131
  %1134 = getelementptr [2 x i32], ptr %stack.ptr_317, i32 0, i32 1
  store i32 4294967295, ptr %1134
  %1135 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %1135, ptr %stack.ptr_316, ptr %stack.ptr_318)
  %1136 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %1136, ptr %stack.ptr_317, ptr %stack.ptr_319)
  br label %loop_70
loop_70:
  %1137 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_318, ptr %stack.ptr_319)
  br i1 %1137, label %if_80, label %end_if_80
if_80:
  br label %range_query.end_69
end_if_80:
  %1138 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_318)
  %1139 = getelementptr [3 x i32], ptr %stack.ptr_320, i32 0, i32 0
  %1140 = getelementptr [2 x i32], ptr %1114, i32 0, i32 0
  %1141 = load i32, ptr %1140
  store i32 %1141, ptr %1139
  %1142 = getelementptr [3 x i32], ptr %stack.ptr_320, i32 0, i32 1
  %1143 = getelementptr [2 x i32], ptr %1126, i32 0, i32 1
  %1144 = load i32, ptr %1143
  store i32 %1144, ptr %1142
  %1145 = getelementptr [3 x i32], ptr %stack.ptr_320, i32 0, i32 2
  %1146 = getelementptr [2 x i32], ptr %1138, i32 0, i32 1
  %1147 = load i32, ptr %1146
  store i32 %1147, ptr %1145
  %1148 = getelementptr %program, ptr %arg_0, i32 0, i32 8
  %1149 = call ccc i1 @eclair_btree_contains_0(ptr %1148, ptr %stack.ptr_320)
  %1150 = select i1 %1149, i1 0, i1 1
  br i1 %1150, label %if_81, label %end_if_81
if_81:
  %1151 = getelementptr [3 x i32], ptr %stack.ptr_321, i32 0, i32 0
  %1152 = getelementptr [2 x i32], ptr %1114, i32 0, i32 0
  %1153 = load i32, ptr %1152
  store i32 %1153, ptr %1151
  %1154 = getelementptr [3 x i32], ptr %stack.ptr_321, i32 0, i32 1
  %1155 = getelementptr [2 x i32], ptr %1126, i32 0, i32 1
  %1156 = load i32, ptr %1155
  store i32 %1156, ptr %1154
  %1157 = getelementptr [3 x i32], ptr %stack.ptr_321, i32 0, i32 2
  %1158 = getelementptr [2 x i32], ptr %1138, i32 0, i32 1
  %1159 = load i32, ptr %1158
  store i32 %1159, ptr %1157
  %1160 = getelementptr %program, ptr %arg_0, i32 0, i32 63
  %1161 = call ccc i1 @eclair_btree_insert_value_0(ptr %1160, ptr %stack.ptr_321)
  br label %end_if_81
end_if_81:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_318)
  br label %loop_70
range_query.end_69:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_314)
  br label %loop_69
range_query.end_68:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_310)
  br label %loop_68
range_query.end_67:
  %1162 = getelementptr [2 x i32], ptr %stack.ptr_322, i32 0, i32 0
  store i32 0, ptr %1162
  %1163 = getelementptr [2 x i32], ptr %stack.ptr_322, i32 0, i32 1
  store i32 0, ptr %1163
  %1164 = getelementptr [2 x i32], ptr %stack.ptr_323, i32 0, i32 0
  store i32 4294967295, ptr %1164
  %1165 = getelementptr [2 x i32], ptr %stack.ptr_323, i32 0, i32 1
  store i32 4294967295, ptr %1165
  %1166 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1166, ptr %stack.ptr_322, ptr %stack.ptr_324)
  %1167 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1167, ptr %stack.ptr_323, ptr %stack.ptr_325)
  br label %loop_71
loop_71:
  %1168 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_324, ptr %stack.ptr_325)
  br i1 %1168, label %if_82, label %end_if_82
if_82:
  br label %range_query.end_70
end_if_82:
  %1169 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_324)
  %1170 = getelementptr [2 x i32], ptr %stack.ptr_326, i32 0, i32 0
  %1171 = getelementptr [2 x i32], ptr %1169, i32 0, i32 1
  %1172 = load i32, ptr %1171
  store i32 %1172, ptr %1170
  %1173 = getelementptr [2 x i32], ptr %stack.ptr_326, i32 0, i32 1
  store i32 0, ptr %1173
  %1174 = getelementptr [2 x i32], ptr %stack.ptr_327, i32 0, i32 0
  %1175 = getelementptr [2 x i32], ptr %1169, i32 0, i32 1
  %1176 = load i32, ptr %1175
  store i32 %1176, ptr %1174
  %1177 = getelementptr [2 x i32], ptr %stack.ptr_327, i32 0, i32 1
  store i32 4294967295, ptr %1177
  %1178 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1178, ptr %stack.ptr_326, ptr %stack.ptr_328)
  %1179 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1179, ptr %stack.ptr_327, ptr %stack.ptr_329)
  br label %loop_72
loop_72:
  %1180 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_328, ptr %stack.ptr_329)
  br i1 %1180, label %if_83, label %end_if_83
if_83:
  br label %range_query.end_71
end_if_83:
  %1181 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_328)
  %1182 = getelementptr [2 x i32], ptr %stack.ptr_330, i32 0, i32 0
  %1183 = getelementptr [2 x i32], ptr %1169, i32 0, i32 1
  %1184 = load i32, ptr %1183
  store i32 %1184, ptr %1182
  %1185 = getelementptr [2 x i32], ptr %stack.ptr_330, i32 0, i32 1
  store i32 0, ptr %1185
  %1186 = getelementptr [2 x i32], ptr %stack.ptr_331, i32 0, i32 0
  %1187 = getelementptr [2 x i32], ptr %1169, i32 0, i32 1
  %1188 = load i32, ptr %1187
  store i32 %1188, ptr %1186
  %1189 = getelementptr [2 x i32], ptr %stack.ptr_331, i32 0, i32 1
  store i32 4294967295, ptr %1189
  %1190 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %1190, ptr %stack.ptr_330, ptr %stack.ptr_332)
  %1191 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %1191, ptr %stack.ptr_331, ptr %stack.ptr_333)
  br label %loop_73
loop_73:
  %1192 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_332, ptr %stack.ptr_333)
  br i1 %1192, label %if_84, label %end_if_84
if_84:
  br label %range_query.end_72
end_if_84:
  %1193 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_332)
  %1194 = getelementptr [2 x i32], ptr %stack.ptr_334, i32 0, i32 0
  %1195 = getelementptr [2 x i32], ptr %1193, i32 0, i32 1
  %1196 = load i32, ptr %1195
  store i32 %1196, ptr %1194
  %1197 = getelementptr [2 x i32], ptr %stack.ptr_334, i32 0, i32 1
  store i32 0, ptr %1197
  %1198 = getelementptr [2 x i32], ptr %stack.ptr_335, i32 0, i32 0
  %1199 = getelementptr [2 x i32], ptr %1193, i32 0, i32 1
  %1200 = load i32, ptr %1199
  store i32 %1200, ptr %1198
  %1201 = getelementptr [2 x i32], ptr %stack.ptr_335, i32 0, i32 1
  store i32 4294967295, ptr %1201
  %1202 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %1202, ptr %stack.ptr_334, ptr %stack.ptr_336)
  %1203 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %1203, ptr %stack.ptr_335, ptr %stack.ptr_337)
  br label %loop_74
loop_74:
  %1204 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_336, ptr %stack.ptr_337)
  br i1 %1204, label %if_85, label %end_if_85
if_85:
  br label %range_query.end_73
end_if_85:
  %1205 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_336)
  %1206 = getelementptr [2 x i32], ptr %1205, i32 0, i32 1
  %1207 = load i32, ptr %1206
  %1208 = icmp ne i32 %1207, 54
  br i1 %1208, label %if_86, label %end_if_86
if_86:
  %1209 = getelementptr [3 x i32], ptr %stack.ptr_338, i32 0, i32 0
  %1210 = getelementptr [2 x i32], ptr %1169, i32 0, i32 1
  %1211 = load i32, ptr %1210
  store i32 %1211, ptr %1209
  %1212 = getelementptr [3 x i32], ptr %stack.ptr_338, i32 0, i32 1
  %1213 = getelementptr [2 x i32], ptr %1193, i32 0, i32 1
  %1214 = load i32, ptr %1213
  store i32 %1214, ptr %1212
  %1215 = getelementptr [3 x i32], ptr %stack.ptr_338, i32 0, i32 2
  %1216 = getelementptr [2 x i32], ptr %1205, i32 0, i32 1
  %1217 = load i32, ptr %1216
  store i32 %1217, ptr %1215
  %1218 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %1219 = call ccc i1 @eclair_btree_insert_value_0(ptr %1218, ptr %stack.ptr_338)
  br label %end_if_86
end_if_86:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_336)
  br label %loop_74
range_query.end_73:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_332)
  br label %loop_73
range_query.end_72:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_328)
  br label %loop_72
range_query.end_71:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_324)
  br label %loop_71
range_query.end_70:
  %1220 = getelementptr [2 x i32], ptr %stack.ptr_339, i32 0, i32 0
  store i32 0, ptr %1220
  %1221 = getelementptr [2 x i32], ptr %stack.ptr_339, i32 0, i32 1
  store i32 0, ptr %1221
  %1222 = getelementptr [2 x i32], ptr %stack.ptr_340, i32 0, i32 0
  store i32 4294967295, ptr %1222
  %1223 = getelementptr [2 x i32], ptr %stack.ptr_340, i32 0, i32 1
  store i32 4294967295, ptr %1223
  %1224 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1224, ptr %stack.ptr_339, ptr %stack.ptr_341)
  %1225 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1225, ptr %stack.ptr_340, ptr %stack.ptr_342)
  br label %loop_75
loop_75:
  %1226 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_341, ptr %stack.ptr_342)
  br i1 %1226, label %if_87, label %end_if_87
if_87:
  br label %range_query.end_74
end_if_87:
  %1227 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_341)
  %1228 = getelementptr [2 x i32], ptr %stack.ptr_343, i32 0, i32 0
  %1229 = getelementptr [2 x i32], ptr %1227, i32 0, i32 1
  %1230 = load i32, ptr %1229
  store i32 %1230, ptr %1228
  %1231 = getelementptr [2 x i32], ptr %stack.ptr_343, i32 0, i32 1
  store i32 0, ptr %1231
  %1232 = getelementptr [2 x i32], ptr %stack.ptr_344, i32 0, i32 0
  %1233 = getelementptr [2 x i32], ptr %1227, i32 0, i32 1
  %1234 = load i32, ptr %1233
  store i32 %1234, ptr %1232
  %1235 = getelementptr [2 x i32], ptr %stack.ptr_344, i32 0, i32 1
  store i32 4294967295, ptr %1235
  %1236 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_lower_bound_1(ptr %1236, ptr %stack.ptr_343, ptr %stack.ptr_345)
  %1237 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_upper_bound_1(ptr %1237, ptr %stack.ptr_344, ptr %stack.ptr_346)
  br label %loop_76
loop_76:
  %1238 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_345, ptr %stack.ptr_346)
  br i1 %1238, label %if_88, label %end_if_88
if_88:
  br label %range_query.end_75
end_if_88:
  %1239 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_345)
  %1240 = getelementptr [1 x i32], ptr %stack.ptr_347, i32 0, i32 0
  %1241 = getelementptr [2 x i32], ptr %1239, i32 0, i32 1
  %1242 = load i32, ptr %1241
  store i32 %1242, ptr %1240
  %1243 = getelementptr [1 x i32], ptr %stack.ptr_348, i32 0, i32 0
  %1244 = getelementptr [2 x i32], ptr %1239, i32 0, i32 1
  %1245 = load i32, ptr %1244
  store i32 %1245, ptr %1243
  %1246 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_lower_bound_6(ptr %1246, ptr %stack.ptr_347, ptr %stack.ptr_349)
  %1247 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_upper_bound_6(ptr %1247, ptr %stack.ptr_348, ptr %stack.ptr_350)
  br label %loop_77
loop_77:
  %1248 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_349, ptr %stack.ptr_350)
  br i1 %1248, label %if_89, label %end_if_89
if_89:
  br label %range_query.end_76
end_if_89:
  %1249 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_349)
  %1250 = getelementptr [1 x i32], ptr %stack.ptr_351, i32 0, i32 0
  %1251 = getelementptr [2 x i32], ptr %1227, i32 0, i32 0
  %1252 = load i32, ptr %1251
  store i32 %1252, ptr %1250
  %1253 = getelementptr %program, ptr %arg_0, i32 0, i32 30
  %1254 = call ccc i1 @eclair_btree_insert_value_6(ptr %1253, ptr %stack.ptr_351)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_349)
  br label %loop_77
range_query.end_76:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_345)
  br label %loop_76
range_query.end_75:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_341)
  br label %loop_75
range_query.end_74:
  %1255 = getelementptr [2 x i32], ptr %stack.ptr_352, i32 0, i32 0
  store i32 0, ptr %1255
  %1256 = getelementptr [2 x i32], ptr %stack.ptr_352, i32 0, i32 1
  store i32 0, ptr %1256
  %1257 = getelementptr [2 x i32], ptr %stack.ptr_353, i32 0, i32 0
  store i32 4294967295, ptr %1257
  %1258 = getelementptr [2 x i32], ptr %stack.ptr_353, i32 0, i32 1
  store i32 4294967295, ptr %1258
  %1259 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1259, ptr %stack.ptr_352, ptr %stack.ptr_354)
  %1260 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1260, ptr %stack.ptr_353, ptr %stack.ptr_355)
  br label %loop_78
loop_78:
  %1261 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_354, ptr %stack.ptr_355)
  br i1 %1261, label %if_90, label %end_if_90
if_90:
  br label %range_query.end_77
end_if_90:
  %1262 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_354)
  %1263 = getelementptr [1 x i32], ptr %stack.ptr_356, i32 0, i32 0
  %1264 = getelementptr [2 x i32], ptr %1262, i32 0, i32 0
  %1265 = load i32, ptr %1264
  store i32 %1265, ptr %1263
  %1266 = getelementptr %program, ptr %arg_0, i32 0, i32 30
  %1267 = call ccc i1 @eclair_btree_contains_6(ptr %1266, ptr %stack.ptr_356)
  %1268 = select i1 %1267, i1 0, i1 1
  br i1 %1268, label %if_91, label %end_if_91
if_91:
  %1269 = getelementptr [1 x i32], ptr %stack.ptr_357, i32 0, i32 0
  %1270 = getelementptr [2 x i32], ptr %1262, i32 0, i32 0
  %1271 = load i32, ptr %1270
  store i32 %1271, ptr %1269
  %1272 = getelementptr %program, ptr %arg_0, i32 0, i32 46
  %1273 = call ccc i1 @eclair_btree_insert_value_6(ptr %1272, ptr %stack.ptr_357)
  br label %end_if_91
end_if_91:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_354)
  br label %loop_78
range_query.end_77:
  %1274 = getelementptr [2 x i32], ptr %stack.ptr_358, i32 0, i32 0
  store i32 0, ptr %1274
  %1275 = getelementptr [2 x i32], ptr %stack.ptr_358, i32 0, i32 1
  store i32 0, ptr %1275
  %1276 = getelementptr [2 x i32], ptr %stack.ptr_359, i32 0, i32 0
  store i32 4294967295, ptr %1276
  %1277 = getelementptr [2 x i32], ptr %stack.ptr_359, i32 0, i32 1
  store i32 4294967295, ptr %1277
  %1278 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1278, ptr %stack.ptr_358, ptr %stack.ptr_360)
  %1279 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1279, ptr %stack.ptr_359, ptr %stack.ptr_361)
  br label %loop_79
loop_79:
  %1280 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_360, ptr %stack.ptr_361)
  br i1 %1280, label %if_92, label %end_if_92
if_92:
  br label %range_query.end_78
end_if_92:
  %1281 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_360)
  %1282 = getelementptr [2 x i32], ptr %stack.ptr_362, i32 0, i32 0
  %1283 = getelementptr [2 x i32], ptr %1281, i32 0, i32 1
  %1284 = load i32, ptr %1283
  store i32 %1284, ptr %1282
  %1285 = getelementptr [2 x i32], ptr %stack.ptr_362, i32 0, i32 1
  store i32 0, ptr %1285
  %1286 = getelementptr [2 x i32], ptr %stack.ptr_363, i32 0, i32 0
  %1287 = getelementptr [2 x i32], ptr %1281, i32 0, i32 1
  %1288 = load i32, ptr %1287
  store i32 %1288, ptr %1286
  %1289 = getelementptr [2 x i32], ptr %stack.ptr_363, i32 0, i32 1
  store i32 4294967295, ptr %1289
  %1290 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1290, ptr %stack.ptr_362, ptr %stack.ptr_364)
  %1291 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1291, ptr %stack.ptr_363, ptr %stack.ptr_365)
  br label %loop_80
loop_80:
  %1292 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_364, ptr %stack.ptr_365)
  br i1 %1292, label %if_93, label %end_if_93
if_93:
  br label %range_query.end_79
end_if_93:
  %1293 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_364)
  %1294 = getelementptr [1 x i32], ptr %stack.ptr_366, i32 0, i32 0
  %1295 = getelementptr [2 x i32], ptr %1293, i32 0, i32 1
  %1296 = load i32, ptr %1295
  store i32 %1296, ptr %1294
  %1297 = getelementptr %program, ptr %arg_0, i32 0, i32 29
  %1298 = call ccc i1 @eclair_btree_insert_value_6(ptr %1297, ptr %stack.ptr_366)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_364)
  br label %loop_80
range_query.end_79:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_360)
  br label %loop_79
range_query.end_78:
  %1299 = getelementptr [2 x i32], ptr %stack.ptr_367, i32 0, i32 0
  store i32 0, ptr %1299
  %1300 = getelementptr [2 x i32], ptr %stack.ptr_367, i32 0, i32 1
  store i32 0, ptr %1300
  %1301 = getelementptr [2 x i32], ptr %stack.ptr_368, i32 0, i32 0
  store i32 4294967295, ptr %1301
  %1302 = getelementptr [2 x i32], ptr %stack.ptr_368, i32 0, i32 1
  store i32 4294967295, ptr %1302
  %1303 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1303, ptr %stack.ptr_367, ptr %stack.ptr_369)
  %1304 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1304, ptr %stack.ptr_368, ptr %stack.ptr_370)
  br label %loop_81
loop_81:
  %1305 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_369, ptr %stack.ptr_370)
  br i1 %1305, label %if_94, label %end_if_94
if_94:
  br label %range_query.end_80
end_if_94:
  %1306 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_369)
  %1307 = getelementptr [2 x i32], ptr %stack.ptr_371, i32 0, i32 0
  %1308 = getelementptr [2 x i32], ptr %1306, i32 0, i32 1
  %1309 = load i32, ptr %1308
  store i32 %1309, ptr %1307
  %1310 = getelementptr [2 x i32], ptr %stack.ptr_371, i32 0, i32 1
  store i32 0, ptr %1310
  %1311 = getelementptr [2 x i32], ptr %stack.ptr_372, i32 0, i32 0
  %1312 = getelementptr [2 x i32], ptr %1306, i32 0, i32 1
  %1313 = load i32, ptr %1312
  store i32 %1313, ptr %1311
  %1314 = getelementptr [2 x i32], ptr %stack.ptr_372, i32 0, i32 1
  store i32 4294967295, ptr %1314
  %1315 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1315, ptr %stack.ptr_371, ptr %stack.ptr_373)
  %1316 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1316, ptr %stack.ptr_372, ptr %stack.ptr_374)
  br label %loop_82
loop_82:
  %1317 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_373, ptr %stack.ptr_374)
  br i1 %1317, label %if_95, label %end_if_95
if_95:
  br label %range_query.end_81
end_if_95:
  %1318 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_373)
  %1319 = getelementptr [1 x i32], ptr %stack.ptr_375, i32 0, i32 0
  %1320 = getelementptr [2 x i32], ptr %1318, i32 0, i32 1
  %1321 = load i32, ptr %1320
  store i32 %1321, ptr %1319
  %1322 = getelementptr %program, ptr %arg_0, i32 0, i32 29
  %1323 = call ccc i1 @eclair_btree_insert_value_6(ptr %1322, ptr %stack.ptr_375)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_373)
  br label %loop_82
range_query.end_81:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_369)
  br label %loop_81
range_query.end_80:
  %1324 = getelementptr [1 x i32], ptr %stack.ptr_376, i32 0, i32 0
  store i32 0, ptr %1324
  %1325 = getelementptr [1 x i32], ptr %stack.ptr_377, i32 0, i32 0
  store i32 4294967295, ptr %1325
  %1326 = getelementptr %program, ptr %arg_0, i32 0, i32 33
  call ccc void @eclair_btree_lower_bound_6(ptr %1326, ptr %stack.ptr_376, ptr %stack.ptr_378)
  %1327 = getelementptr %program, ptr %arg_0, i32 0, i32 33
  call ccc void @eclair_btree_upper_bound_6(ptr %1327, ptr %stack.ptr_377, ptr %stack.ptr_379)
  br label %loop_83
loop_83:
  %1328 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_378, ptr %stack.ptr_379)
  br i1 %1328, label %if_96, label %end_if_96
if_96:
  br label %range_query.end_82
end_if_96:
  %1329 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_378)
  %1330 = getelementptr [1 x i32], ptr %stack.ptr_380, i32 0, i32 0
  %1331 = getelementptr [1 x i32], ptr %1329, i32 0, i32 0
  %1332 = load i32, ptr %1331
  store i32 %1332, ptr %1330
  %1333 = getelementptr %program, ptr %arg_0, i32 0, i32 29
  %1334 = call ccc i1 @eclair_btree_contains_6(ptr %1333, ptr %stack.ptr_380)
  %1335 = select i1 %1334, i1 0, i1 1
  br i1 %1335, label %if_97, label %end_if_98
if_97:
  %1336 = getelementptr [2 x i32], ptr %stack.ptr_381, i32 0, i32 0
  store i32 0, ptr %1336
  %1337 = getelementptr [2 x i32], ptr %stack.ptr_381, i32 0, i32 1
  %1338 = getelementptr [1 x i32], ptr %1329, i32 0, i32 0
  %1339 = load i32, ptr %1338
  store i32 %1339, ptr %1337
  %1340 = getelementptr [2 x i32], ptr %stack.ptr_382, i32 0, i32 0
  store i32 4294967295, ptr %1340
  %1341 = getelementptr [2 x i32], ptr %stack.ptr_382, i32 0, i32 1
  %1342 = getelementptr [1 x i32], ptr %1329, i32 0, i32 0
  %1343 = load i32, ptr %1342
  store i32 %1343, ptr %1341
  %1344 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_lower_bound_2(ptr %1344, ptr %stack.ptr_381, ptr %stack.ptr_383)
  %1345 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_upper_bound_2(ptr %1345, ptr %stack.ptr_382, ptr %stack.ptr_384)
  br label %loop_84
loop_84:
  %1346 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_383, ptr %stack.ptr_384)
  br i1 %1346, label %if_98, label %end_if_97
if_98:
  br label %range_query.end_83
end_if_97:
  %1347 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_383)
  %1348 = getelementptr [2 x i32], ptr %stack.ptr_385, i32 0, i32 0
  %1349 = getelementptr [2 x i32], ptr %1347, i32 0, i32 0
  %1350 = load i32, ptr %1349
  store i32 %1350, ptr %1348
  %1351 = getelementptr [2 x i32], ptr %stack.ptr_385, i32 0, i32 1
  %1352 = getelementptr [1 x i32], ptr %1329, i32 0, i32 0
  %1353 = load i32, ptr %1352
  store i32 %1353, ptr %1351
  %1354 = getelementptr %program, ptr %arg_0, i32 0, i32 13
  %1355 = call ccc i1 @eclair_btree_insert_value_1(ptr %1354, ptr %stack.ptr_385)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_383)
  br label %loop_84
range_query.end_83:
  br label %end_if_98
end_if_98:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_378)
  br label %loop_83
range_query.end_82:
  %1356 = getelementptr [1 x i32], ptr %stack.ptr_386, i32 0, i32 0
  store i32 0, ptr %1356
  %1357 = getelementptr [1 x i32], ptr %stack.ptr_387, i32 0, i32 0
  store i32 4294967295, ptr %1357
  %1358 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  call ccc void @eclair_btree_lower_bound_6(ptr %1358, ptr %stack.ptr_386, ptr %stack.ptr_388)
  %1359 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  call ccc void @eclair_btree_upper_bound_6(ptr %1359, ptr %stack.ptr_387, ptr %stack.ptr_389)
  br label %loop_85
loop_85:
  %1360 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_388, ptr %stack.ptr_389)
  br i1 %1360, label %if_99, label %end_if_99
if_99:
  br label %range_query.end_84
end_if_99:
  %1361 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_388)
  %1362 = getelementptr [1 x i32], ptr %stack.ptr_390, i32 0, i32 0
  %1363 = getelementptr [1 x i32], ptr %1361, i32 0, i32 0
  %1364 = load i32, ptr %1363
  store i32 %1364, ptr %1362
  %1365 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1366 = call ccc i1 @eclair_btree_insert_value_6(ptr %1365, ptr %stack.ptr_390)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_388)
  br label %loop_85
range_query.end_84:
  %1367 = getelementptr [2 x i32], ptr %stack.ptr_391, i32 0, i32 0
  store i32 0, ptr %1367
  %1368 = getelementptr [2 x i32], ptr %stack.ptr_391, i32 0, i32 1
  store i32 0, ptr %1368
  %1369 = getelementptr [2 x i32], ptr %stack.ptr_392, i32 0, i32 0
  store i32 4294967295, ptr %1369
  %1370 = getelementptr [2 x i32], ptr %stack.ptr_392, i32 0, i32 1
  store i32 4294967295, ptr %1370
  %1371 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1371, ptr %stack.ptr_391, ptr %stack.ptr_393)
  %1372 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1372, ptr %stack.ptr_392, ptr %stack.ptr_394)
  br label %loop_86
loop_86:
  %1373 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_393, ptr %stack.ptr_394)
  br i1 %1373, label %if_100, label %end_if_100
if_100:
  br label %range_query.end_85
end_if_100:
  %1374 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_393)
  %1375 = getelementptr [2 x i32], ptr %stack.ptr_395, i32 0, i32 0
  %1376 = getelementptr [2 x i32], ptr %1374, i32 0, i32 1
  %1377 = load i32, ptr %1376
  store i32 %1377, ptr %1375
  %1378 = getelementptr [2 x i32], ptr %stack.ptr_395, i32 0, i32 1
  store i32 0, ptr %1378
  %1379 = getelementptr [2 x i32], ptr %stack.ptr_396, i32 0, i32 0
  %1380 = getelementptr [2 x i32], ptr %1374, i32 0, i32 1
  %1381 = load i32, ptr %1380
  store i32 %1381, ptr %1379
  %1382 = getelementptr [2 x i32], ptr %stack.ptr_396, i32 0, i32 1
  store i32 4294967295, ptr %1382
  %1383 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1383, ptr %stack.ptr_395, ptr %stack.ptr_397)
  %1384 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1384, ptr %stack.ptr_396, ptr %stack.ptr_398)
  br label %loop_87
loop_87:
  %1385 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_397, ptr %stack.ptr_398)
  br i1 %1385, label %if_101, label %end_if_101
if_101:
  br label %range_query.end_86
end_if_101:
  %1386 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_397)
  %1387 = getelementptr [1 x i32], ptr %stack.ptr_399, i32 0, i32 0
  %1388 = getelementptr [2 x i32], ptr %1386, i32 0, i32 1
  %1389 = load i32, ptr %1388
  store i32 %1389, ptr %1387
  %1390 = getelementptr %program, ptr %arg_0, i32 0, i32 32
  %1391 = call ccc i1 @eclair_btree_contains_6(ptr %1390, ptr %stack.ptr_399)
  %1392 = select i1 %1391, i1 0, i1 1
  br i1 %1392, label %if_102, label %end_if_102
if_102:
  %1393 = getelementptr [1 x i32], ptr %stack.ptr_400, i32 0, i32 0
  %1394 = getelementptr [2 x i32], ptr %1386, i32 0, i32 1
  %1395 = load i32, ptr %1394
  store i32 %1395, ptr %1393
  %1396 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  %1397 = call ccc i1 @eclair_btree_insert_value_6(ptr %1396, ptr %stack.ptr_400)
  br label %end_if_102
end_if_102:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_397)
  br label %loop_87
range_query.end_86:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_393)
  br label %loop_86
range_query.end_85:
  %1398 = getelementptr [1 x i32], ptr %stack.ptr_401, i32 0, i32 0
  store i32 0, ptr %1398
  %1399 = getelementptr [1 x i32], ptr %stack.ptr_402, i32 0, i32 0
  store i32 4294967295, ptr %1399
  %1400 = getelementptr %program, ptr %arg_0, i32 0, i32 32
  call ccc void @eclair_btree_lower_bound_6(ptr %1400, ptr %stack.ptr_401, ptr %stack.ptr_403)
  %1401 = getelementptr %program, ptr %arg_0, i32 0, i32 32
  call ccc void @eclair_btree_upper_bound_6(ptr %1401, ptr %stack.ptr_402, ptr %stack.ptr_404)
  br label %loop_88
loop_88:
  %1402 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_403, ptr %stack.ptr_404)
  br i1 %1402, label %if_103, label %end_if_103
if_103:
  br label %range_query.end_87
end_if_103:
  %1403 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_403)
  %1404 = getelementptr [1 x i32], ptr %stack.ptr_405, i32 0, i32 0
  %1405 = getelementptr [1 x i32], ptr %1403, i32 0, i32 0
  %1406 = load i32, ptr %1405
  store i32 %1406, ptr %1404
  %1407 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  %1408 = call ccc i1 @eclair_btree_insert_value_6(ptr %1407, ptr %stack.ptr_405)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_403)
  br label %loop_88
range_query.end_87:
  %1409 = getelementptr [1 x i32], ptr %stack.ptr_406, i32 0, i32 0
  store i32 0, ptr %1409
  %1410 = getelementptr [1 x i32], ptr %stack.ptr_407, i32 0, i32 0
  store i32 4294967295, ptr %1410
  %1411 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_lower_bound_6(ptr %1411, ptr %stack.ptr_406, ptr %stack.ptr_408)
  %1412 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_upper_bound_6(ptr %1412, ptr %stack.ptr_407, ptr %stack.ptr_409)
  br label %loop_89
loop_89:
  %1413 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_408, ptr %stack.ptr_409)
  br i1 %1413, label %if_104, label %end_if_104
if_104:
  br label %range_query.end_88
end_if_104:
  %1414 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_408)
  %1415 = getelementptr [1 x i32], ptr %stack.ptr_410, i32 0, i32 0
  %1416 = getelementptr [1 x i32], ptr %1414, i32 0, i32 0
  %1417 = load i32, ptr %1416
  store i32 %1417, ptr %1415
  %1418 = getelementptr [1 x i32], ptr %stack.ptr_411, i32 0, i32 0
  %1419 = getelementptr [1 x i32], ptr %1414, i32 0, i32 0
  %1420 = load i32, ptr %1419
  store i32 %1420, ptr %1418
  %1421 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_lower_bound_6(ptr %1421, ptr %stack.ptr_410, ptr %stack.ptr_412)
  %1422 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_upper_bound_6(ptr %1422, ptr %stack.ptr_411, ptr %stack.ptr_413)
  br label %loop_90
loop_90:
  %1423 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_412, ptr %stack.ptr_413)
  br i1 %1423, label %if_105, label %end_if_105
if_105:
  br label %range_query.end_89
end_if_105:
  %1424 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_412)
  %1425 = getelementptr [1 x i32], ptr %stack.ptr_414, i32 0, i32 0
  %1426 = getelementptr [1 x i32], ptr %1414, i32 0, i32 0
  %1427 = load i32, ptr %1426
  store i32 %1427, ptr %1425
  %1428 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1429 = call ccc i1 @eclair_btree_insert_value_6(ptr %1428, ptr %stack.ptr_414)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_412)
  br label %loop_90
range_query.end_89:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_408)
  br label %loop_89
range_query.end_88:
  %1430 = getelementptr [2 x i32], ptr %stack.ptr_415, i32 0, i32 0
  store i32 0, ptr %1430
  %1431 = getelementptr [2 x i32], ptr %stack.ptr_415, i32 0, i32 1
  store i32 0, ptr %1431
  %1432 = getelementptr [2 x i32], ptr %stack.ptr_416, i32 0, i32 0
  store i32 4294967295, ptr %1432
  %1433 = getelementptr [2 x i32], ptr %stack.ptr_416, i32 0, i32 1
  store i32 4294967295, ptr %1433
  %1434 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1434, ptr %stack.ptr_415, ptr %stack.ptr_417)
  %1435 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1435, ptr %stack.ptr_416, ptr %stack.ptr_418)
  br label %loop_91
loop_91:
  %1436 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_417, ptr %stack.ptr_418)
  br i1 %1436, label %if_106, label %end_if_106
if_106:
  br label %range_query.end_90
end_if_106:
  %1437 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_417)
  %1438 = getelementptr [3 x i32], ptr %stack.ptr_419, i32 0, i32 0
  %1439 = getelementptr [2 x i32], ptr %1437, i32 0, i32 0
  %1440 = load i32, ptr %1439
  store i32 %1440, ptr %1438
  %1441 = getelementptr [3 x i32], ptr %stack.ptr_419, i32 0, i32 1
  store i32 0, ptr %1441
  %1442 = getelementptr [3 x i32], ptr %stack.ptr_419, i32 0, i32 2
  store i32 0, ptr %1442
  %1443 = getelementptr [3 x i32], ptr %stack.ptr_420, i32 0, i32 0
  %1444 = getelementptr [2 x i32], ptr %1437, i32 0, i32 0
  %1445 = load i32, ptr %1444
  store i32 %1445, ptr %1443
  %1446 = getelementptr [3 x i32], ptr %stack.ptr_420, i32 0, i32 1
  store i32 4294967295, ptr %1446
  %1447 = getelementptr [3 x i32], ptr %stack.ptr_420, i32 0, i32 2
  store i32 4294967295, ptr %1447
  %1448 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1448, ptr %stack.ptr_419, ptr %stack.ptr_421)
  %1449 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1449, ptr %stack.ptr_420, ptr %stack.ptr_422)
  br label %loop_92
loop_92:
  %1450 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_421, ptr %stack.ptr_422)
  br i1 %1450, label %if_107, label %end_if_107
if_107:
  br label %range_query.end_91
end_if_107:
  %1451 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_421)
  %1452 = getelementptr [2 x i32], ptr %stack.ptr_423, i32 0, i32 0
  %1453 = getelementptr [3 x i32], ptr %1451, i32 0, i32 2
  %1454 = load i32, ptr %1453
  store i32 %1454, ptr %1452
  %1455 = getelementptr [2 x i32], ptr %stack.ptr_423, i32 0, i32 1
  store i32 0, ptr %1455
  %1456 = getelementptr [2 x i32], ptr %stack.ptr_424, i32 0, i32 0
  %1457 = getelementptr [3 x i32], ptr %1451, i32 0, i32 2
  %1458 = load i32, ptr %1457
  store i32 %1458, ptr %1456
  %1459 = getelementptr [2 x i32], ptr %stack.ptr_424, i32 0, i32 1
  store i32 4294967295, ptr %1459
  %1460 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %1460, ptr %stack.ptr_423, ptr %stack.ptr_425)
  %1461 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %1461, ptr %stack.ptr_424, ptr %stack.ptr_426)
  br label %loop_93
loop_93:
  %1462 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_425, ptr %stack.ptr_426)
  br i1 %1462, label %if_108, label %end_if_108
if_108:
  br label %range_query.end_92
end_if_108:
  %1463 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_425)
  %1464 = getelementptr [2 x i32], ptr %stack.ptr_427, i32 0, i32 0
  %1465 = getelementptr [2 x i32], ptr %1463, i32 0, i32 1
  %1466 = load i32, ptr %1465
  store i32 %1466, ptr %1464
  %1467 = getelementptr [2 x i32], ptr %stack.ptr_427, i32 0, i32 1
  store i32 0, ptr %1467
  %1468 = getelementptr [2 x i32], ptr %stack.ptr_428, i32 0, i32 0
  %1469 = getelementptr [2 x i32], ptr %1463, i32 0, i32 1
  %1470 = load i32, ptr %1469
  store i32 %1470, ptr %1468
  %1471 = getelementptr [2 x i32], ptr %stack.ptr_428, i32 0, i32 1
  store i32 4294967295, ptr %1471
  %1472 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1472, ptr %stack.ptr_427, ptr %stack.ptr_429)
  %1473 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1473, ptr %stack.ptr_428, ptr %stack.ptr_430)
  br label %loop_94
loop_94:
  %1474 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_429, ptr %stack.ptr_430)
  br i1 %1474, label %if_109, label %end_if_109
if_109:
  br label %range_query.end_93
end_if_109:
  %1475 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_429)
  %1476 = getelementptr [2 x i32], ptr %stack.ptr_431, i32 0, i32 0
  %1477 = getelementptr [2 x i32], ptr %1437, i32 0, i32 1
  %1478 = load i32, ptr %1477
  store i32 %1478, ptr %1476
  %1479 = getelementptr [2 x i32], ptr %stack.ptr_431, i32 0, i32 1
  %1480 = getelementptr [2 x i32], ptr %1475, i32 0, i32 1
  %1481 = load i32, ptr %1480
  store i32 %1481, ptr %1479
  %1482 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  %1483 = call ccc i1 @eclair_btree_insert_value_1(ptr %1482, ptr %stack.ptr_431)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_429)
  br label %loop_94
range_query.end_93:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_425)
  br label %loop_93
range_query.end_92:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_421)
  br label %loop_92
range_query.end_91:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_417)
  br label %loop_91
range_query.end_90:
  %1484 = getelementptr [2 x i32], ptr %stack.ptr_432, i32 0, i32 0
  store i32 0, ptr %1484
  %1485 = getelementptr [2 x i32], ptr %stack.ptr_432, i32 0, i32 1
  store i32 0, ptr %1485
  %1486 = getelementptr [2 x i32], ptr %stack.ptr_433, i32 0, i32 0
  store i32 4294967295, ptr %1486
  %1487 = getelementptr [2 x i32], ptr %stack.ptr_433, i32 0, i32 1
  store i32 4294967295, ptr %1487
  %1488 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1488, ptr %stack.ptr_432, ptr %stack.ptr_434)
  %1489 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1489, ptr %stack.ptr_433, ptr %stack.ptr_435)
  br label %loop_95
loop_95:
  %1490 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_434, ptr %stack.ptr_435)
  br i1 %1490, label %if_110, label %end_if_110
if_110:
  br label %range_query.end_94
end_if_110:
  %1491 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_434)
  %1492 = getelementptr [3 x i32], ptr %stack.ptr_436, i32 0, i32 0
  %1493 = getelementptr [2 x i32], ptr %1491, i32 0, i32 0
  %1494 = load i32, ptr %1493
  store i32 %1494, ptr %1492
  %1495 = getelementptr [3 x i32], ptr %stack.ptr_436, i32 0, i32 1
  store i32 0, ptr %1495
  %1496 = getelementptr [3 x i32], ptr %stack.ptr_436, i32 0, i32 2
  store i32 0, ptr %1496
  %1497 = getelementptr [3 x i32], ptr %stack.ptr_437, i32 0, i32 0
  %1498 = getelementptr [2 x i32], ptr %1491, i32 0, i32 0
  %1499 = load i32, ptr %1498
  store i32 %1499, ptr %1497
  %1500 = getelementptr [3 x i32], ptr %stack.ptr_437, i32 0, i32 1
  store i32 4294967295, ptr %1500
  %1501 = getelementptr [3 x i32], ptr %stack.ptr_437, i32 0, i32 2
  store i32 4294967295, ptr %1501
  %1502 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1502, ptr %stack.ptr_436, ptr %stack.ptr_438)
  %1503 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1503, ptr %stack.ptr_437, ptr %stack.ptr_439)
  br label %loop_96
loop_96:
  %1504 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_438, ptr %stack.ptr_439)
  br i1 %1504, label %if_111, label %end_if_111
if_111:
  br label %range_query.end_95
end_if_111:
  %1505 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_438)
  %1506 = getelementptr [2 x i32], ptr %stack.ptr_440, i32 0, i32 0
  %1507 = getelementptr [3 x i32], ptr %1505, i32 0, i32 2
  %1508 = load i32, ptr %1507
  store i32 %1508, ptr %1506
  %1509 = getelementptr [2 x i32], ptr %stack.ptr_440, i32 0, i32 1
  store i32 0, ptr %1509
  %1510 = getelementptr [2 x i32], ptr %stack.ptr_441, i32 0, i32 0
  %1511 = getelementptr [3 x i32], ptr %1505, i32 0, i32 2
  %1512 = load i32, ptr %1511
  store i32 %1512, ptr %1510
  %1513 = getelementptr [2 x i32], ptr %stack.ptr_441, i32 0, i32 1
  store i32 4294967295, ptr %1513
  %1514 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1514, ptr %stack.ptr_440, ptr %stack.ptr_442)
  %1515 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1515, ptr %stack.ptr_441, ptr %stack.ptr_443)
  br label %loop_97
loop_97:
  %1516 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_442, ptr %stack.ptr_443)
  br i1 %1516, label %if_112, label %end_if_112
if_112:
  br label %range_query.end_96
end_if_112:
  %1517 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_442)
  %1518 = getelementptr [2 x i32], ptr %stack.ptr_444, i32 0, i32 0
  %1519 = getelementptr [2 x i32], ptr %1491, i32 0, i32 1
  %1520 = load i32, ptr %1519
  store i32 %1520, ptr %1518
  %1521 = getelementptr [2 x i32], ptr %stack.ptr_444, i32 0, i32 1
  %1522 = getelementptr [2 x i32], ptr %1517, i32 0, i32 1
  %1523 = load i32, ptr %1522
  store i32 %1523, ptr %1521
  %1524 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  %1525 = call ccc i1 @eclair_btree_insert_value_1(ptr %1524, ptr %stack.ptr_444)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_442)
  br label %loop_97
range_query.end_96:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_438)
  br label %loop_96
range_query.end_95:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_434)
  br label %loop_95
range_query.end_94:
  %1526 = getelementptr [2 x i32], ptr %stack.ptr_445, i32 0, i32 0
  store i32 0, ptr %1526
  %1527 = getelementptr [2 x i32], ptr %stack.ptr_445, i32 0, i32 1
  store i32 0, ptr %1527
  %1528 = getelementptr [2 x i32], ptr %stack.ptr_446, i32 0, i32 0
  store i32 4294967295, ptr %1528
  %1529 = getelementptr [2 x i32], ptr %stack.ptr_446, i32 0, i32 1
  store i32 4294967295, ptr %1529
  %1530 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %1530, ptr %stack.ptr_445, ptr %stack.ptr_447)
  %1531 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %1531, ptr %stack.ptr_446, ptr %stack.ptr_448)
  br label %loop_98
loop_98:
  %1532 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_447, ptr %stack.ptr_448)
  br i1 %1532, label %if_113, label %end_if_113
if_113:
  br label %range_query.end_97
end_if_113:
  %1533 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_447)
  %1534 = getelementptr [2 x i32], ptr %stack.ptr_449, i32 0, i32 0
  store i32 0, ptr %1534
  %1535 = getelementptr [2 x i32], ptr %stack.ptr_449, i32 0, i32 1
  %1536 = getelementptr [2 x i32], ptr %1533, i32 0, i32 1
  %1537 = load i32, ptr %1536
  store i32 %1537, ptr %1535
  %1538 = getelementptr [2 x i32], ptr %stack.ptr_450, i32 0, i32 0
  store i32 4294967295, ptr %1538
  %1539 = getelementptr [2 x i32], ptr %stack.ptr_450, i32 0, i32 1
  %1540 = getelementptr [2 x i32], ptr %1533, i32 0, i32 1
  %1541 = load i32, ptr %1540
  store i32 %1541, ptr %1539
  %1542 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_lower_bound_2(ptr %1542, ptr %stack.ptr_449, ptr %stack.ptr_451)
  %1543 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_upper_bound_2(ptr %1543, ptr %stack.ptr_450, ptr %stack.ptr_452)
  br label %loop_99
loop_99:
  %1544 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_451, ptr %stack.ptr_452)
  br i1 %1544, label %if_114, label %end_if_114
if_114:
  br label %range_query.end_98
end_if_114:
  %1545 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_451)
  %1546 = getelementptr [2 x i32], ptr %stack.ptr_453, i32 0, i32 0
  %1547 = getelementptr [2 x i32], ptr %1545, i32 0, i32 0
  %1548 = load i32, ptr %1547
  store i32 %1548, ptr %1546
  %1549 = getelementptr [2 x i32], ptr %stack.ptr_453, i32 0, i32 1
  %1550 = getelementptr [2 x i32], ptr %1533, i32 0, i32 1
  %1551 = load i32, ptr %1550
  store i32 %1551, ptr %1549
  %1552 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  %1553 = call ccc i1 @eclair_btree_insert_value_1(ptr %1552, ptr %stack.ptr_453)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_451)
  br label %loop_99
range_query.end_98:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_447)
  br label %loop_98
range_query.end_97:
  %1554 = getelementptr [2 x i32], ptr %stack.ptr_454, i32 0, i32 0
  store i32 0, ptr %1554
  %1555 = getelementptr [2 x i32], ptr %stack.ptr_454, i32 0, i32 1
  store i32 0, ptr %1555
  %1556 = getelementptr [2 x i32], ptr %stack.ptr_455, i32 0, i32 0
  store i32 4294967295, ptr %1556
  %1557 = getelementptr [2 x i32], ptr %stack.ptr_455, i32 0, i32 1
  store i32 4294967295, ptr %1557
  %1558 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1558, ptr %stack.ptr_454, ptr %stack.ptr_456)
  %1559 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1559, ptr %stack.ptr_455, ptr %stack.ptr_457)
  br label %loop_100
loop_100:
  %1560 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_456, ptr %stack.ptr_457)
  br i1 %1560, label %if_115, label %end_if_115
if_115:
  br label %range_query.end_99
end_if_115:
  %1561 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_456)
  %1562 = getelementptr [3 x i32], ptr %stack.ptr_458, i32 0, i32 0
  %1563 = getelementptr [2 x i32], ptr %1561, i32 0, i32 0
  %1564 = load i32, ptr %1563
  store i32 %1564, ptr %1562
  %1565 = getelementptr [3 x i32], ptr %stack.ptr_458, i32 0, i32 1
  store i32 0, ptr %1565
  %1566 = getelementptr [3 x i32], ptr %stack.ptr_458, i32 0, i32 2
  store i32 0, ptr %1566
  %1567 = getelementptr [3 x i32], ptr %stack.ptr_459, i32 0, i32 0
  %1568 = getelementptr [2 x i32], ptr %1561, i32 0, i32 0
  %1569 = load i32, ptr %1568
  store i32 %1569, ptr %1567
  %1570 = getelementptr [3 x i32], ptr %stack.ptr_459, i32 0, i32 1
  store i32 4294967295, ptr %1570
  %1571 = getelementptr [3 x i32], ptr %stack.ptr_459, i32 0, i32 2
  store i32 4294967295, ptr %1571
  %1572 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1572, ptr %stack.ptr_458, ptr %stack.ptr_460)
  %1573 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1573, ptr %stack.ptr_459, ptr %stack.ptr_461)
  br label %loop_101
loop_101:
  %1574 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_460, ptr %stack.ptr_461)
  br i1 %1574, label %if_116, label %end_if_116
if_116:
  br label %range_query.end_100
end_if_116:
  %1575 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_460)
  %1576 = getelementptr [3 x i32], ptr %stack.ptr_462, i32 0, i32 0
  %1577 = getelementptr [3 x i32], ptr %1575, i32 0, i32 2
  %1578 = load i32, ptr %1577
  store i32 %1578, ptr %1576
  %1579 = getelementptr [3 x i32], ptr %stack.ptr_462, i32 0, i32 1
  store i32 0, ptr %1579
  %1580 = getelementptr [3 x i32], ptr %stack.ptr_462, i32 0, i32 2
  store i32 0, ptr %1580
  %1581 = getelementptr [3 x i32], ptr %stack.ptr_463, i32 0, i32 0
  %1582 = getelementptr [3 x i32], ptr %1575, i32 0, i32 2
  %1583 = load i32, ptr %1582
  store i32 %1583, ptr %1581
  %1584 = getelementptr [3 x i32], ptr %stack.ptr_463, i32 0, i32 1
  store i32 4294967295, ptr %1584
  %1585 = getelementptr [3 x i32], ptr %stack.ptr_463, i32 0, i32 2
  store i32 4294967295, ptr %1585
  %1586 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %1586, ptr %stack.ptr_462, ptr %stack.ptr_464)
  %1587 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %1587, ptr %stack.ptr_463, ptr %stack.ptr_465)
  br label %loop_102
loop_102:
  %1588 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_464, ptr %stack.ptr_465)
  br i1 %1588, label %if_117, label %end_if_117
if_117:
  br label %range_query.end_101
end_if_117:
  %1589 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_464)
  %1590 = getelementptr [2 x i32], ptr %stack.ptr_466, i32 0, i32 0
  %1591 = getelementptr [3 x i32], ptr %1589, i32 0, i32 1
  %1592 = load i32, ptr %1591
  store i32 %1592, ptr %1590
  %1593 = getelementptr [2 x i32], ptr %stack.ptr_466, i32 0, i32 1
  store i32 0, ptr %1593
  %1594 = getelementptr [2 x i32], ptr %stack.ptr_467, i32 0, i32 0
  %1595 = getelementptr [3 x i32], ptr %1589, i32 0, i32 1
  %1596 = load i32, ptr %1595
  store i32 %1596, ptr %1594
  %1597 = getelementptr [2 x i32], ptr %stack.ptr_467, i32 0, i32 1
  store i32 4294967295, ptr %1597
  %1598 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_lower_bound_1(ptr %1598, ptr %stack.ptr_466, ptr %stack.ptr_468)
  %1599 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_upper_bound_1(ptr %1599, ptr %stack.ptr_467, ptr %stack.ptr_469)
  br label %loop_103
loop_103:
  %1600 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_468, ptr %stack.ptr_469)
  br i1 %1600, label %if_118, label %end_if_118
if_118:
  br label %range_query.end_102
end_if_118:
  %1601 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_468)
  %1602 = getelementptr [2 x i32], ptr %stack.ptr_470, i32 0, i32 0
  %1603 = getelementptr [2 x i32], ptr %1561, i32 0, i32 1
  %1604 = load i32, ptr %1603
  store i32 %1604, ptr %1602
  %1605 = getelementptr [2 x i32], ptr %stack.ptr_470, i32 0, i32 1
  %1606 = getelementptr [2 x i32], ptr %1601, i32 0, i32 1
  %1607 = load i32, ptr %1606
  store i32 %1607, ptr %1605
  %1608 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  %1609 = call ccc i1 @eclair_btree_insert_value_1(ptr %1608, ptr %stack.ptr_470)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_468)
  br label %loop_103
range_query.end_102:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_464)
  br label %loop_102
range_query.end_101:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_460)
  br label %loop_101
range_query.end_100:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_456)
  br label %loop_100
range_query.end_99:
  %1610 = getelementptr [2 x i32], ptr %stack.ptr_471, i32 0, i32 0
  store i32 0, ptr %1610
  %1611 = getelementptr [2 x i32], ptr %stack.ptr_471, i32 0, i32 1
  store i32 0, ptr %1611
  %1612 = getelementptr [2 x i32], ptr %stack.ptr_472, i32 0, i32 0
  store i32 4294967295, ptr %1612
  %1613 = getelementptr [2 x i32], ptr %stack.ptr_472, i32 0, i32 1
  store i32 4294967295, ptr %1613
  %1614 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1614, ptr %stack.ptr_471, ptr %stack.ptr_473)
  %1615 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1615, ptr %stack.ptr_472, ptr %stack.ptr_474)
  br label %loop_104
loop_104:
  %1616 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_473, ptr %stack.ptr_474)
  br i1 %1616, label %if_119, label %end_if_119
if_119:
  br label %range_query.end_103
end_if_119:
  %1617 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_473)
  %1618 = getelementptr [3 x i32], ptr %stack.ptr_475, i32 0, i32 0
  %1619 = getelementptr [2 x i32], ptr %1617, i32 0, i32 0
  %1620 = load i32, ptr %1619
  store i32 %1620, ptr %1618
  %1621 = getelementptr [3 x i32], ptr %stack.ptr_475, i32 0, i32 1
  store i32 0, ptr %1621
  %1622 = getelementptr [3 x i32], ptr %stack.ptr_475, i32 0, i32 2
  store i32 0, ptr %1622
  %1623 = getelementptr [3 x i32], ptr %stack.ptr_476, i32 0, i32 0
  %1624 = getelementptr [2 x i32], ptr %1617, i32 0, i32 0
  %1625 = load i32, ptr %1624
  store i32 %1625, ptr %1623
  %1626 = getelementptr [3 x i32], ptr %stack.ptr_476, i32 0, i32 1
  store i32 4294967295, ptr %1626
  %1627 = getelementptr [3 x i32], ptr %stack.ptr_476, i32 0, i32 2
  store i32 4294967295, ptr %1627
  %1628 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1628, ptr %stack.ptr_475, ptr %stack.ptr_477)
  %1629 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1629, ptr %stack.ptr_476, ptr %stack.ptr_478)
  br label %loop_105
loop_105:
  %1630 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_477, ptr %stack.ptr_478)
  br i1 %1630, label %if_120, label %end_if_120
if_120:
  br label %range_query.end_104
end_if_120:
  %1631 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_477)
  %1632 = getelementptr [3 x i32], ptr %stack.ptr_479, i32 0, i32 0
  %1633 = getelementptr [3 x i32], ptr %1631, i32 0, i32 2
  %1634 = load i32, ptr %1633
  store i32 %1634, ptr %1632
  %1635 = getelementptr [3 x i32], ptr %stack.ptr_479, i32 0, i32 1
  store i32 0, ptr %1635
  %1636 = getelementptr [3 x i32], ptr %stack.ptr_479, i32 0, i32 2
  store i32 0, ptr %1636
  %1637 = getelementptr [3 x i32], ptr %stack.ptr_480, i32 0, i32 0
  %1638 = getelementptr [3 x i32], ptr %1631, i32 0, i32 2
  %1639 = load i32, ptr %1638
  store i32 %1639, ptr %1637
  %1640 = getelementptr [3 x i32], ptr %stack.ptr_480, i32 0, i32 1
  store i32 4294967295, ptr %1640
  %1641 = getelementptr [3 x i32], ptr %stack.ptr_480, i32 0, i32 2
  store i32 4294967295, ptr %1641
  %1642 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %1642, ptr %stack.ptr_479, ptr %stack.ptr_481)
  %1643 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %1643, ptr %stack.ptr_480, ptr %stack.ptr_482)
  br label %loop_106
loop_106:
  %1644 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_481, ptr %stack.ptr_482)
  br i1 %1644, label %if_121, label %end_if_121
if_121:
  br label %range_query.end_105
end_if_121:
  %1645 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_481)
  %1646 = getelementptr [2 x i32], ptr %stack.ptr_483, i32 0, i32 0
  %1647 = getelementptr [3 x i32], ptr %1645, i32 0, i32 2
  %1648 = load i32, ptr %1647
  store i32 %1648, ptr %1646
  %1649 = getelementptr [2 x i32], ptr %stack.ptr_483, i32 0, i32 1
  store i32 0, ptr %1649
  %1650 = getelementptr [2 x i32], ptr %stack.ptr_484, i32 0, i32 0
  %1651 = getelementptr [3 x i32], ptr %1645, i32 0, i32 2
  %1652 = load i32, ptr %1651
  store i32 %1652, ptr %1650
  %1653 = getelementptr [2 x i32], ptr %stack.ptr_484, i32 0, i32 1
  store i32 4294967295, ptr %1653
  %1654 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_lower_bound_1(ptr %1654, ptr %stack.ptr_483, ptr %stack.ptr_485)
  %1655 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_upper_bound_1(ptr %1655, ptr %stack.ptr_484, ptr %stack.ptr_486)
  br label %loop_107
loop_107:
  %1656 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_485, ptr %stack.ptr_486)
  br i1 %1656, label %if_122, label %end_if_122
if_122:
  br label %range_query.end_106
end_if_122:
  %1657 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_485)
  %1658 = getelementptr [2 x i32], ptr %stack.ptr_487, i32 0, i32 0
  %1659 = getelementptr [2 x i32], ptr %1617, i32 0, i32 1
  %1660 = load i32, ptr %1659
  store i32 %1660, ptr %1658
  %1661 = getelementptr [2 x i32], ptr %stack.ptr_487, i32 0, i32 1
  %1662 = getelementptr [2 x i32], ptr %1657, i32 0, i32 1
  %1663 = load i32, ptr %1662
  store i32 %1663, ptr %1661
  %1664 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  %1665 = call ccc i1 @eclair_btree_insert_value_1(ptr %1664, ptr %stack.ptr_487)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_485)
  br label %loop_107
range_query.end_106:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_481)
  br label %loop_106
range_query.end_105:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_477)
  br label %loop_105
range_query.end_104:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_473)
  br label %loop_104
range_query.end_103:
  %1666 = getelementptr [2 x i32], ptr %stack.ptr_488, i32 0, i32 0
  store i32 0, ptr %1666
  %1667 = getelementptr [2 x i32], ptr %stack.ptr_488, i32 0, i32 1
  store i32 0, ptr %1667
  %1668 = getelementptr [2 x i32], ptr %stack.ptr_489, i32 0, i32 0
  store i32 4294967295, ptr %1668
  %1669 = getelementptr [2 x i32], ptr %stack.ptr_489, i32 0, i32 1
  store i32 4294967295, ptr %1669
  %1670 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_lower_bound_1(ptr %1670, ptr %stack.ptr_488, ptr %stack.ptr_490)
  %1671 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_upper_bound_1(ptr %1671, ptr %stack.ptr_489, ptr %stack.ptr_491)
  br label %loop_108
loop_108:
  %1672 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_490, ptr %stack.ptr_491)
  br i1 %1672, label %if_123, label %end_if_123
if_123:
  br label %range_query.end_107
end_if_123:
  %1673 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_490)
  %1674 = getelementptr [2 x i32], ptr %stack.ptr_492, i32 0, i32 0
  %1675 = getelementptr [2 x i32], ptr %1673, i32 0, i32 0
  %1676 = load i32, ptr %1675
  store i32 %1676, ptr %1674
  %1677 = getelementptr [2 x i32], ptr %stack.ptr_492, i32 0, i32 1
  %1678 = getelementptr [2 x i32], ptr %1673, i32 0, i32 1
  %1679 = load i32, ptr %1678
  store i32 %1679, ptr %1677
  %1680 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  %1681 = call ccc i1 @eclair_btree_insert_value_1(ptr %1680, ptr %stack.ptr_492)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_490)
  br label %loop_108
range_query.end_107:
  %1682 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_begin_1(ptr %1682, ptr %stack.ptr_493)
  %1683 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_end_1(ptr %1683, ptr %stack.ptr_494)
  %1684 = getelementptr %program, ptr %arg_0, i32 0, i32 20
  call ccc void @eclair_btree_insert_range_delta_transitive_depends_on_transitive_depends_on(ptr %1684, ptr %stack.ptr_493, ptr %stack.ptr_494)
  br label %loop_109
loop_109:
  %1685 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  call ccc void @eclair_btree_clear_1(ptr %1685)
  %1686 = getelementptr [2 x i32], ptr %stack.ptr_495, i32 0, i32 0
  store i32 0, ptr %1686
  %1687 = getelementptr [2 x i32], ptr %stack.ptr_495, i32 0, i32 1
  store i32 0, ptr %1687
  %1688 = getelementptr [2 x i32], ptr %stack.ptr_496, i32 0, i32 0
  store i32 4294967295, ptr %1688
  %1689 = getelementptr [2 x i32], ptr %stack.ptr_496, i32 0, i32 1
  store i32 4294967295, ptr %1689
  %1690 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_lower_bound_1(ptr %1690, ptr %stack.ptr_495, ptr %stack.ptr_497)
  %1691 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_upper_bound_1(ptr %1691, ptr %stack.ptr_496, ptr %stack.ptr_498)
  br label %loop_110
loop_110:
  %1692 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_497, ptr %stack.ptr_498)
  br i1 %1692, label %if_124, label %end_if_124
if_124:
  br label %range_query.end_108
end_if_124:
  %1693 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_497)
  %1694 = getelementptr [2 x i32], ptr %stack.ptr_499, i32 0, i32 0
  %1695 = getelementptr [2 x i32], ptr %1693, i32 0, i32 1
  %1696 = load i32, ptr %1695
  store i32 %1696, ptr %1694
  %1697 = getelementptr [2 x i32], ptr %stack.ptr_499, i32 0, i32 1
  store i32 0, ptr %1697
  %1698 = getelementptr [2 x i32], ptr %stack.ptr_500, i32 0, i32 0
  %1699 = getelementptr [2 x i32], ptr %1693, i32 0, i32 1
  %1700 = load i32, ptr %1699
  store i32 %1700, ptr %1698
  %1701 = getelementptr [2 x i32], ptr %stack.ptr_500, i32 0, i32 1
  store i32 4294967295, ptr %1701
  %1702 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1702, ptr %stack.ptr_499, ptr %stack.ptr_501)
  %1703 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1703, ptr %stack.ptr_500, ptr %stack.ptr_502)
  br label %loop_111
loop_111:
  %1704 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_501, ptr %stack.ptr_502)
  br i1 %1704, label %if_125, label %end_if_125
if_125:
  br label %range_query.end_109
end_if_125:
  %1705 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_501)
  %1706 = getelementptr [2 x i32], ptr %stack.ptr_503, i32 0, i32 0
  %1707 = getelementptr [2 x i32], ptr %1693, i32 0, i32 1
  %1708 = load i32, ptr %1707
  store i32 %1708, ptr %1706
  %1709 = getelementptr [2 x i32], ptr %stack.ptr_503, i32 0, i32 1
  %1710 = getelementptr [2 x i32], ptr %1705, i32 0, i32 1
  %1711 = load i32, ptr %1710
  store i32 %1711, ptr %1709
  %1712 = getelementptr %program, ptr %arg_0, i32 0, i32 20
  %1713 = call ccc i1 @eclair_btree_contains_1(ptr %1712, ptr %stack.ptr_503)
  %1714 = select i1 %1713, i1 0, i1 1
  br i1 %1714, label %if_126, label %end_if_127
if_126:
  %1715 = getelementptr [2 x i32], ptr %stack.ptr_504, i32 0, i32 0
  %1716 = getelementptr [2 x i32], ptr %1693, i32 0, i32 0
  %1717 = load i32, ptr %1716
  store i32 %1717, ptr %1715
  %1718 = getelementptr [2 x i32], ptr %stack.ptr_504, i32 0, i32 1
  %1719 = getelementptr [2 x i32], ptr %1705, i32 0, i32 1
  %1720 = load i32, ptr %1719
  store i32 %1720, ptr %1718
  %1721 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  %1722 = call ccc i1 @eclair_btree_contains_1(ptr %1721, ptr %stack.ptr_504)
  %1723 = select i1 %1722, i1 0, i1 1
  br i1 %1723, label %if_127, label %end_if_126
if_127:
  %1724 = getelementptr [2 x i32], ptr %stack.ptr_505, i32 0, i32 0
  %1725 = getelementptr [2 x i32], ptr %1693, i32 0, i32 0
  %1726 = load i32, ptr %1725
  store i32 %1726, ptr %1724
  %1727 = getelementptr [2 x i32], ptr %stack.ptr_505, i32 0, i32 1
  %1728 = getelementptr [2 x i32], ptr %1705, i32 0, i32 1
  %1729 = load i32, ptr %1728
  store i32 %1729, ptr %1727
  %1730 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  %1731 = call ccc i1 @eclair_btree_insert_value_1(ptr %1730, ptr %stack.ptr_505)
  br label %end_if_126
end_if_126:
  br label %end_if_127
end_if_127:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_501)
  br label %loop_111
range_query.end_109:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_497)
  br label %loop_110
range_query.end_108:
  %1732 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  %1733 = call ccc i1 @eclair_btree_is_empty_1(ptr %1732)
  br i1 %1733, label %if_128, label %end_if_128
if_128:
  br label %loop.end_1
end_if_128:
  %1734 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  call ccc void @eclair_btree_begin_1(ptr %1734, ptr %stack.ptr_506)
  %1735 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  call ccc void @eclair_btree_end_1(ptr %1735, ptr %stack.ptr_507)
  %1736 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_insert_range_transitive_depends_on_new_transitive_depends_on(ptr %1736, ptr %stack.ptr_506, ptr %stack.ptr_507)
  %1737 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  %1738 = getelementptr %program, ptr %arg_0, i32 0, i32 20
  call ccc void @eclair_btree_swap_1(ptr %1737, ptr %1738)
  br label %loop_109
loop.end_1:
  %1739 = getelementptr [2 x i32], ptr %stack.ptr_508, i32 0, i32 0
  store i32 0, ptr %1739
  %1740 = getelementptr [2 x i32], ptr %stack.ptr_508, i32 0, i32 1
  store i32 0, ptr %1740
  %1741 = getelementptr [2 x i32], ptr %stack.ptr_509, i32 0, i32 0
  store i32 4294967295, ptr %1741
  %1742 = getelementptr [2 x i32], ptr %stack.ptr_509, i32 0, i32 1
  store i32 4294967295, ptr %1742
  %1743 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1743, ptr %stack.ptr_508, ptr %stack.ptr_510)
  %1744 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1744, ptr %stack.ptr_509, ptr %stack.ptr_511)
  br label %loop_112
loop_112:
  %1745 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_510, ptr %stack.ptr_511)
  br i1 %1745, label %if_129, label %end_if_129
if_129:
  br label %range_query.end_110
end_if_129:
  %1746 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_510)
  %1747 = getelementptr [2 x i32], ptr %1746, i32 0, i32 0
  %1748 = load i32, ptr %1747
  %1749 = getelementptr [2 x i32], ptr %1746, i32 0, i32 1
  %1750 = load i32, ptr %1749
  %1751 = icmp eq i32 %1748, %1750
  br i1 %1751, label %if_130, label %end_if_130
if_130:
  %1752 = getelementptr [1 x i32], ptr %stack.ptr_512, i32 0, i32 0
  %1753 = getelementptr [2 x i32], ptr %1746, i32 0, i32 0
  %1754 = load i32, ptr %1753
  store i32 %1754, ptr %1752
  %1755 = getelementptr %program, ptr %arg_0, i32 0, i32 21
  %1756 = call ccc i1 @eclair_btree_insert_value_6(ptr %1755, ptr %stack.ptr_512)
  br label %end_if_130
end_if_130:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_510)
  br label %loop_112
range_query.end_110:
  %1757 = getelementptr [1 x i32], ptr %stack.ptr_513, i32 0, i32 0
  store i32 0, ptr %1757
  %1758 = getelementptr [1 x i32], ptr %stack.ptr_514, i32 0, i32 0
  store i32 4294967295, ptr %1758
  %1759 = getelementptr %program, ptr %arg_0, i32 0, i32 21
  call ccc void @eclair_btree_lower_bound_6(ptr %1759, ptr %stack.ptr_513, ptr %stack.ptr_515)
  %1760 = getelementptr %program, ptr %arg_0, i32 0, i32 21
  call ccc void @eclair_btree_upper_bound_6(ptr %1760, ptr %stack.ptr_514, ptr %stack.ptr_516)
  br label %loop_113
loop_113:
  %1761 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_515, ptr %stack.ptr_516)
  br i1 %1761, label %if_131, label %end_if_131
if_131:
  br label %range_query.end_111
end_if_131:
  %1762 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_515)
  %1763 = getelementptr [2 x i32], ptr %stack.ptr_517, i32 0, i32 0
  %1764 = getelementptr [1 x i32], ptr %1762, i32 0, i32 0
  %1765 = load i32, ptr %1764
  store i32 %1765, ptr %1763
  %1766 = getelementptr [2 x i32], ptr %stack.ptr_517, i32 0, i32 1
  store i32 0, ptr %1766
  %1767 = getelementptr [2 x i32], ptr %stack.ptr_518, i32 0, i32 0
  %1768 = getelementptr [1 x i32], ptr %1762, i32 0, i32 0
  %1769 = load i32, ptr %1768
  store i32 %1769, ptr %1767
  %1770 = getelementptr [2 x i32], ptr %stack.ptr_518, i32 0, i32 1
  store i32 4294967295, ptr %1770
  %1771 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1771, ptr %stack.ptr_517, ptr %stack.ptr_519)
  %1772 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1772, ptr %stack.ptr_518, ptr %stack.ptr_520)
  br label %loop_114
loop_114:
  %1773 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_519, ptr %stack.ptr_520)
  br i1 %1773, label %if_132, label %end_if_132
if_132:
  br label %range_query.end_112
end_if_132:
  %1774 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_519)
  %1775 = getelementptr [2 x i32], ptr %stack.ptr_521, i32 0, i32 0
  store i32 0, ptr %1775
  %1776 = getelementptr [2 x i32], ptr %stack.ptr_521, i32 0, i32 1
  %1777 = getelementptr [2 x i32], ptr %1774, i32 0, i32 1
  %1778 = load i32, ptr %1777
  store i32 %1778, ptr %1776
  %1779 = getelementptr [2 x i32], ptr %stack.ptr_522, i32 0, i32 0
  store i32 4294967295, ptr %1779
  %1780 = getelementptr [2 x i32], ptr %stack.ptr_522, i32 0, i32 1
  %1781 = getelementptr [2 x i32], ptr %1774, i32 0, i32 1
  %1782 = load i32, ptr %1781
  store i32 %1782, ptr %1780
  %1783 = getelementptr %program, ptr %arg_0, i32 0, i32 51
  call ccc void @eclair_btree_lower_bound_2(ptr %1783, ptr %stack.ptr_521, ptr %stack.ptr_523)
  %1784 = getelementptr %program, ptr %arg_0, i32 0, i32 51
  call ccc void @eclair_btree_upper_bound_2(ptr %1784, ptr %stack.ptr_522, ptr %stack.ptr_524)
  br label %loop_115
loop_115:
  %1785 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_523, ptr %stack.ptr_524)
  br i1 %1785, label %if_133, label %end_if_133
if_133:
  br label %range_query.end_113
end_if_133:
  %1786 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_523)
  %1787 = getelementptr [3 x i32], ptr %stack.ptr_525, i32 0, i32 0
  %1788 = getelementptr [2 x i32], ptr %1786, i32 0, i32 0
  %1789 = load i32, ptr %1788
  store i32 %1789, ptr %1787
  %1790 = getelementptr [3 x i32], ptr %stack.ptr_525, i32 0, i32 1
  store i32 0, ptr %1790
  %1791 = getelementptr [3 x i32], ptr %stack.ptr_525, i32 0, i32 2
  store i32 0, ptr %1791
  %1792 = getelementptr [3 x i32], ptr %stack.ptr_526, i32 0, i32 0
  %1793 = getelementptr [2 x i32], ptr %1786, i32 0, i32 0
  %1794 = load i32, ptr %1793
  store i32 %1794, ptr %1792
  %1795 = getelementptr [3 x i32], ptr %stack.ptr_526, i32 0, i32 1
  store i32 4294967295, ptr %1795
  %1796 = getelementptr [3 x i32], ptr %stack.ptr_526, i32 0, i32 2
  store i32 4294967295, ptr %1796
  %1797 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1797, ptr %stack.ptr_525, ptr %stack.ptr_527)
  %1798 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1798, ptr %stack.ptr_526, ptr %stack.ptr_528)
  br label %loop_116
loop_116:
  %1799 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_527, ptr %stack.ptr_528)
  br i1 %1799, label %if_134, label %end_if_134
if_134:
  br label %range_query.end_114
end_if_134:
  %1800 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_527)
  %1801 = getelementptr [2 x i32], ptr %stack.ptr_529, i32 0, i32 0
  %1802 = getelementptr [3 x i32], ptr %1800, i32 0, i32 2
  %1803 = load i32, ptr %1802
  store i32 %1803, ptr %1801
  %1804 = getelementptr [2 x i32], ptr %stack.ptr_529, i32 0, i32 1
  store i32 0, ptr %1804
  %1805 = getelementptr [2 x i32], ptr %stack.ptr_530, i32 0, i32 0
  %1806 = getelementptr [3 x i32], ptr %1800, i32 0, i32 2
  %1807 = load i32, ptr %1806
  store i32 %1807, ptr %1805
  %1808 = getelementptr [2 x i32], ptr %stack.ptr_530, i32 0, i32 1
  store i32 4294967295, ptr %1808
  %1809 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %1809, ptr %stack.ptr_529, ptr %stack.ptr_531)
  %1810 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %1810, ptr %stack.ptr_530, ptr %stack.ptr_532)
  br label %loop_117
loop_117:
  %1811 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_531, ptr %stack.ptr_532)
  br i1 %1811, label %if_135, label %end_if_135
if_135:
  br label %range_query.end_115
end_if_135:
  %1812 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_531)
  %1813 = getelementptr [2 x i32], ptr %stack.ptr_533, i32 0, i32 0
  %1814 = getelementptr [2 x i32], ptr %1812, i32 0, i32 1
  %1815 = load i32, ptr %1814
  store i32 %1815, ptr %1813
  %1816 = getelementptr [2 x i32], ptr %stack.ptr_533, i32 0, i32 1
  store i32 0, ptr %1816
  %1817 = getelementptr [2 x i32], ptr %stack.ptr_534, i32 0, i32 0
  %1818 = getelementptr [2 x i32], ptr %1812, i32 0, i32 1
  %1819 = load i32, ptr %1818
  store i32 %1819, ptr %1817
  %1820 = getelementptr [2 x i32], ptr %stack.ptr_534, i32 0, i32 1
  store i32 4294967295, ptr %1820
  %1821 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1821, ptr %stack.ptr_533, ptr %stack.ptr_535)
  %1822 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1822, ptr %stack.ptr_534, ptr %stack.ptr_536)
  br label %loop_118
loop_118:
  %1823 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_535, ptr %stack.ptr_536)
  br i1 %1823, label %if_136, label %end_if_136
if_136:
  br label %range_query.end_116
end_if_136:
  %1824 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_535)
  %1825 = getelementptr [2 x i32], ptr %stack.ptr_537, i32 0, i32 0
  %1826 = getelementptr [2 x i32], ptr %1824, i32 0, i32 1
  %1827 = load i32, ptr %1826
  store i32 %1827, ptr %1825
  %1828 = getelementptr [2 x i32], ptr %stack.ptr_537, i32 0, i32 1
  %1829 = getelementptr [1 x i32], ptr %1762, i32 0, i32 0
  %1830 = load i32, ptr %1829
  store i32 %1830, ptr %1828
  %1831 = getelementptr [2 x i32], ptr %stack.ptr_538, i32 0, i32 0
  %1832 = getelementptr [2 x i32], ptr %1824, i32 0, i32 1
  %1833 = load i32, ptr %1832
  store i32 %1833, ptr %1831
  %1834 = getelementptr [2 x i32], ptr %stack.ptr_538, i32 0, i32 1
  %1835 = getelementptr [1 x i32], ptr %1762, i32 0, i32 0
  %1836 = load i32, ptr %1835
  store i32 %1836, ptr %1834
  %1837 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1837, ptr %stack.ptr_537, ptr %stack.ptr_539)
  %1838 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1838, ptr %stack.ptr_538, ptr %stack.ptr_540)
  br label %loop_119
loop_119:
  %1839 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_539, ptr %stack.ptr_540)
  br i1 %1839, label %if_137, label %end_if_137
if_137:
  br label %range_query.end_117
end_if_137:
  %1840 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_539)
  %1841 = getelementptr [1 x i32], ptr %stack.ptr_541, i32 0, i32 0
  %1842 = getelementptr [3 x i32], ptr %1800, i32 0, i32 2
  %1843 = load i32, ptr %1842
  store i32 %1843, ptr %1841
  %1844 = getelementptr %program, ptr %arg_0, i32 0, i32 11
  %1845 = call ccc i1 @eclair_btree_insert_value_6(ptr %1844, ptr %stack.ptr_541)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_539)
  br label %loop_119
range_query.end_117:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_535)
  br label %loop_118
range_query.end_116:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_531)
  br label %loop_117
range_query.end_115:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_527)
  br label %loop_116
range_query.end_114:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_523)
  br label %loop_115
range_query.end_113:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_519)
  br label %loop_114
range_query.end_112:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_515)
  br label %loop_113
range_query.end_111:
  %1846 = getelementptr [1 x i32], ptr %stack.ptr_542, i32 0, i32 0
  store i32 0, ptr %1846
  %1847 = getelementptr [1 x i32], ptr %stack.ptr_543, i32 0, i32 0
  store i32 4294967295, ptr %1847
  %1848 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_lower_bound_6(ptr %1848, ptr %stack.ptr_542, ptr %stack.ptr_544)
  %1849 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_upper_bound_6(ptr %1849, ptr %stack.ptr_543, ptr %stack.ptr_545)
  br label %loop_120
loop_120:
  %1850 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_544, ptr %stack.ptr_545)
  br i1 %1850, label %if_138, label %end_if_138
if_138:
  br label %range_query.end_118
end_if_138:
  %1851 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_544)
  %1852 = getelementptr [2 x i32], ptr %stack.ptr_546, i32 0, i32 0
  %1853 = getelementptr [1 x i32], ptr %1851, i32 0, i32 0
  %1854 = load i32, ptr %1853
  store i32 %1854, ptr %1852
  %1855 = getelementptr [2 x i32], ptr %stack.ptr_546, i32 0, i32 1
  store i32 0, ptr %1855
  %1856 = getelementptr [2 x i32], ptr %stack.ptr_547, i32 0, i32 0
  %1857 = getelementptr [1 x i32], ptr %1851, i32 0, i32 0
  %1858 = load i32, ptr %1857
  store i32 %1858, ptr %1856
  %1859 = getelementptr [2 x i32], ptr %stack.ptr_547, i32 0, i32 1
  store i32 4294967295, ptr %1859
  %1860 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1860, ptr %stack.ptr_546, ptr %stack.ptr_548)
  %1861 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1861, ptr %stack.ptr_547, ptr %stack.ptr_549)
  br label %loop_121
loop_121:
  %1862 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_548, ptr %stack.ptr_549)
  br i1 %1862, label %if_139, label %end_if_139
if_139:
  br label %range_query.end_119
end_if_139:
  %1863 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_548)
  %1864 = getelementptr [1 x i32], ptr %stack.ptr_550, i32 0, i32 0
  %1865 = getelementptr [2 x i32], ptr %1863, i32 0, i32 1
  %1866 = load i32, ptr %1865
  store i32 %1866, ptr %1864
  %1867 = getelementptr [1 x i32], ptr %stack.ptr_551, i32 0, i32 0
  %1868 = getelementptr [2 x i32], ptr %1863, i32 0, i32 1
  %1869 = load i32, ptr %1868
  store i32 %1869, ptr %1867
  %1870 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_lower_bound_6(ptr %1870, ptr %stack.ptr_550, ptr %stack.ptr_552)
  %1871 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_upper_bound_6(ptr %1871, ptr %stack.ptr_551, ptr %stack.ptr_553)
  br label %loop_122
loop_122:
  %1872 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_552, ptr %stack.ptr_553)
  br i1 %1872, label %if_140, label %end_if_140
if_140:
  br label %range_query.end_120
end_if_140:
  %1873 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_552)
  %1874 = getelementptr [1 x i32], ptr %stack.ptr_554, i32 0, i32 0
  %1875 = getelementptr [1 x i32], ptr %1851, i32 0, i32 0
  %1876 = load i32, ptr %1875
  store i32 %1876, ptr %1874
  %1877 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1878 = call ccc i1 @eclair_btree_insert_value_6(ptr %1877, ptr %stack.ptr_554)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_552)
  br label %loop_122
range_query.end_120:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_548)
  br label %loop_121
range_query.end_119:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_544)
  br label %loop_120
range_query.end_118:
  %1879 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_begin_6(ptr %1879, ptr %stack.ptr_555)
  %1880 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_end_6(ptr %1880, ptr %stack.ptr_556)
  %1881 = getelementptr %program, ptr %arg_0, i32 0, i32 18
  call ccc void @eclair_btree_insert_range_delta_live_rule_live_rule(ptr %1881, ptr %stack.ptr_555, ptr %stack.ptr_556)
  br label %loop_123
loop_123:
  %1882 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  call ccc void @eclair_btree_clear_6(ptr %1882)
  %1883 = getelementptr [2 x i32], ptr %stack.ptr_557, i32 0, i32 0
  store i32 0, ptr %1883
  %1884 = getelementptr [2 x i32], ptr %stack.ptr_557, i32 0, i32 1
  store i32 0, ptr %1884
  %1885 = getelementptr [2 x i32], ptr %stack.ptr_558, i32 0, i32 0
  store i32 4294967295, ptr %1885
  %1886 = getelementptr [2 x i32], ptr %stack.ptr_558, i32 0, i32 1
  store i32 4294967295, ptr %1886
  %1887 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_lower_bound_1(ptr %1887, ptr %stack.ptr_557, ptr %stack.ptr_559)
  %1888 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_upper_bound_1(ptr %1888, ptr %stack.ptr_558, ptr %stack.ptr_560)
  br label %loop_124
loop_124:
  %1889 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_559, ptr %stack.ptr_560)
  br i1 %1889, label %if_141, label %end_if_141
if_141:
  br label %range_query.end_121
end_if_141:
  %1890 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_559)
  %1891 = getelementptr [1 x i32], ptr %stack.ptr_561, i32 0, i32 0
  %1892 = getelementptr [2 x i32], ptr %1890, i32 0, i32 0
  %1893 = load i32, ptr %1892
  store i32 %1893, ptr %1891
  %1894 = getelementptr %program, ptr %arg_0, i32 0, i32 18
  %1895 = call ccc i1 @eclair_btree_contains_6(ptr %1894, ptr %stack.ptr_561)
  %1896 = select i1 %1895, i1 0, i1 1
  br i1 %1896, label %if_142, label %end_if_144
if_142:
  %1897 = getelementptr [1 x i32], ptr %stack.ptr_562, i32 0, i32 0
  %1898 = getelementptr [2 x i32], ptr %1890, i32 0, i32 1
  %1899 = load i32, ptr %1898
  store i32 %1899, ptr %1897
  %1900 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1901 = call ccc i1 @eclair_btree_contains_6(ptr %1900, ptr %stack.ptr_562)
  %1902 = select i1 %1901, i1 0, i1 1
  br i1 %1902, label %if_143, label %end_if_143
if_143:
  %1903 = getelementptr [1 x i32], ptr %stack.ptr_563, i32 0, i32 0
  %1904 = getelementptr [2 x i32], ptr %1890, i32 0, i32 0
  %1905 = load i32, ptr %1904
  store i32 %1905, ptr %1903
  %1906 = getelementptr [1 x i32], ptr %stack.ptr_564, i32 0, i32 0
  %1907 = getelementptr [2 x i32], ptr %1890, i32 0, i32 0
  %1908 = load i32, ptr %1907
  store i32 %1908, ptr %1906
  %1909 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_lower_bound_6(ptr %1909, ptr %stack.ptr_563, ptr %stack.ptr_565)
  %1910 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_upper_bound_6(ptr %1910, ptr %stack.ptr_564, ptr %stack.ptr_566)
  br label %loop_125
loop_125:
  %1911 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_565, ptr %stack.ptr_566)
  br i1 %1911, label %if_144, label %end_if_142
if_144:
  br label %range_query.end_122
end_if_142:
  %1912 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_565)
  %1913 = getelementptr [1 x i32], ptr %stack.ptr_567, i32 0, i32 0
  %1914 = getelementptr [2 x i32], ptr %1890, i32 0, i32 1
  %1915 = load i32, ptr %1914
  store i32 %1915, ptr %1913
  %1916 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  %1917 = call ccc i1 @eclair_btree_insert_value_6(ptr %1916, ptr %stack.ptr_567)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_565)
  br label %loop_125
range_query.end_122:
  br label %end_if_143
end_if_143:
  br label %end_if_144
end_if_144:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_559)
  br label %loop_124
range_query.end_121:
  %1918 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  %1919 = call ccc i1 @eclair_btree_is_empty_6(ptr %1918)
  br i1 %1919, label %if_145, label %end_if_145
if_145:
  br label %loop.end_2
end_if_145:
  %1920 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  call ccc void @eclair_btree_begin_6(ptr %1920, ptr %stack.ptr_568)
  %1921 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  call ccc void @eclair_btree_end_6(ptr %1921, ptr %stack.ptr_569)
  %1922 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_insert_range_live_rule_new_live_rule(ptr %1922, ptr %stack.ptr_568, ptr %stack.ptr_569)
  %1923 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  %1924 = getelementptr %program, ptr %arg_0, i32 0, i32 18
  call ccc void @eclair_btree_swap_6(ptr %1923, ptr %1924)
  br label %loop_123
loop.end_2:
  %1925 = getelementptr [2 x i32], ptr %stack.ptr_570, i32 0, i32 0
  store i32 0, ptr %1925
  %1926 = getelementptr [2 x i32], ptr %stack.ptr_570, i32 0, i32 1
  store i32 0, ptr %1926
  %1927 = getelementptr [2 x i32], ptr %stack.ptr_571, i32 0, i32 0
  store i32 4294967295, ptr %1927
  %1928 = getelementptr [2 x i32], ptr %stack.ptr_571, i32 0, i32 1
  store i32 4294967295, ptr %1928
  %1929 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1929, ptr %stack.ptr_570, ptr %stack.ptr_572)
  %1930 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1930, ptr %stack.ptr_571, ptr %stack.ptr_573)
  br label %loop_126
loop_126:
  %1931 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_572, ptr %stack.ptr_573)
  br i1 %1931, label %if_146, label %end_if_146
if_146:
  br label %range_query.end_123
end_if_146:
  %1932 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_572)
  %1933 = getelementptr [1 x i32], ptr %stack.ptr_574, i32 0, i32 0
  %1934 = getelementptr [2 x i32], ptr %1932, i32 0, i32 1
  %1935 = load i32, ptr %1934
  store i32 %1935, ptr %1933
  %1936 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1937 = call ccc i1 @eclair_btree_contains_6(ptr %1936, ptr %stack.ptr_574)
  %1938 = select i1 %1937, i1 0, i1 1
  br i1 %1938, label %if_147, label %end_if_147
if_147:
  %1939 = getelementptr [1 x i32], ptr %stack.ptr_575, i32 0, i32 0
  %1940 = getelementptr [2 x i32], ptr %1932, i32 0, i32 0
  %1941 = load i32, ptr %1940
  store i32 %1941, ptr %1939
  %1942 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1943 = call ccc i1 @eclair_btree_insert_value_6(ptr %1942, ptr %stack.ptr_575)
  br label %end_if_147
end_if_147:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_572)
  br label %loop_126
range_query.end_123:
  %1944 = getelementptr [2 x i32], ptr %stack.ptr_576, i32 0, i32 0
  store i32 0, ptr %1944
  %1945 = getelementptr [2 x i32], ptr %stack.ptr_576, i32 0, i32 1
  store i32 0, ptr %1945
  %1946 = getelementptr [2 x i32], ptr %stack.ptr_577, i32 0, i32 0
  store i32 4294967295, ptr %1946
  %1947 = getelementptr [2 x i32], ptr %stack.ptr_577, i32 0, i32 1
  store i32 4294967295, ptr %1947
  %1948 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_lower_bound_1(ptr %1948, ptr %stack.ptr_576, ptr %stack.ptr_578)
  %1949 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_upper_bound_1(ptr %1949, ptr %stack.ptr_577, ptr %stack.ptr_579)
  br label %loop_127
loop_127:
  %1950 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_578, ptr %stack.ptr_579)
  br i1 %1950, label %if_148, label %end_if_148
if_148:
  br label %range_query.end_124
end_if_148:
  %1951 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_578)
  %1952 = getelementptr [1 x i32], ptr %stack.ptr_580, i32 0, i32 0
  %1953 = getelementptr [2 x i32], ptr %1951, i32 0, i32 1
  %1954 = load i32, ptr %1953
  store i32 %1954, ptr %1952
  %1955 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1956 = call ccc i1 @eclair_btree_contains_6(ptr %1955, ptr %stack.ptr_580)
  %1957 = select i1 %1956, i1 0, i1 1
  br i1 %1957, label %if_149, label %end_if_149
if_149:
  %1958 = getelementptr [1 x i32], ptr %stack.ptr_581, i32 0, i32 0
  %1959 = getelementptr [2 x i32], ptr %1951, i32 0, i32 0
  %1960 = load i32, ptr %1959
  store i32 %1960, ptr %1958
  %1961 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1962 = call ccc i1 @eclair_btree_insert_value_6(ptr %1961, ptr %stack.ptr_581)
  br label %end_if_149
end_if_149:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_578)
  br label %loop_127
range_query.end_124:
  %1963 = getelementptr [2 x i32], ptr %stack.ptr_582, i32 0, i32 0
  store i32 0, ptr %1963
  %1964 = getelementptr [2 x i32], ptr %stack.ptr_582, i32 0, i32 1
  store i32 0, ptr %1964
  %1965 = getelementptr [2 x i32], ptr %stack.ptr_583, i32 0, i32 0
  store i32 4294967295, ptr %1965
  %1966 = getelementptr [2 x i32], ptr %stack.ptr_583, i32 0, i32 1
  store i32 4294967295, ptr %1966
  %1967 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %1967, ptr %stack.ptr_582, ptr %stack.ptr_584)
  %1968 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %1968, ptr %stack.ptr_583, ptr %stack.ptr_585)
  br label %loop_128
loop_128:
  %1969 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_584, ptr %stack.ptr_585)
  br i1 %1969, label %if_150, label %end_if_150
if_150:
  br label %range_query.end_125
end_if_150:
  %1970 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_584)
  %1971 = getelementptr [1 x i32], ptr %stack.ptr_586, i32 0, i32 0
  %1972 = getelementptr [2 x i32], ptr %1970, i32 0, i32 1
  %1973 = load i32, ptr %1972
  store i32 %1973, ptr %1971
  %1974 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1975 = call ccc i1 @eclair_btree_contains_6(ptr %1974, ptr %stack.ptr_586)
  %1976 = select i1 %1975, i1 0, i1 1
  br i1 %1976, label %if_151, label %end_if_151
if_151:
  %1977 = getelementptr [1 x i32], ptr %stack.ptr_587, i32 0, i32 0
  %1978 = getelementptr [2 x i32], ptr %1970, i32 0, i32 0
  %1979 = load i32, ptr %1978
  store i32 %1979, ptr %1977
  %1980 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1981 = call ccc i1 @eclair_btree_insert_value_6(ptr %1980, ptr %stack.ptr_587)
  br label %end_if_151
end_if_151:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_584)
  br label %loop_128
range_query.end_125:
  %1982 = getelementptr [2 x i32], ptr %stack.ptr_588, i32 0, i32 0
  store i32 0, ptr %1982
  %1983 = getelementptr [2 x i32], ptr %stack.ptr_588, i32 0, i32 1
  store i32 0, ptr %1983
  %1984 = getelementptr [2 x i32], ptr %stack.ptr_589, i32 0, i32 0
  store i32 4294967295, ptr %1984
  %1985 = getelementptr [2 x i32], ptr %stack.ptr_589, i32 0, i32 1
  store i32 4294967295, ptr %1985
  %1986 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1986, ptr %stack.ptr_588, ptr %stack.ptr_590)
  %1987 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1987, ptr %stack.ptr_589, ptr %stack.ptr_591)
  br label %loop_129
loop_129:
  %1988 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_590, ptr %stack.ptr_591)
  br i1 %1988, label %if_152, label %end_if_152
if_152:
  br label %range_query.end_126
end_if_152:
  %1989 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_590)
  %1990 = getelementptr [1 x i32], ptr %stack.ptr_592, i32 0, i32 0
  %1991 = getelementptr [2 x i32], ptr %1989, i32 0, i32 1
  %1992 = load i32, ptr %1991
  store i32 %1992, ptr %1990
  %1993 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1994 = call ccc i1 @eclair_btree_contains_6(ptr %1993, ptr %stack.ptr_592)
  %1995 = select i1 %1994, i1 0, i1 1
  br i1 %1995, label %if_153, label %end_if_153
if_153:
  %1996 = getelementptr [1 x i32], ptr %stack.ptr_593, i32 0, i32 0
  %1997 = getelementptr [2 x i32], ptr %1989, i32 0, i32 0
  %1998 = load i32, ptr %1997
  store i32 %1998, ptr %1996
  %1999 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %2000 = call ccc i1 @eclair_btree_insert_value_6(ptr %1999, ptr %stack.ptr_593)
  br label %end_if_153
end_if_153:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_590)
  br label %loop_129
range_query.end_126:
  %2001 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_begin_6(ptr %2001, ptr %stack.ptr_594)
  %2002 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_end_6(ptr %2002, ptr %stack.ptr_595)
  %2003 = getelementptr %program, ptr %arg_0, i32 0, i32 16
  call ccc void @eclair_btree_insert_range_delta_dead_code_dead_code(ptr %2003, ptr %stack.ptr_594, ptr %stack.ptr_595)
  br label %loop_130
loop_130:
  %2004 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  call ccc void @eclair_btree_clear_6(ptr %2004)
  %2005 = getelementptr [3 x i32], ptr %stack.ptr_596, i32 0, i32 0
  store i32 0, ptr %2005
  %2006 = getelementptr [3 x i32], ptr %stack.ptr_596, i32 0, i32 1
  store i32 0, ptr %2006
  %2007 = getelementptr [3 x i32], ptr %stack.ptr_596, i32 0, i32 2
  store i32 0, ptr %2007
  %2008 = getelementptr [3 x i32], ptr %stack.ptr_597, i32 0, i32 0
  store i32 4294967295, ptr %2008
  %2009 = getelementptr [3 x i32], ptr %stack.ptr_597, i32 0, i32 1
  store i32 4294967295, ptr %2009
  %2010 = getelementptr [3 x i32], ptr %stack.ptr_597, i32 0, i32 2
  store i32 4294967295, ptr %2010
  %2011 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2011, ptr %stack.ptr_596, ptr %stack.ptr_598)
  %2012 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2012, ptr %stack.ptr_597, ptr %stack.ptr_599)
  br label %loop_131
loop_131:
  %2013 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_598, ptr %stack.ptr_599)
  br i1 %2013, label %if_154, label %end_if_154
if_154:
  br label %range_query.end_127
end_if_154:
  %2014 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_598)
  %2015 = getelementptr [1 x i32], ptr %stack.ptr_600, i32 0, i32 0
  %2016 = getelementptr [3 x i32], ptr %2014, i32 0, i32 2
  %2017 = load i32, ptr %2016
  store i32 %2017, ptr %2015
  %2018 = getelementptr %program, ptr %arg_0, i32 0, i32 16
  %2019 = call ccc i1 @eclair_btree_contains_6(ptr %2018, ptr %stack.ptr_600)
  %2020 = select i1 %2019, i1 0, i1 1
  br i1 %2020, label %if_155, label %end_if_157
if_155:
  %2021 = getelementptr [1 x i32], ptr %stack.ptr_601, i32 0, i32 0
  %2022 = getelementptr [3 x i32], ptr %2014, i32 0, i32 0
  %2023 = load i32, ptr %2022
  store i32 %2023, ptr %2021
  %2024 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %2025 = call ccc i1 @eclair_btree_contains_6(ptr %2024, ptr %stack.ptr_601)
  %2026 = select i1 %2025, i1 0, i1 1
  br i1 %2026, label %if_156, label %end_if_156
if_156:
  %2027 = getelementptr [1 x i32], ptr %stack.ptr_602, i32 0, i32 0
  %2028 = getelementptr [3 x i32], ptr %2014, i32 0, i32 2
  %2029 = load i32, ptr %2028
  store i32 %2029, ptr %2027
  %2030 = getelementptr [1 x i32], ptr %stack.ptr_603, i32 0, i32 0
  %2031 = getelementptr [3 x i32], ptr %2014, i32 0, i32 2
  %2032 = load i32, ptr %2031
  store i32 %2032, ptr %2030
  %2033 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_lower_bound_6(ptr %2033, ptr %stack.ptr_602, ptr %stack.ptr_604)
  %2034 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_upper_bound_6(ptr %2034, ptr %stack.ptr_603, ptr %stack.ptr_605)
  br label %loop_132
loop_132:
  %2035 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_604, ptr %stack.ptr_605)
  br i1 %2035, label %if_157, label %end_if_155
if_157:
  br label %range_query.end_128
end_if_155:
  %2036 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_604)
  %2037 = getelementptr [1 x i32], ptr %stack.ptr_606, i32 0, i32 0
  %2038 = getelementptr [3 x i32], ptr %2014, i32 0, i32 0
  %2039 = load i32, ptr %2038
  store i32 %2039, ptr %2037
  %2040 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  %2041 = call ccc i1 @eclair_btree_insert_value_6(ptr %2040, ptr %stack.ptr_606)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_604)
  br label %loop_132
range_query.end_128:
  br label %end_if_156
end_if_156:
  br label %end_if_157
end_if_157:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_598)
  br label %loop_131
range_query.end_127:
  %2042 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  %2043 = call ccc i1 @eclair_btree_is_empty_6(ptr %2042)
  br i1 %2043, label %if_158, label %end_if_158
if_158:
  br label %loop.end_3
end_if_158:
  %2044 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  call ccc void @eclair_btree_begin_6(ptr %2044, ptr %stack.ptr_607)
  %2045 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  call ccc void @eclair_btree_end_6(ptr %2045, ptr %stack.ptr_608)
  %2046 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_insert_range_dead_code_new_dead_code(ptr %2046, ptr %stack.ptr_607, ptr %stack.ptr_608)
  %2047 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  %2048 = getelementptr %program, ptr %arg_0, i32 0, i32 16
  call ccc void @eclair_btree_swap_6(ptr %2047, ptr %2048)
  br label %loop_130
loop.end_3:
  %2049 = getelementptr [2 x i32], ptr %stack.ptr_609, i32 0, i32 0
  store i32 0, ptr %2049
  %2050 = getelementptr [2 x i32], ptr %stack.ptr_609, i32 0, i32 1
  store i32 0, ptr %2050
  %2051 = getelementptr [2 x i32], ptr %stack.ptr_610, i32 0, i32 0
  store i32 4294967295, ptr %2051
  %2052 = getelementptr [2 x i32], ptr %stack.ptr_610, i32 0, i32 1
  store i32 4294967295, ptr %2052
  %2053 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %2053, ptr %stack.ptr_609, ptr %stack.ptr_611)
  %2054 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %2054, ptr %stack.ptr_610, ptr %stack.ptr_612)
  br label %loop_133
loop_133:
  %2055 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_611, ptr %stack.ptr_612)
  br i1 %2055, label %if_159, label %end_if_159
if_159:
  br label %range_query.end_129
end_if_159:
  %2056 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_611)
  %2057 = getelementptr [2 x i32], ptr %stack.ptr_613, i32 0, i32 0
  %2058 = getelementptr [2 x i32], ptr %2056, i32 0, i32 0
  %2059 = load i32, ptr %2058
  store i32 %2059, ptr %2057
  %2060 = getelementptr [2 x i32], ptr %stack.ptr_613, i32 0, i32 1
  store i32 0, ptr %2060
  %2061 = getelementptr [2 x i32], ptr %stack.ptr_614, i32 0, i32 0
  %2062 = getelementptr [2 x i32], ptr %2056, i32 0, i32 0
  %2063 = load i32, ptr %2062
  store i32 %2063, ptr %2061
  %2064 = getelementptr [2 x i32], ptr %stack.ptr_614, i32 0, i32 1
  store i32 4294967295, ptr %2064
  %2065 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2065, ptr %stack.ptr_613, ptr %stack.ptr_615)
  %2066 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2066, ptr %stack.ptr_614, ptr %stack.ptr_616)
  br label %loop_134
loop_134:
  %2067 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_615, ptr %stack.ptr_616)
  br i1 %2067, label %if_160, label %end_if_160
if_160:
  br label %range_query.end_130
end_if_160:
  %2068 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_615)
  %2069 = getelementptr [2 x i32], ptr %stack.ptr_617, i32 0, i32 0
  %2070 = getelementptr [2 x i32], ptr %2068, i32 0, i32 1
  %2071 = load i32, ptr %2070
  store i32 %2071, ptr %2069
  %2072 = getelementptr [2 x i32], ptr %stack.ptr_617, i32 0, i32 1
  store i32 0, ptr %2072
  %2073 = getelementptr [2 x i32], ptr %stack.ptr_618, i32 0, i32 0
  %2074 = getelementptr [2 x i32], ptr %2068, i32 0, i32 1
  %2075 = load i32, ptr %2074
  store i32 %2075, ptr %2073
  %2076 = getelementptr [2 x i32], ptr %stack.ptr_618, i32 0, i32 1
  store i32 4294967295, ptr %2076
  %2077 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_lower_bound_1(ptr %2077, ptr %stack.ptr_617, ptr %stack.ptr_619)
  %2078 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_upper_bound_1(ptr %2078, ptr %stack.ptr_618, ptr %stack.ptr_620)
  br label %loop_135
loop_135:
  %2079 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_619, ptr %stack.ptr_620)
  br i1 %2079, label %if_161, label %end_if_161
if_161:
  br label %range_query.end_131
end_if_161:
  %2080 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_619)
  %2081 = getelementptr [3 x i32], ptr %stack.ptr_621, i32 0, i32 0
  %2082 = getelementptr [2 x i32], ptr %2068, i32 0, i32 1
  %2083 = load i32, ptr %2082
  store i32 %2083, ptr %2081
  %2084 = getelementptr [3 x i32], ptr %stack.ptr_621, i32 0, i32 1
  store i32 0, ptr %2084
  %2085 = getelementptr [3 x i32], ptr %stack.ptr_621, i32 0, i32 2
  store i32 0, ptr %2085
  %2086 = getelementptr [3 x i32], ptr %stack.ptr_622, i32 0, i32 0
  %2087 = getelementptr [2 x i32], ptr %2068, i32 0, i32 1
  %2088 = load i32, ptr %2087
  store i32 %2088, ptr %2086
  %2089 = getelementptr [3 x i32], ptr %stack.ptr_622, i32 0, i32 1
  store i32 4294967295, ptr %2089
  %2090 = getelementptr [3 x i32], ptr %stack.ptr_622, i32 0, i32 2
  store i32 4294967295, ptr %2090
  %2091 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_0(ptr %2091, ptr %stack.ptr_621, ptr %stack.ptr_623)
  %2092 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_0(ptr %2092, ptr %stack.ptr_622, ptr %stack.ptr_624)
  br label %loop_136
loop_136:
  %2093 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_623, ptr %stack.ptr_624)
  br i1 %2093, label %if_162, label %end_if_162
if_162:
  br label %range_query.end_132
end_if_162:
  %2094 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_623)
  %2095 = getelementptr [1 x i32], ptr %stack.ptr_625, i32 0, i32 0
  %2096 = getelementptr [3 x i32], ptr %2094, i32 0, i32 2
  %2097 = load i32, ptr %2096
  store i32 %2097, ptr %2095
  %2098 = getelementptr [1 x i32], ptr %stack.ptr_626, i32 0, i32 0
  %2099 = getelementptr [3 x i32], ptr %2094, i32 0, i32 2
  %2100 = load i32, ptr %2099
  store i32 %2100, ptr %2098
  %2101 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %2101, ptr %stack.ptr_625, ptr %stack.ptr_627)
  %2102 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %2102, ptr %stack.ptr_626, ptr %stack.ptr_628)
  br label %loop_137
loop_137:
  %2103 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_627, ptr %stack.ptr_628)
  br i1 %2103, label %if_163, label %end_if_163
if_163:
  br label %range_query.end_133
end_if_163:
  %2104 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_627)
  %2105 = getelementptr [3 x i32], ptr %stack.ptr_629, i32 0, i32 0
  %2106 = getelementptr [2 x i32], ptr %2068, i32 0, i32 1
  %2107 = load i32, ptr %2106
  store i32 %2107, ptr %2105
  %2108 = getelementptr [3 x i32], ptr %stack.ptr_629, i32 0, i32 1
  %2109 = getelementptr [3 x i32], ptr %2094, i32 0, i32 2
  %2110 = load i32, ptr %2109
  store i32 %2110, ptr %2108
  %2111 = getelementptr [3 x i32], ptr %stack.ptr_629, i32 0, i32 2
  %2112 = getelementptr [3 x i32], ptr %2094, i32 0, i32 1
  %2113 = load i32, ptr %2112
  store i32 %2113, ptr %2111
  %2114 = getelementptr %program, ptr %arg_0, i32 0, i32 71
  %2115 = call ccc i1 @eclair_btree_insert_value_0(ptr %2114, ptr %stack.ptr_629)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_627)
  br label %loop_137
range_query.end_133:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_623)
  br label %loop_136
range_query.end_132:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_619)
  br label %loop_135
range_query.end_131:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_615)
  br label %loop_134
range_query.end_130:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_611)
  br label %loop_133
range_query.end_129:
  %2116 = getelementptr [2 x i32], ptr %stack.ptr_630, i32 0, i32 0
  store i32 0, ptr %2116
  %2117 = getelementptr [2 x i32], ptr %stack.ptr_630, i32 0, i32 1
  store i32 0, ptr %2117
  %2118 = getelementptr [2 x i32], ptr %stack.ptr_631, i32 0, i32 0
  store i32 4294967295, ptr %2118
  %2119 = getelementptr [2 x i32], ptr %stack.ptr_631, i32 0, i32 1
  store i32 4294967295, ptr %2119
  %2120 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_lower_bound_1(ptr %2120, ptr %stack.ptr_630, ptr %stack.ptr_632)
  %2121 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_upper_bound_1(ptr %2121, ptr %stack.ptr_631, ptr %stack.ptr_633)
  br label %loop_138
loop_138:
  %2122 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_632, ptr %stack.ptr_633)
  br i1 %2122, label %if_164, label %end_if_164
if_164:
  br label %range_query.end_134
end_if_164:
  %2123 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_632)
  %2124 = getelementptr [2 x i32], ptr %stack.ptr_634, i32 0, i32 0
  store i32 0, ptr %2124
  %2125 = getelementptr [2 x i32], ptr %stack.ptr_634, i32 0, i32 1
  %2126 = getelementptr [2 x i32], ptr %2123, i32 0, i32 1
  %2127 = load i32, ptr %2126
  store i32 %2127, ptr %2125
  %2128 = getelementptr [2 x i32], ptr %stack.ptr_635, i32 0, i32 0
  store i32 4294967295, ptr %2128
  %2129 = getelementptr [2 x i32], ptr %stack.ptr_635, i32 0, i32 1
  %2130 = getelementptr [2 x i32], ptr %2123, i32 0, i32 1
  %2131 = load i32, ptr %2130
  store i32 %2131, ptr %2129
  %2132 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_lower_bound_2(ptr %2132, ptr %stack.ptr_634, ptr %stack.ptr_636)
  %2133 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_upper_bound_2(ptr %2133, ptr %stack.ptr_635, ptr %stack.ptr_637)
  br label %loop_139
loop_139:
  %2134 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_636, ptr %stack.ptr_637)
  br i1 %2134, label %if_165, label %end_if_165
if_165:
  br label %range_query.end_135
end_if_165:
  %2135 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_636)
  %2136 = getelementptr [2 x i32], ptr %stack.ptr_638, i32 0, i32 0
  %2137 = getelementptr [2 x i32], ptr %2135, i32 0, i32 0
  %2138 = load i32, ptr %2137
  store i32 %2138, ptr %2136
  %2139 = getelementptr [2 x i32], ptr %stack.ptr_638, i32 0, i32 1
  %2140 = getelementptr [2 x i32], ptr %2123, i32 0, i32 1
  %2141 = load i32, ptr %2140
  store i32 %2141, ptr %2139
  %2142 = getelementptr %program, ptr %arg_0, i32 0, i32 49
  %2143 = call ccc i1 @eclair_btree_insert_value_1(ptr %2142, ptr %stack.ptr_638)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_636)
  br label %loop_139
range_query.end_135:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_632)
  br label %loop_138
range_query.end_134:
  %2144 = getelementptr [3 x i32], ptr %stack.ptr_639, i32 0, i32 0
  store i32 0, ptr %2144
  %2145 = getelementptr [3 x i32], ptr %stack.ptr_639, i32 0, i32 1
  store i32 0, ptr %2145
  %2146 = getelementptr [3 x i32], ptr %stack.ptr_639, i32 0, i32 2
  store i32 0, ptr %2146
  %2147 = getelementptr [3 x i32], ptr %stack.ptr_640, i32 0, i32 0
  store i32 4294967295, ptr %2147
  %2148 = getelementptr [3 x i32], ptr %stack.ptr_640, i32 0, i32 1
  store i32 4294967295, ptr %2148
  %2149 = getelementptr [3 x i32], ptr %stack.ptr_640, i32 0, i32 2
  store i32 4294967295, ptr %2149
  %2150 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2150, ptr %stack.ptr_639, ptr %stack.ptr_641)
  %2151 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2151, ptr %stack.ptr_640, ptr %stack.ptr_642)
  br label %loop_140
loop_140:
  %2152 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_641, ptr %stack.ptr_642)
  br i1 %2152, label %if_166, label %end_if_166
if_166:
  br label %range_query.end_136
end_if_166:
  %2153 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_641)
  %2154 = getelementptr [2 x i32], ptr %stack.ptr_643, i32 0, i32 0
  %2155 = getelementptr [3 x i32], ptr %2153, i32 0, i32 2
  %2156 = load i32, ptr %2155
  store i32 %2156, ptr %2154
  %2157 = getelementptr [2 x i32], ptr %stack.ptr_643, i32 0, i32 1
  store i32 0, ptr %2157
  %2158 = getelementptr [2 x i32], ptr %stack.ptr_644, i32 0, i32 0
  %2159 = getelementptr [3 x i32], ptr %2153, i32 0, i32 2
  %2160 = load i32, ptr %2159
  store i32 %2160, ptr %2158
  %2161 = getelementptr [2 x i32], ptr %stack.ptr_644, i32 0, i32 1
  store i32 4294967295, ptr %2161
  %2162 = getelementptr %program, ptr %arg_0, i32 0, i32 49
  call ccc void @eclair_btree_lower_bound_1(ptr %2162, ptr %stack.ptr_643, ptr %stack.ptr_645)
  %2163 = getelementptr %program, ptr %arg_0, i32 0, i32 49
  call ccc void @eclair_btree_upper_bound_1(ptr %2163, ptr %stack.ptr_644, ptr %stack.ptr_646)
  br label %loop_141
loop_141:
  %2164 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_645, ptr %stack.ptr_646)
  br i1 %2164, label %if_167, label %end_if_167
if_167:
  br label %range_query.end_137
end_if_167:
  %2165 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_645)
  %2166 = getelementptr [3 x i32], ptr %stack.ptr_647, i32 0, i32 0
  %2167 = getelementptr [3 x i32], ptr %2153, i32 0, i32 2
  %2168 = load i32, ptr %2167
  store i32 %2168, ptr %2166
  %2169 = getelementptr [3 x i32], ptr %stack.ptr_647, i32 0, i32 1
  store i32 0, ptr %2169
  %2170 = getelementptr [3 x i32], ptr %stack.ptr_647, i32 0, i32 2
  store i32 0, ptr %2170
  %2171 = getelementptr [3 x i32], ptr %stack.ptr_648, i32 0, i32 0
  %2172 = getelementptr [3 x i32], ptr %2153, i32 0, i32 2
  %2173 = load i32, ptr %2172
  store i32 %2173, ptr %2171
  %2174 = getelementptr [3 x i32], ptr %stack.ptr_648, i32 0, i32 1
  store i32 4294967295, ptr %2174
  %2175 = getelementptr [3 x i32], ptr %stack.ptr_648, i32 0, i32 2
  store i32 4294967295, ptr %2175
  %2176 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_0(ptr %2176, ptr %stack.ptr_647, ptr %stack.ptr_649)
  %2177 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_0(ptr %2177, ptr %stack.ptr_648, ptr %stack.ptr_650)
  br label %loop_142
loop_142:
  %2178 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_649, ptr %stack.ptr_650)
  br i1 %2178, label %if_168, label %end_if_168
if_168:
  br label %range_query.end_138
end_if_168:
  %2179 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_649)
  %2180 = getelementptr [2 x i32], ptr %stack.ptr_651, i32 0, i32 0
  %2181 = getelementptr [3 x i32], ptr %2179, i32 0, i32 2
  %2182 = load i32, ptr %2181
  store i32 %2182, ptr %2180
  %2183 = getelementptr [2 x i32], ptr %stack.ptr_651, i32 0, i32 1
  store i32 0, ptr %2183
  %2184 = getelementptr [2 x i32], ptr %stack.ptr_652, i32 0, i32 0
  %2185 = getelementptr [3 x i32], ptr %2179, i32 0, i32 2
  %2186 = load i32, ptr %2185
  store i32 %2186, ptr %2184
  %2187 = getelementptr [2 x i32], ptr %stack.ptr_652, i32 0, i32 1
  store i32 4294967295, ptr %2187
  %2188 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2188, ptr %stack.ptr_651, ptr %stack.ptr_653)
  %2189 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2189, ptr %stack.ptr_652, ptr %stack.ptr_654)
  br label %loop_143
loop_143:
  %2190 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_653, ptr %stack.ptr_654)
  br i1 %2190, label %if_169, label %end_if_169
if_169:
  br label %range_query.end_139
end_if_169:
  %2191 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_653)
  %2192 = getelementptr [2 x i32], ptr %stack.ptr_655, i32 0, i32 0
  %2193 = getelementptr [3 x i32], ptr %2153, i32 0, i32 0
  %2194 = load i32, ptr %2193
  store i32 %2194, ptr %2192
  %2195 = getelementptr [2 x i32], ptr %stack.ptr_655, i32 0, i32 1
  %2196 = getelementptr [3 x i32], ptr %2179, i32 0, i32 2
  %2197 = load i32, ptr %2196
  store i32 %2197, ptr %2195
  %2198 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2199 = call ccc i1 @eclair_btree_insert_value_1(ptr %2198, ptr %stack.ptr_655)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_653)
  br label %loop_143
range_query.end_139:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_649)
  br label %loop_142
range_query.end_138:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_645)
  br label %loop_141
range_query.end_137:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_641)
  br label %loop_140
range_query.end_136:
  %2200 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_begin_1(ptr %2200, ptr %stack.ptr_656)
  %2201 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_end_1(ptr %2201, ptr %stack.ptr_657)
  %2202 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_insert_range_delta_grounded_node_grounded_node(ptr %2202, ptr %stack.ptr_656, ptr %stack.ptr_657)
  br label %loop_144
loop_144:
  %2203 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  call ccc void @eclair_btree_clear_1(ptr %2203)
  %2204 = getelementptr [3 x i32], ptr %stack.ptr_658, i32 0, i32 0
  store i32 0, ptr %2204
  %2205 = getelementptr [3 x i32], ptr %stack.ptr_658, i32 0, i32 1
  store i32 0, ptr %2205
  %2206 = getelementptr [3 x i32], ptr %stack.ptr_658, i32 0, i32 2
  store i32 0, ptr %2206
  %2207 = getelementptr [3 x i32], ptr %stack.ptr_659, i32 0, i32 0
  store i32 4294967295, ptr %2207
  %2208 = getelementptr [3 x i32], ptr %stack.ptr_659, i32 0, i32 1
  store i32 4294967295, ptr %2208
  %2209 = getelementptr [3 x i32], ptr %stack.ptr_659, i32 0, i32 2
  store i32 4294967295, ptr %2209
  %2210 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2210, ptr %stack.ptr_658, ptr %stack.ptr_660)
  %2211 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2211, ptr %stack.ptr_659, ptr %stack.ptr_661)
  br label %loop_145
loop_145:
  %2212 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_660, ptr %stack.ptr_661)
  br i1 %2212, label %if_170, label %end_if_170
if_170:
  br label %range_query.end_140
end_if_170:
  %2213 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_660)
  %2214 = getelementptr [3 x i32], ptr %stack.ptr_662, i32 0, i32 0
  %2215 = getelementptr [3 x i32], ptr %2213, i32 0, i32 2
  %2216 = load i32, ptr %2215
  store i32 %2216, ptr %2214
  %2217 = getelementptr [3 x i32], ptr %stack.ptr_662, i32 0, i32 1
  store i32 0, ptr %2217
  %2218 = getelementptr [3 x i32], ptr %stack.ptr_662, i32 0, i32 2
  store i32 0, ptr %2218
  %2219 = getelementptr [3 x i32], ptr %stack.ptr_663, i32 0, i32 0
  %2220 = getelementptr [3 x i32], ptr %2213, i32 0, i32 2
  %2221 = load i32, ptr %2220
  store i32 %2221, ptr %2219
  %2222 = getelementptr [3 x i32], ptr %stack.ptr_663, i32 0, i32 1
  store i32 4294967295, ptr %2222
  %2223 = getelementptr [3 x i32], ptr %stack.ptr_663, i32 0, i32 2
  store i32 4294967295, ptr %2223
  %2224 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %2224, ptr %stack.ptr_662, ptr %stack.ptr_664)
  %2225 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %2225, ptr %stack.ptr_663, ptr %stack.ptr_665)
  br label %loop_146
loop_146:
  %2226 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_664, ptr %stack.ptr_665)
  br i1 %2226, label %if_171, label %end_if_171
if_171:
  br label %range_query.end_141
end_if_171:
  %2227 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_664)
  %2228 = getelementptr [2 x i32], ptr %stack.ptr_666, i32 0, i32 0
  %2229 = getelementptr [3 x i32], ptr %2213, i32 0, i32 0
  %2230 = load i32, ptr %2229
  store i32 %2230, ptr %2228
  %2231 = getelementptr [2 x i32], ptr %stack.ptr_666, i32 0, i32 1
  %2232 = getelementptr [3 x i32], ptr %2227, i32 0, i32 2
  %2233 = load i32, ptr %2232
  store i32 %2233, ptr %2231
  %2234 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  %2235 = call ccc i1 @eclair_btree_contains_1(ptr %2234, ptr %stack.ptr_666)
  %2236 = select i1 %2235, i1 0, i1 1
  br i1 %2236, label %if_172, label %end_if_175
if_172:
  %2237 = getelementptr [2 x i32], ptr %stack.ptr_667, i32 0, i32 0
  %2238 = getelementptr [3 x i32], ptr %2213, i32 0, i32 0
  %2239 = load i32, ptr %2238
  store i32 %2239, ptr %2237
  %2240 = getelementptr [2 x i32], ptr %stack.ptr_667, i32 0, i32 1
  %2241 = getelementptr [3 x i32], ptr %2227, i32 0, i32 1
  %2242 = load i32, ptr %2241
  store i32 %2242, ptr %2240
  %2243 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2244 = call ccc i1 @eclair_btree_contains_1(ptr %2243, ptr %stack.ptr_667)
  %2245 = select i1 %2244, i1 0, i1 1
  br i1 %2245, label %if_173, label %end_if_174
if_173:
  %2246 = getelementptr [2 x i32], ptr %stack.ptr_668, i32 0, i32 0
  %2247 = getelementptr [3 x i32], ptr %2213, i32 0, i32 0
  %2248 = load i32, ptr %2247
  store i32 %2248, ptr %2246
  %2249 = getelementptr [2 x i32], ptr %stack.ptr_668, i32 0, i32 1
  %2250 = getelementptr [3 x i32], ptr %2227, i32 0, i32 2
  %2251 = load i32, ptr %2250
  store i32 %2251, ptr %2249
  %2252 = getelementptr [2 x i32], ptr %stack.ptr_669, i32 0, i32 0
  %2253 = getelementptr [3 x i32], ptr %2213, i32 0, i32 0
  %2254 = load i32, ptr %2253
  store i32 %2254, ptr %2252
  %2255 = getelementptr [2 x i32], ptr %stack.ptr_669, i32 0, i32 1
  %2256 = getelementptr [3 x i32], ptr %2227, i32 0, i32 2
  %2257 = load i32, ptr %2256
  store i32 %2257, ptr %2255
  %2258 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %2258, ptr %stack.ptr_668, ptr %stack.ptr_670)
  %2259 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %2259, ptr %stack.ptr_669, ptr %stack.ptr_671)
  br label %loop_147
loop_147:
  %2260 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_670, ptr %stack.ptr_671)
  br i1 %2260, label %if_174, label %end_if_172
if_174:
  br label %range_query.end_142
end_if_172:
  %2261 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_670)
  %2262 = getelementptr [2 x i32], ptr %stack.ptr_672, i32 0, i32 0
  %2263 = getelementptr [3 x i32], ptr %2227, i32 0, i32 1
  %2264 = load i32, ptr %2263
  store i32 %2264, ptr %2262
  %2265 = getelementptr [2 x i32], ptr %stack.ptr_672, i32 0, i32 1
  store i32 0, ptr %2265
  %2266 = getelementptr [2 x i32], ptr %stack.ptr_673, i32 0, i32 0
  %2267 = getelementptr [3 x i32], ptr %2227, i32 0, i32 1
  %2268 = load i32, ptr %2267
  store i32 %2268, ptr %2266
  %2269 = getelementptr [2 x i32], ptr %stack.ptr_673, i32 0, i32 1
  store i32 4294967295, ptr %2269
  %2270 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2270, ptr %stack.ptr_672, ptr %stack.ptr_674)
  %2271 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2271, ptr %stack.ptr_673, ptr %stack.ptr_675)
  br label %loop_148
loop_148:
  %2272 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_674, ptr %stack.ptr_675)
  br i1 %2272, label %if_175, label %end_if_173
if_175:
  br label %range_query.end_143
end_if_173:
  %2273 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_674)
  %2274 = getelementptr [2 x i32], ptr %stack.ptr_676, i32 0, i32 0
  %2275 = getelementptr [3 x i32], ptr %2213, i32 0, i32 0
  %2276 = load i32, ptr %2275
  store i32 %2276, ptr %2274
  %2277 = getelementptr [2 x i32], ptr %stack.ptr_676, i32 0, i32 1
  %2278 = getelementptr [3 x i32], ptr %2227, i32 0, i32 1
  %2279 = load i32, ptr %2278
  store i32 %2279, ptr %2277
  %2280 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2281 = call ccc i1 @eclair_btree_insert_value_1(ptr %2280, ptr %stack.ptr_676)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_674)
  br label %loop_148
range_query.end_143:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_670)
  br label %loop_147
range_query.end_142:
  br label %end_if_174
end_if_174:
  br label %end_if_175
end_if_175:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_664)
  br label %loop_146
range_query.end_141:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_660)
  br label %loop_145
range_query.end_140:
  %2282 = getelementptr [3 x i32], ptr %stack.ptr_677, i32 0, i32 0
  store i32 0, ptr %2282
  %2283 = getelementptr [3 x i32], ptr %stack.ptr_677, i32 0, i32 1
  store i32 0, ptr %2283
  %2284 = getelementptr [3 x i32], ptr %stack.ptr_677, i32 0, i32 2
  store i32 0, ptr %2284
  %2285 = getelementptr [3 x i32], ptr %stack.ptr_678, i32 0, i32 0
  store i32 4294967295, ptr %2285
  %2286 = getelementptr [3 x i32], ptr %stack.ptr_678, i32 0, i32 1
  store i32 4294967295, ptr %2286
  %2287 = getelementptr [3 x i32], ptr %stack.ptr_678, i32 0, i32 2
  store i32 4294967295, ptr %2287
  %2288 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2288, ptr %stack.ptr_677, ptr %stack.ptr_679)
  %2289 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2289, ptr %stack.ptr_678, ptr %stack.ptr_680)
  br label %loop_149
loop_149:
  %2290 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_679, ptr %stack.ptr_680)
  br i1 %2290, label %if_176, label %end_if_176
if_176:
  br label %range_query.end_144
end_if_176:
  %2291 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_679)
  %2292 = getelementptr [3 x i32], ptr %stack.ptr_681, i32 0, i32 0
  %2293 = getelementptr [3 x i32], ptr %2291, i32 0, i32 2
  %2294 = load i32, ptr %2293
  store i32 %2294, ptr %2292
  %2295 = getelementptr [3 x i32], ptr %stack.ptr_681, i32 0, i32 1
  store i32 0, ptr %2295
  %2296 = getelementptr [3 x i32], ptr %stack.ptr_681, i32 0, i32 2
  store i32 0, ptr %2296
  %2297 = getelementptr [3 x i32], ptr %stack.ptr_682, i32 0, i32 0
  %2298 = getelementptr [3 x i32], ptr %2291, i32 0, i32 2
  %2299 = load i32, ptr %2298
  store i32 %2299, ptr %2297
  %2300 = getelementptr [3 x i32], ptr %stack.ptr_682, i32 0, i32 1
  store i32 4294967295, ptr %2300
  %2301 = getelementptr [3 x i32], ptr %stack.ptr_682, i32 0, i32 2
  store i32 4294967295, ptr %2301
  %2302 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %2302, ptr %stack.ptr_681, ptr %stack.ptr_683)
  %2303 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %2303, ptr %stack.ptr_682, ptr %stack.ptr_684)
  br label %loop_150
loop_150:
  %2304 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_683, ptr %stack.ptr_684)
  br i1 %2304, label %if_177, label %end_if_177
if_177:
  br label %range_query.end_145
end_if_177:
  %2305 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_683)
  %2306 = getelementptr [2 x i32], ptr %stack.ptr_685, i32 0, i32 0
  %2307 = getelementptr [3 x i32], ptr %2291, i32 0, i32 0
  %2308 = load i32, ptr %2307
  store i32 %2308, ptr %2306
  %2309 = getelementptr [2 x i32], ptr %stack.ptr_685, i32 0, i32 1
  %2310 = getelementptr [3 x i32], ptr %2305, i32 0, i32 1
  %2311 = load i32, ptr %2310
  store i32 %2311, ptr %2309
  %2312 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  %2313 = call ccc i1 @eclair_btree_contains_1(ptr %2312, ptr %stack.ptr_685)
  %2314 = select i1 %2313, i1 0, i1 1
  br i1 %2314, label %if_178, label %end_if_181
if_178:
  %2315 = getelementptr [2 x i32], ptr %stack.ptr_686, i32 0, i32 0
  %2316 = getelementptr [3 x i32], ptr %2291, i32 0, i32 0
  %2317 = load i32, ptr %2316
  store i32 %2317, ptr %2315
  %2318 = getelementptr [2 x i32], ptr %stack.ptr_686, i32 0, i32 1
  %2319 = getelementptr [3 x i32], ptr %2305, i32 0, i32 2
  %2320 = load i32, ptr %2319
  store i32 %2320, ptr %2318
  %2321 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2322 = call ccc i1 @eclair_btree_contains_1(ptr %2321, ptr %stack.ptr_686)
  %2323 = select i1 %2322, i1 0, i1 1
  br i1 %2323, label %if_179, label %end_if_180
if_179:
  %2324 = getelementptr [2 x i32], ptr %stack.ptr_687, i32 0, i32 0
  %2325 = getelementptr [3 x i32], ptr %2291, i32 0, i32 0
  %2326 = load i32, ptr %2325
  store i32 %2326, ptr %2324
  %2327 = getelementptr [2 x i32], ptr %stack.ptr_687, i32 0, i32 1
  %2328 = getelementptr [3 x i32], ptr %2305, i32 0, i32 1
  %2329 = load i32, ptr %2328
  store i32 %2329, ptr %2327
  %2330 = getelementptr [2 x i32], ptr %stack.ptr_688, i32 0, i32 0
  %2331 = getelementptr [3 x i32], ptr %2291, i32 0, i32 0
  %2332 = load i32, ptr %2331
  store i32 %2332, ptr %2330
  %2333 = getelementptr [2 x i32], ptr %stack.ptr_688, i32 0, i32 1
  %2334 = getelementptr [3 x i32], ptr %2305, i32 0, i32 1
  %2335 = load i32, ptr %2334
  store i32 %2335, ptr %2333
  %2336 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %2336, ptr %stack.ptr_687, ptr %stack.ptr_689)
  %2337 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %2337, ptr %stack.ptr_688, ptr %stack.ptr_690)
  br label %loop_151
loop_151:
  %2338 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_689, ptr %stack.ptr_690)
  br i1 %2338, label %if_180, label %end_if_178
if_180:
  br label %range_query.end_146
end_if_178:
  %2339 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_689)
  %2340 = getelementptr [2 x i32], ptr %stack.ptr_691, i32 0, i32 0
  %2341 = getelementptr [3 x i32], ptr %2305, i32 0, i32 2
  %2342 = load i32, ptr %2341
  store i32 %2342, ptr %2340
  %2343 = getelementptr [2 x i32], ptr %stack.ptr_691, i32 0, i32 1
  store i32 0, ptr %2343
  %2344 = getelementptr [2 x i32], ptr %stack.ptr_692, i32 0, i32 0
  %2345 = getelementptr [3 x i32], ptr %2305, i32 0, i32 2
  %2346 = load i32, ptr %2345
  store i32 %2346, ptr %2344
  %2347 = getelementptr [2 x i32], ptr %stack.ptr_692, i32 0, i32 1
  store i32 4294967295, ptr %2347
  %2348 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2348, ptr %stack.ptr_691, ptr %stack.ptr_693)
  %2349 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2349, ptr %stack.ptr_692, ptr %stack.ptr_694)
  br label %loop_152
loop_152:
  %2350 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_693, ptr %stack.ptr_694)
  br i1 %2350, label %if_181, label %end_if_179
if_181:
  br label %range_query.end_147
end_if_179:
  %2351 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_693)
  %2352 = getelementptr [2 x i32], ptr %stack.ptr_695, i32 0, i32 0
  %2353 = getelementptr [3 x i32], ptr %2291, i32 0, i32 0
  %2354 = load i32, ptr %2353
  store i32 %2354, ptr %2352
  %2355 = getelementptr [2 x i32], ptr %stack.ptr_695, i32 0, i32 1
  %2356 = getelementptr [3 x i32], ptr %2305, i32 0, i32 2
  %2357 = load i32, ptr %2356
  store i32 %2357, ptr %2355
  %2358 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2359 = call ccc i1 @eclair_btree_insert_value_1(ptr %2358, ptr %stack.ptr_695)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_693)
  br label %loop_152
range_query.end_147:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_689)
  br label %loop_151
range_query.end_146:
  br label %end_if_180
end_if_180:
  br label %end_if_181
end_if_181:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_683)
  br label %loop_150
range_query.end_145:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_679)
  br label %loop_149
range_query.end_144:
  %2360 = getelementptr [2 x i32], ptr %stack.ptr_696, i32 0, i32 0
  store i32 0, ptr %2360
  %2361 = getelementptr [2 x i32], ptr %stack.ptr_696, i32 0, i32 1
  store i32 0, ptr %2361
  %2362 = getelementptr [2 x i32], ptr %stack.ptr_697, i32 0, i32 0
  store i32 4294967295, ptr %2362
  %2363 = getelementptr [2 x i32], ptr %stack.ptr_697, i32 0, i32 1
  store i32 4294967295, ptr %2363
  %2364 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_lower_bound_1(ptr %2364, ptr %stack.ptr_696, ptr %stack.ptr_698)
  %2365 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_upper_bound_1(ptr %2365, ptr %stack.ptr_697, ptr %stack.ptr_699)
  br label %loop_153
loop_153:
  %2366 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_698, ptr %stack.ptr_699)
  br i1 %2366, label %if_182, label %end_if_182
if_182:
  br label %range_query.end_148
end_if_182:
  %2367 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_698)
  %2368 = getelementptr [2 x i32], ptr %stack.ptr_700, i32 0, i32 0
  %2369 = getelementptr [2 x i32], ptr %2367, i32 0, i32 0
  %2370 = load i32, ptr %2369
  store i32 %2370, ptr %2368
  %2371 = getelementptr [2 x i32], ptr %stack.ptr_700, i32 0, i32 1
  store i32 0, ptr %2371
  %2372 = getelementptr [2 x i32], ptr %stack.ptr_701, i32 0, i32 0
  %2373 = getelementptr [2 x i32], ptr %2367, i32 0, i32 0
  %2374 = load i32, ptr %2373
  store i32 %2374, ptr %2372
  %2375 = getelementptr [2 x i32], ptr %stack.ptr_701, i32 0, i32 1
  store i32 4294967295, ptr %2375
  %2376 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %2376, ptr %stack.ptr_700, ptr %stack.ptr_702)
  %2377 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %2377, ptr %stack.ptr_701, ptr %stack.ptr_703)
  br label %loop_154
loop_154:
  %2378 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_702, ptr %stack.ptr_703)
  br i1 %2378, label %if_183, label %end_if_183
if_183:
  br label %range_query.end_149
end_if_183:
  %2379 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_702)
  %2380 = getelementptr [2 x i32], ptr %stack.ptr_704, i32 0, i32 0
  %2381 = getelementptr [2 x i32], ptr %2367, i32 0, i32 0
  %2382 = load i32, ptr %2381
  store i32 %2382, ptr %2380
  %2383 = getelementptr [2 x i32], ptr %stack.ptr_704, i32 0, i32 1
  %2384 = getelementptr [2 x i32], ptr %2379, i32 0, i32 1
  %2385 = load i32, ptr %2384
  store i32 %2385, ptr %2383
  %2386 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  %2387 = call ccc i1 @eclair_btree_contains_1(ptr %2386, ptr %stack.ptr_704)
  %2388 = select i1 %2387, i1 0, i1 1
  br i1 %2388, label %if_184, label %end_if_186
if_184:
  %2389 = getelementptr [4 x i32], ptr %stack.ptr_705, i32 0, i32 0
  store i32 0, ptr %2389
  %2390 = getelementptr [4 x i32], ptr %stack.ptr_705, i32 0, i32 1
  store i32 0, ptr %2390
  %2391 = getelementptr [4 x i32], ptr %stack.ptr_705, i32 0, i32 2
  %2392 = getelementptr [2 x i32], ptr %2367, i32 0, i32 1
  %2393 = load i32, ptr %2392
  store i32 %2393, ptr %2391
  %2394 = getelementptr [4 x i32], ptr %stack.ptr_705, i32 0, i32 3
  %2395 = getelementptr [2 x i32], ptr %2379, i32 0, i32 1
  %2396 = load i32, ptr %2395
  store i32 %2396, ptr %2394
  %2397 = getelementptr [4 x i32], ptr %stack.ptr_706, i32 0, i32 0
  store i32 4294967295, ptr %2397
  %2398 = getelementptr [4 x i32], ptr %stack.ptr_706, i32 0, i32 1
  store i32 4294967295, ptr %2398
  %2399 = getelementptr [4 x i32], ptr %stack.ptr_706, i32 0, i32 2
  %2400 = getelementptr [2 x i32], ptr %2367, i32 0, i32 1
  %2401 = load i32, ptr %2400
  store i32 %2401, ptr %2399
  %2402 = getelementptr [4 x i32], ptr %stack.ptr_706, i32 0, i32 3
  %2403 = getelementptr [2 x i32], ptr %2379, i32 0, i32 1
  %2404 = load i32, ptr %2403
  store i32 %2404, ptr %2402
  %2405 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %2405, ptr %stack.ptr_705, ptr %stack.ptr_707)
  %2406 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %2406, ptr %stack.ptr_706, ptr %stack.ptr_708)
  br label %loop_155
loop_155:
  %2407 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_707, ptr %stack.ptr_708)
  br i1 %2407, label %if_185, label %end_if_184
if_185:
  br label %range_query.end_150
end_if_184:
  %2408 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_707)
  %2409 = getelementptr [2 x i32], ptr %stack.ptr_709, i32 0, i32 0
  %2410 = getelementptr [2 x i32], ptr %2367, i32 0, i32 0
  %2411 = load i32, ptr %2410
  store i32 %2411, ptr %2409
  %2412 = getelementptr [2 x i32], ptr %stack.ptr_709, i32 0, i32 1
  %2413 = getelementptr [4 x i32], ptr %2408, i32 0, i32 0
  %2414 = load i32, ptr %2413
  store i32 %2414, ptr %2412
  %2415 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2416 = call ccc i1 @eclair_btree_contains_1(ptr %2415, ptr %stack.ptr_709)
  %2417 = select i1 %2416, i1 0, i1 1
  br i1 %2417, label %if_186, label %end_if_185
if_186:
  %2418 = getelementptr [2 x i32], ptr %stack.ptr_710, i32 0, i32 0
  %2419 = getelementptr [2 x i32], ptr %2367, i32 0, i32 0
  %2420 = load i32, ptr %2419
  store i32 %2420, ptr %2418
  %2421 = getelementptr [2 x i32], ptr %stack.ptr_710, i32 0, i32 1
  %2422 = getelementptr [4 x i32], ptr %2408, i32 0, i32 0
  %2423 = load i32, ptr %2422
  store i32 %2423, ptr %2421
  %2424 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2425 = call ccc i1 @eclair_btree_insert_value_1(ptr %2424, ptr %stack.ptr_710)
  br label %end_if_185
end_if_185:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_707)
  br label %loop_155
range_query.end_150:
  br label %end_if_186
end_if_186:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_702)
  br label %loop_154
range_query.end_149:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_698)
  br label %loop_153
range_query.end_148:
  %2426 = getelementptr [2 x i32], ptr %stack.ptr_711, i32 0, i32 0
  store i32 0, ptr %2426
  %2427 = getelementptr [2 x i32], ptr %stack.ptr_711, i32 0, i32 1
  store i32 0, ptr %2427
  %2428 = getelementptr [2 x i32], ptr %stack.ptr_712, i32 0, i32 0
  store i32 4294967295, ptr %2428
  %2429 = getelementptr [2 x i32], ptr %stack.ptr_712, i32 0, i32 1
  store i32 4294967295, ptr %2429
  %2430 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %2430, ptr %stack.ptr_711, ptr %stack.ptr_713)
  %2431 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %2431, ptr %stack.ptr_712, ptr %stack.ptr_714)
  br label %loop_156
loop_156:
  %2432 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_713, ptr %stack.ptr_714)
  br i1 %2432, label %if_187, label %end_if_187
if_187:
  br label %range_query.end_151
end_if_187:
  %2433 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_713)
  %2434 = getelementptr [3 x i32], ptr %stack.ptr_715, i32 0, i32 0
  store i32 0, ptr %2434
  %2435 = getelementptr [3 x i32], ptr %stack.ptr_715, i32 0, i32 1
  store i32 0, ptr %2435
  %2436 = getelementptr [3 x i32], ptr %stack.ptr_715, i32 0, i32 2
  %2437 = getelementptr [2 x i32], ptr %2433, i32 0, i32 0
  %2438 = load i32, ptr %2437
  store i32 %2438, ptr %2436
  %2439 = getelementptr [3 x i32], ptr %stack.ptr_716, i32 0, i32 0
  store i32 4294967295, ptr %2439
  %2440 = getelementptr [3 x i32], ptr %stack.ptr_716, i32 0, i32 1
  store i32 4294967295, ptr %2440
  %2441 = getelementptr [3 x i32], ptr %stack.ptr_716, i32 0, i32 2
  %2442 = getelementptr [2 x i32], ptr %2433, i32 0, i32 0
  %2443 = load i32, ptr %2442
  store i32 %2443, ptr %2441
  %2444 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_lower_bound_8(ptr %2444, ptr %stack.ptr_715, ptr %stack.ptr_717)
  %2445 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_upper_bound_8(ptr %2445, ptr %stack.ptr_716, ptr %stack.ptr_718)
  br label %loop_157
loop_157:
  %2446 = call ccc i1 @eclair_btree_iterator_is_equal_8(ptr %stack.ptr_717, ptr %stack.ptr_718)
  br i1 %2446, label %if_188, label %end_if_188
if_188:
  br label %range_query.end_152
end_if_188:
  %2447 = call ccc ptr @eclair_btree_iterator_current_8(ptr %stack.ptr_717)
  %2448 = getelementptr [2 x i32], ptr %stack.ptr_719, i32 0, i32 0
  %2449 = getelementptr [3 x i32], ptr %2447, i32 0, i32 0
  %2450 = load i32, ptr %2449
  store i32 %2450, ptr %2448
  %2451 = getelementptr [2 x i32], ptr %stack.ptr_719, i32 0, i32 1
  store i32 0, ptr %2451
  %2452 = getelementptr [2 x i32], ptr %stack.ptr_720, i32 0, i32 0
  %2453 = getelementptr [3 x i32], ptr %2447, i32 0, i32 0
  %2454 = load i32, ptr %2453
  store i32 %2454, ptr %2452
  %2455 = getelementptr [2 x i32], ptr %stack.ptr_720, i32 0, i32 1
  store i32 4294967295, ptr %2455
  %2456 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2456, ptr %stack.ptr_719, ptr %stack.ptr_721)
  %2457 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2457, ptr %stack.ptr_720, ptr %stack.ptr_722)
  br label %loop_158
loop_158:
  %2458 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_721, ptr %stack.ptr_722)
  br i1 %2458, label %if_189, label %end_if_189
if_189:
  br label %range_query.end_153
end_if_189:
  %2459 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_721)
  %2460 = getelementptr [2 x i32], ptr %stack.ptr_723, i32 0, i32 0
  %2461 = getelementptr [3 x i32], ptr %2447, i32 0, i32 0
  %2462 = load i32, ptr %2461
  store i32 %2462, ptr %2460
  %2463 = getelementptr [2 x i32], ptr %stack.ptr_723, i32 0, i32 1
  %2464 = getelementptr [2 x i32], ptr %2459, i32 0, i32 1
  %2465 = load i32, ptr %2464
  store i32 %2465, ptr %2463
  %2466 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  %2467 = call ccc i1 @eclair_btree_contains_1(ptr %2466, ptr %stack.ptr_723)
  %2468 = select i1 %2467, i1 0, i1 1
  br i1 %2468, label %if_190, label %end_if_195
if_190:
  %2469 = getelementptr [2 x i32], ptr %stack.ptr_724, i32 0, i32 0
  %2470 = getelementptr [2 x i32], ptr %2459, i32 0, i32 1
  %2471 = load i32, ptr %2470
  store i32 %2471, ptr %2469
  %2472 = getelementptr [2 x i32], ptr %stack.ptr_724, i32 0, i32 1
  store i32 0, ptr %2472
  %2473 = getelementptr [2 x i32], ptr %stack.ptr_725, i32 0, i32 0
  %2474 = getelementptr [2 x i32], ptr %2459, i32 0, i32 1
  %2475 = load i32, ptr %2474
  store i32 %2475, ptr %2473
  %2476 = getelementptr [2 x i32], ptr %stack.ptr_725, i32 0, i32 1
  store i32 4294967295, ptr %2476
  %2477 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2477, ptr %stack.ptr_724, ptr %stack.ptr_726)
  %2478 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2478, ptr %stack.ptr_725, ptr %stack.ptr_727)
  br label %loop_159
loop_159:
  %2479 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_726, ptr %stack.ptr_727)
  br i1 %2479, label %if_191, label %end_if_190
if_191:
  br label %range_query.end_154
end_if_190:
  %2480 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_726)
  %2481 = getelementptr [2 x i32], ptr %stack.ptr_728, i32 0, i32 0
  %2482 = getelementptr [3 x i32], ptr %2447, i32 0, i32 0
  %2483 = load i32, ptr %2482
  store i32 %2483, ptr %2481
  %2484 = getelementptr [2 x i32], ptr %stack.ptr_728, i32 0, i32 1
  %2485 = getelementptr [2 x i32], ptr %2459, i32 0, i32 1
  %2486 = load i32, ptr %2485
  store i32 %2486, ptr %2484
  %2487 = getelementptr [2 x i32], ptr %stack.ptr_729, i32 0, i32 0
  %2488 = getelementptr [3 x i32], ptr %2447, i32 0, i32 0
  %2489 = load i32, ptr %2488
  store i32 %2489, ptr %2487
  %2490 = getelementptr [2 x i32], ptr %stack.ptr_729, i32 0, i32 1
  %2491 = getelementptr [2 x i32], ptr %2459, i32 0, i32 1
  %2492 = load i32, ptr %2491
  store i32 %2492, ptr %2490
  %2493 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %2493, ptr %stack.ptr_728, ptr %stack.ptr_730)
  %2494 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %2494, ptr %stack.ptr_729, ptr %stack.ptr_731)
  br label %loop_160
loop_160:
  %2495 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_730, ptr %stack.ptr_731)
  br i1 %2495, label %if_192, label %end_if_191
if_192:
  br label %range_query.end_155
end_if_191:
  %2496 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_730)
  %2497 = getelementptr [2 x i32], ptr %stack.ptr_732, i32 0, i32 0
  %2498 = getelementptr [2 x i32], ptr %2433, i32 0, i32 0
  %2499 = load i32, ptr %2498
  store i32 %2499, ptr %2497
  %2500 = getelementptr [2 x i32], ptr %stack.ptr_732, i32 0, i32 1
  store i32 0, ptr %2500
  %2501 = getelementptr [2 x i32], ptr %stack.ptr_733, i32 0, i32 0
  %2502 = getelementptr [2 x i32], ptr %2433, i32 0, i32 0
  %2503 = load i32, ptr %2502
  store i32 %2503, ptr %2501
  %2504 = getelementptr [2 x i32], ptr %stack.ptr_733, i32 0, i32 1
  store i32 4294967295, ptr %2504
  %2505 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2505, ptr %stack.ptr_732, ptr %stack.ptr_734)
  %2506 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2506, ptr %stack.ptr_733, ptr %stack.ptr_735)
  br label %loop_161
loop_161:
  %2507 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_734, ptr %stack.ptr_735)
  br i1 %2507, label %if_193, label %end_if_192
if_193:
  br label %range_query.end_156
end_if_192:
  %2508 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_734)
  %2509 = getelementptr [2 x i32], ptr %stack.ptr_736, i32 0, i32 0
  %2510 = getelementptr [3 x i32], ptr %2447, i32 0, i32 0
  %2511 = load i32, ptr %2510
  store i32 %2511, ptr %2509
  %2512 = getelementptr [2 x i32], ptr %stack.ptr_736, i32 0, i32 1
  %2513 = getelementptr [2 x i32], ptr %2508, i32 0, i32 1
  %2514 = load i32, ptr %2513
  store i32 %2514, ptr %2512
  %2515 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2516 = call ccc i1 @eclair_btree_contains_1(ptr %2515, ptr %stack.ptr_736)
  %2517 = select i1 %2516, i1 0, i1 1
  br i1 %2517, label %if_194, label %end_if_194
if_194:
  %2518 = getelementptr [2 x i32], ptr %stack.ptr_737, i32 0, i32 0
  %2519 = getelementptr [2 x i32], ptr %2508, i32 0, i32 1
  %2520 = load i32, ptr %2519
  store i32 %2520, ptr %2518
  %2521 = getelementptr [2 x i32], ptr %stack.ptr_737, i32 0, i32 1
  %2522 = getelementptr [2 x i32], ptr %2480, i32 0, i32 1
  %2523 = load i32, ptr %2522
  store i32 %2523, ptr %2521
  %2524 = getelementptr [2 x i32], ptr %stack.ptr_738, i32 0, i32 0
  %2525 = getelementptr [2 x i32], ptr %2508, i32 0, i32 1
  %2526 = load i32, ptr %2525
  store i32 %2526, ptr %2524
  %2527 = getelementptr [2 x i32], ptr %stack.ptr_738, i32 0, i32 1
  %2528 = getelementptr [2 x i32], ptr %2480, i32 0, i32 1
  %2529 = load i32, ptr %2528
  store i32 %2529, ptr %2527
  %2530 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2530, ptr %stack.ptr_737, ptr %stack.ptr_739)
  %2531 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2531, ptr %stack.ptr_738, ptr %stack.ptr_740)
  br label %loop_162
loop_162:
  %2532 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_739, ptr %stack.ptr_740)
  br i1 %2532, label %if_195, label %end_if_193
if_195:
  br label %range_query.end_157
end_if_193:
  %2533 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_739)
  %2534 = getelementptr [2 x i32], ptr %stack.ptr_741, i32 0, i32 0
  %2535 = getelementptr [3 x i32], ptr %2447, i32 0, i32 0
  %2536 = load i32, ptr %2535
  store i32 %2536, ptr %2534
  %2537 = getelementptr [2 x i32], ptr %stack.ptr_741, i32 0, i32 1
  %2538 = getelementptr [2 x i32], ptr %2508, i32 0, i32 1
  %2539 = load i32, ptr %2538
  store i32 %2539, ptr %2537
  %2540 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2541 = call ccc i1 @eclair_btree_insert_value_1(ptr %2540, ptr %stack.ptr_741)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_739)
  br label %loop_162
range_query.end_157:
  br label %end_if_194
end_if_194:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_734)
  br label %loop_161
range_query.end_156:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_730)
  br label %loop_160
range_query.end_155:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_726)
  br label %loop_159
range_query.end_154:
  br label %end_if_195
end_if_195:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_721)
  br label %loop_158
range_query.end_153:
  call ccc void @eclair_btree_iterator_next_8(ptr %stack.ptr_717)
  br label %loop_157
range_query.end_152:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_713)
  br label %loop_156
range_query.end_151:
  %2542 = getelementptr [2 x i32], ptr %stack.ptr_742, i32 0, i32 0
  store i32 0, ptr %2542
  %2543 = getelementptr [2 x i32], ptr %stack.ptr_742, i32 0, i32 1
  store i32 0, ptr %2543
  %2544 = getelementptr [2 x i32], ptr %stack.ptr_743, i32 0, i32 0
  store i32 4294967295, ptr %2544
  %2545 = getelementptr [2 x i32], ptr %stack.ptr_743, i32 0, i32 1
  store i32 4294967295, ptr %2545
  %2546 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2546, ptr %stack.ptr_742, ptr %stack.ptr_744)
  %2547 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2547, ptr %stack.ptr_743, ptr %stack.ptr_745)
  br label %loop_163
loop_163:
  %2548 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_744, ptr %stack.ptr_745)
  br i1 %2548, label %if_196, label %end_if_196
if_196:
  br label %range_query.end_158
end_if_196:
  %2549 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_744)
  %2550 = getelementptr [2 x i32], ptr %stack.ptr_746, i32 0, i32 0
  %2551 = getelementptr [2 x i32], ptr %2549, i32 0, i32 0
  %2552 = load i32, ptr %2551
  store i32 %2552, ptr %2550
  %2553 = getelementptr [2 x i32], ptr %stack.ptr_746, i32 0, i32 1
  %2554 = getelementptr [2 x i32], ptr %2549, i32 0, i32 1
  %2555 = load i32, ptr %2554
  store i32 %2555, ptr %2553
  %2556 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  %2557 = call ccc i1 @eclair_btree_contains_1(ptr %2556, ptr %stack.ptr_746)
  %2558 = select i1 %2557, i1 0, i1 1
  br i1 %2558, label %if_197, label %end_if_202
if_197:
  %2559 = getelementptr [2 x i32], ptr %stack.ptr_747, i32 0, i32 0
  %2560 = getelementptr [2 x i32], ptr %2549, i32 0, i32 1
  %2561 = load i32, ptr %2560
  store i32 %2561, ptr %2559
  %2562 = getelementptr [2 x i32], ptr %stack.ptr_747, i32 0, i32 1
  store i32 0, ptr %2562
  %2563 = getelementptr [2 x i32], ptr %stack.ptr_748, i32 0, i32 0
  %2564 = getelementptr [2 x i32], ptr %2549, i32 0, i32 1
  %2565 = load i32, ptr %2564
  store i32 %2565, ptr %2563
  %2566 = getelementptr [2 x i32], ptr %stack.ptr_748, i32 0, i32 1
  store i32 4294967295, ptr %2566
  %2567 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2567, ptr %stack.ptr_747, ptr %stack.ptr_749)
  %2568 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2568, ptr %stack.ptr_748, ptr %stack.ptr_750)
  br label %loop_164
loop_164:
  %2569 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_749, ptr %stack.ptr_750)
  br i1 %2569, label %if_198, label %end_if_197
if_198:
  br label %range_query.end_159
end_if_197:
  %2570 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_749)
  %2571 = getelementptr [2 x i32], ptr %stack.ptr_751, i32 0, i32 0
  %2572 = getelementptr [2 x i32], ptr %2549, i32 0, i32 0
  %2573 = load i32, ptr %2572
  store i32 %2573, ptr %2571
  %2574 = getelementptr [2 x i32], ptr %stack.ptr_751, i32 0, i32 1
  %2575 = getelementptr [2 x i32], ptr %2549, i32 0, i32 1
  %2576 = load i32, ptr %2575
  store i32 %2576, ptr %2574
  %2577 = getelementptr [2 x i32], ptr %stack.ptr_752, i32 0, i32 0
  %2578 = getelementptr [2 x i32], ptr %2549, i32 0, i32 0
  %2579 = load i32, ptr %2578
  store i32 %2579, ptr %2577
  %2580 = getelementptr [2 x i32], ptr %stack.ptr_752, i32 0, i32 1
  %2581 = getelementptr [2 x i32], ptr %2549, i32 0, i32 1
  %2582 = load i32, ptr %2581
  store i32 %2582, ptr %2580
  %2583 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %2583, ptr %stack.ptr_751, ptr %stack.ptr_753)
  %2584 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %2584, ptr %stack.ptr_752, ptr %stack.ptr_754)
  br label %loop_165
loop_165:
  %2585 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_753, ptr %stack.ptr_754)
  br i1 %2585, label %if_199, label %end_if_198
if_199:
  br label %range_query.end_160
end_if_198:
  %2586 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_753)
  %2587 = getelementptr [2 x i32], ptr %stack.ptr_755, i32 0, i32 0
  %2588 = getelementptr [2 x i32], ptr %2549, i32 0, i32 0
  %2589 = load i32, ptr %2588
  store i32 %2589, ptr %2587
  %2590 = getelementptr [2 x i32], ptr %stack.ptr_755, i32 0, i32 1
  store i32 0, ptr %2590
  %2591 = getelementptr [2 x i32], ptr %stack.ptr_756, i32 0, i32 0
  %2592 = getelementptr [2 x i32], ptr %2549, i32 0, i32 0
  %2593 = load i32, ptr %2592
  store i32 %2593, ptr %2591
  %2594 = getelementptr [2 x i32], ptr %stack.ptr_756, i32 0, i32 1
  store i32 4294967295, ptr %2594
  %2595 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2595, ptr %stack.ptr_755, ptr %stack.ptr_757)
  %2596 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2596, ptr %stack.ptr_756, ptr %stack.ptr_758)
  br label %loop_166
loop_166:
  %2597 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_757, ptr %stack.ptr_758)
  br i1 %2597, label %if_200, label %end_if_199
if_200:
  br label %range_query.end_161
end_if_199:
  %2598 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_757)
  %2599 = getelementptr [2 x i32], ptr %stack.ptr_759, i32 0, i32 0
  %2600 = getelementptr [2 x i32], ptr %2549, i32 0, i32 0
  %2601 = load i32, ptr %2600
  store i32 %2601, ptr %2599
  %2602 = getelementptr [2 x i32], ptr %stack.ptr_759, i32 0, i32 1
  %2603 = getelementptr [2 x i32], ptr %2598, i32 0, i32 1
  %2604 = load i32, ptr %2603
  store i32 %2604, ptr %2602
  %2605 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2606 = call ccc i1 @eclair_btree_contains_1(ptr %2605, ptr %stack.ptr_759)
  %2607 = select i1 %2606, i1 0, i1 1
  br i1 %2607, label %if_201, label %end_if_201
if_201:
  %2608 = getelementptr [2 x i32], ptr %stack.ptr_760, i32 0, i32 0
  %2609 = getelementptr [2 x i32], ptr %2598, i32 0, i32 1
  %2610 = load i32, ptr %2609
  store i32 %2610, ptr %2608
  %2611 = getelementptr [2 x i32], ptr %stack.ptr_760, i32 0, i32 1
  %2612 = getelementptr [2 x i32], ptr %2570, i32 0, i32 1
  %2613 = load i32, ptr %2612
  store i32 %2613, ptr %2611
  %2614 = getelementptr [2 x i32], ptr %stack.ptr_761, i32 0, i32 0
  %2615 = getelementptr [2 x i32], ptr %2598, i32 0, i32 1
  %2616 = load i32, ptr %2615
  store i32 %2616, ptr %2614
  %2617 = getelementptr [2 x i32], ptr %stack.ptr_761, i32 0, i32 1
  %2618 = getelementptr [2 x i32], ptr %2570, i32 0, i32 1
  %2619 = load i32, ptr %2618
  store i32 %2619, ptr %2617
  %2620 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2620, ptr %stack.ptr_760, ptr %stack.ptr_762)
  %2621 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2621, ptr %stack.ptr_761, ptr %stack.ptr_763)
  br label %loop_167
loop_167:
  %2622 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_762, ptr %stack.ptr_763)
  br i1 %2622, label %if_202, label %end_if_200
if_202:
  br label %range_query.end_162
end_if_200:
  %2623 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_762)
  %2624 = getelementptr [2 x i32], ptr %stack.ptr_764, i32 0, i32 0
  %2625 = getelementptr [2 x i32], ptr %2549, i32 0, i32 0
  %2626 = load i32, ptr %2625
  store i32 %2626, ptr %2624
  %2627 = getelementptr [2 x i32], ptr %stack.ptr_764, i32 0, i32 1
  %2628 = getelementptr [2 x i32], ptr %2598, i32 0, i32 1
  %2629 = load i32, ptr %2628
  store i32 %2629, ptr %2627
  %2630 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2631 = call ccc i1 @eclair_btree_insert_value_1(ptr %2630, ptr %stack.ptr_764)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_762)
  br label %loop_167
range_query.end_162:
  br label %end_if_201
end_if_201:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_757)
  br label %loop_166
range_query.end_161:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_753)
  br label %loop_165
range_query.end_160:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_749)
  br label %loop_164
range_query.end_159:
  br label %end_if_202
end_if_202:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_744)
  br label %loop_163
range_query.end_158:
  %2632 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2633 = call ccc i1 @eclair_btree_is_empty_1(ptr %2632)
  br i1 %2633, label %if_203, label %end_if_203
if_203:
  br label %loop.end_4
end_if_203:
  %2634 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  call ccc void @eclair_btree_begin_1(ptr %2634, ptr %stack.ptr_765)
  %2635 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  call ccc void @eclair_btree_end_1(ptr %2635, ptr %stack.ptr_766)
  %2636 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_insert_range_grounded_node_new_grounded_node(ptr %2636, ptr %stack.ptr_765, ptr %stack.ptr_766)
  %2637 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2638 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_swap_1(ptr %2637, ptr %2638)
  br label %loop_144
loop.end_4:
  %2639 = getelementptr [2 x i32], ptr %stack.ptr_767, i32 0, i32 0
  store i32 0, ptr %2639
  %2640 = getelementptr [2 x i32], ptr %stack.ptr_767, i32 0, i32 1
  store i32 0, ptr %2640
  %2641 = getelementptr [2 x i32], ptr %stack.ptr_768, i32 0, i32 0
  store i32 4294967295, ptr %2641
  %2642 = getelementptr [2 x i32], ptr %stack.ptr_768, i32 0, i32 1
  store i32 4294967295, ptr %2642
  %2643 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %2643, ptr %stack.ptr_767, ptr %stack.ptr_769)
  %2644 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %2644, ptr %stack.ptr_768, ptr %stack.ptr_770)
  br label %loop_168
loop_168:
  %2645 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_769, ptr %stack.ptr_770)
  br i1 %2645, label %if_204, label %end_if_204
if_204:
  br label %range_query.end_163
end_if_204:
  %2646 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_769)
  %2647 = getelementptr [3 x i32], ptr %stack.ptr_771, i32 0, i32 0
  store i32 0, ptr %2647
  %2648 = getelementptr [3 x i32], ptr %stack.ptr_771, i32 0, i32 1
  store i32 0, ptr %2648
  %2649 = getelementptr [3 x i32], ptr %stack.ptr_771, i32 0, i32 2
  %2650 = getelementptr [2 x i32], ptr %2646, i32 0, i32 0
  %2651 = load i32, ptr %2650
  store i32 %2651, ptr %2649
  %2652 = getelementptr [3 x i32], ptr %stack.ptr_772, i32 0, i32 0
  store i32 4294967295, ptr %2652
  %2653 = getelementptr [3 x i32], ptr %stack.ptr_772, i32 0, i32 1
  store i32 4294967295, ptr %2653
  %2654 = getelementptr [3 x i32], ptr %stack.ptr_772, i32 0, i32 2
  %2655 = getelementptr [2 x i32], ptr %2646, i32 0, i32 0
  %2656 = load i32, ptr %2655
  store i32 %2656, ptr %2654
  %2657 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_lower_bound_8(ptr %2657, ptr %stack.ptr_771, ptr %stack.ptr_773)
  %2658 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_upper_bound_8(ptr %2658, ptr %stack.ptr_772, ptr %stack.ptr_774)
  br label %loop_169
loop_169:
  %2659 = call ccc i1 @eclair_btree_iterator_is_equal_8(ptr %stack.ptr_773, ptr %stack.ptr_774)
  br i1 %2659, label %if_205, label %end_if_205
if_205:
  br label %range_query.end_164
end_if_205:
  %2660 = call ccc ptr @eclair_btree_iterator_current_8(ptr %stack.ptr_773)
  %2661 = getelementptr [2 x i32], ptr %stack.ptr_775, i32 0, i32 0
  %2662 = getelementptr [2 x i32], ptr %2646, i32 0, i32 0
  %2663 = load i32, ptr %2662
  store i32 %2663, ptr %2661
  %2664 = getelementptr [2 x i32], ptr %stack.ptr_775, i32 0, i32 1
  store i32 0, ptr %2664
  %2665 = getelementptr [2 x i32], ptr %stack.ptr_776, i32 0, i32 0
  %2666 = getelementptr [2 x i32], ptr %2646, i32 0, i32 0
  %2667 = load i32, ptr %2666
  store i32 %2667, ptr %2665
  %2668 = getelementptr [2 x i32], ptr %stack.ptr_776, i32 0, i32 1
  store i32 4294967295, ptr %2668
  %2669 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2669, ptr %stack.ptr_775, ptr %stack.ptr_777)
  %2670 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2670, ptr %stack.ptr_776, ptr %stack.ptr_778)
  br label %loop_170
loop_170:
  %2671 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_777, ptr %stack.ptr_778)
  br i1 %2671, label %if_206, label %end_if_206
if_206:
  br label %range_query.end_165
end_if_206:
  %2672 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_777)
  %2673 = getelementptr [2 x i32], ptr %stack.ptr_779, i32 0, i32 0
  %2674 = getelementptr [3 x i32], ptr %2660, i32 0, i32 0
  %2675 = load i32, ptr %2674
  store i32 %2675, ptr %2673
  %2676 = getelementptr [2 x i32], ptr %stack.ptr_779, i32 0, i32 1
  %2677 = getelementptr [2 x i32], ptr %2672, i32 0, i32 1
  %2678 = load i32, ptr %2677
  store i32 %2678, ptr %2676
  %2679 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2680 = call ccc i1 @eclair_btree_contains_1(ptr %2679, ptr %stack.ptr_779)
  %2681 = select i1 %2680, i1 0, i1 1
  br i1 %2681, label %if_207, label %end_if_208
if_207:
  %2682 = getelementptr [2 x i32], ptr %stack.ptr_780, i32 0, i32 0
  %2683 = getelementptr [2 x i32], ptr %2672, i32 0, i32 1
  %2684 = load i32, ptr %2683
  store i32 %2684, ptr %2682
  %2685 = getelementptr [2 x i32], ptr %stack.ptr_780, i32 0, i32 1
  store i32 0, ptr %2685
  %2686 = getelementptr [2 x i32], ptr %stack.ptr_781, i32 0, i32 0
  %2687 = getelementptr [2 x i32], ptr %2672, i32 0, i32 1
  %2688 = load i32, ptr %2687
  store i32 %2688, ptr %2686
  %2689 = getelementptr [2 x i32], ptr %stack.ptr_781, i32 0, i32 1
  store i32 4294967295, ptr %2689
  %2690 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2690, ptr %stack.ptr_780, ptr %stack.ptr_782)
  %2691 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2691, ptr %stack.ptr_781, ptr %stack.ptr_783)
  br label %loop_171
loop_171:
  %2692 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_782, ptr %stack.ptr_783)
  br i1 %2692, label %if_208, label %end_if_207
if_208:
  br label %range_query.end_166
end_if_207:
  %2693 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_782)
  %2694 = getelementptr [3 x i32], ptr %stack.ptr_784, i32 0, i32 0
  %2695 = getelementptr [3 x i32], ptr %2660, i32 0, i32 0
  %2696 = load i32, ptr %2695
  store i32 %2696, ptr %2694
  %2697 = getelementptr [3 x i32], ptr %stack.ptr_784, i32 0, i32 1
  %2698 = getelementptr [2 x i32], ptr %2672, i32 0, i32 1
  %2699 = load i32, ptr %2698
  store i32 %2699, ptr %2697
  %2700 = getelementptr [3 x i32], ptr %stack.ptr_784, i32 0, i32 2
  %2701 = getelementptr [2 x i32], ptr %2693, i32 0, i32 1
  %2702 = load i32, ptr %2701
  store i32 %2702, ptr %2700
  %2703 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %2704 = call ccc i1 @eclair_btree_insert_value_0(ptr %2703, ptr %stack.ptr_784)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_782)
  br label %loop_171
range_query.end_166:
  br label %end_if_208
end_if_208:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_777)
  br label %loop_170
range_query.end_165:
  call ccc void @eclair_btree_iterator_next_8(ptr %stack.ptr_773)
  br label %loop_169
range_query.end_164:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_769)
  br label %loop_168
range_query.end_163:
  %2705 = getelementptr [2 x i32], ptr %stack.ptr_785, i32 0, i32 0
  store i32 0, ptr %2705
  %2706 = getelementptr [2 x i32], ptr %stack.ptr_785, i32 0, i32 1
  store i32 0, ptr %2706
  %2707 = getelementptr [2 x i32], ptr %stack.ptr_786, i32 0, i32 0
  store i32 4294967295, ptr %2707
  %2708 = getelementptr [2 x i32], ptr %stack.ptr_786, i32 0, i32 1
  store i32 4294967295, ptr %2708
  %2709 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %2709, ptr %stack.ptr_785, ptr %stack.ptr_787)
  %2710 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %2710, ptr %stack.ptr_786, ptr %stack.ptr_788)
  br label %loop_172
loop_172:
  %2711 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_787, ptr %stack.ptr_788)
  br i1 %2711, label %if_209, label %end_if_209
if_209:
  br label %range_query.end_167
end_if_209:
  %2712 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_787)
  %2713 = getelementptr [3 x i32], ptr %stack.ptr_789, i32 0, i32 0
  %2714 = getelementptr [2 x i32], ptr %2712, i32 0, i32 1
  %2715 = load i32, ptr %2714
  store i32 %2715, ptr %2713
  %2716 = getelementptr [3 x i32], ptr %stack.ptr_789, i32 0, i32 1
  store i32 0, ptr %2716
  %2717 = getelementptr [3 x i32], ptr %stack.ptr_789, i32 0, i32 2
  store i32 0, ptr %2717
  %2718 = getelementptr [3 x i32], ptr %stack.ptr_790, i32 0, i32 0
  %2719 = getelementptr [2 x i32], ptr %2712, i32 0, i32 1
  %2720 = load i32, ptr %2719
  store i32 %2720, ptr %2718
  %2721 = getelementptr [3 x i32], ptr %stack.ptr_790, i32 0, i32 1
  store i32 4294967295, ptr %2721
  %2722 = getelementptr [3 x i32], ptr %stack.ptr_790, i32 0, i32 2
  store i32 4294967295, ptr %2722
  %2723 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_0(ptr %2723, ptr %stack.ptr_789, ptr %stack.ptr_791)
  %2724 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_0(ptr %2724, ptr %stack.ptr_790, ptr %stack.ptr_792)
  br label %loop_173
loop_173:
  %2725 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_791, ptr %stack.ptr_792)
  br i1 %2725, label %if_210, label %end_if_210
if_210:
  br label %range_query.end_168
end_if_210:
  %2726 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_791)
  %2727 = getelementptr [2 x i32], ptr %stack.ptr_793, i32 0, i32 0
  %2728 = getelementptr [2 x i32], ptr %2712, i32 0, i32 1
  %2729 = load i32, ptr %2728
  store i32 %2729, ptr %2727
  %2730 = getelementptr [2 x i32], ptr %stack.ptr_793, i32 0, i32 1
  %2731 = getelementptr [3 x i32], ptr %2726, i32 0, i32 2
  %2732 = load i32, ptr %2731
  store i32 %2732, ptr %2730
  %2733 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2734 = call ccc i1 @eclair_btree_contains_1(ptr %2733, ptr %stack.ptr_793)
  %2735 = select i1 %2734, i1 0, i1 1
  br i1 %2735, label %if_211, label %end_if_212
if_211:
  %2736 = getelementptr [2 x i32], ptr %stack.ptr_794, i32 0, i32 0
  %2737 = getelementptr [3 x i32], ptr %2726, i32 0, i32 2
  %2738 = load i32, ptr %2737
  store i32 %2738, ptr %2736
  %2739 = getelementptr [2 x i32], ptr %stack.ptr_794, i32 0, i32 1
  store i32 0, ptr %2739
  %2740 = getelementptr [2 x i32], ptr %stack.ptr_795, i32 0, i32 0
  %2741 = getelementptr [3 x i32], ptr %2726, i32 0, i32 2
  %2742 = load i32, ptr %2741
  store i32 %2742, ptr %2740
  %2743 = getelementptr [2 x i32], ptr %stack.ptr_795, i32 0, i32 1
  store i32 4294967295, ptr %2743
  %2744 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_lower_bound_1(ptr %2744, ptr %stack.ptr_794, ptr %stack.ptr_796)
  %2745 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_upper_bound_1(ptr %2745, ptr %stack.ptr_795, ptr %stack.ptr_797)
  br label %loop_174
loop_174:
  %2746 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_796, ptr %stack.ptr_797)
  br i1 %2746, label %if_212, label %end_if_211
if_212:
  br label %range_query.end_169
end_if_211:
  %2747 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_796)
  %2748 = getelementptr [3 x i32], ptr %stack.ptr_798, i32 0, i32 0
  %2749 = getelementptr [2 x i32], ptr %2712, i32 0, i32 1
  %2750 = load i32, ptr %2749
  store i32 %2750, ptr %2748
  %2751 = getelementptr [3 x i32], ptr %stack.ptr_798, i32 0, i32 1
  %2752 = getelementptr [3 x i32], ptr %2726, i32 0, i32 2
  %2753 = load i32, ptr %2752
  store i32 %2753, ptr %2751
  %2754 = getelementptr [3 x i32], ptr %stack.ptr_798, i32 0, i32 2
  %2755 = getelementptr [2 x i32], ptr %2747, i32 0, i32 1
  %2756 = load i32, ptr %2755
  store i32 %2756, ptr %2754
  %2757 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %2758 = call ccc i1 @eclair_btree_insert_value_0(ptr %2757, ptr %stack.ptr_798)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_796)
  br label %loop_174
range_query.end_169:
  br label %end_if_212
end_if_212:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_791)
  br label %loop_173
range_query.end_168:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_787)
  br label %loop_172
range_query.end_167:
  %2759 = getelementptr [3 x i32], ptr %stack.ptr_799, i32 0, i32 0
  store i32 0, ptr %2759
  %2760 = getelementptr [3 x i32], ptr %stack.ptr_799, i32 0, i32 1
  store i32 0, ptr %2760
  %2761 = getelementptr [3 x i32], ptr %stack.ptr_799, i32 0, i32 2
  store i32 0, ptr %2761
  %2762 = getelementptr [3 x i32], ptr %stack.ptr_800, i32 0, i32 0
  store i32 4294967295, ptr %2762
  %2763 = getelementptr [3 x i32], ptr %stack.ptr_800, i32 0, i32 1
  store i32 4294967295, ptr %2763
  %2764 = getelementptr [3 x i32], ptr %stack.ptr_800, i32 0, i32 2
  store i32 4294967295, ptr %2764
  %2765 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_lower_bound_0(ptr %2765, ptr %stack.ptr_799, ptr %stack.ptr_801)
  %2766 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_upper_bound_0(ptr %2766, ptr %stack.ptr_800, ptr %stack.ptr_802)
  br label %loop_175
loop_175:
  %2767 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_801, ptr %stack.ptr_802)
  br i1 %2767, label %if_213, label %end_if_213
if_213:
  br label %range_query.end_170
end_if_213:
  %2768 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_801)
  %2769 = getelementptr [2 x i32], ptr %stack.ptr_803, i32 0, i32 0
  %2770 = getelementptr [3 x i32], ptr %2768, i32 0, i32 0
  %2771 = load i32, ptr %2770
  store i32 %2771, ptr %2769
  %2772 = getelementptr [2 x i32], ptr %stack.ptr_803, i32 0, i32 1
  %2773 = getelementptr [3 x i32], ptr %2768, i32 0, i32 2
  %2774 = load i32, ptr %2773
  store i32 %2774, ptr %2772
  %2775 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2776 = call ccc i1 @eclair_btree_contains_1(ptr %2775, ptr %stack.ptr_803)
  %2777 = select i1 %2776, i1 0, i1 1
  br i1 %2777, label %if_214, label %end_if_216
if_214:
  %2778 = getelementptr [2 x i32], ptr %stack.ptr_804, i32 0, i32 0
  %2779 = getelementptr [3 x i32], ptr %2768, i32 0, i32 2
  %2780 = load i32, ptr %2779
  store i32 %2780, ptr %2778
  %2781 = getelementptr [2 x i32], ptr %stack.ptr_804, i32 0, i32 1
  store i32 0, ptr %2781
  %2782 = getelementptr [2 x i32], ptr %stack.ptr_805, i32 0, i32 0
  %2783 = getelementptr [3 x i32], ptr %2768, i32 0, i32 2
  %2784 = load i32, ptr %2783
  store i32 %2784, ptr %2782
  %2785 = getelementptr [2 x i32], ptr %stack.ptr_805, i32 0, i32 1
  store i32 4294967295, ptr %2785
  %2786 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %2786, ptr %stack.ptr_804, ptr %stack.ptr_806)
  %2787 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %2787, ptr %stack.ptr_805, ptr %stack.ptr_807)
  br label %loop_176
loop_176:
  %2788 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_806, ptr %stack.ptr_807)
  br i1 %2788, label %if_215, label %end_if_214
if_215:
  br label %range_query.end_171
end_if_214:
  %2789 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_806)
  %2790 = getelementptr [2 x i32], ptr %stack.ptr_808, i32 0, i32 0
  %2791 = getelementptr [3 x i32], ptr %2768, i32 0, i32 0
  %2792 = load i32, ptr %2791
  store i32 %2792, ptr %2790
  %2793 = getelementptr [2 x i32], ptr %stack.ptr_808, i32 0, i32 1
  %2794 = getelementptr [3 x i32], ptr %2768, i32 0, i32 2
  %2795 = load i32, ptr %2794
  store i32 %2795, ptr %2793
  %2796 = getelementptr [2 x i32], ptr %stack.ptr_809, i32 0, i32 0
  %2797 = getelementptr [3 x i32], ptr %2768, i32 0, i32 0
  %2798 = load i32, ptr %2797
  store i32 %2798, ptr %2796
  %2799 = getelementptr [2 x i32], ptr %stack.ptr_809, i32 0, i32 1
  %2800 = getelementptr [3 x i32], ptr %2768, i32 0, i32 2
  %2801 = load i32, ptr %2800
  store i32 %2801, ptr %2799
  %2802 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2802, ptr %stack.ptr_808, ptr %stack.ptr_810)
  %2803 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2803, ptr %stack.ptr_809, ptr %stack.ptr_811)
  br label %loop_177
loop_177:
  %2804 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_810, ptr %stack.ptr_811)
  br i1 %2804, label %if_216, label %end_if_215
if_216:
  br label %range_query.end_172
end_if_215:
  %2805 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_810)
  %2806 = getelementptr [3 x i32], ptr %stack.ptr_812, i32 0, i32 0
  %2807 = getelementptr [3 x i32], ptr %2768, i32 0, i32 0
  %2808 = load i32, ptr %2807
  store i32 %2808, ptr %2806
  %2809 = getelementptr [3 x i32], ptr %stack.ptr_812, i32 0, i32 1
  %2810 = getelementptr [3 x i32], ptr %2768, i32 0, i32 2
  %2811 = load i32, ptr %2810
  store i32 %2811, ptr %2809
  %2812 = getelementptr [3 x i32], ptr %stack.ptr_812, i32 0, i32 2
  %2813 = getelementptr [2 x i32], ptr %2789, i32 0, i32 1
  %2814 = load i32, ptr %2813
  store i32 %2814, ptr %2812
  %2815 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %2816 = call ccc i1 @eclair_btree_insert_value_0(ptr %2815, ptr %stack.ptr_812)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_810)
  br label %loop_177
range_query.end_172:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_806)
  br label %loop_176
range_query.end_171:
  br label %end_if_216
end_if_216:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_801)
  br label %loop_175
range_query.end_170:
  %2817 = getelementptr [3 x i32], ptr %stack.ptr_813, i32 0, i32 0
  store i32 0, ptr %2817
  %2818 = getelementptr [3 x i32], ptr %stack.ptr_813, i32 0, i32 1
  store i32 0, ptr %2818
  %2819 = getelementptr [3 x i32], ptr %stack.ptr_813, i32 0, i32 2
  store i32 0, ptr %2819
  %2820 = getelementptr [3 x i32], ptr %stack.ptr_814, i32 0, i32 0
  store i32 4294967295, ptr %2820
  %2821 = getelementptr [3 x i32], ptr %stack.ptr_814, i32 0, i32 1
  store i32 4294967295, ptr %2821
  %2822 = getelementptr [3 x i32], ptr %stack.ptr_814, i32 0, i32 2
  store i32 4294967295, ptr %2822
  %2823 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2823, ptr %stack.ptr_813, ptr %stack.ptr_815)
  %2824 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2824, ptr %stack.ptr_814, ptr %stack.ptr_816)
  br label %loop_178
loop_178:
  %2825 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_815, ptr %stack.ptr_816)
  br i1 %2825, label %if_217, label %end_if_217
if_217:
  br label %range_query.end_173
end_if_217:
  %2826 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_815)
  %2827 = getelementptr [4 x i32], ptr %stack.ptr_817, i32 0, i32 0
  %2828 = getelementptr [3 x i32], ptr %2826, i32 0, i32 2
  %2829 = load i32, ptr %2828
  store i32 %2829, ptr %2827
  %2830 = getelementptr [4 x i32], ptr %stack.ptr_817, i32 0, i32 1
  store i32 0, ptr %2830
  %2831 = getelementptr [4 x i32], ptr %stack.ptr_817, i32 0, i32 2
  store i32 0, ptr %2831
  %2832 = getelementptr [4 x i32], ptr %stack.ptr_817, i32 0, i32 3
  store i32 0, ptr %2832
  %2833 = getelementptr [4 x i32], ptr %stack.ptr_818, i32 0, i32 0
  %2834 = getelementptr [3 x i32], ptr %2826, i32 0, i32 2
  %2835 = load i32, ptr %2834
  store i32 %2835, ptr %2833
  %2836 = getelementptr [4 x i32], ptr %stack.ptr_818, i32 0, i32 1
  store i32 4294967295, ptr %2836
  %2837 = getelementptr [4 x i32], ptr %stack.ptr_818, i32 0, i32 2
  store i32 4294967295, ptr %2837
  %2838 = getelementptr [4 x i32], ptr %stack.ptr_818, i32 0, i32 3
  store i32 4294967295, ptr %2838
  %2839 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %2839, ptr %stack.ptr_817, ptr %stack.ptr_819)
  %2840 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %2840, ptr %stack.ptr_818, ptr %stack.ptr_820)
  br label %loop_179
loop_179:
  %2841 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_819, ptr %stack.ptr_820)
  br i1 %2841, label %if_218, label %end_if_218
if_218:
  br label %range_query.end_174
end_if_218:
  %2842 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_819)
  %2843 = getelementptr [2 x i32], ptr %stack.ptr_821, i32 0, i32 0
  %2844 = getelementptr [3 x i32], ptr %2826, i32 0, i32 0
  %2845 = load i32, ptr %2844
  store i32 %2845, ptr %2843
  %2846 = getelementptr [2 x i32], ptr %stack.ptr_821, i32 0, i32 1
  %2847 = getelementptr [4 x i32], ptr %2842, i32 0, i32 3
  %2848 = load i32, ptr %2847
  store i32 %2848, ptr %2846
  %2849 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2850 = call ccc i1 @eclair_btree_contains_1(ptr %2849, ptr %stack.ptr_821)
  %2851 = select i1 %2850, i1 0, i1 1
  br i1 %2851, label %if_219, label %end_if_222
if_219:
  %2852 = getelementptr [1 x i32], ptr %stack.ptr_822, i32 0, i32 0
  %2853 = getelementptr [4 x i32], ptr %2842, i32 0, i32 1
  %2854 = load i32, ptr %2853
  store i32 %2854, ptr %2852
  %2855 = getelementptr [1 x i32], ptr %stack.ptr_823, i32 0, i32 0
  %2856 = getelementptr [4 x i32], ptr %2842, i32 0, i32 1
  %2857 = load i32, ptr %2856
  store i32 %2857, ptr %2855
  %2858 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_lower_bound_6(ptr %2858, ptr %stack.ptr_822, ptr %stack.ptr_824)
  %2859 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_upper_bound_6(ptr %2859, ptr %stack.ptr_823, ptr %stack.ptr_825)
  br label %loop_180
loop_180:
  %2860 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_824, ptr %stack.ptr_825)
  br i1 %2860, label %if_220, label %end_if_219
if_220:
  br label %range_query.end_175
end_if_219:
  %2861 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_824)
  %2862 = getelementptr [2 x i32], ptr %stack.ptr_826, i32 0, i32 0
  %2863 = getelementptr [4 x i32], ptr %2842, i32 0, i32 3
  %2864 = load i32, ptr %2863
  store i32 %2864, ptr %2862
  %2865 = getelementptr [2 x i32], ptr %stack.ptr_826, i32 0, i32 1
  store i32 0, ptr %2865
  %2866 = getelementptr [2 x i32], ptr %stack.ptr_827, i32 0, i32 0
  %2867 = getelementptr [4 x i32], ptr %2842, i32 0, i32 3
  %2868 = load i32, ptr %2867
  store i32 %2868, ptr %2866
  %2869 = getelementptr [2 x i32], ptr %stack.ptr_827, i32 0, i32 1
  store i32 4294967295, ptr %2869
  %2870 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %2870, ptr %stack.ptr_826, ptr %stack.ptr_828)
  %2871 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %2871, ptr %stack.ptr_827, ptr %stack.ptr_829)
  br label %loop_181
loop_181:
  %2872 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_828, ptr %stack.ptr_829)
  br i1 %2872, label %if_221, label %end_if_220
if_221:
  br label %range_query.end_176
end_if_220:
  %2873 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_828)
  %2874 = getelementptr [2 x i32], ptr %stack.ptr_830, i32 0, i32 0
  %2875 = getelementptr [3 x i32], ptr %2826, i32 0, i32 0
  %2876 = load i32, ptr %2875
  store i32 %2876, ptr %2874
  %2877 = getelementptr [2 x i32], ptr %stack.ptr_830, i32 0, i32 1
  %2878 = getelementptr [4 x i32], ptr %2842, i32 0, i32 3
  %2879 = load i32, ptr %2878
  store i32 %2879, ptr %2877
  %2880 = getelementptr [2 x i32], ptr %stack.ptr_831, i32 0, i32 0
  %2881 = getelementptr [3 x i32], ptr %2826, i32 0, i32 0
  %2882 = load i32, ptr %2881
  store i32 %2882, ptr %2880
  %2883 = getelementptr [2 x i32], ptr %stack.ptr_831, i32 0, i32 1
  %2884 = getelementptr [4 x i32], ptr %2842, i32 0, i32 3
  %2885 = load i32, ptr %2884
  store i32 %2885, ptr %2883
  %2886 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2886, ptr %stack.ptr_830, ptr %stack.ptr_832)
  %2887 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2887, ptr %stack.ptr_831, ptr %stack.ptr_833)
  br label %loop_182
loop_182:
  %2888 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_832, ptr %stack.ptr_833)
  br i1 %2888, label %if_222, label %end_if_221
if_222:
  br label %range_query.end_177
end_if_221:
  %2889 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_832)
  %2890 = getelementptr [3 x i32], ptr %stack.ptr_834, i32 0, i32 0
  %2891 = getelementptr [3 x i32], ptr %2826, i32 0, i32 0
  %2892 = load i32, ptr %2891
  store i32 %2892, ptr %2890
  %2893 = getelementptr [3 x i32], ptr %stack.ptr_834, i32 0, i32 1
  %2894 = getelementptr [4 x i32], ptr %2842, i32 0, i32 3
  %2895 = load i32, ptr %2894
  store i32 %2895, ptr %2893
  %2896 = getelementptr [3 x i32], ptr %stack.ptr_834, i32 0, i32 2
  %2897 = getelementptr [2 x i32], ptr %2873, i32 0, i32 1
  %2898 = load i32, ptr %2897
  store i32 %2898, ptr %2896
  %2899 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %2900 = call ccc i1 @eclair_btree_insert_value_0(ptr %2899, ptr %stack.ptr_834)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_832)
  br label %loop_182
range_query.end_177:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_828)
  br label %loop_181
range_query.end_176:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_824)
  br label %loop_180
range_query.end_175:
  br label %end_if_222
end_if_222:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_819)
  br label %loop_179
range_query.end_174:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_815)
  br label %loop_178
range_query.end_173:
  %2901 = getelementptr [3 x i32], ptr %stack.ptr_835, i32 0, i32 0
  store i32 0, ptr %2901
  %2902 = getelementptr [3 x i32], ptr %stack.ptr_835, i32 0, i32 1
  store i32 0, ptr %2902
  %2903 = getelementptr [3 x i32], ptr %stack.ptr_835, i32 0, i32 2
  store i32 0, ptr %2903
  %2904 = getelementptr [3 x i32], ptr %stack.ptr_836, i32 0, i32 0
  store i32 4294967295, ptr %2904
  %2905 = getelementptr [3 x i32], ptr %stack.ptr_836, i32 0, i32 1
  store i32 4294967295, ptr %2905
  %2906 = getelementptr [3 x i32], ptr %stack.ptr_836, i32 0, i32 2
  store i32 4294967295, ptr %2906
  %2907 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2907, ptr %stack.ptr_835, ptr %stack.ptr_837)
  %2908 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2908, ptr %stack.ptr_836, ptr %stack.ptr_838)
  br label %loop_183
loop_183:
  %2909 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_837, ptr %stack.ptr_838)
  br i1 %2909, label %if_223, label %end_if_223
if_223:
  br label %range_query.end_178
end_if_223:
  %2910 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_837)
  %2911 = getelementptr [4 x i32], ptr %stack.ptr_839, i32 0, i32 0
  %2912 = getelementptr [3 x i32], ptr %2910, i32 0, i32 2
  %2913 = load i32, ptr %2912
  store i32 %2913, ptr %2911
  %2914 = getelementptr [4 x i32], ptr %stack.ptr_839, i32 0, i32 1
  store i32 0, ptr %2914
  %2915 = getelementptr [4 x i32], ptr %stack.ptr_839, i32 0, i32 2
  store i32 0, ptr %2915
  %2916 = getelementptr [4 x i32], ptr %stack.ptr_839, i32 0, i32 3
  store i32 0, ptr %2916
  %2917 = getelementptr [4 x i32], ptr %stack.ptr_840, i32 0, i32 0
  %2918 = getelementptr [3 x i32], ptr %2910, i32 0, i32 2
  %2919 = load i32, ptr %2918
  store i32 %2919, ptr %2917
  %2920 = getelementptr [4 x i32], ptr %stack.ptr_840, i32 0, i32 1
  store i32 4294967295, ptr %2920
  %2921 = getelementptr [4 x i32], ptr %stack.ptr_840, i32 0, i32 2
  store i32 4294967295, ptr %2921
  %2922 = getelementptr [4 x i32], ptr %stack.ptr_840, i32 0, i32 3
  store i32 4294967295, ptr %2922
  %2923 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %2923, ptr %stack.ptr_839, ptr %stack.ptr_841)
  %2924 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %2924, ptr %stack.ptr_840, ptr %stack.ptr_842)
  br label %loop_184
loop_184:
  %2925 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_841, ptr %stack.ptr_842)
  br i1 %2925, label %if_224, label %end_if_224
if_224:
  br label %range_query.end_179
end_if_224:
  %2926 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_841)
  %2927 = getelementptr [2 x i32], ptr %stack.ptr_843, i32 0, i32 0
  %2928 = getelementptr [3 x i32], ptr %2910, i32 0, i32 0
  %2929 = load i32, ptr %2928
  store i32 %2929, ptr %2927
  %2930 = getelementptr [2 x i32], ptr %stack.ptr_843, i32 0, i32 1
  %2931 = getelementptr [4 x i32], ptr %2926, i32 0, i32 2
  %2932 = load i32, ptr %2931
  store i32 %2932, ptr %2930
  %2933 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2934 = call ccc i1 @eclair_btree_contains_1(ptr %2933, ptr %stack.ptr_843)
  %2935 = select i1 %2934, i1 0, i1 1
  br i1 %2935, label %if_225, label %end_if_228
if_225:
  %2936 = getelementptr [1 x i32], ptr %stack.ptr_844, i32 0, i32 0
  %2937 = getelementptr [4 x i32], ptr %2926, i32 0, i32 1
  %2938 = load i32, ptr %2937
  store i32 %2938, ptr %2936
  %2939 = getelementptr [1 x i32], ptr %stack.ptr_845, i32 0, i32 0
  %2940 = getelementptr [4 x i32], ptr %2926, i32 0, i32 1
  %2941 = load i32, ptr %2940
  store i32 %2941, ptr %2939
  %2942 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_lower_bound_6(ptr %2942, ptr %stack.ptr_844, ptr %stack.ptr_846)
  %2943 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_upper_bound_6(ptr %2943, ptr %stack.ptr_845, ptr %stack.ptr_847)
  br label %loop_185
loop_185:
  %2944 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_846, ptr %stack.ptr_847)
  br i1 %2944, label %if_226, label %end_if_225
if_226:
  br label %range_query.end_180
end_if_225:
  %2945 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_846)
  %2946 = getelementptr [2 x i32], ptr %stack.ptr_848, i32 0, i32 0
  %2947 = getelementptr [4 x i32], ptr %2926, i32 0, i32 2
  %2948 = load i32, ptr %2947
  store i32 %2948, ptr %2946
  %2949 = getelementptr [2 x i32], ptr %stack.ptr_848, i32 0, i32 1
  store i32 0, ptr %2949
  %2950 = getelementptr [2 x i32], ptr %stack.ptr_849, i32 0, i32 0
  %2951 = getelementptr [4 x i32], ptr %2926, i32 0, i32 2
  %2952 = load i32, ptr %2951
  store i32 %2952, ptr %2950
  %2953 = getelementptr [2 x i32], ptr %stack.ptr_849, i32 0, i32 1
  store i32 4294967295, ptr %2953
  %2954 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %2954, ptr %stack.ptr_848, ptr %stack.ptr_850)
  %2955 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %2955, ptr %stack.ptr_849, ptr %stack.ptr_851)
  br label %loop_186
loop_186:
  %2956 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_850, ptr %stack.ptr_851)
  br i1 %2956, label %if_227, label %end_if_226
if_227:
  br label %range_query.end_181
end_if_226:
  %2957 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_850)
  %2958 = getelementptr [2 x i32], ptr %stack.ptr_852, i32 0, i32 0
  %2959 = getelementptr [3 x i32], ptr %2910, i32 0, i32 0
  %2960 = load i32, ptr %2959
  store i32 %2960, ptr %2958
  %2961 = getelementptr [2 x i32], ptr %stack.ptr_852, i32 0, i32 1
  %2962 = getelementptr [4 x i32], ptr %2926, i32 0, i32 2
  %2963 = load i32, ptr %2962
  store i32 %2963, ptr %2961
  %2964 = getelementptr [2 x i32], ptr %stack.ptr_853, i32 0, i32 0
  %2965 = getelementptr [3 x i32], ptr %2910, i32 0, i32 0
  %2966 = load i32, ptr %2965
  store i32 %2966, ptr %2964
  %2967 = getelementptr [2 x i32], ptr %stack.ptr_853, i32 0, i32 1
  %2968 = getelementptr [4 x i32], ptr %2926, i32 0, i32 2
  %2969 = load i32, ptr %2968
  store i32 %2969, ptr %2967
  %2970 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2970, ptr %stack.ptr_852, ptr %stack.ptr_854)
  %2971 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2971, ptr %stack.ptr_853, ptr %stack.ptr_855)
  br label %loop_187
loop_187:
  %2972 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_854, ptr %stack.ptr_855)
  br i1 %2972, label %if_228, label %end_if_227
if_228:
  br label %range_query.end_182
end_if_227:
  %2973 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_854)
  %2974 = getelementptr [3 x i32], ptr %stack.ptr_856, i32 0, i32 0
  %2975 = getelementptr [3 x i32], ptr %2910, i32 0, i32 0
  %2976 = load i32, ptr %2975
  store i32 %2976, ptr %2974
  %2977 = getelementptr [3 x i32], ptr %stack.ptr_856, i32 0, i32 1
  %2978 = getelementptr [4 x i32], ptr %2926, i32 0, i32 2
  %2979 = load i32, ptr %2978
  store i32 %2979, ptr %2977
  %2980 = getelementptr [3 x i32], ptr %stack.ptr_856, i32 0, i32 2
  %2981 = getelementptr [2 x i32], ptr %2957, i32 0, i32 1
  %2982 = load i32, ptr %2981
  store i32 %2982, ptr %2980
  %2983 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %2984 = call ccc i1 @eclair_btree_insert_value_0(ptr %2983, ptr %stack.ptr_856)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_854)
  br label %loop_187
range_query.end_182:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_850)
  br label %loop_186
range_query.end_181:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_846)
  br label %loop_185
range_query.end_180:
  br label %end_if_228
end_if_228:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_841)
  br label %loop_184
range_query.end_179:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_837)
  br label %loop_183
range_query.end_178:
  %2985 = getelementptr [4 x i32], ptr %stack.ptr_857, i32 0, i32 0
  store i32 0, ptr %2985
  %2986 = getelementptr [4 x i32], ptr %stack.ptr_857, i32 0, i32 1
  store i32 0, ptr %2986
  %2987 = getelementptr [4 x i32], ptr %stack.ptr_857, i32 0, i32 2
  store i32 0, ptr %2987
  %2988 = getelementptr [4 x i32], ptr %stack.ptr_857, i32 0, i32 3
  store i32 0, ptr %2988
  %2989 = getelementptr [4 x i32], ptr %stack.ptr_858, i32 0, i32 0
  store i32 4294967295, ptr %2989
  %2990 = getelementptr [4 x i32], ptr %stack.ptr_858, i32 0, i32 1
  store i32 4294967295, ptr %2990
  %2991 = getelementptr [4 x i32], ptr %stack.ptr_858, i32 0, i32 2
  store i32 4294967295, ptr %2991
  %2992 = getelementptr [4 x i32], ptr %stack.ptr_858, i32 0, i32 3
  store i32 4294967295, ptr %2992
  %2993 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %2993, ptr %stack.ptr_857, ptr %stack.ptr_859)
  %2994 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %2994, ptr %stack.ptr_858, ptr %stack.ptr_860)
  br label %loop_188
loop_188:
  %2995 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_859, ptr %stack.ptr_860)
  br i1 %2995, label %if_229, label %end_if_229
if_229:
  br label %range_query.end_183
end_if_229:
  %2996 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_859)
  %2997 = getelementptr [2 x i32], ptr %stack.ptr_861, i32 0, i32 0
  %2998 = getelementptr [4 x i32], ptr %2996, i32 0, i32 3
  %2999 = load i32, ptr %2998
  store i32 %2999, ptr %2997
  %3000 = getelementptr [2 x i32], ptr %stack.ptr_861, i32 0, i32 1
  store i32 0, ptr %3000
  %3001 = getelementptr [2 x i32], ptr %stack.ptr_862, i32 0, i32 0
  %3002 = getelementptr [4 x i32], ptr %2996, i32 0, i32 3
  %3003 = load i32, ptr %3002
  store i32 %3003, ptr %3001
  %3004 = getelementptr [2 x i32], ptr %stack.ptr_862, i32 0, i32 1
  store i32 4294967295, ptr %3004
  %3005 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %3005, ptr %stack.ptr_861, ptr %stack.ptr_863)
  %3006 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %3006, ptr %stack.ptr_862, ptr %stack.ptr_864)
  br label %loop_189
loop_189:
  %3007 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_863, ptr %stack.ptr_864)
  br i1 %3007, label %if_230, label %end_if_230
if_230:
  br label %range_query.end_184
end_if_230:
  %3008 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_863)
  %3009 = getelementptr [2 x i32], ptr %stack.ptr_865, i32 0, i32 0
  store i32 0, ptr %3009
  %3010 = getelementptr [2 x i32], ptr %stack.ptr_865, i32 0, i32 1
  %3011 = getelementptr [4 x i32], ptr %2996, i32 0, i32 3
  %3012 = load i32, ptr %3011
  store i32 %3012, ptr %3010
  %3013 = getelementptr [2 x i32], ptr %stack.ptr_866, i32 0, i32 0
  store i32 4294967295, ptr %3013
  %3014 = getelementptr [2 x i32], ptr %stack.ptr_866, i32 0, i32 1
  %3015 = getelementptr [4 x i32], ptr %2996, i32 0, i32 3
  %3016 = load i32, ptr %3015
  store i32 %3016, ptr %3014
  %3017 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_lower_bound_2(ptr %3017, ptr %stack.ptr_865, ptr %stack.ptr_867)
  %3018 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_upper_bound_2(ptr %3018, ptr %stack.ptr_866, ptr %stack.ptr_868)
  br label %loop_190
loop_190:
  %3019 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_867, ptr %stack.ptr_868)
  br i1 %3019, label %if_231, label %end_if_231
if_231:
  br label %range_query.end_185
end_if_231:
  %3020 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_867)
  %3021 = getelementptr [2 x i32], ptr %stack.ptr_869, i32 0, i32 0
  %3022 = getelementptr [2 x i32], ptr %3020, i32 0, i32 0
  %3023 = load i32, ptr %3022
  store i32 %3023, ptr %3021
  %3024 = getelementptr [2 x i32], ptr %stack.ptr_869, i32 0, i32 1
  %3025 = getelementptr [4 x i32], ptr %2996, i32 0, i32 3
  %3026 = load i32, ptr %3025
  store i32 %3026, ptr %3024
  %3027 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %3028 = call ccc i1 @eclair_btree_contains_1(ptr %3027, ptr %stack.ptr_869)
  %3029 = select i1 %3028, i1 0, i1 1
  br i1 %3029, label %if_232, label %end_if_232
if_232:
  %3030 = getelementptr [3 x i32], ptr %stack.ptr_870, i32 0, i32 0
  %3031 = getelementptr [2 x i32], ptr %3020, i32 0, i32 0
  %3032 = load i32, ptr %3031
  store i32 %3032, ptr %3030
  %3033 = getelementptr [3 x i32], ptr %stack.ptr_870, i32 0, i32 1
  %3034 = getelementptr [4 x i32], ptr %2996, i32 0, i32 3
  %3035 = load i32, ptr %3034
  store i32 %3035, ptr %3033
  %3036 = getelementptr [3 x i32], ptr %stack.ptr_870, i32 0, i32 2
  %3037 = getelementptr [2 x i32], ptr %3008, i32 0, i32 1
  %3038 = load i32, ptr %3037
  store i32 %3038, ptr %3036
  %3039 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %3040 = call ccc i1 @eclair_btree_insert_value_0(ptr %3039, ptr %stack.ptr_870)
  br label %end_if_232
end_if_232:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_867)
  br label %loop_190
range_query.end_185:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_863)
  br label %loop_189
range_query.end_184:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_859)
  br label %loop_188
range_query.end_183:
  %3041 = getelementptr [4 x i32], ptr %stack.ptr_871, i32 0, i32 0
  store i32 0, ptr %3041
  %3042 = getelementptr [4 x i32], ptr %stack.ptr_871, i32 0, i32 1
  store i32 0, ptr %3042
  %3043 = getelementptr [4 x i32], ptr %stack.ptr_871, i32 0, i32 2
  store i32 0, ptr %3043
  %3044 = getelementptr [4 x i32], ptr %stack.ptr_871, i32 0, i32 3
  store i32 0, ptr %3044
  %3045 = getelementptr [4 x i32], ptr %stack.ptr_872, i32 0, i32 0
  store i32 4294967295, ptr %3045
  %3046 = getelementptr [4 x i32], ptr %stack.ptr_872, i32 0, i32 1
  store i32 4294967295, ptr %3046
  %3047 = getelementptr [4 x i32], ptr %stack.ptr_872, i32 0, i32 2
  store i32 4294967295, ptr %3047
  %3048 = getelementptr [4 x i32], ptr %stack.ptr_872, i32 0, i32 3
  store i32 4294967295, ptr %3048
  %3049 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %3049, ptr %stack.ptr_871, ptr %stack.ptr_873)
  %3050 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %3050, ptr %stack.ptr_872, ptr %stack.ptr_874)
  br label %loop_191
loop_191:
  %3051 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_873, ptr %stack.ptr_874)
  br i1 %3051, label %if_233, label %end_if_233
if_233:
  br label %range_query.end_186
end_if_233:
  %3052 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_873)
  %3053 = getelementptr [2 x i32], ptr %stack.ptr_875, i32 0, i32 0
  %3054 = getelementptr [4 x i32], ptr %3052, i32 0, i32 2
  %3055 = load i32, ptr %3054
  store i32 %3055, ptr %3053
  %3056 = getelementptr [2 x i32], ptr %stack.ptr_875, i32 0, i32 1
  store i32 0, ptr %3056
  %3057 = getelementptr [2 x i32], ptr %stack.ptr_876, i32 0, i32 0
  %3058 = getelementptr [4 x i32], ptr %3052, i32 0, i32 2
  %3059 = load i32, ptr %3058
  store i32 %3059, ptr %3057
  %3060 = getelementptr [2 x i32], ptr %stack.ptr_876, i32 0, i32 1
  store i32 4294967295, ptr %3060
  %3061 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %3061, ptr %stack.ptr_875, ptr %stack.ptr_877)
  %3062 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %3062, ptr %stack.ptr_876, ptr %stack.ptr_878)
  br label %loop_192
loop_192:
  %3063 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_877, ptr %stack.ptr_878)
  br i1 %3063, label %if_234, label %end_if_234
if_234:
  br label %range_query.end_187
end_if_234:
  %3064 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_877)
  %3065 = getelementptr [2 x i32], ptr %stack.ptr_879, i32 0, i32 0
  store i32 0, ptr %3065
  %3066 = getelementptr [2 x i32], ptr %stack.ptr_879, i32 0, i32 1
  %3067 = getelementptr [4 x i32], ptr %3052, i32 0, i32 2
  %3068 = load i32, ptr %3067
  store i32 %3068, ptr %3066
  %3069 = getelementptr [2 x i32], ptr %stack.ptr_880, i32 0, i32 0
  store i32 4294967295, ptr %3069
  %3070 = getelementptr [2 x i32], ptr %stack.ptr_880, i32 0, i32 1
  %3071 = getelementptr [4 x i32], ptr %3052, i32 0, i32 2
  %3072 = load i32, ptr %3071
  store i32 %3072, ptr %3070
  %3073 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_lower_bound_2(ptr %3073, ptr %stack.ptr_879, ptr %stack.ptr_881)
  %3074 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_upper_bound_2(ptr %3074, ptr %stack.ptr_880, ptr %stack.ptr_882)
  br label %loop_193
loop_193:
  %3075 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_881, ptr %stack.ptr_882)
  br i1 %3075, label %if_235, label %end_if_235
if_235:
  br label %range_query.end_188
end_if_235:
  %3076 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_881)
  %3077 = getelementptr [2 x i32], ptr %stack.ptr_883, i32 0, i32 0
  %3078 = getelementptr [2 x i32], ptr %3076, i32 0, i32 0
  %3079 = load i32, ptr %3078
  store i32 %3079, ptr %3077
  %3080 = getelementptr [2 x i32], ptr %stack.ptr_883, i32 0, i32 1
  %3081 = getelementptr [4 x i32], ptr %3052, i32 0, i32 2
  %3082 = load i32, ptr %3081
  store i32 %3082, ptr %3080
  %3083 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %3084 = call ccc i1 @eclair_btree_contains_1(ptr %3083, ptr %stack.ptr_883)
  %3085 = select i1 %3084, i1 0, i1 1
  br i1 %3085, label %if_236, label %end_if_236
if_236:
  %3086 = getelementptr [3 x i32], ptr %stack.ptr_884, i32 0, i32 0
  %3087 = getelementptr [2 x i32], ptr %3076, i32 0, i32 0
  %3088 = load i32, ptr %3087
  store i32 %3088, ptr %3086
  %3089 = getelementptr [3 x i32], ptr %stack.ptr_884, i32 0, i32 1
  %3090 = getelementptr [4 x i32], ptr %3052, i32 0, i32 2
  %3091 = load i32, ptr %3090
  store i32 %3091, ptr %3089
  %3092 = getelementptr [3 x i32], ptr %stack.ptr_884, i32 0, i32 2
  %3093 = getelementptr [2 x i32], ptr %3064, i32 0, i32 1
  %3094 = load i32, ptr %3093
  store i32 %3094, ptr %3092
  %3095 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %3096 = call ccc i1 @eclair_btree_insert_value_0(ptr %3095, ptr %stack.ptr_884)
  br label %end_if_236
end_if_236:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_881)
  br label %loop_193
range_query.end_188:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_877)
  br label %loop_192
range_query.end_187:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_873)
  br label %loop_191
range_query.end_186:
  %3097 = getelementptr [2 x i32], ptr %stack.ptr_885, i32 0, i32 0
  store i32 0, ptr %3097
  %3098 = getelementptr [2 x i32], ptr %stack.ptr_885, i32 0, i32 1
  store i32 0, ptr %3098
  %3099 = getelementptr [2 x i32], ptr %stack.ptr_886, i32 0, i32 0
  store i32 4294967295, ptr %3099
  %3100 = getelementptr [2 x i32], ptr %stack.ptr_886, i32 0, i32 1
  store i32 4294967295, ptr %3100
  %3101 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %3101, ptr %stack.ptr_885, ptr %stack.ptr_887)
  %3102 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %3102, ptr %stack.ptr_886, ptr %stack.ptr_888)
  br label %loop_194
loop_194:
  %3103 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_887, ptr %stack.ptr_888)
  br i1 %3103, label %if_237, label %end_if_237
if_237:
  br label %range_query.end_189
end_if_237:
  %3104 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_887)
  %3105 = getelementptr [2 x i32], ptr %stack.ptr_889, i32 0, i32 0
  %3106 = getelementptr [2 x i32], ptr %3104, i32 0, i32 1
  %3107 = load i32, ptr %3106
  store i32 %3107, ptr %3105
  %3108 = getelementptr [2 x i32], ptr %stack.ptr_889, i32 0, i32 1
  store i32 0, ptr %3108
  %3109 = getelementptr [2 x i32], ptr %stack.ptr_890, i32 0, i32 0
  %3110 = getelementptr [2 x i32], ptr %3104, i32 0, i32 1
  %3111 = load i32, ptr %3110
  store i32 %3111, ptr %3109
  %3112 = getelementptr [2 x i32], ptr %stack.ptr_890, i32 0, i32 1
  store i32 4294967295, ptr %3112
  %3113 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3113, ptr %stack.ptr_889, ptr %stack.ptr_891)
  %3114 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3114, ptr %stack.ptr_890, ptr %stack.ptr_892)
  br label %loop_195
loop_195:
  %3115 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_891, ptr %stack.ptr_892)
  br i1 %3115, label %if_238, label %end_if_238
if_238:
  br label %range_query.end_190
end_if_238:
  %3116 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_891)
  %3117 = getelementptr [2 x i32], ptr %stack.ptr_893, i32 0, i32 0
  %3118 = getelementptr [2 x i32], ptr %3104, i32 0, i32 0
  %3119 = load i32, ptr %3118
  store i32 %3119, ptr %3117
  %3120 = getelementptr [2 x i32], ptr %stack.ptr_893, i32 0, i32 1
  %3121 = getelementptr [2 x i32], ptr %3116, i32 0, i32 1
  %3122 = load i32, ptr %3121
  store i32 %3122, ptr %3120
  %3123 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3124 = call ccc i1 @eclair_btree_insert_value_1(ptr %3123, ptr %stack.ptr_893)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_891)
  br label %loop_195
range_query.end_190:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_887)
  br label %loop_194
range_query.end_189:
  %3125 = getelementptr [3 x i32], ptr %stack.ptr_894, i32 0, i32 0
  store i32 0, ptr %3125
  %3126 = getelementptr [3 x i32], ptr %stack.ptr_894, i32 0, i32 1
  store i32 0, ptr %3126
  %3127 = getelementptr [3 x i32], ptr %stack.ptr_894, i32 0, i32 2
  store i32 0, ptr %3127
  %3128 = getelementptr [3 x i32], ptr %stack.ptr_895, i32 0, i32 0
  store i32 4294967295, ptr %3128
  %3129 = getelementptr [3 x i32], ptr %stack.ptr_895, i32 0, i32 1
  store i32 4294967295, ptr %3129
  %3130 = getelementptr [3 x i32], ptr %stack.ptr_895, i32 0, i32 2
  store i32 4294967295, ptr %3130
  %3131 = getelementptr %program, ptr %arg_0, i32 0, i32 55
  call ccc void @eclair_btree_lower_bound_0(ptr %3131, ptr %stack.ptr_894, ptr %stack.ptr_896)
  %3132 = getelementptr %program, ptr %arg_0, i32 0, i32 55
  call ccc void @eclair_btree_upper_bound_0(ptr %3132, ptr %stack.ptr_895, ptr %stack.ptr_897)
  br label %loop_196
loop_196:
  %3133 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_896, ptr %stack.ptr_897)
  br i1 %3133, label %if_239, label %end_if_239
if_239:
  br label %range_query.end_191
end_if_239:
  %3134 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_896)
  %3135 = getelementptr [3 x i32], ptr %3134, i32 0, i32 2
  %3136 = load i32, ptr %3135
  %3137 = icmp ne i32 %3136, 54
  br i1 %3137, label %if_240, label %end_if_241
if_240:
  %3138 = getelementptr [2 x i32], ptr %stack.ptr_898, i32 0, i32 0
  %3139 = getelementptr [3 x i32], ptr %3134, i32 0, i32 0
  %3140 = load i32, ptr %3139
  store i32 %3140, ptr %3138
  %3141 = getelementptr [2 x i32], ptr %stack.ptr_898, i32 0, i32 1
  %3142 = getelementptr [3 x i32], ptr %3134, i32 0, i32 2
  %3143 = load i32, ptr %3142
  store i32 %3143, ptr %3141
  %3144 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3145 = call ccc i1 @eclair_btree_contains_1(ptr %3144, ptr %stack.ptr_898)
  %3146 = select i1 %3145, i1 0, i1 1
  br i1 %3146, label %if_241, label %end_if_240
if_241:
  %3147 = getelementptr [3 x i32], ptr %stack.ptr_899, i32 0, i32 0
  %3148 = getelementptr [3 x i32], ptr %3134, i32 0, i32 0
  %3149 = load i32, ptr %3148
  store i32 %3149, ptr %3147
  %3150 = getelementptr [3 x i32], ptr %stack.ptr_899, i32 0, i32 1
  %3151 = getelementptr [3 x i32], ptr %3134, i32 0, i32 1
  %3152 = load i32, ptr %3151
  store i32 %3152, ptr %3150
  %3153 = getelementptr [3 x i32], ptr %stack.ptr_899, i32 0, i32 2
  %3154 = getelementptr [3 x i32], ptr %3134, i32 0, i32 2
  %3155 = load i32, ptr %3154
  store i32 %3155, ptr %3153
  %3156 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3157 = call ccc i1 @eclair_btree_insert_value_0(ptr %3156, ptr %stack.ptr_899)
  br label %end_if_240
end_if_240:
  br label %end_if_241
end_if_241:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_896)
  br label %loop_196
range_query.end_191:
  %3158 = getelementptr [3 x i32], ptr %stack.ptr_900, i32 0, i32 0
  store i32 0, ptr %3158
  %3159 = getelementptr [3 x i32], ptr %stack.ptr_900, i32 0, i32 1
  store i32 0, ptr %3159
  %3160 = getelementptr [3 x i32], ptr %stack.ptr_900, i32 0, i32 2
  store i32 0, ptr %3160
  %3161 = getelementptr [3 x i32], ptr %stack.ptr_901, i32 0, i32 0
  store i32 4294967295, ptr %3161
  %3162 = getelementptr [3 x i32], ptr %stack.ptr_901, i32 0, i32 1
  store i32 4294967295, ptr %3162
  %3163 = getelementptr [3 x i32], ptr %stack.ptr_901, i32 0, i32 2
  store i32 4294967295, ptr %3163
  %3164 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %3164, ptr %stack.ptr_900, ptr %stack.ptr_902)
  %3165 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %3165, ptr %stack.ptr_901, ptr %stack.ptr_903)
  br label %loop_197
loop_197:
  %3166 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_902, ptr %stack.ptr_903)
  br i1 %3166, label %if_242, label %end_if_242
if_242:
  br label %range_query.end_192
end_if_242:
  %3167 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_902)
  %3168 = getelementptr [4 x i32], ptr %stack.ptr_904, i32 0, i32 0
  %3169 = getelementptr [3 x i32], ptr %3167, i32 0, i32 2
  %3170 = load i32, ptr %3169
  store i32 %3170, ptr %3168
  %3171 = getelementptr [4 x i32], ptr %stack.ptr_904, i32 0, i32 1
  store i32 0, ptr %3171
  %3172 = getelementptr [4 x i32], ptr %stack.ptr_904, i32 0, i32 2
  store i32 0, ptr %3172
  %3173 = getelementptr [4 x i32], ptr %stack.ptr_904, i32 0, i32 3
  store i32 0, ptr %3173
  %3174 = getelementptr [4 x i32], ptr %stack.ptr_905, i32 0, i32 0
  %3175 = getelementptr [3 x i32], ptr %3167, i32 0, i32 2
  %3176 = load i32, ptr %3175
  store i32 %3176, ptr %3174
  %3177 = getelementptr [4 x i32], ptr %stack.ptr_905, i32 0, i32 1
  store i32 4294967295, ptr %3177
  %3178 = getelementptr [4 x i32], ptr %stack.ptr_905, i32 0, i32 2
  store i32 4294967295, ptr %3178
  %3179 = getelementptr [4 x i32], ptr %stack.ptr_905, i32 0, i32 3
  store i32 4294967295, ptr %3179
  %3180 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %3180, ptr %stack.ptr_904, ptr %stack.ptr_906)
  %3181 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %3181, ptr %stack.ptr_905, ptr %stack.ptr_907)
  br label %loop_198
loop_198:
  %3182 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_906, ptr %stack.ptr_907)
  br i1 %3182, label %if_243, label %end_if_243
if_243:
  br label %range_query.end_193
end_if_243:
  %3183 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_906)
  %3184 = getelementptr [2 x i32], ptr %stack.ptr_908, i32 0, i32 0
  %3185 = getelementptr [4 x i32], ptr %3183, i32 0, i32 2
  %3186 = load i32, ptr %3185
  store i32 %3186, ptr %3184
  %3187 = getelementptr [2 x i32], ptr %stack.ptr_908, i32 0, i32 1
  store i32 0, ptr %3187
  %3188 = getelementptr [2 x i32], ptr %stack.ptr_909, i32 0, i32 0
  %3189 = getelementptr [4 x i32], ptr %3183, i32 0, i32 2
  %3190 = load i32, ptr %3189
  store i32 %3190, ptr %3188
  %3191 = getelementptr [2 x i32], ptr %stack.ptr_909, i32 0, i32 1
  store i32 4294967295, ptr %3191
  %3192 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3192, ptr %stack.ptr_908, ptr %stack.ptr_910)
  %3193 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3193, ptr %stack.ptr_909, ptr %stack.ptr_911)
  br label %loop_199
loop_199:
  %3194 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_910, ptr %stack.ptr_911)
  br i1 %3194, label %if_244, label %end_if_244
if_244:
  br label %range_query.end_194
end_if_244:
  %3195 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_910)
  %3196 = getelementptr [2 x i32], ptr %3195, i32 0, i32 1
  %3197 = load i32, ptr %3196
  %3198 = icmp ne i32 %3197, 54
  br i1 %3198, label %if_245, label %end_if_246
if_245:
  %3199 = getelementptr [2 x i32], ptr %stack.ptr_912, i32 0, i32 0
  %3200 = getelementptr [3 x i32], ptr %3167, i32 0, i32 0
  %3201 = load i32, ptr %3200
  store i32 %3201, ptr %3199
  %3202 = getelementptr [2 x i32], ptr %stack.ptr_912, i32 0, i32 1
  %3203 = getelementptr [2 x i32], ptr %3195, i32 0, i32 1
  %3204 = load i32, ptr %3203
  store i32 %3204, ptr %3202
  %3205 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3206 = call ccc i1 @eclair_btree_contains_1(ptr %3205, ptr %stack.ptr_912)
  %3207 = select i1 %3206, i1 0, i1 1
  br i1 %3207, label %if_246, label %end_if_245
if_246:
  %3208 = getelementptr [3 x i32], ptr %stack.ptr_913, i32 0, i32 0
  %3209 = getelementptr [3 x i32], ptr %3167, i32 0, i32 0
  %3210 = load i32, ptr %3209
  store i32 %3210, ptr %3208
  %3211 = getelementptr [3 x i32], ptr %stack.ptr_913, i32 0, i32 1
  %3212 = getelementptr [4 x i32], ptr %3183, i32 0, i32 2
  %3213 = load i32, ptr %3212
  store i32 %3213, ptr %3211
  %3214 = getelementptr [3 x i32], ptr %stack.ptr_913, i32 0, i32 2
  %3215 = getelementptr [2 x i32], ptr %3195, i32 0, i32 1
  %3216 = load i32, ptr %3215
  store i32 %3216, ptr %3214
  %3217 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3218 = call ccc i1 @eclair_btree_insert_value_0(ptr %3217, ptr %stack.ptr_913)
  br label %end_if_245
end_if_245:
  br label %end_if_246
end_if_246:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_910)
  br label %loop_199
range_query.end_194:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_906)
  br label %loop_198
range_query.end_193:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_902)
  br label %loop_197
range_query.end_192:
  %3219 = getelementptr [3 x i32], ptr %stack.ptr_914, i32 0, i32 0
  store i32 0, ptr %3219
  %3220 = getelementptr [3 x i32], ptr %stack.ptr_914, i32 0, i32 1
  store i32 0, ptr %3220
  %3221 = getelementptr [3 x i32], ptr %stack.ptr_914, i32 0, i32 2
  store i32 0, ptr %3221
  %3222 = getelementptr [3 x i32], ptr %stack.ptr_915, i32 0, i32 0
  store i32 4294967295, ptr %3222
  %3223 = getelementptr [3 x i32], ptr %stack.ptr_915, i32 0, i32 1
  store i32 4294967295, ptr %3223
  %3224 = getelementptr [3 x i32], ptr %stack.ptr_915, i32 0, i32 2
  store i32 4294967295, ptr %3224
  %3225 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %3225, ptr %stack.ptr_914, ptr %stack.ptr_916)
  %3226 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %3226, ptr %stack.ptr_915, ptr %stack.ptr_917)
  br label %loop_200
loop_200:
  %3227 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_916, ptr %stack.ptr_917)
  br i1 %3227, label %if_247, label %end_if_247
if_247:
  br label %range_query.end_195
end_if_247:
  %3228 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_916)
  %3229 = getelementptr [4 x i32], ptr %stack.ptr_918, i32 0, i32 0
  %3230 = getelementptr [3 x i32], ptr %3228, i32 0, i32 2
  %3231 = load i32, ptr %3230
  store i32 %3231, ptr %3229
  %3232 = getelementptr [4 x i32], ptr %stack.ptr_918, i32 0, i32 1
  store i32 0, ptr %3232
  %3233 = getelementptr [4 x i32], ptr %stack.ptr_918, i32 0, i32 2
  store i32 0, ptr %3233
  %3234 = getelementptr [4 x i32], ptr %stack.ptr_918, i32 0, i32 3
  store i32 0, ptr %3234
  %3235 = getelementptr [4 x i32], ptr %stack.ptr_919, i32 0, i32 0
  %3236 = getelementptr [3 x i32], ptr %3228, i32 0, i32 2
  %3237 = load i32, ptr %3236
  store i32 %3237, ptr %3235
  %3238 = getelementptr [4 x i32], ptr %stack.ptr_919, i32 0, i32 1
  store i32 4294967295, ptr %3238
  %3239 = getelementptr [4 x i32], ptr %stack.ptr_919, i32 0, i32 2
  store i32 4294967295, ptr %3239
  %3240 = getelementptr [4 x i32], ptr %stack.ptr_919, i32 0, i32 3
  store i32 4294967295, ptr %3240
  %3241 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %3241, ptr %stack.ptr_918, ptr %stack.ptr_920)
  %3242 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %3242, ptr %stack.ptr_919, ptr %stack.ptr_921)
  br label %loop_201
loop_201:
  %3243 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_920, ptr %stack.ptr_921)
  br i1 %3243, label %if_248, label %end_if_248
if_248:
  br label %range_query.end_196
end_if_248:
  %3244 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_920)
  %3245 = getelementptr [2 x i32], ptr %stack.ptr_922, i32 0, i32 0
  %3246 = getelementptr [4 x i32], ptr %3244, i32 0, i32 3
  %3247 = load i32, ptr %3246
  store i32 %3247, ptr %3245
  %3248 = getelementptr [2 x i32], ptr %stack.ptr_922, i32 0, i32 1
  store i32 0, ptr %3248
  %3249 = getelementptr [2 x i32], ptr %stack.ptr_923, i32 0, i32 0
  %3250 = getelementptr [4 x i32], ptr %3244, i32 0, i32 3
  %3251 = load i32, ptr %3250
  store i32 %3251, ptr %3249
  %3252 = getelementptr [2 x i32], ptr %stack.ptr_923, i32 0, i32 1
  store i32 4294967295, ptr %3252
  %3253 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3253, ptr %stack.ptr_922, ptr %stack.ptr_924)
  %3254 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3254, ptr %stack.ptr_923, ptr %stack.ptr_925)
  br label %loop_202
loop_202:
  %3255 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_924, ptr %stack.ptr_925)
  br i1 %3255, label %if_249, label %end_if_249
if_249:
  br label %range_query.end_197
end_if_249:
  %3256 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_924)
  %3257 = getelementptr [2 x i32], ptr %3256, i32 0, i32 1
  %3258 = load i32, ptr %3257
  %3259 = icmp ne i32 %3258, 54
  br i1 %3259, label %if_250, label %end_if_251
if_250:
  %3260 = getelementptr [2 x i32], ptr %stack.ptr_926, i32 0, i32 0
  %3261 = getelementptr [3 x i32], ptr %3228, i32 0, i32 0
  %3262 = load i32, ptr %3261
  store i32 %3262, ptr %3260
  %3263 = getelementptr [2 x i32], ptr %stack.ptr_926, i32 0, i32 1
  %3264 = getelementptr [2 x i32], ptr %3256, i32 0, i32 1
  %3265 = load i32, ptr %3264
  store i32 %3265, ptr %3263
  %3266 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3267 = call ccc i1 @eclair_btree_contains_1(ptr %3266, ptr %stack.ptr_926)
  %3268 = select i1 %3267, i1 0, i1 1
  br i1 %3268, label %if_251, label %end_if_250
if_251:
  %3269 = getelementptr [3 x i32], ptr %stack.ptr_927, i32 0, i32 0
  %3270 = getelementptr [3 x i32], ptr %3228, i32 0, i32 0
  %3271 = load i32, ptr %3270
  store i32 %3271, ptr %3269
  %3272 = getelementptr [3 x i32], ptr %stack.ptr_927, i32 0, i32 1
  %3273 = getelementptr [4 x i32], ptr %3244, i32 0, i32 3
  %3274 = load i32, ptr %3273
  store i32 %3274, ptr %3272
  %3275 = getelementptr [3 x i32], ptr %stack.ptr_927, i32 0, i32 2
  %3276 = getelementptr [2 x i32], ptr %3256, i32 0, i32 1
  %3277 = load i32, ptr %3276
  store i32 %3277, ptr %3275
  %3278 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3279 = call ccc i1 @eclair_btree_insert_value_0(ptr %3278, ptr %stack.ptr_927)
  br label %end_if_250
end_if_250:
  br label %end_if_251
end_if_251:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_924)
  br label %loop_202
range_query.end_197:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_920)
  br label %loop_201
range_query.end_196:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_916)
  br label %loop_200
range_query.end_195:
  %3280 = getelementptr [4 x i32], ptr %stack.ptr_928, i32 0, i32 0
  store i32 0, ptr %3280
  %3281 = getelementptr [4 x i32], ptr %stack.ptr_928, i32 0, i32 1
  store i32 0, ptr %3281
  %3282 = getelementptr [4 x i32], ptr %stack.ptr_928, i32 0, i32 2
  store i32 0, ptr %3282
  %3283 = getelementptr [4 x i32], ptr %stack.ptr_928, i32 0, i32 3
  store i32 0, ptr %3283
  %3284 = getelementptr [4 x i32], ptr %stack.ptr_929, i32 0, i32 0
  store i32 4294967295, ptr %3284
  %3285 = getelementptr [4 x i32], ptr %stack.ptr_929, i32 0, i32 1
  store i32 4294967295, ptr %3285
  %3286 = getelementptr [4 x i32], ptr %stack.ptr_929, i32 0, i32 2
  store i32 4294967295, ptr %3286
  %3287 = getelementptr [4 x i32], ptr %stack.ptr_929, i32 0, i32 3
  store i32 4294967295, ptr %3287
  %3288 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %3288, ptr %stack.ptr_928, ptr %stack.ptr_930)
  %3289 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %3289, ptr %stack.ptr_929, ptr %stack.ptr_931)
  br label %loop_203
loop_203:
  %3290 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_930, ptr %stack.ptr_931)
  br i1 %3290, label %if_252, label %end_if_252
if_252:
  br label %range_query.end_198
end_if_252:
  %3291 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_930)
  %3292 = getelementptr [2 x i32], ptr %stack.ptr_932, i32 0, i32 0
  %3293 = getelementptr [4 x i32], ptr %3291, i32 0, i32 2
  %3294 = load i32, ptr %3293
  store i32 %3294, ptr %3292
  %3295 = getelementptr [2 x i32], ptr %stack.ptr_932, i32 0, i32 1
  store i32 0, ptr %3295
  %3296 = getelementptr [2 x i32], ptr %stack.ptr_933, i32 0, i32 0
  %3297 = getelementptr [4 x i32], ptr %3291, i32 0, i32 2
  %3298 = load i32, ptr %3297
  store i32 %3298, ptr %3296
  %3299 = getelementptr [2 x i32], ptr %stack.ptr_933, i32 0, i32 1
  store i32 4294967295, ptr %3299
  %3300 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3300, ptr %stack.ptr_932, ptr %stack.ptr_934)
  %3301 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3301, ptr %stack.ptr_933, ptr %stack.ptr_935)
  br label %loop_204
loop_204:
  %3302 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_934, ptr %stack.ptr_935)
  br i1 %3302, label %if_253, label %end_if_253
if_253:
  br label %range_query.end_199
end_if_253:
  %3303 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_934)
  %3304 = getelementptr [2 x i32], ptr %stack.ptr_936, i32 0, i32 0
  store i32 0, ptr %3304
  %3305 = getelementptr [2 x i32], ptr %stack.ptr_936, i32 0, i32 1
  %3306 = getelementptr [4 x i32], ptr %3291, i32 0, i32 2
  %3307 = load i32, ptr %3306
  store i32 %3307, ptr %3305
  %3308 = getelementptr [2 x i32], ptr %stack.ptr_937, i32 0, i32 0
  store i32 4294967295, ptr %3308
  %3309 = getelementptr [2 x i32], ptr %stack.ptr_937, i32 0, i32 1
  %3310 = getelementptr [4 x i32], ptr %3291, i32 0, i32 2
  %3311 = load i32, ptr %3310
  store i32 %3311, ptr %3309
  %3312 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_lower_bound_2(ptr %3312, ptr %stack.ptr_936, ptr %stack.ptr_938)
  %3313 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_upper_bound_2(ptr %3313, ptr %stack.ptr_937, ptr %stack.ptr_939)
  br label %loop_205
loop_205:
  %3314 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_938, ptr %stack.ptr_939)
  br i1 %3314, label %if_254, label %end_if_254
if_254:
  br label %range_query.end_200
end_if_254:
  %3315 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_938)
  %3316 = getelementptr [2 x i32], ptr %stack.ptr_940, i32 0, i32 0
  store i32 0, ptr %3316
  %3317 = getelementptr [2 x i32], ptr %stack.ptr_940, i32 0, i32 1
  %3318 = getelementptr [2 x i32], ptr %3315, i32 0, i32 0
  %3319 = load i32, ptr %3318
  store i32 %3319, ptr %3317
  %3320 = getelementptr [2 x i32], ptr %stack.ptr_941, i32 0, i32 0
  store i32 4294967295, ptr %3320
  %3321 = getelementptr [2 x i32], ptr %stack.ptr_941, i32 0, i32 1
  %3322 = getelementptr [2 x i32], ptr %3315, i32 0, i32 0
  %3323 = load i32, ptr %3322
  store i32 %3323, ptr %3321
  %3324 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_lower_bound_2(ptr %3324, ptr %stack.ptr_940, ptr %stack.ptr_942)
  %3325 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_upper_bound_2(ptr %3325, ptr %stack.ptr_941, ptr %stack.ptr_943)
  br label %loop_206
loop_206:
  %3326 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_942, ptr %stack.ptr_943)
  br i1 %3326, label %if_255, label %end_if_255
if_255:
  br label %range_query.end_201
end_if_255:
  %3327 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_942)
  %3328 = getelementptr [2 x i32], ptr %stack.ptr_944, i32 0, i32 0
  %3329 = getelementptr [2 x i32], ptr %3327, i32 0, i32 0
  %3330 = load i32, ptr %3329
  store i32 %3330, ptr %3328
  %3331 = getelementptr [2 x i32], ptr %stack.ptr_944, i32 0, i32 1
  %3332 = getelementptr [2 x i32], ptr %3303, i32 0, i32 1
  %3333 = load i32, ptr %3332
  store i32 %3333, ptr %3331
  %3334 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3335 = call ccc i1 @eclair_btree_contains_1(ptr %3334, ptr %stack.ptr_944)
  %3336 = select i1 %3335, i1 0, i1 1
  br i1 %3336, label %if_256, label %end_if_256
if_256:
  %3337 = getelementptr [3 x i32], ptr %stack.ptr_945, i32 0, i32 0
  %3338 = getelementptr [2 x i32], ptr %3327, i32 0, i32 0
  %3339 = load i32, ptr %3338
  store i32 %3339, ptr %3337
  %3340 = getelementptr [3 x i32], ptr %stack.ptr_945, i32 0, i32 1
  %3341 = getelementptr [4 x i32], ptr %3291, i32 0, i32 2
  %3342 = load i32, ptr %3341
  store i32 %3342, ptr %3340
  %3343 = getelementptr [3 x i32], ptr %stack.ptr_945, i32 0, i32 2
  %3344 = getelementptr [2 x i32], ptr %3303, i32 0, i32 1
  %3345 = load i32, ptr %3344
  store i32 %3345, ptr %3343
  %3346 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3347 = call ccc i1 @eclair_btree_insert_value_0(ptr %3346, ptr %stack.ptr_945)
  br label %end_if_256
end_if_256:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_942)
  br label %loop_206
range_query.end_201:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_938)
  br label %loop_205
range_query.end_200:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_934)
  br label %loop_204
range_query.end_199:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_930)
  br label %loop_203
range_query.end_198:
  %3348 = getelementptr [4 x i32], ptr %stack.ptr_946, i32 0, i32 0
  store i32 0, ptr %3348
  %3349 = getelementptr [4 x i32], ptr %stack.ptr_946, i32 0, i32 1
  store i32 0, ptr %3349
  %3350 = getelementptr [4 x i32], ptr %stack.ptr_946, i32 0, i32 2
  store i32 0, ptr %3350
  %3351 = getelementptr [4 x i32], ptr %stack.ptr_946, i32 0, i32 3
  store i32 0, ptr %3351
  %3352 = getelementptr [4 x i32], ptr %stack.ptr_947, i32 0, i32 0
  store i32 4294967295, ptr %3352
  %3353 = getelementptr [4 x i32], ptr %stack.ptr_947, i32 0, i32 1
  store i32 4294967295, ptr %3353
  %3354 = getelementptr [4 x i32], ptr %stack.ptr_947, i32 0, i32 2
  store i32 4294967295, ptr %3354
  %3355 = getelementptr [4 x i32], ptr %stack.ptr_947, i32 0, i32 3
  store i32 4294967295, ptr %3355
  %3356 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %3356, ptr %stack.ptr_946, ptr %stack.ptr_948)
  %3357 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %3357, ptr %stack.ptr_947, ptr %stack.ptr_949)
  br label %loop_207
loop_207:
  %3358 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_948, ptr %stack.ptr_949)
  br i1 %3358, label %if_257, label %end_if_257
if_257:
  br label %range_query.end_202
end_if_257:
  %3359 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_948)
  %3360 = getelementptr [2 x i32], ptr %stack.ptr_950, i32 0, i32 0
  %3361 = getelementptr [4 x i32], ptr %3359, i32 0, i32 3
  %3362 = load i32, ptr %3361
  store i32 %3362, ptr %3360
  %3363 = getelementptr [2 x i32], ptr %stack.ptr_950, i32 0, i32 1
  store i32 0, ptr %3363
  %3364 = getelementptr [2 x i32], ptr %stack.ptr_951, i32 0, i32 0
  %3365 = getelementptr [4 x i32], ptr %3359, i32 0, i32 3
  %3366 = load i32, ptr %3365
  store i32 %3366, ptr %3364
  %3367 = getelementptr [2 x i32], ptr %stack.ptr_951, i32 0, i32 1
  store i32 4294967295, ptr %3367
  %3368 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3368, ptr %stack.ptr_950, ptr %stack.ptr_952)
  %3369 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3369, ptr %stack.ptr_951, ptr %stack.ptr_953)
  br label %loop_208
loop_208:
  %3370 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_952, ptr %stack.ptr_953)
  br i1 %3370, label %if_258, label %end_if_258
if_258:
  br label %range_query.end_203
end_if_258:
  %3371 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_952)
  %3372 = getelementptr [2 x i32], ptr %stack.ptr_954, i32 0, i32 0
  store i32 0, ptr %3372
  %3373 = getelementptr [2 x i32], ptr %stack.ptr_954, i32 0, i32 1
  %3374 = getelementptr [4 x i32], ptr %3359, i32 0, i32 3
  %3375 = load i32, ptr %3374
  store i32 %3375, ptr %3373
  %3376 = getelementptr [2 x i32], ptr %stack.ptr_955, i32 0, i32 0
  store i32 4294967295, ptr %3376
  %3377 = getelementptr [2 x i32], ptr %stack.ptr_955, i32 0, i32 1
  %3378 = getelementptr [4 x i32], ptr %3359, i32 0, i32 3
  %3379 = load i32, ptr %3378
  store i32 %3379, ptr %3377
  %3380 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_lower_bound_2(ptr %3380, ptr %stack.ptr_954, ptr %stack.ptr_956)
  %3381 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_upper_bound_2(ptr %3381, ptr %stack.ptr_955, ptr %stack.ptr_957)
  br label %loop_209
loop_209:
  %3382 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_956, ptr %stack.ptr_957)
  br i1 %3382, label %if_259, label %end_if_259
if_259:
  br label %range_query.end_204
end_if_259:
  %3383 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_956)
  %3384 = getelementptr [2 x i32], ptr %stack.ptr_958, i32 0, i32 0
  store i32 0, ptr %3384
  %3385 = getelementptr [2 x i32], ptr %stack.ptr_958, i32 0, i32 1
  %3386 = getelementptr [2 x i32], ptr %3383, i32 0, i32 0
  %3387 = load i32, ptr %3386
  store i32 %3387, ptr %3385
  %3388 = getelementptr [2 x i32], ptr %stack.ptr_959, i32 0, i32 0
  store i32 4294967295, ptr %3388
  %3389 = getelementptr [2 x i32], ptr %stack.ptr_959, i32 0, i32 1
  %3390 = getelementptr [2 x i32], ptr %3383, i32 0, i32 0
  %3391 = load i32, ptr %3390
  store i32 %3391, ptr %3389
  %3392 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_lower_bound_2(ptr %3392, ptr %stack.ptr_958, ptr %stack.ptr_960)
  %3393 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_upper_bound_2(ptr %3393, ptr %stack.ptr_959, ptr %stack.ptr_961)
  br label %loop_210
loop_210:
  %3394 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_960, ptr %stack.ptr_961)
  br i1 %3394, label %if_260, label %end_if_260
if_260:
  br label %range_query.end_205
end_if_260:
  %3395 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_960)
  %3396 = getelementptr [2 x i32], ptr %stack.ptr_962, i32 0, i32 0
  %3397 = getelementptr [2 x i32], ptr %3395, i32 0, i32 0
  %3398 = load i32, ptr %3397
  store i32 %3398, ptr %3396
  %3399 = getelementptr [2 x i32], ptr %stack.ptr_962, i32 0, i32 1
  %3400 = getelementptr [2 x i32], ptr %3371, i32 0, i32 1
  %3401 = load i32, ptr %3400
  store i32 %3401, ptr %3399
  %3402 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3403 = call ccc i1 @eclair_btree_contains_1(ptr %3402, ptr %stack.ptr_962)
  %3404 = select i1 %3403, i1 0, i1 1
  br i1 %3404, label %if_261, label %end_if_261
if_261:
  %3405 = getelementptr [3 x i32], ptr %stack.ptr_963, i32 0, i32 0
  %3406 = getelementptr [2 x i32], ptr %3395, i32 0, i32 0
  %3407 = load i32, ptr %3406
  store i32 %3407, ptr %3405
  %3408 = getelementptr [3 x i32], ptr %stack.ptr_963, i32 0, i32 1
  %3409 = getelementptr [4 x i32], ptr %3359, i32 0, i32 3
  %3410 = load i32, ptr %3409
  store i32 %3410, ptr %3408
  %3411 = getelementptr [3 x i32], ptr %stack.ptr_963, i32 0, i32 2
  %3412 = getelementptr [2 x i32], ptr %3371, i32 0, i32 1
  %3413 = load i32, ptr %3412
  store i32 %3413, ptr %3411
  %3414 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3415 = call ccc i1 @eclair_btree_insert_value_0(ptr %3414, ptr %stack.ptr_963)
  br label %end_if_261
end_if_261:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_960)
  br label %loop_210
range_query.end_205:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_956)
  br label %loop_209
range_query.end_204:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_952)
  br label %loop_208
range_query.end_203:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_948)
  br label %loop_207
range_query.end_202:
  ret void
}

define external ccc void @eclair_add_facts(ptr %eclair_program_0, i32 %fact_type_0, ptr %memory_0, i32 %fact_count_0) "wasm-export-name"="eclair_add_facts" {
start:
  switch i32 %fact_type_0, label %switch.default_0 [i32 5, label %atom_0 i32 6, label %atom_arg_0 i32 4, label %binop_0 i32 3, label %constraint_0 i32 15, label %declare_type_0 i32 14, label %extern_definition_0 i32 11, label %input_relation_0 i32 13, label %internal_relation_0 i32 0, label %lit_number_0 i32 1, label %lit_string_0 i32 16, label %module_declaration_0 i32 10, label %negation_0 i32 12, label %output_relation_0 i32 7, label %rule_0 i32 8, label %rule_arg_0 i32 9, label %rule_clause_0 i32 17, label %scoped_value_0 i32 2, label %variable_0]
atom_0:
  %0 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 4
  br label %for_begin_0
for_begin_0:
  %1 = phi i32 [0, %atom_0], [%5, %for_body_0]
  %2 = icmp ult i32 %1, %fact_count_0
  br i1 %2, label %for_body_0, label %for_end_0
for_body_0:
  %3 = getelementptr [2 x i32], ptr %memory_0, i32 %1
  %4 = call ccc i1 @eclair_btree_insert_value_2(ptr %0, ptr %3)
  %5 = add i32 1, %1
  br label %for_begin_0
for_end_0:
  %6 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 3
  br label %for_begin_1
for_begin_1:
  %7 = phi i32 [0, %for_end_0], [%11, %for_body_1]
  %8 = icmp ult i32 %7, %fact_count_0
  br i1 %8, label %for_body_1, label %for_end_1
for_body_1:
  %9 = getelementptr [2 x i32], ptr %memory_0, i32 %7
  %10 = call ccc i1 @eclair_btree_insert_value_1(ptr %6, ptr %9)
  %11 = add i32 1, %7
  br label %for_begin_1
for_end_1:
  br label %end_0
atom_arg_0:
  %12 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 5
  br label %for_begin_2
for_begin_2:
  %13 = phi i32 [0, %atom_arg_0], [%17, %for_body_2]
  %14 = icmp ult i32 %13, %fact_count_0
  br i1 %14, label %for_body_2, label %for_end_2
for_body_2:
  %15 = getelementptr [3 x i32], ptr %memory_0, i32 %13
  %16 = call ccc i1 @eclair_btree_insert_value_0(ptr %12, ptr %15)
  %17 = add i32 1, %13
  br label %for_begin_2
for_end_2:
  br label %end_0
binop_0:
  %18 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 6
  br label %for_begin_3
for_begin_3:
  %19 = phi i32 [0, %binop_0], [%23, %for_body_3]
  %20 = icmp ult i32 %19, %fact_count_0
  br i1 %20, label %for_body_3, label %for_end_3
for_body_3:
  %21 = getelementptr [4 x i32], ptr %memory_0, i32 %19
  %22 = call ccc i1 @eclair_btree_insert_value_3(ptr %18, ptr %21)
  %23 = add i32 1, %19
  br label %for_begin_3
for_end_3:
  br label %end_0
constraint_0:
  %24 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 10
  br label %for_begin_4
for_begin_4:
  %25 = phi i32 [0, %constraint_0], [%29, %for_body_4]
  %26 = icmp ult i32 %25, %fact_count_0
  br i1 %26, label %for_body_4, label %for_end_4
for_body_4:
  %27 = getelementptr [4 x i32], ptr %memory_0, i32 %25
  %28 = call ccc i1 @eclair_btree_insert_value_5(ptr %24, ptr %27)
  %29 = add i32 1, %25
  br label %for_begin_4
for_end_4:
  %30 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 9
  br label %for_begin_5
for_begin_5:
  %31 = phi i32 [0, %for_end_4], [%35, %for_body_5]
  %32 = icmp ult i32 %31, %fact_count_0
  br i1 %32, label %for_body_5, label %for_end_5
for_body_5:
  %33 = getelementptr [4 x i32], ptr %memory_0, i32 %31
  %34 = call ccc i1 @eclair_btree_insert_value_4(ptr %30, ptr %33)
  %35 = add i32 1, %31
  br label %for_begin_5
for_end_5:
  br label %end_0
declare_type_0:
  %36 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 15
  br label %for_begin_6
for_begin_6:
  %37 = phi i32 [0, %declare_type_0], [%41, %for_body_6]
  %38 = icmp ult i32 %37, %fact_count_0
  br i1 %38, label %for_body_6, label %for_end_6
for_body_6:
  %39 = getelementptr [2 x i32], ptr %memory_0, i32 %37
  %40 = call ccc i1 @eclair_btree_insert_value_2(ptr %36, ptr %39)
  %41 = add i32 1, %37
  br label %for_begin_6
for_end_6:
  %42 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 14
  br label %for_begin_7
for_begin_7:
  %43 = phi i32 [0, %for_end_6], [%47, %for_body_7]
  %44 = icmp ult i32 %43, %fact_count_0
  br i1 %44, label %for_body_7, label %for_end_7
for_body_7:
  %45 = getelementptr [2 x i32], ptr %memory_0, i32 %43
  %46 = call ccc i1 @eclair_btree_insert_value_1(ptr %42, ptr %45)
  %47 = add i32 1, %43
  br label %for_begin_7
for_end_7:
  br label %end_0
extern_definition_0:
  %48 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 24
  br label %for_begin_8
for_begin_8:
  %49 = phi i32 [0, %extern_definition_0], [%53, %for_body_8]
  %50 = icmp ult i32 %49, %fact_count_0
  br i1 %50, label %for_body_8, label %for_end_8
for_body_8:
  %51 = getelementptr [2 x i32], ptr %memory_0, i32 %49
  %52 = call ccc i1 @eclair_btree_insert_value_7(ptr %48, ptr %51)
  %53 = add i32 1, %49
  br label %for_begin_8
for_end_8:
  br label %end_0
input_relation_0:
  %54 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 32
  br label %for_begin_9
for_begin_9:
  %55 = phi i32 [0, %input_relation_0], [%59, %for_body_9]
  %56 = icmp ult i32 %55, %fact_count_0
  br i1 %56, label %for_body_9, label %for_end_9
for_body_9:
  %57 = getelementptr [1 x i32], ptr %memory_0, i32 %55
  %58 = call ccc i1 @eclair_btree_insert_value_6(ptr %54, ptr %57)
  %59 = add i32 1, %55
  br label %for_begin_9
for_end_9:
  br label %end_0
internal_relation_0:
  %60 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 33
  br label %for_begin_10
for_begin_10:
  %61 = phi i32 [0, %internal_relation_0], [%65, %for_body_10]
  %62 = icmp ult i32 %61, %fact_count_0
  br i1 %62, label %for_body_10, label %for_end_10
for_body_10:
  %63 = getelementptr [1 x i32], ptr %memory_0, i32 %61
  %64 = call ccc i1 @eclair_btree_insert_value_6(ptr %60, ptr %63)
  %65 = add i32 1, %61
  br label %for_begin_10
for_end_10:
  br label %end_0
lit_number_0:
  %66 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 34
  br label %for_begin_11
for_begin_11:
  %67 = phi i32 [0, %lit_number_0], [%71, %for_body_11]
  %68 = icmp ult i32 %67, %fact_count_0
  br i1 %68, label %for_body_11, label %for_end_11
for_body_11:
  %69 = getelementptr [2 x i32], ptr %memory_0, i32 %67
  %70 = call ccc i1 @eclair_btree_insert_value_1(ptr %66, ptr %69)
  %71 = add i32 1, %67
  br label %for_begin_11
for_end_11:
  br label %end_0
lit_string_0:
  %72 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 35
  br label %for_begin_12
for_begin_12:
  %73 = phi i32 [0, %lit_string_0], [%77, %for_body_12]
  %74 = icmp ult i32 %73, %fact_count_0
  br i1 %74, label %for_body_12, label %for_end_12
for_body_12:
  %75 = getelementptr [2 x i32], ptr %memory_0, i32 %73
  %76 = call ccc i1 @eclair_btree_insert_value_1(ptr %72, ptr %75)
  %77 = add i32 1, %73
  br label %for_begin_12
for_end_12:
  br label %end_0
module_declaration_0:
  %78 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 39
  br label %for_begin_13
for_begin_13:
  %79 = phi i32 [0, %module_declaration_0], [%83, %for_body_13]
  %80 = icmp ult i32 %79, %fact_count_0
  br i1 %80, label %for_body_13, label %for_end_13
for_body_13:
  %81 = getelementptr [2 x i32], ptr %memory_0, i32 %79
  %82 = call ccc i1 @eclair_btree_insert_value_1(ptr %78, ptr %81)
  %83 = add i32 1, %79
  br label %for_begin_13
for_end_13:
  br label %end_0
negation_0:
  %84 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 40
  br label %for_begin_14
for_begin_14:
  %85 = phi i32 [0, %negation_0], [%89, %for_body_14]
  %86 = icmp ult i32 %85, %fact_count_0
  br i1 %86, label %for_body_14, label %for_end_14
for_body_14:
  %87 = getelementptr [2 x i32], ptr %memory_0, i32 %85
  %88 = call ccc i1 @eclair_btree_insert_value_1(ptr %84, ptr %87)
  %89 = add i32 1, %85
  br label %for_begin_14
for_end_14:
  br label %end_0
output_relation_0:
  %90 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 47
  br label %for_begin_15
for_begin_15:
  %91 = phi i32 [0, %output_relation_0], [%95, %for_body_15]
  %92 = icmp ult i32 %91, %fact_count_0
  br i1 %92, label %for_body_15, label %for_end_15
for_body_15:
  %93 = getelementptr [1 x i32], ptr %memory_0, i32 %91
  %94 = call ccc i1 @eclair_btree_insert_value_6(ptr %90, ptr %93)
  %95 = add i32 1, %91
  br label %for_begin_15
for_end_15:
  br label %end_0
rule_0:
  %96 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 51
  br label %for_begin_16
for_begin_16:
  %97 = phi i32 [0, %rule_0], [%101, %for_body_16]
  %98 = icmp ult i32 %97, %fact_count_0
  br i1 %98, label %for_body_16, label %for_end_16
for_body_16:
  %99 = getelementptr [2 x i32], ptr %memory_0, i32 %97
  %100 = call ccc i1 @eclair_btree_insert_value_2(ptr %96, ptr %99)
  %101 = add i32 1, %97
  br label %for_begin_16
for_end_16:
  %102 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 50
  br label %for_begin_17
for_begin_17:
  %103 = phi i32 [0, %for_end_16], [%107, %for_body_17]
  %104 = icmp ult i32 %103, %fact_count_0
  br i1 %104, label %for_body_17, label %for_end_17
for_body_17:
  %105 = getelementptr [2 x i32], ptr %memory_0, i32 %103
  %106 = call ccc i1 @eclair_btree_insert_value_1(ptr %102, ptr %105)
  %107 = add i32 1, %103
  br label %for_begin_17
for_end_17:
  br label %end_0
rule_arg_0:
  %108 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 52
  br label %for_begin_18
for_begin_18:
  %109 = phi i32 [0, %rule_arg_0], [%113, %for_body_18]
  %110 = icmp ult i32 %109, %fact_count_0
  br i1 %110, label %for_body_18, label %for_end_18
for_body_18:
  %111 = getelementptr [3 x i32], ptr %memory_0, i32 %109
  %112 = call ccc i1 @eclair_btree_insert_value_0(ptr %108, ptr %111)
  %113 = add i32 1, %109
  br label %for_begin_18
for_end_18:
  br label %end_0
rule_clause_0:
  %114 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 54
  br label %for_begin_19
for_begin_19:
  %115 = phi i32 [0, %rule_clause_0], [%119, %for_body_19]
  %116 = icmp ult i32 %115, %fact_count_0
  br i1 %116, label %for_body_19, label %for_end_19
for_body_19:
  %117 = getelementptr [3 x i32], ptr %memory_0, i32 %115
  %118 = call ccc i1 @eclair_btree_insert_value_8(ptr %114, ptr %117)
  %119 = add i32 1, %115
  br label %for_begin_19
for_end_19:
  %120 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 53
  br label %for_begin_20
for_begin_20:
  %121 = phi i32 [0, %for_end_19], [%125, %for_body_20]
  %122 = icmp ult i32 %121, %fact_count_0
  br i1 %122, label %for_body_20, label %for_end_20
for_body_20:
  %123 = getelementptr [3 x i32], ptr %memory_0, i32 %121
  %124 = call ccc i1 @eclair_btree_insert_value_0(ptr %120, ptr %123)
  %125 = add i32 1, %121
  br label %for_begin_20
for_end_20:
  br label %end_0
scoped_value_0:
  %126 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 60
  br label %for_begin_21
for_begin_21:
  %127 = phi i32 [0, %scoped_value_0], [%131, %for_body_21]
  %128 = icmp ult i32 %127, %fact_count_0
  br i1 %128, label %for_body_21, label %for_end_21
for_body_21:
  %129 = getelementptr [2 x i32], ptr %memory_0, i32 %127
  %130 = call ccc i1 @eclair_btree_insert_value_2(ptr %126, ptr %129)
  %131 = add i32 1, %127
  br label %for_begin_21
for_end_21:
  %132 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 59
  br label %for_begin_22
for_begin_22:
  %133 = phi i32 [0, %for_end_21], [%137, %for_body_22]
  %134 = icmp ult i32 %133, %fact_count_0
  br i1 %134, label %for_body_22, label %for_end_22
for_body_22:
  %135 = getelementptr [2 x i32], ptr %memory_0, i32 %133
  %136 = call ccc i1 @eclair_btree_insert_value_1(ptr %132, ptr %135)
  %137 = add i32 1, %133
  br label %for_begin_22
for_end_22:
  br label %end_0
variable_0:
  %138 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 67
  br label %for_begin_23
for_begin_23:
  %139 = phi i32 [0, %variable_0], [%143, %for_body_23]
  %140 = icmp ult i32 %139, %fact_count_0
  br i1 %140, label %for_body_23, label %for_end_23
for_body_23:
  %141 = getelementptr [2 x i32], ptr %memory_0, i32 %139
  %142 = call ccc i1 @eclair_btree_insert_value_2(ptr %138, ptr %141)
  %143 = add i32 1, %139
  br label %for_begin_23
for_end_23:
  %144 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 66
  br label %for_begin_24
for_begin_24:
  %145 = phi i32 [0, %for_end_23], [%149, %for_body_24]
  %146 = icmp ult i32 %145, %fact_count_0
  br i1 %146, label %for_body_24, label %for_end_24
for_body_24:
  %147 = getelementptr [2 x i32], ptr %memory_0, i32 %145
  %148 = call ccc i1 @eclair_btree_insert_value_1(ptr %144, ptr %147)
  %149 = add i32 1, %145
  br label %for_begin_24
for_end_24:
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
  %stack.ptr_3 = alloca i32, i32 1
  %stack.ptr_4 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_5 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_6 = alloca i32, i32 1
  %stack.ptr_7 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_8 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_9 = alloca i32, i32 1
  %stack.ptr_10 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_11 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_12 = alloca i32, i32 1
  %stack.ptr_13 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_14 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_15 = alloca i32, i32 1
  %stack.ptr_16 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_17 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_18 = alloca i32, i32 1
  %stack.ptr_19 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_20 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_21 = alloca i32, i32 1
  %stack.ptr_22 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_23 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_24 = alloca i32, i32 1
  %stack.ptr_25 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_26 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_27 = alloca i32, i32 1
  %stack.ptr_28 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_29 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_30 = alloca i32, i32 1
  %stack.ptr_31 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_32 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_33 = alloca i32, i32 1
  %stack.ptr_34 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_35 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_36 = alloca i32, i32 1
  %stack.ptr_37 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_38 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_39 = alloca i32, i32 1
  %stack.ptr_40 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_41 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_42 = alloca i32, i32 1
  %stack.ptr_43 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_44 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_45 = alloca i32, i32 1
  %stack.ptr_46 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_47 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_48 = alloca i32, i32 1
  %stack.ptr_49 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_50 = alloca %btree_iterator_t_0, i32 1
  switch i32 %fact_type_0, label %switch.default_0 [i32 50, label %conflicting_definitions_0 i32 53, label %cyclic_negation_0 i32 47, label %dead_code_0 i32 49, label %dead_internal_relation_0 i32 51, label %extern_used_as_fact_0 i32 52, label %extern_used_as_rule_0 i32 37, label %grounded_variable_0 i32 48, label %no_output_relation_0 i32 46, label %rule_with_contradiction_0 i32 45, label %unconstrained_rule_var_0 i32 39, label %ungrounded_external_atom_0 i32 38, label %ungrounded_variable_0 i32 44, label %wildcard_in_binop_0 i32 43, label %wildcard_in_constraint_0 i32 41, label %wildcard_in_extern_0 i32 40, label %wildcard_in_fact_0 i32 42, label %wildcard_in_rule_head_0]
conflicting_definitions_0:
  %0 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 7
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
cyclic_negation_0:
  %14 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 11
  %15 = call ccc i64 @eclair_btree_size_6(ptr %14)
  %16 = trunc i64 %15 to i32
  %17 = mul i32 %16, 4
  %18 = call ccc ptr @malloc(i32 %17)
  store i32 0, ptr %stack.ptr_3
  call ccc void @eclair_btree_begin_6(ptr %14, ptr %stack.ptr_4)
  call ccc void @eclair_btree_end_6(ptr %14, ptr %stack.ptr_5)
  br label %while_begin_1
while_begin_1:
  %19 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_4, ptr %stack.ptr_5)
  %20 = select i1 %19, i1 0, i1 1
  br i1 %20, label %while_body_1, label %while_end_1
while_body_1:
  %21 = load i32, ptr %stack.ptr_3
  %22 = getelementptr [1 x i32], ptr %18, i32 %21
  %23 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_4)
  %24 = getelementptr [1 x i32], ptr %23, i32 0
  %25 = load [1 x i32], ptr %24
  %26 = getelementptr [1 x i32], ptr %22, i32 0
  store [1 x i32] %25, ptr %26
  %27 = add i32 %21, 1
  store i32 %27, ptr %stack.ptr_3
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_4)
  br label %while_begin_1
while_end_1:
  ret ptr %18
dead_code_0:
  %28 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 12
  %29 = call ccc i64 @eclair_btree_size_6(ptr %28)
  %30 = trunc i64 %29 to i32
  %31 = mul i32 %30, 4
  %32 = call ccc ptr @malloc(i32 %31)
  store i32 0, ptr %stack.ptr_6
  call ccc void @eclair_btree_begin_6(ptr %28, ptr %stack.ptr_7)
  call ccc void @eclair_btree_end_6(ptr %28, ptr %stack.ptr_8)
  br label %while_begin_2
while_begin_2:
  %33 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_7, ptr %stack.ptr_8)
  %34 = select i1 %33, i1 0, i1 1
  br i1 %34, label %while_body_2, label %while_end_2
while_body_2:
  %35 = load i32, ptr %stack.ptr_6
  %36 = getelementptr [1 x i32], ptr %32, i32 %35
  %37 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_7)
  %38 = getelementptr [1 x i32], ptr %37, i32 0
  %39 = load [1 x i32], ptr %38
  %40 = getelementptr [1 x i32], ptr %36, i32 0
  store [1 x i32] %39, ptr %40
  %41 = add i32 %35, 1
  store i32 %41, ptr %stack.ptr_6
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_7)
  br label %while_begin_2
while_end_2:
  ret ptr %32
dead_internal_relation_0:
  %42 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 13
  %43 = call ccc i64 @eclair_btree_size_1(ptr %42)
  %44 = trunc i64 %43 to i32
  %45 = mul i32 %44, 8
  %46 = call ccc ptr @malloc(i32 %45)
  store i32 0, ptr %stack.ptr_9
  call ccc void @eclair_btree_begin_1(ptr %42, ptr %stack.ptr_10)
  call ccc void @eclair_btree_end_1(ptr %42, ptr %stack.ptr_11)
  br label %while_begin_3
while_begin_3:
  %47 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_10, ptr %stack.ptr_11)
  %48 = select i1 %47, i1 0, i1 1
  br i1 %48, label %while_body_3, label %while_end_3
while_body_3:
  %49 = load i32, ptr %stack.ptr_9
  %50 = getelementptr [2 x i32], ptr %46, i32 %49
  %51 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_10)
  %52 = getelementptr [2 x i32], ptr %51, i32 0
  %53 = load [2 x i32], ptr %52
  %54 = getelementptr [2 x i32], ptr %50, i32 0
  store [2 x i32] %53, ptr %54
  %55 = add i32 %49, 1
  store i32 %55, ptr %stack.ptr_9
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_10)
  br label %while_begin_3
while_end_3:
  ret ptr %46
extern_used_as_fact_0:
  %56 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 25
  %57 = call ccc i64 @eclair_btree_size_0(ptr %56)
  %58 = trunc i64 %57 to i32
  %59 = mul i32 %58, 12
  %60 = call ccc ptr @malloc(i32 %59)
  store i32 0, ptr %stack.ptr_12
  call ccc void @eclair_btree_begin_0(ptr %56, ptr %stack.ptr_13)
  call ccc void @eclair_btree_end_0(ptr %56, ptr %stack.ptr_14)
  br label %while_begin_4
while_begin_4:
  %61 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_13, ptr %stack.ptr_14)
  %62 = select i1 %61, i1 0, i1 1
  br i1 %62, label %while_body_4, label %while_end_4
while_body_4:
  %63 = load i32, ptr %stack.ptr_12
  %64 = getelementptr [3 x i32], ptr %60, i32 %63
  %65 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_13)
  %66 = getelementptr [3 x i32], ptr %65, i32 0
  %67 = load [3 x i32], ptr %66
  %68 = getelementptr [3 x i32], ptr %64, i32 0
  store [3 x i32] %67, ptr %68
  %69 = add i32 %63, 1
  store i32 %69, ptr %stack.ptr_12
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_13)
  br label %while_begin_4
while_end_4:
  ret ptr %60
extern_used_as_rule_0:
  %70 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 26
  %71 = call ccc i64 @eclair_btree_size_0(ptr %70)
  %72 = trunc i64 %71 to i32
  %73 = mul i32 %72, 12
  %74 = call ccc ptr @malloc(i32 %73)
  store i32 0, ptr %stack.ptr_15
  call ccc void @eclair_btree_begin_0(ptr %70, ptr %stack.ptr_16)
  call ccc void @eclair_btree_end_0(ptr %70, ptr %stack.ptr_17)
  br label %while_begin_5
while_begin_5:
  %75 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_16, ptr %stack.ptr_17)
  %76 = select i1 %75, i1 0, i1 1
  br i1 %76, label %while_body_5, label %while_end_5
while_body_5:
  %77 = load i32, ptr %stack.ptr_15
  %78 = getelementptr [3 x i32], ptr %74, i32 %77
  %79 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_16)
  %80 = getelementptr [3 x i32], ptr %79, i32 0
  %81 = load [3 x i32], ptr %80
  %82 = getelementptr [3 x i32], ptr %78, i32 0
  store [3 x i32] %81, ptr %82
  %83 = add i32 %77, 1
  store i32 %83, ptr %stack.ptr_15
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_16)
  br label %while_begin_5
while_end_5:
  ret ptr %74
grounded_variable_0:
  %84 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 28
  %85 = call ccc i64 @eclair_btree_size_1(ptr %84)
  %86 = trunc i64 %85 to i32
  %87 = mul i32 %86, 8
  %88 = call ccc ptr @malloc(i32 %87)
  store i32 0, ptr %stack.ptr_18
  call ccc void @eclair_btree_begin_1(ptr %84, ptr %stack.ptr_19)
  call ccc void @eclair_btree_end_1(ptr %84, ptr %stack.ptr_20)
  br label %while_begin_6
while_begin_6:
  %89 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_19, ptr %stack.ptr_20)
  %90 = select i1 %89, i1 0, i1 1
  br i1 %90, label %while_body_6, label %while_end_6
while_body_6:
  %91 = load i32, ptr %stack.ptr_18
  %92 = getelementptr [2 x i32], ptr %88, i32 %91
  %93 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_19)
  %94 = getelementptr [2 x i32], ptr %93, i32 0
  %95 = load [2 x i32], ptr %94
  %96 = getelementptr [2 x i32], ptr %92, i32 0
  store [2 x i32] %95, ptr %96
  %97 = add i32 %91, 1
  store i32 %97, ptr %stack.ptr_18
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_19)
  br label %while_begin_6
while_end_6:
  ret ptr %88
no_output_relation_0:
  %98 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 46
  %99 = call ccc i64 @eclair_btree_size_6(ptr %98)
  %100 = trunc i64 %99 to i32
  %101 = mul i32 %100, 4
  %102 = call ccc ptr @malloc(i32 %101)
  store i32 0, ptr %stack.ptr_21
  call ccc void @eclair_btree_begin_6(ptr %98, ptr %stack.ptr_22)
  call ccc void @eclair_btree_end_6(ptr %98, ptr %stack.ptr_23)
  br label %while_begin_7
while_begin_7:
  %103 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_22, ptr %stack.ptr_23)
  %104 = select i1 %103, i1 0, i1 1
  br i1 %104, label %while_body_7, label %while_end_7
while_body_7:
  %105 = load i32, ptr %stack.ptr_21
  %106 = getelementptr [1 x i32], ptr %102, i32 %105
  %107 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_22)
  %108 = getelementptr [1 x i32], ptr %107, i32 0
  %109 = load [1 x i32], ptr %108
  %110 = getelementptr [1 x i32], ptr %106, i32 0
  store [1 x i32] %109, ptr %110
  %111 = add i32 %105, 1
  store i32 %111, ptr %stack.ptr_21
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_22)
  br label %while_begin_7
while_end_7:
  ret ptr %102
rule_with_contradiction_0:
  %112 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 58
  %113 = call ccc i64 @eclair_btree_size_6(ptr %112)
  %114 = trunc i64 %113 to i32
  %115 = mul i32 %114, 4
  %116 = call ccc ptr @malloc(i32 %115)
  store i32 0, ptr %stack.ptr_24
  call ccc void @eclair_btree_begin_6(ptr %112, ptr %stack.ptr_25)
  call ccc void @eclair_btree_end_6(ptr %112, ptr %stack.ptr_26)
  br label %while_begin_8
while_begin_8:
  %117 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_25, ptr %stack.ptr_26)
  %118 = select i1 %117, i1 0, i1 1
  br i1 %118, label %while_body_8, label %while_end_8
while_body_8:
  %119 = load i32, ptr %stack.ptr_24
  %120 = getelementptr [1 x i32], ptr %116, i32 %119
  %121 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_25)
  %122 = getelementptr [1 x i32], ptr %121, i32 0
  %123 = load [1 x i32], ptr %122
  %124 = getelementptr [1 x i32], ptr %120, i32 0
  store [1 x i32] %123, ptr %124
  %125 = add i32 %119, 1
  store i32 %125, ptr %stack.ptr_24
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_25)
  br label %while_begin_8
while_end_8:
  ret ptr %116
unconstrained_rule_var_0:
  %126 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 63
  %127 = call ccc i64 @eclair_btree_size_0(ptr %126)
  %128 = trunc i64 %127 to i32
  %129 = mul i32 %128, 12
  %130 = call ccc ptr @malloc(i32 %129)
  store i32 0, ptr %stack.ptr_27
  call ccc void @eclair_btree_begin_0(ptr %126, ptr %stack.ptr_28)
  call ccc void @eclair_btree_end_0(ptr %126, ptr %stack.ptr_29)
  br label %while_begin_9
while_begin_9:
  %131 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_28, ptr %stack.ptr_29)
  %132 = select i1 %131, i1 0, i1 1
  br i1 %132, label %while_body_9, label %while_end_9
while_body_9:
  %133 = load i32, ptr %stack.ptr_27
  %134 = getelementptr [3 x i32], ptr %130, i32 %133
  %135 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_28)
  %136 = getelementptr [3 x i32], ptr %135, i32 0
  %137 = load [3 x i32], ptr %136
  %138 = getelementptr [3 x i32], ptr %134, i32 0
  store [3 x i32] %137, ptr %138
  %139 = add i32 %133, 1
  store i32 %139, ptr %stack.ptr_27
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_28)
  br label %while_begin_9
while_end_9:
  ret ptr %130
ungrounded_external_atom_0:
  %140 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 64
  %141 = call ccc i64 @eclair_btree_size_0(ptr %140)
  %142 = trunc i64 %141 to i32
  %143 = mul i32 %142, 12
  %144 = call ccc ptr @malloc(i32 %143)
  store i32 0, ptr %stack.ptr_30
  call ccc void @eclair_btree_begin_0(ptr %140, ptr %stack.ptr_31)
  call ccc void @eclair_btree_end_0(ptr %140, ptr %stack.ptr_32)
  br label %while_begin_10
while_begin_10:
  %145 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_31, ptr %stack.ptr_32)
  %146 = select i1 %145, i1 0, i1 1
  br i1 %146, label %while_body_10, label %while_end_10
while_body_10:
  %147 = load i32, ptr %stack.ptr_30
  %148 = getelementptr [3 x i32], ptr %144, i32 %147
  %149 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_31)
  %150 = getelementptr [3 x i32], ptr %149, i32 0
  %151 = load [3 x i32], ptr %150
  %152 = getelementptr [3 x i32], ptr %148, i32 0
  store [3 x i32] %151, ptr %152
  %153 = add i32 %147, 1
  store i32 %153, ptr %stack.ptr_30
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_31)
  br label %while_begin_10
while_end_10:
  ret ptr %144
ungrounded_variable_0:
  %154 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 65
  %155 = call ccc i64 @eclair_btree_size_0(ptr %154)
  %156 = trunc i64 %155 to i32
  %157 = mul i32 %156, 12
  %158 = call ccc ptr @malloc(i32 %157)
  store i32 0, ptr %stack.ptr_33
  call ccc void @eclair_btree_begin_0(ptr %154, ptr %stack.ptr_34)
  call ccc void @eclair_btree_end_0(ptr %154, ptr %stack.ptr_35)
  br label %while_begin_11
while_begin_11:
  %159 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_34, ptr %stack.ptr_35)
  %160 = select i1 %159, i1 0, i1 1
  br i1 %160, label %while_body_11, label %while_end_11
while_body_11:
  %161 = load i32, ptr %stack.ptr_33
  %162 = getelementptr [3 x i32], ptr %158, i32 %161
  %163 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_34)
  %164 = getelementptr [3 x i32], ptr %163, i32 0
  %165 = load [3 x i32], ptr %164
  %166 = getelementptr [3 x i32], ptr %162, i32 0
  store [3 x i32] %165, ptr %166
  %167 = add i32 %161, 1
  store i32 %167, ptr %stack.ptr_33
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_34)
  br label %while_begin_11
while_end_11:
  ret ptr %158
wildcard_in_binop_0:
  %168 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 69
  %169 = call ccc i64 @eclair_btree_size_1(ptr %168)
  %170 = trunc i64 %169 to i32
  %171 = mul i32 %170, 8
  %172 = call ccc ptr @malloc(i32 %171)
  store i32 0, ptr %stack.ptr_36
  call ccc void @eclair_btree_begin_1(ptr %168, ptr %stack.ptr_37)
  call ccc void @eclair_btree_end_1(ptr %168, ptr %stack.ptr_38)
  br label %while_begin_12
while_begin_12:
  %173 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_37, ptr %stack.ptr_38)
  %174 = select i1 %173, i1 0, i1 1
  br i1 %174, label %while_body_12, label %while_end_12
while_body_12:
  %175 = load i32, ptr %stack.ptr_36
  %176 = getelementptr [2 x i32], ptr %172, i32 %175
  %177 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_37)
  %178 = getelementptr [2 x i32], ptr %177, i32 0
  %179 = load [2 x i32], ptr %178
  %180 = getelementptr [2 x i32], ptr %176, i32 0
  store [2 x i32] %179, ptr %180
  %181 = add i32 %175, 1
  store i32 %181, ptr %stack.ptr_36
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_37)
  br label %while_begin_12
while_end_12:
  ret ptr %172
wildcard_in_constraint_0:
  %182 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 70
  %183 = call ccc i64 @eclair_btree_size_1(ptr %182)
  %184 = trunc i64 %183 to i32
  %185 = mul i32 %184, 8
  %186 = call ccc ptr @malloc(i32 %185)
  store i32 0, ptr %stack.ptr_39
  call ccc void @eclair_btree_begin_1(ptr %182, ptr %stack.ptr_40)
  call ccc void @eclair_btree_end_1(ptr %182, ptr %stack.ptr_41)
  br label %while_begin_13
while_begin_13:
  %187 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_40, ptr %stack.ptr_41)
  %188 = select i1 %187, i1 0, i1 1
  br i1 %188, label %while_body_13, label %while_end_13
while_body_13:
  %189 = load i32, ptr %stack.ptr_39
  %190 = getelementptr [2 x i32], ptr %186, i32 %189
  %191 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_40)
  %192 = getelementptr [2 x i32], ptr %191, i32 0
  %193 = load [2 x i32], ptr %192
  %194 = getelementptr [2 x i32], ptr %190, i32 0
  store [2 x i32] %193, ptr %194
  %195 = add i32 %189, 1
  store i32 %195, ptr %stack.ptr_39
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_40)
  br label %while_begin_13
while_end_13:
  ret ptr %186
wildcard_in_extern_0:
  %196 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 71
  %197 = call ccc i64 @eclair_btree_size_0(ptr %196)
  %198 = trunc i64 %197 to i32
  %199 = mul i32 %198, 12
  %200 = call ccc ptr @malloc(i32 %199)
  store i32 0, ptr %stack.ptr_42
  call ccc void @eclair_btree_begin_0(ptr %196, ptr %stack.ptr_43)
  call ccc void @eclair_btree_end_0(ptr %196, ptr %stack.ptr_44)
  br label %while_begin_14
while_begin_14:
  %201 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_43, ptr %stack.ptr_44)
  %202 = select i1 %201, i1 0, i1 1
  br i1 %202, label %while_body_14, label %while_end_14
while_body_14:
  %203 = load i32, ptr %stack.ptr_42
  %204 = getelementptr [3 x i32], ptr %200, i32 %203
  %205 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_43)
  %206 = getelementptr [3 x i32], ptr %205, i32 0
  %207 = load [3 x i32], ptr %206
  %208 = getelementptr [3 x i32], ptr %204, i32 0
  store [3 x i32] %207, ptr %208
  %209 = add i32 %203, 1
  store i32 %209, ptr %stack.ptr_42
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_43)
  br label %while_begin_14
while_end_14:
  ret ptr %200
wildcard_in_fact_0:
  %210 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 72
  %211 = call ccc i64 @eclair_btree_size_0(ptr %210)
  %212 = trunc i64 %211 to i32
  %213 = mul i32 %212, 12
  %214 = call ccc ptr @malloc(i32 %213)
  store i32 0, ptr %stack.ptr_45
  call ccc void @eclair_btree_begin_0(ptr %210, ptr %stack.ptr_46)
  call ccc void @eclair_btree_end_0(ptr %210, ptr %stack.ptr_47)
  br label %while_begin_15
while_begin_15:
  %215 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_46, ptr %stack.ptr_47)
  %216 = select i1 %215, i1 0, i1 1
  br i1 %216, label %while_body_15, label %while_end_15
while_body_15:
  %217 = load i32, ptr %stack.ptr_45
  %218 = getelementptr [3 x i32], ptr %214, i32 %217
  %219 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_46)
  %220 = getelementptr [3 x i32], ptr %219, i32 0
  %221 = load [3 x i32], ptr %220
  %222 = getelementptr [3 x i32], ptr %218, i32 0
  store [3 x i32] %221, ptr %222
  %223 = add i32 %217, 1
  store i32 %223, ptr %stack.ptr_45
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_46)
  br label %while_begin_15
while_end_15:
  ret ptr %214
wildcard_in_rule_head_0:
  %224 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 73
  %225 = call ccc i64 @eclair_btree_size_0(ptr %224)
  %226 = trunc i64 %225 to i32
  %227 = mul i32 %226, 12
  %228 = call ccc ptr @malloc(i32 %227)
  store i32 0, ptr %stack.ptr_48
  call ccc void @eclair_btree_begin_0(ptr %224, ptr %stack.ptr_49)
  call ccc void @eclair_btree_end_0(ptr %224, ptr %stack.ptr_50)
  br label %while_begin_16
while_begin_16:
  %229 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_49, ptr %stack.ptr_50)
  %230 = select i1 %229, i1 0, i1 1
  br i1 %230, label %while_body_16, label %while_end_16
while_body_16:
  %231 = load i32, ptr %stack.ptr_48
  %232 = getelementptr [3 x i32], ptr %228, i32 %231
  %233 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_49)
  %234 = getelementptr [3 x i32], ptr %233, i32 0
  %235 = load [3 x i32], ptr %234
  %236 = getelementptr [3 x i32], ptr %232, i32 0
  store [3 x i32] %235, ptr %236
  %237 = add i32 %231, 1
  store i32 %237, ptr %stack.ptr_48
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_49)
  br label %while_begin_16
while_end_16:
  ret ptr %228
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
  switch i32 %fact_type_0, label %switch.default_0 [i32 50, label %conflicting_definitions_0 i32 53, label %cyclic_negation_0 i32 47, label %dead_code_0 i32 49, label %dead_internal_relation_0 i32 51, label %extern_used_as_fact_0 i32 52, label %extern_used_as_rule_0 i32 37, label %grounded_variable_0 i32 48, label %no_output_relation_0 i32 46, label %rule_with_contradiction_0 i32 45, label %unconstrained_rule_var_0 i32 39, label %ungrounded_external_atom_0 i32 38, label %ungrounded_variable_0 i32 44, label %wildcard_in_binop_0 i32 43, label %wildcard_in_constraint_0 i32 41, label %wildcard_in_extern_0 i32 40, label %wildcard_in_fact_0 i32 42, label %wildcard_in_rule_head_0]
conflicting_definitions_0:
  %0 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 7
  %1 = call ccc i64 @eclair_btree_size_0(ptr %0)
  %2 = trunc i64 %1 to i32
  ret i32 %2
cyclic_negation_0:
  %3 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 11
  %4 = call ccc i64 @eclair_btree_size_6(ptr %3)
  %5 = trunc i64 %4 to i32
  ret i32 %5
dead_code_0:
  %6 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 12
  %7 = call ccc i64 @eclair_btree_size_6(ptr %6)
  %8 = trunc i64 %7 to i32
  ret i32 %8
dead_internal_relation_0:
  %9 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 13
  %10 = call ccc i64 @eclair_btree_size_1(ptr %9)
  %11 = trunc i64 %10 to i32
  ret i32 %11
extern_used_as_fact_0:
  %12 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 25
  %13 = call ccc i64 @eclair_btree_size_0(ptr %12)
  %14 = trunc i64 %13 to i32
  ret i32 %14
extern_used_as_rule_0:
  %15 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 26
  %16 = call ccc i64 @eclair_btree_size_0(ptr %15)
  %17 = trunc i64 %16 to i32
  ret i32 %17
grounded_variable_0:
  %18 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 28
  %19 = call ccc i64 @eclair_btree_size_1(ptr %18)
  %20 = trunc i64 %19 to i32
  ret i32 %20
no_output_relation_0:
  %21 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 46
  %22 = call ccc i64 @eclair_btree_size_6(ptr %21)
  %23 = trunc i64 %22 to i32
  ret i32 %23
rule_with_contradiction_0:
  %24 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 58
  %25 = call ccc i64 @eclair_btree_size_6(ptr %24)
  %26 = trunc i64 %25 to i32
  ret i32 %26
unconstrained_rule_var_0:
  %27 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 63
  %28 = call ccc i64 @eclair_btree_size_0(ptr %27)
  %29 = trunc i64 %28 to i32
  ret i32 %29
ungrounded_external_atom_0:
  %30 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 64
  %31 = call ccc i64 @eclair_btree_size_0(ptr %30)
  %32 = trunc i64 %31 to i32
  ret i32 %32
ungrounded_variable_0:
  %33 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 65
  %34 = call ccc i64 @eclair_btree_size_0(ptr %33)
  %35 = trunc i64 %34 to i32
  ret i32 %35
wildcard_in_binop_0:
  %36 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 69
  %37 = call ccc i64 @eclair_btree_size_1(ptr %36)
  %38 = trunc i64 %37 to i32
  ret i32 %38
wildcard_in_constraint_0:
  %39 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 70
  %40 = call ccc i64 @eclair_btree_size_1(ptr %39)
  %41 = trunc i64 %40 to i32
  ret i32 %41
wildcard_in_extern_0:
  %42 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 71
  %43 = call ccc i64 @eclair_btree_size_0(ptr %42)
  %44 = trunc i64 %43 to i32
  ret i32 %44
wildcard_in_fact_0:
  %45 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 72
  %46 = call ccc i64 @eclair_btree_size_0(ptr %45)
  %47 = trunc i64 %46 to i32
  ret i32 %47
wildcard_in_rule_head_0:
  %48 = getelementptr %program, ptr %eclair_program_0, i32 0, i32 73
  %49 = call ccc i64 @eclair_btree_size_0(ptr %48)
  %50 = trunc i64 %49 to i32
  ret i32 %50
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
