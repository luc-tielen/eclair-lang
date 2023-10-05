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
  %23 = icmp sge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
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
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_2, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_2, ptr %65, i32 0, i32 0, i32 1
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
  %74 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_2, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_2, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_2, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
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
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_2, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_2, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_2, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_2(ptr %val_0, ptr %30, ptr %31)
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
  %40 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 30
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_2, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_2(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_2, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_2, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_2, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_2, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [2 x i32], ptr %66
  %68 = getelementptr %node_t_2, ptr %59, i32 0, i32 1, i16 %63
  store [2 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [2 x i32], ptr %val_0
  %71 = getelementptr %node_t_2, ptr %59, i32 0, i32 1, i16 %60
  store [2 x i32] %70, ptr %71
  %72 = getelementptr %node_t_2, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
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
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_2(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_2, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_2(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_2, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
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
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_2(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_2(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_2(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_2, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
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
  %23 = icmp sge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
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
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_3, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_3, ptr %65, i32 0, i32 0, i32 1
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
  %74 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_3, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_3, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_3, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
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
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_3, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_3, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_3, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_3(ptr %val_0, ptr %30, ptr %31)
  %33 = ptrtoint ptr %32 to i64
  %34 = ptrtoint ptr %30 to i64
  %35 = sub i64 %33, %34
  %36 = trunc i64 %35 to i16
  %37 = udiv i16 %36, 16
  store i16 %37, ptr %stack.ptr_1
  %38 = icmp ne ptr %32, %30
  br i1 %38, label %if_1, label %end_if_1
if_1:
  %39 = getelementptr [4 x i32], ptr %32, i32 -1
  %40 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 15
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_3, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_3(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_3, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_3, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_3, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_3, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [4 x i32], ptr %66
  %68 = getelementptr %node_t_3, ptr %59, i32 0, i32 1, i16 %63
  store [4 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [4 x i32], ptr %val_0
  %71 = getelementptr %node_t_3, ptr %59, i32 0, i32 1, i16 %60
  store [4 x i32] %70, ptr %71
  %72 = getelementptr %node_t_3, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
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
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_3(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_3, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_3(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_3, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
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
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_3(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_3(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_3(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_3, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
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
  %23 = icmp sge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
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
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_4, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_4, ptr %65, i32 0, i32 0, i32 1
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
  %74 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_4, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_4, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_4, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
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
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_4, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_4, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_4, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_4(ptr %val_0, ptr %30, ptr %31)
  %33 = ptrtoint ptr %32 to i64
  %34 = ptrtoint ptr %30 to i64
  %35 = sub i64 %33, %34
  %36 = trunc i64 %35 to i16
  %37 = udiv i16 %36, 16
  store i16 %37, ptr %stack.ptr_1
  %38 = icmp ne ptr %32, %30
  br i1 %38, label %if_1, label %end_if_1
if_1:
  %39 = getelementptr [4 x i32], ptr %32, i32 -1
  %40 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 15
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_4, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_4(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_4, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_4, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_4, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_4, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [4 x i32], ptr %66
  %68 = getelementptr %node_t_4, ptr %59, i32 0, i32 1, i16 %63
  store [4 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [4 x i32], ptr %val_0
  %71 = getelementptr %node_t_4, ptr %59, i32 0, i32 1, i16 %60
  store [4 x i32] %70, ptr %71
  %72 = getelementptr %node_t_4, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
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
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_4(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_4, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_4(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_4, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
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
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_4(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_4(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_4(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_4, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
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
  %23 = icmp sge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
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
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_5, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_5, ptr %65, i32 0, i32 0, i32 1
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
  %74 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_5, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_5, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_5, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
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
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_5, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_5, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_5, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_5(ptr %val_0, ptr %30, ptr %31)
  %33 = ptrtoint ptr %32 to i64
  %34 = ptrtoint ptr %30 to i64
  %35 = sub i64 %33, %34
  %36 = trunc i64 %35 to i16
  %37 = udiv i16 %36, 16
  store i16 %37, ptr %stack.ptr_1
  %38 = icmp ne ptr %32, %30
  br i1 %38, label %if_1, label %end_if_1
if_1:
  %39 = getelementptr [4 x i32], ptr %32, i32 -1
  %40 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 15
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_5, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_5(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_5, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_5, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_5, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_5, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [4 x i32], ptr %66
  %68 = getelementptr %node_t_5, ptr %59, i32 0, i32 1, i16 %63
  store [4 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [4 x i32], ptr %val_0
  %71 = getelementptr %node_t_5, ptr %59, i32 0, i32 1, i16 %60
  store [4 x i32] %70, ptr %71
  %72 = getelementptr %node_t_5, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
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
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_5(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_5, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_5(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_5, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
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
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_5(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_5(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_5(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_5, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
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
  %23 = icmp sge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
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
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_6, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_6, ptr %65, i32 0, i32 0, i32 1
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
  %74 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_6, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_6, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_6, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
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
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_6, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_6, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_6, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_6(ptr %val_0, ptr %30, ptr %31)
  %33 = ptrtoint ptr %32 to i64
  %34 = ptrtoint ptr %30 to i64
  %35 = sub i64 %33, %34
  %36 = trunc i64 %35 to i16
  %37 = udiv i16 %36, 4
  store i16 %37, ptr %stack.ptr_1
  %38 = icmp ne ptr %32, %30
  br i1 %38, label %if_1, label %end_if_1
if_1:
  %39 = getelementptr [1 x i32], ptr %32, i32 -1
  %40 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 60
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_6, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_6(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_6, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_6, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_6, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_6, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [1 x i32], ptr %66
  %68 = getelementptr %node_t_6, ptr %59, i32 0, i32 1, i16 %63
  store [1 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [1 x i32], ptr %val_0
  %71 = getelementptr %node_t_6, ptr %59, i32 0, i32 1, i16 %60
  store [1 x i32] %70, ptr %71
  %72 = getelementptr %node_t_6, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
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
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_6(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_6, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_6(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_6, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
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
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_6(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_6(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_6(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_6, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
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
  %23 = icmp sge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
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
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_7, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_7, ptr %65, i32 0, i32 0, i32 1
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
  %74 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_7, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_7, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_7, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
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
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_7, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_7, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_7, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_7(ptr %val_0, ptr %30, ptr %31)
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
  %40 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 30
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_7, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_7(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_7, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_7, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_7, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_7, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [2 x i32], ptr %66
  %68 = getelementptr %node_t_7, ptr %59, i32 0, i32 1, i16 %63
  store [2 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [2 x i32], ptr %val_0
  %71 = getelementptr %node_t_7, ptr %59, i32 0, i32 1, i16 %60
  store [2 x i32] %70, ptr %71
  %72 = getelementptr %node_t_7, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
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
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_7(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_7, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_7(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_7, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
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
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_7(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_7(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_7(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_7, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
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
  %23 = icmp sge i16 %22, %21
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
  %48 = phi i16 [0, %if_1], [%57, %for_body_2]
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
  %57 = add i16 1, %48
  br label %for_begin_2
for_end_2:
  br label %for_begin_3
for_begin_3:
  %58 = phi i16 [0, %for_end_2], [%68, %for_body_3]
  %59 = icmp ult i16 %58, %14
  br i1 %59, label %for_body_3, label %for_end_3
for_body_3:
  %60 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %61 = load i16, ptr %60
  %62 = add i16 %61, 1
  %63 = add i16 %58, %62
  %64 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %58
  %65 = load ptr, ptr %64
  %66 = getelementptr %node_t_8, ptr %65, i32 0, i32 0, i32 0
  store ptr %9, ptr %66
  %67 = getelementptr %node_t_8, ptr %65, i32 0, i32 0, i32 1
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
  %74 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %73
  %75 = load ptr, ptr %74
  %76 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %71
  store ptr %75, ptr %76
  %77 = getelementptr %inner_node_t_8, ptr %node_0, i32 0, i32 1, i16 %71
  %78 = load ptr, ptr %77
  %79 = getelementptr %node_t_8, ptr %78, i32 0, i32 0, i32 1
  store i16 %71, ptr %79
  %80 = add i16 1, %71
  br label %for_begin_4
for_end_4:
  br label %end_if_0
end_if_0:
  %81 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %82 = load i16, ptr %81
  %83 = add i16 %82, %14
  store i16 %83, ptr %81
  %84 = getelementptr %node_t_8, ptr %node_0, i32 0, i32 0, i32 2
  %85 = load i16, ptr %84
  %86 = sub i16 %85, %14
  store i16 %86, ptr %84
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
  br i1 %23, label %if_0, label %end_if_0
if_0:
  %24 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %17, ptr %val_0)
  %25 = icmp eq i8 0, %24
  br i1 %25, label %no_insert_0, label %inner_continue_insert_0
end_if_0:
  br label %inner_continue_insert_0
inner_continue_insert_0:
  %26 = getelementptr %inner_node_t_8, ptr %9, i32 0, i32 1, i16 %22
  %27 = load ptr, ptr %26
  store ptr %27, ptr %stack.ptr_0
  br label %loop_0
leaf_0:
  %28 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %29 = load i16, ptr %28
  %30 = getelementptr %node_t_8, ptr %9, i32 0, i32 1, i16 0
  %31 = getelementptr %node_t_8, ptr %9, i32 0, i32 1, i16 %29
  %32 = call ccc ptr @eclair_btree_linear_search_upper_bound_8(ptr %val_0, ptr %30, ptr %31)
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
  %40 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %39, ptr %val_0)
  %41 = icmp eq i8 0, %40
  br i1 %41, label %no_insert_0, label %leaf_continue_insert_0
end_if_1:
  br label %leaf_continue_insert_0
leaf_continue_insert_0:
  %42 = icmp uge i16 %29, 20
  br i1 %42, label %split_0, label %no_split_0
split_0:
  %43 = getelementptr %btree_t_8, ptr %tree_0, i32 0, i32 0
  %44 = load i16, ptr %stack.ptr_1
  %45 = call ccc i16 @eclair_btree_node_rebalance_or_split_8(ptr %9, ptr %43, i16 %44)
  %46 = sub i16 %44, %45
  store i16 %46, ptr %stack.ptr_1
  %47 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 2
  %48 = load i16, ptr %47
  %49 = icmp ugt i16 %46, %48
  br i1 %49, label %if_2, label %end_if_2
if_2:
  %50 = add i16 %48, 1
  %51 = sub i16 %46, %50
  store i16 %51, ptr %stack.ptr_1
  %52 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 0
  %53 = load ptr, ptr %52
  %54 = getelementptr %node_t_8, ptr %9, i32 0, i32 0, i32 1
  %55 = load i16, ptr %54
  %56 = add i16 1, %55
  %57 = getelementptr %inner_node_t_8, ptr %53, i32 0, i32 1, i16 %56
  %58 = load ptr, ptr %57
  store ptr %58, ptr %stack.ptr_0
  br label %end_if_2
end_if_2:
  br label %no_split_0
no_split_0:
  %59 = load ptr, ptr %stack.ptr_0
  %60 = load i16, ptr %stack.ptr_1
  %61 = getelementptr %node_t_8, ptr %59, i32 0, i32 0, i32 2
  %62 = load i16, ptr %61
  br label %for_begin_0
for_begin_0:
  %63 = phi i16 [%62, %no_split_0], [%69, %for_body_0]
  %64 = icmp ugt i16 %63, %60
  br i1 %64, label %for_body_0, label %for_end_0
for_body_0:
  %65 = sub i16 %63, 1
  %66 = getelementptr %node_t_8, ptr %59, i32 0, i32 1, i16 %65
  %67 = load [3 x i32], ptr %66
  %68 = getelementptr %node_t_8, ptr %59, i32 0, i32 1, i16 %63
  store [3 x i32] %67, ptr %68
  %69 = sub i16 %63, 1
  br label %for_begin_0
for_end_0:
  %70 = load [3 x i32], ptr %val_0
  %71 = getelementptr %node_t_8, ptr %59, i32 0, i32 1, i16 %60
  store [3 x i32] %70, ptr %71
  %72 = getelementptr %node_t_8, ptr %59, i32 0, i32 0, i32 2
  %73 = load i16, ptr %72
  %74 = add i16 1, %73
  store i16 %74, ptr %72
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
  br i1 %14, label %if_1, label %end_if_2
if_1:
  %15 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %8, ptr %val_0)
  %16 = icmp eq i8 0, %15
  br i1 %16, label %if_2, label %end_if_1
if_2:
  call ccc void @eclair_btree_iterator_init_8(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_1:
  br label %end_if_2
end_if_2:
  %17 = getelementptr %node_t_8, ptr %3, i32 0, i32 0, i32 3
  %18 = load i1, ptr %17
  %19 = icmp eq i1 %18, 0
  br i1 %19, label %if_3, label %end_if_3
if_3:
  call ccc void @eclair_btree_iterator_end_init_8(ptr %result_0)
  ret void
end_if_3:
  %20 = getelementptr %inner_node_t_8, ptr %3, i32 0, i32 1, i16 %13
  %21 = load ptr, ptr %20
  store ptr %21, ptr %stack.ptr_0
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
  br i1 %24, label %if_2, label %end_if_3
if_2:
  %25 = call ccc i8 @eclair_btree_value_compare_values_8(ptr %8, ptr %val_0)
  %26 = icmp eq i8 0, %25
  br i1 %26, label %if_3, label %end_if_2
if_3:
  call ccc void @eclair_btree_iterator_init_8(ptr %result_0, ptr %3, i16 %13)
  ret void
end_if_2:
  br label %end_if_3
end_if_3:
  br i1 %24, label %if_4, label %end_if_4
if_4:
  call ccc void @eclair_btree_iterator_init_8(ptr %stack.ptr_0, ptr %3, i16 %13)
  br label %end_if_4
end_if_4:
  %27 = getelementptr %inner_node_t_8, ptr %3, i32 0, i32 1, i16 %13
  %28 = load ptr, ptr %27
  store ptr %28, ptr %stack.ptr_1
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
  %stack.ptr_158 = alloca [2 x i32], i32 1
  %stack.ptr_159 = alloca [2 x i32], i32 1
  %stack.ptr_160 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_161 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_162 = alloca [2 x i32], i32 1
  %stack.ptr_163 = alloca [2 x i32], i32 1
  %stack.ptr_164 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_165 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_166 = alloca [3 x i32], i32 1
  %stack.ptr_167 = alloca [3 x i32], i32 1
  %stack.ptr_168 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_169 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_170 = alloca [3 x i32], i32 1
  %stack.ptr_171 = alloca [3 x i32], i32 1
  %stack.ptr_172 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_173 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_174 = alloca [3 x i32], i32 1
  %stack.ptr_175 = alloca [3 x i32], i32 1
  %stack.ptr_176 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_177 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_178 = alloca [3 x i32], i32 1
  %stack.ptr_179 = alloca [3 x i32], i32 1
  %stack.ptr_180 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_181 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_182 = alloca [2 x i32], i32 1
  %stack.ptr_183 = alloca [2 x i32], i32 1
  %stack.ptr_184 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_185 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_186 = alloca [1 x i32], i32 1
  %stack.ptr_187 = alloca [3 x i32], i32 1
  %stack.ptr_188 = alloca [3 x i32], i32 1
  %stack.ptr_189 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_190 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_191 = alloca [3 x i32], i32 1
  %stack.ptr_192 = alloca [3 x i32], i32 1
  %stack.ptr_193 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_194 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_195 = alloca [2 x i32], i32 1
  %stack.ptr_196 = alloca [2 x i32], i32 1
  %stack.ptr_197 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_198 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_199 = alloca [1 x i32], i32 1
  %stack.ptr_200 = alloca [2 x i32], i32 1
  %stack.ptr_201 = alloca [2 x i32], i32 1
  %stack.ptr_202 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_203 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_204 = alloca [1 x i32], i32 1
  %stack.ptr_205 = alloca [3 x i32], i32 1
  %stack.ptr_206 = alloca [3 x i32], i32 1
  %stack.ptr_207 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_208 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_209 = alloca [1 x i32], i32 1
  %stack.ptr_210 = alloca [1 x i32], i32 1
  %stack.ptr_211 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_212 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_213 = alloca [3 x i32], i32 1
  %stack.ptr_214 = alloca [2 x i32], i32 1
  %stack.ptr_215 = alloca [2 x i32], i32 1
  %stack.ptr_216 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_217 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_218 = alloca [3 x i32], i32 1
  %stack.ptr_219 = alloca [3 x i32], i32 1
  %stack.ptr_220 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_221 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_222 = alloca [1 x i32], i32 1
  %stack.ptr_223 = alloca [1 x i32], i32 1
  %stack.ptr_224 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_225 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_226 = alloca [3 x i32], i32 1
  %stack.ptr_227 = alloca [4 x i32], i32 1
  %stack.ptr_228 = alloca [4 x i32], i32 1
  %stack.ptr_229 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_230 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_231 = alloca [1 x i32], i32 1
  %stack.ptr_232 = alloca [1 x i32], i32 1
  %stack.ptr_233 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_234 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_235 = alloca [2 x i32], i32 1
  %stack.ptr_236 = alloca [4 x i32], i32 1
  %stack.ptr_237 = alloca [4 x i32], i32 1
  %stack.ptr_238 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_239 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_240 = alloca [1 x i32], i32 1
  %stack.ptr_241 = alloca [1 x i32], i32 1
  %stack.ptr_242 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_243 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_244 = alloca [2 x i32], i32 1
  %stack.ptr_245 = alloca [4 x i32], i32 1
  %stack.ptr_246 = alloca [4 x i32], i32 1
  %stack.ptr_247 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_248 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_249 = alloca [1 x i32], i32 1
  %stack.ptr_250 = alloca [1 x i32], i32 1
  %stack.ptr_251 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_252 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_253 = alloca [2 x i32], i32 1
  %stack.ptr_254 = alloca [4 x i32], i32 1
  %stack.ptr_255 = alloca [4 x i32], i32 1
  %stack.ptr_256 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_257 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_258 = alloca [1 x i32], i32 1
  %stack.ptr_259 = alloca [1 x i32], i32 1
  %stack.ptr_260 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_261 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_262 = alloca [2 x i32], i32 1
  %stack.ptr_263 = alloca [1 x i32], i32 1
  %stack.ptr_264 = alloca [1 x i32], i32 1
  %stack.ptr_265 = alloca [1 x i32], i32 1
  %stack.ptr_266 = alloca [1 x i32], i32 1
  %stack.ptr_267 = alloca [1 x i32], i32 1
  %stack.ptr_268 = alloca [3 x i32], i32 1
  %stack.ptr_269 = alloca [3 x i32], i32 1
  %stack.ptr_270 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_271 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_272 = alloca [2 x i32], i32 1
  %stack.ptr_273 = alloca [2 x i32], i32 1
  %stack.ptr_274 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_275 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_276 = alloca [2 x i32], i32 1
  %stack.ptr_277 = alloca [2 x i32], i32 1
  %stack.ptr_278 = alloca [2 x i32], i32 1
  %stack.ptr_279 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_280 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_281 = alloca [2 x i32], i32 1
  %stack.ptr_282 = alloca [2 x i32], i32 1
  %stack.ptr_283 = alloca [2 x i32], i32 1
  %stack.ptr_284 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_285 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_286 = alloca [2 x i32], i32 1
  %stack.ptr_287 = alloca [2 x i32], i32 1
  %stack.ptr_288 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_289 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_290 = alloca [2 x i32], i32 1
  %stack.ptr_291 = alloca [2 x i32], i32 1
  %stack.ptr_292 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_293 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_294 = alloca [2 x i32], i32 1
  %stack.ptr_295 = alloca [2 x i32], i32 1
  %stack.ptr_296 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_297 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_298 = alloca [2 x i32], i32 1
  %stack.ptr_299 = alloca [2 x i32], i32 1
  %stack.ptr_300 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_301 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_302 = alloca [2 x i32], i32 1
  %stack.ptr_303 = alloca [2 x i32], i32 1
  %stack.ptr_304 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_305 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_306 = alloca [3 x i32], i32 1
  %stack.ptr_307 = alloca [2 x i32], i32 1
  %stack.ptr_308 = alloca [2 x i32], i32 1
  %stack.ptr_309 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_310 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_311 = alloca [2 x i32], i32 1
  %stack.ptr_312 = alloca [2 x i32], i32 1
  %stack.ptr_313 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_314 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_315 = alloca [2 x i32], i32 1
  %stack.ptr_316 = alloca [2 x i32], i32 1
  %stack.ptr_317 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_318 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_319 = alloca [3 x i32], i32 1
  %stack.ptr_320 = alloca [3 x i32], i32 1
  %stack.ptr_321 = alloca [2 x i32], i32 1
  %stack.ptr_322 = alloca [2 x i32], i32 1
  %stack.ptr_323 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_324 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_325 = alloca [2 x i32], i32 1
  %stack.ptr_326 = alloca [2 x i32], i32 1
  %stack.ptr_327 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_328 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_329 = alloca [2 x i32], i32 1
  %stack.ptr_330 = alloca [2 x i32], i32 1
  %stack.ptr_331 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_332 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_333 = alloca [2 x i32], i32 1
  %stack.ptr_334 = alloca [2 x i32], i32 1
  %stack.ptr_335 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_336 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_337 = alloca [3 x i32], i32 1
  %stack.ptr_338 = alloca [2 x i32], i32 1
  %stack.ptr_339 = alloca [2 x i32], i32 1
  %stack.ptr_340 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_341 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_342 = alloca [2 x i32], i32 1
  %stack.ptr_343 = alloca [2 x i32], i32 1
  %stack.ptr_344 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_345 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_346 = alloca [1 x i32], i32 1
  %stack.ptr_347 = alloca [1 x i32], i32 1
  %stack.ptr_348 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_349 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_350 = alloca [1 x i32], i32 1
  %stack.ptr_351 = alloca [2 x i32], i32 1
  %stack.ptr_352 = alloca [2 x i32], i32 1
  %stack.ptr_353 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_354 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_355 = alloca [1 x i32], i32 1
  %stack.ptr_356 = alloca [1 x i32], i32 1
  %stack.ptr_357 = alloca [2 x i32], i32 1
  %stack.ptr_358 = alloca [2 x i32], i32 1
  %stack.ptr_359 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_360 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_361 = alloca [2 x i32], i32 1
  %stack.ptr_362 = alloca [2 x i32], i32 1
  %stack.ptr_363 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_364 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_365 = alloca [1 x i32], i32 1
  %stack.ptr_366 = alloca [2 x i32], i32 1
  %stack.ptr_367 = alloca [2 x i32], i32 1
  %stack.ptr_368 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_369 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_370 = alloca [2 x i32], i32 1
  %stack.ptr_371 = alloca [2 x i32], i32 1
  %stack.ptr_372 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_373 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_374 = alloca [1 x i32], i32 1
  %stack.ptr_375 = alloca [1 x i32], i32 1
  %stack.ptr_376 = alloca [1 x i32], i32 1
  %stack.ptr_377 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_378 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_379 = alloca [1 x i32], i32 1
  %stack.ptr_380 = alloca [2 x i32], i32 1
  %stack.ptr_381 = alloca [2 x i32], i32 1
  %stack.ptr_382 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_383 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_384 = alloca [2 x i32], i32 1
  %stack.ptr_385 = alloca [1 x i32], i32 1
  %stack.ptr_386 = alloca [1 x i32], i32 1
  %stack.ptr_387 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_388 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_389 = alloca [1 x i32], i32 1
  %stack.ptr_390 = alloca [2 x i32], i32 1
  %stack.ptr_391 = alloca [2 x i32], i32 1
  %stack.ptr_392 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_393 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_394 = alloca [2 x i32], i32 1
  %stack.ptr_395 = alloca [2 x i32], i32 1
  %stack.ptr_396 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_397 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_398 = alloca [1 x i32], i32 1
  %stack.ptr_399 = alloca [1 x i32], i32 1
  %stack.ptr_400 = alloca [1 x i32], i32 1
  %stack.ptr_401 = alloca [1 x i32], i32 1
  %stack.ptr_402 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_403 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_404 = alloca [1 x i32], i32 1
  %stack.ptr_405 = alloca [1 x i32], i32 1
  %stack.ptr_406 = alloca [1 x i32], i32 1
  %stack.ptr_407 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_408 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_409 = alloca [1 x i32], i32 1
  %stack.ptr_410 = alloca [1 x i32], i32 1
  %stack.ptr_411 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_412 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_413 = alloca [1 x i32], i32 1
  %stack.ptr_414 = alloca [2 x i32], i32 1
  %stack.ptr_415 = alloca [2 x i32], i32 1
  %stack.ptr_416 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_417 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_418 = alloca [3 x i32], i32 1
  %stack.ptr_419 = alloca [3 x i32], i32 1
  %stack.ptr_420 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_421 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_422 = alloca [2 x i32], i32 1
  %stack.ptr_423 = alloca [2 x i32], i32 1
  %stack.ptr_424 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_425 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_426 = alloca [2 x i32], i32 1
  %stack.ptr_427 = alloca [2 x i32], i32 1
  %stack.ptr_428 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_429 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_430 = alloca [2 x i32], i32 1
  %stack.ptr_431 = alloca [2 x i32], i32 1
  %stack.ptr_432 = alloca [2 x i32], i32 1
  %stack.ptr_433 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_434 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_435 = alloca [3 x i32], i32 1
  %stack.ptr_436 = alloca [3 x i32], i32 1
  %stack.ptr_437 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_438 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_439 = alloca [2 x i32], i32 1
  %stack.ptr_440 = alloca [2 x i32], i32 1
  %stack.ptr_441 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_442 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_443 = alloca [2 x i32], i32 1
  %stack.ptr_444 = alloca [2 x i32], i32 1
  %stack.ptr_445 = alloca [2 x i32], i32 1
  %stack.ptr_446 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_447 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_448 = alloca [2 x i32], i32 1
  %stack.ptr_449 = alloca [2 x i32], i32 1
  %stack.ptr_450 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_451 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_452 = alloca [2 x i32], i32 1
  %stack.ptr_453 = alloca [2 x i32], i32 1
  %stack.ptr_454 = alloca [2 x i32], i32 1
  %stack.ptr_455 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_456 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_457 = alloca [3 x i32], i32 1
  %stack.ptr_458 = alloca [3 x i32], i32 1
  %stack.ptr_459 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_460 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_461 = alloca [3 x i32], i32 1
  %stack.ptr_462 = alloca [3 x i32], i32 1
  %stack.ptr_463 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_464 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_465 = alloca [2 x i32], i32 1
  %stack.ptr_466 = alloca [2 x i32], i32 1
  %stack.ptr_467 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_468 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_469 = alloca [2 x i32], i32 1
  %stack.ptr_470 = alloca [2 x i32], i32 1
  %stack.ptr_471 = alloca [2 x i32], i32 1
  %stack.ptr_472 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_473 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_474 = alloca [3 x i32], i32 1
  %stack.ptr_475 = alloca [3 x i32], i32 1
  %stack.ptr_476 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_477 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_478 = alloca [3 x i32], i32 1
  %stack.ptr_479 = alloca [3 x i32], i32 1
  %stack.ptr_480 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_481 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_482 = alloca [2 x i32], i32 1
  %stack.ptr_483 = alloca [2 x i32], i32 1
  %stack.ptr_484 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_485 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_486 = alloca [2 x i32], i32 1
  %stack.ptr_487 = alloca [2 x i32], i32 1
  %stack.ptr_488 = alloca [2 x i32], i32 1
  %stack.ptr_489 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_490 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_491 = alloca [2 x i32], i32 1
  %stack.ptr_492 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_493 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_494 = alloca [2 x i32], i32 1
  %stack.ptr_495 = alloca [2 x i32], i32 1
  %stack.ptr_496 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_497 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_498 = alloca [2 x i32], i32 1
  %stack.ptr_499 = alloca [2 x i32], i32 1
  %stack.ptr_500 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_501 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_502 = alloca [2 x i32], i32 1
  %stack.ptr_503 = alloca [2 x i32], i32 1
  %stack.ptr_504 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_505 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_506 = alloca [2 x i32], i32 1
  %stack.ptr_507 = alloca [2 x i32], i32 1
  %stack.ptr_508 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_509 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_510 = alloca [1 x i32], i32 1
  %stack.ptr_511 = alloca [1 x i32], i32 1
  %stack.ptr_512 = alloca [1 x i32], i32 1
  %stack.ptr_513 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_514 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_515 = alloca [2 x i32], i32 1
  %stack.ptr_516 = alloca [2 x i32], i32 1
  %stack.ptr_517 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_518 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_519 = alloca [2 x i32], i32 1
  %stack.ptr_520 = alloca [2 x i32], i32 1
  %stack.ptr_521 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_522 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_523 = alloca [3 x i32], i32 1
  %stack.ptr_524 = alloca [3 x i32], i32 1
  %stack.ptr_525 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_526 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_527 = alloca [2 x i32], i32 1
  %stack.ptr_528 = alloca [2 x i32], i32 1
  %stack.ptr_529 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_530 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_531 = alloca [2 x i32], i32 1
  %stack.ptr_532 = alloca [2 x i32], i32 1
  %stack.ptr_533 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_534 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_535 = alloca [2 x i32], i32 1
  %stack.ptr_536 = alloca [2 x i32], i32 1
  %stack.ptr_537 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_538 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_539 = alloca [1 x i32], i32 1
  %stack.ptr_540 = alloca [1 x i32], i32 1
  %stack.ptr_541 = alloca [1 x i32], i32 1
  %stack.ptr_542 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_543 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_544 = alloca [2 x i32], i32 1
  %stack.ptr_545 = alloca [2 x i32], i32 1
  %stack.ptr_546 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_547 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_548 = alloca [1 x i32], i32 1
  %stack.ptr_549 = alloca [1 x i32], i32 1
  %stack.ptr_550 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_551 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_552 = alloca [1 x i32], i32 1
  %stack.ptr_553 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_554 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_555 = alloca [2 x i32], i32 1
  %stack.ptr_556 = alloca [2 x i32], i32 1
  %stack.ptr_557 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_558 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_559 = alloca [1 x i32], i32 1
  %stack.ptr_560 = alloca [1 x i32], i32 1
  %stack.ptr_561 = alloca [1 x i32], i32 1
  %stack.ptr_562 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_563 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_564 = alloca [1 x i32], i32 1
  %stack.ptr_565 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_566 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_567 = alloca [2 x i32], i32 1
  %stack.ptr_568 = alloca [2 x i32], i32 1
  %stack.ptr_569 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_570 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_571 = alloca [1 x i32], i32 1
  %stack.ptr_572 = alloca [1 x i32], i32 1
  %stack.ptr_573 = alloca [2 x i32], i32 1
  %stack.ptr_574 = alloca [2 x i32], i32 1
  %stack.ptr_575 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_576 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_577 = alloca [1 x i32], i32 1
  %stack.ptr_578 = alloca [1 x i32], i32 1
  %stack.ptr_579 = alloca [2 x i32], i32 1
  %stack.ptr_580 = alloca [2 x i32], i32 1
  %stack.ptr_581 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_582 = alloca %btree_iterator_t_7, i32 1
  %stack.ptr_583 = alloca [1 x i32], i32 1
  %stack.ptr_584 = alloca [1 x i32], i32 1
  %stack.ptr_585 = alloca [2 x i32], i32 1
  %stack.ptr_586 = alloca [2 x i32], i32 1
  %stack.ptr_587 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_588 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_589 = alloca [1 x i32], i32 1
  %stack.ptr_590 = alloca [1 x i32], i32 1
  %stack.ptr_591 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_592 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_593 = alloca [3 x i32], i32 1
  %stack.ptr_594 = alloca [3 x i32], i32 1
  %stack.ptr_595 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_596 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_597 = alloca [1 x i32], i32 1
  %stack.ptr_598 = alloca [1 x i32], i32 1
  %stack.ptr_599 = alloca [1 x i32], i32 1
  %stack.ptr_600 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_601 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_602 = alloca [1 x i32], i32 1
  %stack.ptr_603 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_604 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_605 = alloca [2 x i32], i32 1
  %stack.ptr_606 = alloca [2 x i32], i32 1
  %stack.ptr_607 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_608 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_609 = alloca [2 x i32], i32 1
  %stack.ptr_610 = alloca [2 x i32], i32 1
  %stack.ptr_611 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_612 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_613 = alloca [2 x i32], i32 1
  %stack.ptr_614 = alloca [2 x i32], i32 1
  %stack.ptr_615 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_616 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_617 = alloca [3 x i32], i32 1
  %stack.ptr_618 = alloca [3 x i32], i32 1
  %stack.ptr_619 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_620 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_621 = alloca [1 x i32], i32 1
  %stack.ptr_622 = alloca [1 x i32], i32 1
  %stack.ptr_623 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_624 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_625 = alloca [3 x i32], i32 1
  %stack.ptr_626 = alloca [2 x i32], i32 1
  %stack.ptr_627 = alloca [2 x i32], i32 1
  %stack.ptr_628 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_629 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_630 = alloca [2 x i32], i32 1
  %stack.ptr_631 = alloca [2 x i32], i32 1
  %stack.ptr_632 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_633 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_634 = alloca [2 x i32], i32 1
  %stack.ptr_635 = alloca [3 x i32], i32 1
  %stack.ptr_636 = alloca [3 x i32], i32 1
  %stack.ptr_637 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_638 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_639 = alloca [2 x i32], i32 1
  %stack.ptr_640 = alloca [2 x i32], i32 1
  %stack.ptr_641 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_642 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_643 = alloca [3 x i32], i32 1
  %stack.ptr_644 = alloca [3 x i32], i32 1
  %stack.ptr_645 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_646 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_647 = alloca [2 x i32], i32 1
  %stack.ptr_648 = alloca [2 x i32], i32 1
  %stack.ptr_649 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_650 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_651 = alloca [2 x i32], i32 1
  %stack.ptr_652 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_653 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_654 = alloca [3 x i32], i32 1
  %stack.ptr_655 = alloca [3 x i32], i32 1
  %stack.ptr_656 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_657 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_658 = alloca [3 x i32], i32 1
  %stack.ptr_659 = alloca [3 x i32], i32 1
  %stack.ptr_660 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_661 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_662 = alloca [2 x i32], i32 1
  %stack.ptr_663 = alloca [2 x i32], i32 1
  %stack.ptr_664 = alloca [2 x i32], i32 1
  %stack.ptr_665 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_666 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_667 = alloca [2 x i32], i32 1
  %stack.ptr_668 = alloca [2 x i32], i32 1
  %stack.ptr_669 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_670 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_671 = alloca [2 x i32], i32 1
  %stack.ptr_672 = alloca [3 x i32], i32 1
  %stack.ptr_673 = alloca [3 x i32], i32 1
  %stack.ptr_674 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_675 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_676 = alloca [3 x i32], i32 1
  %stack.ptr_677 = alloca [3 x i32], i32 1
  %stack.ptr_678 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_679 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_680 = alloca [2 x i32], i32 1
  %stack.ptr_681 = alloca [2 x i32], i32 1
  %stack.ptr_682 = alloca [2 x i32], i32 1
  %stack.ptr_683 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_684 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_685 = alloca [2 x i32], i32 1
  %stack.ptr_686 = alloca [2 x i32], i32 1
  %stack.ptr_687 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_688 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_689 = alloca [2 x i32], i32 1
  %stack.ptr_690 = alloca [2 x i32], i32 1
  %stack.ptr_691 = alloca [2 x i32], i32 1
  %stack.ptr_692 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_693 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_694 = alloca [2 x i32], i32 1
  %stack.ptr_695 = alloca [2 x i32], i32 1
  %stack.ptr_696 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_697 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_698 = alloca [2 x i32], i32 1
  %stack.ptr_699 = alloca [4 x i32], i32 1
  %stack.ptr_700 = alloca [4 x i32], i32 1
  %stack.ptr_701 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_702 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_703 = alloca [2 x i32], i32 1
  %stack.ptr_704 = alloca [2 x i32], i32 1
  %stack.ptr_705 = alloca [2 x i32], i32 1
  %stack.ptr_706 = alloca [2 x i32], i32 1
  %stack.ptr_707 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_708 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_709 = alloca [2 x i32], i32 1
  %stack.ptr_710 = alloca [2 x i32], i32 1
  %stack.ptr_711 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_712 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_713 = alloca [4 x i32], i32 1
  %stack.ptr_714 = alloca [4 x i32], i32 1
  %stack.ptr_715 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_716 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_717 = alloca [2 x i32], i32 1
  %stack.ptr_718 = alloca [2 x i32], i32 1
  %stack.ptr_719 = alloca [2 x i32], i32 1
  %stack.ptr_720 = alloca [2 x i32], i32 1
  %stack.ptr_721 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_722 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_723 = alloca [3 x i32], i32 1
  %stack.ptr_724 = alloca [3 x i32], i32 1
  %stack.ptr_725 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_726 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_727 = alloca [2 x i32], i32 1
  %stack.ptr_728 = alloca [2 x i32], i32 1
  %stack.ptr_729 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_730 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_731 = alloca [2 x i32], i32 1
  %stack.ptr_732 = alloca [2 x i32], i32 1
  %stack.ptr_733 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_734 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_735 = alloca [2 x i32], i32 1
  %stack.ptr_736 = alloca [2 x i32], i32 1
  %stack.ptr_737 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_738 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_739 = alloca [2 x i32], i32 1
  %stack.ptr_740 = alloca [2 x i32], i32 1
  %stack.ptr_741 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_742 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_743 = alloca [2 x i32], i32 1
  %stack.ptr_744 = alloca [2 x i32], i32 1
  %stack.ptr_745 = alloca [2 x i32], i32 1
  %stack.ptr_746 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_747 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_748 = alloca [2 x i32], i32 1
  %stack.ptr_749 = alloca [2 x i32], i32 1
  %stack.ptr_750 = alloca [2 x i32], i32 1
  %stack.ptr_751 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_752 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_753 = alloca [2 x i32], i32 1
  %stack.ptr_754 = alloca [2 x i32], i32 1
  %stack.ptr_755 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_756 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_757 = alloca [2 x i32], i32 1
  %stack.ptr_758 = alloca [2 x i32], i32 1
  %stack.ptr_759 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_760 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_761 = alloca [2 x i32], i32 1
  %stack.ptr_762 = alloca [2 x i32], i32 1
  %stack.ptr_763 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_764 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_765 = alloca [2 x i32], i32 1
  %stack.ptr_766 = alloca [2 x i32], i32 1
  %stack.ptr_767 = alloca [2 x i32], i32 1
  %stack.ptr_768 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_769 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_770 = alloca [2 x i32], i32 1
  %stack.ptr_771 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_772 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_773 = alloca [2 x i32], i32 1
  %stack.ptr_774 = alloca [2 x i32], i32 1
  %stack.ptr_775 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_776 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_777 = alloca [3 x i32], i32 1
  %stack.ptr_778 = alloca [3 x i32], i32 1
  %stack.ptr_779 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_780 = alloca %btree_iterator_t_8, i32 1
  %stack.ptr_781 = alloca [2 x i32], i32 1
  %stack.ptr_782 = alloca [2 x i32], i32 1
  %stack.ptr_783 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_784 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_785 = alloca [2 x i32], i32 1
  %stack.ptr_786 = alloca [2 x i32], i32 1
  %stack.ptr_787 = alloca [2 x i32], i32 1
  %stack.ptr_788 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_789 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_790 = alloca [3 x i32], i32 1
  %stack.ptr_791 = alloca [2 x i32], i32 1
  %stack.ptr_792 = alloca [2 x i32], i32 1
  %stack.ptr_793 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_794 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_795 = alloca [3 x i32], i32 1
  %stack.ptr_796 = alloca [3 x i32], i32 1
  %stack.ptr_797 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_798 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_799 = alloca [2 x i32], i32 1
  %stack.ptr_800 = alloca [2 x i32], i32 1
  %stack.ptr_801 = alloca [2 x i32], i32 1
  %stack.ptr_802 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_803 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_804 = alloca [3 x i32], i32 1
  %stack.ptr_805 = alloca [3 x i32], i32 1
  %stack.ptr_806 = alloca [3 x i32], i32 1
  %stack.ptr_807 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_808 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_809 = alloca [2 x i32], i32 1
  %stack.ptr_810 = alloca [2 x i32], i32 1
  %stack.ptr_811 = alloca [2 x i32], i32 1
  %stack.ptr_812 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_813 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_814 = alloca [2 x i32], i32 1
  %stack.ptr_815 = alloca [2 x i32], i32 1
  %stack.ptr_816 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_817 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_818 = alloca [3 x i32], i32 1
  %stack.ptr_819 = alloca [3 x i32], i32 1
  %stack.ptr_820 = alloca [3 x i32], i32 1
  %stack.ptr_821 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_822 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_823 = alloca [4 x i32], i32 1
  %stack.ptr_824 = alloca [4 x i32], i32 1
  %stack.ptr_825 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_826 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_827 = alloca [2 x i32], i32 1
  %stack.ptr_828 = alloca [1 x i32], i32 1
  %stack.ptr_829 = alloca [1 x i32], i32 1
  %stack.ptr_830 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_831 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_832 = alloca [2 x i32], i32 1
  %stack.ptr_833 = alloca [2 x i32], i32 1
  %stack.ptr_834 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_835 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_836 = alloca [2 x i32], i32 1
  %stack.ptr_837 = alloca [2 x i32], i32 1
  %stack.ptr_838 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_839 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_840 = alloca [3 x i32], i32 1
  %stack.ptr_841 = alloca [3 x i32], i32 1
  %stack.ptr_842 = alloca [3 x i32], i32 1
  %stack.ptr_843 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_844 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_845 = alloca [4 x i32], i32 1
  %stack.ptr_846 = alloca [4 x i32], i32 1
  %stack.ptr_847 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_848 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_849 = alloca [2 x i32], i32 1
  %stack.ptr_850 = alloca [1 x i32], i32 1
  %stack.ptr_851 = alloca [1 x i32], i32 1
  %stack.ptr_852 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_853 = alloca %btree_iterator_t_6, i32 1
  %stack.ptr_854 = alloca [2 x i32], i32 1
  %stack.ptr_855 = alloca [2 x i32], i32 1
  %stack.ptr_856 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_857 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_858 = alloca [2 x i32], i32 1
  %stack.ptr_859 = alloca [2 x i32], i32 1
  %stack.ptr_860 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_861 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_862 = alloca [3 x i32], i32 1
  %stack.ptr_863 = alloca [4 x i32], i32 1
  %stack.ptr_864 = alloca [4 x i32], i32 1
  %stack.ptr_865 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_866 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_867 = alloca [2 x i32], i32 1
  %stack.ptr_868 = alloca [2 x i32], i32 1
  %stack.ptr_869 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_870 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_871 = alloca [2 x i32], i32 1
  %stack.ptr_872 = alloca [2 x i32], i32 1
  %stack.ptr_873 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_874 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_875 = alloca [2 x i32], i32 1
  %stack.ptr_876 = alloca [3 x i32], i32 1
  %stack.ptr_877 = alloca [4 x i32], i32 1
  %stack.ptr_878 = alloca [4 x i32], i32 1
  %stack.ptr_879 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_880 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_881 = alloca [2 x i32], i32 1
  %stack.ptr_882 = alloca [2 x i32], i32 1
  %stack.ptr_883 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_884 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_885 = alloca [2 x i32], i32 1
  %stack.ptr_886 = alloca [2 x i32], i32 1
  %stack.ptr_887 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_888 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_889 = alloca [2 x i32], i32 1
  %stack.ptr_890 = alloca [3 x i32], i32 1
  %stack.ptr_891 = alloca [2 x i32], i32 1
  %stack.ptr_892 = alloca [2 x i32], i32 1
  %stack.ptr_893 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_894 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_895 = alloca [2 x i32], i32 1
  %stack.ptr_896 = alloca [2 x i32], i32 1
  %stack.ptr_897 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_898 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_899 = alloca [2 x i32], i32 1
  %stack.ptr_900 = alloca [3 x i32], i32 1
  %stack.ptr_901 = alloca [3 x i32], i32 1
  %stack.ptr_902 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_903 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_904 = alloca [2 x i32], i32 1
  %stack.ptr_905 = alloca [3 x i32], i32 1
  %stack.ptr_906 = alloca [3 x i32], i32 1
  %stack.ptr_907 = alloca [3 x i32], i32 1
  %stack.ptr_908 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_909 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_910 = alloca [4 x i32], i32 1
  %stack.ptr_911 = alloca [4 x i32], i32 1
  %stack.ptr_912 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_913 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_914 = alloca [2 x i32], i32 1
  %stack.ptr_915 = alloca [2 x i32], i32 1
  %stack.ptr_916 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_917 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_918 = alloca [2 x i32], i32 1
  %stack.ptr_919 = alloca [3 x i32], i32 1
  %stack.ptr_920 = alloca [3 x i32], i32 1
  %stack.ptr_921 = alloca [3 x i32], i32 1
  %stack.ptr_922 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_923 = alloca %btree_iterator_t_0, i32 1
  %stack.ptr_924 = alloca [4 x i32], i32 1
  %stack.ptr_925 = alloca [4 x i32], i32 1
  %stack.ptr_926 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_927 = alloca %btree_iterator_t_4, i32 1
  %stack.ptr_928 = alloca [2 x i32], i32 1
  %stack.ptr_929 = alloca [2 x i32], i32 1
  %stack.ptr_930 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_931 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_932 = alloca [2 x i32], i32 1
  %stack.ptr_933 = alloca [3 x i32], i32 1
  %stack.ptr_934 = alloca [4 x i32], i32 1
  %stack.ptr_935 = alloca [4 x i32], i32 1
  %stack.ptr_936 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_937 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_938 = alloca [2 x i32], i32 1
  %stack.ptr_939 = alloca [2 x i32], i32 1
  %stack.ptr_940 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_941 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_942 = alloca [2 x i32], i32 1
  %stack.ptr_943 = alloca [2 x i32], i32 1
  %stack.ptr_944 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_945 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_946 = alloca [2 x i32], i32 1
  %stack.ptr_947 = alloca [2 x i32], i32 1
  %stack.ptr_948 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_949 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_950 = alloca [2 x i32], i32 1
  %stack.ptr_951 = alloca [3 x i32], i32 1
  %stack.ptr_952 = alloca [4 x i32], i32 1
  %stack.ptr_953 = alloca [4 x i32], i32 1
  %stack.ptr_954 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_955 = alloca %btree_iterator_t_3, i32 1
  %stack.ptr_956 = alloca [2 x i32], i32 1
  %stack.ptr_957 = alloca [2 x i32], i32 1
  %stack.ptr_958 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_959 = alloca %btree_iterator_t_1, i32 1
  %stack.ptr_960 = alloca [2 x i32], i32 1
  %stack.ptr_961 = alloca [2 x i32], i32 1
  %stack.ptr_962 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_963 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_964 = alloca [2 x i32], i32 1
  %stack.ptr_965 = alloca [2 x i32], i32 1
  %stack.ptr_966 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_967 = alloca %btree_iterator_t_2, i32 1
  %stack.ptr_968 = alloca [2 x i32], i32 1
  %stack.ptr_969 = alloca [3 x i32], i32 1
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
  %565 = getelementptr %program, ptr %arg_0, i32 0, i32 19
  call ccc void @eclair_btree_lower_bound_0(ptr %565, ptr %stack.ptr_154, ptr %stack.ptr_156)
  %566 = getelementptr %program, ptr %arg_0, i32 0, i32 19
  call ccc void @eclair_btree_upper_bound_0(ptr %566, ptr %stack.ptr_155, ptr %stack.ptr_157)
  br label %loop_35
loop_35:
  %567 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_156, ptr %stack.ptr_157)
  br i1 %567, label %if_41, label %end_if_41
if_41:
  br label %range_query.end_34
end_if_41:
  %568 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_156)
  %569 = getelementptr [2 x i32], ptr %stack.ptr_158, i32 0, i32 0
  %570 = getelementptr [3 x i32], ptr %568, i32 0, i32 2
  %571 = load i32, ptr %570
  store i32 %571, ptr %569
  %572 = getelementptr [2 x i32], ptr %stack.ptr_158, i32 0, i32 1
  store i32 0, ptr %572
  %573 = getelementptr [2 x i32], ptr %stack.ptr_159, i32 0, i32 0
  %574 = getelementptr [3 x i32], ptr %568, i32 0, i32 2
  %575 = load i32, ptr %574
  store i32 %575, ptr %573
  %576 = getelementptr [2 x i32], ptr %stack.ptr_159, i32 0, i32 1
  store i32 4294967295, ptr %576
  %577 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %577, ptr %stack.ptr_158, ptr %stack.ptr_160)
  %578 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %578, ptr %stack.ptr_159, ptr %stack.ptr_161)
  br label %loop_36
loop_36:
  %579 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_160, ptr %stack.ptr_161)
  br i1 %579, label %if_42, label %end_if_42
if_42:
  br label %range_query.end_35
end_if_42:
  %580 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_160)
  %581 = getelementptr [2 x i32], ptr %stack.ptr_162, i32 0, i32 0
  store i32 0, ptr %581
  %582 = getelementptr [2 x i32], ptr %stack.ptr_162, i32 0, i32 1
  %583 = getelementptr [2 x i32], ptr %580, i32 0, i32 1
  %584 = load i32, ptr %583
  store i32 %584, ptr %582
  %585 = getelementptr [2 x i32], ptr %stack.ptr_163, i32 0, i32 0
  store i32 4294967295, ptr %585
  %586 = getelementptr [2 x i32], ptr %stack.ptr_163, i32 0, i32 1
  %587 = getelementptr [2 x i32], ptr %580, i32 0, i32 1
  %588 = load i32, ptr %587
  store i32 %588, ptr %586
  %589 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_lower_bound_2(ptr %589, ptr %stack.ptr_162, ptr %stack.ptr_164)
  %590 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_upper_bound_2(ptr %590, ptr %stack.ptr_163, ptr %stack.ptr_165)
  br label %loop_37
loop_37:
  %591 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_164, ptr %stack.ptr_165)
  br i1 %591, label %if_43, label %end_if_43
if_43:
  br label %range_query.end_36
end_if_43:
  %592 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_164)
  %593 = getelementptr [3 x i32], ptr %stack.ptr_166, i32 0, i32 0
  %594 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %595 = load i32, ptr %594
  store i32 %595, ptr %593
  %596 = getelementptr [3 x i32], ptr %stack.ptr_166, i32 0, i32 1
  %597 = getelementptr [2 x i32], ptr %592, i32 0, i32 0
  %598 = load i32, ptr %597
  store i32 %598, ptr %596
  %599 = getelementptr [3 x i32], ptr %stack.ptr_166, i32 0, i32 2
  store i32 0, ptr %599
  %600 = getelementptr [3 x i32], ptr %stack.ptr_167, i32 0, i32 0
  %601 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %602 = load i32, ptr %601
  store i32 %602, ptr %600
  %603 = getelementptr [3 x i32], ptr %stack.ptr_167, i32 0, i32 1
  %604 = getelementptr [2 x i32], ptr %592, i32 0, i32 0
  %605 = load i32, ptr %604
  store i32 %605, ptr %603
  %606 = getelementptr [3 x i32], ptr %stack.ptr_167, i32 0, i32 2
  store i32 4294967295, ptr %606
  %607 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_lower_bound_0(ptr %607, ptr %stack.ptr_166, ptr %stack.ptr_168)
  %608 = getelementptr %program, ptr %arg_0, i32 0, i32 1
  call ccc void @eclair_btree_upper_bound_0(ptr %608, ptr %stack.ptr_167, ptr %stack.ptr_169)
  br label %loop_38
loop_38:
  %609 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_168, ptr %stack.ptr_169)
  br i1 %609, label %if_44, label %end_if_44
if_44:
  br label %range_query.end_37
end_if_44:
  %610 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_168)
  %611 = getelementptr [3 x i32], ptr %stack.ptr_170, i32 0, i32 0
  %612 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %613 = load i32, ptr %612
  store i32 %613, ptr %611
  %614 = getelementptr [3 x i32], ptr %stack.ptr_170, i32 0, i32 1
  %615 = getelementptr [3 x i32], ptr %568, i32 0, i32 1
  %616 = load i32, ptr %615
  store i32 %616, ptr %614
  %617 = getelementptr [3 x i32], ptr %stack.ptr_170, i32 0, i32 2
  %618 = getelementptr [3 x i32], ptr %610, i32 0, i32 2
  %619 = load i32, ptr %618
  store i32 %619, ptr %617
  %620 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  %621 = call ccc i1 @eclair_btree_contains_0(ptr %620, ptr %stack.ptr_170)
  %622 = select i1 %621, i1 0, i1 1
  br i1 %622, label %if_45, label %end_if_45
if_45:
  %623 = getelementptr [3 x i32], ptr %stack.ptr_171, i32 0, i32 0
  %624 = getelementptr [3 x i32], ptr %568, i32 0, i32 0
  %625 = load i32, ptr %624
  store i32 %625, ptr %623
  %626 = getelementptr [3 x i32], ptr %stack.ptr_171, i32 0, i32 1
  %627 = getelementptr [3 x i32], ptr %568, i32 0, i32 1
  %628 = load i32, ptr %627
  store i32 %628, ptr %626
  %629 = getelementptr [3 x i32], ptr %stack.ptr_171, i32 0, i32 2
  %630 = getelementptr [3 x i32], ptr %610, i32 0, i32 2
  %631 = load i32, ptr %630
  store i32 %631, ptr %629
  %632 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  %633 = call ccc i1 @eclair_btree_insert_value_0(ptr %632, ptr %stack.ptr_171)
  br label %end_if_45
end_if_45:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_168)
  br label %loop_38
range_query.end_37:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_164)
  br label %loop_37
range_query.end_36:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_160)
  br label %loop_36
range_query.end_35:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_156)
  br label %loop_35
range_query.end_34:
  %634 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  %635 = call ccc i1 @eclair_btree_is_empty_0(ptr %634)
  br i1 %635, label %if_46, label %end_if_46
if_46:
  br label %loop.end
end_if_46:
  %636 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  call ccc void @eclair_btree_begin_0(ptr %636, ptr %stack.ptr_172)
  %637 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  call ccc void @eclair_btree_end_0(ptr %637, ptr %stack.ptr_173)
  %638 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_insert_range_points_to_new_points_to(ptr %638, ptr %stack.ptr_172, ptr %stack.ptr_173)
  %639 = getelementptr %program, ptr %arg_0, i32 0, i32 44
  %640 = getelementptr %program, ptr %arg_0, i32 0, i32 19
  call ccc void @eclair_btree_swap_0(ptr %639, ptr %640)
  br label %loop_34
loop.end:
  %641 = getelementptr [3 x i32], ptr %stack.ptr_174, i32 0, i32 0
  store i32 0, ptr %641
  %642 = getelementptr [3 x i32], ptr %stack.ptr_174, i32 0, i32 1
  store i32 0, ptr %642
  %643 = getelementptr [3 x i32], ptr %stack.ptr_174, i32 0, i32 2
  store i32 0, ptr %643
  %644 = getelementptr [3 x i32], ptr %stack.ptr_175, i32 0, i32 0
  store i32 4294967295, ptr %644
  %645 = getelementptr [3 x i32], ptr %stack.ptr_175, i32 0, i32 1
  store i32 4294967295, ptr %645
  %646 = getelementptr [3 x i32], ptr %stack.ptr_175, i32 0, i32 2
  store i32 4294967295, ptr %646
  %647 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_lower_bound_0(ptr %647, ptr %stack.ptr_174, ptr %stack.ptr_176)
  %648 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_upper_bound_0(ptr %648, ptr %stack.ptr_175, ptr %stack.ptr_177)
  br label %loop_39
loop_39:
  %649 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_176, ptr %stack.ptr_177)
  br i1 %649, label %if_47, label %end_if_47
if_47:
  br label %range_query.end_38
end_if_47:
  %650 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_176)
  %651 = getelementptr [3 x i32], ptr %stack.ptr_178, i32 0, i32 0
  %652 = getelementptr [3 x i32], ptr %650, i32 0, i32 0
  %653 = load i32, ptr %652
  store i32 %653, ptr %651
  %654 = getelementptr [3 x i32], ptr %stack.ptr_178, i32 0, i32 1
  %655 = getelementptr [3 x i32], ptr %650, i32 0, i32 1
  %656 = load i32, ptr %655
  store i32 %656, ptr %654
  %657 = getelementptr [3 x i32], ptr %stack.ptr_178, i32 0, i32 2
  store i32 0, ptr %657
  %658 = getelementptr [3 x i32], ptr %stack.ptr_179, i32 0, i32 0
  %659 = getelementptr [3 x i32], ptr %650, i32 0, i32 0
  %660 = load i32, ptr %659
  store i32 %660, ptr %658
  %661 = getelementptr [3 x i32], ptr %stack.ptr_179, i32 0, i32 1
  %662 = getelementptr [3 x i32], ptr %650, i32 0, i32 1
  %663 = load i32, ptr %662
  store i32 %663, ptr %661
  %664 = getelementptr [3 x i32], ptr %stack.ptr_179, i32 0, i32 2
  store i32 4294967295, ptr %664
  %665 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_lower_bound_0(ptr %665, ptr %stack.ptr_178, ptr %stack.ptr_180)
  %666 = getelementptr %program, ptr %arg_0, i32 0, i32 48
  call ccc void @eclair_btree_upper_bound_0(ptr %666, ptr %stack.ptr_179, ptr %stack.ptr_181)
  br label %loop_40
loop_40:
  %667 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_180, ptr %stack.ptr_181)
  br i1 %667, label %if_48, label %end_if_48
if_48:
  br label %range_query.end_39
end_if_48:
  %668 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_180)
  %669 = getelementptr [2 x i32], ptr %stack.ptr_182, i32 0, i32 0
  %670 = getelementptr [3 x i32], ptr %650, i32 0, i32 2
  %671 = load i32, ptr %670
  store i32 %671, ptr %669
  %672 = getelementptr [2 x i32], ptr %stack.ptr_182, i32 0, i32 1
  %673 = getelementptr [3 x i32], ptr %668, i32 0, i32 2
  %674 = load i32, ptr %673
  store i32 %674, ptr %672
  %675 = getelementptr [2 x i32], ptr %stack.ptr_183, i32 0, i32 0
  %676 = getelementptr [3 x i32], ptr %650, i32 0, i32 2
  %677 = load i32, ptr %676
  store i32 %677, ptr %675
  %678 = getelementptr [2 x i32], ptr %stack.ptr_183, i32 0, i32 1
  %679 = getelementptr [3 x i32], ptr %668, i32 0, i32 2
  %680 = load i32, ptr %679
  store i32 %680, ptr %678
  %681 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_lower_bound_1(ptr %681, ptr %stack.ptr_182, ptr %stack.ptr_184)
  %682 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_upper_bound_1(ptr %682, ptr %stack.ptr_183, ptr %stack.ptr_185)
  br label %loop_41
loop_41:
  %683 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_184, ptr %stack.ptr_185)
  br i1 %683, label %if_49, label %end_if_49
if_49:
  br label %range_query.end_40
end_if_49:
  %684 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_184)
  %685 = getelementptr [1 x i32], ptr %stack.ptr_186, i32 0, i32 0
  %686 = getelementptr [3 x i32], ptr %650, i32 0, i32 0
  %687 = load i32, ptr %686
  store i32 %687, ptr %685
  %688 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  %689 = call ccc i1 @eclair_btree_insert_value_6(ptr %688, ptr %stack.ptr_186)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_184)
  br label %loop_41
range_query.end_40:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_180)
  br label %loop_40
range_query.end_39:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_176)
  br label %loop_39
range_query.end_38:
  %690 = getelementptr [3 x i32], ptr %stack.ptr_187, i32 0, i32 0
  store i32 0, ptr %690
  %691 = getelementptr [3 x i32], ptr %stack.ptr_187, i32 0, i32 1
  store i32 0, ptr %691
  %692 = getelementptr [3 x i32], ptr %stack.ptr_187, i32 0, i32 2
  store i32 0, ptr %692
  %693 = getelementptr [3 x i32], ptr %stack.ptr_188, i32 0, i32 0
  store i32 4294967295, ptr %693
  %694 = getelementptr [3 x i32], ptr %stack.ptr_188, i32 0, i32 1
  store i32 4294967295, ptr %694
  %695 = getelementptr [3 x i32], ptr %stack.ptr_188, i32 0, i32 2
  store i32 4294967295, ptr %695
  %696 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %696, ptr %stack.ptr_187, ptr %stack.ptr_189)
  %697 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %697, ptr %stack.ptr_188, ptr %stack.ptr_190)
  br label %loop_42
loop_42:
  %698 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_189, ptr %stack.ptr_190)
  br i1 %698, label %if_50, label %end_if_50
if_50:
  br label %range_query.end_41
end_if_50:
  %699 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_189)
  %700 = getelementptr [3 x i32], ptr %stack.ptr_191, i32 0, i32 0
  %701 = getelementptr [3 x i32], ptr %699, i32 0, i32 2
  %702 = load i32, ptr %701
  store i32 %702, ptr %700
  %703 = getelementptr [3 x i32], ptr %stack.ptr_191, i32 0, i32 1
  store i32 0, ptr %703
  %704 = getelementptr [3 x i32], ptr %stack.ptr_191, i32 0, i32 2
  store i32 0, ptr %704
  %705 = getelementptr [3 x i32], ptr %stack.ptr_192, i32 0, i32 0
  %706 = getelementptr [3 x i32], ptr %699, i32 0, i32 2
  %707 = load i32, ptr %706
  store i32 %707, ptr %705
  %708 = getelementptr [3 x i32], ptr %stack.ptr_192, i32 0, i32 1
  store i32 4294967295, ptr %708
  %709 = getelementptr [3 x i32], ptr %stack.ptr_192, i32 0, i32 2
  store i32 4294967295, ptr %709
  %710 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %710, ptr %stack.ptr_191, ptr %stack.ptr_193)
  %711 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %711, ptr %stack.ptr_192, ptr %stack.ptr_194)
  br label %loop_43
loop_43:
  %712 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_193, ptr %stack.ptr_194)
  br i1 %712, label %if_51, label %end_if_51
if_51:
  br label %range_query.end_42
end_if_51:
  %713 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_193)
  %714 = getelementptr [2 x i32], ptr %stack.ptr_195, i32 0, i32 0
  %715 = getelementptr [3 x i32], ptr %713, i32 0, i32 1
  %716 = load i32, ptr %715
  store i32 %716, ptr %714
  %717 = getelementptr [2 x i32], ptr %stack.ptr_195, i32 0, i32 1
  %718 = getelementptr [3 x i32], ptr %713, i32 0, i32 2
  %719 = load i32, ptr %718
  store i32 %719, ptr %717
  %720 = getelementptr [2 x i32], ptr %stack.ptr_196, i32 0, i32 0
  %721 = getelementptr [3 x i32], ptr %713, i32 0, i32 1
  %722 = load i32, ptr %721
  store i32 %722, ptr %720
  %723 = getelementptr [2 x i32], ptr %stack.ptr_196, i32 0, i32 1
  %724 = getelementptr [3 x i32], ptr %713, i32 0, i32 2
  %725 = load i32, ptr %724
  store i32 %725, ptr %723
  %726 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_lower_bound_1(ptr %726, ptr %stack.ptr_195, ptr %stack.ptr_197)
  %727 = getelementptr %program, ptr %arg_0, i32 0, i32 36
  call ccc void @eclair_btree_upper_bound_1(ptr %727, ptr %stack.ptr_196, ptr %stack.ptr_198)
  br label %loop_44
loop_44:
  %728 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_197, ptr %stack.ptr_198)
  br i1 %728, label %if_52, label %end_if_52
if_52:
  br label %range_query.end_43
end_if_52:
  %729 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_197)
  %730 = getelementptr [1 x i32], ptr %stack.ptr_199, i32 0, i32 0
  %731 = getelementptr [3 x i32], ptr %699, i32 0, i32 0
  %732 = load i32, ptr %731
  store i32 %732, ptr %730
  %733 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  %734 = call ccc i1 @eclair_btree_insert_value_6(ptr %733, ptr %stack.ptr_199)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_197)
  br label %loop_44
range_query.end_43:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_193)
  br label %loop_43
range_query.end_42:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_189)
  br label %loop_42
range_query.end_41:
  %735 = getelementptr [2 x i32], ptr %stack.ptr_200, i32 0, i32 0
  store i32 0, ptr %735
  %736 = getelementptr [2 x i32], ptr %stack.ptr_200, i32 0, i32 1
  store i32 54, ptr %736
  %737 = getelementptr [2 x i32], ptr %stack.ptr_201, i32 0, i32 0
  store i32 4294967295, ptr %737
  %738 = getelementptr [2 x i32], ptr %stack.ptr_201, i32 0, i32 1
  store i32 54, ptr %738
  %739 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_lower_bound_2(ptr %739, ptr %stack.ptr_200, ptr %stack.ptr_202)
  %740 = getelementptr %program, ptr %arg_0, i32 0, i32 67
  call ccc void @eclair_btree_upper_bound_2(ptr %740, ptr %stack.ptr_201, ptr %stack.ptr_203)
  br label %loop_45
loop_45:
  %741 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_202, ptr %stack.ptr_203)
  br i1 %741, label %if_53, label %end_if_53
if_53:
  br label %range_query.end_44
end_if_53:
  %742 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_202)
  %743 = getelementptr [1 x i32], ptr %stack.ptr_204, i32 0, i32 0
  %744 = getelementptr [2 x i32], ptr %742, i32 0, i32 0
  %745 = load i32, ptr %744
  store i32 %745, ptr %743
  %746 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  %747 = call ccc i1 @eclair_btree_insert_value_6(ptr %746, ptr %stack.ptr_204)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_202)
  br label %loop_45
range_query.end_44:
  %748 = getelementptr [3 x i32], ptr %stack.ptr_205, i32 0, i32 0
  store i32 0, ptr %748
  %749 = getelementptr [3 x i32], ptr %stack.ptr_205, i32 0, i32 1
  store i32 0, ptr %749
  %750 = getelementptr [3 x i32], ptr %stack.ptr_205, i32 0, i32 2
  store i32 0, ptr %750
  %751 = getelementptr [3 x i32], ptr %stack.ptr_206, i32 0, i32 0
  store i32 4294967295, ptr %751
  %752 = getelementptr [3 x i32], ptr %stack.ptr_206, i32 0, i32 1
  store i32 4294967295, ptr %752
  %753 = getelementptr [3 x i32], ptr %stack.ptr_206, i32 0, i32 2
  store i32 4294967295, ptr %753
  %754 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_lower_bound_0(ptr %754, ptr %stack.ptr_205, ptr %stack.ptr_207)
  %755 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_upper_bound_0(ptr %755, ptr %stack.ptr_206, ptr %stack.ptr_208)
  br label %loop_46
loop_46:
  %756 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_207, ptr %stack.ptr_208)
  br i1 %756, label %if_54, label %end_if_54
if_54:
  br label %range_query.end_45
end_if_54:
  %757 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_207)
  %758 = getelementptr [1 x i32], ptr %stack.ptr_209, i32 0, i32 0
  %759 = getelementptr [3 x i32], ptr %757, i32 0, i32 2
  %760 = load i32, ptr %759
  store i32 %760, ptr %758
  %761 = getelementptr [1 x i32], ptr %stack.ptr_210, i32 0, i32 0
  %762 = getelementptr [3 x i32], ptr %757, i32 0, i32 2
  %763 = load i32, ptr %762
  store i32 %763, ptr %761
  %764 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %764, ptr %stack.ptr_209, ptr %stack.ptr_211)
  %765 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %765, ptr %stack.ptr_210, ptr %stack.ptr_212)
  br label %loop_47
loop_47:
  %766 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_211, ptr %stack.ptr_212)
  br i1 %766, label %if_55, label %end_if_55
if_55:
  br label %range_query.end_46
end_if_55:
  %767 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_211)
  %768 = getelementptr [3 x i32], ptr %stack.ptr_213, i32 0, i32 0
  %769 = getelementptr [3 x i32], ptr %757, i32 0, i32 0
  %770 = load i32, ptr %769
  store i32 %770, ptr %768
  %771 = getelementptr [3 x i32], ptr %stack.ptr_213, i32 0, i32 1
  %772 = getelementptr [3 x i32], ptr %757, i32 0, i32 2
  %773 = load i32, ptr %772
  store i32 %773, ptr %771
  %774 = getelementptr [3 x i32], ptr %stack.ptr_213, i32 0, i32 2
  %775 = getelementptr [3 x i32], ptr %757, i32 0, i32 1
  %776 = load i32, ptr %775
  store i32 %776, ptr %774
  %777 = getelementptr %program, ptr %arg_0, i32 0, i32 73
  %778 = call ccc i1 @eclair_btree_insert_value_0(ptr %777, ptr %stack.ptr_213)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_211)
  br label %loop_47
range_query.end_46:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_207)
  br label %loop_46
range_query.end_45:
  %779 = getelementptr [2 x i32], ptr %stack.ptr_214, i32 0, i32 0
  store i32 0, ptr %779
  %780 = getelementptr [2 x i32], ptr %stack.ptr_214, i32 0, i32 1
  store i32 0, ptr %780
  %781 = getelementptr [2 x i32], ptr %stack.ptr_215, i32 0, i32 0
  store i32 4294967295, ptr %781
  %782 = getelementptr [2 x i32], ptr %stack.ptr_215, i32 0, i32 1
  store i32 4294967295, ptr %782
  %783 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %783, ptr %stack.ptr_214, ptr %stack.ptr_216)
  %784 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %784, ptr %stack.ptr_215, ptr %stack.ptr_217)
  br label %loop_48
loop_48:
  %785 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_216, ptr %stack.ptr_217)
  br i1 %785, label %if_56, label %end_if_56
if_56:
  br label %range_query.end_47
end_if_56:
  %786 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_216)
  %787 = getelementptr [3 x i32], ptr %stack.ptr_218, i32 0, i32 0
  %788 = getelementptr [2 x i32], ptr %786, i32 0, i32 1
  %789 = load i32, ptr %788
  store i32 %789, ptr %787
  %790 = getelementptr [3 x i32], ptr %stack.ptr_218, i32 0, i32 1
  store i32 0, ptr %790
  %791 = getelementptr [3 x i32], ptr %stack.ptr_218, i32 0, i32 2
  store i32 0, ptr %791
  %792 = getelementptr [3 x i32], ptr %stack.ptr_219, i32 0, i32 0
  %793 = getelementptr [2 x i32], ptr %786, i32 0, i32 1
  %794 = load i32, ptr %793
  store i32 %794, ptr %792
  %795 = getelementptr [3 x i32], ptr %stack.ptr_219, i32 0, i32 1
  store i32 4294967295, ptr %795
  %796 = getelementptr [3 x i32], ptr %stack.ptr_219, i32 0, i32 2
  store i32 4294967295, ptr %796
  %797 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_0(ptr %797, ptr %stack.ptr_218, ptr %stack.ptr_220)
  %798 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_0(ptr %798, ptr %stack.ptr_219, ptr %stack.ptr_221)
  br label %loop_49
loop_49:
  %799 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_220, ptr %stack.ptr_221)
  br i1 %799, label %if_57, label %end_if_57
if_57:
  br label %range_query.end_48
end_if_57:
  %800 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_220)
  %801 = getelementptr [1 x i32], ptr %stack.ptr_222, i32 0, i32 0
  %802 = getelementptr [3 x i32], ptr %800, i32 0, i32 2
  %803 = load i32, ptr %802
  store i32 %803, ptr %801
  %804 = getelementptr [1 x i32], ptr %stack.ptr_223, i32 0, i32 0
  %805 = getelementptr [3 x i32], ptr %800, i32 0, i32 2
  %806 = load i32, ptr %805
  store i32 %806, ptr %804
  %807 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %807, ptr %stack.ptr_222, ptr %stack.ptr_224)
  %808 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %808, ptr %stack.ptr_223, ptr %stack.ptr_225)
  br label %loop_50
loop_50:
  %809 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_224, ptr %stack.ptr_225)
  br i1 %809, label %if_58, label %end_if_58
if_58:
  br label %range_query.end_49
end_if_58:
  %810 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_224)
  %811 = getelementptr [3 x i32], ptr %stack.ptr_226, i32 0, i32 0
  %812 = getelementptr [2 x i32], ptr %786, i32 0, i32 1
  %813 = load i32, ptr %812
  store i32 %813, ptr %811
  %814 = getelementptr [3 x i32], ptr %stack.ptr_226, i32 0, i32 1
  %815 = getelementptr [3 x i32], ptr %800, i32 0, i32 2
  %816 = load i32, ptr %815
  store i32 %816, ptr %814
  %817 = getelementptr [3 x i32], ptr %stack.ptr_226, i32 0, i32 2
  %818 = getelementptr [3 x i32], ptr %800, i32 0, i32 1
  %819 = load i32, ptr %818
  store i32 %819, ptr %817
  %820 = getelementptr %program, ptr %arg_0, i32 0, i32 72
  %821 = call ccc i1 @eclair_btree_insert_value_0(ptr %820, ptr %stack.ptr_226)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_224)
  br label %loop_50
range_query.end_49:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_220)
  br label %loop_49
range_query.end_48:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_216)
  br label %loop_48
range_query.end_47:
  %822 = getelementptr [4 x i32], ptr %stack.ptr_227, i32 0, i32 0
  store i32 0, ptr %822
  %823 = getelementptr [4 x i32], ptr %stack.ptr_227, i32 0, i32 1
  store i32 0, ptr %823
  %824 = getelementptr [4 x i32], ptr %stack.ptr_227, i32 0, i32 2
  store i32 0, ptr %824
  %825 = getelementptr [4 x i32], ptr %stack.ptr_227, i32 0, i32 3
  store i32 0, ptr %825
  %826 = getelementptr [4 x i32], ptr %stack.ptr_228, i32 0, i32 0
  store i32 4294967295, ptr %826
  %827 = getelementptr [4 x i32], ptr %stack.ptr_228, i32 0, i32 1
  store i32 4294967295, ptr %827
  %828 = getelementptr [4 x i32], ptr %stack.ptr_228, i32 0, i32 2
  store i32 4294967295, ptr %828
  %829 = getelementptr [4 x i32], ptr %stack.ptr_228, i32 0, i32 3
  store i32 4294967295, ptr %829
  %830 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %830, ptr %stack.ptr_227, ptr %stack.ptr_229)
  %831 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %831, ptr %stack.ptr_228, ptr %stack.ptr_230)
  br label %loop_51
loop_51:
  %832 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_229, ptr %stack.ptr_230)
  br i1 %832, label %if_59, label %end_if_59
if_59:
  br label %range_query.end_50
end_if_59:
  %833 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_229)
  %834 = getelementptr [1 x i32], ptr %stack.ptr_231, i32 0, i32 0
  %835 = getelementptr [4 x i32], ptr %833, i32 0, i32 2
  %836 = load i32, ptr %835
  store i32 %836, ptr %834
  %837 = getelementptr [1 x i32], ptr %stack.ptr_232, i32 0, i32 0
  %838 = getelementptr [4 x i32], ptr %833, i32 0, i32 2
  %839 = load i32, ptr %838
  store i32 %839, ptr %837
  %840 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %840, ptr %stack.ptr_231, ptr %stack.ptr_233)
  %841 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %841, ptr %stack.ptr_232, ptr %stack.ptr_234)
  br label %loop_52
loop_52:
  %842 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_233, ptr %stack.ptr_234)
  br i1 %842, label %if_60, label %end_if_60
if_60:
  br label %range_query.end_51
end_if_60:
  %843 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_233)
  %844 = getelementptr [2 x i32], ptr %stack.ptr_235, i32 0, i32 0
  %845 = getelementptr [4 x i32], ptr %833, i32 0, i32 0
  %846 = load i32, ptr %845
  store i32 %846, ptr %844
  %847 = getelementptr [2 x i32], ptr %stack.ptr_235, i32 0, i32 1
  %848 = getelementptr [4 x i32], ptr %833, i32 0, i32 2
  %849 = load i32, ptr %848
  store i32 %849, ptr %847
  %850 = getelementptr %program, ptr %arg_0, i32 0, i32 70
  %851 = call ccc i1 @eclair_btree_insert_value_1(ptr %850, ptr %stack.ptr_235)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_233)
  br label %loop_52
range_query.end_51:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_229)
  br label %loop_51
range_query.end_50:
  %852 = getelementptr [4 x i32], ptr %stack.ptr_236, i32 0, i32 0
  store i32 0, ptr %852
  %853 = getelementptr [4 x i32], ptr %stack.ptr_236, i32 0, i32 1
  store i32 0, ptr %853
  %854 = getelementptr [4 x i32], ptr %stack.ptr_236, i32 0, i32 2
  store i32 0, ptr %854
  %855 = getelementptr [4 x i32], ptr %stack.ptr_236, i32 0, i32 3
  store i32 0, ptr %855
  %856 = getelementptr [4 x i32], ptr %stack.ptr_237, i32 0, i32 0
  store i32 4294967295, ptr %856
  %857 = getelementptr [4 x i32], ptr %stack.ptr_237, i32 0, i32 1
  store i32 4294967295, ptr %857
  %858 = getelementptr [4 x i32], ptr %stack.ptr_237, i32 0, i32 2
  store i32 4294967295, ptr %858
  %859 = getelementptr [4 x i32], ptr %stack.ptr_237, i32 0, i32 3
  store i32 4294967295, ptr %859
  %860 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %860, ptr %stack.ptr_236, ptr %stack.ptr_238)
  %861 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %861, ptr %stack.ptr_237, ptr %stack.ptr_239)
  br label %loop_53
loop_53:
  %862 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_238, ptr %stack.ptr_239)
  br i1 %862, label %if_61, label %end_if_61
if_61:
  br label %range_query.end_52
end_if_61:
  %863 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_238)
  %864 = getelementptr [1 x i32], ptr %stack.ptr_240, i32 0, i32 0
  %865 = getelementptr [4 x i32], ptr %863, i32 0, i32 3
  %866 = load i32, ptr %865
  store i32 %866, ptr %864
  %867 = getelementptr [1 x i32], ptr %stack.ptr_241, i32 0, i32 0
  %868 = getelementptr [4 x i32], ptr %863, i32 0, i32 3
  %869 = load i32, ptr %868
  store i32 %869, ptr %867
  %870 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %870, ptr %stack.ptr_240, ptr %stack.ptr_242)
  %871 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %871, ptr %stack.ptr_241, ptr %stack.ptr_243)
  br label %loop_54
loop_54:
  %872 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_242, ptr %stack.ptr_243)
  br i1 %872, label %if_62, label %end_if_62
if_62:
  br label %range_query.end_53
end_if_62:
  %873 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_242)
  %874 = getelementptr [2 x i32], ptr %stack.ptr_244, i32 0, i32 0
  %875 = getelementptr [4 x i32], ptr %863, i32 0, i32 0
  %876 = load i32, ptr %875
  store i32 %876, ptr %874
  %877 = getelementptr [2 x i32], ptr %stack.ptr_244, i32 0, i32 1
  %878 = getelementptr [4 x i32], ptr %863, i32 0, i32 3
  %879 = load i32, ptr %878
  store i32 %879, ptr %877
  %880 = getelementptr %program, ptr %arg_0, i32 0, i32 70
  %881 = call ccc i1 @eclair_btree_insert_value_1(ptr %880, ptr %stack.ptr_244)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_242)
  br label %loop_54
range_query.end_53:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_238)
  br label %loop_53
range_query.end_52:
  %882 = getelementptr [4 x i32], ptr %stack.ptr_245, i32 0, i32 0
  store i32 0, ptr %882
  %883 = getelementptr [4 x i32], ptr %stack.ptr_245, i32 0, i32 1
  store i32 0, ptr %883
  %884 = getelementptr [4 x i32], ptr %stack.ptr_245, i32 0, i32 2
  store i32 0, ptr %884
  %885 = getelementptr [4 x i32], ptr %stack.ptr_245, i32 0, i32 3
  store i32 0, ptr %885
  %886 = getelementptr [4 x i32], ptr %stack.ptr_246, i32 0, i32 0
  store i32 4294967295, ptr %886
  %887 = getelementptr [4 x i32], ptr %stack.ptr_246, i32 0, i32 1
  store i32 4294967295, ptr %887
  %888 = getelementptr [4 x i32], ptr %stack.ptr_246, i32 0, i32 2
  store i32 4294967295, ptr %888
  %889 = getelementptr [4 x i32], ptr %stack.ptr_246, i32 0, i32 3
  store i32 4294967295, ptr %889
  %890 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %890, ptr %stack.ptr_245, ptr %stack.ptr_247)
  %891 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %891, ptr %stack.ptr_246, ptr %stack.ptr_248)
  br label %loop_55
loop_55:
  %892 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_247, ptr %stack.ptr_248)
  br i1 %892, label %if_63, label %end_if_63
if_63:
  br label %range_query.end_54
end_if_63:
  %893 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_247)
  %894 = getelementptr [1 x i32], ptr %stack.ptr_249, i32 0, i32 0
  %895 = getelementptr [4 x i32], ptr %893, i32 0, i32 2
  %896 = load i32, ptr %895
  store i32 %896, ptr %894
  %897 = getelementptr [1 x i32], ptr %stack.ptr_250, i32 0, i32 0
  %898 = getelementptr [4 x i32], ptr %893, i32 0, i32 2
  %899 = load i32, ptr %898
  store i32 %899, ptr %897
  %900 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %900, ptr %stack.ptr_249, ptr %stack.ptr_251)
  %901 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %901, ptr %stack.ptr_250, ptr %stack.ptr_252)
  br label %loop_56
loop_56:
  %902 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_251, ptr %stack.ptr_252)
  br i1 %902, label %if_64, label %end_if_64
if_64:
  br label %range_query.end_55
end_if_64:
  %903 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_251)
  %904 = getelementptr [2 x i32], ptr %stack.ptr_253, i32 0, i32 0
  %905 = getelementptr [4 x i32], ptr %893, i32 0, i32 0
  %906 = load i32, ptr %905
  store i32 %906, ptr %904
  %907 = getelementptr [2 x i32], ptr %stack.ptr_253, i32 0, i32 1
  %908 = getelementptr [4 x i32], ptr %893, i32 0, i32 2
  %909 = load i32, ptr %908
  store i32 %909, ptr %907
  %910 = getelementptr %program, ptr %arg_0, i32 0, i32 69
  %911 = call ccc i1 @eclair_btree_insert_value_1(ptr %910, ptr %stack.ptr_253)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_251)
  br label %loop_56
range_query.end_55:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_247)
  br label %loop_55
range_query.end_54:
  %912 = getelementptr [4 x i32], ptr %stack.ptr_254, i32 0, i32 0
  store i32 0, ptr %912
  %913 = getelementptr [4 x i32], ptr %stack.ptr_254, i32 0, i32 1
  store i32 0, ptr %913
  %914 = getelementptr [4 x i32], ptr %stack.ptr_254, i32 0, i32 2
  store i32 0, ptr %914
  %915 = getelementptr [4 x i32], ptr %stack.ptr_254, i32 0, i32 3
  store i32 0, ptr %915
  %916 = getelementptr [4 x i32], ptr %stack.ptr_255, i32 0, i32 0
  store i32 4294967295, ptr %916
  %917 = getelementptr [4 x i32], ptr %stack.ptr_255, i32 0, i32 1
  store i32 4294967295, ptr %917
  %918 = getelementptr [4 x i32], ptr %stack.ptr_255, i32 0, i32 2
  store i32 4294967295, ptr %918
  %919 = getelementptr [4 x i32], ptr %stack.ptr_255, i32 0, i32 3
  store i32 4294967295, ptr %919
  %920 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %920, ptr %stack.ptr_254, ptr %stack.ptr_256)
  %921 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %921, ptr %stack.ptr_255, ptr %stack.ptr_257)
  br label %loop_57
loop_57:
  %922 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_256, ptr %stack.ptr_257)
  br i1 %922, label %if_65, label %end_if_65
if_65:
  br label %range_query.end_56
end_if_65:
  %923 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_256)
  %924 = getelementptr [1 x i32], ptr %stack.ptr_258, i32 0, i32 0
  %925 = getelementptr [4 x i32], ptr %923, i32 0, i32 3
  %926 = load i32, ptr %925
  store i32 %926, ptr %924
  %927 = getelementptr [1 x i32], ptr %stack.ptr_259, i32 0, i32 0
  %928 = getelementptr [4 x i32], ptr %923, i32 0, i32 3
  %929 = load i32, ptr %928
  store i32 %929, ptr %927
  %930 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %930, ptr %stack.ptr_258, ptr %stack.ptr_260)
  %931 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %931, ptr %stack.ptr_259, ptr %stack.ptr_261)
  br label %loop_58
loop_58:
  %932 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_260, ptr %stack.ptr_261)
  br i1 %932, label %if_66, label %end_if_66
if_66:
  br label %range_query.end_57
end_if_66:
  %933 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_260)
  %934 = getelementptr [2 x i32], ptr %stack.ptr_262, i32 0, i32 0
  %935 = getelementptr [4 x i32], ptr %923, i32 0, i32 0
  %936 = load i32, ptr %935
  store i32 %936, ptr %934
  %937 = getelementptr [2 x i32], ptr %stack.ptr_262, i32 0, i32 1
  %938 = getelementptr [4 x i32], ptr %923, i32 0, i32 3
  %939 = load i32, ptr %938
  store i32 %939, ptr %937
  %940 = getelementptr %program, ptr %arg_0, i32 0, i32 69
  %941 = call ccc i1 @eclair_btree_insert_value_1(ptr %940, ptr %stack.ptr_262)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_260)
  br label %loop_58
range_query.end_57:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_256)
  br label %loop_57
range_query.end_56:
  %942 = getelementptr [1 x i32], ptr %stack.ptr_263, i32 0, i32 0
  store i32 59, ptr %942
  %943 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %944 = call ccc i1 @eclair_btree_insert_value_6(ptr %943, ptr %stack.ptr_263)
  %945 = getelementptr [1 x i32], ptr %stack.ptr_264, i32 0, i32 0
  store i32 58, ptr %945
  %946 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %947 = call ccc i1 @eclair_btree_insert_value_6(ptr %946, ptr %stack.ptr_264)
  %948 = getelementptr [1 x i32], ptr %stack.ptr_265, i32 0, i32 0
  store i32 57, ptr %948
  %949 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %950 = call ccc i1 @eclair_btree_insert_value_6(ptr %949, ptr %stack.ptr_265)
  %951 = getelementptr [1 x i32], ptr %stack.ptr_266, i32 0, i32 0
  store i32 56, ptr %951
  %952 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %953 = call ccc i1 @eclair_btree_insert_value_6(ptr %952, ptr %stack.ptr_266)
  %954 = getelementptr [1 x i32], ptr %stack.ptr_267, i32 0, i32 0
  store i32 55, ptr %954
  %955 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  %956 = call ccc i1 @eclair_btree_insert_value_6(ptr %955, ptr %stack.ptr_267)
  %957 = getelementptr [3 x i32], ptr %stack.ptr_268, i32 0, i32 0
  store i32 0, ptr %957
  %958 = getelementptr [3 x i32], ptr %stack.ptr_268, i32 0, i32 1
  store i32 0, ptr %958
  %959 = getelementptr [3 x i32], ptr %stack.ptr_268, i32 0, i32 2
  store i32 0, ptr %959
  %960 = getelementptr [3 x i32], ptr %stack.ptr_269, i32 0, i32 0
  store i32 4294967295, ptr %960
  %961 = getelementptr [3 x i32], ptr %stack.ptr_269, i32 0, i32 1
  store i32 4294967295, ptr %961
  %962 = getelementptr [3 x i32], ptr %stack.ptr_269, i32 0, i32 2
  store i32 4294967295, ptr %962
  %963 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %963, ptr %stack.ptr_268, ptr %stack.ptr_270)
  %964 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %964, ptr %stack.ptr_269, ptr %stack.ptr_271)
  br label %loop_59
loop_59:
  %965 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_270, ptr %stack.ptr_271)
  br i1 %965, label %if_67, label %end_if_67
if_67:
  br label %range_query.end_58
end_if_67:
  %966 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_270)
  %967 = getelementptr [2 x i32], ptr %stack.ptr_272, i32 0, i32 0
  %968 = getelementptr [3 x i32], ptr %966, i32 0, i32 2
  %969 = load i32, ptr %968
  store i32 %969, ptr %967
  %970 = getelementptr [2 x i32], ptr %stack.ptr_272, i32 0, i32 1
  store i32 0, ptr %970
  %971 = getelementptr [2 x i32], ptr %stack.ptr_273, i32 0, i32 0
  %972 = getelementptr [3 x i32], ptr %966, i32 0, i32 2
  %973 = load i32, ptr %972
  store i32 %973, ptr %971
  %974 = getelementptr [2 x i32], ptr %stack.ptr_273, i32 0, i32 1
  store i32 4294967295, ptr %974
  %975 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %975, ptr %stack.ptr_272, ptr %stack.ptr_274)
  %976 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %976, ptr %stack.ptr_273, ptr %stack.ptr_275)
  br label %loop_60
loop_60:
  %977 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_274, ptr %stack.ptr_275)
  br i1 %977, label %if_68, label %end_if_68
if_68:
  br label %range_query.end_59
end_if_68:
  %978 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_274)
  %979 = getelementptr [2 x i32], ptr %stack.ptr_276, i32 0, i32 0
  %980 = getelementptr [3 x i32], ptr %966, i32 0, i32 0
  %981 = load i32, ptr %980
  store i32 %981, ptr %979
  %982 = getelementptr [2 x i32], ptr %stack.ptr_276, i32 0, i32 1
  %983 = getelementptr [3 x i32], ptr %966, i32 0, i32 2
  %984 = load i32, ptr %983
  store i32 %984, ptr %982
  %985 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  %986 = call ccc i1 @eclair_btree_insert_value_1(ptr %985, ptr %stack.ptr_276)
  %987 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  %988 = call ccc i1 @eclair_btree_insert_value_2(ptr %987, ptr %stack.ptr_276)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_274)
  br label %loop_60
range_query.end_59:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_270)
  br label %loop_59
range_query.end_58:
  %989 = getelementptr [2 x i32], ptr %stack.ptr_277, i32 0, i32 0
  store i32 0, ptr %989
  %990 = getelementptr [2 x i32], ptr %stack.ptr_277, i32 0, i32 1
  store i32 0, ptr %990
  %991 = getelementptr [2 x i32], ptr %stack.ptr_278, i32 0, i32 0
  store i32 4294967295, ptr %991
  %992 = getelementptr [2 x i32], ptr %stack.ptr_278, i32 0, i32 1
  store i32 4294967295, ptr %992
  %993 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %993, ptr %stack.ptr_277, ptr %stack.ptr_279)
  %994 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %994, ptr %stack.ptr_278, ptr %stack.ptr_280)
  br label %loop_61
loop_61:
  %995 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_279, ptr %stack.ptr_280)
  br i1 %995, label %if_69, label %end_if_69
if_69:
  br label %range_query.end_60
end_if_69:
  %996 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_279)
  %997 = getelementptr [2 x i32], ptr %stack.ptr_281, i32 0, i32 0
  %998 = getelementptr [2 x i32], ptr %996, i32 0, i32 0
  %999 = load i32, ptr %998
  store i32 %999, ptr %997
  %1000 = getelementptr [2 x i32], ptr %stack.ptr_281, i32 0, i32 1
  %1001 = getelementptr [2 x i32], ptr %996, i32 0, i32 0
  %1002 = load i32, ptr %1001
  store i32 %1002, ptr %1000
  %1003 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  %1004 = call ccc i1 @eclair_btree_insert_value_1(ptr %1003, ptr %stack.ptr_281)
  %1005 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  %1006 = call ccc i1 @eclair_btree_insert_value_2(ptr %1005, ptr %stack.ptr_281)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_279)
  br label %loop_61
range_query.end_60:
  %1007 = getelementptr [2 x i32], ptr %stack.ptr_282, i32 0, i32 0
  store i32 0, ptr %1007
  %1008 = getelementptr [2 x i32], ptr %stack.ptr_282, i32 0, i32 1
  store i32 0, ptr %1008
  %1009 = getelementptr [2 x i32], ptr %stack.ptr_283, i32 0, i32 0
  store i32 4294967295, ptr %1009
  %1010 = getelementptr [2 x i32], ptr %stack.ptr_283, i32 0, i32 1
  store i32 4294967295, ptr %1010
  %1011 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_lower_bound_1(ptr %1011, ptr %stack.ptr_282, ptr %stack.ptr_284)
  %1012 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_upper_bound_1(ptr %1012, ptr %stack.ptr_283, ptr %stack.ptr_285)
  br label %loop_62
loop_62:
  %1013 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_284, ptr %stack.ptr_285)
  br i1 %1013, label %if_70, label %end_if_70
if_70:
  br label %range_query.end_61
end_if_70:
  %1014 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_284)
  %1015 = getelementptr [2 x i32], ptr %stack.ptr_286, i32 0, i32 0
  %1016 = getelementptr [2 x i32], ptr %1014, i32 0, i32 0
  %1017 = load i32, ptr %1016
  store i32 %1017, ptr %1015
  %1018 = getelementptr [2 x i32], ptr %stack.ptr_286, i32 0, i32 1
  store i32 0, ptr %1018
  %1019 = getelementptr [2 x i32], ptr %stack.ptr_287, i32 0, i32 0
  %1020 = getelementptr [2 x i32], ptr %1014, i32 0, i32 0
  %1021 = load i32, ptr %1020
  store i32 %1021, ptr %1019
  %1022 = getelementptr [2 x i32], ptr %stack.ptr_287, i32 0, i32 1
  store i32 4294967295, ptr %1022
  %1023 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_lower_bound_1(ptr %1023, ptr %stack.ptr_286, ptr %stack.ptr_288)
  %1024 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_upper_bound_1(ptr %1024, ptr %stack.ptr_287, ptr %stack.ptr_289)
  br label %loop_63
loop_63:
  %1025 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_288, ptr %stack.ptr_289)
  br i1 %1025, label %if_71, label %end_if_71
if_71:
  br label %range_query.end_62
end_if_71:
  %1026 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_288)
  %1027 = getelementptr [2 x i32], ptr %stack.ptr_290, i32 0, i32 0
  %1028 = getelementptr [2 x i32], ptr %1014, i32 0, i32 1
  %1029 = load i32, ptr %1028
  store i32 %1029, ptr %1027
  %1030 = getelementptr [2 x i32], ptr %stack.ptr_290, i32 0, i32 1
  store i32 0, ptr %1030
  %1031 = getelementptr [2 x i32], ptr %stack.ptr_291, i32 0, i32 0
  %1032 = getelementptr [2 x i32], ptr %1014, i32 0, i32 1
  %1033 = load i32, ptr %1032
  store i32 %1033, ptr %1031
  %1034 = getelementptr [2 x i32], ptr %stack.ptr_291, i32 0, i32 1
  store i32 4294967295, ptr %1034
  %1035 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %1035, ptr %stack.ptr_290, ptr %stack.ptr_292)
  %1036 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %1036, ptr %stack.ptr_291, ptr %stack.ptr_293)
  br label %loop_64
loop_64:
  %1037 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_292, ptr %stack.ptr_293)
  br i1 %1037, label %if_72, label %end_if_72
if_72:
  br label %range_query.end_63
end_if_72:
  %1038 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_292)
  %1039 = getelementptr [2 x i32], ptr %stack.ptr_294, i32 0, i32 0
  %1040 = getelementptr [2 x i32], ptr %1026, i32 0, i32 1
  %1041 = load i32, ptr %1040
  store i32 %1041, ptr %1039
  %1042 = getelementptr [2 x i32], ptr %stack.ptr_294, i32 0, i32 1
  store i32 0, ptr %1042
  %1043 = getelementptr [2 x i32], ptr %stack.ptr_295, i32 0, i32 0
  %1044 = getelementptr [2 x i32], ptr %1026, i32 0, i32 1
  %1045 = load i32, ptr %1044
  store i32 %1045, ptr %1043
  %1046 = getelementptr [2 x i32], ptr %stack.ptr_295, i32 0, i32 1
  store i32 4294967295, ptr %1046
  %1047 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %1047, ptr %stack.ptr_294, ptr %stack.ptr_296)
  %1048 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %1048, ptr %stack.ptr_295, ptr %stack.ptr_297)
  br label %loop_65
loop_65:
  %1049 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_296, ptr %stack.ptr_297)
  br i1 %1049, label %if_73, label %end_if_73
if_73:
  br label %range_query.end_64
end_if_73:
  %1050 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_296)
  %1051 = getelementptr [2 x i32], ptr %1038, i32 0, i32 1
  %1052 = load i32, ptr %1051
  %1053 = getelementptr [2 x i32], ptr %1050, i32 0, i32 1
  %1054 = load i32, ptr %1053
  %1055 = icmp ne i32 %1052, %1054
  br i1 %1055, label %if_74, label %end_if_76
if_74:
  %1056 = getelementptr [2 x i32], ptr %stack.ptr_298, i32 0, i32 0
  %1057 = getelementptr [2 x i32], ptr %1038, i32 0, i32 1
  %1058 = load i32, ptr %1057
  store i32 %1058, ptr %1056
  %1059 = getelementptr [2 x i32], ptr %stack.ptr_298, i32 0, i32 1
  store i32 0, ptr %1059
  %1060 = getelementptr [2 x i32], ptr %stack.ptr_299, i32 0, i32 0
  %1061 = getelementptr [2 x i32], ptr %1038, i32 0, i32 1
  %1062 = load i32, ptr %1061
  store i32 %1062, ptr %1060
  %1063 = getelementptr [2 x i32], ptr %stack.ptr_299, i32 0, i32 1
  store i32 4294967295, ptr %1063
  %1064 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %1064, ptr %stack.ptr_298, ptr %stack.ptr_300)
  %1065 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %1065, ptr %stack.ptr_299, ptr %stack.ptr_301)
  br label %loop_66
loop_66:
  %1066 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_300, ptr %stack.ptr_301)
  br i1 %1066, label %if_75, label %end_if_74
if_75:
  br label %range_query.end_65
end_if_74:
  %1067 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_300)
  %1068 = getelementptr [2 x i32], ptr %stack.ptr_302, i32 0, i32 0
  %1069 = getelementptr [2 x i32], ptr %1050, i32 0, i32 1
  %1070 = load i32, ptr %1069
  store i32 %1070, ptr %1068
  %1071 = getelementptr [2 x i32], ptr %stack.ptr_302, i32 0, i32 1
  %1072 = getelementptr [2 x i32], ptr %1067, i32 0, i32 1
  %1073 = load i32, ptr %1072
  store i32 %1073, ptr %1071
  %1074 = getelementptr [2 x i32], ptr %stack.ptr_303, i32 0, i32 0
  %1075 = getelementptr [2 x i32], ptr %1050, i32 0, i32 1
  %1076 = load i32, ptr %1075
  store i32 %1076, ptr %1074
  %1077 = getelementptr [2 x i32], ptr %stack.ptr_303, i32 0, i32 1
  %1078 = getelementptr [2 x i32], ptr %1067, i32 0, i32 1
  %1079 = load i32, ptr %1078
  store i32 %1079, ptr %1077
  %1080 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %1080, ptr %stack.ptr_302, ptr %stack.ptr_304)
  %1081 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %1081, ptr %stack.ptr_303, ptr %stack.ptr_305)
  br label %loop_67
loop_67:
  %1082 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_304, ptr %stack.ptr_305)
  br i1 %1082, label %if_76, label %end_if_75
if_76:
  br label %range_query.end_66
end_if_75:
  %1083 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_304)
  %1084 = getelementptr [3 x i32], ptr %stack.ptr_306, i32 0, i32 0
  %1085 = getelementptr [2 x i32], ptr %1014, i32 0, i32 0
  %1086 = load i32, ptr %1085
  store i32 %1086, ptr %1084
  %1087 = getelementptr [3 x i32], ptr %stack.ptr_306, i32 0, i32 1
  %1088 = getelementptr [2 x i32], ptr %1038, i32 0, i32 1
  %1089 = load i32, ptr %1088
  store i32 %1089, ptr %1087
  %1090 = getelementptr [3 x i32], ptr %stack.ptr_306, i32 0, i32 2
  %1091 = getelementptr [2 x i32], ptr %1067, i32 0, i32 1
  %1092 = load i32, ptr %1091
  store i32 %1092, ptr %1090
  %1093 = getelementptr %program, ptr %arg_0, i32 0, i32 8
  %1094 = call ccc i1 @eclair_btree_insert_value_0(ptr %1093, ptr %stack.ptr_306)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_304)
  br label %loop_67
range_query.end_66:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_300)
  br label %loop_66
range_query.end_65:
  br label %end_if_76
end_if_76:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_296)
  br label %loop_65
range_query.end_64:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_292)
  br label %loop_64
range_query.end_63:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_288)
  br label %loop_63
range_query.end_62:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_284)
  br label %loop_62
range_query.end_61:
  %1095 = getelementptr [2 x i32], ptr %stack.ptr_307, i32 0, i32 0
  store i32 0, ptr %1095
  %1096 = getelementptr [2 x i32], ptr %stack.ptr_307, i32 0, i32 1
  store i32 0, ptr %1096
  %1097 = getelementptr [2 x i32], ptr %stack.ptr_308, i32 0, i32 0
  store i32 4294967295, ptr %1097
  %1098 = getelementptr [2 x i32], ptr %stack.ptr_308, i32 0, i32 1
  store i32 4294967295, ptr %1098
  %1099 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_lower_bound_1(ptr %1099, ptr %stack.ptr_307, ptr %stack.ptr_309)
  %1100 = getelementptr %program, ptr %arg_0, i32 0, i32 56
  call ccc void @eclair_btree_upper_bound_1(ptr %1100, ptr %stack.ptr_308, ptr %stack.ptr_310)
  br label %loop_68
loop_68:
  %1101 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_309, ptr %stack.ptr_310)
  br i1 %1101, label %if_77, label %end_if_77
if_77:
  br label %range_query.end_67
end_if_77:
  %1102 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_309)
  %1103 = getelementptr [2 x i32], ptr %stack.ptr_311, i32 0, i32 0
  %1104 = getelementptr [2 x i32], ptr %1102, i32 0, i32 1
  %1105 = load i32, ptr %1104
  store i32 %1105, ptr %1103
  %1106 = getelementptr [2 x i32], ptr %stack.ptr_311, i32 0, i32 1
  store i32 0, ptr %1106
  %1107 = getelementptr [2 x i32], ptr %stack.ptr_312, i32 0, i32 0
  %1108 = getelementptr [2 x i32], ptr %1102, i32 0, i32 1
  %1109 = load i32, ptr %1108
  store i32 %1109, ptr %1107
  %1110 = getelementptr [2 x i32], ptr %stack.ptr_312, i32 0, i32 1
  store i32 4294967295, ptr %1110
  %1111 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %1111, ptr %stack.ptr_311, ptr %stack.ptr_313)
  %1112 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %1112, ptr %stack.ptr_312, ptr %stack.ptr_314)
  br label %loop_69
loop_69:
  %1113 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_313, ptr %stack.ptr_314)
  br i1 %1113, label %if_78, label %end_if_78
if_78:
  br label %range_query.end_68
end_if_78:
  %1114 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_313)
  %1115 = getelementptr [2 x i32], ptr %stack.ptr_315, i32 0, i32 0
  %1116 = getelementptr [2 x i32], ptr %1114, i32 0, i32 1
  %1117 = load i32, ptr %1116
  store i32 %1117, ptr %1115
  %1118 = getelementptr [2 x i32], ptr %stack.ptr_315, i32 0, i32 1
  store i32 0, ptr %1118
  %1119 = getelementptr [2 x i32], ptr %stack.ptr_316, i32 0, i32 0
  %1120 = getelementptr [2 x i32], ptr %1114, i32 0, i32 1
  %1121 = load i32, ptr %1120
  store i32 %1121, ptr %1119
  %1122 = getelementptr [2 x i32], ptr %stack.ptr_316, i32 0, i32 1
  store i32 4294967295, ptr %1122
  %1123 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %1123, ptr %stack.ptr_315, ptr %stack.ptr_317)
  %1124 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %1124, ptr %stack.ptr_316, ptr %stack.ptr_318)
  br label %loop_70
loop_70:
  %1125 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_317, ptr %stack.ptr_318)
  br i1 %1125, label %if_79, label %end_if_79
if_79:
  br label %range_query.end_69
end_if_79:
  %1126 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_317)
  %1127 = getelementptr [3 x i32], ptr %stack.ptr_319, i32 0, i32 0
  %1128 = getelementptr [2 x i32], ptr %1102, i32 0, i32 0
  %1129 = load i32, ptr %1128
  store i32 %1129, ptr %1127
  %1130 = getelementptr [3 x i32], ptr %stack.ptr_319, i32 0, i32 1
  %1131 = getelementptr [2 x i32], ptr %1114, i32 0, i32 1
  %1132 = load i32, ptr %1131
  store i32 %1132, ptr %1130
  %1133 = getelementptr [3 x i32], ptr %stack.ptr_319, i32 0, i32 2
  %1134 = getelementptr [2 x i32], ptr %1126, i32 0, i32 1
  %1135 = load i32, ptr %1134
  store i32 %1135, ptr %1133
  %1136 = getelementptr %program, ptr %arg_0, i32 0, i32 8
  %1137 = call ccc i1 @eclair_btree_contains_0(ptr %1136, ptr %stack.ptr_319)
  %1138 = select i1 %1137, i1 0, i1 1
  br i1 %1138, label %if_80, label %end_if_80
if_80:
  %1139 = getelementptr [3 x i32], ptr %stack.ptr_320, i32 0, i32 0
  %1140 = getelementptr [2 x i32], ptr %1102, i32 0, i32 0
  %1141 = load i32, ptr %1140
  store i32 %1141, ptr %1139
  %1142 = getelementptr [3 x i32], ptr %stack.ptr_320, i32 0, i32 1
  %1143 = getelementptr [2 x i32], ptr %1114, i32 0, i32 1
  %1144 = load i32, ptr %1143
  store i32 %1144, ptr %1142
  %1145 = getelementptr [3 x i32], ptr %stack.ptr_320, i32 0, i32 2
  %1146 = getelementptr [2 x i32], ptr %1126, i32 0, i32 1
  %1147 = load i32, ptr %1146
  store i32 %1147, ptr %1145
  %1148 = getelementptr %program, ptr %arg_0, i32 0, i32 63
  %1149 = call ccc i1 @eclair_btree_insert_value_0(ptr %1148, ptr %stack.ptr_320)
  br label %end_if_80
end_if_80:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_317)
  br label %loop_70
range_query.end_69:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_313)
  br label %loop_69
range_query.end_68:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_309)
  br label %loop_68
range_query.end_67:
  %1150 = getelementptr [2 x i32], ptr %stack.ptr_321, i32 0, i32 0
  store i32 0, ptr %1150
  %1151 = getelementptr [2 x i32], ptr %stack.ptr_321, i32 0, i32 1
  store i32 0, ptr %1151
  %1152 = getelementptr [2 x i32], ptr %stack.ptr_322, i32 0, i32 0
  store i32 4294967295, ptr %1152
  %1153 = getelementptr [2 x i32], ptr %stack.ptr_322, i32 0, i32 1
  store i32 4294967295, ptr %1153
  %1154 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1154, ptr %stack.ptr_321, ptr %stack.ptr_323)
  %1155 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1155, ptr %stack.ptr_322, ptr %stack.ptr_324)
  br label %loop_71
loop_71:
  %1156 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_323, ptr %stack.ptr_324)
  br i1 %1156, label %if_81, label %end_if_81
if_81:
  br label %range_query.end_70
end_if_81:
  %1157 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_323)
  %1158 = getelementptr [2 x i32], ptr %stack.ptr_325, i32 0, i32 0
  %1159 = getelementptr [2 x i32], ptr %1157, i32 0, i32 1
  %1160 = load i32, ptr %1159
  store i32 %1160, ptr %1158
  %1161 = getelementptr [2 x i32], ptr %stack.ptr_325, i32 0, i32 1
  store i32 0, ptr %1161
  %1162 = getelementptr [2 x i32], ptr %stack.ptr_326, i32 0, i32 0
  %1163 = getelementptr [2 x i32], ptr %1157, i32 0, i32 1
  %1164 = load i32, ptr %1163
  store i32 %1164, ptr %1162
  %1165 = getelementptr [2 x i32], ptr %stack.ptr_326, i32 0, i32 1
  store i32 4294967295, ptr %1165
  %1166 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1166, ptr %stack.ptr_325, ptr %stack.ptr_327)
  %1167 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1167, ptr %stack.ptr_326, ptr %stack.ptr_328)
  br label %loop_72
loop_72:
  %1168 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_327, ptr %stack.ptr_328)
  br i1 %1168, label %if_82, label %end_if_82
if_82:
  br label %range_query.end_71
end_if_82:
  %1169 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_327)
  %1170 = getelementptr [2 x i32], ptr %stack.ptr_329, i32 0, i32 0
  %1171 = getelementptr [2 x i32], ptr %1157, i32 0, i32 1
  %1172 = load i32, ptr %1171
  store i32 %1172, ptr %1170
  %1173 = getelementptr [2 x i32], ptr %stack.ptr_329, i32 0, i32 1
  store i32 0, ptr %1173
  %1174 = getelementptr [2 x i32], ptr %stack.ptr_330, i32 0, i32 0
  %1175 = getelementptr [2 x i32], ptr %1157, i32 0, i32 1
  %1176 = load i32, ptr %1175
  store i32 %1176, ptr %1174
  %1177 = getelementptr [2 x i32], ptr %stack.ptr_330, i32 0, i32 1
  store i32 4294967295, ptr %1177
  %1178 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %1178, ptr %stack.ptr_329, ptr %stack.ptr_331)
  %1179 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %1179, ptr %stack.ptr_330, ptr %stack.ptr_332)
  br label %loop_73
loop_73:
  %1180 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_331, ptr %stack.ptr_332)
  br i1 %1180, label %if_83, label %end_if_83
if_83:
  br label %range_query.end_72
end_if_83:
  %1181 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_331)
  %1182 = getelementptr [2 x i32], ptr %stack.ptr_333, i32 0, i32 0
  %1183 = getelementptr [2 x i32], ptr %1181, i32 0, i32 1
  %1184 = load i32, ptr %1183
  store i32 %1184, ptr %1182
  %1185 = getelementptr [2 x i32], ptr %stack.ptr_333, i32 0, i32 1
  store i32 0, ptr %1185
  %1186 = getelementptr [2 x i32], ptr %stack.ptr_334, i32 0, i32 0
  %1187 = getelementptr [2 x i32], ptr %1181, i32 0, i32 1
  %1188 = load i32, ptr %1187
  store i32 %1188, ptr %1186
  %1189 = getelementptr [2 x i32], ptr %stack.ptr_334, i32 0, i32 1
  store i32 4294967295, ptr %1189
  %1190 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %1190, ptr %stack.ptr_333, ptr %stack.ptr_335)
  %1191 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %1191, ptr %stack.ptr_334, ptr %stack.ptr_336)
  br label %loop_74
loop_74:
  %1192 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_335, ptr %stack.ptr_336)
  br i1 %1192, label %if_84, label %end_if_84
if_84:
  br label %range_query.end_73
end_if_84:
  %1193 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_335)
  %1194 = getelementptr [2 x i32], ptr %1193, i32 0, i32 1
  %1195 = load i32, ptr %1194
  %1196 = icmp ne i32 %1195, 54
  br i1 %1196, label %if_85, label %end_if_85
if_85:
  %1197 = getelementptr [3 x i32], ptr %stack.ptr_337, i32 0, i32 0
  %1198 = getelementptr [2 x i32], ptr %1157, i32 0, i32 1
  %1199 = load i32, ptr %1198
  store i32 %1199, ptr %1197
  %1200 = getelementptr [3 x i32], ptr %stack.ptr_337, i32 0, i32 1
  %1201 = getelementptr [2 x i32], ptr %1181, i32 0, i32 1
  %1202 = load i32, ptr %1201
  store i32 %1202, ptr %1200
  %1203 = getelementptr [3 x i32], ptr %stack.ptr_337, i32 0, i32 2
  %1204 = getelementptr [2 x i32], ptr %1193, i32 0, i32 1
  %1205 = load i32, ptr %1204
  store i32 %1205, ptr %1203
  %1206 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %1207 = call ccc i1 @eclair_btree_insert_value_0(ptr %1206, ptr %stack.ptr_337)
  br label %end_if_85
end_if_85:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_335)
  br label %loop_74
range_query.end_73:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_331)
  br label %loop_73
range_query.end_72:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_327)
  br label %loop_72
range_query.end_71:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_323)
  br label %loop_71
range_query.end_70:
  %1208 = getelementptr [2 x i32], ptr %stack.ptr_338, i32 0, i32 0
  store i32 0, ptr %1208
  %1209 = getelementptr [2 x i32], ptr %stack.ptr_338, i32 0, i32 1
  store i32 0, ptr %1209
  %1210 = getelementptr [2 x i32], ptr %stack.ptr_339, i32 0, i32 0
  store i32 4294967295, ptr %1210
  %1211 = getelementptr [2 x i32], ptr %stack.ptr_339, i32 0, i32 1
  store i32 4294967295, ptr %1211
  %1212 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1212, ptr %stack.ptr_338, ptr %stack.ptr_340)
  %1213 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1213, ptr %stack.ptr_339, ptr %stack.ptr_341)
  br label %loop_75
loop_75:
  %1214 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_340, ptr %stack.ptr_341)
  br i1 %1214, label %if_86, label %end_if_86
if_86:
  br label %range_query.end_74
end_if_86:
  %1215 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_340)
  %1216 = getelementptr [2 x i32], ptr %stack.ptr_342, i32 0, i32 0
  %1217 = getelementptr [2 x i32], ptr %1215, i32 0, i32 1
  %1218 = load i32, ptr %1217
  store i32 %1218, ptr %1216
  %1219 = getelementptr [2 x i32], ptr %stack.ptr_342, i32 0, i32 1
  store i32 0, ptr %1219
  %1220 = getelementptr [2 x i32], ptr %stack.ptr_343, i32 0, i32 0
  %1221 = getelementptr [2 x i32], ptr %1215, i32 0, i32 1
  %1222 = load i32, ptr %1221
  store i32 %1222, ptr %1220
  %1223 = getelementptr [2 x i32], ptr %stack.ptr_343, i32 0, i32 1
  store i32 4294967295, ptr %1223
  %1224 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_lower_bound_1(ptr %1224, ptr %stack.ptr_342, ptr %stack.ptr_344)
  %1225 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_upper_bound_1(ptr %1225, ptr %stack.ptr_343, ptr %stack.ptr_345)
  br label %loop_76
loop_76:
  %1226 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_344, ptr %stack.ptr_345)
  br i1 %1226, label %if_87, label %end_if_87
if_87:
  br label %range_query.end_75
end_if_87:
  %1227 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_344)
  %1228 = getelementptr [1 x i32], ptr %stack.ptr_346, i32 0, i32 0
  %1229 = getelementptr [2 x i32], ptr %1227, i32 0, i32 1
  %1230 = load i32, ptr %1229
  store i32 %1230, ptr %1228
  %1231 = getelementptr [1 x i32], ptr %stack.ptr_347, i32 0, i32 0
  %1232 = getelementptr [2 x i32], ptr %1227, i32 0, i32 1
  %1233 = load i32, ptr %1232
  store i32 %1233, ptr %1231
  %1234 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_lower_bound_6(ptr %1234, ptr %stack.ptr_346, ptr %stack.ptr_348)
  %1235 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_upper_bound_6(ptr %1235, ptr %stack.ptr_347, ptr %stack.ptr_349)
  br label %loop_77
loop_77:
  %1236 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_348, ptr %stack.ptr_349)
  br i1 %1236, label %if_88, label %end_if_88
if_88:
  br label %range_query.end_76
end_if_88:
  %1237 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_348)
  %1238 = getelementptr [1 x i32], ptr %stack.ptr_350, i32 0, i32 0
  %1239 = getelementptr [2 x i32], ptr %1215, i32 0, i32 0
  %1240 = load i32, ptr %1239
  store i32 %1240, ptr %1238
  %1241 = getelementptr %program, ptr %arg_0, i32 0, i32 30
  %1242 = call ccc i1 @eclair_btree_insert_value_6(ptr %1241, ptr %stack.ptr_350)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_348)
  br label %loop_77
range_query.end_76:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_344)
  br label %loop_76
range_query.end_75:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_340)
  br label %loop_75
range_query.end_74:
  %1243 = getelementptr [2 x i32], ptr %stack.ptr_351, i32 0, i32 0
  store i32 0, ptr %1243
  %1244 = getelementptr [2 x i32], ptr %stack.ptr_351, i32 0, i32 1
  store i32 0, ptr %1244
  %1245 = getelementptr [2 x i32], ptr %stack.ptr_352, i32 0, i32 0
  store i32 4294967295, ptr %1245
  %1246 = getelementptr [2 x i32], ptr %stack.ptr_352, i32 0, i32 1
  store i32 4294967295, ptr %1246
  %1247 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1247, ptr %stack.ptr_351, ptr %stack.ptr_353)
  %1248 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1248, ptr %stack.ptr_352, ptr %stack.ptr_354)
  br label %loop_78
loop_78:
  %1249 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_353, ptr %stack.ptr_354)
  br i1 %1249, label %if_89, label %end_if_89
if_89:
  br label %range_query.end_77
end_if_89:
  %1250 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_353)
  %1251 = getelementptr [1 x i32], ptr %stack.ptr_355, i32 0, i32 0
  %1252 = getelementptr [2 x i32], ptr %1250, i32 0, i32 0
  %1253 = load i32, ptr %1252
  store i32 %1253, ptr %1251
  %1254 = getelementptr %program, ptr %arg_0, i32 0, i32 30
  %1255 = call ccc i1 @eclair_btree_contains_6(ptr %1254, ptr %stack.ptr_355)
  %1256 = select i1 %1255, i1 0, i1 1
  br i1 %1256, label %if_90, label %end_if_90
if_90:
  %1257 = getelementptr [1 x i32], ptr %stack.ptr_356, i32 0, i32 0
  %1258 = getelementptr [2 x i32], ptr %1250, i32 0, i32 0
  %1259 = load i32, ptr %1258
  store i32 %1259, ptr %1257
  %1260 = getelementptr %program, ptr %arg_0, i32 0, i32 46
  %1261 = call ccc i1 @eclair_btree_insert_value_6(ptr %1260, ptr %stack.ptr_356)
  br label %end_if_90
end_if_90:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_353)
  br label %loop_78
range_query.end_77:
  %1262 = getelementptr [2 x i32], ptr %stack.ptr_357, i32 0, i32 0
  store i32 0, ptr %1262
  %1263 = getelementptr [2 x i32], ptr %stack.ptr_357, i32 0, i32 1
  store i32 0, ptr %1263
  %1264 = getelementptr [2 x i32], ptr %stack.ptr_358, i32 0, i32 0
  store i32 4294967295, ptr %1264
  %1265 = getelementptr [2 x i32], ptr %stack.ptr_358, i32 0, i32 1
  store i32 4294967295, ptr %1265
  %1266 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1266, ptr %stack.ptr_357, ptr %stack.ptr_359)
  %1267 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1267, ptr %stack.ptr_358, ptr %stack.ptr_360)
  br label %loop_79
loop_79:
  %1268 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_359, ptr %stack.ptr_360)
  br i1 %1268, label %if_91, label %end_if_91
if_91:
  br label %range_query.end_78
end_if_91:
  %1269 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_359)
  %1270 = getelementptr [2 x i32], ptr %stack.ptr_361, i32 0, i32 0
  %1271 = getelementptr [2 x i32], ptr %1269, i32 0, i32 1
  %1272 = load i32, ptr %1271
  store i32 %1272, ptr %1270
  %1273 = getelementptr [2 x i32], ptr %stack.ptr_361, i32 0, i32 1
  store i32 0, ptr %1273
  %1274 = getelementptr [2 x i32], ptr %stack.ptr_362, i32 0, i32 0
  %1275 = getelementptr [2 x i32], ptr %1269, i32 0, i32 1
  %1276 = load i32, ptr %1275
  store i32 %1276, ptr %1274
  %1277 = getelementptr [2 x i32], ptr %stack.ptr_362, i32 0, i32 1
  store i32 4294967295, ptr %1277
  %1278 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1278, ptr %stack.ptr_361, ptr %stack.ptr_363)
  %1279 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1279, ptr %stack.ptr_362, ptr %stack.ptr_364)
  br label %loop_80
loop_80:
  %1280 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_363, ptr %stack.ptr_364)
  br i1 %1280, label %if_92, label %end_if_92
if_92:
  br label %range_query.end_79
end_if_92:
  %1281 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_363)
  %1282 = getelementptr [1 x i32], ptr %stack.ptr_365, i32 0, i32 0
  %1283 = getelementptr [2 x i32], ptr %1281, i32 0, i32 1
  %1284 = load i32, ptr %1283
  store i32 %1284, ptr %1282
  %1285 = getelementptr %program, ptr %arg_0, i32 0, i32 29
  %1286 = call ccc i1 @eclair_btree_insert_value_6(ptr %1285, ptr %stack.ptr_365)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_363)
  br label %loop_80
range_query.end_79:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_359)
  br label %loop_79
range_query.end_78:
  %1287 = getelementptr [2 x i32], ptr %stack.ptr_366, i32 0, i32 0
  store i32 0, ptr %1287
  %1288 = getelementptr [2 x i32], ptr %stack.ptr_366, i32 0, i32 1
  store i32 0, ptr %1288
  %1289 = getelementptr [2 x i32], ptr %stack.ptr_367, i32 0, i32 0
  store i32 4294967295, ptr %1289
  %1290 = getelementptr [2 x i32], ptr %stack.ptr_367, i32 0, i32 1
  store i32 4294967295, ptr %1290
  %1291 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1291, ptr %stack.ptr_366, ptr %stack.ptr_368)
  %1292 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1292, ptr %stack.ptr_367, ptr %stack.ptr_369)
  br label %loop_81
loop_81:
  %1293 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_368, ptr %stack.ptr_369)
  br i1 %1293, label %if_93, label %end_if_93
if_93:
  br label %range_query.end_80
end_if_93:
  %1294 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_368)
  %1295 = getelementptr [2 x i32], ptr %stack.ptr_370, i32 0, i32 0
  %1296 = getelementptr [2 x i32], ptr %1294, i32 0, i32 1
  %1297 = load i32, ptr %1296
  store i32 %1297, ptr %1295
  %1298 = getelementptr [2 x i32], ptr %stack.ptr_370, i32 0, i32 1
  store i32 0, ptr %1298
  %1299 = getelementptr [2 x i32], ptr %stack.ptr_371, i32 0, i32 0
  %1300 = getelementptr [2 x i32], ptr %1294, i32 0, i32 1
  %1301 = load i32, ptr %1300
  store i32 %1301, ptr %1299
  %1302 = getelementptr [2 x i32], ptr %stack.ptr_371, i32 0, i32 1
  store i32 4294967295, ptr %1302
  %1303 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1303, ptr %stack.ptr_370, ptr %stack.ptr_372)
  %1304 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1304, ptr %stack.ptr_371, ptr %stack.ptr_373)
  br label %loop_82
loop_82:
  %1305 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_372, ptr %stack.ptr_373)
  br i1 %1305, label %if_94, label %end_if_94
if_94:
  br label %range_query.end_81
end_if_94:
  %1306 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_372)
  %1307 = getelementptr [1 x i32], ptr %stack.ptr_374, i32 0, i32 0
  %1308 = getelementptr [2 x i32], ptr %1306, i32 0, i32 1
  %1309 = load i32, ptr %1308
  store i32 %1309, ptr %1307
  %1310 = getelementptr %program, ptr %arg_0, i32 0, i32 29
  %1311 = call ccc i1 @eclair_btree_insert_value_6(ptr %1310, ptr %stack.ptr_374)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_372)
  br label %loop_82
range_query.end_81:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_368)
  br label %loop_81
range_query.end_80:
  %1312 = getelementptr [1 x i32], ptr %stack.ptr_375, i32 0, i32 0
  store i32 0, ptr %1312
  %1313 = getelementptr [1 x i32], ptr %stack.ptr_376, i32 0, i32 0
  store i32 4294967295, ptr %1313
  %1314 = getelementptr %program, ptr %arg_0, i32 0, i32 33
  call ccc void @eclair_btree_lower_bound_6(ptr %1314, ptr %stack.ptr_375, ptr %stack.ptr_377)
  %1315 = getelementptr %program, ptr %arg_0, i32 0, i32 33
  call ccc void @eclair_btree_upper_bound_6(ptr %1315, ptr %stack.ptr_376, ptr %stack.ptr_378)
  br label %loop_83
loop_83:
  %1316 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_377, ptr %stack.ptr_378)
  br i1 %1316, label %if_95, label %end_if_95
if_95:
  br label %range_query.end_82
end_if_95:
  %1317 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_377)
  %1318 = getelementptr [1 x i32], ptr %stack.ptr_379, i32 0, i32 0
  %1319 = getelementptr [1 x i32], ptr %1317, i32 0, i32 0
  %1320 = load i32, ptr %1319
  store i32 %1320, ptr %1318
  %1321 = getelementptr %program, ptr %arg_0, i32 0, i32 29
  %1322 = call ccc i1 @eclair_btree_contains_6(ptr %1321, ptr %stack.ptr_379)
  %1323 = select i1 %1322, i1 0, i1 1
  br i1 %1323, label %if_96, label %end_if_97
if_96:
  %1324 = getelementptr [2 x i32], ptr %stack.ptr_380, i32 0, i32 0
  store i32 0, ptr %1324
  %1325 = getelementptr [2 x i32], ptr %stack.ptr_380, i32 0, i32 1
  %1326 = getelementptr [1 x i32], ptr %1317, i32 0, i32 0
  %1327 = load i32, ptr %1326
  store i32 %1327, ptr %1325
  %1328 = getelementptr [2 x i32], ptr %stack.ptr_381, i32 0, i32 0
  store i32 4294967295, ptr %1328
  %1329 = getelementptr [2 x i32], ptr %stack.ptr_381, i32 0, i32 1
  %1330 = getelementptr [1 x i32], ptr %1317, i32 0, i32 0
  %1331 = load i32, ptr %1330
  store i32 %1331, ptr %1329
  %1332 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_lower_bound_2(ptr %1332, ptr %stack.ptr_380, ptr %stack.ptr_382)
  %1333 = getelementptr %program, ptr %arg_0, i32 0, i32 15
  call ccc void @eclair_btree_upper_bound_2(ptr %1333, ptr %stack.ptr_381, ptr %stack.ptr_383)
  br label %loop_84
loop_84:
  %1334 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_382, ptr %stack.ptr_383)
  br i1 %1334, label %if_97, label %end_if_96
if_97:
  br label %range_query.end_83
end_if_96:
  %1335 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_382)
  %1336 = getelementptr [2 x i32], ptr %stack.ptr_384, i32 0, i32 0
  %1337 = getelementptr [2 x i32], ptr %1335, i32 0, i32 0
  %1338 = load i32, ptr %1337
  store i32 %1338, ptr %1336
  %1339 = getelementptr [2 x i32], ptr %stack.ptr_384, i32 0, i32 1
  %1340 = getelementptr [1 x i32], ptr %1317, i32 0, i32 0
  %1341 = load i32, ptr %1340
  store i32 %1341, ptr %1339
  %1342 = getelementptr %program, ptr %arg_0, i32 0, i32 13
  %1343 = call ccc i1 @eclair_btree_insert_value_1(ptr %1342, ptr %stack.ptr_384)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_382)
  br label %loop_84
range_query.end_83:
  br label %end_if_97
end_if_97:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_377)
  br label %loop_83
range_query.end_82:
  %1344 = getelementptr [1 x i32], ptr %stack.ptr_385, i32 0, i32 0
  store i32 0, ptr %1344
  %1345 = getelementptr [1 x i32], ptr %stack.ptr_386, i32 0, i32 0
  store i32 4294967295, ptr %1345
  %1346 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  call ccc void @eclair_btree_lower_bound_6(ptr %1346, ptr %stack.ptr_385, ptr %stack.ptr_387)
  %1347 = getelementptr %program, ptr %arg_0, i32 0, i32 58
  call ccc void @eclair_btree_upper_bound_6(ptr %1347, ptr %stack.ptr_386, ptr %stack.ptr_388)
  br label %loop_85
loop_85:
  %1348 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_387, ptr %stack.ptr_388)
  br i1 %1348, label %if_98, label %end_if_98
if_98:
  br label %range_query.end_84
end_if_98:
  %1349 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_387)
  %1350 = getelementptr [1 x i32], ptr %stack.ptr_389, i32 0, i32 0
  %1351 = getelementptr [1 x i32], ptr %1349, i32 0, i32 0
  %1352 = load i32, ptr %1351
  store i32 %1352, ptr %1350
  %1353 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1354 = call ccc i1 @eclair_btree_insert_value_6(ptr %1353, ptr %stack.ptr_389)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_387)
  br label %loop_85
range_query.end_84:
  %1355 = getelementptr [2 x i32], ptr %stack.ptr_390, i32 0, i32 0
  store i32 0, ptr %1355
  %1356 = getelementptr [2 x i32], ptr %stack.ptr_390, i32 0, i32 1
  store i32 0, ptr %1356
  %1357 = getelementptr [2 x i32], ptr %stack.ptr_391, i32 0, i32 0
  store i32 4294967295, ptr %1357
  %1358 = getelementptr [2 x i32], ptr %stack.ptr_391, i32 0, i32 1
  store i32 4294967295, ptr %1358
  %1359 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %1359, ptr %stack.ptr_390, ptr %stack.ptr_392)
  %1360 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %1360, ptr %stack.ptr_391, ptr %stack.ptr_393)
  br label %loop_86
loop_86:
  %1361 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_392, ptr %stack.ptr_393)
  br i1 %1361, label %if_99, label %end_if_99
if_99:
  br label %range_query.end_85
end_if_99:
  %1362 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_392)
  %1363 = getelementptr [2 x i32], ptr %stack.ptr_394, i32 0, i32 0
  %1364 = getelementptr [2 x i32], ptr %1362, i32 0, i32 1
  %1365 = load i32, ptr %1364
  store i32 %1365, ptr %1363
  %1366 = getelementptr [2 x i32], ptr %stack.ptr_394, i32 0, i32 1
  store i32 0, ptr %1366
  %1367 = getelementptr [2 x i32], ptr %stack.ptr_395, i32 0, i32 0
  %1368 = getelementptr [2 x i32], ptr %1362, i32 0, i32 1
  %1369 = load i32, ptr %1368
  store i32 %1369, ptr %1367
  %1370 = getelementptr [2 x i32], ptr %stack.ptr_395, i32 0, i32 1
  store i32 4294967295, ptr %1370
  %1371 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1371, ptr %stack.ptr_394, ptr %stack.ptr_396)
  %1372 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1372, ptr %stack.ptr_395, ptr %stack.ptr_397)
  br label %loop_87
loop_87:
  %1373 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_396, ptr %stack.ptr_397)
  br i1 %1373, label %if_100, label %end_if_100
if_100:
  br label %range_query.end_86
end_if_100:
  %1374 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_396)
  %1375 = getelementptr [1 x i32], ptr %stack.ptr_398, i32 0, i32 0
  %1376 = getelementptr [2 x i32], ptr %1374, i32 0, i32 1
  %1377 = load i32, ptr %1376
  store i32 %1377, ptr %1375
  %1378 = getelementptr %program, ptr %arg_0, i32 0, i32 32
  %1379 = call ccc i1 @eclair_btree_contains_6(ptr %1378, ptr %stack.ptr_398)
  %1380 = select i1 %1379, i1 0, i1 1
  br i1 %1380, label %if_101, label %end_if_101
if_101:
  %1381 = getelementptr [1 x i32], ptr %stack.ptr_399, i32 0, i32 0
  %1382 = getelementptr [2 x i32], ptr %1374, i32 0, i32 1
  %1383 = load i32, ptr %1382
  store i32 %1383, ptr %1381
  %1384 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  %1385 = call ccc i1 @eclair_btree_insert_value_6(ptr %1384, ptr %stack.ptr_399)
  br label %end_if_101
end_if_101:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_396)
  br label %loop_87
range_query.end_86:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_392)
  br label %loop_86
range_query.end_85:
  %1386 = getelementptr [1 x i32], ptr %stack.ptr_400, i32 0, i32 0
  store i32 0, ptr %1386
  %1387 = getelementptr [1 x i32], ptr %stack.ptr_401, i32 0, i32 0
  store i32 4294967295, ptr %1387
  %1388 = getelementptr %program, ptr %arg_0, i32 0, i32 32
  call ccc void @eclair_btree_lower_bound_6(ptr %1388, ptr %stack.ptr_400, ptr %stack.ptr_402)
  %1389 = getelementptr %program, ptr %arg_0, i32 0, i32 32
  call ccc void @eclair_btree_upper_bound_6(ptr %1389, ptr %stack.ptr_401, ptr %stack.ptr_403)
  br label %loop_88
loop_88:
  %1390 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_402, ptr %stack.ptr_403)
  br i1 %1390, label %if_102, label %end_if_102
if_102:
  br label %range_query.end_87
end_if_102:
  %1391 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_402)
  %1392 = getelementptr [1 x i32], ptr %stack.ptr_404, i32 0, i32 0
  %1393 = getelementptr [1 x i32], ptr %1391, i32 0, i32 0
  %1394 = load i32, ptr %1393
  store i32 %1394, ptr %1392
  %1395 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  %1396 = call ccc i1 @eclair_btree_insert_value_6(ptr %1395, ptr %stack.ptr_404)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_402)
  br label %loop_88
range_query.end_87:
  %1397 = getelementptr [1 x i32], ptr %stack.ptr_405, i32 0, i32 0
  store i32 0, ptr %1397
  %1398 = getelementptr [1 x i32], ptr %stack.ptr_406, i32 0, i32 0
  store i32 4294967295, ptr %1398
  %1399 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_lower_bound_6(ptr %1399, ptr %stack.ptr_405, ptr %stack.ptr_407)
  %1400 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_upper_bound_6(ptr %1400, ptr %stack.ptr_406, ptr %stack.ptr_408)
  br label %loop_89
loop_89:
  %1401 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_407, ptr %stack.ptr_408)
  br i1 %1401, label %if_103, label %end_if_103
if_103:
  br label %range_query.end_88
end_if_103:
  %1402 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_407)
  %1403 = getelementptr [1 x i32], ptr %stack.ptr_409, i32 0, i32 0
  %1404 = getelementptr [1 x i32], ptr %1402, i32 0, i32 0
  %1405 = load i32, ptr %1404
  store i32 %1405, ptr %1403
  %1406 = getelementptr [1 x i32], ptr %stack.ptr_410, i32 0, i32 0
  %1407 = getelementptr [1 x i32], ptr %1402, i32 0, i32 0
  %1408 = load i32, ptr %1407
  store i32 %1408, ptr %1406
  %1409 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_lower_bound_6(ptr %1409, ptr %stack.ptr_409, ptr %stack.ptr_411)
  %1410 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_upper_bound_6(ptr %1410, ptr %stack.ptr_410, ptr %stack.ptr_412)
  br label %loop_90
loop_90:
  %1411 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_411, ptr %stack.ptr_412)
  br i1 %1411, label %if_104, label %end_if_104
if_104:
  br label %range_query.end_89
end_if_104:
  %1412 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_411)
  %1413 = getelementptr [1 x i32], ptr %stack.ptr_413, i32 0, i32 0
  %1414 = getelementptr [1 x i32], ptr %1402, i32 0, i32 0
  %1415 = load i32, ptr %1414
  store i32 %1415, ptr %1413
  %1416 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1417 = call ccc i1 @eclair_btree_insert_value_6(ptr %1416, ptr %stack.ptr_413)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_411)
  br label %loop_90
range_query.end_89:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_407)
  br label %loop_89
range_query.end_88:
  %1418 = getelementptr [2 x i32], ptr %stack.ptr_414, i32 0, i32 0
  store i32 0, ptr %1418
  %1419 = getelementptr [2 x i32], ptr %stack.ptr_414, i32 0, i32 1
  store i32 0, ptr %1419
  %1420 = getelementptr [2 x i32], ptr %stack.ptr_415, i32 0, i32 0
  store i32 4294967295, ptr %1420
  %1421 = getelementptr [2 x i32], ptr %stack.ptr_415, i32 0, i32 1
  store i32 4294967295, ptr %1421
  %1422 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1422, ptr %stack.ptr_414, ptr %stack.ptr_416)
  %1423 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1423, ptr %stack.ptr_415, ptr %stack.ptr_417)
  br label %loop_91
loop_91:
  %1424 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_416, ptr %stack.ptr_417)
  br i1 %1424, label %if_105, label %end_if_105
if_105:
  br label %range_query.end_90
end_if_105:
  %1425 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_416)
  %1426 = getelementptr [3 x i32], ptr %stack.ptr_418, i32 0, i32 0
  %1427 = getelementptr [2 x i32], ptr %1425, i32 0, i32 0
  %1428 = load i32, ptr %1427
  store i32 %1428, ptr %1426
  %1429 = getelementptr [3 x i32], ptr %stack.ptr_418, i32 0, i32 1
  store i32 0, ptr %1429
  %1430 = getelementptr [3 x i32], ptr %stack.ptr_418, i32 0, i32 2
  store i32 0, ptr %1430
  %1431 = getelementptr [3 x i32], ptr %stack.ptr_419, i32 0, i32 0
  %1432 = getelementptr [2 x i32], ptr %1425, i32 0, i32 0
  %1433 = load i32, ptr %1432
  store i32 %1433, ptr %1431
  %1434 = getelementptr [3 x i32], ptr %stack.ptr_419, i32 0, i32 1
  store i32 4294967295, ptr %1434
  %1435 = getelementptr [3 x i32], ptr %stack.ptr_419, i32 0, i32 2
  store i32 4294967295, ptr %1435
  %1436 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1436, ptr %stack.ptr_418, ptr %stack.ptr_420)
  %1437 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1437, ptr %stack.ptr_419, ptr %stack.ptr_421)
  br label %loop_92
loop_92:
  %1438 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_420, ptr %stack.ptr_421)
  br i1 %1438, label %if_106, label %end_if_106
if_106:
  br label %range_query.end_91
end_if_106:
  %1439 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_420)
  %1440 = getelementptr [2 x i32], ptr %stack.ptr_422, i32 0, i32 0
  %1441 = getelementptr [3 x i32], ptr %1439, i32 0, i32 2
  %1442 = load i32, ptr %1441
  store i32 %1442, ptr %1440
  %1443 = getelementptr [2 x i32], ptr %stack.ptr_422, i32 0, i32 1
  store i32 0, ptr %1443
  %1444 = getelementptr [2 x i32], ptr %stack.ptr_423, i32 0, i32 0
  %1445 = getelementptr [3 x i32], ptr %1439, i32 0, i32 2
  %1446 = load i32, ptr %1445
  store i32 %1446, ptr %1444
  %1447 = getelementptr [2 x i32], ptr %stack.ptr_423, i32 0, i32 1
  store i32 4294967295, ptr %1447
  %1448 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %1448, ptr %stack.ptr_422, ptr %stack.ptr_424)
  %1449 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %1449, ptr %stack.ptr_423, ptr %stack.ptr_425)
  br label %loop_93
loop_93:
  %1450 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_424, ptr %stack.ptr_425)
  br i1 %1450, label %if_107, label %end_if_107
if_107:
  br label %range_query.end_92
end_if_107:
  %1451 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_424)
  %1452 = getelementptr [2 x i32], ptr %stack.ptr_426, i32 0, i32 0
  %1453 = getelementptr [2 x i32], ptr %1451, i32 0, i32 1
  %1454 = load i32, ptr %1453
  store i32 %1454, ptr %1452
  %1455 = getelementptr [2 x i32], ptr %stack.ptr_426, i32 0, i32 1
  store i32 0, ptr %1455
  %1456 = getelementptr [2 x i32], ptr %stack.ptr_427, i32 0, i32 0
  %1457 = getelementptr [2 x i32], ptr %1451, i32 0, i32 1
  %1458 = load i32, ptr %1457
  store i32 %1458, ptr %1456
  %1459 = getelementptr [2 x i32], ptr %stack.ptr_427, i32 0, i32 1
  store i32 4294967295, ptr %1459
  %1460 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1460, ptr %stack.ptr_426, ptr %stack.ptr_428)
  %1461 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1461, ptr %stack.ptr_427, ptr %stack.ptr_429)
  br label %loop_94
loop_94:
  %1462 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_428, ptr %stack.ptr_429)
  br i1 %1462, label %if_108, label %end_if_108
if_108:
  br label %range_query.end_93
end_if_108:
  %1463 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_428)
  %1464 = getelementptr [2 x i32], ptr %stack.ptr_430, i32 0, i32 0
  %1465 = getelementptr [2 x i32], ptr %1425, i32 0, i32 1
  %1466 = load i32, ptr %1465
  store i32 %1466, ptr %1464
  %1467 = getelementptr [2 x i32], ptr %stack.ptr_430, i32 0, i32 1
  %1468 = getelementptr [2 x i32], ptr %1463, i32 0, i32 1
  %1469 = load i32, ptr %1468
  store i32 %1469, ptr %1467
  %1470 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  %1471 = call ccc i1 @eclair_btree_insert_value_1(ptr %1470, ptr %stack.ptr_430)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_428)
  br label %loop_94
range_query.end_93:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_424)
  br label %loop_93
range_query.end_92:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_420)
  br label %loop_92
range_query.end_91:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_416)
  br label %loop_91
range_query.end_90:
  %1472 = getelementptr [2 x i32], ptr %stack.ptr_431, i32 0, i32 0
  store i32 0, ptr %1472
  %1473 = getelementptr [2 x i32], ptr %stack.ptr_431, i32 0, i32 1
  store i32 0, ptr %1473
  %1474 = getelementptr [2 x i32], ptr %stack.ptr_432, i32 0, i32 0
  store i32 4294967295, ptr %1474
  %1475 = getelementptr [2 x i32], ptr %stack.ptr_432, i32 0, i32 1
  store i32 4294967295, ptr %1475
  %1476 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1476, ptr %stack.ptr_431, ptr %stack.ptr_433)
  %1477 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1477, ptr %stack.ptr_432, ptr %stack.ptr_434)
  br label %loop_95
loop_95:
  %1478 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_433, ptr %stack.ptr_434)
  br i1 %1478, label %if_109, label %end_if_109
if_109:
  br label %range_query.end_94
end_if_109:
  %1479 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_433)
  %1480 = getelementptr [3 x i32], ptr %stack.ptr_435, i32 0, i32 0
  %1481 = getelementptr [2 x i32], ptr %1479, i32 0, i32 0
  %1482 = load i32, ptr %1481
  store i32 %1482, ptr %1480
  %1483 = getelementptr [3 x i32], ptr %stack.ptr_435, i32 0, i32 1
  store i32 0, ptr %1483
  %1484 = getelementptr [3 x i32], ptr %stack.ptr_435, i32 0, i32 2
  store i32 0, ptr %1484
  %1485 = getelementptr [3 x i32], ptr %stack.ptr_436, i32 0, i32 0
  %1486 = getelementptr [2 x i32], ptr %1479, i32 0, i32 0
  %1487 = load i32, ptr %1486
  store i32 %1487, ptr %1485
  %1488 = getelementptr [3 x i32], ptr %stack.ptr_436, i32 0, i32 1
  store i32 4294967295, ptr %1488
  %1489 = getelementptr [3 x i32], ptr %stack.ptr_436, i32 0, i32 2
  store i32 4294967295, ptr %1489
  %1490 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1490, ptr %stack.ptr_435, ptr %stack.ptr_437)
  %1491 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1491, ptr %stack.ptr_436, ptr %stack.ptr_438)
  br label %loop_96
loop_96:
  %1492 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_437, ptr %stack.ptr_438)
  br i1 %1492, label %if_110, label %end_if_110
if_110:
  br label %range_query.end_95
end_if_110:
  %1493 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_437)
  %1494 = getelementptr [2 x i32], ptr %stack.ptr_439, i32 0, i32 0
  %1495 = getelementptr [3 x i32], ptr %1493, i32 0, i32 2
  %1496 = load i32, ptr %1495
  store i32 %1496, ptr %1494
  %1497 = getelementptr [2 x i32], ptr %stack.ptr_439, i32 0, i32 1
  store i32 0, ptr %1497
  %1498 = getelementptr [2 x i32], ptr %stack.ptr_440, i32 0, i32 0
  %1499 = getelementptr [3 x i32], ptr %1493, i32 0, i32 2
  %1500 = load i32, ptr %1499
  store i32 %1500, ptr %1498
  %1501 = getelementptr [2 x i32], ptr %stack.ptr_440, i32 0, i32 1
  store i32 4294967295, ptr %1501
  %1502 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1502, ptr %stack.ptr_439, ptr %stack.ptr_441)
  %1503 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1503, ptr %stack.ptr_440, ptr %stack.ptr_442)
  br label %loop_97
loop_97:
  %1504 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_441, ptr %stack.ptr_442)
  br i1 %1504, label %if_111, label %end_if_111
if_111:
  br label %range_query.end_96
end_if_111:
  %1505 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_441)
  %1506 = getelementptr [2 x i32], ptr %stack.ptr_443, i32 0, i32 0
  %1507 = getelementptr [2 x i32], ptr %1479, i32 0, i32 1
  %1508 = load i32, ptr %1507
  store i32 %1508, ptr %1506
  %1509 = getelementptr [2 x i32], ptr %stack.ptr_443, i32 0, i32 1
  %1510 = getelementptr [2 x i32], ptr %1505, i32 0, i32 1
  %1511 = load i32, ptr %1510
  store i32 %1511, ptr %1509
  %1512 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  %1513 = call ccc i1 @eclair_btree_insert_value_1(ptr %1512, ptr %stack.ptr_443)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_441)
  br label %loop_97
range_query.end_96:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_437)
  br label %loop_96
range_query.end_95:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_433)
  br label %loop_95
range_query.end_94:
  %1514 = getelementptr [2 x i32], ptr %stack.ptr_444, i32 0, i32 0
  store i32 0, ptr %1514
  %1515 = getelementptr [2 x i32], ptr %stack.ptr_444, i32 0, i32 1
  store i32 0, ptr %1515
  %1516 = getelementptr [2 x i32], ptr %stack.ptr_445, i32 0, i32 0
  store i32 4294967295, ptr %1516
  %1517 = getelementptr [2 x i32], ptr %stack.ptr_445, i32 0, i32 1
  store i32 4294967295, ptr %1517
  %1518 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %1518, ptr %stack.ptr_444, ptr %stack.ptr_446)
  %1519 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %1519, ptr %stack.ptr_445, ptr %stack.ptr_447)
  br label %loop_98
loop_98:
  %1520 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_446, ptr %stack.ptr_447)
  br i1 %1520, label %if_112, label %end_if_112
if_112:
  br label %range_query.end_97
end_if_112:
  %1521 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_446)
  %1522 = getelementptr [2 x i32], ptr %stack.ptr_448, i32 0, i32 0
  store i32 0, ptr %1522
  %1523 = getelementptr [2 x i32], ptr %stack.ptr_448, i32 0, i32 1
  %1524 = getelementptr [2 x i32], ptr %1521, i32 0, i32 1
  %1525 = load i32, ptr %1524
  store i32 %1525, ptr %1523
  %1526 = getelementptr [2 x i32], ptr %stack.ptr_449, i32 0, i32 0
  store i32 4294967295, ptr %1526
  %1527 = getelementptr [2 x i32], ptr %stack.ptr_449, i32 0, i32 1
  %1528 = getelementptr [2 x i32], ptr %1521, i32 0, i32 1
  %1529 = load i32, ptr %1528
  store i32 %1529, ptr %1527
  %1530 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_lower_bound_2(ptr %1530, ptr %stack.ptr_448, ptr %stack.ptr_450)
  %1531 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_upper_bound_2(ptr %1531, ptr %stack.ptr_449, ptr %stack.ptr_451)
  br label %loop_99
loop_99:
  %1532 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_450, ptr %stack.ptr_451)
  br i1 %1532, label %if_113, label %end_if_113
if_113:
  br label %range_query.end_98
end_if_113:
  %1533 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_450)
  %1534 = getelementptr [2 x i32], ptr %stack.ptr_452, i32 0, i32 0
  %1535 = getelementptr [2 x i32], ptr %1533, i32 0, i32 0
  %1536 = load i32, ptr %1535
  store i32 %1536, ptr %1534
  %1537 = getelementptr [2 x i32], ptr %stack.ptr_452, i32 0, i32 1
  %1538 = getelementptr [2 x i32], ptr %1521, i32 0, i32 1
  %1539 = load i32, ptr %1538
  store i32 %1539, ptr %1537
  %1540 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  %1541 = call ccc i1 @eclair_btree_insert_value_1(ptr %1540, ptr %stack.ptr_452)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_450)
  br label %loop_99
range_query.end_98:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_446)
  br label %loop_98
range_query.end_97:
  %1542 = getelementptr [2 x i32], ptr %stack.ptr_453, i32 0, i32 0
  store i32 0, ptr %1542
  %1543 = getelementptr [2 x i32], ptr %stack.ptr_453, i32 0, i32 1
  store i32 0, ptr %1543
  %1544 = getelementptr [2 x i32], ptr %stack.ptr_454, i32 0, i32 0
  store i32 4294967295, ptr %1544
  %1545 = getelementptr [2 x i32], ptr %stack.ptr_454, i32 0, i32 1
  store i32 4294967295, ptr %1545
  %1546 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1546, ptr %stack.ptr_453, ptr %stack.ptr_455)
  %1547 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1547, ptr %stack.ptr_454, ptr %stack.ptr_456)
  br label %loop_100
loop_100:
  %1548 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_455, ptr %stack.ptr_456)
  br i1 %1548, label %if_114, label %end_if_114
if_114:
  br label %range_query.end_99
end_if_114:
  %1549 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_455)
  %1550 = getelementptr [3 x i32], ptr %stack.ptr_457, i32 0, i32 0
  %1551 = getelementptr [2 x i32], ptr %1549, i32 0, i32 0
  %1552 = load i32, ptr %1551
  store i32 %1552, ptr %1550
  %1553 = getelementptr [3 x i32], ptr %stack.ptr_457, i32 0, i32 1
  store i32 0, ptr %1553
  %1554 = getelementptr [3 x i32], ptr %stack.ptr_457, i32 0, i32 2
  store i32 0, ptr %1554
  %1555 = getelementptr [3 x i32], ptr %stack.ptr_458, i32 0, i32 0
  %1556 = getelementptr [2 x i32], ptr %1549, i32 0, i32 0
  %1557 = load i32, ptr %1556
  store i32 %1557, ptr %1555
  %1558 = getelementptr [3 x i32], ptr %stack.ptr_458, i32 0, i32 1
  store i32 4294967295, ptr %1558
  %1559 = getelementptr [3 x i32], ptr %stack.ptr_458, i32 0, i32 2
  store i32 4294967295, ptr %1559
  %1560 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1560, ptr %stack.ptr_457, ptr %stack.ptr_459)
  %1561 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1561, ptr %stack.ptr_458, ptr %stack.ptr_460)
  br label %loop_101
loop_101:
  %1562 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_459, ptr %stack.ptr_460)
  br i1 %1562, label %if_115, label %end_if_115
if_115:
  br label %range_query.end_100
end_if_115:
  %1563 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_459)
  %1564 = getelementptr [3 x i32], ptr %stack.ptr_461, i32 0, i32 0
  %1565 = getelementptr [3 x i32], ptr %1563, i32 0, i32 2
  %1566 = load i32, ptr %1565
  store i32 %1566, ptr %1564
  %1567 = getelementptr [3 x i32], ptr %stack.ptr_461, i32 0, i32 1
  store i32 0, ptr %1567
  %1568 = getelementptr [3 x i32], ptr %stack.ptr_461, i32 0, i32 2
  store i32 0, ptr %1568
  %1569 = getelementptr [3 x i32], ptr %stack.ptr_462, i32 0, i32 0
  %1570 = getelementptr [3 x i32], ptr %1563, i32 0, i32 2
  %1571 = load i32, ptr %1570
  store i32 %1571, ptr %1569
  %1572 = getelementptr [3 x i32], ptr %stack.ptr_462, i32 0, i32 1
  store i32 4294967295, ptr %1572
  %1573 = getelementptr [3 x i32], ptr %stack.ptr_462, i32 0, i32 2
  store i32 4294967295, ptr %1573
  %1574 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %1574, ptr %stack.ptr_461, ptr %stack.ptr_463)
  %1575 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %1575, ptr %stack.ptr_462, ptr %stack.ptr_464)
  br label %loop_102
loop_102:
  %1576 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_463, ptr %stack.ptr_464)
  br i1 %1576, label %if_116, label %end_if_116
if_116:
  br label %range_query.end_101
end_if_116:
  %1577 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_463)
  %1578 = getelementptr [2 x i32], ptr %stack.ptr_465, i32 0, i32 0
  %1579 = getelementptr [3 x i32], ptr %1577, i32 0, i32 1
  %1580 = load i32, ptr %1579
  store i32 %1580, ptr %1578
  %1581 = getelementptr [2 x i32], ptr %stack.ptr_465, i32 0, i32 1
  store i32 0, ptr %1581
  %1582 = getelementptr [2 x i32], ptr %stack.ptr_466, i32 0, i32 0
  %1583 = getelementptr [3 x i32], ptr %1577, i32 0, i32 1
  %1584 = load i32, ptr %1583
  store i32 %1584, ptr %1582
  %1585 = getelementptr [2 x i32], ptr %stack.ptr_466, i32 0, i32 1
  store i32 4294967295, ptr %1585
  %1586 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_lower_bound_1(ptr %1586, ptr %stack.ptr_465, ptr %stack.ptr_467)
  %1587 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_upper_bound_1(ptr %1587, ptr %stack.ptr_466, ptr %stack.ptr_468)
  br label %loop_103
loop_103:
  %1588 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_467, ptr %stack.ptr_468)
  br i1 %1588, label %if_117, label %end_if_117
if_117:
  br label %range_query.end_102
end_if_117:
  %1589 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_467)
  %1590 = getelementptr [2 x i32], ptr %stack.ptr_469, i32 0, i32 0
  %1591 = getelementptr [2 x i32], ptr %1549, i32 0, i32 1
  %1592 = load i32, ptr %1591
  store i32 %1592, ptr %1590
  %1593 = getelementptr [2 x i32], ptr %stack.ptr_469, i32 0, i32 1
  %1594 = getelementptr [2 x i32], ptr %1589, i32 0, i32 1
  %1595 = load i32, ptr %1594
  store i32 %1595, ptr %1593
  %1596 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  %1597 = call ccc i1 @eclair_btree_insert_value_1(ptr %1596, ptr %stack.ptr_469)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_467)
  br label %loop_103
range_query.end_102:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_463)
  br label %loop_102
range_query.end_101:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_459)
  br label %loop_101
range_query.end_100:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_455)
  br label %loop_100
range_query.end_99:
  %1598 = getelementptr [2 x i32], ptr %stack.ptr_470, i32 0, i32 0
  store i32 0, ptr %1598
  %1599 = getelementptr [2 x i32], ptr %stack.ptr_470, i32 0, i32 1
  store i32 0, ptr %1599
  %1600 = getelementptr [2 x i32], ptr %stack.ptr_471, i32 0, i32 0
  store i32 4294967295, ptr %1600
  %1601 = getelementptr [2 x i32], ptr %stack.ptr_471, i32 0, i32 1
  store i32 4294967295, ptr %1601
  %1602 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1602, ptr %stack.ptr_470, ptr %stack.ptr_472)
  %1603 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1603, ptr %stack.ptr_471, ptr %stack.ptr_473)
  br label %loop_104
loop_104:
  %1604 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_472, ptr %stack.ptr_473)
  br i1 %1604, label %if_118, label %end_if_118
if_118:
  br label %range_query.end_103
end_if_118:
  %1605 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_472)
  %1606 = getelementptr [3 x i32], ptr %stack.ptr_474, i32 0, i32 0
  %1607 = getelementptr [2 x i32], ptr %1605, i32 0, i32 0
  %1608 = load i32, ptr %1607
  store i32 %1608, ptr %1606
  %1609 = getelementptr [3 x i32], ptr %stack.ptr_474, i32 0, i32 1
  store i32 0, ptr %1609
  %1610 = getelementptr [3 x i32], ptr %stack.ptr_474, i32 0, i32 2
  store i32 0, ptr %1610
  %1611 = getelementptr [3 x i32], ptr %stack.ptr_475, i32 0, i32 0
  %1612 = getelementptr [2 x i32], ptr %1605, i32 0, i32 0
  %1613 = load i32, ptr %1612
  store i32 %1613, ptr %1611
  %1614 = getelementptr [3 x i32], ptr %stack.ptr_475, i32 0, i32 1
  store i32 4294967295, ptr %1614
  %1615 = getelementptr [3 x i32], ptr %stack.ptr_475, i32 0, i32 2
  store i32 4294967295, ptr %1615
  %1616 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1616, ptr %stack.ptr_474, ptr %stack.ptr_476)
  %1617 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1617, ptr %stack.ptr_475, ptr %stack.ptr_477)
  br label %loop_105
loop_105:
  %1618 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_476, ptr %stack.ptr_477)
  br i1 %1618, label %if_119, label %end_if_119
if_119:
  br label %range_query.end_104
end_if_119:
  %1619 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_476)
  %1620 = getelementptr [3 x i32], ptr %stack.ptr_478, i32 0, i32 0
  %1621 = getelementptr [3 x i32], ptr %1619, i32 0, i32 2
  %1622 = load i32, ptr %1621
  store i32 %1622, ptr %1620
  %1623 = getelementptr [3 x i32], ptr %stack.ptr_478, i32 0, i32 1
  store i32 0, ptr %1623
  %1624 = getelementptr [3 x i32], ptr %stack.ptr_478, i32 0, i32 2
  store i32 0, ptr %1624
  %1625 = getelementptr [3 x i32], ptr %stack.ptr_479, i32 0, i32 0
  %1626 = getelementptr [3 x i32], ptr %1619, i32 0, i32 2
  %1627 = load i32, ptr %1626
  store i32 %1627, ptr %1625
  %1628 = getelementptr [3 x i32], ptr %stack.ptr_479, i32 0, i32 1
  store i32 4294967295, ptr %1628
  %1629 = getelementptr [3 x i32], ptr %stack.ptr_479, i32 0, i32 2
  store i32 4294967295, ptr %1629
  %1630 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %1630, ptr %stack.ptr_478, ptr %stack.ptr_480)
  %1631 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %1631, ptr %stack.ptr_479, ptr %stack.ptr_481)
  br label %loop_106
loop_106:
  %1632 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_480, ptr %stack.ptr_481)
  br i1 %1632, label %if_120, label %end_if_120
if_120:
  br label %range_query.end_105
end_if_120:
  %1633 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_480)
  %1634 = getelementptr [2 x i32], ptr %stack.ptr_482, i32 0, i32 0
  %1635 = getelementptr [3 x i32], ptr %1633, i32 0, i32 2
  %1636 = load i32, ptr %1635
  store i32 %1636, ptr %1634
  %1637 = getelementptr [2 x i32], ptr %stack.ptr_482, i32 0, i32 1
  store i32 0, ptr %1637
  %1638 = getelementptr [2 x i32], ptr %stack.ptr_483, i32 0, i32 0
  %1639 = getelementptr [3 x i32], ptr %1633, i32 0, i32 2
  %1640 = load i32, ptr %1639
  store i32 %1640, ptr %1638
  %1641 = getelementptr [2 x i32], ptr %stack.ptr_483, i32 0, i32 1
  store i32 4294967295, ptr %1641
  %1642 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_lower_bound_1(ptr %1642, ptr %stack.ptr_482, ptr %stack.ptr_484)
  %1643 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_upper_bound_1(ptr %1643, ptr %stack.ptr_483, ptr %stack.ptr_485)
  br label %loop_107
loop_107:
  %1644 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_484, ptr %stack.ptr_485)
  br i1 %1644, label %if_121, label %end_if_121
if_121:
  br label %range_query.end_106
end_if_121:
  %1645 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_484)
  %1646 = getelementptr [2 x i32], ptr %stack.ptr_486, i32 0, i32 0
  %1647 = getelementptr [2 x i32], ptr %1605, i32 0, i32 1
  %1648 = load i32, ptr %1647
  store i32 %1648, ptr %1646
  %1649 = getelementptr [2 x i32], ptr %stack.ptr_486, i32 0, i32 1
  %1650 = getelementptr [2 x i32], ptr %1645, i32 0, i32 1
  %1651 = load i32, ptr %1650
  store i32 %1651, ptr %1649
  %1652 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  %1653 = call ccc i1 @eclair_btree_insert_value_1(ptr %1652, ptr %stack.ptr_486)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_484)
  br label %loop_107
range_query.end_106:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_480)
  br label %loop_106
range_query.end_105:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_476)
  br label %loop_105
range_query.end_104:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_472)
  br label %loop_104
range_query.end_103:
  %1654 = getelementptr [2 x i32], ptr %stack.ptr_487, i32 0, i32 0
  store i32 0, ptr %1654
  %1655 = getelementptr [2 x i32], ptr %stack.ptr_487, i32 0, i32 1
  store i32 0, ptr %1655
  %1656 = getelementptr [2 x i32], ptr %stack.ptr_488, i32 0, i32 0
  store i32 4294967295, ptr %1656
  %1657 = getelementptr [2 x i32], ptr %stack.ptr_488, i32 0, i32 1
  store i32 4294967295, ptr %1657
  %1658 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_lower_bound_1(ptr %1658, ptr %stack.ptr_487, ptr %stack.ptr_489)
  %1659 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_upper_bound_1(ptr %1659, ptr %stack.ptr_488, ptr %stack.ptr_490)
  br label %loop_108
loop_108:
  %1660 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_489, ptr %stack.ptr_490)
  br i1 %1660, label %if_122, label %end_if_122
if_122:
  br label %range_query.end_107
end_if_122:
  %1661 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_489)
  %1662 = getelementptr [2 x i32], ptr %stack.ptr_491, i32 0, i32 0
  %1663 = getelementptr [2 x i32], ptr %1661, i32 0, i32 0
  %1664 = load i32, ptr %1663
  store i32 %1664, ptr %1662
  %1665 = getelementptr [2 x i32], ptr %stack.ptr_491, i32 0, i32 1
  %1666 = getelementptr [2 x i32], ptr %1661, i32 0, i32 1
  %1667 = load i32, ptr %1666
  store i32 %1667, ptr %1665
  %1668 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  %1669 = call ccc i1 @eclair_btree_insert_value_1(ptr %1668, ptr %stack.ptr_491)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_489)
  br label %loop_108
range_query.end_107:
  %1670 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_begin_1(ptr %1670, ptr %stack.ptr_492)
  %1671 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_end_1(ptr %1671, ptr %stack.ptr_493)
  %1672 = getelementptr %program, ptr %arg_0, i32 0, i32 20
  call ccc void @eclair_btree_insert_range_delta_transitive_depends_on_transitive_depends_on(ptr %1672, ptr %stack.ptr_492, ptr %stack.ptr_493)
  br label %loop_109
loop_109:
  %1673 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  call ccc void @eclair_btree_clear_1(ptr %1673)
  %1674 = getelementptr [2 x i32], ptr %stack.ptr_494, i32 0, i32 0
  store i32 0, ptr %1674
  %1675 = getelementptr [2 x i32], ptr %stack.ptr_494, i32 0, i32 1
  store i32 0, ptr %1675
  %1676 = getelementptr [2 x i32], ptr %stack.ptr_495, i32 0, i32 0
  store i32 4294967295, ptr %1676
  %1677 = getelementptr [2 x i32], ptr %stack.ptr_495, i32 0, i32 1
  store i32 4294967295, ptr %1677
  %1678 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_lower_bound_1(ptr %1678, ptr %stack.ptr_494, ptr %stack.ptr_496)
  %1679 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_upper_bound_1(ptr %1679, ptr %stack.ptr_495, ptr %stack.ptr_497)
  br label %loop_110
loop_110:
  %1680 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_496, ptr %stack.ptr_497)
  br i1 %1680, label %if_123, label %end_if_123
if_123:
  br label %range_query.end_108
end_if_123:
  %1681 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_496)
  %1682 = getelementptr [2 x i32], ptr %stack.ptr_498, i32 0, i32 0
  %1683 = getelementptr [2 x i32], ptr %1681, i32 0, i32 1
  %1684 = load i32, ptr %1683
  store i32 %1684, ptr %1682
  %1685 = getelementptr [2 x i32], ptr %stack.ptr_498, i32 0, i32 1
  store i32 0, ptr %1685
  %1686 = getelementptr [2 x i32], ptr %stack.ptr_499, i32 0, i32 0
  %1687 = getelementptr [2 x i32], ptr %1681, i32 0, i32 1
  %1688 = load i32, ptr %1687
  store i32 %1688, ptr %1686
  %1689 = getelementptr [2 x i32], ptr %stack.ptr_499, i32 0, i32 1
  store i32 4294967295, ptr %1689
  %1690 = getelementptr %program, ptr %arg_0, i32 0, i32 20
  call ccc void @eclair_btree_lower_bound_1(ptr %1690, ptr %stack.ptr_498, ptr %stack.ptr_500)
  %1691 = getelementptr %program, ptr %arg_0, i32 0, i32 20
  call ccc void @eclair_btree_upper_bound_1(ptr %1691, ptr %stack.ptr_499, ptr %stack.ptr_501)
  br label %loop_111
loop_111:
  %1692 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_500, ptr %stack.ptr_501)
  br i1 %1692, label %if_124, label %end_if_124
if_124:
  br label %range_query.end_109
end_if_124:
  %1693 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_500)
  %1694 = getelementptr [2 x i32], ptr %stack.ptr_502, i32 0, i32 0
  %1695 = getelementptr [2 x i32], ptr %1681, i32 0, i32 0
  %1696 = load i32, ptr %1695
  store i32 %1696, ptr %1694
  %1697 = getelementptr [2 x i32], ptr %stack.ptr_502, i32 0, i32 1
  %1698 = getelementptr [2 x i32], ptr %1693, i32 0, i32 1
  %1699 = load i32, ptr %1698
  store i32 %1699, ptr %1697
  %1700 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  %1701 = call ccc i1 @eclair_btree_contains_1(ptr %1700, ptr %stack.ptr_502)
  %1702 = select i1 %1701, i1 0, i1 1
  br i1 %1702, label %if_125, label %end_if_125
if_125:
  %1703 = getelementptr [2 x i32], ptr %stack.ptr_503, i32 0, i32 0
  %1704 = getelementptr [2 x i32], ptr %1681, i32 0, i32 0
  %1705 = load i32, ptr %1704
  store i32 %1705, ptr %1703
  %1706 = getelementptr [2 x i32], ptr %stack.ptr_503, i32 0, i32 1
  %1707 = getelementptr [2 x i32], ptr %1693, i32 0, i32 1
  %1708 = load i32, ptr %1707
  store i32 %1708, ptr %1706
  %1709 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  %1710 = call ccc i1 @eclair_btree_insert_value_1(ptr %1709, ptr %stack.ptr_503)
  br label %end_if_125
end_if_125:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_500)
  br label %loop_111
range_query.end_109:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_496)
  br label %loop_110
range_query.end_108:
  %1711 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  %1712 = call ccc i1 @eclair_btree_is_empty_1(ptr %1711)
  br i1 %1712, label %if_126, label %end_if_126
if_126:
  br label %loop.end_1
end_if_126:
  %1713 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  call ccc void @eclair_btree_begin_1(ptr %1713, ptr %stack.ptr_504)
  %1714 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  call ccc void @eclair_btree_end_1(ptr %1714, ptr %stack.ptr_505)
  %1715 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_insert_range_transitive_depends_on_new_transitive_depends_on(ptr %1715, ptr %stack.ptr_504, ptr %stack.ptr_505)
  %1716 = getelementptr %program, ptr %arg_0, i32 0, i32 45
  %1717 = getelementptr %program, ptr %arg_0, i32 0, i32 20
  call ccc void @eclair_btree_swap_1(ptr %1716, ptr %1717)
  br label %loop_109
loop.end_1:
  %1718 = getelementptr [2 x i32], ptr %stack.ptr_506, i32 0, i32 0
  store i32 0, ptr %1718
  %1719 = getelementptr [2 x i32], ptr %stack.ptr_506, i32 0, i32 1
  store i32 0, ptr %1719
  %1720 = getelementptr [2 x i32], ptr %stack.ptr_507, i32 0, i32 0
  store i32 4294967295, ptr %1720
  %1721 = getelementptr [2 x i32], ptr %stack.ptr_507, i32 0, i32 1
  store i32 4294967295, ptr %1721
  %1722 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1722, ptr %stack.ptr_506, ptr %stack.ptr_508)
  %1723 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1723, ptr %stack.ptr_507, ptr %stack.ptr_509)
  br label %loop_112
loop_112:
  %1724 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_508, ptr %stack.ptr_509)
  br i1 %1724, label %if_127, label %end_if_127
if_127:
  br label %range_query.end_110
end_if_127:
  %1725 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_508)
  %1726 = getelementptr [2 x i32], ptr %1725, i32 0, i32 0
  %1727 = load i32, ptr %1726
  %1728 = getelementptr [2 x i32], ptr %1725, i32 0, i32 1
  %1729 = load i32, ptr %1728
  %1730 = icmp eq i32 %1727, %1729
  br i1 %1730, label %if_128, label %end_if_128
if_128:
  %1731 = getelementptr [1 x i32], ptr %stack.ptr_510, i32 0, i32 0
  %1732 = getelementptr [2 x i32], ptr %1725, i32 0, i32 0
  %1733 = load i32, ptr %1732
  store i32 %1733, ptr %1731
  %1734 = getelementptr %program, ptr %arg_0, i32 0, i32 21
  %1735 = call ccc i1 @eclair_btree_insert_value_6(ptr %1734, ptr %stack.ptr_510)
  br label %end_if_128
end_if_128:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_508)
  br label %loop_112
range_query.end_110:
  %1736 = getelementptr [1 x i32], ptr %stack.ptr_511, i32 0, i32 0
  store i32 0, ptr %1736
  %1737 = getelementptr [1 x i32], ptr %stack.ptr_512, i32 0, i32 0
  store i32 4294967295, ptr %1737
  %1738 = getelementptr %program, ptr %arg_0, i32 0, i32 21
  call ccc void @eclair_btree_lower_bound_6(ptr %1738, ptr %stack.ptr_511, ptr %stack.ptr_513)
  %1739 = getelementptr %program, ptr %arg_0, i32 0, i32 21
  call ccc void @eclair_btree_upper_bound_6(ptr %1739, ptr %stack.ptr_512, ptr %stack.ptr_514)
  br label %loop_113
loop_113:
  %1740 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_513, ptr %stack.ptr_514)
  br i1 %1740, label %if_129, label %end_if_129
if_129:
  br label %range_query.end_111
end_if_129:
  %1741 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_513)
  %1742 = getelementptr [2 x i32], ptr %stack.ptr_515, i32 0, i32 0
  %1743 = getelementptr [1 x i32], ptr %1741, i32 0, i32 0
  %1744 = load i32, ptr %1743
  store i32 %1744, ptr %1742
  %1745 = getelementptr [2 x i32], ptr %stack.ptr_515, i32 0, i32 1
  store i32 0, ptr %1745
  %1746 = getelementptr [2 x i32], ptr %stack.ptr_516, i32 0, i32 0
  %1747 = getelementptr [1 x i32], ptr %1741, i32 0, i32 0
  %1748 = load i32, ptr %1747
  store i32 %1748, ptr %1746
  %1749 = getelementptr [2 x i32], ptr %stack.ptr_516, i32 0, i32 1
  store i32 4294967295, ptr %1749
  %1750 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1750, ptr %stack.ptr_515, ptr %stack.ptr_517)
  %1751 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1751, ptr %stack.ptr_516, ptr %stack.ptr_518)
  br label %loop_114
loop_114:
  %1752 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_517, ptr %stack.ptr_518)
  br i1 %1752, label %if_130, label %end_if_130
if_130:
  br label %range_query.end_112
end_if_130:
  %1753 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_517)
  %1754 = getelementptr [2 x i32], ptr %stack.ptr_519, i32 0, i32 0
  store i32 0, ptr %1754
  %1755 = getelementptr [2 x i32], ptr %stack.ptr_519, i32 0, i32 1
  %1756 = getelementptr [2 x i32], ptr %1753, i32 0, i32 1
  %1757 = load i32, ptr %1756
  store i32 %1757, ptr %1755
  %1758 = getelementptr [2 x i32], ptr %stack.ptr_520, i32 0, i32 0
  store i32 4294967295, ptr %1758
  %1759 = getelementptr [2 x i32], ptr %stack.ptr_520, i32 0, i32 1
  %1760 = getelementptr [2 x i32], ptr %1753, i32 0, i32 1
  %1761 = load i32, ptr %1760
  store i32 %1761, ptr %1759
  %1762 = getelementptr %program, ptr %arg_0, i32 0, i32 51
  call ccc void @eclair_btree_lower_bound_2(ptr %1762, ptr %stack.ptr_519, ptr %stack.ptr_521)
  %1763 = getelementptr %program, ptr %arg_0, i32 0, i32 51
  call ccc void @eclair_btree_upper_bound_2(ptr %1763, ptr %stack.ptr_520, ptr %stack.ptr_522)
  br label %loop_115
loop_115:
  %1764 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_521, ptr %stack.ptr_522)
  br i1 %1764, label %if_131, label %end_if_131
if_131:
  br label %range_query.end_113
end_if_131:
  %1765 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_521)
  %1766 = getelementptr [3 x i32], ptr %stack.ptr_523, i32 0, i32 0
  %1767 = getelementptr [2 x i32], ptr %1765, i32 0, i32 0
  %1768 = load i32, ptr %1767
  store i32 %1768, ptr %1766
  %1769 = getelementptr [3 x i32], ptr %stack.ptr_523, i32 0, i32 1
  store i32 0, ptr %1769
  %1770 = getelementptr [3 x i32], ptr %stack.ptr_523, i32 0, i32 2
  store i32 0, ptr %1770
  %1771 = getelementptr [3 x i32], ptr %stack.ptr_524, i32 0, i32 0
  %1772 = getelementptr [2 x i32], ptr %1765, i32 0, i32 0
  %1773 = load i32, ptr %1772
  store i32 %1773, ptr %1771
  %1774 = getelementptr [3 x i32], ptr %stack.ptr_524, i32 0, i32 1
  store i32 4294967295, ptr %1774
  %1775 = getelementptr [3 x i32], ptr %stack.ptr_524, i32 0, i32 2
  store i32 4294967295, ptr %1775
  %1776 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1776, ptr %stack.ptr_523, ptr %stack.ptr_525)
  %1777 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1777, ptr %stack.ptr_524, ptr %stack.ptr_526)
  br label %loop_116
loop_116:
  %1778 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_525, ptr %stack.ptr_526)
  br i1 %1778, label %if_132, label %end_if_132
if_132:
  br label %range_query.end_114
end_if_132:
  %1779 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_525)
  %1780 = getelementptr [2 x i32], ptr %stack.ptr_527, i32 0, i32 0
  %1781 = getelementptr [3 x i32], ptr %1779, i32 0, i32 2
  %1782 = load i32, ptr %1781
  store i32 %1782, ptr %1780
  %1783 = getelementptr [2 x i32], ptr %stack.ptr_527, i32 0, i32 1
  store i32 0, ptr %1783
  %1784 = getelementptr [2 x i32], ptr %stack.ptr_528, i32 0, i32 0
  %1785 = getelementptr [3 x i32], ptr %1779, i32 0, i32 2
  %1786 = load i32, ptr %1785
  store i32 %1786, ptr %1784
  %1787 = getelementptr [2 x i32], ptr %stack.ptr_528, i32 0, i32 1
  store i32 4294967295, ptr %1787
  %1788 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %1788, ptr %stack.ptr_527, ptr %stack.ptr_529)
  %1789 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %1789, ptr %stack.ptr_528, ptr %stack.ptr_530)
  br label %loop_117
loop_117:
  %1790 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_529, ptr %stack.ptr_530)
  br i1 %1790, label %if_133, label %end_if_133
if_133:
  br label %range_query.end_115
end_if_133:
  %1791 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_529)
  %1792 = getelementptr [2 x i32], ptr %stack.ptr_531, i32 0, i32 0
  %1793 = getelementptr [2 x i32], ptr %1791, i32 0, i32 1
  %1794 = load i32, ptr %1793
  store i32 %1794, ptr %1792
  %1795 = getelementptr [2 x i32], ptr %stack.ptr_531, i32 0, i32 1
  store i32 0, ptr %1795
  %1796 = getelementptr [2 x i32], ptr %stack.ptr_532, i32 0, i32 0
  %1797 = getelementptr [2 x i32], ptr %1791, i32 0, i32 1
  %1798 = load i32, ptr %1797
  store i32 %1798, ptr %1796
  %1799 = getelementptr [2 x i32], ptr %stack.ptr_532, i32 0, i32 1
  store i32 4294967295, ptr %1799
  %1800 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1800, ptr %stack.ptr_531, ptr %stack.ptr_533)
  %1801 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1801, ptr %stack.ptr_532, ptr %stack.ptr_534)
  br label %loop_118
loop_118:
  %1802 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_533, ptr %stack.ptr_534)
  br i1 %1802, label %if_134, label %end_if_134
if_134:
  br label %range_query.end_116
end_if_134:
  %1803 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_533)
  %1804 = getelementptr [2 x i32], ptr %stack.ptr_535, i32 0, i32 0
  %1805 = getelementptr [2 x i32], ptr %1803, i32 0, i32 1
  %1806 = load i32, ptr %1805
  store i32 %1806, ptr %1804
  %1807 = getelementptr [2 x i32], ptr %stack.ptr_535, i32 0, i32 1
  %1808 = getelementptr [1 x i32], ptr %1741, i32 0, i32 0
  %1809 = load i32, ptr %1808
  store i32 %1809, ptr %1807
  %1810 = getelementptr [2 x i32], ptr %stack.ptr_536, i32 0, i32 0
  %1811 = getelementptr [2 x i32], ptr %1803, i32 0, i32 1
  %1812 = load i32, ptr %1811
  store i32 %1812, ptr %1810
  %1813 = getelementptr [2 x i32], ptr %stack.ptr_536, i32 0, i32 1
  %1814 = getelementptr [1 x i32], ptr %1741, i32 0, i32 0
  %1815 = load i32, ptr %1814
  store i32 %1815, ptr %1813
  %1816 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1816, ptr %stack.ptr_535, ptr %stack.ptr_537)
  %1817 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1817, ptr %stack.ptr_536, ptr %stack.ptr_538)
  br label %loop_119
loop_119:
  %1818 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_537, ptr %stack.ptr_538)
  br i1 %1818, label %if_135, label %end_if_135
if_135:
  br label %range_query.end_117
end_if_135:
  %1819 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_537)
  %1820 = getelementptr [1 x i32], ptr %stack.ptr_539, i32 0, i32 0
  %1821 = getelementptr [3 x i32], ptr %1779, i32 0, i32 2
  %1822 = load i32, ptr %1821
  store i32 %1822, ptr %1820
  %1823 = getelementptr %program, ptr %arg_0, i32 0, i32 11
  %1824 = call ccc i1 @eclair_btree_insert_value_6(ptr %1823, ptr %stack.ptr_539)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_537)
  br label %loop_119
range_query.end_117:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_533)
  br label %loop_118
range_query.end_116:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_529)
  br label %loop_117
range_query.end_115:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_525)
  br label %loop_116
range_query.end_114:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_521)
  br label %loop_115
range_query.end_113:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_517)
  br label %loop_114
range_query.end_112:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_513)
  br label %loop_113
range_query.end_111:
  %1825 = getelementptr [1 x i32], ptr %stack.ptr_540, i32 0, i32 0
  store i32 0, ptr %1825
  %1826 = getelementptr [1 x i32], ptr %stack.ptr_541, i32 0, i32 0
  store i32 4294967295, ptr %1826
  %1827 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_lower_bound_6(ptr %1827, ptr %stack.ptr_540, ptr %stack.ptr_542)
  %1828 = getelementptr %program, ptr %arg_0, i32 0, i32 47
  call ccc void @eclair_btree_upper_bound_6(ptr %1828, ptr %stack.ptr_541, ptr %stack.ptr_543)
  br label %loop_120
loop_120:
  %1829 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_542, ptr %stack.ptr_543)
  br i1 %1829, label %if_136, label %end_if_136
if_136:
  br label %range_query.end_118
end_if_136:
  %1830 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_542)
  %1831 = getelementptr [2 x i32], ptr %stack.ptr_544, i32 0, i32 0
  %1832 = getelementptr [1 x i32], ptr %1830, i32 0, i32 0
  %1833 = load i32, ptr %1832
  store i32 %1833, ptr %1831
  %1834 = getelementptr [2 x i32], ptr %stack.ptr_544, i32 0, i32 1
  store i32 0, ptr %1834
  %1835 = getelementptr [2 x i32], ptr %stack.ptr_545, i32 0, i32 0
  %1836 = getelementptr [1 x i32], ptr %1830, i32 0, i32 0
  %1837 = load i32, ptr %1836
  store i32 %1837, ptr %1835
  %1838 = getelementptr [2 x i32], ptr %stack.ptr_545, i32 0, i32 1
  store i32 4294967295, ptr %1838
  %1839 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_lower_bound_1(ptr %1839, ptr %stack.ptr_544, ptr %stack.ptr_546)
  %1840 = getelementptr %program, ptr %arg_0, i32 0, i32 62
  call ccc void @eclair_btree_upper_bound_1(ptr %1840, ptr %stack.ptr_545, ptr %stack.ptr_547)
  br label %loop_121
loop_121:
  %1841 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_546, ptr %stack.ptr_547)
  br i1 %1841, label %if_137, label %end_if_137
if_137:
  br label %range_query.end_119
end_if_137:
  %1842 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_546)
  %1843 = getelementptr [1 x i32], ptr %stack.ptr_548, i32 0, i32 0
  %1844 = getelementptr [2 x i32], ptr %1842, i32 0, i32 1
  %1845 = load i32, ptr %1844
  store i32 %1845, ptr %1843
  %1846 = getelementptr [1 x i32], ptr %stack.ptr_549, i32 0, i32 0
  %1847 = getelementptr [2 x i32], ptr %1842, i32 0, i32 1
  %1848 = load i32, ptr %1847
  store i32 %1848, ptr %1846
  %1849 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_lower_bound_6(ptr %1849, ptr %stack.ptr_548, ptr %stack.ptr_550)
  %1850 = getelementptr %program, ptr %arg_0, i32 0, i32 61
  call ccc void @eclair_btree_upper_bound_6(ptr %1850, ptr %stack.ptr_549, ptr %stack.ptr_551)
  br label %loop_122
loop_122:
  %1851 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_550, ptr %stack.ptr_551)
  br i1 %1851, label %if_138, label %end_if_138
if_138:
  br label %range_query.end_120
end_if_138:
  %1852 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_550)
  %1853 = getelementptr [1 x i32], ptr %stack.ptr_552, i32 0, i32 0
  %1854 = getelementptr [1 x i32], ptr %1830, i32 0, i32 0
  %1855 = load i32, ptr %1854
  store i32 %1855, ptr %1853
  %1856 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1857 = call ccc i1 @eclair_btree_insert_value_6(ptr %1856, ptr %stack.ptr_552)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_550)
  br label %loop_122
range_query.end_120:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_546)
  br label %loop_121
range_query.end_119:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_542)
  br label %loop_120
range_query.end_118:
  %1858 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_begin_6(ptr %1858, ptr %stack.ptr_553)
  %1859 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_end_6(ptr %1859, ptr %stack.ptr_554)
  %1860 = getelementptr %program, ptr %arg_0, i32 0, i32 18
  call ccc void @eclair_btree_insert_range_delta_live_rule_live_rule(ptr %1860, ptr %stack.ptr_553, ptr %stack.ptr_554)
  br label %loop_123
loop_123:
  %1861 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  call ccc void @eclair_btree_clear_6(ptr %1861)
  %1862 = getelementptr [2 x i32], ptr %stack.ptr_555, i32 0, i32 0
  store i32 0, ptr %1862
  %1863 = getelementptr [2 x i32], ptr %stack.ptr_555, i32 0, i32 1
  store i32 0, ptr %1863
  %1864 = getelementptr [2 x i32], ptr %stack.ptr_556, i32 0, i32 0
  store i32 4294967295, ptr %1864
  %1865 = getelementptr [2 x i32], ptr %stack.ptr_556, i32 0, i32 1
  store i32 4294967295, ptr %1865
  %1866 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_lower_bound_1(ptr %1866, ptr %stack.ptr_555, ptr %stack.ptr_557)
  %1867 = getelementptr %program, ptr %arg_0, i32 0, i32 22
  call ccc void @eclair_btree_upper_bound_1(ptr %1867, ptr %stack.ptr_556, ptr %stack.ptr_558)
  br label %loop_124
loop_124:
  %1868 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_557, ptr %stack.ptr_558)
  br i1 %1868, label %if_139, label %end_if_139
if_139:
  br label %range_query.end_121
end_if_139:
  %1869 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_557)
  %1870 = getelementptr [1 x i32], ptr %stack.ptr_559, i32 0, i32 0
  %1871 = getelementptr [2 x i32], ptr %1869, i32 0, i32 1
  %1872 = load i32, ptr %1871
  store i32 %1872, ptr %1870
  %1873 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1874 = call ccc i1 @eclair_btree_contains_6(ptr %1873, ptr %stack.ptr_559)
  %1875 = select i1 %1874, i1 0, i1 1
  br i1 %1875, label %if_140, label %end_if_141
if_140:
  %1876 = getelementptr [1 x i32], ptr %stack.ptr_560, i32 0, i32 0
  %1877 = getelementptr [2 x i32], ptr %1869, i32 0, i32 0
  %1878 = load i32, ptr %1877
  store i32 %1878, ptr %1876
  %1879 = getelementptr [1 x i32], ptr %stack.ptr_561, i32 0, i32 0
  %1880 = getelementptr [2 x i32], ptr %1869, i32 0, i32 0
  %1881 = load i32, ptr %1880
  store i32 %1881, ptr %1879
  %1882 = getelementptr %program, ptr %arg_0, i32 0, i32 18
  call ccc void @eclair_btree_lower_bound_6(ptr %1882, ptr %stack.ptr_560, ptr %stack.ptr_562)
  %1883 = getelementptr %program, ptr %arg_0, i32 0, i32 18
  call ccc void @eclair_btree_upper_bound_6(ptr %1883, ptr %stack.ptr_561, ptr %stack.ptr_563)
  br label %loop_125
loop_125:
  %1884 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_562, ptr %stack.ptr_563)
  br i1 %1884, label %if_141, label %end_if_140
if_141:
  br label %range_query.end_122
end_if_140:
  %1885 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_562)
  %1886 = getelementptr [1 x i32], ptr %stack.ptr_564, i32 0, i32 0
  %1887 = getelementptr [2 x i32], ptr %1869, i32 0, i32 1
  %1888 = load i32, ptr %1887
  store i32 %1888, ptr %1886
  %1889 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  %1890 = call ccc i1 @eclair_btree_insert_value_6(ptr %1889, ptr %stack.ptr_564)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_562)
  br label %loop_125
range_query.end_122:
  br label %end_if_141
end_if_141:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_557)
  br label %loop_124
range_query.end_121:
  %1891 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  %1892 = call ccc i1 @eclair_btree_is_empty_6(ptr %1891)
  br i1 %1892, label %if_142, label %end_if_142
if_142:
  br label %loop.end_2
end_if_142:
  %1893 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  call ccc void @eclair_btree_begin_6(ptr %1893, ptr %stack.ptr_565)
  %1894 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  call ccc void @eclair_btree_end_6(ptr %1894, ptr %stack.ptr_566)
  %1895 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  call ccc void @eclair_btree_insert_range_live_rule_new_live_rule(ptr %1895, ptr %stack.ptr_565, ptr %stack.ptr_566)
  %1896 = getelementptr %program, ptr %arg_0, i32 0, i32 43
  %1897 = getelementptr %program, ptr %arg_0, i32 0, i32 18
  call ccc void @eclair_btree_swap_6(ptr %1896, ptr %1897)
  br label %loop_123
loop.end_2:
  %1898 = getelementptr [2 x i32], ptr %stack.ptr_567, i32 0, i32 0
  store i32 0, ptr %1898
  %1899 = getelementptr [2 x i32], ptr %stack.ptr_567, i32 0, i32 1
  store i32 0, ptr %1899
  %1900 = getelementptr [2 x i32], ptr %stack.ptr_568, i32 0, i32 0
  store i32 4294967295, ptr %1900
  %1901 = getelementptr [2 x i32], ptr %stack.ptr_568, i32 0, i32 1
  store i32 4294967295, ptr %1901
  %1902 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %1902, ptr %stack.ptr_567, ptr %stack.ptr_569)
  %1903 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %1903, ptr %stack.ptr_568, ptr %stack.ptr_570)
  br label %loop_126
loop_126:
  %1904 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_569, ptr %stack.ptr_570)
  br i1 %1904, label %if_143, label %end_if_143
if_143:
  br label %range_query.end_123
end_if_143:
  %1905 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_569)
  %1906 = getelementptr [1 x i32], ptr %stack.ptr_571, i32 0, i32 0
  %1907 = getelementptr [2 x i32], ptr %1905, i32 0, i32 1
  %1908 = load i32, ptr %1907
  store i32 %1908, ptr %1906
  %1909 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1910 = call ccc i1 @eclair_btree_contains_6(ptr %1909, ptr %stack.ptr_571)
  %1911 = select i1 %1910, i1 0, i1 1
  br i1 %1911, label %if_144, label %end_if_144
if_144:
  %1912 = getelementptr [1 x i32], ptr %stack.ptr_572, i32 0, i32 0
  %1913 = getelementptr [2 x i32], ptr %1905, i32 0, i32 0
  %1914 = load i32, ptr %1913
  store i32 %1914, ptr %1912
  %1915 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1916 = call ccc i1 @eclair_btree_insert_value_6(ptr %1915, ptr %stack.ptr_572)
  br label %end_if_144
end_if_144:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_569)
  br label %loop_126
range_query.end_123:
  %1917 = getelementptr [2 x i32], ptr %stack.ptr_573, i32 0, i32 0
  store i32 0, ptr %1917
  %1918 = getelementptr [2 x i32], ptr %stack.ptr_573, i32 0, i32 1
  store i32 0, ptr %1918
  %1919 = getelementptr [2 x i32], ptr %stack.ptr_574, i32 0, i32 0
  store i32 4294967295, ptr %1919
  %1920 = getelementptr [2 x i32], ptr %stack.ptr_574, i32 0, i32 1
  store i32 4294967295, ptr %1920
  %1921 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_lower_bound_1(ptr %1921, ptr %stack.ptr_573, ptr %stack.ptr_575)
  %1922 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_upper_bound_1(ptr %1922, ptr %stack.ptr_574, ptr %stack.ptr_576)
  br label %loop_127
loop_127:
  %1923 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_575, ptr %stack.ptr_576)
  br i1 %1923, label %if_145, label %end_if_145
if_145:
  br label %range_query.end_124
end_if_145:
  %1924 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_575)
  %1925 = getelementptr [1 x i32], ptr %stack.ptr_577, i32 0, i32 0
  %1926 = getelementptr [2 x i32], ptr %1924, i32 0, i32 1
  %1927 = load i32, ptr %1926
  store i32 %1927, ptr %1925
  %1928 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1929 = call ccc i1 @eclair_btree_contains_6(ptr %1928, ptr %stack.ptr_577)
  %1930 = select i1 %1929, i1 0, i1 1
  br i1 %1930, label %if_146, label %end_if_146
if_146:
  %1931 = getelementptr [1 x i32], ptr %stack.ptr_578, i32 0, i32 0
  %1932 = getelementptr [2 x i32], ptr %1924, i32 0, i32 0
  %1933 = load i32, ptr %1932
  store i32 %1933, ptr %1931
  %1934 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1935 = call ccc i1 @eclair_btree_insert_value_6(ptr %1934, ptr %stack.ptr_578)
  br label %end_if_146
end_if_146:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_575)
  br label %loop_127
range_query.end_124:
  %1936 = getelementptr [2 x i32], ptr %stack.ptr_579, i32 0, i32 0
  store i32 0, ptr %1936
  %1937 = getelementptr [2 x i32], ptr %stack.ptr_579, i32 0, i32 1
  store i32 0, ptr %1937
  %1938 = getelementptr [2 x i32], ptr %stack.ptr_580, i32 0, i32 0
  store i32 4294967295, ptr %1938
  %1939 = getelementptr [2 x i32], ptr %stack.ptr_580, i32 0, i32 1
  store i32 4294967295, ptr %1939
  %1940 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_lower_bound_7(ptr %1940, ptr %stack.ptr_579, ptr %stack.ptr_581)
  %1941 = getelementptr %program, ptr %arg_0, i32 0, i32 24
  call ccc void @eclair_btree_upper_bound_7(ptr %1941, ptr %stack.ptr_580, ptr %stack.ptr_582)
  br label %loop_128
loop_128:
  %1942 = call ccc i1 @eclair_btree_iterator_is_equal_7(ptr %stack.ptr_581, ptr %stack.ptr_582)
  br i1 %1942, label %if_147, label %end_if_147
if_147:
  br label %range_query.end_125
end_if_147:
  %1943 = call ccc ptr @eclair_btree_iterator_current_7(ptr %stack.ptr_581)
  %1944 = getelementptr [1 x i32], ptr %stack.ptr_583, i32 0, i32 0
  %1945 = getelementptr [2 x i32], ptr %1943, i32 0, i32 1
  %1946 = load i32, ptr %1945
  store i32 %1946, ptr %1944
  %1947 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1948 = call ccc i1 @eclair_btree_contains_6(ptr %1947, ptr %stack.ptr_583)
  %1949 = select i1 %1948, i1 0, i1 1
  br i1 %1949, label %if_148, label %end_if_148
if_148:
  %1950 = getelementptr [1 x i32], ptr %stack.ptr_584, i32 0, i32 0
  %1951 = getelementptr [2 x i32], ptr %1943, i32 0, i32 0
  %1952 = load i32, ptr %1951
  store i32 %1952, ptr %1950
  %1953 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1954 = call ccc i1 @eclair_btree_insert_value_6(ptr %1953, ptr %stack.ptr_584)
  br label %end_if_148
end_if_148:
  call ccc void @eclair_btree_iterator_next_7(ptr %stack.ptr_581)
  br label %loop_128
range_query.end_125:
  %1955 = getelementptr [2 x i32], ptr %stack.ptr_585, i32 0, i32 0
  store i32 0, ptr %1955
  %1956 = getelementptr [2 x i32], ptr %stack.ptr_585, i32 0, i32 1
  store i32 0, ptr %1956
  %1957 = getelementptr [2 x i32], ptr %stack.ptr_586, i32 0, i32 0
  store i32 4294967295, ptr %1957
  %1958 = getelementptr [2 x i32], ptr %stack.ptr_586, i32 0, i32 1
  store i32 4294967295, ptr %1958
  %1959 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %1959, ptr %stack.ptr_585, ptr %stack.ptr_587)
  %1960 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %1960, ptr %stack.ptr_586, ptr %stack.ptr_588)
  br label %loop_129
loop_129:
  %1961 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_587, ptr %stack.ptr_588)
  br i1 %1961, label %if_149, label %end_if_149
if_149:
  br label %range_query.end_126
end_if_149:
  %1962 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_587)
  %1963 = getelementptr [1 x i32], ptr %stack.ptr_589, i32 0, i32 0
  %1964 = getelementptr [2 x i32], ptr %1962, i32 0, i32 1
  %1965 = load i32, ptr %1964
  store i32 %1965, ptr %1963
  %1966 = getelementptr %program, ptr %arg_0, i32 0, i32 37
  %1967 = call ccc i1 @eclair_btree_contains_6(ptr %1966, ptr %stack.ptr_589)
  %1968 = select i1 %1967, i1 0, i1 1
  br i1 %1968, label %if_150, label %end_if_150
if_150:
  %1969 = getelementptr [1 x i32], ptr %stack.ptr_590, i32 0, i32 0
  %1970 = getelementptr [2 x i32], ptr %1962, i32 0, i32 0
  %1971 = load i32, ptr %1970
  store i32 %1971, ptr %1969
  %1972 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1973 = call ccc i1 @eclair_btree_insert_value_6(ptr %1972, ptr %stack.ptr_590)
  br label %end_if_150
end_if_150:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_587)
  br label %loop_129
range_query.end_126:
  %1974 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_begin_6(ptr %1974, ptr %stack.ptr_591)
  %1975 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_end_6(ptr %1975, ptr %stack.ptr_592)
  %1976 = getelementptr %program, ptr %arg_0, i32 0, i32 16
  call ccc void @eclair_btree_insert_range_delta_dead_code_dead_code(ptr %1976, ptr %stack.ptr_591, ptr %stack.ptr_592)
  br label %loop_130
loop_130:
  %1977 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  call ccc void @eclair_btree_clear_6(ptr %1977)
  %1978 = getelementptr [3 x i32], ptr %stack.ptr_593, i32 0, i32 0
  store i32 0, ptr %1978
  %1979 = getelementptr [3 x i32], ptr %stack.ptr_593, i32 0, i32 1
  store i32 0, ptr %1979
  %1980 = getelementptr [3 x i32], ptr %stack.ptr_593, i32 0, i32 2
  store i32 0, ptr %1980
  %1981 = getelementptr [3 x i32], ptr %stack.ptr_594, i32 0, i32 0
  store i32 4294967295, ptr %1981
  %1982 = getelementptr [3 x i32], ptr %stack.ptr_594, i32 0, i32 1
  store i32 4294967295, ptr %1982
  %1983 = getelementptr [3 x i32], ptr %stack.ptr_594, i32 0, i32 2
  store i32 4294967295, ptr %1983
  %1984 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %1984, ptr %stack.ptr_593, ptr %stack.ptr_595)
  %1985 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %1985, ptr %stack.ptr_594, ptr %stack.ptr_596)
  br label %loop_131
loop_131:
  %1986 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_595, ptr %stack.ptr_596)
  br i1 %1986, label %if_151, label %end_if_151
if_151:
  br label %range_query.end_127
end_if_151:
  %1987 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_595)
  %1988 = getelementptr [1 x i32], ptr %stack.ptr_597, i32 0, i32 0
  %1989 = getelementptr [3 x i32], ptr %1987, i32 0, i32 0
  %1990 = load i32, ptr %1989
  store i32 %1990, ptr %1988
  %1991 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  %1992 = call ccc i1 @eclair_btree_contains_6(ptr %1991, ptr %stack.ptr_597)
  %1993 = select i1 %1992, i1 0, i1 1
  br i1 %1993, label %if_152, label %end_if_153
if_152:
  %1994 = getelementptr [1 x i32], ptr %stack.ptr_598, i32 0, i32 0
  %1995 = getelementptr [3 x i32], ptr %1987, i32 0, i32 2
  %1996 = load i32, ptr %1995
  store i32 %1996, ptr %1994
  %1997 = getelementptr [1 x i32], ptr %stack.ptr_599, i32 0, i32 0
  %1998 = getelementptr [3 x i32], ptr %1987, i32 0, i32 2
  %1999 = load i32, ptr %1998
  store i32 %1999, ptr %1997
  %2000 = getelementptr %program, ptr %arg_0, i32 0, i32 16
  call ccc void @eclair_btree_lower_bound_6(ptr %2000, ptr %stack.ptr_598, ptr %stack.ptr_600)
  %2001 = getelementptr %program, ptr %arg_0, i32 0, i32 16
  call ccc void @eclair_btree_upper_bound_6(ptr %2001, ptr %stack.ptr_599, ptr %stack.ptr_601)
  br label %loop_132
loop_132:
  %2002 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_600, ptr %stack.ptr_601)
  br i1 %2002, label %if_153, label %end_if_152
if_153:
  br label %range_query.end_128
end_if_152:
  %2003 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_600)
  %2004 = getelementptr [1 x i32], ptr %stack.ptr_602, i32 0, i32 0
  %2005 = getelementptr [3 x i32], ptr %1987, i32 0, i32 0
  %2006 = load i32, ptr %2005
  store i32 %2006, ptr %2004
  %2007 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  %2008 = call ccc i1 @eclair_btree_insert_value_6(ptr %2007, ptr %stack.ptr_602)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_600)
  br label %loop_132
range_query.end_128:
  br label %end_if_153
end_if_153:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_595)
  br label %loop_131
range_query.end_127:
  %2009 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  %2010 = call ccc i1 @eclair_btree_is_empty_6(ptr %2009)
  br i1 %2010, label %if_154, label %end_if_154
if_154:
  br label %loop.end_3
end_if_154:
  %2011 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  call ccc void @eclair_btree_begin_6(ptr %2011, ptr %stack.ptr_603)
  %2012 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  call ccc void @eclair_btree_end_6(ptr %2012, ptr %stack.ptr_604)
  %2013 = getelementptr %program, ptr %arg_0, i32 0, i32 12
  call ccc void @eclair_btree_insert_range_dead_code_new_dead_code(ptr %2013, ptr %stack.ptr_603, ptr %stack.ptr_604)
  %2014 = getelementptr %program, ptr %arg_0, i32 0, i32 41
  %2015 = getelementptr %program, ptr %arg_0, i32 0, i32 16
  call ccc void @eclair_btree_swap_6(ptr %2014, ptr %2015)
  br label %loop_130
loop.end_3:
  %2016 = getelementptr [2 x i32], ptr %stack.ptr_605, i32 0, i32 0
  store i32 0, ptr %2016
  %2017 = getelementptr [2 x i32], ptr %stack.ptr_605, i32 0, i32 1
  store i32 0, ptr %2017
  %2018 = getelementptr [2 x i32], ptr %stack.ptr_606, i32 0, i32 0
  store i32 4294967295, ptr %2018
  %2019 = getelementptr [2 x i32], ptr %stack.ptr_606, i32 0, i32 1
  store i32 4294967295, ptr %2019
  %2020 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_lower_bound_1(ptr %2020, ptr %stack.ptr_605, ptr %stack.ptr_607)
  %2021 = getelementptr %program, ptr %arg_0, i32 0, i32 50
  call ccc void @eclair_btree_upper_bound_1(ptr %2021, ptr %stack.ptr_606, ptr %stack.ptr_608)
  br label %loop_133
loop_133:
  %2022 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_607, ptr %stack.ptr_608)
  br i1 %2022, label %if_155, label %end_if_155
if_155:
  br label %range_query.end_129
end_if_155:
  %2023 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_607)
  %2024 = getelementptr [2 x i32], ptr %stack.ptr_609, i32 0, i32 0
  %2025 = getelementptr [2 x i32], ptr %2023, i32 0, i32 0
  %2026 = load i32, ptr %2025
  store i32 %2026, ptr %2024
  %2027 = getelementptr [2 x i32], ptr %stack.ptr_609, i32 0, i32 1
  store i32 0, ptr %2027
  %2028 = getelementptr [2 x i32], ptr %stack.ptr_610, i32 0, i32 0
  %2029 = getelementptr [2 x i32], ptr %2023, i32 0, i32 0
  %2030 = load i32, ptr %2029
  store i32 %2030, ptr %2028
  %2031 = getelementptr [2 x i32], ptr %stack.ptr_610, i32 0, i32 1
  store i32 4294967295, ptr %2031
  %2032 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2032, ptr %stack.ptr_609, ptr %stack.ptr_611)
  %2033 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2033, ptr %stack.ptr_610, ptr %stack.ptr_612)
  br label %loop_134
loop_134:
  %2034 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_611, ptr %stack.ptr_612)
  br i1 %2034, label %if_156, label %end_if_156
if_156:
  br label %range_query.end_130
end_if_156:
  %2035 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_611)
  %2036 = getelementptr [2 x i32], ptr %stack.ptr_613, i32 0, i32 0
  %2037 = getelementptr [2 x i32], ptr %2035, i32 0, i32 1
  %2038 = load i32, ptr %2037
  store i32 %2038, ptr %2036
  %2039 = getelementptr [2 x i32], ptr %stack.ptr_613, i32 0, i32 1
  store i32 0, ptr %2039
  %2040 = getelementptr [2 x i32], ptr %stack.ptr_614, i32 0, i32 0
  %2041 = getelementptr [2 x i32], ptr %2035, i32 0, i32 1
  %2042 = load i32, ptr %2041
  store i32 %2042, ptr %2040
  %2043 = getelementptr [2 x i32], ptr %stack.ptr_614, i32 0, i32 1
  store i32 4294967295, ptr %2043
  %2044 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_lower_bound_1(ptr %2044, ptr %stack.ptr_613, ptr %stack.ptr_615)
  %2045 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_upper_bound_1(ptr %2045, ptr %stack.ptr_614, ptr %stack.ptr_616)
  br label %loop_135
loop_135:
  %2046 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_615, ptr %stack.ptr_616)
  br i1 %2046, label %if_157, label %end_if_157
if_157:
  br label %range_query.end_131
end_if_157:
  %2047 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_615)
  %2048 = getelementptr [3 x i32], ptr %stack.ptr_617, i32 0, i32 0
  %2049 = getelementptr [2 x i32], ptr %2035, i32 0, i32 1
  %2050 = load i32, ptr %2049
  store i32 %2050, ptr %2048
  %2051 = getelementptr [3 x i32], ptr %stack.ptr_617, i32 0, i32 1
  store i32 0, ptr %2051
  %2052 = getelementptr [3 x i32], ptr %stack.ptr_617, i32 0, i32 2
  store i32 0, ptr %2052
  %2053 = getelementptr [3 x i32], ptr %stack.ptr_618, i32 0, i32 0
  %2054 = getelementptr [2 x i32], ptr %2035, i32 0, i32 1
  %2055 = load i32, ptr %2054
  store i32 %2055, ptr %2053
  %2056 = getelementptr [3 x i32], ptr %stack.ptr_618, i32 0, i32 1
  store i32 4294967295, ptr %2056
  %2057 = getelementptr [3 x i32], ptr %stack.ptr_618, i32 0, i32 2
  store i32 4294967295, ptr %2057
  %2058 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_0(ptr %2058, ptr %stack.ptr_617, ptr %stack.ptr_619)
  %2059 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_0(ptr %2059, ptr %stack.ptr_618, ptr %stack.ptr_620)
  br label %loop_136
loop_136:
  %2060 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_619, ptr %stack.ptr_620)
  br i1 %2060, label %if_158, label %end_if_158
if_158:
  br label %range_query.end_132
end_if_158:
  %2061 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_619)
  %2062 = getelementptr [1 x i32], ptr %stack.ptr_621, i32 0, i32 0
  %2063 = getelementptr [3 x i32], ptr %2061, i32 0, i32 2
  %2064 = load i32, ptr %2063
  store i32 %2064, ptr %2062
  %2065 = getelementptr [1 x i32], ptr %stack.ptr_622, i32 0, i32 0
  %2066 = getelementptr [3 x i32], ptr %2061, i32 0, i32 2
  %2067 = load i32, ptr %2066
  store i32 %2067, ptr %2065
  %2068 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_lower_bound_6(ptr %2068, ptr %stack.ptr_621, ptr %stack.ptr_623)
  %2069 = getelementptr %program, ptr %arg_0, i32 0, i32 68
  call ccc void @eclair_btree_upper_bound_6(ptr %2069, ptr %stack.ptr_622, ptr %stack.ptr_624)
  br label %loop_137
loop_137:
  %2070 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_623, ptr %stack.ptr_624)
  br i1 %2070, label %if_159, label %end_if_159
if_159:
  br label %range_query.end_133
end_if_159:
  %2071 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_623)
  %2072 = getelementptr [3 x i32], ptr %stack.ptr_625, i32 0, i32 0
  %2073 = getelementptr [2 x i32], ptr %2035, i32 0, i32 1
  %2074 = load i32, ptr %2073
  store i32 %2074, ptr %2072
  %2075 = getelementptr [3 x i32], ptr %stack.ptr_625, i32 0, i32 1
  %2076 = getelementptr [3 x i32], ptr %2061, i32 0, i32 2
  %2077 = load i32, ptr %2076
  store i32 %2077, ptr %2075
  %2078 = getelementptr [3 x i32], ptr %stack.ptr_625, i32 0, i32 2
  %2079 = getelementptr [3 x i32], ptr %2061, i32 0, i32 1
  %2080 = load i32, ptr %2079
  store i32 %2080, ptr %2078
  %2081 = getelementptr %program, ptr %arg_0, i32 0, i32 71
  %2082 = call ccc i1 @eclair_btree_insert_value_0(ptr %2081, ptr %stack.ptr_625)
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_623)
  br label %loop_137
range_query.end_133:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_619)
  br label %loop_136
range_query.end_132:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_615)
  br label %loop_135
range_query.end_131:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_611)
  br label %loop_134
range_query.end_130:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_607)
  br label %loop_133
range_query.end_129:
  %2083 = getelementptr [2 x i32], ptr %stack.ptr_626, i32 0, i32 0
  store i32 0, ptr %2083
  %2084 = getelementptr [2 x i32], ptr %stack.ptr_626, i32 0, i32 1
  store i32 0, ptr %2084
  %2085 = getelementptr [2 x i32], ptr %stack.ptr_627, i32 0, i32 0
  store i32 4294967295, ptr %2085
  %2086 = getelementptr [2 x i32], ptr %stack.ptr_627, i32 0, i32 1
  store i32 4294967295, ptr %2086
  %2087 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_lower_bound_1(ptr %2087, ptr %stack.ptr_626, ptr %stack.ptr_628)
  %2088 = getelementptr %program, ptr %arg_0, i32 0, i32 14
  call ccc void @eclair_btree_upper_bound_1(ptr %2088, ptr %stack.ptr_627, ptr %stack.ptr_629)
  br label %loop_138
loop_138:
  %2089 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_628, ptr %stack.ptr_629)
  br i1 %2089, label %if_160, label %end_if_160
if_160:
  br label %range_query.end_134
end_if_160:
  %2090 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_628)
  %2091 = getelementptr [2 x i32], ptr %stack.ptr_630, i32 0, i32 0
  store i32 0, ptr %2091
  %2092 = getelementptr [2 x i32], ptr %stack.ptr_630, i32 0, i32 1
  %2093 = getelementptr [2 x i32], ptr %2090, i32 0, i32 1
  %2094 = load i32, ptr %2093
  store i32 %2094, ptr %2092
  %2095 = getelementptr [2 x i32], ptr %stack.ptr_631, i32 0, i32 0
  store i32 4294967295, ptr %2095
  %2096 = getelementptr [2 x i32], ptr %stack.ptr_631, i32 0, i32 1
  %2097 = getelementptr [2 x i32], ptr %2090, i32 0, i32 1
  %2098 = load i32, ptr %2097
  store i32 %2098, ptr %2096
  %2099 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_lower_bound_2(ptr %2099, ptr %stack.ptr_630, ptr %stack.ptr_632)
  %2100 = getelementptr %program, ptr %arg_0, i32 0, i32 4
  call ccc void @eclair_btree_upper_bound_2(ptr %2100, ptr %stack.ptr_631, ptr %stack.ptr_633)
  br label %loop_139
loop_139:
  %2101 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_632, ptr %stack.ptr_633)
  br i1 %2101, label %if_161, label %end_if_161
if_161:
  br label %range_query.end_135
end_if_161:
  %2102 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_632)
  %2103 = getelementptr [2 x i32], ptr %stack.ptr_634, i32 0, i32 0
  %2104 = getelementptr [2 x i32], ptr %2102, i32 0, i32 0
  %2105 = load i32, ptr %2104
  store i32 %2105, ptr %2103
  %2106 = getelementptr [2 x i32], ptr %stack.ptr_634, i32 0, i32 1
  %2107 = getelementptr [2 x i32], ptr %2090, i32 0, i32 1
  %2108 = load i32, ptr %2107
  store i32 %2108, ptr %2106
  %2109 = getelementptr %program, ptr %arg_0, i32 0, i32 49
  %2110 = call ccc i1 @eclair_btree_insert_value_1(ptr %2109, ptr %stack.ptr_634)
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_632)
  br label %loop_139
range_query.end_135:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_628)
  br label %loop_138
range_query.end_134:
  %2111 = getelementptr [3 x i32], ptr %stack.ptr_635, i32 0, i32 0
  store i32 0, ptr %2111
  %2112 = getelementptr [3 x i32], ptr %stack.ptr_635, i32 0, i32 1
  store i32 0, ptr %2112
  %2113 = getelementptr [3 x i32], ptr %stack.ptr_635, i32 0, i32 2
  store i32 0, ptr %2113
  %2114 = getelementptr [3 x i32], ptr %stack.ptr_636, i32 0, i32 0
  store i32 4294967295, ptr %2114
  %2115 = getelementptr [3 x i32], ptr %stack.ptr_636, i32 0, i32 1
  store i32 4294967295, ptr %2115
  %2116 = getelementptr [3 x i32], ptr %stack.ptr_636, i32 0, i32 2
  store i32 4294967295, ptr %2116
  %2117 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2117, ptr %stack.ptr_635, ptr %stack.ptr_637)
  %2118 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2118, ptr %stack.ptr_636, ptr %stack.ptr_638)
  br label %loop_140
loop_140:
  %2119 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_637, ptr %stack.ptr_638)
  br i1 %2119, label %if_162, label %end_if_162
if_162:
  br label %range_query.end_136
end_if_162:
  %2120 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_637)
  %2121 = getelementptr [2 x i32], ptr %stack.ptr_639, i32 0, i32 0
  %2122 = getelementptr [3 x i32], ptr %2120, i32 0, i32 2
  %2123 = load i32, ptr %2122
  store i32 %2123, ptr %2121
  %2124 = getelementptr [2 x i32], ptr %stack.ptr_639, i32 0, i32 1
  store i32 0, ptr %2124
  %2125 = getelementptr [2 x i32], ptr %stack.ptr_640, i32 0, i32 0
  %2126 = getelementptr [3 x i32], ptr %2120, i32 0, i32 2
  %2127 = load i32, ptr %2126
  store i32 %2127, ptr %2125
  %2128 = getelementptr [2 x i32], ptr %stack.ptr_640, i32 0, i32 1
  store i32 4294967295, ptr %2128
  %2129 = getelementptr %program, ptr %arg_0, i32 0, i32 49
  call ccc void @eclair_btree_lower_bound_1(ptr %2129, ptr %stack.ptr_639, ptr %stack.ptr_641)
  %2130 = getelementptr %program, ptr %arg_0, i32 0, i32 49
  call ccc void @eclair_btree_upper_bound_1(ptr %2130, ptr %stack.ptr_640, ptr %stack.ptr_642)
  br label %loop_141
loop_141:
  %2131 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_641, ptr %stack.ptr_642)
  br i1 %2131, label %if_163, label %end_if_163
if_163:
  br label %range_query.end_137
end_if_163:
  %2132 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_641)
  %2133 = getelementptr [3 x i32], ptr %stack.ptr_643, i32 0, i32 0
  %2134 = getelementptr [3 x i32], ptr %2120, i32 0, i32 2
  %2135 = load i32, ptr %2134
  store i32 %2135, ptr %2133
  %2136 = getelementptr [3 x i32], ptr %stack.ptr_643, i32 0, i32 1
  store i32 0, ptr %2136
  %2137 = getelementptr [3 x i32], ptr %stack.ptr_643, i32 0, i32 2
  store i32 0, ptr %2137
  %2138 = getelementptr [3 x i32], ptr %stack.ptr_644, i32 0, i32 0
  %2139 = getelementptr [3 x i32], ptr %2120, i32 0, i32 2
  %2140 = load i32, ptr %2139
  store i32 %2140, ptr %2138
  %2141 = getelementptr [3 x i32], ptr %stack.ptr_644, i32 0, i32 1
  store i32 4294967295, ptr %2141
  %2142 = getelementptr [3 x i32], ptr %stack.ptr_644, i32 0, i32 2
  store i32 4294967295, ptr %2142
  %2143 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_0(ptr %2143, ptr %stack.ptr_643, ptr %stack.ptr_645)
  %2144 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_0(ptr %2144, ptr %stack.ptr_644, ptr %stack.ptr_646)
  br label %loop_142
loop_142:
  %2145 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_645, ptr %stack.ptr_646)
  br i1 %2145, label %if_164, label %end_if_164
if_164:
  br label %range_query.end_138
end_if_164:
  %2146 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_645)
  %2147 = getelementptr [2 x i32], ptr %stack.ptr_647, i32 0, i32 0
  %2148 = getelementptr [3 x i32], ptr %2146, i32 0, i32 2
  %2149 = load i32, ptr %2148
  store i32 %2149, ptr %2147
  %2150 = getelementptr [2 x i32], ptr %stack.ptr_647, i32 0, i32 1
  store i32 0, ptr %2150
  %2151 = getelementptr [2 x i32], ptr %stack.ptr_648, i32 0, i32 0
  %2152 = getelementptr [3 x i32], ptr %2146, i32 0, i32 2
  %2153 = load i32, ptr %2152
  store i32 %2153, ptr %2151
  %2154 = getelementptr [2 x i32], ptr %stack.ptr_648, i32 0, i32 1
  store i32 4294967295, ptr %2154
  %2155 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2155, ptr %stack.ptr_647, ptr %stack.ptr_649)
  %2156 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2156, ptr %stack.ptr_648, ptr %stack.ptr_650)
  br label %loop_143
loop_143:
  %2157 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_649, ptr %stack.ptr_650)
  br i1 %2157, label %if_165, label %end_if_165
if_165:
  br label %range_query.end_139
end_if_165:
  %2158 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_649)
  %2159 = getelementptr [2 x i32], ptr %stack.ptr_651, i32 0, i32 0
  %2160 = getelementptr [3 x i32], ptr %2120, i32 0, i32 0
  %2161 = load i32, ptr %2160
  store i32 %2161, ptr %2159
  %2162 = getelementptr [2 x i32], ptr %stack.ptr_651, i32 0, i32 1
  %2163 = getelementptr [3 x i32], ptr %2146, i32 0, i32 2
  %2164 = load i32, ptr %2163
  store i32 %2164, ptr %2162
  %2165 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2166 = call ccc i1 @eclair_btree_insert_value_1(ptr %2165, ptr %stack.ptr_651)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_649)
  br label %loop_143
range_query.end_139:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_645)
  br label %loop_142
range_query.end_138:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_641)
  br label %loop_141
range_query.end_137:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_637)
  br label %loop_140
range_query.end_136:
  %2167 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_begin_1(ptr %2167, ptr %stack.ptr_652)
  %2168 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_end_1(ptr %2168, ptr %stack.ptr_653)
  %2169 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_insert_range_delta_grounded_node_grounded_node(ptr %2169, ptr %stack.ptr_652, ptr %stack.ptr_653)
  br label %loop_144
loop_144:
  %2170 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  call ccc void @eclair_btree_clear_1(ptr %2170)
  %2171 = getelementptr [3 x i32], ptr %stack.ptr_654, i32 0, i32 0
  store i32 0, ptr %2171
  %2172 = getelementptr [3 x i32], ptr %stack.ptr_654, i32 0, i32 1
  store i32 0, ptr %2172
  %2173 = getelementptr [3 x i32], ptr %stack.ptr_654, i32 0, i32 2
  store i32 0, ptr %2173
  %2174 = getelementptr [3 x i32], ptr %stack.ptr_655, i32 0, i32 0
  store i32 4294967295, ptr %2174
  %2175 = getelementptr [3 x i32], ptr %stack.ptr_655, i32 0, i32 1
  store i32 4294967295, ptr %2175
  %2176 = getelementptr [3 x i32], ptr %stack.ptr_655, i32 0, i32 2
  store i32 4294967295, ptr %2176
  %2177 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2177, ptr %stack.ptr_654, ptr %stack.ptr_656)
  %2178 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2178, ptr %stack.ptr_655, ptr %stack.ptr_657)
  br label %loop_145
loop_145:
  %2179 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_656, ptr %stack.ptr_657)
  br i1 %2179, label %if_166, label %end_if_166
if_166:
  br label %range_query.end_140
end_if_166:
  %2180 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_656)
  %2181 = getelementptr [3 x i32], ptr %stack.ptr_658, i32 0, i32 0
  %2182 = getelementptr [3 x i32], ptr %2180, i32 0, i32 2
  %2183 = load i32, ptr %2182
  store i32 %2183, ptr %2181
  %2184 = getelementptr [3 x i32], ptr %stack.ptr_658, i32 0, i32 1
  store i32 0, ptr %2184
  %2185 = getelementptr [3 x i32], ptr %stack.ptr_658, i32 0, i32 2
  store i32 0, ptr %2185
  %2186 = getelementptr [3 x i32], ptr %stack.ptr_659, i32 0, i32 0
  %2187 = getelementptr [3 x i32], ptr %2180, i32 0, i32 2
  %2188 = load i32, ptr %2187
  store i32 %2188, ptr %2186
  %2189 = getelementptr [3 x i32], ptr %stack.ptr_659, i32 0, i32 1
  store i32 4294967295, ptr %2189
  %2190 = getelementptr [3 x i32], ptr %stack.ptr_659, i32 0, i32 2
  store i32 4294967295, ptr %2190
  %2191 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %2191, ptr %stack.ptr_658, ptr %stack.ptr_660)
  %2192 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %2192, ptr %stack.ptr_659, ptr %stack.ptr_661)
  br label %loop_146
loop_146:
  %2193 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_660, ptr %stack.ptr_661)
  br i1 %2193, label %if_167, label %end_if_167
if_167:
  br label %range_query.end_141
end_if_167:
  %2194 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_660)
  %2195 = getelementptr [2 x i32], ptr %stack.ptr_662, i32 0, i32 0
  %2196 = getelementptr [3 x i32], ptr %2180, i32 0, i32 0
  %2197 = load i32, ptr %2196
  store i32 %2197, ptr %2195
  %2198 = getelementptr [2 x i32], ptr %stack.ptr_662, i32 0, i32 1
  %2199 = getelementptr [3 x i32], ptr %2194, i32 0, i32 1
  %2200 = load i32, ptr %2199
  store i32 %2200, ptr %2198
  %2201 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2202 = call ccc i1 @eclair_btree_contains_1(ptr %2201, ptr %stack.ptr_662)
  %2203 = select i1 %2202, i1 0, i1 1
  br i1 %2203, label %if_168, label %end_if_170
if_168:
  %2204 = getelementptr [2 x i32], ptr %stack.ptr_663, i32 0, i32 0
  %2205 = getelementptr [3 x i32], ptr %2180, i32 0, i32 0
  %2206 = load i32, ptr %2205
  store i32 %2206, ptr %2204
  %2207 = getelementptr [2 x i32], ptr %stack.ptr_663, i32 0, i32 1
  %2208 = getelementptr [3 x i32], ptr %2194, i32 0, i32 2
  %2209 = load i32, ptr %2208
  store i32 %2209, ptr %2207
  %2210 = getelementptr [2 x i32], ptr %stack.ptr_664, i32 0, i32 0
  %2211 = getelementptr [3 x i32], ptr %2180, i32 0, i32 0
  %2212 = load i32, ptr %2211
  store i32 %2212, ptr %2210
  %2213 = getelementptr [2 x i32], ptr %stack.ptr_664, i32 0, i32 1
  %2214 = getelementptr [3 x i32], ptr %2194, i32 0, i32 2
  %2215 = load i32, ptr %2214
  store i32 %2215, ptr %2213
  %2216 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_lower_bound_1(ptr %2216, ptr %stack.ptr_663, ptr %stack.ptr_665)
  %2217 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_upper_bound_1(ptr %2217, ptr %stack.ptr_664, ptr %stack.ptr_666)
  br label %loop_147
loop_147:
  %2218 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_665, ptr %stack.ptr_666)
  br i1 %2218, label %if_169, label %end_if_168
if_169:
  br label %range_query.end_142
end_if_168:
  %2219 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_665)
  %2220 = getelementptr [2 x i32], ptr %stack.ptr_667, i32 0, i32 0
  %2221 = getelementptr [3 x i32], ptr %2194, i32 0, i32 1
  %2222 = load i32, ptr %2221
  store i32 %2222, ptr %2220
  %2223 = getelementptr [2 x i32], ptr %stack.ptr_667, i32 0, i32 1
  store i32 0, ptr %2223
  %2224 = getelementptr [2 x i32], ptr %stack.ptr_668, i32 0, i32 0
  %2225 = getelementptr [3 x i32], ptr %2194, i32 0, i32 1
  %2226 = load i32, ptr %2225
  store i32 %2226, ptr %2224
  %2227 = getelementptr [2 x i32], ptr %stack.ptr_668, i32 0, i32 1
  store i32 4294967295, ptr %2227
  %2228 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2228, ptr %stack.ptr_667, ptr %stack.ptr_669)
  %2229 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2229, ptr %stack.ptr_668, ptr %stack.ptr_670)
  br label %loop_148
loop_148:
  %2230 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_669, ptr %stack.ptr_670)
  br i1 %2230, label %if_170, label %end_if_169
if_170:
  br label %range_query.end_143
end_if_169:
  %2231 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_669)
  %2232 = getelementptr [2 x i32], ptr %stack.ptr_671, i32 0, i32 0
  %2233 = getelementptr [3 x i32], ptr %2180, i32 0, i32 0
  %2234 = load i32, ptr %2233
  store i32 %2234, ptr %2232
  %2235 = getelementptr [2 x i32], ptr %stack.ptr_671, i32 0, i32 1
  %2236 = getelementptr [3 x i32], ptr %2194, i32 0, i32 1
  %2237 = load i32, ptr %2236
  store i32 %2237, ptr %2235
  %2238 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2239 = call ccc i1 @eclair_btree_insert_value_1(ptr %2238, ptr %stack.ptr_671)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_669)
  br label %loop_148
range_query.end_143:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_665)
  br label %loop_147
range_query.end_142:
  br label %end_if_170
end_if_170:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_660)
  br label %loop_146
range_query.end_141:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_656)
  br label %loop_145
range_query.end_140:
  %2240 = getelementptr [3 x i32], ptr %stack.ptr_672, i32 0, i32 0
  store i32 0, ptr %2240
  %2241 = getelementptr [3 x i32], ptr %stack.ptr_672, i32 0, i32 1
  store i32 0, ptr %2241
  %2242 = getelementptr [3 x i32], ptr %stack.ptr_672, i32 0, i32 2
  store i32 0, ptr %2242
  %2243 = getelementptr [3 x i32], ptr %stack.ptr_673, i32 0, i32 0
  store i32 4294967295, ptr %2243
  %2244 = getelementptr [3 x i32], ptr %stack.ptr_673, i32 0, i32 1
  store i32 4294967295, ptr %2244
  %2245 = getelementptr [3 x i32], ptr %stack.ptr_673, i32 0, i32 2
  store i32 4294967295, ptr %2245
  %2246 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2246, ptr %stack.ptr_672, ptr %stack.ptr_674)
  %2247 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2247, ptr %stack.ptr_673, ptr %stack.ptr_675)
  br label %loop_149
loop_149:
  %2248 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_674, ptr %stack.ptr_675)
  br i1 %2248, label %if_171, label %end_if_171
if_171:
  br label %range_query.end_144
end_if_171:
  %2249 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_674)
  %2250 = getelementptr [3 x i32], ptr %stack.ptr_676, i32 0, i32 0
  %2251 = getelementptr [3 x i32], ptr %2249, i32 0, i32 2
  %2252 = load i32, ptr %2251
  store i32 %2252, ptr %2250
  %2253 = getelementptr [3 x i32], ptr %stack.ptr_676, i32 0, i32 1
  store i32 0, ptr %2253
  %2254 = getelementptr [3 x i32], ptr %stack.ptr_676, i32 0, i32 2
  store i32 0, ptr %2254
  %2255 = getelementptr [3 x i32], ptr %stack.ptr_677, i32 0, i32 0
  %2256 = getelementptr [3 x i32], ptr %2249, i32 0, i32 2
  %2257 = load i32, ptr %2256
  store i32 %2257, ptr %2255
  %2258 = getelementptr [3 x i32], ptr %stack.ptr_677, i32 0, i32 1
  store i32 4294967295, ptr %2258
  %2259 = getelementptr [3 x i32], ptr %stack.ptr_677, i32 0, i32 2
  store i32 4294967295, ptr %2259
  %2260 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_lower_bound_0(ptr %2260, ptr %stack.ptr_676, ptr %stack.ptr_678)
  %2261 = getelementptr %program, ptr %arg_0, i32 0, i32 2
  call ccc void @eclair_btree_upper_bound_0(ptr %2261, ptr %stack.ptr_677, ptr %stack.ptr_679)
  br label %loop_150
loop_150:
  %2262 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_678, ptr %stack.ptr_679)
  br i1 %2262, label %if_172, label %end_if_172
if_172:
  br label %range_query.end_145
end_if_172:
  %2263 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_678)
  %2264 = getelementptr [2 x i32], ptr %stack.ptr_680, i32 0, i32 0
  %2265 = getelementptr [3 x i32], ptr %2249, i32 0, i32 0
  %2266 = load i32, ptr %2265
  store i32 %2266, ptr %2264
  %2267 = getelementptr [2 x i32], ptr %stack.ptr_680, i32 0, i32 1
  %2268 = getelementptr [3 x i32], ptr %2263, i32 0, i32 2
  %2269 = load i32, ptr %2268
  store i32 %2269, ptr %2267
  %2270 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2271 = call ccc i1 @eclair_btree_contains_1(ptr %2270, ptr %stack.ptr_680)
  %2272 = select i1 %2271, i1 0, i1 1
  br i1 %2272, label %if_173, label %end_if_175
if_173:
  %2273 = getelementptr [2 x i32], ptr %stack.ptr_681, i32 0, i32 0
  %2274 = getelementptr [3 x i32], ptr %2249, i32 0, i32 0
  %2275 = load i32, ptr %2274
  store i32 %2275, ptr %2273
  %2276 = getelementptr [2 x i32], ptr %stack.ptr_681, i32 0, i32 1
  %2277 = getelementptr [3 x i32], ptr %2263, i32 0, i32 1
  %2278 = load i32, ptr %2277
  store i32 %2278, ptr %2276
  %2279 = getelementptr [2 x i32], ptr %stack.ptr_682, i32 0, i32 0
  %2280 = getelementptr [3 x i32], ptr %2249, i32 0, i32 0
  %2281 = load i32, ptr %2280
  store i32 %2281, ptr %2279
  %2282 = getelementptr [2 x i32], ptr %stack.ptr_682, i32 0, i32 1
  %2283 = getelementptr [3 x i32], ptr %2263, i32 0, i32 1
  %2284 = load i32, ptr %2283
  store i32 %2284, ptr %2282
  %2285 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_lower_bound_1(ptr %2285, ptr %stack.ptr_681, ptr %stack.ptr_683)
  %2286 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_upper_bound_1(ptr %2286, ptr %stack.ptr_682, ptr %stack.ptr_684)
  br label %loop_151
loop_151:
  %2287 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_683, ptr %stack.ptr_684)
  br i1 %2287, label %if_174, label %end_if_173
if_174:
  br label %range_query.end_146
end_if_173:
  %2288 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_683)
  %2289 = getelementptr [2 x i32], ptr %stack.ptr_685, i32 0, i32 0
  %2290 = getelementptr [3 x i32], ptr %2263, i32 0, i32 2
  %2291 = load i32, ptr %2290
  store i32 %2291, ptr %2289
  %2292 = getelementptr [2 x i32], ptr %stack.ptr_685, i32 0, i32 1
  store i32 0, ptr %2292
  %2293 = getelementptr [2 x i32], ptr %stack.ptr_686, i32 0, i32 0
  %2294 = getelementptr [3 x i32], ptr %2263, i32 0, i32 2
  %2295 = load i32, ptr %2294
  store i32 %2295, ptr %2293
  %2296 = getelementptr [2 x i32], ptr %stack.ptr_686, i32 0, i32 1
  store i32 4294967295, ptr %2296
  %2297 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2297, ptr %stack.ptr_685, ptr %stack.ptr_687)
  %2298 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2298, ptr %stack.ptr_686, ptr %stack.ptr_688)
  br label %loop_152
loop_152:
  %2299 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_687, ptr %stack.ptr_688)
  br i1 %2299, label %if_175, label %end_if_174
if_175:
  br label %range_query.end_147
end_if_174:
  %2300 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_687)
  %2301 = getelementptr [2 x i32], ptr %stack.ptr_689, i32 0, i32 0
  %2302 = getelementptr [3 x i32], ptr %2249, i32 0, i32 0
  %2303 = load i32, ptr %2302
  store i32 %2303, ptr %2301
  %2304 = getelementptr [2 x i32], ptr %stack.ptr_689, i32 0, i32 1
  %2305 = getelementptr [3 x i32], ptr %2263, i32 0, i32 2
  %2306 = load i32, ptr %2305
  store i32 %2306, ptr %2304
  %2307 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2308 = call ccc i1 @eclair_btree_insert_value_1(ptr %2307, ptr %stack.ptr_689)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_687)
  br label %loop_152
range_query.end_147:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_683)
  br label %loop_151
range_query.end_146:
  br label %end_if_175
end_if_175:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_678)
  br label %loop_150
range_query.end_145:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_674)
  br label %loop_149
range_query.end_144:
  %2309 = getelementptr [2 x i32], ptr %stack.ptr_690, i32 0, i32 0
  store i32 0, ptr %2309
  %2310 = getelementptr [2 x i32], ptr %stack.ptr_690, i32 0, i32 1
  store i32 0, ptr %2310
  %2311 = getelementptr [2 x i32], ptr %stack.ptr_691, i32 0, i32 0
  store i32 4294967295, ptr %2311
  %2312 = getelementptr [2 x i32], ptr %stack.ptr_691, i32 0, i32 1
  store i32 4294967295, ptr %2312
  %2313 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_lower_bound_1(ptr %2313, ptr %stack.ptr_690, ptr %stack.ptr_692)
  %2314 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_upper_bound_1(ptr %2314, ptr %stack.ptr_691, ptr %stack.ptr_693)
  br label %loop_153
loop_153:
  %2315 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_692, ptr %stack.ptr_693)
  br i1 %2315, label %if_176, label %end_if_176
if_176:
  br label %range_query.end_148
end_if_176:
  %2316 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_692)
  %2317 = getelementptr [2 x i32], ptr %stack.ptr_694, i32 0, i32 0
  %2318 = getelementptr [2 x i32], ptr %2316, i32 0, i32 0
  %2319 = load i32, ptr %2318
  store i32 %2319, ptr %2317
  %2320 = getelementptr [2 x i32], ptr %stack.ptr_694, i32 0, i32 1
  store i32 0, ptr %2320
  %2321 = getelementptr [2 x i32], ptr %stack.ptr_695, i32 0, i32 0
  %2322 = getelementptr [2 x i32], ptr %2316, i32 0, i32 0
  %2323 = load i32, ptr %2322
  store i32 %2323, ptr %2321
  %2324 = getelementptr [2 x i32], ptr %stack.ptr_695, i32 0, i32 1
  store i32 4294967295, ptr %2324
  %2325 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %2325, ptr %stack.ptr_694, ptr %stack.ptr_696)
  %2326 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %2326, ptr %stack.ptr_695, ptr %stack.ptr_697)
  br label %loop_154
loop_154:
  %2327 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_696, ptr %stack.ptr_697)
  br i1 %2327, label %if_177, label %end_if_177
if_177:
  br label %range_query.end_149
end_if_177:
  %2328 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_696)
  %2329 = getelementptr [2 x i32], ptr %stack.ptr_698, i32 0, i32 0
  %2330 = getelementptr [2 x i32], ptr %2316, i32 0, i32 0
  %2331 = load i32, ptr %2330
  store i32 %2331, ptr %2329
  %2332 = getelementptr [2 x i32], ptr %stack.ptr_698, i32 0, i32 1
  %2333 = getelementptr [2 x i32], ptr %2328, i32 0, i32 1
  %2334 = load i32, ptr %2333
  store i32 %2334, ptr %2332
  %2335 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  %2336 = call ccc i1 @eclair_btree_contains_1(ptr %2335, ptr %stack.ptr_698)
  %2337 = select i1 %2336, i1 0, i1 1
  br i1 %2337, label %if_178, label %end_if_180
if_178:
  %2338 = getelementptr [4 x i32], ptr %stack.ptr_699, i32 0, i32 0
  store i32 0, ptr %2338
  %2339 = getelementptr [4 x i32], ptr %stack.ptr_699, i32 0, i32 1
  store i32 0, ptr %2339
  %2340 = getelementptr [4 x i32], ptr %stack.ptr_699, i32 0, i32 2
  %2341 = getelementptr [2 x i32], ptr %2316, i32 0, i32 1
  %2342 = load i32, ptr %2341
  store i32 %2342, ptr %2340
  %2343 = getelementptr [4 x i32], ptr %stack.ptr_699, i32 0, i32 3
  %2344 = getelementptr [2 x i32], ptr %2328, i32 0, i32 1
  %2345 = load i32, ptr %2344
  store i32 %2345, ptr %2343
  %2346 = getelementptr [4 x i32], ptr %stack.ptr_700, i32 0, i32 0
  store i32 4294967295, ptr %2346
  %2347 = getelementptr [4 x i32], ptr %stack.ptr_700, i32 0, i32 1
  store i32 4294967295, ptr %2347
  %2348 = getelementptr [4 x i32], ptr %stack.ptr_700, i32 0, i32 2
  %2349 = getelementptr [2 x i32], ptr %2316, i32 0, i32 1
  %2350 = load i32, ptr %2349
  store i32 %2350, ptr %2348
  %2351 = getelementptr [4 x i32], ptr %stack.ptr_700, i32 0, i32 3
  %2352 = getelementptr [2 x i32], ptr %2328, i32 0, i32 1
  %2353 = load i32, ptr %2352
  store i32 %2353, ptr %2351
  %2354 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %2354, ptr %stack.ptr_699, ptr %stack.ptr_701)
  %2355 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %2355, ptr %stack.ptr_700, ptr %stack.ptr_702)
  br label %loop_155
loop_155:
  %2356 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_701, ptr %stack.ptr_702)
  br i1 %2356, label %if_179, label %end_if_178
if_179:
  br label %range_query.end_150
end_if_178:
  %2357 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_701)
  %2358 = getelementptr [2 x i32], ptr %stack.ptr_703, i32 0, i32 0
  %2359 = getelementptr [2 x i32], ptr %2316, i32 0, i32 0
  %2360 = load i32, ptr %2359
  store i32 %2360, ptr %2358
  %2361 = getelementptr [2 x i32], ptr %stack.ptr_703, i32 0, i32 1
  %2362 = getelementptr [4 x i32], ptr %2357, i32 0, i32 0
  %2363 = load i32, ptr %2362
  store i32 %2363, ptr %2361
  %2364 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2365 = call ccc i1 @eclair_btree_contains_1(ptr %2364, ptr %stack.ptr_703)
  %2366 = select i1 %2365, i1 0, i1 1
  br i1 %2366, label %if_180, label %end_if_179
if_180:
  %2367 = getelementptr [2 x i32], ptr %stack.ptr_704, i32 0, i32 0
  %2368 = getelementptr [2 x i32], ptr %2316, i32 0, i32 0
  %2369 = load i32, ptr %2368
  store i32 %2369, ptr %2367
  %2370 = getelementptr [2 x i32], ptr %stack.ptr_704, i32 0, i32 1
  %2371 = getelementptr [4 x i32], ptr %2357, i32 0, i32 0
  %2372 = load i32, ptr %2371
  store i32 %2372, ptr %2370
  %2373 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2374 = call ccc i1 @eclair_btree_insert_value_1(ptr %2373, ptr %stack.ptr_704)
  br label %end_if_179
end_if_179:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_701)
  br label %loop_155
range_query.end_150:
  br label %end_if_180
end_if_180:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_696)
  br label %loop_154
range_query.end_149:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_692)
  br label %loop_153
range_query.end_148:
  %2375 = getelementptr [2 x i32], ptr %stack.ptr_705, i32 0, i32 0
  store i32 0, ptr %2375
  %2376 = getelementptr [2 x i32], ptr %stack.ptr_705, i32 0, i32 1
  store i32 0, ptr %2376
  %2377 = getelementptr [2 x i32], ptr %stack.ptr_706, i32 0, i32 0
  store i32 4294967295, ptr %2377
  %2378 = getelementptr [2 x i32], ptr %stack.ptr_706, i32 0, i32 1
  store i32 4294967295, ptr %2378
  %2379 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %2379, ptr %stack.ptr_705, ptr %stack.ptr_707)
  %2380 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %2380, ptr %stack.ptr_706, ptr %stack.ptr_708)
  br label %loop_156
loop_156:
  %2381 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_707, ptr %stack.ptr_708)
  br i1 %2381, label %if_181, label %end_if_181
if_181:
  br label %range_query.end_151
end_if_181:
  %2382 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_707)
  %2383 = getelementptr [2 x i32], ptr %stack.ptr_709, i32 0, i32 0
  %2384 = getelementptr [2 x i32], ptr %2382, i32 0, i32 0
  %2385 = load i32, ptr %2384
  store i32 %2385, ptr %2383
  %2386 = getelementptr [2 x i32], ptr %stack.ptr_709, i32 0, i32 1
  store i32 0, ptr %2386
  %2387 = getelementptr [2 x i32], ptr %stack.ptr_710, i32 0, i32 0
  %2388 = getelementptr [2 x i32], ptr %2382, i32 0, i32 0
  %2389 = load i32, ptr %2388
  store i32 %2389, ptr %2387
  %2390 = getelementptr [2 x i32], ptr %stack.ptr_710, i32 0, i32 1
  store i32 4294967295, ptr %2390
  %2391 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_lower_bound_1(ptr %2391, ptr %stack.ptr_709, ptr %stack.ptr_711)
  %2392 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_upper_bound_1(ptr %2392, ptr %stack.ptr_710, ptr %stack.ptr_712)
  br label %loop_157
loop_157:
  %2393 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_711, ptr %stack.ptr_712)
  br i1 %2393, label %if_182, label %end_if_182
if_182:
  br label %range_query.end_152
end_if_182:
  %2394 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_711)
  %2395 = getelementptr [4 x i32], ptr %stack.ptr_713, i32 0, i32 0
  store i32 0, ptr %2395
  %2396 = getelementptr [4 x i32], ptr %stack.ptr_713, i32 0, i32 1
  store i32 0, ptr %2396
  %2397 = getelementptr [4 x i32], ptr %stack.ptr_713, i32 0, i32 2
  %2398 = getelementptr [2 x i32], ptr %2382, i32 0, i32 1
  %2399 = load i32, ptr %2398
  store i32 %2399, ptr %2397
  %2400 = getelementptr [4 x i32], ptr %stack.ptr_713, i32 0, i32 3
  %2401 = getelementptr [2 x i32], ptr %2394, i32 0, i32 1
  %2402 = load i32, ptr %2401
  store i32 %2402, ptr %2400
  %2403 = getelementptr [4 x i32], ptr %stack.ptr_714, i32 0, i32 0
  store i32 4294967295, ptr %2403
  %2404 = getelementptr [4 x i32], ptr %stack.ptr_714, i32 0, i32 1
  store i32 4294967295, ptr %2404
  %2405 = getelementptr [4 x i32], ptr %stack.ptr_714, i32 0, i32 2
  %2406 = getelementptr [2 x i32], ptr %2382, i32 0, i32 1
  %2407 = load i32, ptr %2406
  store i32 %2407, ptr %2405
  %2408 = getelementptr [4 x i32], ptr %stack.ptr_714, i32 0, i32 3
  %2409 = getelementptr [2 x i32], ptr %2394, i32 0, i32 1
  %2410 = load i32, ptr %2409
  store i32 %2410, ptr %2408
  %2411 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %2411, ptr %stack.ptr_713, ptr %stack.ptr_715)
  %2412 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %2412, ptr %stack.ptr_714, ptr %stack.ptr_716)
  br label %loop_158
loop_158:
  %2413 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_715, ptr %stack.ptr_716)
  br i1 %2413, label %if_183, label %end_if_183
if_183:
  br label %range_query.end_153
end_if_183:
  %2414 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_715)
  %2415 = getelementptr [2 x i32], ptr %stack.ptr_717, i32 0, i32 0
  %2416 = getelementptr [2 x i32], ptr %2382, i32 0, i32 0
  %2417 = load i32, ptr %2416
  store i32 %2417, ptr %2415
  %2418 = getelementptr [2 x i32], ptr %stack.ptr_717, i32 0, i32 1
  %2419 = getelementptr [4 x i32], ptr %2414, i32 0, i32 0
  %2420 = load i32, ptr %2419
  store i32 %2420, ptr %2418
  %2421 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2422 = call ccc i1 @eclair_btree_contains_1(ptr %2421, ptr %stack.ptr_717)
  %2423 = select i1 %2422, i1 0, i1 1
  br i1 %2423, label %if_184, label %end_if_184
if_184:
  %2424 = getelementptr [2 x i32], ptr %stack.ptr_718, i32 0, i32 0
  %2425 = getelementptr [2 x i32], ptr %2382, i32 0, i32 0
  %2426 = load i32, ptr %2425
  store i32 %2426, ptr %2424
  %2427 = getelementptr [2 x i32], ptr %stack.ptr_718, i32 0, i32 1
  %2428 = getelementptr [4 x i32], ptr %2414, i32 0, i32 0
  %2429 = load i32, ptr %2428
  store i32 %2429, ptr %2427
  %2430 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2431 = call ccc i1 @eclair_btree_insert_value_1(ptr %2430, ptr %stack.ptr_718)
  br label %end_if_184
end_if_184:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_715)
  br label %loop_158
range_query.end_153:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_711)
  br label %loop_157
range_query.end_152:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_707)
  br label %loop_156
range_query.end_151:
  %2432 = getelementptr [2 x i32], ptr %stack.ptr_719, i32 0, i32 0
  store i32 0, ptr %2432
  %2433 = getelementptr [2 x i32], ptr %stack.ptr_719, i32 0, i32 1
  store i32 0, ptr %2433
  %2434 = getelementptr [2 x i32], ptr %stack.ptr_720, i32 0, i32 0
  store i32 4294967295, ptr %2434
  %2435 = getelementptr [2 x i32], ptr %stack.ptr_720, i32 0, i32 1
  store i32 4294967295, ptr %2435
  %2436 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %2436, ptr %stack.ptr_719, ptr %stack.ptr_721)
  %2437 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %2437, ptr %stack.ptr_720, ptr %stack.ptr_722)
  br label %loop_159
loop_159:
  %2438 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_721, ptr %stack.ptr_722)
  br i1 %2438, label %if_185, label %end_if_185
if_185:
  br label %range_query.end_154
end_if_185:
  %2439 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_721)
  %2440 = getelementptr [3 x i32], ptr %stack.ptr_723, i32 0, i32 0
  store i32 0, ptr %2440
  %2441 = getelementptr [3 x i32], ptr %stack.ptr_723, i32 0, i32 1
  store i32 0, ptr %2441
  %2442 = getelementptr [3 x i32], ptr %stack.ptr_723, i32 0, i32 2
  %2443 = getelementptr [2 x i32], ptr %2439, i32 0, i32 0
  %2444 = load i32, ptr %2443
  store i32 %2444, ptr %2442
  %2445 = getelementptr [3 x i32], ptr %stack.ptr_724, i32 0, i32 0
  store i32 4294967295, ptr %2445
  %2446 = getelementptr [3 x i32], ptr %stack.ptr_724, i32 0, i32 1
  store i32 4294967295, ptr %2446
  %2447 = getelementptr [3 x i32], ptr %stack.ptr_724, i32 0, i32 2
  %2448 = getelementptr [2 x i32], ptr %2439, i32 0, i32 0
  %2449 = load i32, ptr %2448
  store i32 %2449, ptr %2447
  %2450 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_lower_bound_8(ptr %2450, ptr %stack.ptr_723, ptr %stack.ptr_725)
  %2451 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_upper_bound_8(ptr %2451, ptr %stack.ptr_724, ptr %stack.ptr_726)
  br label %loop_160
loop_160:
  %2452 = call ccc i1 @eclair_btree_iterator_is_equal_8(ptr %stack.ptr_725, ptr %stack.ptr_726)
  br i1 %2452, label %if_186, label %end_if_186
if_186:
  br label %range_query.end_155
end_if_186:
  %2453 = call ccc ptr @eclair_btree_iterator_current_8(ptr %stack.ptr_725)
  %2454 = getelementptr [2 x i32], ptr %stack.ptr_727, i32 0, i32 0
  %2455 = getelementptr [3 x i32], ptr %2453, i32 0, i32 0
  %2456 = load i32, ptr %2455
  store i32 %2456, ptr %2454
  %2457 = getelementptr [2 x i32], ptr %stack.ptr_727, i32 0, i32 1
  store i32 0, ptr %2457
  %2458 = getelementptr [2 x i32], ptr %stack.ptr_728, i32 0, i32 0
  %2459 = getelementptr [3 x i32], ptr %2453, i32 0, i32 0
  %2460 = load i32, ptr %2459
  store i32 %2460, ptr %2458
  %2461 = getelementptr [2 x i32], ptr %stack.ptr_728, i32 0, i32 1
  store i32 4294967295, ptr %2461
  %2462 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2462, ptr %stack.ptr_727, ptr %stack.ptr_729)
  %2463 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2463, ptr %stack.ptr_728, ptr %stack.ptr_730)
  br label %loop_161
loop_161:
  %2464 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_729, ptr %stack.ptr_730)
  br i1 %2464, label %if_187, label %end_if_187
if_187:
  br label %range_query.end_156
end_if_187:
  %2465 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_729)
  %2466 = getelementptr [2 x i32], ptr %stack.ptr_731, i32 0, i32 0
  %2467 = getelementptr [2 x i32], ptr %2465, i32 0, i32 1
  %2468 = load i32, ptr %2467
  store i32 %2468, ptr %2466
  %2469 = getelementptr [2 x i32], ptr %stack.ptr_731, i32 0, i32 1
  store i32 0, ptr %2469
  %2470 = getelementptr [2 x i32], ptr %stack.ptr_732, i32 0, i32 0
  %2471 = getelementptr [2 x i32], ptr %2465, i32 0, i32 1
  %2472 = load i32, ptr %2471
  store i32 %2472, ptr %2470
  %2473 = getelementptr [2 x i32], ptr %stack.ptr_732, i32 0, i32 1
  store i32 4294967295, ptr %2473
  %2474 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2474, ptr %stack.ptr_731, ptr %stack.ptr_733)
  %2475 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2475, ptr %stack.ptr_732, ptr %stack.ptr_734)
  br label %loop_162
loop_162:
  %2476 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_733, ptr %stack.ptr_734)
  br i1 %2476, label %if_188, label %end_if_188
if_188:
  br label %range_query.end_157
end_if_188:
  %2477 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_733)
  %2478 = getelementptr [2 x i32], ptr %stack.ptr_735, i32 0, i32 0
  %2479 = getelementptr [3 x i32], ptr %2453, i32 0, i32 0
  %2480 = load i32, ptr %2479
  store i32 %2480, ptr %2478
  %2481 = getelementptr [2 x i32], ptr %stack.ptr_735, i32 0, i32 1
  %2482 = getelementptr [2 x i32], ptr %2465, i32 0, i32 1
  %2483 = load i32, ptr %2482
  store i32 %2483, ptr %2481
  %2484 = getelementptr [2 x i32], ptr %stack.ptr_736, i32 0, i32 0
  %2485 = getelementptr [3 x i32], ptr %2453, i32 0, i32 0
  %2486 = load i32, ptr %2485
  store i32 %2486, ptr %2484
  %2487 = getelementptr [2 x i32], ptr %stack.ptr_736, i32 0, i32 1
  %2488 = getelementptr [2 x i32], ptr %2465, i32 0, i32 1
  %2489 = load i32, ptr %2488
  store i32 %2489, ptr %2487
  %2490 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_lower_bound_1(ptr %2490, ptr %stack.ptr_735, ptr %stack.ptr_737)
  %2491 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_upper_bound_1(ptr %2491, ptr %stack.ptr_736, ptr %stack.ptr_738)
  br label %loop_163
loop_163:
  %2492 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_737, ptr %stack.ptr_738)
  br i1 %2492, label %if_189, label %end_if_189
if_189:
  br label %range_query.end_158
end_if_189:
  %2493 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_737)
  %2494 = getelementptr [2 x i32], ptr %stack.ptr_739, i32 0, i32 0
  %2495 = getelementptr [2 x i32], ptr %2439, i32 0, i32 0
  %2496 = load i32, ptr %2495
  store i32 %2496, ptr %2494
  %2497 = getelementptr [2 x i32], ptr %stack.ptr_739, i32 0, i32 1
  store i32 0, ptr %2497
  %2498 = getelementptr [2 x i32], ptr %stack.ptr_740, i32 0, i32 0
  %2499 = getelementptr [2 x i32], ptr %2439, i32 0, i32 0
  %2500 = load i32, ptr %2499
  store i32 %2500, ptr %2498
  %2501 = getelementptr [2 x i32], ptr %stack.ptr_740, i32 0, i32 1
  store i32 4294967295, ptr %2501
  %2502 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2502, ptr %stack.ptr_739, ptr %stack.ptr_741)
  %2503 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2503, ptr %stack.ptr_740, ptr %stack.ptr_742)
  br label %loop_164
loop_164:
  %2504 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_741, ptr %stack.ptr_742)
  br i1 %2504, label %if_190, label %end_if_190
if_190:
  br label %range_query.end_159
end_if_190:
  %2505 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_741)
  %2506 = getelementptr [2 x i32], ptr %stack.ptr_743, i32 0, i32 0
  %2507 = getelementptr [3 x i32], ptr %2453, i32 0, i32 0
  %2508 = load i32, ptr %2507
  store i32 %2508, ptr %2506
  %2509 = getelementptr [2 x i32], ptr %stack.ptr_743, i32 0, i32 1
  %2510 = getelementptr [2 x i32], ptr %2505, i32 0, i32 1
  %2511 = load i32, ptr %2510
  store i32 %2511, ptr %2509
  %2512 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2513 = call ccc i1 @eclair_btree_contains_1(ptr %2512, ptr %stack.ptr_743)
  %2514 = select i1 %2513, i1 0, i1 1
  br i1 %2514, label %if_191, label %end_if_192
if_191:
  %2515 = getelementptr [2 x i32], ptr %stack.ptr_744, i32 0, i32 0
  %2516 = getelementptr [2 x i32], ptr %2505, i32 0, i32 1
  %2517 = load i32, ptr %2516
  store i32 %2517, ptr %2515
  %2518 = getelementptr [2 x i32], ptr %stack.ptr_744, i32 0, i32 1
  %2519 = getelementptr [2 x i32], ptr %2477, i32 0, i32 1
  %2520 = load i32, ptr %2519
  store i32 %2520, ptr %2518
  %2521 = getelementptr [2 x i32], ptr %stack.ptr_745, i32 0, i32 0
  %2522 = getelementptr [2 x i32], ptr %2505, i32 0, i32 1
  %2523 = load i32, ptr %2522
  store i32 %2523, ptr %2521
  %2524 = getelementptr [2 x i32], ptr %stack.ptr_745, i32 0, i32 1
  %2525 = getelementptr [2 x i32], ptr %2477, i32 0, i32 1
  %2526 = load i32, ptr %2525
  store i32 %2526, ptr %2524
  %2527 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2527, ptr %stack.ptr_744, ptr %stack.ptr_746)
  %2528 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2528, ptr %stack.ptr_745, ptr %stack.ptr_747)
  br label %loop_165
loop_165:
  %2529 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_746, ptr %stack.ptr_747)
  br i1 %2529, label %if_192, label %end_if_191
if_192:
  br label %range_query.end_160
end_if_191:
  %2530 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_746)
  %2531 = getelementptr [2 x i32], ptr %stack.ptr_748, i32 0, i32 0
  %2532 = getelementptr [3 x i32], ptr %2453, i32 0, i32 0
  %2533 = load i32, ptr %2532
  store i32 %2533, ptr %2531
  %2534 = getelementptr [2 x i32], ptr %stack.ptr_748, i32 0, i32 1
  %2535 = getelementptr [2 x i32], ptr %2505, i32 0, i32 1
  %2536 = load i32, ptr %2535
  store i32 %2536, ptr %2534
  %2537 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2538 = call ccc i1 @eclair_btree_insert_value_1(ptr %2537, ptr %stack.ptr_748)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_746)
  br label %loop_165
range_query.end_160:
  br label %end_if_192
end_if_192:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_741)
  br label %loop_164
range_query.end_159:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_737)
  br label %loop_163
range_query.end_158:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_733)
  br label %loop_162
range_query.end_157:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_729)
  br label %loop_161
range_query.end_156:
  call ccc void @eclair_btree_iterator_next_8(ptr %stack.ptr_725)
  br label %loop_160
range_query.end_155:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_721)
  br label %loop_159
range_query.end_154:
  %2539 = getelementptr [2 x i32], ptr %stack.ptr_749, i32 0, i32 0
  store i32 0, ptr %2539
  %2540 = getelementptr [2 x i32], ptr %stack.ptr_749, i32 0, i32 1
  store i32 0, ptr %2540
  %2541 = getelementptr [2 x i32], ptr %stack.ptr_750, i32 0, i32 0
  store i32 4294967295, ptr %2541
  %2542 = getelementptr [2 x i32], ptr %stack.ptr_750, i32 0, i32 1
  store i32 4294967295, ptr %2542
  %2543 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2543, ptr %stack.ptr_749, ptr %stack.ptr_751)
  %2544 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2544, ptr %stack.ptr_750, ptr %stack.ptr_752)
  br label %loop_166
loop_166:
  %2545 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_751, ptr %stack.ptr_752)
  br i1 %2545, label %if_193, label %end_if_193
if_193:
  br label %range_query.end_161
end_if_193:
  %2546 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_751)
  %2547 = getelementptr [2 x i32], ptr %stack.ptr_753, i32 0, i32 0
  %2548 = getelementptr [2 x i32], ptr %2546, i32 0, i32 1
  %2549 = load i32, ptr %2548
  store i32 %2549, ptr %2547
  %2550 = getelementptr [2 x i32], ptr %stack.ptr_753, i32 0, i32 1
  store i32 0, ptr %2550
  %2551 = getelementptr [2 x i32], ptr %stack.ptr_754, i32 0, i32 0
  %2552 = getelementptr [2 x i32], ptr %2546, i32 0, i32 1
  %2553 = load i32, ptr %2552
  store i32 %2553, ptr %2551
  %2554 = getelementptr [2 x i32], ptr %stack.ptr_754, i32 0, i32 1
  store i32 4294967295, ptr %2554
  %2555 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2555, ptr %stack.ptr_753, ptr %stack.ptr_755)
  %2556 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2556, ptr %stack.ptr_754, ptr %stack.ptr_756)
  br label %loop_167
loop_167:
  %2557 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_755, ptr %stack.ptr_756)
  br i1 %2557, label %if_194, label %end_if_194
if_194:
  br label %range_query.end_162
end_if_194:
  %2558 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_755)
  %2559 = getelementptr [2 x i32], ptr %stack.ptr_757, i32 0, i32 0
  %2560 = getelementptr [2 x i32], ptr %2546, i32 0, i32 0
  %2561 = load i32, ptr %2560
  store i32 %2561, ptr %2559
  %2562 = getelementptr [2 x i32], ptr %stack.ptr_757, i32 0, i32 1
  %2563 = getelementptr [2 x i32], ptr %2546, i32 0, i32 1
  %2564 = load i32, ptr %2563
  store i32 %2564, ptr %2562
  %2565 = getelementptr [2 x i32], ptr %stack.ptr_758, i32 0, i32 0
  %2566 = getelementptr [2 x i32], ptr %2546, i32 0, i32 0
  %2567 = load i32, ptr %2566
  store i32 %2567, ptr %2565
  %2568 = getelementptr [2 x i32], ptr %stack.ptr_758, i32 0, i32 1
  %2569 = getelementptr [2 x i32], ptr %2546, i32 0, i32 1
  %2570 = load i32, ptr %2569
  store i32 %2570, ptr %2568
  %2571 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_lower_bound_1(ptr %2571, ptr %stack.ptr_757, ptr %stack.ptr_759)
  %2572 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_upper_bound_1(ptr %2572, ptr %stack.ptr_758, ptr %stack.ptr_760)
  br label %loop_168
loop_168:
  %2573 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_759, ptr %stack.ptr_760)
  br i1 %2573, label %if_195, label %end_if_195
if_195:
  br label %range_query.end_163
end_if_195:
  %2574 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_759)
  %2575 = getelementptr [2 x i32], ptr %stack.ptr_761, i32 0, i32 0
  %2576 = getelementptr [2 x i32], ptr %2546, i32 0, i32 0
  %2577 = load i32, ptr %2576
  store i32 %2577, ptr %2575
  %2578 = getelementptr [2 x i32], ptr %stack.ptr_761, i32 0, i32 1
  store i32 0, ptr %2578
  %2579 = getelementptr [2 x i32], ptr %stack.ptr_762, i32 0, i32 0
  %2580 = getelementptr [2 x i32], ptr %2546, i32 0, i32 0
  %2581 = load i32, ptr %2580
  store i32 %2581, ptr %2579
  %2582 = getelementptr [2 x i32], ptr %stack.ptr_762, i32 0, i32 1
  store i32 4294967295, ptr %2582
  %2583 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2583, ptr %stack.ptr_761, ptr %stack.ptr_763)
  %2584 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2584, ptr %stack.ptr_762, ptr %stack.ptr_764)
  br label %loop_169
loop_169:
  %2585 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_763, ptr %stack.ptr_764)
  br i1 %2585, label %if_196, label %end_if_196
if_196:
  br label %range_query.end_164
end_if_196:
  %2586 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_763)
  %2587 = getelementptr [2 x i32], ptr %stack.ptr_765, i32 0, i32 0
  %2588 = getelementptr [2 x i32], ptr %2546, i32 0, i32 0
  %2589 = load i32, ptr %2588
  store i32 %2589, ptr %2587
  %2590 = getelementptr [2 x i32], ptr %stack.ptr_765, i32 0, i32 1
  %2591 = getelementptr [2 x i32], ptr %2586, i32 0, i32 1
  %2592 = load i32, ptr %2591
  store i32 %2592, ptr %2590
  %2593 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2594 = call ccc i1 @eclair_btree_contains_1(ptr %2593, ptr %stack.ptr_765)
  %2595 = select i1 %2594, i1 0, i1 1
  br i1 %2595, label %if_197, label %end_if_198
if_197:
  %2596 = getelementptr [2 x i32], ptr %stack.ptr_766, i32 0, i32 0
  %2597 = getelementptr [2 x i32], ptr %2586, i32 0, i32 1
  %2598 = load i32, ptr %2597
  store i32 %2598, ptr %2596
  %2599 = getelementptr [2 x i32], ptr %stack.ptr_766, i32 0, i32 1
  %2600 = getelementptr [2 x i32], ptr %2558, i32 0, i32 1
  %2601 = load i32, ptr %2600
  store i32 %2601, ptr %2599
  %2602 = getelementptr [2 x i32], ptr %stack.ptr_767, i32 0, i32 0
  %2603 = getelementptr [2 x i32], ptr %2586, i32 0, i32 1
  %2604 = load i32, ptr %2603
  store i32 %2604, ptr %2602
  %2605 = getelementptr [2 x i32], ptr %stack.ptr_767, i32 0, i32 1
  %2606 = getelementptr [2 x i32], ptr %2558, i32 0, i32 1
  %2607 = load i32, ptr %2606
  store i32 %2607, ptr %2605
  %2608 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2608, ptr %stack.ptr_766, ptr %stack.ptr_768)
  %2609 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2609, ptr %stack.ptr_767, ptr %stack.ptr_769)
  br label %loop_170
loop_170:
  %2610 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_768, ptr %stack.ptr_769)
  br i1 %2610, label %if_198, label %end_if_197
if_198:
  br label %range_query.end_165
end_if_197:
  %2611 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_768)
  %2612 = getelementptr [2 x i32], ptr %stack.ptr_770, i32 0, i32 0
  %2613 = getelementptr [2 x i32], ptr %2546, i32 0, i32 0
  %2614 = load i32, ptr %2613
  store i32 %2614, ptr %2612
  %2615 = getelementptr [2 x i32], ptr %stack.ptr_770, i32 0, i32 1
  %2616 = getelementptr [2 x i32], ptr %2586, i32 0, i32 1
  %2617 = load i32, ptr %2616
  store i32 %2617, ptr %2615
  %2618 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2619 = call ccc i1 @eclair_btree_insert_value_1(ptr %2618, ptr %stack.ptr_770)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_768)
  br label %loop_170
range_query.end_165:
  br label %end_if_198
end_if_198:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_763)
  br label %loop_169
range_query.end_164:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_759)
  br label %loop_168
range_query.end_163:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_755)
  br label %loop_167
range_query.end_162:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_751)
  br label %loop_166
range_query.end_161:
  %2620 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2621 = call ccc i1 @eclair_btree_is_empty_1(ptr %2620)
  br i1 %2621, label %if_199, label %end_if_199
if_199:
  br label %loop.end_4
end_if_199:
  %2622 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  call ccc void @eclair_btree_begin_1(ptr %2622, ptr %stack.ptr_771)
  %2623 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  call ccc void @eclair_btree_end_1(ptr %2623, ptr %stack.ptr_772)
  %2624 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_insert_range_grounded_node_new_grounded_node(ptr %2624, ptr %stack.ptr_771, ptr %stack.ptr_772)
  %2625 = getelementptr %program, ptr %arg_0, i32 0, i32 42
  %2626 = getelementptr %program, ptr %arg_0, i32 0, i32 17
  call ccc void @eclair_btree_swap_1(ptr %2625, ptr %2626)
  br label %loop_144
loop.end_4:
  %2627 = getelementptr [2 x i32], ptr %stack.ptr_773, i32 0, i32 0
  store i32 0, ptr %2627
  %2628 = getelementptr [2 x i32], ptr %stack.ptr_773, i32 0, i32 1
  store i32 0, ptr %2628
  %2629 = getelementptr [2 x i32], ptr %stack.ptr_774, i32 0, i32 0
  store i32 4294967295, ptr %2629
  %2630 = getelementptr [2 x i32], ptr %stack.ptr_774, i32 0, i32 1
  store i32 4294967295, ptr %2630
  %2631 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_lower_bound_1(ptr %2631, ptr %stack.ptr_773, ptr %stack.ptr_775)
  %2632 = getelementptr %program, ptr %arg_0, i32 0, i32 40
  call ccc void @eclair_btree_upper_bound_1(ptr %2632, ptr %stack.ptr_774, ptr %stack.ptr_776)
  br label %loop_171
loop_171:
  %2633 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_775, ptr %stack.ptr_776)
  br i1 %2633, label %if_200, label %end_if_200
if_200:
  br label %range_query.end_166
end_if_200:
  %2634 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_775)
  %2635 = getelementptr [3 x i32], ptr %stack.ptr_777, i32 0, i32 0
  store i32 0, ptr %2635
  %2636 = getelementptr [3 x i32], ptr %stack.ptr_777, i32 0, i32 1
  store i32 0, ptr %2636
  %2637 = getelementptr [3 x i32], ptr %stack.ptr_777, i32 0, i32 2
  %2638 = getelementptr [2 x i32], ptr %2634, i32 0, i32 0
  %2639 = load i32, ptr %2638
  store i32 %2639, ptr %2637
  %2640 = getelementptr [3 x i32], ptr %stack.ptr_778, i32 0, i32 0
  store i32 4294967295, ptr %2640
  %2641 = getelementptr [3 x i32], ptr %stack.ptr_778, i32 0, i32 1
  store i32 4294967295, ptr %2641
  %2642 = getelementptr [3 x i32], ptr %stack.ptr_778, i32 0, i32 2
  %2643 = getelementptr [2 x i32], ptr %2634, i32 0, i32 0
  %2644 = load i32, ptr %2643
  store i32 %2644, ptr %2642
  %2645 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_lower_bound_8(ptr %2645, ptr %stack.ptr_777, ptr %stack.ptr_779)
  %2646 = getelementptr %program, ptr %arg_0, i32 0, i32 54
  call ccc void @eclair_btree_upper_bound_8(ptr %2646, ptr %stack.ptr_778, ptr %stack.ptr_780)
  br label %loop_172
loop_172:
  %2647 = call ccc i1 @eclair_btree_iterator_is_equal_8(ptr %stack.ptr_779, ptr %stack.ptr_780)
  br i1 %2647, label %if_201, label %end_if_201
if_201:
  br label %range_query.end_167
end_if_201:
  %2648 = call ccc ptr @eclair_btree_iterator_current_8(ptr %stack.ptr_779)
  %2649 = getelementptr [2 x i32], ptr %stack.ptr_781, i32 0, i32 0
  %2650 = getelementptr [2 x i32], ptr %2634, i32 0, i32 0
  %2651 = load i32, ptr %2650
  store i32 %2651, ptr %2649
  %2652 = getelementptr [2 x i32], ptr %stack.ptr_781, i32 0, i32 1
  store i32 0, ptr %2652
  %2653 = getelementptr [2 x i32], ptr %stack.ptr_782, i32 0, i32 0
  %2654 = getelementptr [2 x i32], ptr %2634, i32 0, i32 0
  %2655 = load i32, ptr %2654
  store i32 %2655, ptr %2653
  %2656 = getelementptr [2 x i32], ptr %stack.ptr_782, i32 0, i32 1
  store i32 4294967295, ptr %2656
  %2657 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2657, ptr %stack.ptr_781, ptr %stack.ptr_783)
  %2658 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2658, ptr %stack.ptr_782, ptr %stack.ptr_784)
  br label %loop_173
loop_173:
  %2659 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_783, ptr %stack.ptr_784)
  br i1 %2659, label %if_202, label %end_if_202
if_202:
  br label %range_query.end_168
end_if_202:
  %2660 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_783)
  %2661 = getelementptr [2 x i32], ptr %stack.ptr_785, i32 0, i32 0
  %2662 = getelementptr [3 x i32], ptr %2648, i32 0, i32 0
  %2663 = load i32, ptr %2662
  store i32 %2663, ptr %2661
  %2664 = getelementptr [2 x i32], ptr %stack.ptr_785, i32 0, i32 1
  %2665 = getelementptr [2 x i32], ptr %2660, i32 0, i32 1
  %2666 = load i32, ptr %2665
  store i32 %2666, ptr %2664
  %2667 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2668 = call ccc i1 @eclair_btree_contains_1(ptr %2667, ptr %stack.ptr_785)
  %2669 = select i1 %2668, i1 0, i1 1
  br i1 %2669, label %if_203, label %end_if_204
if_203:
  %2670 = getelementptr [2 x i32], ptr %stack.ptr_786, i32 0, i32 0
  %2671 = getelementptr [2 x i32], ptr %2660, i32 0, i32 1
  %2672 = load i32, ptr %2671
  store i32 %2672, ptr %2670
  %2673 = getelementptr [2 x i32], ptr %stack.ptr_786, i32 0, i32 1
  store i32 0, ptr %2673
  %2674 = getelementptr [2 x i32], ptr %stack.ptr_787, i32 0, i32 0
  %2675 = getelementptr [2 x i32], ptr %2660, i32 0, i32 1
  %2676 = load i32, ptr %2675
  store i32 %2676, ptr %2674
  %2677 = getelementptr [2 x i32], ptr %stack.ptr_787, i32 0, i32 1
  store i32 4294967295, ptr %2677
  %2678 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %2678, ptr %stack.ptr_786, ptr %stack.ptr_788)
  %2679 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %2679, ptr %stack.ptr_787, ptr %stack.ptr_789)
  br label %loop_174
loop_174:
  %2680 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_788, ptr %stack.ptr_789)
  br i1 %2680, label %if_204, label %end_if_203
if_204:
  br label %range_query.end_169
end_if_203:
  %2681 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_788)
  %2682 = getelementptr [3 x i32], ptr %stack.ptr_790, i32 0, i32 0
  %2683 = getelementptr [3 x i32], ptr %2648, i32 0, i32 0
  %2684 = load i32, ptr %2683
  store i32 %2684, ptr %2682
  %2685 = getelementptr [3 x i32], ptr %stack.ptr_790, i32 0, i32 1
  %2686 = getelementptr [2 x i32], ptr %2660, i32 0, i32 1
  %2687 = load i32, ptr %2686
  store i32 %2687, ptr %2685
  %2688 = getelementptr [3 x i32], ptr %stack.ptr_790, i32 0, i32 2
  %2689 = getelementptr [2 x i32], ptr %2681, i32 0, i32 1
  %2690 = load i32, ptr %2689
  store i32 %2690, ptr %2688
  %2691 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %2692 = call ccc i1 @eclair_btree_insert_value_0(ptr %2691, ptr %stack.ptr_790)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_788)
  br label %loop_174
range_query.end_169:
  br label %end_if_204
end_if_204:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_783)
  br label %loop_173
range_query.end_168:
  call ccc void @eclair_btree_iterator_next_8(ptr %stack.ptr_779)
  br label %loop_172
range_query.end_167:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_775)
  br label %loop_171
range_query.end_166:
  %2693 = getelementptr [2 x i32], ptr %stack.ptr_791, i32 0, i32 0
  store i32 0, ptr %2693
  %2694 = getelementptr [2 x i32], ptr %stack.ptr_791, i32 0, i32 1
  store i32 0, ptr %2694
  %2695 = getelementptr [2 x i32], ptr %stack.ptr_792, i32 0, i32 0
  store i32 4294967295, ptr %2695
  %2696 = getelementptr [2 x i32], ptr %stack.ptr_792, i32 0, i32 1
  store i32 4294967295, ptr %2696
  %2697 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_lower_bound_1(ptr %2697, ptr %stack.ptr_791, ptr %stack.ptr_793)
  %2698 = getelementptr %program, ptr %arg_0, i32 0, i32 39
  call ccc void @eclair_btree_upper_bound_1(ptr %2698, ptr %stack.ptr_792, ptr %stack.ptr_794)
  br label %loop_175
loop_175:
  %2699 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_793, ptr %stack.ptr_794)
  br i1 %2699, label %if_205, label %end_if_205
if_205:
  br label %range_query.end_170
end_if_205:
  %2700 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_793)
  %2701 = getelementptr [3 x i32], ptr %stack.ptr_795, i32 0, i32 0
  %2702 = getelementptr [2 x i32], ptr %2700, i32 0, i32 1
  %2703 = load i32, ptr %2702
  store i32 %2703, ptr %2701
  %2704 = getelementptr [3 x i32], ptr %stack.ptr_795, i32 0, i32 1
  store i32 0, ptr %2704
  %2705 = getelementptr [3 x i32], ptr %stack.ptr_795, i32 0, i32 2
  store i32 0, ptr %2705
  %2706 = getelementptr [3 x i32], ptr %stack.ptr_796, i32 0, i32 0
  %2707 = getelementptr [2 x i32], ptr %2700, i32 0, i32 1
  %2708 = load i32, ptr %2707
  store i32 %2708, ptr %2706
  %2709 = getelementptr [3 x i32], ptr %stack.ptr_796, i32 0, i32 1
  store i32 4294967295, ptr %2709
  %2710 = getelementptr [3 x i32], ptr %stack.ptr_796, i32 0, i32 2
  store i32 4294967295, ptr %2710
  %2711 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_lower_bound_0(ptr %2711, ptr %stack.ptr_795, ptr %stack.ptr_797)
  %2712 = getelementptr %program, ptr %arg_0, i32 0, i32 5
  call ccc void @eclair_btree_upper_bound_0(ptr %2712, ptr %stack.ptr_796, ptr %stack.ptr_798)
  br label %loop_176
loop_176:
  %2713 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_797, ptr %stack.ptr_798)
  br i1 %2713, label %if_206, label %end_if_206
if_206:
  br label %range_query.end_171
end_if_206:
  %2714 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_797)
  %2715 = getelementptr [2 x i32], ptr %stack.ptr_799, i32 0, i32 0
  %2716 = getelementptr [2 x i32], ptr %2700, i32 0, i32 1
  %2717 = load i32, ptr %2716
  store i32 %2717, ptr %2715
  %2718 = getelementptr [2 x i32], ptr %stack.ptr_799, i32 0, i32 1
  %2719 = getelementptr [3 x i32], ptr %2714, i32 0, i32 2
  %2720 = load i32, ptr %2719
  store i32 %2720, ptr %2718
  %2721 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2722 = call ccc i1 @eclair_btree_contains_1(ptr %2721, ptr %stack.ptr_799)
  %2723 = select i1 %2722, i1 0, i1 1
  br i1 %2723, label %if_207, label %end_if_208
if_207:
  %2724 = getelementptr [2 x i32], ptr %stack.ptr_800, i32 0, i32 0
  %2725 = getelementptr [3 x i32], ptr %2714, i32 0, i32 2
  %2726 = load i32, ptr %2725
  store i32 %2726, ptr %2724
  %2727 = getelementptr [2 x i32], ptr %stack.ptr_800, i32 0, i32 1
  store i32 0, ptr %2727
  %2728 = getelementptr [2 x i32], ptr %stack.ptr_801, i32 0, i32 0
  %2729 = getelementptr [3 x i32], ptr %2714, i32 0, i32 2
  %2730 = load i32, ptr %2729
  store i32 %2730, ptr %2728
  %2731 = getelementptr [2 x i32], ptr %stack.ptr_801, i32 0, i32 1
  store i32 4294967295, ptr %2731
  %2732 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_lower_bound_1(ptr %2732, ptr %stack.ptr_800, ptr %stack.ptr_802)
  %2733 = getelementptr %program, ptr %arg_0, i32 0, i32 23
  call ccc void @eclair_btree_upper_bound_1(ptr %2733, ptr %stack.ptr_801, ptr %stack.ptr_803)
  br label %loop_177
loop_177:
  %2734 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_802, ptr %stack.ptr_803)
  br i1 %2734, label %if_208, label %end_if_207
if_208:
  br label %range_query.end_172
end_if_207:
  %2735 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_802)
  %2736 = getelementptr [3 x i32], ptr %stack.ptr_804, i32 0, i32 0
  %2737 = getelementptr [2 x i32], ptr %2700, i32 0, i32 1
  %2738 = load i32, ptr %2737
  store i32 %2738, ptr %2736
  %2739 = getelementptr [3 x i32], ptr %stack.ptr_804, i32 0, i32 1
  %2740 = getelementptr [3 x i32], ptr %2714, i32 0, i32 2
  %2741 = load i32, ptr %2740
  store i32 %2741, ptr %2739
  %2742 = getelementptr [3 x i32], ptr %stack.ptr_804, i32 0, i32 2
  %2743 = getelementptr [2 x i32], ptr %2735, i32 0, i32 1
  %2744 = load i32, ptr %2743
  store i32 %2744, ptr %2742
  %2745 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %2746 = call ccc i1 @eclair_btree_insert_value_0(ptr %2745, ptr %stack.ptr_804)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_802)
  br label %loop_177
range_query.end_172:
  br label %end_if_208
end_if_208:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_797)
  br label %loop_176
range_query.end_171:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_793)
  br label %loop_175
range_query.end_170:
  %2747 = getelementptr [3 x i32], ptr %stack.ptr_805, i32 0, i32 0
  store i32 0, ptr %2747
  %2748 = getelementptr [3 x i32], ptr %stack.ptr_805, i32 0, i32 1
  store i32 0, ptr %2748
  %2749 = getelementptr [3 x i32], ptr %stack.ptr_805, i32 0, i32 2
  store i32 0, ptr %2749
  %2750 = getelementptr [3 x i32], ptr %stack.ptr_806, i32 0, i32 0
  store i32 4294967295, ptr %2750
  %2751 = getelementptr [3 x i32], ptr %stack.ptr_806, i32 0, i32 1
  store i32 4294967295, ptr %2751
  %2752 = getelementptr [3 x i32], ptr %stack.ptr_806, i32 0, i32 2
  store i32 4294967295, ptr %2752
  %2753 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_lower_bound_0(ptr %2753, ptr %stack.ptr_805, ptr %stack.ptr_807)
  %2754 = getelementptr %program, ptr %arg_0, i32 0, i32 52
  call ccc void @eclair_btree_upper_bound_0(ptr %2754, ptr %stack.ptr_806, ptr %stack.ptr_808)
  br label %loop_178
loop_178:
  %2755 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_807, ptr %stack.ptr_808)
  br i1 %2755, label %if_209, label %end_if_209
if_209:
  br label %range_query.end_173
end_if_209:
  %2756 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_807)
  %2757 = getelementptr [2 x i32], ptr %stack.ptr_809, i32 0, i32 0
  %2758 = getelementptr [3 x i32], ptr %2756, i32 0, i32 0
  %2759 = load i32, ptr %2758
  store i32 %2759, ptr %2757
  %2760 = getelementptr [2 x i32], ptr %stack.ptr_809, i32 0, i32 1
  %2761 = getelementptr [3 x i32], ptr %2756, i32 0, i32 2
  %2762 = load i32, ptr %2761
  store i32 %2762, ptr %2760
  %2763 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2764 = call ccc i1 @eclair_btree_contains_1(ptr %2763, ptr %stack.ptr_809)
  %2765 = select i1 %2764, i1 0, i1 1
  br i1 %2765, label %if_210, label %end_if_212
if_210:
  %2766 = getelementptr [2 x i32], ptr %stack.ptr_810, i32 0, i32 0
  %2767 = getelementptr [3 x i32], ptr %2756, i32 0, i32 2
  %2768 = load i32, ptr %2767
  store i32 %2768, ptr %2766
  %2769 = getelementptr [2 x i32], ptr %stack.ptr_810, i32 0, i32 1
  store i32 0, ptr %2769
  %2770 = getelementptr [2 x i32], ptr %stack.ptr_811, i32 0, i32 0
  %2771 = getelementptr [3 x i32], ptr %2756, i32 0, i32 2
  %2772 = load i32, ptr %2771
  store i32 %2772, ptr %2770
  %2773 = getelementptr [2 x i32], ptr %stack.ptr_811, i32 0, i32 1
  store i32 4294967295, ptr %2773
  %2774 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %2774, ptr %stack.ptr_810, ptr %stack.ptr_812)
  %2775 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %2775, ptr %stack.ptr_811, ptr %stack.ptr_813)
  br label %loop_179
loop_179:
  %2776 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_812, ptr %stack.ptr_813)
  br i1 %2776, label %if_211, label %end_if_210
if_211:
  br label %range_query.end_174
end_if_210:
  %2777 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_812)
  %2778 = getelementptr [2 x i32], ptr %stack.ptr_814, i32 0, i32 0
  %2779 = getelementptr [3 x i32], ptr %2756, i32 0, i32 0
  %2780 = load i32, ptr %2779
  store i32 %2780, ptr %2778
  %2781 = getelementptr [2 x i32], ptr %stack.ptr_814, i32 0, i32 1
  %2782 = getelementptr [3 x i32], ptr %2756, i32 0, i32 2
  %2783 = load i32, ptr %2782
  store i32 %2783, ptr %2781
  %2784 = getelementptr [2 x i32], ptr %stack.ptr_815, i32 0, i32 0
  %2785 = getelementptr [3 x i32], ptr %2756, i32 0, i32 0
  %2786 = load i32, ptr %2785
  store i32 %2786, ptr %2784
  %2787 = getelementptr [2 x i32], ptr %stack.ptr_815, i32 0, i32 1
  %2788 = getelementptr [3 x i32], ptr %2756, i32 0, i32 2
  %2789 = load i32, ptr %2788
  store i32 %2789, ptr %2787
  %2790 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2790, ptr %stack.ptr_814, ptr %stack.ptr_816)
  %2791 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2791, ptr %stack.ptr_815, ptr %stack.ptr_817)
  br label %loop_180
loop_180:
  %2792 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_816, ptr %stack.ptr_817)
  br i1 %2792, label %if_212, label %end_if_211
if_212:
  br label %range_query.end_175
end_if_211:
  %2793 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_816)
  %2794 = getelementptr [3 x i32], ptr %stack.ptr_818, i32 0, i32 0
  %2795 = getelementptr [3 x i32], ptr %2756, i32 0, i32 0
  %2796 = load i32, ptr %2795
  store i32 %2796, ptr %2794
  %2797 = getelementptr [3 x i32], ptr %stack.ptr_818, i32 0, i32 1
  %2798 = getelementptr [3 x i32], ptr %2756, i32 0, i32 2
  %2799 = load i32, ptr %2798
  store i32 %2799, ptr %2797
  %2800 = getelementptr [3 x i32], ptr %stack.ptr_818, i32 0, i32 2
  %2801 = getelementptr [2 x i32], ptr %2777, i32 0, i32 1
  %2802 = load i32, ptr %2801
  store i32 %2802, ptr %2800
  %2803 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %2804 = call ccc i1 @eclair_btree_insert_value_0(ptr %2803, ptr %stack.ptr_818)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_816)
  br label %loop_180
range_query.end_175:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_812)
  br label %loop_179
range_query.end_174:
  br label %end_if_212
end_if_212:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_807)
  br label %loop_178
range_query.end_173:
  %2805 = getelementptr [3 x i32], ptr %stack.ptr_819, i32 0, i32 0
  store i32 0, ptr %2805
  %2806 = getelementptr [3 x i32], ptr %stack.ptr_819, i32 0, i32 1
  store i32 0, ptr %2806
  %2807 = getelementptr [3 x i32], ptr %stack.ptr_819, i32 0, i32 2
  store i32 0, ptr %2807
  %2808 = getelementptr [3 x i32], ptr %stack.ptr_820, i32 0, i32 0
  store i32 4294967295, ptr %2808
  %2809 = getelementptr [3 x i32], ptr %stack.ptr_820, i32 0, i32 1
  store i32 4294967295, ptr %2809
  %2810 = getelementptr [3 x i32], ptr %stack.ptr_820, i32 0, i32 2
  store i32 4294967295, ptr %2810
  %2811 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2811, ptr %stack.ptr_819, ptr %stack.ptr_821)
  %2812 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2812, ptr %stack.ptr_820, ptr %stack.ptr_822)
  br label %loop_181
loop_181:
  %2813 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_821, ptr %stack.ptr_822)
  br i1 %2813, label %if_213, label %end_if_213
if_213:
  br label %range_query.end_176
end_if_213:
  %2814 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_821)
  %2815 = getelementptr [4 x i32], ptr %stack.ptr_823, i32 0, i32 0
  %2816 = getelementptr [3 x i32], ptr %2814, i32 0, i32 2
  %2817 = load i32, ptr %2816
  store i32 %2817, ptr %2815
  %2818 = getelementptr [4 x i32], ptr %stack.ptr_823, i32 0, i32 1
  store i32 0, ptr %2818
  %2819 = getelementptr [4 x i32], ptr %stack.ptr_823, i32 0, i32 2
  store i32 0, ptr %2819
  %2820 = getelementptr [4 x i32], ptr %stack.ptr_823, i32 0, i32 3
  store i32 0, ptr %2820
  %2821 = getelementptr [4 x i32], ptr %stack.ptr_824, i32 0, i32 0
  %2822 = getelementptr [3 x i32], ptr %2814, i32 0, i32 2
  %2823 = load i32, ptr %2822
  store i32 %2823, ptr %2821
  %2824 = getelementptr [4 x i32], ptr %stack.ptr_824, i32 0, i32 1
  store i32 4294967295, ptr %2824
  %2825 = getelementptr [4 x i32], ptr %stack.ptr_824, i32 0, i32 2
  store i32 4294967295, ptr %2825
  %2826 = getelementptr [4 x i32], ptr %stack.ptr_824, i32 0, i32 3
  store i32 4294967295, ptr %2826
  %2827 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %2827, ptr %stack.ptr_823, ptr %stack.ptr_825)
  %2828 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %2828, ptr %stack.ptr_824, ptr %stack.ptr_826)
  br label %loop_182
loop_182:
  %2829 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_825, ptr %stack.ptr_826)
  br i1 %2829, label %if_214, label %end_if_214
if_214:
  br label %range_query.end_177
end_if_214:
  %2830 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_825)
  %2831 = getelementptr [2 x i32], ptr %stack.ptr_827, i32 0, i32 0
  %2832 = getelementptr [3 x i32], ptr %2814, i32 0, i32 0
  %2833 = load i32, ptr %2832
  store i32 %2833, ptr %2831
  %2834 = getelementptr [2 x i32], ptr %stack.ptr_827, i32 0, i32 1
  %2835 = getelementptr [4 x i32], ptr %2830, i32 0, i32 3
  %2836 = load i32, ptr %2835
  store i32 %2836, ptr %2834
  %2837 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2838 = call ccc i1 @eclair_btree_contains_1(ptr %2837, ptr %stack.ptr_827)
  %2839 = select i1 %2838, i1 0, i1 1
  br i1 %2839, label %if_215, label %end_if_218
if_215:
  %2840 = getelementptr [1 x i32], ptr %stack.ptr_828, i32 0, i32 0
  %2841 = getelementptr [4 x i32], ptr %2830, i32 0, i32 1
  %2842 = load i32, ptr %2841
  store i32 %2842, ptr %2840
  %2843 = getelementptr [1 x i32], ptr %stack.ptr_829, i32 0, i32 0
  %2844 = getelementptr [4 x i32], ptr %2830, i32 0, i32 1
  %2845 = load i32, ptr %2844
  store i32 %2845, ptr %2843
  %2846 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_lower_bound_6(ptr %2846, ptr %stack.ptr_828, ptr %stack.ptr_830)
  %2847 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_upper_bound_6(ptr %2847, ptr %stack.ptr_829, ptr %stack.ptr_831)
  br label %loop_183
loop_183:
  %2848 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_830, ptr %stack.ptr_831)
  br i1 %2848, label %if_216, label %end_if_215
if_216:
  br label %range_query.end_178
end_if_215:
  %2849 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_830)
  %2850 = getelementptr [2 x i32], ptr %stack.ptr_832, i32 0, i32 0
  %2851 = getelementptr [4 x i32], ptr %2830, i32 0, i32 3
  %2852 = load i32, ptr %2851
  store i32 %2852, ptr %2850
  %2853 = getelementptr [2 x i32], ptr %stack.ptr_832, i32 0, i32 1
  store i32 0, ptr %2853
  %2854 = getelementptr [2 x i32], ptr %stack.ptr_833, i32 0, i32 0
  %2855 = getelementptr [4 x i32], ptr %2830, i32 0, i32 3
  %2856 = load i32, ptr %2855
  store i32 %2856, ptr %2854
  %2857 = getelementptr [2 x i32], ptr %stack.ptr_833, i32 0, i32 1
  store i32 4294967295, ptr %2857
  %2858 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %2858, ptr %stack.ptr_832, ptr %stack.ptr_834)
  %2859 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %2859, ptr %stack.ptr_833, ptr %stack.ptr_835)
  br label %loop_184
loop_184:
  %2860 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_834, ptr %stack.ptr_835)
  br i1 %2860, label %if_217, label %end_if_216
if_217:
  br label %range_query.end_179
end_if_216:
  %2861 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_834)
  %2862 = getelementptr [2 x i32], ptr %stack.ptr_836, i32 0, i32 0
  %2863 = getelementptr [3 x i32], ptr %2814, i32 0, i32 0
  %2864 = load i32, ptr %2863
  store i32 %2864, ptr %2862
  %2865 = getelementptr [2 x i32], ptr %stack.ptr_836, i32 0, i32 1
  %2866 = getelementptr [4 x i32], ptr %2830, i32 0, i32 3
  %2867 = load i32, ptr %2866
  store i32 %2867, ptr %2865
  %2868 = getelementptr [2 x i32], ptr %stack.ptr_837, i32 0, i32 0
  %2869 = getelementptr [3 x i32], ptr %2814, i32 0, i32 0
  %2870 = load i32, ptr %2869
  store i32 %2870, ptr %2868
  %2871 = getelementptr [2 x i32], ptr %stack.ptr_837, i32 0, i32 1
  %2872 = getelementptr [4 x i32], ptr %2830, i32 0, i32 3
  %2873 = load i32, ptr %2872
  store i32 %2873, ptr %2871
  %2874 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2874, ptr %stack.ptr_836, ptr %stack.ptr_838)
  %2875 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2875, ptr %stack.ptr_837, ptr %stack.ptr_839)
  br label %loop_185
loop_185:
  %2876 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_838, ptr %stack.ptr_839)
  br i1 %2876, label %if_218, label %end_if_217
if_218:
  br label %range_query.end_180
end_if_217:
  %2877 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_838)
  %2878 = getelementptr [3 x i32], ptr %stack.ptr_840, i32 0, i32 0
  %2879 = getelementptr [3 x i32], ptr %2814, i32 0, i32 0
  %2880 = load i32, ptr %2879
  store i32 %2880, ptr %2878
  %2881 = getelementptr [3 x i32], ptr %stack.ptr_840, i32 0, i32 1
  %2882 = getelementptr [4 x i32], ptr %2830, i32 0, i32 3
  %2883 = load i32, ptr %2882
  store i32 %2883, ptr %2881
  %2884 = getelementptr [3 x i32], ptr %stack.ptr_840, i32 0, i32 2
  %2885 = getelementptr [2 x i32], ptr %2861, i32 0, i32 1
  %2886 = load i32, ptr %2885
  store i32 %2886, ptr %2884
  %2887 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %2888 = call ccc i1 @eclair_btree_insert_value_0(ptr %2887, ptr %stack.ptr_840)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_838)
  br label %loop_185
range_query.end_180:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_834)
  br label %loop_184
range_query.end_179:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_830)
  br label %loop_183
range_query.end_178:
  br label %end_if_218
end_if_218:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_825)
  br label %loop_182
range_query.end_177:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_821)
  br label %loop_181
range_query.end_176:
  %2889 = getelementptr [3 x i32], ptr %stack.ptr_841, i32 0, i32 0
  store i32 0, ptr %2889
  %2890 = getelementptr [3 x i32], ptr %stack.ptr_841, i32 0, i32 1
  store i32 0, ptr %2890
  %2891 = getelementptr [3 x i32], ptr %stack.ptr_841, i32 0, i32 2
  store i32 0, ptr %2891
  %2892 = getelementptr [3 x i32], ptr %stack.ptr_842, i32 0, i32 0
  store i32 4294967295, ptr %2892
  %2893 = getelementptr [3 x i32], ptr %stack.ptr_842, i32 0, i32 1
  store i32 4294967295, ptr %2893
  %2894 = getelementptr [3 x i32], ptr %stack.ptr_842, i32 0, i32 2
  store i32 4294967295, ptr %2894
  %2895 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %2895, ptr %stack.ptr_841, ptr %stack.ptr_843)
  %2896 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %2896, ptr %stack.ptr_842, ptr %stack.ptr_844)
  br label %loop_186
loop_186:
  %2897 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_843, ptr %stack.ptr_844)
  br i1 %2897, label %if_219, label %end_if_219
if_219:
  br label %range_query.end_181
end_if_219:
  %2898 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_843)
  %2899 = getelementptr [4 x i32], ptr %stack.ptr_845, i32 0, i32 0
  %2900 = getelementptr [3 x i32], ptr %2898, i32 0, i32 2
  %2901 = load i32, ptr %2900
  store i32 %2901, ptr %2899
  %2902 = getelementptr [4 x i32], ptr %stack.ptr_845, i32 0, i32 1
  store i32 0, ptr %2902
  %2903 = getelementptr [4 x i32], ptr %stack.ptr_845, i32 0, i32 2
  store i32 0, ptr %2903
  %2904 = getelementptr [4 x i32], ptr %stack.ptr_845, i32 0, i32 3
  store i32 0, ptr %2904
  %2905 = getelementptr [4 x i32], ptr %stack.ptr_846, i32 0, i32 0
  %2906 = getelementptr [3 x i32], ptr %2898, i32 0, i32 2
  %2907 = load i32, ptr %2906
  store i32 %2907, ptr %2905
  %2908 = getelementptr [4 x i32], ptr %stack.ptr_846, i32 0, i32 1
  store i32 4294967295, ptr %2908
  %2909 = getelementptr [4 x i32], ptr %stack.ptr_846, i32 0, i32 2
  store i32 4294967295, ptr %2909
  %2910 = getelementptr [4 x i32], ptr %stack.ptr_846, i32 0, i32 3
  store i32 4294967295, ptr %2910
  %2911 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %2911, ptr %stack.ptr_845, ptr %stack.ptr_847)
  %2912 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %2912, ptr %stack.ptr_846, ptr %stack.ptr_848)
  br label %loop_187
loop_187:
  %2913 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_847, ptr %stack.ptr_848)
  br i1 %2913, label %if_220, label %end_if_220
if_220:
  br label %range_query.end_182
end_if_220:
  %2914 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_847)
  %2915 = getelementptr [2 x i32], ptr %stack.ptr_849, i32 0, i32 0
  %2916 = getelementptr [3 x i32], ptr %2898, i32 0, i32 0
  %2917 = load i32, ptr %2916
  store i32 %2917, ptr %2915
  %2918 = getelementptr [2 x i32], ptr %stack.ptr_849, i32 0, i32 1
  %2919 = getelementptr [4 x i32], ptr %2914, i32 0, i32 2
  %2920 = load i32, ptr %2919
  store i32 %2920, ptr %2918
  %2921 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %2922 = call ccc i1 @eclair_btree_contains_1(ptr %2921, ptr %stack.ptr_849)
  %2923 = select i1 %2922, i1 0, i1 1
  br i1 %2923, label %if_221, label %end_if_224
if_221:
  %2924 = getelementptr [1 x i32], ptr %stack.ptr_850, i32 0, i32 0
  %2925 = getelementptr [4 x i32], ptr %2914, i32 0, i32 1
  %2926 = load i32, ptr %2925
  store i32 %2926, ptr %2924
  %2927 = getelementptr [1 x i32], ptr %stack.ptr_851, i32 0, i32 0
  %2928 = getelementptr [4 x i32], ptr %2914, i32 0, i32 1
  %2929 = load i32, ptr %2928
  store i32 %2929, ptr %2927
  %2930 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_lower_bound_6(ptr %2930, ptr %stack.ptr_850, ptr %stack.ptr_852)
  %2931 = getelementptr %program, ptr %arg_0, i32 0, i32 31
  call ccc void @eclair_btree_upper_bound_6(ptr %2931, ptr %stack.ptr_851, ptr %stack.ptr_853)
  br label %loop_188
loop_188:
  %2932 = call ccc i1 @eclair_btree_iterator_is_equal_6(ptr %stack.ptr_852, ptr %stack.ptr_853)
  br i1 %2932, label %if_222, label %end_if_221
if_222:
  br label %range_query.end_183
end_if_221:
  %2933 = call ccc ptr @eclair_btree_iterator_current_6(ptr %stack.ptr_852)
  %2934 = getelementptr [2 x i32], ptr %stack.ptr_854, i32 0, i32 0
  %2935 = getelementptr [4 x i32], ptr %2914, i32 0, i32 2
  %2936 = load i32, ptr %2935
  store i32 %2936, ptr %2934
  %2937 = getelementptr [2 x i32], ptr %stack.ptr_854, i32 0, i32 1
  store i32 0, ptr %2937
  %2938 = getelementptr [2 x i32], ptr %stack.ptr_855, i32 0, i32 0
  %2939 = getelementptr [4 x i32], ptr %2914, i32 0, i32 2
  %2940 = load i32, ptr %2939
  store i32 %2940, ptr %2938
  %2941 = getelementptr [2 x i32], ptr %stack.ptr_855, i32 0, i32 1
  store i32 4294967295, ptr %2941
  %2942 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %2942, ptr %stack.ptr_854, ptr %stack.ptr_856)
  %2943 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %2943, ptr %stack.ptr_855, ptr %stack.ptr_857)
  br label %loop_189
loop_189:
  %2944 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_856, ptr %stack.ptr_857)
  br i1 %2944, label %if_223, label %end_if_222
if_223:
  br label %range_query.end_184
end_if_222:
  %2945 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_856)
  %2946 = getelementptr [2 x i32], ptr %stack.ptr_858, i32 0, i32 0
  %2947 = getelementptr [3 x i32], ptr %2898, i32 0, i32 0
  %2948 = load i32, ptr %2947
  store i32 %2948, ptr %2946
  %2949 = getelementptr [2 x i32], ptr %stack.ptr_858, i32 0, i32 1
  %2950 = getelementptr [4 x i32], ptr %2914, i32 0, i32 2
  %2951 = load i32, ptr %2950
  store i32 %2951, ptr %2949
  %2952 = getelementptr [2 x i32], ptr %stack.ptr_859, i32 0, i32 0
  %2953 = getelementptr [3 x i32], ptr %2898, i32 0, i32 0
  %2954 = load i32, ptr %2953
  store i32 %2954, ptr %2952
  %2955 = getelementptr [2 x i32], ptr %stack.ptr_859, i32 0, i32 1
  %2956 = getelementptr [4 x i32], ptr %2914, i32 0, i32 2
  %2957 = load i32, ptr %2956
  store i32 %2957, ptr %2955
  %2958 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_lower_bound_1(ptr %2958, ptr %stack.ptr_858, ptr %stack.ptr_860)
  %2959 = getelementptr %program, ptr %arg_0, i32 0, i32 59
  call ccc void @eclair_btree_upper_bound_1(ptr %2959, ptr %stack.ptr_859, ptr %stack.ptr_861)
  br label %loop_190
loop_190:
  %2960 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_860, ptr %stack.ptr_861)
  br i1 %2960, label %if_224, label %end_if_223
if_224:
  br label %range_query.end_185
end_if_223:
  %2961 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_860)
  %2962 = getelementptr [3 x i32], ptr %stack.ptr_862, i32 0, i32 0
  %2963 = getelementptr [3 x i32], ptr %2898, i32 0, i32 0
  %2964 = load i32, ptr %2963
  store i32 %2964, ptr %2962
  %2965 = getelementptr [3 x i32], ptr %stack.ptr_862, i32 0, i32 1
  %2966 = getelementptr [4 x i32], ptr %2914, i32 0, i32 2
  %2967 = load i32, ptr %2966
  store i32 %2967, ptr %2965
  %2968 = getelementptr [3 x i32], ptr %stack.ptr_862, i32 0, i32 2
  %2969 = getelementptr [2 x i32], ptr %2945, i32 0, i32 1
  %2970 = load i32, ptr %2969
  store i32 %2970, ptr %2968
  %2971 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %2972 = call ccc i1 @eclair_btree_insert_value_0(ptr %2971, ptr %stack.ptr_862)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_860)
  br label %loop_190
range_query.end_185:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_856)
  br label %loop_189
range_query.end_184:
  call ccc void @eclair_btree_iterator_next_6(ptr %stack.ptr_852)
  br label %loop_188
range_query.end_183:
  br label %end_if_224
end_if_224:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_847)
  br label %loop_187
range_query.end_182:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_843)
  br label %loop_186
range_query.end_181:
  %2973 = getelementptr [4 x i32], ptr %stack.ptr_863, i32 0, i32 0
  store i32 0, ptr %2973
  %2974 = getelementptr [4 x i32], ptr %stack.ptr_863, i32 0, i32 1
  store i32 0, ptr %2974
  %2975 = getelementptr [4 x i32], ptr %stack.ptr_863, i32 0, i32 2
  store i32 0, ptr %2975
  %2976 = getelementptr [4 x i32], ptr %stack.ptr_863, i32 0, i32 3
  store i32 0, ptr %2976
  %2977 = getelementptr [4 x i32], ptr %stack.ptr_864, i32 0, i32 0
  store i32 4294967295, ptr %2977
  %2978 = getelementptr [4 x i32], ptr %stack.ptr_864, i32 0, i32 1
  store i32 4294967295, ptr %2978
  %2979 = getelementptr [4 x i32], ptr %stack.ptr_864, i32 0, i32 2
  store i32 4294967295, ptr %2979
  %2980 = getelementptr [4 x i32], ptr %stack.ptr_864, i32 0, i32 3
  store i32 4294967295, ptr %2980
  %2981 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %2981, ptr %stack.ptr_863, ptr %stack.ptr_865)
  %2982 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %2982, ptr %stack.ptr_864, ptr %stack.ptr_866)
  br label %loop_191
loop_191:
  %2983 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_865, ptr %stack.ptr_866)
  br i1 %2983, label %if_225, label %end_if_225
if_225:
  br label %range_query.end_186
end_if_225:
  %2984 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_865)
  %2985 = getelementptr [2 x i32], ptr %stack.ptr_867, i32 0, i32 0
  %2986 = getelementptr [4 x i32], ptr %2984, i32 0, i32 3
  %2987 = load i32, ptr %2986
  store i32 %2987, ptr %2985
  %2988 = getelementptr [2 x i32], ptr %stack.ptr_867, i32 0, i32 1
  store i32 0, ptr %2988
  %2989 = getelementptr [2 x i32], ptr %stack.ptr_868, i32 0, i32 0
  %2990 = getelementptr [4 x i32], ptr %2984, i32 0, i32 3
  %2991 = load i32, ptr %2990
  store i32 %2991, ptr %2989
  %2992 = getelementptr [2 x i32], ptr %stack.ptr_868, i32 0, i32 1
  store i32 4294967295, ptr %2992
  %2993 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %2993, ptr %stack.ptr_867, ptr %stack.ptr_869)
  %2994 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %2994, ptr %stack.ptr_868, ptr %stack.ptr_870)
  br label %loop_192
loop_192:
  %2995 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_869, ptr %stack.ptr_870)
  br i1 %2995, label %if_226, label %end_if_226
if_226:
  br label %range_query.end_187
end_if_226:
  %2996 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_869)
  %2997 = getelementptr [2 x i32], ptr %stack.ptr_871, i32 0, i32 0
  store i32 0, ptr %2997
  %2998 = getelementptr [2 x i32], ptr %stack.ptr_871, i32 0, i32 1
  %2999 = getelementptr [4 x i32], ptr %2984, i32 0, i32 3
  %3000 = load i32, ptr %2999
  store i32 %3000, ptr %2998
  %3001 = getelementptr [2 x i32], ptr %stack.ptr_872, i32 0, i32 0
  store i32 4294967295, ptr %3001
  %3002 = getelementptr [2 x i32], ptr %stack.ptr_872, i32 0, i32 1
  %3003 = getelementptr [4 x i32], ptr %2984, i32 0, i32 3
  %3004 = load i32, ptr %3003
  store i32 %3004, ptr %3002
  %3005 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_lower_bound_2(ptr %3005, ptr %stack.ptr_871, ptr %stack.ptr_873)
  %3006 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_upper_bound_2(ptr %3006, ptr %stack.ptr_872, ptr %stack.ptr_874)
  br label %loop_193
loop_193:
  %3007 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_873, ptr %stack.ptr_874)
  br i1 %3007, label %if_227, label %end_if_227
if_227:
  br label %range_query.end_188
end_if_227:
  %3008 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_873)
  %3009 = getelementptr [2 x i32], ptr %stack.ptr_875, i32 0, i32 0
  %3010 = getelementptr [2 x i32], ptr %3008, i32 0, i32 0
  %3011 = load i32, ptr %3010
  store i32 %3011, ptr %3009
  %3012 = getelementptr [2 x i32], ptr %stack.ptr_875, i32 0, i32 1
  %3013 = getelementptr [4 x i32], ptr %2984, i32 0, i32 3
  %3014 = load i32, ptr %3013
  store i32 %3014, ptr %3012
  %3015 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %3016 = call ccc i1 @eclair_btree_contains_1(ptr %3015, ptr %stack.ptr_875)
  %3017 = select i1 %3016, i1 0, i1 1
  br i1 %3017, label %if_228, label %end_if_228
if_228:
  %3018 = getelementptr [3 x i32], ptr %stack.ptr_876, i32 0, i32 0
  %3019 = getelementptr [2 x i32], ptr %3008, i32 0, i32 0
  %3020 = load i32, ptr %3019
  store i32 %3020, ptr %3018
  %3021 = getelementptr [3 x i32], ptr %stack.ptr_876, i32 0, i32 1
  %3022 = getelementptr [4 x i32], ptr %2984, i32 0, i32 3
  %3023 = load i32, ptr %3022
  store i32 %3023, ptr %3021
  %3024 = getelementptr [3 x i32], ptr %stack.ptr_876, i32 0, i32 2
  %3025 = getelementptr [2 x i32], ptr %2996, i32 0, i32 1
  %3026 = load i32, ptr %3025
  store i32 %3026, ptr %3024
  %3027 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %3028 = call ccc i1 @eclair_btree_insert_value_0(ptr %3027, ptr %stack.ptr_876)
  br label %end_if_228
end_if_228:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_873)
  br label %loop_193
range_query.end_188:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_869)
  br label %loop_192
range_query.end_187:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_865)
  br label %loop_191
range_query.end_186:
  %3029 = getelementptr [4 x i32], ptr %stack.ptr_877, i32 0, i32 0
  store i32 0, ptr %3029
  %3030 = getelementptr [4 x i32], ptr %stack.ptr_877, i32 0, i32 1
  store i32 0, ptr %3030
  %3031 = getelementptr [4 x i32], ptr %stack.ptr_877, i32 0, i32 2
  store i32 0, ptr %3031
  %3032 = getelementptr [4 x i32], ptr %stack.ptr_877, i32 0, i32 3
  store i32 0, ptr %3032
  %3033 = getelementptr [4 x i32], ptr %stack.ptr_878, i32 0, i32 0
  store i32 4294967295, ptr %3033
  %3034 = getelementptr [4 x i32], ptr %stack.ptr_878, i32 0, i32 1
  store i32 4294967295, ptr %3034
  %3035 = getelementptr [4 x i32], ptr %stack.ptr_878, i32 0, i32 2
  store i32 4294967295, ptr %3035
  %3036 = getelementptr [4 x i32], ptr %stack.ptr_878, i32 0, i32 3
  store i32 4294967295, ptr %3036
  %3037 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %3037, ptr %stack.ptr_877, ptr %stack.ptr_879)
  %3038 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %3038, ptr %stack.ptr_878, ptr %stack.ptr_880)
  br label %loop_194
loop_194:
  %3039 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_879, ptr %stack.ptr_880)
  br i1 %3039, label %if_229, label %end_if_229
if_229:
  br label %range_query.end_189
end_if_229:
  %3040 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_879)
  %3041 = getelementptr [2 x i32], ptr %stack.ptr_881, i32 0, i32 0
  %3042 = getelementptr [4 x i32], ptr %3040, i32 0, i32 2
  %3043 = load i32, ptr %3042
  store i32 %3043, ptr %3041
  %3044 = getelementptr [2 x i32], ptr %stack.ptr_881, i32 0, i32 1
  store i32 0, ptr %3044
  %3045 = getelementptr [2 x i32], ptr %stack.ptr_882, i32 0, i32 0
  %3046 = getelementptr [4 x i32], ptr %3040, i32 0, i32 2
  %3047 = load i32, ptr %3046
  store i32 %3047, ptr %3045
  %3048 = getelementptr [2 x i32], ptr %stack.ptr_882, i32 0, i32 1
  store i32 4294967295, ptr %3048
  %3049 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_lower_bound_1(ptr %3049, ptr %stack.ptr_881, ptr %stack.ptr_883)
  %3050 = getelementptr %program, ptr %arg_0, i32 0, i32 3
  call ccc void @eclair_btree_upper_bound_1(ptr %3050, ptr %stack.ptr_882, ptr %stack.ptr_884)
  br label %loop_195
loop_195:
  %3051 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_883, ptr %stack.ptr_884)
  br i1 %3051, label %if_230, label %end_if_230
if_230:
  br label %range_query.end_190
end_if_230:
  %3052 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_883)
  %3053 = getelementptr [2 x i32], ptr %stack.ptr_885, i32 0, i32 0
  store i32 0, ptr %3053
  %3054 = getelementptr [2 x i32], ptr %stack.ptr_885, i32 0, i32 1
  %3055 = getelementptr [4 x i32], ptr %3040, i32 0, i32 2
  %3056 = load i32, ptr %3055
  store i32 %3056, ptr %3054
  %3057 = getelementptr [2 x i32], ptr %stack.ptr_886, i32 0, i32 0
  store i32 4294967295, ptr %3057
  %3058 = getelementptr [2 x i32], ptr %stack.ptr_886, i32 0, i32 1
  %3059 = getelementptr [4 x i32], ptr %3040, i32 0, i32 2
  %3060 = load i32, ptr %3059
  store i32 %3060, ptr %3058
  %3061 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_lower_bound_2(ptr %3061, ptr %stack.ptr_885, ptr %stack.ptr_887)
  %3062 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_upper_bound_2(ptr %3062, ptr %stack.ptr_886, ptr %stack.ptr_888)
  br label %loop_196
loop_196:
  %3063 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_887, ptr %stack.ptr_888)
  br i1 %3063, label %if_231, label %end_if_231
if_231:
  br label %range_query.end_191
end_if_231:
  %3064 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_887)
  %3065 = getelementptr [2 x i32], ptr %stack.ptr_889, i32 0, i32 0
  %3066 = getelementptr [2 x i32], ptr %3064, i32 0, i32 0
  %3067 = load i32, ptr %3066
  store i32 %3067, ptr %3065
  %3068 = getelementptr [2 x i32], ptr %stack.ptr_889, i32 0, i32 1
  %3069 = getelementptr [4 x i32], ptr %3040, i32 0, i32 2
  %3070 = load i32, ptr %3069
  store i32 %3070, ptr %3068
  %3071 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  %3072 = call ccc i1 @eclair_btree_contains_1(ptr %3071, ptr %stack.ptr_889)
  %3073 = select i1 %3072, i1 0, i1 1
  br i1 %3073, label %if_232, label %end_if_232
if_232:
  %3074 = getelementptr [3 x i32], ptr %stack.ptr_890, i32 0, i32 0
  %3075 = getelementptr [2 x i32], ptr %3064, i32 0, i32 0
  %3076 = load i32, ptr %3075
  store i32 %3076, ptr %3074
  %3077 = getelementptr [3 x i32], ptr %stack.ptr_890, i32 0, i32 1
  %3078 = getelementptr [4 x i32], ptr %3040, i32 0, i32 2
  %3079 = load i32, ptr %3078
  store i32 %3079, ptr %3077
  %3080 = getelementptr [3 x i32], ptr %stack.ptr_890, i32 0, i32 2
  %3081 = getelementptr [2 x i32], ptr %3052, i32 0, i32 1
  %3082 = load i32, ptr %3081
  store i32 %3082, ptr %3080
  %3083 = getelementptr %program, ptr %arg_0, i32 0, i32 64
  %3084 = call ccc i1 @eclair_btree_insert_value_0(ptr %3083, ptr %stack.ptr_890)
  br label %end_if_232
end_if_232:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_887)
  br label %loop_196
range_query.end_191:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_883)
  br label %loop_195
range_query.end_190:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_879)
  br label %loop_194
range_query.end_189:
  %3085 = getelementptr [2 x i32], ptr %stack.ptr_891, i32 0, i32 0
  store i32 0, ptr %3085
  %3086 = getelementptr [2 x i32], ptr %stack.ptr_891, i32 0, i32 1
  store i32 0, ptr %3086
  %3087 = getelementptr [2 x i32], ptr %stack.ptr_892, i32 0, i32 0
  store i32 4294967295, ptr %3087
  %3088 = getelementptr [2 x i32], ptr %stack.ptr_892, i32 0, i32 1
  store i32 4294967295, ptr %3088
  %3089 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_lower_bound_1(ptr %3089, ptr %stack.ptr_891, ptr %stack.ptr_893)
  %3090 = getelementptr %program, ptr %arg_0, i32 0, i32 27
  call ccc void @eclair_btree_upper_bound_1(ptr %3090, ptr %stack.ptr_892, ptr %stack.ptr_894)
  br label %loop_197
loop_197:
  %3091 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_893, ptr %stack.ptr_894)
  br i1 %3091, label %if_233, label %end_if_233
if_233:
  br label %range_query.end_192
end_if_233:
  %3092 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_893)
  %3093 = getelementptr [2 x i32], ptr %stack.ptr_895, i32 0, i32 0
  %3094 = getelementptr [2 x i32], ptr %3092, i32 0, i32 1
  %3095 = load i32, ptr %3094
  store i32 %3095, ptr %3093
  %3096 = getelementptr [2 x i32], ptr %stack.ptr_895, i32 0, i32 1
  store i32 0, ptr %3096
  %3097 = getelementptr [2 x i32], ptr %stack.ptr_896, i32 0, i32 0
  %3098 = getelementptr [2 x i32], ptr %3092, i32 0, i32 1
  %3099 = load i32, ptr %3098
  store i32 %3099, ptr %3097
  %3100 = getelementptr [2 x i32], ptr %stack.ptr_896, i32 0, i32 1
  store i32 4294967295, ptr %3100
  %3101 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3101, ptr %stack.ptr_895, ptr %stack.ptr_897)
  %3102 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3102, ptr %stack.ptr_896, ptr %stack.ptr_898)
  br label %loop_198
loop_198:
  %3103 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_897, ptr %stack.ptr_898)
  br i1 %3103, label %if_234, label %end_if_234
if_234:
  br label %range_query.end_193
end_if_234:
  %3104 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_897)
  %3105 = getelementptr [2 x i32], ptr %stack.ptr_899, i32 0, i32 0
  %3106 = getelementptr [2 x i32], ptr %3092, i32 0, i32 0
  %3107 = load i32, ptr %3106
  store i32 %3107, ptr %3105
  %3108 = getelementptr [2 x i32], ptr %stack.ptr_899, i32 0, i32 1
  %3109 = getelementptr [2 x i32], ptr %3104, i32 0, i32 1
  %3110 = load i32, ptr %3109
  store i32 %3110, ptr %3108
  %3111 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3112 = call ccc i1 @eclair_btree_insert_value_1(ptr %3111, ptr %stack.ptr_899)
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_897)
  br label %loop_198
range_query.end_193:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_893)
  br label %loop_197
range_query.end_192:
  %3113 = getelementptr [3 x i32], ptr %stack.ptr_900, i32 0, i32 0
  store i32 0, ptr %3113
  %3114 = getelementptr [3 x i32], ptr %stack.ptr_900, i32 0, i32 1
  store i32 0, ptr %3114
  %3115 = getelementptr [3 x i32], ptr %stack.ptr_900, i32 0, i32 2
  store i32 0, ptr %3115
  %3116 = getelementptr [3 x i32], ptr %stack.ptr_901, i32 0, i32 0
  store i32 4294967295, ptr %3116
  %3117 = getelementptr [3 x i32], ptr %stack.ptr_901, i32 0, i32 1
  store i32 4294967295, ptr %3117
  %3118 = getelementptr [3 x i32], ptr %stack.ptr_901, i32 0, i32 2
  store i32 4294967295, ptr %3118
  %3119 = getelementptr %program, ptr %arg_0, i32 0, i32 55
  call ccc void @eclair_btree_lower_bound_0(ptr %3119, ptr %stack.ptr_900, ptr %stack.ptr_902)
  %3120 = getelementptr %program, ptr %arg_0, i32 0, i32 55
  call ccc void @eclair_btree_upper_bound_0(ptr %3120, ptr %stack.ptr_901, ptr %stack.ptr_903)
  br label %loop_199
loop_199:
  %3121 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_902, ptr %stack.ptr_903)
  br i1 %3121, label %if_235, label %end_if_235
if_235:
  br label %range_query.end_194
end_if_235:
  %3122 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_902)
  %3123 = getelementptr [3 x i32], ptr %3122, i32 0, i32 2
  %3124 = load i32, ptr %3123
  %3125 = icmp ne i32 %3124, 54
  br i1 %3125, label %if_236, label %end_if_237
if_236:
  %3126 = getelementptr [2 x i32], ptr %stack.ptr_904, i32 0, i32 0
  %3127 = getelementptr [3 x i32], ptr %3122, i32 0, i32 0
  %3128 = load i32, ptr %3127
  store i32 %3128, ptr %3126
  %3129 = getelementptr [2 x i32], ptr %stack.ptr_904, i32 0, i32 1
  %3130 = getelementptr [3 x i32], ptr %3122, i32 0, i32 2
  %3131 = load i32, ptr %3130
  store i32 %3131, ptr %3129
  %3132 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3133 = call ccc i1 @eclair_btree_contains_1(ptr %3132, ptr %stack.ptr_904)
  %3134 = select i1 %3133, i1 0, i1 1
  br i1 %3134, label %if_237, label %end_if_236
if_237:
  %3135 = getelementptr [3 x i32], ptr %stack.ptr_905, i32 0, i32 0
  %3136 = getelementptr [3 x i32], ptr %3122, i32 0, i32 0
  %3137 = load i32, ptr %3136
  store i32 %3137, ptr %3135
  %3138 = getelementptr [3 x i32], ptr %stack.ptr_905, i32 0, i32 1
  %3139 = getelementptr [3 x i32], ptr %3122, i32 0, i32 1
  %3140 = load i32, ptr %3139
  store i32 %3140, ptr %3138
  %3141 = getelementptr [3 x i32], ptr %stack.ptr_905, i32 0, i32 2
  %3142 = getelementptr [3 x i32], ptr %3122, i32 0, i32 2
  %3143 = load i32, ptr %3142
  store i32 %3143, ptr %3141
  %3144 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3145 = call ccc i1 @eclair_btree_insert_value_0(ptr %3144, ptr %stack.ptr_905)
  br label %end_if_236
end_if_236:
  br label %end_if_237
end_if_237:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_902)
  br label %loop_199
range_query.end_194:
  %3146 = getelementptr [3 x i32], ptr %stack.ptr_906, i32 0, i32 0
  store i32 0, ptr %3146
  %3147 = getelementptr [3 x i32], ptr %stack.ptr_906, i32 0, i32 1
  store i32 0, ptr %3147
  %3148 = getelementptr [3 x i32], ptr %stack.ptr_906, i32 0, i32 2
  store i32 0, ptr %3148
  %3149 = getelementptr [3 x i32], ptr %stack.ptr_907, i32 0, i32 0
  store i32 4294967295, ptr %3149
  %3150 = getelementptr [3 x i32], ptr %stack.ptr_907, i32 0, i32 1
  store i32 4294967295, ptr %3150
  %3151 = getelementptr [3 x i32], ptr %stack.ptr_907, i32 0, i32 2
  store i32 4294967295, ptr %3151
  %3152 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %3152, ptr %stack.ptr_906, ptr %stack.ptr_908)
  %3153 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %3153, ptr %stack.ptr_907, ptr %stack.ptr_909)
  br label %loop_200
loop_200:
  %3154 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_908, ptr %stack.ptr_909)
  br i1 %3154, label %if_238, label %end_if_238
if_238:
  br label %range_query.end_195
end_if_238:
  %3155 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_908)
  %3156 = getelementptr [4 x i32], ptr %stack.ptr_910, i32 0, i32 0
  %3157 = getelementptr [3 x i32], ptr %3155, i32 0, i32 2
  %3158 = load i32, ptr %3157
  store i32 %3158, ptr %3156
  %3159 = getelementptr [4 x i32], ptr %stack.ptr_910, i32 0, i32 1
  store i32 0, ptr %3159
  %3160 = getelementptr [4 x i32], ptr %stack.ptr_910, i32 0, i32 2
  store i32 0, ptr %3160
  %3161 = getelementptr [4 x i32], ptr %stack.ptr_910, i32 0, i32 3
  store i32 0, ptr %3161
  %3162 = getelementptr [4 x i32], ptr %stack.ptr_911, i32 0, i32 0
  %3163 = getelementptr [3 x i32], ptr %3155, i32 0, i32 2
  %3164 = load i32, ptr %3163
  store i32 %3164, ptr %3162
  %3165 = getelementptr [4 x i32], ptr %stack.ptr_911, i32 0, i32 1
  store i32 4294967295, ptr %3165
  %3166 = getelementptr [4 x i32], ptr %stack.ptr_911, i32 0, i32 2
  store i32 4294967295, ptr %3166
  %3167 = getelementptr [4 x i32], ptr %stack.ptr_911, i32 0, i32 3
  store i32 4294967295, ptr %3167
  %3168 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %3168, ptr %stack.ptr_910, ptr %stack.ptr_912)
  %3169 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %3169, ptr %stack.ptr_911, ptr %stack.ptr_913)
  br label %loop_201
loop_201:
  %3170 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_912, ptr %stack.ptr_913)
  br i1 %3170, label %if_239, label %end_if_239
if_239:
  br label %range_query.end_196
end_if_239:
  %3171 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_912)
  %3172 = getelementptr [2 x i32], ptr %stack.ptr_914, i32 0, i32 0
  %3173 = getelementptr [4 x i32], ptr %3171, i32 0, i32 2
  %3174 = load i32, ptr %3173
  store i32 %3174, ptr %3172
  %3175 = getelementptr [2 x i32], ptr %stack.ptr_914, i32 0, i32 1
  store i32 0, ptr %3175
  %3176 = getelementptr [2 x i32], ptr %stack.ptr_915, i32 0, i32 0
  %3177 = getelementptr [4 x i32], ptr %3171, i32 0, i32 2
  %3178 = load i32, ptr %3177
  store i32 %3178, ptr %3176
  %3179 = getelementptr [2 x i32], ptr %stack.ptr_915, i32 0, i32 1
  store i32 4294967295, ptr %3179
  %3180 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3180, ptr %stack.ptr_914, ptr %stack.ptr_916)
  %3181 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3181, ptr %stack.ptr_915, ptr %stack.ptr_917)
  br label %loop_202
loop_202:
  %3182 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_916, ptr %stack.ptr_917)
  br i1 %3182, label %if_240, label %end_if_240
if_240:
  br label %range_query.end_197
end_if_240:
  %3183 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_916)
  %3184 = getelementptr [2 x i32], ptr %3183, i32 0, i32 1
  %3185 = load i32, ptr %3184
  %3186 = icmp ne i32 %3185, 54
  br i1 %3186, label %if_241, label %end_if_242
if_241:
  %3187 = getelementptr [2 x i32], ptr %stack.ptr_918, i32 0, i32 0
  %3188 = getelementptr [3 x i32], ptr %3155, i32 0, i32 0
  %3189 = load i32, ptr %3188
  store i32 %3189, ptr %3187
  %3190 = getelementptr [2 x i32], ptr %stack.ptr_918, i32 0, i32 1
  %3191 = getelementptr [2 x i32], ptr %3183, i32 0, i32 1
  %3192 = load i32, ptr %3191
  store i32 %3192, ptr %3190
  %3193 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3194 = call ccc i1 @eclair_btree_contains_1(ptr %3193, ptr %stack.ptr_918)
  %3195 = select i1 %3194, i1 0, i1 1
  br i1 %3195, label %if_242, label %end_if_241
if_242:
  %3196 = getelementptr [3 x i32], ptr %stack.ptr_919, i32 0, i32 0
  %3197 = getelementptr [3 x i32], ptr %3155, i32 0, i32 0
  %3198 = load i32, ptr %3197
  store i32 %3198, ptr %3196
  %3199 = getelementptr [3 x i32], ptr %stack.ptr_919, i32 0, i32 1
  %3200 = getelementptr [4 x i32], ptr %3171, i32 0, i32 2
  %3201 = load i32, ptr %3200
  store i32 %3201, ptr %3199
  %3202 = getelementptr [3 x i32], ptr %stack.ptr_919, i32 0, i32 2
  %3203 = getelementptr [2 x i32], ptr %3183, i32 0, i32 1
  %3204 = load i32, ptr %3203
  store i32 %3204, ptr %3202
  %3205 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3206 = call ccc i1 @eclair_btree_insert_value_0(ptr %3205, ptr %stack.ptr_919)
  br label %end_if_241
end_if_241:
  br label %end_if_242
end_if_242:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_916)
  br label %loop_202
range_query.end_197:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_912)
  br label %loop_201
range_query.end_196:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_908)
  br label %loop_200
range_query.end_195:
  %3207 = getelementptr [3 x i32], ptr %stack.ptr_920, i32 0, i32 0
  store i32 0, ptr %3207
  %3208 = getelementptr [3 x i32], ptr %stack.ptr_920, i32 0, i32 1
  store i32 0, ptr %3208
  %3209 = getelementptr [3 x i32], ptr %stack.ptr_920, i32 0, i32 2
  store i32 0, ptr %3209
  %3210 = getelementptr [3 x i32], ptr %stack.ptr_921, i32 0, i32 0
  store i32 4294967295, ptr %3210
  %3211 = getelementptr [3 x i32], ptr %stack.ptr_921, i32 0, i32 1
  store i32 4294967295, ptr %3211
  %3212 = getelementptr [3 x i32], ptr %stack.ptr_921, i32 0, i32 2
  store i32 4294967295, ptr %3212
  %3213 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_lower_bound_0(ptr %3213, ptr %stack.ptr_920, ptr %stack.ptr_922)
  %3214 = getelementptr %program, ptr %arg_0, i32 0, i32 53
  call ccc void @eclair_btree_upper_bound_0(ptr %3214, ptr %stack.ptr_921, ptr %stack.ptr_923)
  br label %loop_203
loop_203:
  %3215 = call ccc i1 @eclair_btree_iterator_is_equal_0(ptr %stack.ptr_922, ptr %stack.ptr_923)
  br i1 %3215, label %if_243, label %end_if_243
if_243:
  br label %range_query.end_198
end_if_243:
  %3216 = call ccc ptr @eclair_btree_iterator_current_0(ptr %stack.ptr_922)
  %3217 = getelementptr [4 x i32], ptr %stack.ptr_924, i32 0, i32 0
  %3218 = getelementptr [3 x i32], ptr %3216, i32 0, i32 2
  %3219 = load i32, ptr %3218
  store i32 %3219, ptr %3217
  %3220 = getelementptr [4 x i32], ptr %stack.ptr_924, i32 0, i32 1
  store i32 0, ptr %3220
  %3221 = getelementptr [4 x i32], ptr %stack.ptr_924, i32 0, i32 2
  store i32 0, ptr %3221
  %3222 = getelementptr [4 x i32], ptr %stack.ptr_924, i32 0, i32 3
  store i32 0, ptr %3222
  %3223 = getelementptr [4 x i32], ptr %stack.ptr_925, i32 0, i32 0
  %3224 = getelementptr [3 x i32], ptr %3216, i32 0, i32 2
  %3225 = load i32, ptr %3224
  store i32 %3225, ptr %3223
  %3226 = getelementptr [4 x i32], ptr %stack.ptr_925, i32 0, i32 1
  store i32 4294967295, ptr %3226
  %3227 = getelementptr [4 x i32], ptr %stack.ptr_925, i32 0, i32 2
  store i32 4294967295, ptr %3227
  %3228 = getelementptr [4 x i32], ptr %stack.ptr_925, i32 0, i32 3
  store i32 4294967295, ptr %3228
  %3229 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_lower_bound_4(ptr %3229, ptr %stack.ptr_924, ptr %stack.ptr_926)
  %3230 = getelementptr %program, ptr %arg_0, i32 0, i32 9
  call ccc void @eclair_btree_upper_bound_4(ptr %3230, ptr %stack.ptr_925, ptr %stack.ptr_927)
  br label %loop_204
loop_204:
  %3231 = call ccc i1 @eclair_btree_iterator_is_equal_4(ptr %stack.ptr_926, ptr %stack.ptr_927)
  br i1 %3231, label %if_244, label %end_if_244
if_244:
  br label %range_query.end_199
end_if_244:
  %3232 = call ccc ptr @eclair_btree_iterator_current_4(ptr %stack.ptr_926)
  %3233 = getelementptr [2 x i32], ptr %stack.ptr_928, i32 0, i32 0
  %3234 = getelementptr [4 x i32], ptr %3232, i32 0, i32 3
  %3235 = load i32, ptr %3234
  store i32 %3235, ptr %3233
  %3236 = getelementptr [2 x i32], ptr %stack.ptr_928, i32 0, i32 1
  store i32 0, ptr %3236
  %3237 = getelementptr [2 x i32], ptr %stack.ptr_929, i32 0, i32 0
  %3238 = getelementptr [4 x i32], ptr %3232, i32 0, i32 3
  %3239 = load i32, ptr %3238
  store i32 %3239, ptr %3237
  %3240 = getelementptr [2 x i32], ptr %stack.ptr_929, i32 0, i32 1
  store i32 4294967295, ptr %3240
  %3241 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3241, ptr %stack.ptr_928, ptr %stack.ptr_930)
  %3242 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3242, ptr %stack.ptr_929, ptr %stack.ptr_931)
  br label %loop_205
loop_205:
  %3243 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_930, ptr %stack.ptr_931)
  br i1 %3243, label %if_245, label %end_if_245
if_245:
  br label %range_query.end_200
end_if_245:
  %3244 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_930)
  %3245 = getelementptr [2 x i32], ptr %3244, i32 0, i32 1
  %3246 = load i32, ptr %3245
  %3247 = icmp ne i32 %3246, 54
  br i1 %3247, label %if_246, label %end_if_247
if_246:
  %3248 = getelementptr [2 x i32], ptr %stack.ptr_932, i32 0, i32 0
  %3249 = getelementptr [3 x i32], ptr %3216, i32 0, i32 0
  %3250 = load i32, ptr %3249
  store i32 %3250, ptr %3248
  %3251 = getelementptr [2 x i32], ptr %stack.ptr_932, i32 0, i32 1
  %3252 = getelementptr [2 x i32], ptr %3244, i32 0, i32 1
  %3253 = load i32, ptr %3252
  store i32 %3253, ptr %3251
  %3254 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3255 = call ccc i1 @eclair_btree_contains_1(ptr %3254, ptr %stack.ptr_932)
  %3256 = select i1 %3255, i1 0, i1 1
  br i1 %3256, label %if_247, label %end_if_246
if_247:
  %3257 = getelementptr [3 x i32], ptr %stack.ptr_933, i32 0, i32 0
  %3258 = getelementptr [3 x i32], ptr %3216, i32 0, i32 0
  %3259 = load i32, ptr %3258
  store i32 %3259, ptr %3257
  %3260 = getelementptr [3 x i32], ptr %stack.ptr_933, i32 0, i32 1
  %3261 = getelementptr [4 x i32], ptr %3232, i32 0, i32 3
  %3262 = load i32, ptr %3261
  store i32 %3262, ptr %3260
  %3263 = getelementptr [3 x i32], ptr %stack.ptr_933, i32 0, i32 2
  %3264 = getelementptr [2 x i32], ptr %3244, i32 0, i32 1
  %3265 = load i32, ptr %3264
  store i32 %3265, ptr %3263
  %3266 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3267 = call ccc i1 @eclair_btree_insert_value_0(ptr %3266, ptr %stack.ptr_933)
  br label %end_if_246
end_if_246:
  br label %end_if_247
end_if_247:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_930)
  br label %loop_205
range_query.end_200:
  call ccc void @eclair_btree_iterator_next_4(ptr %stack.ptr_926)
  br label %loop_204
range_query.end_199:
  call ccc void @eclair_btree_iterator_next_0(ptr %stack.ptr_922)
  br label %loop_203
range_query.end_198:
  %3268 = getelementptr [4 x i32], ptr %stack.ptr_934, i32 0, i32 0
  store i32 0, ptr %3268
  %3269 = getelementptr [4 x i32], ptr %stack.ptr_934, i32 0, i32 1
  store i32 0, ptr %3269
  %3270 = getelementptr [4 x i32], ptr %stack.ptr_934, i32 0, i32 2
  store i32 0, ptr %3270
  %3271 = getelementptr [4 x i32], ptr %stack.ptr_934, i32 0, i32 3
  store i32 0, ptr %3271
  %3272 = getelementptr [4 x i32], ptr %stack.ptr_935, i32 0, i32 0
  store i32 4294967295, ptr %3272
  %3273 = getelementptr [4 x i32], ptr %stack.ptr_935, i32 0, i32 1
  store i32 4294967295, ptr %3273
  %3274 = getelementptr [4 x i32], ptr %stack.ptr_935, i32 0, i32 2
  store i32 4294967295, ptr %3274
  %3275 = getelementptr [4 x i32], ptr %stack.ptr_935, i32 0, i32 3
  store i32 4294967295, ptr %3275
  %3276 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %3276, ptr %stack.ptr_934, ptr %stack.ptr_936)
  %3277 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %3277, ptr %stack.ptr_935, ptr %stack.ptr_937)
  br label %loop_206
loop_206:
  %3278 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_936, ptr %stack.ptr_937)
  br i1 %3278, label %if_248, label %end_if_248
if_248:
  br label %range_query.end_201
end_if_248:
  %3279 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_936)
  %3280 = getelementptr [2 x i32], ptr %stack.ptr_938, i32 0, i32 0
  %3281 = getelementptr [4 x i32], ptr %3279, i32 0, i32 2
  %3282 = load i32, ptr %3281
  store i32 %3282, ptr %3280
  %3283 = getelementptr [2 x i32], ptr %stack.ptr_938, i32 0, i32 1
  store i32 0, ptr %3283
  %3284 = getelementptr [2 x i32], ptr %stack.ptr_939, i32 0, i32 0
  %3285 = getelementptr [4 x i32], ptr %3279, i32 0, i32 2
  %3286 = load i32, ptr %3285
  store i32 %3286, ptr %3284
  %3287 = getelementptr [2 x i32], ptr %stack.ptr_939, i32 0, i32 1
  store i32 4294967295, ptr %3287
  %3288 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3288, ptr %stack.ptr_938, ptr %stack.ptr_940)
  %3289 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3289, ptr %stack.ptr_939, ptr %stack.ptr_941)
  br label %loop_207
loop_207:
  %3290 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_940, ptr %stack.ptr_941)
  br i1 %3290, label %if_249, label %end_if_249
if_249:
  br label %range_query.end_202
end_if_249:
  %3291 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_940)
  %3292 = getelementptr [2 x i32], ptr %stack.ptr_942, i32 0, i32 0
  store i32 0, ptr %3292
  %3293 = getelementptr [2 x i32], ptr %stack.ptr_942, i32 0, i32 1
  %3294 = getelementptr [4 x i32], ptr %3279, i32 0, i32 2
  %3295 = load i32, ptr %3294
  store i32 %3295, ptr %3293
  %3296 = getelementptr [2 x i32], ptr %stack.ptr_943, i32 0, i32 0
  store i32 4294967295, ptr %3296
  %3297 = getelementptr [2 x i32], ptr %stack.ptr_943, i32 0, i32 1
  %3298 = getelementptr [4 x i32], ptr %3279, i32 0, i32 2
  %3299 = load i32, ptr %3298
  store i32 %3299, ptr %3297
  %3300 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_lower_bound_2(ptr %3300, ptr %stack.ptr_942, ptr %stack.ptr_944)
  %3301 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_upper_bound_2(ptr %3301, ptr %stack.ptr_943, ptr %stack.ptr_945)
  br label %loop_208
loop_208:
  %3302 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_944, ptr %stack.ptr_945)
  br i1 %3302, label %if_250, label %end_if_250
if_250:
  br label %range_query.end_203
end_if_250:
  %3303 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_944)
  %3304 = getelementptr [2 x i32], ptr %stack.ptr_946, i32 0, i32 0
  store i32 0, ptr %3304
  %3305 = getelementptr [2 x i32], ptr %stack.ptr_946, i32 0, i32 1
  %3306 = getelementptr [2 x i32], ptr %3303, i32 0, i32 0
  %3307 = load i32, ptr %3306
  store i32 %3307, ptr %3305
  %3308 = getelementptr [2 x i32], ptr %stack.ptr_947, i32 0, i32 0
  store i32 4294967295, ptr %3308
  %3309 = getelementptr [2 x i32], ptr %stack.ptr_947, i32 0, i32 1
  %3310 = getelementptr [2 x i32], ptr %3303, i32 0, i32 0
  %3311 = load i32, ptr %3310
  store i32 %3311, ptr %3309
  %3312 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_lower_bound_2(ptr %3312, ptr %stack.ptr_946, ptr %stack.ptr_948)
  %3313 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_upper_bound_2(ptr %3313, ptr %stack.ptr_947, ptr %stack.ptr_949)
  br label %loop_209
loop_209:
  %3314 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_948, ptr %stack.ptr_949)
  br i1 %3314, label %if_251, label %end_if_251
if_251:
  br label %range_query.end_204
end_if_251:
  %3315 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_948)
  %3316 = getelementptr [2 x i32], ptr %stack.ptr_950, i32 0, i32 0
  %3317 = getelementptr [2 x i32], ptr %3315, i32 0, i32 0
  %3318 = load i32, ptr %3317
  store i32 %3318, ptr %3316
  %3319 = getelementptr [2 x i32], ptr %stack.ptr_950, i32 0, i32 1
  %3320 = getelementptr [2 x i32], ptr %3291, i32 0, i32 1
  %3321 = load i32, ptr %3320
  store i32 %3321, ptr %3319
  %3322 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3323 = call ccc i1 @eclair_btree_contains_1(ptr %3322, ptr %stack.ptr_950)
  %3324 = select i1 %3323, i1 0, i1 1
  br i1 %3324, label %if_252, label %end_if_252
if_252:
  %3325 = getelementptr [3 x i32], ptr %stack.ptr_951, i32 0, i32 0
  %3326 = getelementptr [2 x i32], ptr %3315, i32 0, i32 0
  %3327 = load i32, ptr %3326
  store i32 %3327, ptr %3325
  %3328 = getelementptr [3 x i32], ptr %stack.ptr_951, i32 0, i32 1
  %3329 = getelementptr [4 x i32], ptr %3279, i32 0, i32 2
  %3330 = load i32, ptr %3329
  store i32 %3330, ptr %3328
  %3331 = getelementptr [3 x i32], ptr %stack.ptr_951, i32 0, i32 2
  %3332 = getelementptr [2 x i32], ptr %3291, i32 0, i32 1
  %3333 = load i32, ptr %3332
  store i32 %3333, ptr %3331
  %3334 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3335 = call ccc i1 @eclair_btree_insert_value_0(ptr %3334, ptr %stack.ptr_951)
  br label %end_if_252
end_if_252:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_948)
  br label %loop_209
range_query.end_204:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_944)
  br label %loop_208
range_query.end_203:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_940)
  br label %loop_207
range_query.end_202:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_936)
  br label %loop_206
range_query.end_201:
  %3336 = getelementptr [4 x i32], ptr %stack.ptr_952, i32 0, i32 0
  store i32 0, ptr %3336
  %3337 = getelementptr [4 x i32], ptr %stack.ptr_952, i32 0, i32 1
  store i32 0, ptr %3337
  %3338 = getelementptr [4 x i32], ptr %stack.ptr_952, i32 0, i32 2
  store i32 0, ptr %3338
  %3339 = getelementptr [4 x i32], ptr %stack.ptr_952, i32 0, i32 3
  store i32 0, ptr %3339
  %3340 = getelementptr [4 x i32], ptr %stack.ptr_953, i32 0, i32 0
  store i32 4294967295, ptr %3340
  %3341 = getelementptr [4 x i32], ptr %stack.ptr_953, i32 0, i32 1
  store i32 4294967295, ptr %3341
  %3342 = getelementptr [4 x i32], ptr %stack.ptr_953, i32 0, i32 2
  store i32 4294967295, ptr %3342
  %3343 = getelementptr [4 x i32], ptr %stack.ptr_953, i32 0, i32 3
  store i32 4294967295, ptr %3343
  %3344 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_lower_bound_3(ptr %3344, ptr %stack.ptr_952, ptr %stack.ptr_954)
  %3345 = getelementptr %program, ptr %arg_0, i32 0, i32 6
  call ccc void @eclair_btree_upper_bound_3(ptr %3345, ptr %stack.ptr_953, ptr %stack.ptr_955)
  br label %loop_210
loop_210:
  %3346 = call ccc i1 @eclair_btree_iterator_is_equal_3(ptr %stack.ptr_954, ptr %stack.ptr_955)
  br i1 %3346, label %if_253, label %end_if_253
if_253:
  br label %range_query.end_205
end_if_253:
  %3347 = call ccc ptr @eclair_btree_iterator_current_3(ptr %stack.ptr_954)
  %3348 = getelementptr [2 x i32], ptr %stack.ptr_956, i32 0, i32 0
  %3349 = getelementptr [4 x i32], ptr %3347, i32 0, i32 3
  %3350 = load i32, ptr %3349
  store i32 %3350, ptr %3348
  %3351 = getelementptr [2 x i32], ptr %stack.ptr_956, i32 0, i32 1
  store i32 0, ptr %3351
  %3352 = getelementptr [2 x i32], ptr %stack.ptr_957, i32 0, i32 0
  %3353 = getelementptr [4 x i32], ptr %3347, i32 0, i32 3
  %3354 = load i32, ptr %3353
  store i32 %3354, ptr %3352
  %3355 = getelementptr [2 x i32], ptr %stack.ptr_957, i32 0, i32 1
  store i32 4294967295, ptr %3355
  %3356 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_lower_bound_1(ptr %3356, ptr %stack.ptr_956, ptr %stack.ptr_958)
  %3357 = getelementptr %program, ptr %arg_0, i32 0, i32 66
  call ccc void @eclair_btree_upper_bound_1(ptr %3357, ptr %stack.ptr_957, ptr %stack.ptr_959)
  br label %loop_211
loop_211:
  %3358 = call ccc i1 @eclair_btree_iterator_is_equal_1(ptr %stack.ptr_958, ptr %stack.ptr_959)
  br i1 %3358, label %if_254, label %end_if_254
if_254:
  br label %range_query.end_206
end_if_254:
  %3359 = call ccc ptr @eclair_btree_iterator_current_1(ptr %stack.ptr_958)
  %3360 = getelementptr [2 x i32], ptr %stack.ptr_960, i32 0, i32 0
  store i32 0, ptr %3360
  %3361 = getelementptr [2 x i32], ptr %stack.ptr_960, i32 0, i32 1
  %3362 = getelementptr [4 x i32], ptr %3347, i32 0, i32 3
  %3363 = load i32, ptr %3362
  store i32 %3363, ptr %3361
  %3364 = getelementptr [2 x i32], ptr %stack.ptr_961, i32 0, i32 0
  store i32 4294967295, ptr %3364
  %3365 = getelementptr [2 x i32], ptr %stack.ptr_961, i32 0, i32 1
  %3366 = getelementptr [4 x i32], ptr %3347, i32 0, i32 3
  %3367 = load i32, ptr %3366
  store i32 %3367, ptr %3365
  %3368 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_lower_bound_2(ptr %3368, ptr %stack.ptr_960, ptr %stack.ptr_962)
  %3369 = getelementptr %program, ptr %arg_0, i32 0, i32 60
  call ccc void @eclair_btree_upper_bound_2(ptr %3369, ptr %stack.ptr_961, ptr %stack.ptr_963)
  br label %loop_212
loop_212:
  %3370 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_962, ptr %stack.ptr_963)
  br i1 %3370, label %if_255, label %end_if_255
if_255:
  br label %range_query.end_207
end_if_255:
  %3371 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_962)
  %3372 = getelementptr [2 x i32], ptr %stack.ptr_964, i32 0, i32 0
  store i32 0, ptr %3372
  %3373 = getelementptr [2 x i32], ptr %stack.ptr_964, i32 0, i32 1
  %3374 = getelementptr [2 x i32], ptr %3371, i32 0, i32 0
  %3375 = load i32, ptr %3374
  store i32 %3375, ptr %3373
  %3376 = getelementptr [2 x i32], ptr %stack.ptr_965, i32 0, i32 0
  store i32 4294967295, ptr %3376
  %3377 = getelementptr [2 x i32], ptr %stack.ptr_965, i32 0, i32 1
  %3378 = getelementptr [2 x i32], ptr %3371, i32 0, i32 0
  %3379 = load i32, ptr %3378
  store i32 %3379, ptr %3377
  %3380 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_lower_bound_2(ptr %3380, ptr %stack.ptr_964, ptr %stack.ptr_966)
  %3381 = getelementptr %program, ptr %arg_0, i32 0, i32 57
  call ccc void @eclair_btree_upper_bound_2(ptr %3381, ptr %stack.ptr_965, ptr %stack.ptr_967)
  br label %loop_213
loop_213:
  %3382 = call ccc i1 @eclair_btree_iterator_is_equal_2(ptr %stack.ptr_966, ptr %stack.ptr_967)
  br i1 %3382, label %if_256, label %end_if_256
if_256:
  br label %range_query.end_208
end_if_256:
  %3383 = call ccc ptr @eclair_btree_iterator_current_2(ptr %stack.ptr_966)
  %3384 = getelementptr [2 x i32], ptr %stack.ptr_968, i32 0, i32 0
  %3385 = getelementptr [2 x i32], ptr %3383, i32 0, i32 0
  %3386 = load i32, ptr %3385
  store i32 %3386, ptr %3384
  %3387 = getelementptr [2 x i32], ptr %stack.ptr_968, i32 0, i32 1
  %3388 = getelementptr [2 x i32], ptr %3359, i32 0, i32 1
  %3389 = load i32, ptr %3388
  store i32 %3389, ptr %3387
  %3390 = getelementptr %program, ptr %arg_0, i32 0, i32 28
  %3391 = call ccc i1 @eclair_btree_contains_1(ptr %3390, ptr %stack.ptr_968)
  %3392 = select i1 %3391, i1 0, i1 1
  br i1 %3392, label %if_257, label %end_if_257
if_257:
  %3393 = getelementptr [3 x i32], ptr %stack.ptr_969, i32 0, i32 0
  %3394 = getelementptr [2 x i32], ptr %3383, i32 0, i32 0
  %3395 = load i32, ptr %3394
  store i32 %3395, ptr %3393
  %3396 = getelementptr [3 x i32], ptr %stack.ptr_969, i32 0, i32 1
  %3397 = getelementptr [4 x i32], ptr %3347, i32 0, i32 3
  %3398 = load i32, ptr %3397
  store i32 %3398, ptr %3396
  %3399 = getelementptr [3 x i32], ptr %stack.ptr_969, i32 0, i32 2
  %3400 = getelementptr [2 x i32], ptr %3359, i32 0, i32 1
  %3401 = load i32, ptr %3400
  store i32 %3401, ptr %3399
  %3402 = getelementptr %program, ptr %arg_0, i32 0, i32 65
  %3403 = call ccc i1 @eclair_btree_insert_value_0(ptr %3402, ptr %stack.ptr_969)
  br label %end_if_257
end_if_257:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_966)
  br label %loop_213
range_query.end_208:
  call ccc void @eclair_btree_iterator_next_2(ptr %stack.ptr_962)
  br label %loop_212
range_query.end_207:
  call ccc void @eclair_btree_iterator_next_1(ptr %stack.ptr_958)
  br label %loop_211
range_query.end_206:
  call ccc void @eclair_btree_iterator_next_3(ptr %stack.ptr_954)
  br label %loop_210
range_query.end_205:
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
