# 003 ITAB Aggregation 代码详解
---

## 代码部分
```abap
"Note: lt means local table. ls means local structure.

CLASS zcl_itab_aggregation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES group TYPE c LENGTH 1.
    TYPES: BEGIN OF initial_numbers_type,
             group  TYPE group,
             number TYPE i,
           END OF initial_numbers_type,
           initial_numbers TYPE STANDARD TABLE OF initial_numbers_type WITH EMPTY KEY.

    TYPES: BEGIN OF aggregated_data_type,
             group   TYPE group,
             count   TYPE i,
             sum     TYPE i,
             min     TYPE i,
             max     TYPE i,
             average TYPE f,
           END OF aggregated_data_type,
           aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY.

    METHODS perform_aggregation
      IMPORTING
        initial_numbers        TYPE initial_numbers
      RETURNING
        VALUE(aggregated_data) TYPE aggregated_data.

    METHODS min
      IMPORTING
        val1 TYPE i
        val2 TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS max
      IMPORTING
        val1 TYPE i
        val2 TYPE i
      RETURNING
        VALUE(result) TYPE i.
        
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_itab_aggregation IMPLEMENTATION.
  METHOD perform_aggregation.
    DATA: lt_aggregated_data TYPE aggregated_data,
          ls_aggregated_data TYPE aggregated_data_type,
          lt_grouped_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY,
          ls_grouped_data TYPE aggregated_data_type.
    
    LOOP AT initial_numbers INTO DATA(ls_initial).
      READ TABLE lt_grouped_data WITH KEY group = ls_initial-group INTO ls_grouped_data.
      IF sy-subrc <> 0.
        CLEAR ls_grouped_data.
        ls_grouped_data-group = ls_initial-group.
        ls_grouped_data-count = 1.
        ls_grouped_data-sum = ls_initial-number.
        ls_grouped_data-min = ls_initial-number.
        ls_grouped_data-max = ls_initial-number.
        APPEND ls_grouped_data TO lt_grouped_data.
      ELSE.
        ls_grouped_data-count = ls_grouped_data-count + 1.
        ls_grouped_data-sum = ls_grouped_data-sum + ls_initial-number.
        ls_grouped_data-min = min( val1 = ls_grouped_data-min val2 = ls_initial-number ).
        ls_grouped_data-max = max( val1 = ls_grouped_data-max val2 = ls_initial-number ).
        DELETE lt_grouped_data WHERE group = ls_grouped_data-group.
        APPEND ls_grouped_data TO lt_grouped_data.
      ENDIF.
    ENDLOOP.
    
    SORT lt_grouped_data BY group.
    
    LOOP AT lt_grouped_data INTO ls_aggregated_data.
      ls_aggregated_data-average = CONV f( ls_aggregated_data-sum ) / ls_aggregated_data-count.
      APPEND ls_aggregated_data TO lt_aggregated_data.
    ENDLOOP.

    aggregated_data = lt_aggregated_data.
  ENDMETHOD.

  METHOD min.
    IF val1 < val2.
      result = val1.
    ELSE.
      result = val2.
    ENDIF.
  ENDMETHOD.

  METHOD max.
    IF val1 > val2.
      result = val1.
    ELSE.
      result = val2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

## 类定义部分
```abap
CLASS zcl_itab_aggregation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
```
- `CLASS zcl_itab_aggregation DEFINITION`：定义一个名为 zcl_itab_aggregation 的类。
- `PUBLIC`：类的可见性为公共，意味着该类的所有方法和属性都可以被外部访问。
- `FINAL`：类是最终的，不能被继承。
- `CREATE PUBLIC`：类的实例可以在外部创建。

```abap
PUBLIC SECTION.
```
- `PUBLIC SECTION`：定义类的公共部分，所有在此部分定义的方法和属性都可以被外部访问。

```abap
    TYPES group TYPE c LENGTH 1.
```
- `TYPES group TYPE c LENGTH 1`：定义一个名为 group 的类型，它是一个长度为1的字符类型。

```abap
    TYPES: BEGIN OF initial_numbers_type,
             group  TYPE group,
             number TYPE i,
           END OF initial_numbers_type,
           initial_numbers TYPE STANDARD TABLE OF initial_numbers_type WITH EMPTY KEY.
```
- `TYPES`: BEGIN OF initial_numbers_type：开始定义一个结构类型 initial_numbers_type。
- `group TYPE group`：结构中的 group 字段，类型为之前定义的 group。
- `number TYPE i`：结构中的 number 字段，类型为整数。
- `END OF initial_numbers_type`：结束结构类型定义。
- `initial_numbers TYPE STANDARD TABLE OF initial_numbers_type WITH EMPTY KEY`：定义一个标准表类型 `initial_numbers`，表的行类型为 `initial_numbers_type`，且没有键。

```abap
 TYPES: BEGIN OF aggregated_data_type,
             group   TYPE group,
             count   TYPE i,
             sum     TYPE i,
             min     TYPE i,
             max     TYPE i,
             average TYPE f,
           END OF aggregated_data_type,
           aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY.
```
- `TYPES: BEGIN OF aggregated_data_type`：开始定义一个结构类型 aggregated_data_type。
- `group TYPE group`：结构中的 group 字段，类型为之前定义的 group。
- `count TYPE i`：结构中的 count 字段，类型为整数。
- `sum TYPE i`：结构中的 sum 字段，类型为整数。
- `min TYPE i`：结构中的 min 字段，类型为整数。
- `max TYPE i`：结构中的 max 字段，类型为整数。
- `average TYPE f`：结构中的 average 字段，类型为浮点数。
- `END OF aggregated_data_type`：结束结构类型定义。
- `aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY`：定义一个标准表类型 `aggregated_data`，表的行类型为 `aggregated_data_type`，且没有键。

```abap
    METHODS perform_aggregation
      IMPORTING
        initial_numbers        TYPE initial_numbers
      RETURNING
        VALUE(aggregated_data) TYPE aggregated_data.
```        
- `METHODS perform_aggregation`：定义一个名为 `perform_aggregation` 的方法。
- `IMPORTING initial_numbers TYPE initial_numbers`：方法的输入参数 `initial_numbers`，类型为之前定义的 `initial_numbers`。
- `RETURNING VALUE(aggregated_data) TYPE aggregated_data`：方法的返回值 `aggregated_data`，类型为之前定义的 `aggregated_data`。

```abap
    METHODS min
      IMPORTING
        val1 TYPE i
        val2 TYPE i
      RETURNING
        VALUE(result) TYPE i.
```
- `METHODS min`：定义一个名为 `min` 的方法。
- `IMPORTING val1 TYPE i val2 TYPE i`：方法的输入参数 `val1` 和 `val2`，类型为整数。
- `RETURNING VALUE(result) TYPE i`：方法的返回值 `result`，类型为整数。

```abap
    METHODS max
      IMPORTING
        val1 TYPE i
        val2 TYPE i
      RETURNING
        VALUE(result) TYPE i.
```
- `METHODS max`：定义一个名为 `max` 的方法。
- `IMPORTING val1 TYPE i val2 TYPE i`：方法的输入参数 `val1` 和 `val2`，类型为整数。
- `RETURNING VALUE(result) TYPE i`：方法的返回值 `result`，类型为整数。

```abap
  PROTECTED SECTION.
  PRIVATE SECTION.
```
- `PROTECTED SECTION` 和 `PRIVATE SECTION`：定义类的保护部分和私有部分，但在这个类中没有具体内容。

```abap
ENDCLASS.
```
- `ENDCLASS`：结束类定义。

## 类实现部分
```abap
CLASS zcl_itab_aggregation IMPLEMENTATION.
```
- `CLASS zcl_itab_aggregation IMPLEMENTATION`：开始类 `zcl_itab_aggregation` 的实现部分。

```abap
METHOD perform_aggregation.
```
- `METHOD perform_aggregation`：开始实现方法 `perform_aggregation`。

```abap
    DATA: lt_aggregated_data TYPE aggregated_data,
          ls_aggregated_data TYPE aggregated_data_type,
          lt_grouped_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY,
          ls_grouped_data TYPE aggregated_data_type.
```
- `DATA`：定义方法内部使用的局部变量。
- `lt_aggregated_data TYPE aggregated_data`：定义一个名为 `lt_aggregated_data` 的局部表，类型为 `aggregated_data`。
- `ls_aggregated_data TYPE aggregated_data_type`：定义一个名为 `ls_aggregated_data` 的局部结构，类型为 `aggregated_data_type`。
- `lt_grouped_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY`：定义一个名为 `lt_grouped_data` 的局部表，类型为 `aggregated_data_type`，且没有键。
- `ls_grouped_data TYPE aggregated_data_type`：定义一个名为 `ls_grouped_data` 的局部结构，类型为 `aggregated_data_type`。

```abap
LOOP AT initial_numbers INTO DATA(ls_initial).
```
- `LOOP AT initial_numbers INTO DATA(ls_initial)`：遍历输入表 `initial_numbers`，每行数据存入局部结构 `ls_initial`。

```abap
READ TABLE lt_grouped_data WITH KEY group = ls_initial-group INTO ls_grouped_data.
```
- `READ TABLE lt_grouped_data WITH KEY group = ls_initial-group INTO ls_grouped_data`：在 `lt_grouped_data` 表中查找 `group` 字段等于 `ls_initial-group` 的行，并将结果存入 `ls_grouped_data`。

```abap
IF sy-subrc <> 0.
```
- `IF sy-subrc <> 0`：如果查找失败（即没有找到匹配的行），则执行以下代码块。

```abap
    CLEAR ls_grouped_data.
    ls_grouped_data-group = ls_initial-group.
    ls_grouped_data-count = 1.
    ls_grouped_data-sum = ls_initial-number.
    ls_grouped_data-min = ls_initial-number.
    ls_grouped_data-max = ls_initial-number.
    APPEND ls_grouped_data TO lt_grouped_data.
```
- `CLEAR ls_grouped_data`：清空 `ls_grouped_data` 结构。
- `ls_grouped_data-group = ls_initial-group`：将 `ls_initial` 的 `group` 字段值赋给 `ls_grouped_data` 的 `group` 字段。
- `ls_grouped_data-count = 1`：将 `ls_grouped_data` 的 `count` 字段设为`1`。
- `ls_grouped_data-sum = ls_initial-number`：将 `ls_initial` 的 `number` 字段值赋给 `ls_grouped_data` 的 `sum`字段。
- `ls_grouped_data-min = ls_initial-number`：将 `ls_initial` 的 `number` 字段值赋给 `ls_grouped_data` 的 `min` 字段。
- `ls_grouped_data-max = ls_initial-number`：将 `ls_initial` 的 `number` 字段值赋给 `ls_grouped_data` 的 `max` 字段。
- `APPEND ls_grouped_data TO lt_grouped_data`：将 `ls_grouped_data` 追加到 `lt_grouped_data` 表中。

```abap
    ELSE.
    ls_grouped_data-count = ls_grouped_data-count + 1.
    ls_grouped_data-sum = ls_grouped_data-sum + ls_initial-number.
    ls_grouped_data-min = min( val1 = ls_grouped_data-min val2 = ls_initial-number ).
    ls_grouped_data-max = max( val1 = ls_grouped_data-max val2 = ls_initial-number ).
    DELETE lt_grouped_data WHERE group = ls_grouped_data-group.
    APPEND ls_grouped_data TO lt_grouped_data.
    ENDIF.
ENDLOOP.
```
- `ELSE`：如果查找成功（即找到了匹配的行），则执行以下代码块。
- `ls_grouped_data-count = ls_grouped_data-count + 1`：将 `ls_grouped_data` 的 `count` 字段值加`1`。
- `ls_grouped_data-sum = ls_grouped_data-sum + ls_initial-number`：将 `ls_initial` 的 `number` 字段值加到 `ls_grouped_data` 的 `sum` 字段。
- `ls_grouped_data-min = min( val1 = ls_grouped_data-min val2 = ls_initial-number )`：调用 `min` 方法，计算 `ls_grouped_data`的 `min` 字段和 `ls_initial` 的 `number` 字段的最小值，并赋给 `ls_grouped_data` 的 `min` 字段。
- `ls_grouped_data-max = max( val1 = ls_grouped_data-max val2 = ls_initial-number )`：调用 `max` 方法，计算 `ls_grouped_data` 的 `max` 字段和 `ls_initial` 的 `number` 字段的最大值，并赋给 `ls_grouped_data` 的 `max` 字段。
- `DELETE lt_grouped_data WHERE group = ls_grouped_data-group`：删除 `lt_grouped_data` 表中 `group` 字段等于 `ls_grouped_data-group` 的行。
- `APPEND ls_grouped_data TO lt_grouped_data`：将更新后的 `ls_grouped_data` 追加到 `lt_grouped_data` 表中。
- `ENDLOOP`：结束循环。

```abap
 SORT lt_grouped_data BY group.
```
- `SORT lt_grouped_data BY group`：按 `group` 字段对 `lt_grouped_data` 表进行排序。

```abap
    LOOP AT lt_grouped_data INTO ls_aggregated_data.
      ls_aggregated_data-average = CONV f( ls_aggregated_data-sum ) / ls_aggregated_data-count.
      APPEND ls_aggregated_data TO lt_aggregated_data.
    ENDLOOP.
```
- `LOOP AT lt_grouped_data INTO ls_aggregated_data`：遍历 `lt_grouped_data` 表，逐行将数据存入 `ls_aggregated_data` 结构。
- `ls_aggregated_data-average = CONV f( ls_aggregated_data-sum ) / ls_aggregated_data-count`：计算 `ls_aggregated_data` 的 `average` 字段，使用浮点数除法。
- `APPEND ls_aggregated_data TO lt_aggregated_data`：将 `ls_aggregated_data` 追加到 `lt_aggregated_data` 表中。
- `ENDLOOP`：结束循环。

```abap
aggregated_data = lt_aggregated_data.
```
- `aggregated_data = lt_aggregated_data`：将 `lt_aggregated_data` 表赋值给返回值 `aggregated_data`。

```abap
  ENDMETHOD.
```
- `ENDMETHOD`：结束方法 `perform_aggregation` 的实现。

```abap
  METHOD min.
    IF val1 < val2.
      result = val1.
    ELSE.
      result = val2.
    ENDIF.
  ENDMETHOD.
```
- `METHOD min`：开始实现方法 `min`。
- `IF val1 < val2`：如果 `val1` 小于 `val2`，则执行以下代码块。
- `result = val1`：将 `val1` 赋值给返回值 `result`。
- `ELSE`：否则，执行以下代码块。
- `result = val2`：将 `val2` 赋值给返回值 `result`。
- `ENDIF`：结束条件判断。
- `ENDMETHOD`：结束方法 `min` 的实现。

```abap
  METHOD max.
    IF val1 > val2.
      result = val1.
    ELSE.
      result = val2.
    ENDIF.
  ENDMETHOD.
```
- `METHOD max`：开始实现方法 `max`。
- `IF val1 > val2`：如果 `val1` 大于 `val2`，则执行以下代码块。
- `result = val1`：将 `val1` 赋值给返回值 `result`。
- `ELSE`：否则，执行以下代码块。
- `result = val2`：将 `val2` 赋值给返回值 `result`。
- `ENDIF`：结束条件判断。
- `ENDMETHOD`：结束方法 `max` 的实现。

```abap
ENDCLASS.
```
- `ENDCLASS`：结束类 `zcl_itab_aggregation` 的实现。
