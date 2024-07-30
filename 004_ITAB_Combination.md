# 004 ITAB combination 代码详解
---
```abap
CLASS zcl_itab_combination DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF alphatab_type,
             cola TYPE string,
             colb TYPE string,
             colc TYPE string,
           END OF alphatab_type.
    TYPES alphas TYPE STANDARD TABLE OF alphatab_type.

    TYPES: BEGIN OF numtab_type,
             col1 TYPE string,
             col2 TYPE string,
             col3 TYPE string,
           END OF numtab_type.
    TYPES nums TYPE STANDARD TABLE OF numtab_type.

    TYPES: BEGIN OF combined_data_type,
             colx TYPE string,
             coly TYPE string,
             colz TYPE string,
           END OF combined_data_type.
    TYPES combined_data TYPE STANDARD TABLE OF combined_data_type WITH EMPTY KEY.

    METHODS perform_combination
      IMPORTING
        alphas             TYPE alphas
        nums               TYPE nums
      RETURNING
        VALUE(combined_data) TYPE combined_data.

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.

CLASS zcl_itab_combination IMPLEMENTATION.

  METHOD perform_combination.
    DATA: lt_combined_data TYPE combined_data,
          ls_combined_data TYPE combined_data_type,
          ls_alpha TYPE alphatab_type,
          ls_num TYPE numtab_type.

    LOOP AT alphas INTO ls_alpha.
        READ TABLE nums INTO ls_num INDEX sy-tabix.
        IF sy-subrc = 0.
            ls_combined_data-colx = ls_alpha-cola & ls_num-col1.
            ls_combined_data-coly = ls_alpha-colb & ls_num-col2.
            ls_combined_data-colz = ls_alpha-colc & ls_num-col3.
            APPEND ls_combined_data TO lt_combined_data.
        ENDIF.
    ENDLOOP.

    combined_data = lt_combined_data.
  ENDMETHOD.

ENDCLASS.
```

## 代码解释
### 类定义部分：

- 定义了三个类型 `alphatab_type`、`numtab_type` 和 `combined_data_type`，分别表示 `alphas 表`、`nums 表`和组合后的 `combined_data 表`的行结构。
- 定义了三个表类型 `alphas`、`nums` 和 `combined_data`，分别表示 `alphas 表`、`nums` 表和组合后的 `combined_data 表`。

### 方法 `perform_combination` 的实现：

- 定义了局部变量 `lt_combined_data`、`ls_combined_data`、`ls_alpha` 和 `ls_num`，分别表示组合后的表、组合后的行、`alphas` 表的行和 `nums` 表的行。
- 使用 `LOOP AT alphas INTO ls_alpha` 遍历 `alphas` 表的每一行。
- 使用 `READ TABLE nums INTO ls_num INDEX sy-tabix` 读取 `nums` 表中与当前 `alphas` 表行对应的行。
- 如果读取成功（`sy-subrc = 0`），则将 `alphas` 表和 `nums` 表对应单元格的值组合起来，并赋值给 `ls_combined_data` 的相应字段。
- 使用 `APPEND ls_combined_data TO lt_combined_data` 将组合后的行追加到 `lt_combined_data` 表中。
- 最后将 `lt_combined_data` 表赋值给返回值 `combined_data`。

这个实现确保了 alphas 表和 nums 表的每一行对应的单元格值被正确组合，并返回一个新的内部表 combined_data。

---
# 参考： SAP ABAP的系统字段

`sy-tabix`
- 定义：`sy-tabix` 是一个系统字段，用于存储当前内部表行的索引。
- 使用场景：
    - 在 `LOOP AT` 语句中，`sy-tabix` 表示当前循环迭代的行索引。
    - 在 `READ TABLE` 语句中，`sy-tabix` 可以用来指定要读取的行的索引。
- 示例：
```abap
LOOP AT itab INTO wa.
  WRITE: / 'Current index:', sy-tabix.
ENDLOOP.
```

`sy-subrc`
- 定义：`sy-subrc` 是一个系统字段，用于存储上一个操作的返回代码。通常用于检查操作是否成功。
- 使用场景：
    - 在 `READ TABLE`、`SELECT`、`CALL FUNCTION` 等语句后，`sy-subrc` 用于检查操作是否成功。
    - `sy-subrc = 0` 表示操作成功，其他值表示操作失败或部分成功。
- 示例：
```abap
READ TABLE itab INTO wa WITH KEY key = 'value'.
IF sy-subrc = 0.
  WRITE: / 'Read successful'.
ELSE.
  WRITE: / 'Read failed'.
ENDIF.
```
## 其他常用的系统字段

- `sy-datum`：当前日期。
```abap
WRITE: / 'Current date:', sy-datum.
```
- `sy-uzeit`：当前时间。
```abap
WRITE: / 'Current time:', sy-uzeit.
```
- `sy-index`：在 `DO` 或 `WHILE` 循环中，表示当前迭代的索引。
```abap
DO 10 TIMES.
  WRITE: / 'Current index:', sy-index.
ENDDO.
```
- `sy-fdpos`：在字符串操作中，表示找到的子字符串的位置。
```abap
FIND 'substring' IN 'string'.
IF sy-subrc = 0.
  WRITE: / 'Found at position:', sy-fdpos.
ELSE.
  WRITE: / 'Not found'.
ENDIF.
```

- `sy-lsind`：在 `LOOP AT SCREEN` 中，表示当前屏幕字段的索引。
```abap
LOOP AT SCREEN.
  WRITE: / 'Current screen field index:', sy-lsind.
ENDLOOP.
```
