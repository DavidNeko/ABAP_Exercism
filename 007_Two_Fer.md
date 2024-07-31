# 007_Two_Fer.abap代码详解
---

完整代码
```abap
CLASS zcl_two_fer DEFINITION PUBLIC.
    PUBLIC SECTION.
      METHODS two_fer
        IMPORTING
          input         TYPE string OPTIONAL
        RETURNING
          VALUE(result) TYPE string.
  ENDCLASS.
  
  CLASS zcl_two_fer IMPLEMENTATION.
  
  METHOD two_fer.
      IF input IS INITIAL.
        result = 'One for you, one for me.'.
      ELSE.
        result = |One for { input }, one for me.|.
      ENDIF.
    ENDMETHOD.
  
  ENDCLASS.
  
```

---
代码详解部分

```abap
CLASS zcl_two_fer DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS two_fer
      IMPORTING
        input         TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.
```

- `CLASS zcl_two_fer DEFINITION PUBLIC.`: 定义一个名为 `zcl_two_fer` 的公共类。
- `PUBLIC SECTION.`: 公共部分，定义类的公共成员和方法。
- `METHODS two_fer`: 定义一个名为 `two_fer` 的方法。
  - `IMPORTING input TYPE string OPTIONAL`: 该方法接受一个可选的字符串参数 `input`。
  - `RETURNING VALUE(result) TYPE string.`: 该方法返回一个字符串类型的值 `result`。

```abap
CLASS zcl_two_fer IMPLEMENTATION.

  METHOD two_fer.
    IF input IS INITIAL.
      result = 'One for you, one for me.'.
    ELSE.
      result = |One for { input }, one for me.|. " 使用字符串模板
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```

- `CLASS zcl_two_fer IMPLEMENTATION.`: 开始类 `zcl_two_fer` 的实现部分。
- `METHOD two_fer.`: 实现 `two_fer` 方法。
  - `IF input IS INITIAL.`: 检查 `input` 是否为空。
    - `result = 'One for you, one for me.'.`: 如果 `input` 为空，返回字符串 "One for you, one for me."。
  - `ELSE.`: 否则（即 `input` 不为空）。
    - `result = |One for { input }, one for me.|.`: 使用字符串模板生成字符串 "One for {input}, one for me."，其中 `{input}` 是传入的名字。
  - `ENDIF.`: 结束条件判断。
- `ENDMETHOD.`: 结束方法 `two_fer` 的实现。
- `ENDCLASS.`: 结束类 `zcl_two_fer` 的实现部分。

### 其他相关知识

- **字符串模板**: 在ABAP中，字符串模板使用 `|...|` 语法，可以在模板中嵌入变量，类似于其他编程语言中的模板字符串。
- **可选参数**: 在方法定义中，使用 `OPTIONAL` 关键字可以使参数变为可选。如果调用方法时未提供该参数，则其值为初始值（对于字符串类型，初始值为空字符串）。
- **条件判断**: `IF ... ELSE ... ENDIF` 结构用于条件判断，根据条件的真假执行不同的代码块。

### 测试代码

可以通过以下方式测试这个类：

```abap
DATA: lo_two_fer TYPE REF TO zcl_two_fer,
      lv_result  TYPE string.

CREATE OBJECT lo_two_fer.

lv_result = lo_two_fer->two_fer( 'Alice' ).
WRITE: / lv_result. " 输出: One for Alice, one for me.

lv_result = lo_two_fer->two_fer( ).
WRITE: / lv_result. " 输出: One for you, one for me.
```

- `DATA: lo_two_fer TYPE REF TO zcl_two_fer, lv_result TYPE string.`: 定义类的引用变量 `lo_two_fer` 和字符串变量 `lv_result`。
- `CREATE OBJECT lo_two_fer.`: 创建 `zcl_two_fer` 类的实例。
- `lv_result = lo_two_fer->two_fer( 'Alice' ).`: 调用 `two_fer` 方法，传入参数 'Alice'，并将结果赋值给 `lv_result`。
- `WRITE: / lv_result.`: 输出 `lv_result` 的值。
- `lv_result = lo_two_fer->two_fer( ).`: 调用 `two_fer` 方法，不传入参数，结果赋值给 `lv_result`。
- `WRITE: / lv_result.`: 输出 `lv_result` 的值。
