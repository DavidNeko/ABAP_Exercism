# ABAP的基本语法。
---
## 1. 语句结束符
在ABAP中，每一行代码通常以句点（.）结束。这与Java中的分号（;）类似。
```abap
WRITE 'Hello, World!'.
``` 

## 2.注释
如前所述，ABAP支持单行注释，可以使用星号（*）或双引号（"）。
```abap
* 这是一个单行注释
WRITE 'Hello, World!'. " 这是另一个单行注释
```

## 3.数据声明
在ABAP中，使用 DATA 关键字来声明变量。变量类型可以是内置类型（如 STRING、INTEGER 等）或自定义类型。
```abap
DATA: lv_text TYPE string,
      lv_number TYPE i.
```

## 4.字符串操作
字符串可以用单引号（'）括起来。字符串连接使用 & 运算符。
```abap
DATA: lv_text1 TYPE string,
      lv_text2 TYPE string,
      lv_result TYPE string.

lv_text1 = 'Hello'.
lv_text2 = 'World'.
lv_result = lv_text1 & ', ' & lv_text2 & '!'.
WRITE lv_result. " 输出: Hello, World!
```

## 5.条件语句
ABAP中的条件语句使用 IF、ELSEIF 和 ELSE。
```abap
DATA: lv_number TYPE i.
lv_number = 10.

IF lv_number > 0.
  WRITE 'Number is positive.'.
ELSEIF lv_number < 0.
  WRITE 'Number is negative.'.
ELSE.
  WRITE 'Number is zero.'.
ENDIF.
```

## 6.循环语句
ABAP支持多种循环语句，如 DO、WHILE 和 LOOP。
```abap
DATA: lv_counter TYPE i.
lv_counter = 1.

DO 5 TIMES.
  WRITE: / 'Counter:', lv_counter.
  lv_counter = lv_counter + 1.
ENDDO.
```

## 7.内表（Internal Tables）
内表在ABAP中非常常用，用于存储多行数据。可以使用 APPEND、LOOP AT 等操作。
```abap
DATA: lt_table TYPE TABLE OF string,
      lv_line  TYPE string.

APPEND 'Line 1' TO lt_table.
APPEND 'Line 2' TO lt_table.

LOOP AT lt_table INTO lv_line.
  WRITE: / lv_line.
ENDLOOP.
```

## 8.模块化编程
ABAP支持模块化编程，可以使用 FORM 和 PERFORM 来定义和调用子程序。
```abap
FORM display_text.
  WRITE 'Hello from subroutine!'.
ENDFORM.

START-OF-SELECTION.
  PERFORM display_text.
```

## 9.类和对象
ABAP也支持面向对象编程（OOP），可以定义类和对象。
```abap
CLASS lcl_example DEFINITION.
  PUBLIC SECTION.
    METHODS: display_message.
ENDCLASS.

CLASS lcl_example IMPLEMENTATION.
  METHOD display_message.
    WRITE 'Hello from class method!'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_example TYPE REF TO lcl_example.
  CREATE OBJECT lo_example.
  lo_example->display_message( ).
```
