# ABAPのSyntax（文法）
---
## 1. 句の終了記号
ABAPでは、各行のコードは通常ピリオド（.）で終了します。これはJavaのセミコロン（;）に似ています。
```abap
WRITE 'Hello, World!'.
``` 

## 2.コメント
前述のように、ABAPは単一行コメントをサポートしており、アスタリスク（*）またはダブルクォート（"）を使用できます。
```abap
* これは単一行コメントです
WRITE 'Hello, World!'. " これは別の単一行コメントです
```

## 3.データ宣言
ABAPでは、DATAキーワードを使用して変数を宣言します。変数の型は組み込み型（例えばSTRING、INTEGERなど）またはカスタム型にすることができます。
```abap
DATA: lv_text TYPE string,
      lv_number TYPE i.
```

## 4.文字列操作
文字列はシングルクォート（'）で囲むことができます。文字列の連結には`&`演算子を使用します。
```abap
DATA: lv_text1 TYPE string,
      lv_text2 TYPE string,
      lv_result TYPE string.

lv_text1 = 'Hello'.
lv_text2 = 'World'.
lv_result = lv_text1 & ', ' & lv_text2 & '!'.
WRITE lv_result. " 出力: Hello, World!
```

## 5.条件文
ABAPの条件文はIF、ELSEIF、およびELSEを使用します。
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

## 6.ループ文
ABAPはDO、WHILE、およびLOOPなどのさまざまなループ文をサポートしています。
```abap
DATA: lv_counter TYPE i.
lv_counter = 1.

DO 5 TIMES.
  WRITE: / 'Counter:', lv_counter.
  lv_counter = lv_counter + 1.
ENDDO.
```

## 7.内部テーブル（Internal Tables）
内部テーブルはABAPで非常に一般的に使用され、多くの行のデータを格納するために使用されます。APPEND、LOOP ATなどの操作を使用できます。
```abap
DATA: lt_table TYPE TABLE OF string,
      lv_line  TYPE string.

APPEND 'Line 1' TO lt_table.
APPEND 'Line 2' TO lt_table.

LOOP AT lt_table INTO lv_line.
  WRITE: / lv_line.
ENDLOOP.
```

## 8.モジュール化プログラミング
ABAPはモジュール化プログラミングをサポートしており、FORMおよびPERFORMを使用してサブルーチンを定義および呼び出すことができます。
```abap
FORM display_text.
  WRITE 'Hello from subroutine!'.
ENDFORM.

START-OF-SELECTION.
  PERFORM display_text.
```

## 9.クラスとオブジェクト
ABAPはオブジェクト指向プログラミング（OOP）もサポートしており、クラスとオブジェクトを定義できます。
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
