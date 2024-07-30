"この行のコードは、zcl_hello_worldという名前のクラスを定義し、
"このクラスがパブリック（PUBLIC）であることを示しています。
"つまり、他のプログラムやクラスからアクセス可能です。
CLASS zcl_hello_world DEFINITION PUBLIC.
  "これはクラスのパブリックセクションです。
  "このセクションで定義された属性とメソッドはクラス外部のコードからアクセス可能です。
  PUBLIC SECTION.
    "この行のコードは、helloという名前のメソッドを定義しています。
    "このメソッドには入力パラメータがありませんが、resultという名前の戻り値があり、その型はstringです。
    METHODS hello RETURNING VALUE(result) TYPE string.
"この行のコードはクラス定義の終了を示しています。      
ENDCLASS.

"クラス実装部分
"この行のコードは、zcl_hello_worldクラスの実装部分の開始を示しています。
CLASS zcl_hello_world IMPLEMENTATION.
 "この行のコードは、helloメソッドの実装の開始を示しています。
  METHOD hello.
    "この行のコードは、文字列 'Goodbye, Mars!' をresult変数に代入しています。
    "このresult変数はhelloメソッドの戻り値です。
    result = 'Goodbye, Mars!'.
  "この行のコードは、helloメソッドの実装の終了を示しています。
  ENDMETHOD.
"この行のコードはクラス実装の終了を示しています。
ENDCLASS.
