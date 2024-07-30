" クラス zcl_itab_basics の定義を開始します。
CLASS zcl_itab_basics DEFINITION
  PUBLIC " クラスはパブリックであり、他のプログラムからアクセス可能です。
  FINAL " クラスは最終クラスであり、継承できません。
  CREATE PUBLIC . " クラスのインスタンスはパブリックに作成できます。

  PUBLIC SECTION. " パブリックセクションの開始
    " group という名前の1文字のキャラクタ型を定義します。
    TYPES group TYPE c LENGTH 1.
    
    " initial_type という名前の構造体を定義します。
    TYPES: BEGIN OF initial_type,
             group       TYPE group, " group フィールド
             number      TYPE i, " number フィールド
             description TYPE string, " description フィールド
           END OF initial_type,
           " initial_type の標準テーブル型 itab_data_type を定義します。
           itab_data_type TYPE STANDARD TABLE OF initial_type WITH EMPTY KEY.

    " fill_itab メソッドを定義します。戻り値は itab_data_type 型です。
    METHODS fill_itab
           RETURNING
             VALUE(initial_data) TYPE itab_data_type.

    " add_to_itab メソッドを定義します。引数として initial_data を受け取り、戻り値は itab_data_type 型です。
    METHODS add_to_itab
           IMPORTING initial_data TYPE itab_data_type
           RETURNING
            VALUE(updated_data) TYPE itab_data_type.

    " sort_itab メソッドを定義します。引数として initial_data を受け取り、戻り値は itab_data_type 型です。
    METHODS sort_itab
           IMPORTING initial_data TYPE itab_data_type
           RETURNING
            VALUE(updated_data) TYPE itab_data_type.

    " search_itab メソッドを定義します。引数として initial_data を受け取り、戻り値は整数型の result_index です。
    METHODS search_itab
           IMPORTING initial_data TYPE itab_data_type
           RETURNING
             VALUE(result_index) TYPE i.

  PROTECTED SECTION. " プロテクテッドセクションの開始
  PRIVATE SECTION. " プライベートセクションの開始
ENDCLASS. " クラス定義の終了

" クラス zcl_itab_basics の実装を開始します。
CLASS zcl_itab_basics IMPLEMENTATION.
  " fill_itab メソッドの実装を開始します。
  " fill_itab は internal tableを初期化するメソッドです。
  METHOD fill_itab.
    " fill_itab メソッド内で、itab_data_type 型の内部テーブルを宣言します。
    DATA lt_initial_data TYPE itab_data_type.
    "指定された6つのレコードを内部テーブルに追加します。各レコードは initial_type 型の構造体として定義されます。 
    DATA ls_initial_data TYPE initial_type.

    "レコード1
    ls_initial_data-group = 'A'.
    ls_initial_data-number = 10.
    ls_initial_data-description = 'Group A-2'.
    APPEND ls_initial_data TO lt_initial_data.

    "レコード2
    ls_initial_data-group = 'B'.
    ls_initial_data-number = 5.
    ls_initial_data-description = 'Group B'.
    APPEND ls_initial_data TO lt_initial_data.
    
    "レコード3
    ls_initial_data-group = 'A'.
    ls_initial_data-number = 6.
    ls_initial_data-description = 'Group A-1'.
    APPEND ls_initial_data TO lt_initial_data.

    "レコード4
    ls_initial_data-group = 'C'.
    ls_initial_data-number = 22.
    ls_initial_data-description = 'Group C-1'.
    APPEND ls_initial_data TO lt_initial_data.

    "レコード5
    ls_initial_data-group = 'A'.
    ls_initial_data-number = 13.
    ls_initial_data-description = 'Group A-3'.
    APPEND ls_initial_data TO lt_initial_data.

    "レコード6
    ls_initial_data-group = 'C'.
    ls_initial_data-number = 500.
    ls_initial_data-description = 'Group C-2'.
    APPEND ls_initial_data TO lt_initial_data.
    
    "内部テーブル lt_initial_data をメソッドの戻り値 initial_data に設定します。
    initial_data = lt_initial_data.
  ENDMETHOD. " fill_itab メソッドの終了

  " add_to_itab メソッドの実装を開始します。
  " 指定されたレコードが内部テーブルの末尾に追加されます。
  METHOD add_to_itab.
    " initial_data を updated_data に代入します。
    updated_data = initial_data.
    
    DATA: ls_new_record TYPE initial_type

    "新しいレコードの値を設定します
    ls_new_record-group = 'A'.
    ls_new_record-number = 19.
    ls_new_record-description = 'Group A-4'.

    "新しいレコードを内部テーブルの末尾に追加します
    APPEND ls_new_record TO updated_data.
  ENDMETHOD. " add_to_itab メソッドの終了

  " sort_itab メソッドの実装を開始します。
  METHOD sort_itab.
    " initial_data を updated_data に代入します。
    updated_data = initial_data.

    "SORT ステートメントを使用して、GROUP 列でアルファベット順に、NUMBER 列で降順にソートします。
    SORT updated_data BY group ASCENDING number DESCENDING.
  ENDMETHOD. " sort_itab メソッドの終了

  " search_itab メソッドの実装を開始します。
  METHOD search_itab.
    " initial_data を temp_data に代入します。
    DATA(temp_data) = initial_data.

    "インデックスを格納する変数を初期化します。
    DATA: lv_index TYPE i VALUE 0.

    "LOOP AT ステートメントを使用して、内部テーブルをループし、NUMBER 列の値が6であるレコードを検索します。
    LOOP AT temp_data INTO DATA(ls_record).
        lv_index = lv_index + 1.
        IF ls_record-number = 6.
            result_index = lv_index.
            EXIT.
        ENDIF.
    ENDLOOP.

    IF result_index = 0.
        result_index = 0. "見つからなかった場合のインデックス
    ENDIF.

  ENDMETHOD. " search_itab メソッドの終了

ENDCLASS. " クラス実装の終了
