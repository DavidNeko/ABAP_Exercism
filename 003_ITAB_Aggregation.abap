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
