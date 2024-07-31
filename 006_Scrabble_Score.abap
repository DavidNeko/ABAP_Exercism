CLASS zcl_scrabble_score DEFINITION PUBLIC .

    PUBLIC SECTION.
      METHODS score
        IMPORTING
          input         TYPE string OPTIONAL
        RETURNING
          VALUE(result) TYPE i.
    PROTECTED SECTION.
    PRIVATE SECTION.
  
ENDCLASS.
  
  
CLASS zcl_scrabble_score IMPLEMENTATION.

    METHOD score.

    result = count( val = input regex = '[aeioulnrst]' case = abap_false ) * 1 +
             count( val = input regex = '[dg]' case = abap_false ) * 2 +
             count( val = input regex = '[bcmp]' case = abap_false ) * 3 +
             count( val = input regex = '[fhvwy]' case = abap_false ) * 4 +
             count( val = input regex = '[k]' case = abap_false ) * 5 +
             count( val = input regex = '[jx]' case = abap_false ) * 8 +
             count( val = input regex = '[qz]' case = abap_false ) * 10.

    ENDMETHOD.

ENDCLASS.
