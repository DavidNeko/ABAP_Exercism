"这行代码定义了一个名为 zcl_hello_world 的类，并且这个类是公共的（PUBLIC），
"意味着它可以被其他程序或类访问。
CLASS zcl_hello_world DEFINITION PUBLIC.
  "这是类的公共部分。
  "在这个部分定义的属性和方法可以被类外部的代码访问。
  PUBLIC SECTION.
    "这行代码定义了一个名为 hello 的方法。
    "这个方法没有输入参数，但有一个返回值 result，其类型是 string。
    METHODS hello RETURNING VALUE(result) TYPE string.
"这行代码标志着类定义的结束。      
ENDCLASS.

"类实现部分
"这行代码开始了类 zcl_hello_world 的实现部分。
CLASS zcl_hello_world IMPLEMENTATION.
 "这行代码开始了 hello 方法的实现。
  METHOD hello.
    "这行代码将字符串 'Goodbye, Mars!' 赋值给 result 变量。
    "这个 result 变量是 hello 方法的返回值。
    result = 'Goodbye, Mars!'.
  "这行代码标志着 hello 方法实现的结束。
  ENDMETHOD.
"这行代码标志着类实现的结束。
ENDCLASS.
