# Scrabble Score 代码详解
---

完整代码
```abap
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
```


## 类定义

```abap
CLASS zcl_scrabble_score DEFINITION PUBLIC .
```
定义一个公共类 `zcl_scrabble_score`。

```abap
    PUBLIC SECTION.
      METHODS score
        IMPORTING
          input         TYPE string OPTIONAL
        RETURNING
          VALUE(result) TYPE i.
```
在公共部分定义一个方法 `score`，该方法接受一个可选的字符串参数 `input`，并返回一个整数 `result`。

```abap
    PROTECTED SECTION.
    PRIVATE SECTION.
  
ENDCLASS.
```
保护部分和私有部分为空。

## 类实现

```abap
CLASS zcl_scrabble_score IMPLEMENTATION.
```
开始类的实现部分。

```abap
    METHOD score.
```
实现 `score` 方法。

```abap
    result = count( val = input regex = '[aeioulnrst]' case = abap_false ) * 1 +
             count( val = input regex = '[dg]' case = abap_false ) * 2 +
             count( val = input regex = '[bcmp]' case = abap_false ) * 3 +
             count( val = input regex = '[fhvwy]' case = abap_false ) * 4 +
             count( val = input regex = '[k]' case = abap_false ) * 5 +
             count( val = input regex = '[jx]' case = abap_false ) * 8 +
             count( val = input regex = '[qz]' case = abap_false ) * 10.
```
使用 `count()` 函数计算不同字母组的出现次数，并乘以相应的Scrabble分数。具体解释如下：

- `count( val = input regex = '[aeioulnrst]' case = abap_false ) * 1`：计算字母 `a, e, i, o, u, l, n, r, s, t` 的出现次数，并乘以1。
- `count( val = input regex = '[dg]' case = abap_false ) * 2`：计算字母 `d, g` 的出现次数，并乘以2。
- `count( val = input regex = '[bcmp]' case = abap_false ) * 3`：计算字母 `b, c, m, p` 的出现次数，并乘以3。
- `count( val = input regex = '[fhvwy]' case = abap_false ) * 4`：计算字母 `f, h, v, w, y` 的出现次数，并乘以4。
- `count( val = input regex = '[k]' case = abap_false ) * 5`：计算字母 `k` 的出现次数，并乘以5。
- `count( val = input regex = '[jx]' case = abap_false ) * 8`：计算字母 `j, x` 的出现次数，并乘以8。
- `count( val = input regex = '[qz]' case = abap_false ) * 10`：计算字母 `q, z` 的出现次数，并乘以10。

```abap
    ENDMETHOD.
```
结束 `score` 方法的实现。

```abap
ENDCLASS.
```
结束类的实现部分。

## `count()` 函数说明

`count()` 函数是一个内置的ABAP函数，用于使用正则表达式在字符串中计算模式的出现次数。其语法如下：

```abap
DATA(result) = count( val = <string> regex = <pattern> [case = <boolean>] ).
```

- `val`：要在其中计算出现次数的输入字符串。
- `regex`：要匹配的正则表达式模式。
- `case`：一个可选参数，指定匹配是否区分大小写。`abap_false` 表示不区分大小写，`abap_true` 表示区分大小写。

### 示例

```abap
DATA(input) = 'Hello World'.
DATA(count_a) = count( val = input regex = 'l' case = abap_false ). " 计算 'l' 和 'L' 的出现次数
WRITE: / 'Number of l:', count_a.
```

这将输出：

```
Number of l: 3
```

## 如何学习和使用内置函数

1. **文档**：始终参考官方的SAP ABAP文档。该文档提供了关于内置函数及其用法的详细信息。
2. **ABAP编辑器**：使用ABAP编辑器的代码补全和帮助功能。输入 `count(` 并按 `Ctrl+Space` 将显示函数的签名和参数。
3. **社区和论坛**：通过SAP社区网络（SCN）和Stack Overflow等论坛与ABAP社区互动,可以从其他开发者发布的问题和答案中学到很多东西。
4. **实践**：在沙箱环境中尝试不同的函数。尝试小段代码以了解它们的工作原理。
