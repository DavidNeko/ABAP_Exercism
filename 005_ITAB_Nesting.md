# 005 ITAB Nesting 代码详解
---
完整代码
```abap:005_ITAB_Nesting.abap
CLASS zcl_itab_nesting DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF artists_type,
             artist_id   TYPE string,
             artist_name TYPE string,
           END OF artists_type.
    TYPES artists TYPE STANDARD TABLE OF artists_type WITH KEY artist_id.
    TYPES: BEGIN OF albums_type,
             artist_id  TYPE string,
             album_id   TYPE string,
             album_name TYPE string,
           END OF albums_type.
    TYPES albums TYPE STANDARD TABLE OF albums_type WITH KEY artist_id album_id.
    TYPES: BEGIN OF songs_type,
             artist_id TYPE string,
             album_id  TYPE string,
             song_id   TYPE string,
             song_name TYPE string,
           END OF songs_type.
    TYPES songs TYPE STANDARD TABLE OF songs_type WITH KEY artist_id album_id song_id.


    TYPES: BEGIN OF song_nested_type,
             song_id   TYPE string,
             song_name TYPE string,
           END OF song_nested_type.
    TYPES: BEGIN OF album_song_nested_type,
             album_id   TYPE string,
             album_name TYPE string,
             songs      TYPE STANDARD TABLE OF song_nested_type WITH KEY song_id,
           END OF album_song_nested_type.
    TYPES: BEGIN OF artist_album_nested_type,
             artist_id   TYPE string,
             artist_name TYPE string,
             albums      TYPE STANDARD TABLE OF album_song_nested_type WITH KEY album_id,
           END OF artist_album_nested_type.
    TYPES nested_data TYPE STANDARD TABLE OF artist_album_nested_type WITH KEY artist_id.

    METHODS perform_nesting
      IMPORTING
        artists            TYPE artists
        albums             TYPE albums
        songs              TYPE songs
      RETURNING
        VALUE(nested_data) TYPE nested_data.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_itab_nesting IMPLEMENTATION.

  METHOD perform_nesting.
    Data: lt_nested_data TYPE nested_data,
          ls_artist TYPE artist_album_nested_type,
          ls_album TYPE album_song_nested_type,
          ls_song TYPE song_nested_type.

    " Step 1: Initialize the result table
    CLEAR lt_nested_data.

    " Step 2: Loop through Artists table
    LOOP AT artists INTO DATA(ls_artist_row).
        CLEAR ls_artist.
        ls_artist-artist_id = ls_artist_row-artist_id.
        ls_artist-artist_name = ls_artist_row-artist_name.

        " Step 3: Loop through ALBUMS table for the current artist
        LOOP AT albums INTO DATA(ls_album_row) WHERE artist_id = ls_artist_row-artist_id.
            CLEAR ls_album.
            ls_album-album_id = ls_album_row-album_id.
            ls_album-album_name = ls_album_row-album_name.

            "Step 4: Loop through SONGS table for the current album
            LOOP AT songs INTO DATA(ls_song_row) WHERE artist_id = ls_album_row-artist_id AND album_id = ls_album_row-album_id.
                CLEAR ls_song.
                ls_song-song_id = ls_song_row-song_id.
                ls_song-song_name = ls_song_row-song_name.

                " Insert song into album's songs table.
                INSERT ls_song INTO TABLE ls_album-songs.
            ENDLOOP.

            " Insert album into artist's albums table.
            INSERT ls_album INTO TABLE ls_artist-albums.
        ENDLOOP.

        " Insert artist into result table.
        INSERT ls_artist INTO TABLE lt_nested_data.
    ENDLOOP.

    " Return the nested data.
    nested_data = lt_nested_data.

  ENDMETHOD.

ENDCLASS.

```

---

```abap
CLASS zcl_itab_nesting DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
```
- 定义一个名为 `zcl_itab_nesting` 的类。
- `PUBLIC` 表示这个类是公共的，可以被其他程序访问。
- `FINAL` 表示这个类不能被继承。
- `CREATE PUBLIC` 表示这个类的实例可以被公共创建。

```abap
  PUBLIC SECTION.
```
- 公共部分，定义在这里的属性和方法可以被类外部访问。

```abap
    TYPES: BEGIN OF artists_type,
             artist_id   TYPE string,
             artist_name TYPE string,
           END OF artists_type.
    TYPES artists TYPE STANDARD TABLE OF artists_type WITH KEY artist_id.
```
- 定义一个结构 `artists_type`，包含 `artist_id` 和 `artist_name` 两个字段。
- 定义一个标准表类型 `artists`，表的行类型为 `artists_type`，并以 `artist_id` 为键。

```abap
    TYPES: BEGIN OF albums_type,
             artist_id  TYPE string,
             album_id   TYPE string,
             album_name TYPE string,
           END OF albums_type.
    TYPES albums TYPE STANDARD TABLE OF albums_type WITH KEY artist_id album_id.
```
- 定义一个结构 `albums_type`，包含 `artist_id`、`album_id` 和 `album_name` 三个字段。
- 定义一个标准表类型 `albums`，表的行类型为 `albums_type`，并以 `artist_id` 和 `album_id` 为键。

```abap
    TYPES: BEGIN OF songs_type,
             artist_id TYPE string,
             album_id  TYPE string,
             song_id   TYPE string,
             song_name TYPE string,
           END OF songs_type.
    TYPES songs TYPE STANDARD TABLE OF songs_type WITH KEY artist_id album_id song_id.
```
- 定义一个结构 `songs_type`，包含 `artist_id`、`album_id`、`song_id` 和 `song_name` 四个字段。
- 定义一个标准表类型 `songs`，表的行类型为 `songs_type`，并以 `artist_id`、`album_id` 和 `song_id` 为键。

```abap
    TYPES: BEGIN OF song_nested_type,
             song_id   TYPE string,
             song_name TYPE string,
           END OF song_nested_type.
    TYPES: BEGIN OF album_song_nested_type,
             album_id   TYPE string,
             album_name TYPE string,
             songs      TYPE STANDARD TABLE OF song_nested_type WITH KEY song_id,
           END OF album_song_nested_type.
    TYPES: BEGIN OF artist_album_nested_type,
             artist_id   TYPE string,
             artist_name TYPE string,
             albums      TYPE STANDARD TABLE OF album_song_nested_type WITH KEY album_id,
           END OF artist_album_nested_type.
    TYPES nested_data TYPE STANDARD TABLE OF artist_album_nested_type WITH KEY artist_id.
```
- 定义嵌套结构：
  - `song_nested_type` 包含 `song_id` 和 `song_name`。
  - `album_song_nested_type` 包含 `album_id`、`album_name` 和一个 `songs` 表。
  - `artist_album_nested_type` 包含 `artist_id`、`artist_name` 和一个 `albums` 表。
- 定义一个标准表类型 `nested_data`，表的行类型为 `artist_album_nested_type`，并以 `artist_id` 为键。

```abap
    METHODS perform_nesting
      IMPORTING
        artists            TYPE artists
        albums             TYPE albums
        songs              TYPE songs
      RETURNING
        VALUE(nested_data) TYPE nested_data.
```
- 定义一个方法 `perform_nesting`，用于将三个内部表嵌套组合成一个结果表。
- `IMPORTING` 参数：`artists`、`albums` 和 `songs`。
- `RETURNING` 参数：`nested_data`。

```abap
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
```
- `PROTECTED SECTION` 和 `PRIVATE SECTION` 是类的保护和私有部分，当前没有定义任何内容。

```abap
CLASS zcl_itab_nesting IMPLEMENTATION.
```
- 开始类 `zcl_itab_nesting` 的实现部分。

```abap
  METHOD perform_nesting.
    DATA: lt_nested_data TYPE nested_data,
          ls_artist TYPE artist_album_nested_type,
          ls_album TYPE album_song_nested_type,
          ls_song TYPE song_nested_type.
```
- 定义方法 `perform_nesting` 的实现。
- 定义局部变量：
  - `lt_nested_data` 类型为 `nested_data`，用于存储最终的嵌套结果。
  - `ls_artist` 类型为 `artist_album_nested_type`，用于存储当前处理的艺术家信息。
  - `ls_album` 类型为 `album_song_nested_type`，用于存储当前处理的专辑信息。
  - `ls_song` 类型为 `song_nested_type`，用于存储当前处理的歌曲信息。

```abap
    " Step 1: Initialize the result table
    CLEAR lt_nested_data.
```
- 第一步：初始化结果表 `lt_nested_data`。

```abap
    " Step 2: Loop through Artists table
    LOOP AT artists INTO DATA(ls_artist_row).
        CLEAR ls_artist.
        ls_artist-artist_id = ls_artist_row-artist_id.
        ls_artist-artist_name = ls_artist_row-artist_name.
```
- 第二步：遍历 `artists` 表。
- 对于每个艺术家，清空 `ls_artist` 并赋值 `artist_id` 和 `artist_name`。

```abap
        " Step 3: Loop through ALBUMS table for the current artist
        LOOP AT albums INTO DATA(ls_album_row) WHERE artist_id = ls_artist_row-artist_id.
            CLEAR ls_album.
            ls_album-album_id = ls_album_row-album_id.
            ls_album-album_name = ls_album_row-album_name.
```
- 第三步：遍历当前艺术家的 `albums` 表。
- 对于每个专辑，清空 `ls_album` 并赋值 `album_id` 和 `album_name`。

```abap
            "Step 4: Loop through SONGS table for the current album
            LOOP AT songs INTO DATA(ls_song_row) WHERE artist_id = ls_album_row-artist_id AND album_id = ls_album_row-album_id.
                CLEAR ls_song.
                ls_song-song_id = ls_song_row-song_id.
                ls_song-song_name = ls_song_row-song_name.
```
- 第四步：遍历当前专辑的 `songs` 表。
- 对于每首歌，清空 `ls_song` 并赋值 `song_id` 和 `song_name`。

```abap
                " Insert song into album's songs table.
                INSERT ls_song INTO TABLE ls_album-songs.
            ENDLOOP.
```
- 将当前歌曲插入到专辑的 `songs` 表中。

```abap
            " Insert album into artist's albums table.
            INSERT ls_album INTO TABLE ls_artist-albums.
        ENDLOOP.
```
- 将当前专辑插入到艺术家的 `albums` 表中。

```abap
        " Insert artist into result table.
        INSERT ls_artist INTO TABLE lt_nested_data.
    ENDLOOP.
```
- 将当前艺术家插入到结果表 `lt_nested_data` 中。

```abap
    " Return the nested data.
    nested_data = lt_nested_data.
```
- 返回嵌套的数据 `nested_data`。

```abap
  ENDMETHOD.
ENDCLASS.
```
- 结束方法 `perform_nesting` 的实现。
- 结束类 `zcl_itab_nesting` 的实现。

### 总结
这段代码的主要功能是将三个内部表（`artists`、`albums` 和 `songs`）的数据嵌套组合成一个结果表 `nested_data`。通过遍历每个表并将数据插入到相应的嵌套结构中，最终生成一个包含艺术家、专辑和歌曲信息的嵌套表。
