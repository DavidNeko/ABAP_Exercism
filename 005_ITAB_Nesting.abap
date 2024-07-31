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
