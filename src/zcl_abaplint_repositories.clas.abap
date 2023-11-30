CLASS zcl_abaplint_repositories DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      ty_repos TYPE STANDARD TABLE OF zabaplint_repos WITH KEY url.

    METHODS read_repo
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_data) TYPE zabaplint_repos.

    METHODS list_repos
      RETURNING
        VALUE(rt_data) TYPE ty_repos.

    METHODS add_repo
      IMPORTING
        !iv_url  TYPE string
        !iv_text TYPE string.

    METHODS remove_repo
      IMPORTING
        !iv_url TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abaplint_repositories IMPLEMENTATION.


  METHOD add_repo.

    DATA ls_data TYPE zabaplint_repos.

    ls_data-url  = iv_url.
    ls_data-text = iv_text.

    INSERT zabaplint_repos FROM ls_data.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD list_repos.

    SELECT * FROM zabaplint_repos INTO TABLE rt_data ORDER BY PRIMARY KEY. "#EC CI_NOWHERE

  ENDMETHOD.


  METHOD read_repo.

    SELECT SINGLE url FROM zabaplint_repos INTO rv_data WHERE url = iv_url.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD remove_repo.

    DELETE FROM zabaplint_repos WHERE url = iv_url.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
