*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zaqo_load_2_smw0.

PARAMETERS:
 p_path TYPE string OBLIGATORY.

PERFORM start.

FORM start.
  PERFORM load USING  'Z_AQO_INDEX_HTML'           'index.html'.
  PERFORM load USING  'Z_AQO_MULTI_UI'             'z_aqo_multi_ui.w3mi.data.js'.
  PERFORM load USING  'Z_AQO_UTIL_JS'              'z_aqo_util_js.w3mi.data.js'.
  PERFORM load USING  'Z_AQO_MAIN_CONTROLLER'      'z_aqo_main_controller.w3mi.data.js'.

  PERFORM load USING  'Z_AQO_I18N_EN_JSON'         'z_aqo_i18n_en_json.w3mi.data.json'.
  PERFORM load USING  'Z_AQO_I18N_RU_JSON'         'z_aqo_i18n_ru_json.w3mi.data.json'.

  PERFORM load USING  'Z_AQO_SPLIT_APP'            'view/splitApp.xml'.
  PERFORM load USING  'Z_AQO_FIELD_CATALOG_DIALOG' 'view/fieldCatalogDialog.xml'.

  MESSAGE 'Done' TYPE 'S'.
ENDFORM.

FORM load USING iv_objid    TYPE wwwdata-objid
                iv_rel_path TYPE csequence.
  DATA:
    lv_path TYPE text255,
    ls_key  TYPE wwwdatatab.

  CONCATENATE p_path iv_rel_path INTO lv_path.
  ls_key-relid = 'MI'.
  ls_key-objid = iv_objid.

  CALL FUNCTION 'UPLOAD_WEB_OBJECT'
    EXPORTING
      key  = ls_key
      temp = lv_path.
ENDFORM.
