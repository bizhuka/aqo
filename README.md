### ABAP quick options
User and programmer friendly options\
Create flexible application parameters with nested tables without any effort\
No more headache with Z* table maintenance SM30, view clusters SE54, SM34 or range options STVARV

---

#### Launch:
#### tr. SE38-> ZAQO_TESTER, tr. SE24 -> ZCL_AQO_TESTER
and then
#### tr. ZAQO_EDITOR_OLD

---
Separation by *Package* and *Option ID*

![image](https://user-images.githubusercontent.com/36256417/80679757-f0742780-8ad6-11ea-9e86-b4b84151f13b.png)

View change history\
Integration with transport system\
Editing nested catalogs (Labels & search helps)

![image](https://user-images.githubusercontent.com/36256417/80679960-58c30900-8ad7-11ea-8484-59db16b563a6.png)

Tables can contain checkboxes, lists, ranges, memo fields and even another tables
![image](https://user-images.githubusercontent.com/36256417/80680457-3f6e8c80-8ad8-11ea-95cf-8be964484559.png)

---

All you need is to describe structure(or class) in program **ms_opt** and press F8
```abap
    TRY.
        zcl_aqo_option=>create(
          iv_package_id = '$TMP'               " Package  "#EC NOTEXT
          iv_option_id  = 'Main options'(op1)  " Any text < 30 symbols
          ir_data       = REF #( ms_opt )
          " iv_repair     = abap_true
        ).
      CATCH zcx_aqo_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
```

---
The main purpose of the library is to avoid [magic numbers](https://en.wikipedia.org/wiki/Magic_number_(programming)#Unnamed_numerical_constants) and other `"permanent data"` from the code. And give the ability to user to change the `"constants"` in friendly interface.

Accounts by specific mask or texts in generated FI documents or range of BLART in selections are all good examples of options to maintain.

The best way to describe the library,it is something like tr. STVARV but all parameters and select-options are grouped together as in tr. SLG1, with the interface similar to SAP Fiori and displayed directly in SAP GUI via CL_GUI_HTML_VIEWER.

The maintainable data usually stored in a program structure (or in class attributes) and can contain data such as:
* ranges (SELECT-OPTION)
* parameters (any simple value as dates, time or BUKRS)
* strings (memo texts)
* any tables (structure based tables)

The first 2 are completely similar to STVARV, in strings for example you can store message templates, and tables are suitable when you need to write a large CASE that depends on a condition that can change, but creating a database table is burdensome.

---

For more information read [wiki](https://github.com/bizhuka/aqo/wiki)

| English| Русский |
|-------------|-------------|
|[Creating an option in SE38](https://github.com/bizhuka/aqo/wiki/en-00-SE38-ZAQO_TEST)| [Создание настройки SE38](https://github.com/bizhuka/aqo/wiki/ru-00-SE38-ZAQO_TEST) |
|[Creating an option in SE24](https://github.com/bizhuka/aqo/wiki/en-00-SE24-ZCL_AQO_TESTER)| [Создание настройки SE24](https://github.com/bizhuka/aqo/wiki/ru-00-SE24-ZCL_AQO_TESTER)  |
|[Which edit transaction to choose?](https://github.com/bizhuka/aqo/wiki/en-01-Which-transaction-to-choose) | [Какую транзакцию ведения выбрать?](https://github.com/bizhuka/aqo/wiki/ru-01-Какую-транзакцию-выбрать)  |
|[Interface description](https://github.com/bizhuka/aqo/wiki/en-09-Interface-description)| [Описание интерфейса](https://github.com/bizhuka/aqo/wiki/ru-09-Описание-интерфейса)  |
|[Changing fields descriptions](https://github.com/bizhuka/aqo/wiki/en-08-Changing-fields-descriptions)| [Изменение описания полей](https://github.com/bizhuka/aqo/wiki/ru-08-Изменение-описания-полей)  |
|[Complex data types](https://github.com/bizhuka/aqo/wiki/en-04-Complex-data-types)| [Сложные типы данных](https://github.com/bizhuka/aqo/wiki/ru-04-Сложные-типы-данных)  |
|[Search help in ui5](https://github.com/bizhuka/aqo/wiki/en-07-Search-help-in-ui5)| [Средство поиска в ui5](https://github.com/bizhuka/aqo/wiki/ru-07-Средство-поиска-в-ui5)  |
|[Create option via interface](https://github.com/bizhuka/aqo/wiki/en-06-Create-Option-Via-Interface)| [Создание настройки через интерфейс](https://github.com/bizhuka/aqo/wiki/ru-06-Создание-настройки-через-интерфейс)  |
|[Export and import data](https://github.com/bizhuka/aqo/wiki/en-05-Export-and-import-data)| [Экспорт и импорт данных](https://github.com/bizhuka/aqo/wiki/ru-05-Экспорт-и-импорт-данных)  |
|[Extensions](https://github.com/bizhuka/aqo/wiki/en-03-Extensions)| [Расширения](https://github.com/bizhuka/aqo/wiki/ru-03-Расширения)  |
|[Search abap code](https://github.com/bizhuka/aqo/wiki/en-02-Search-abap-code)| [Поиск кода с настройкой](https://github.com/bizhuka/aqo/wiki/ru-02-Поиск-кода-с-настройкой)  |
|[Template storage](https://github.com/bizhuka/aqo/wiki/en-10-OAOR)| [Хранение шаблонов](https://github.com/bizhuka/aqo/wiki/ru-10-OAOR)  |
|[Installation guide](https://github.com/bizhuka/aqo/wiki/Installation-guide)| [Инструкция по установке](https://github.com/bizhuka/aqo/wiki/Инструкция-по-установке)  |


---

### From 2020
You have to install [EUI](https://github.com/bizhuka/eui) library first


### Due to installation problems
the [aqo_ui5](https://github.com/bizhuka/aqo_ui5) version should installed separately as an add-in
