### ABAP quick options

#### Launch:
#### tr. SE38-> ZAQO_TESTER, tr. SE24 -> ZCL_AQO_TESTER  ===> F8
#### tr. ZAQO_EDITOR_OLD

The main purpose of the library is to avoid [magic numbers](https://en.wikipedia.org/wiki/Magic_number_(programming)#Unnamed_numerical_constants) and other `"permanent data"` from the code. And give the ability to user to change the `"constants"` in friendly interface.

Accounts by specific mask or texts in generated FI documents or range of BLART in selections are all good examples of options to maintain.

The best way to describe the library,it is something like tr. STVARV but all parameters and select-options are grouped together as in tr. SLG1, with the interface similar to SAP Fiori and displayed directly in SAP GUI via CL_GUI_HTML_VIEWER.

The maintainable data usually stored in a program structure (or in class attributes) and can contain data such as:
* ranges (SELECT-OPTION)
* parameters (any simple value as dates, time or BUKRS)
* strings (memo texts)
* any tables (structure based tables)

The first 2 are completely similar to STVARV, in strings for example you can store message templates, and tables are suitable when you need to write a large CASE that depends on a condition that can change, but creating a database table is burdensome.

For more information read [wiki](https://github.com/bizhuka/aqo/wiki)


### From 2020
You have to install [EUI](https://github.com/bizhuka/eui) library first


### Due to installation problems
the [aqo_ui5](https://github.com/bizhuka/aqo_ui5) version should installed separately as an add-in
