sap.ui.define([
    'com/modekz/aqo/model/formatter',
    'com/modekz/aqo/model/f4Helper',
    'sap/m/Button',
    'sap/m/CheckBox',
    'sap/m/ComboBox',
    'sap/m/DatePicker',
    'sap/m/DateTimePicker',
    'sap/m/Dialog',
    'sap/m/TimePicker',
    'sap/m/Input',
    'sap/m/InputType',
    'sap/m/MessageBox',
    'sap/m/MultiInput',
    'sap/m/TextArea',
    'sap/m/Token',
    'sap/m/HBox',
    'sap/m/VBox',

    'sap/ui/table/Table',
    'sap/ui/table/Column',
    'sap/m/StepInput',
    'sap/m/Text',
    'sap/m/Toolbar',
    'sap/ui/unified/FileUploader',

    'sap/ui/core/Item',
    'sap/ui/core/IconPool',
    'sap/ui/comp/providers/TokenParser',
    'sap/ui/comp/odata/type/StringDate',
    'sap/ui/model/json/JSONModel',
    'sap/ui/model/odata/type/Double',
    'sap/ui/model/odata/type/String',
    'sap/ui/model/odata/type/Int16',
    'sap/ui/model/odata/type/Single',
    'sap/ui/model/odata/type/Decimal',
    'sap/ui/model/odata/type/Date',
    'sap/ui/model/odata/type/DateTimeOffset',
    'sap/ui/model/odata/type/Time',
    'sap/ui/model/odata/type/DateTime',
    'sap/ui/model/odata/type/Boolean',
    'sap/ui/model/type/Date',
    'sap/ui/model/type/Time',
    'sap/ui/model/type/Boolean'
], function (formatter, f4Helper, Button, CheckBox, ComboBox, DatePicker, DateTimePicker, Dialog, TimePicker, Input, InputType, MessageBox, MultiInput, TextArea, Token, HBox, VBox,
             Table, Column, StepInput, Text, Toolbar, FileUploader,
             CoreItem, IconPool, TokenParser, StringDate, JSONModel, typeDouble, typeString, typeInt16, typeSingle, typeDecimal, typeOdataDate, typeOdataDateTimeOffset, typeOdataTime, typeOdataDateTime, typeOdataBoolean, typeDate, typeTime, typeBoolean) {
    "use strict";

    return {
        shItems: [],

        createControl: function (params) {
            var _this = this;
            var checkF4 = false;
            var control = null;

            if (params.isEnabled === undefined)
                params.isEnabled = "{= ${appView>/IS_READ_ONLY}===true? false : ${fld>IS_EDITABLE} || ${appView>/IS_DEV} }";

            switch (params.field.UI_TYPE) {
                case "char":
                    checkF4 = true;
                    control = new Input({
                        value: "{fld>" + params.valueField + "}",
                        width: "100%",
                        enabled: params.isEnabled
                    });
                    break;

                case "numc":
                case "numeric":
                    checkF4 = true;
                    control = new Input({
                        value: "{fld>" + params.valueField + "}",
                        width: "100%",
                        type: InputType.Number,
                        enabled: params.isEnabled
                    });
                    break;

                case "string":
                    control = new TextArea({
                        value: "{fld>" + params.valueField + "}",
                        width: "100%",
                        valueLiveUpdate: true,
                        growing: true,
                        growingMaxLines: 5,
                        enabled: params.isEnabled
                    });
                    break;

                case "boolean":
                    control = new CheckBox({
                        selected: "{fld>" + params.valueField + "}",
                        width: "100%",
                        enabled: params.isEnabled
                    });
                    break;

                case "date":
                    control = new DatePicker({
                        value: "{fld>" + params.valueField + "}",
                        valueFormat: "yyyy-MM-dd",
                        displayFormat: "long",
                        enabled: params.isEnabled
                    });
                    break;

                case "time":
                    control = new TimePicker({
                        value: "{fld>" + params.valueField + "}",
                        width: "100%",
                        valueFormat: "HH:mm:ss",
                        displayFormat: "HH:mm:ss",
                        enabled: params.isEnabled
                    });
                    break;

                case "datetime":
                    // All by default
                    control = new DateTimePicker({
                        value: "{fld>" + params.valueField + "}",
                        width: "100%",
                        valueFormat: "yyyy-MM-ddTHH:mm:ss",
                        enabled: params.isEnabled
                    });
                    break;

                case "range":
                    checkF4 = true;
                    control = this._getRangeControl(params);
                    break;

                case "table":
                    control = this._getTableControl(params);
                    break;
            }

            // Oops!
            if (control === null)
                throw  "No case for UI_TYPE: " + params.field.UI_TYPE;

            // Has F4 ?
            checkF4 = checkF4 && !params.field.IS_KEY && params.field.ROLLNAME.indexOf('-') > 0;
            if (checkF4)
                _this._addShItem({
                    owner: params.owner,

                    control: control,

                    isRange: params.field.UI_TYPE === "range",

                    title: params.field.LABEL,

                    parentKey: params.field.NAME,

                    entityName: 'FLD_' + params.field.ROLLNAME,

                    uiWrapper: _this,

                    shIsReady: function (result) {
                        _this.checkIsReady(control, params, result);
                    }
                });
            else
                _this.checkIsReady(control, params);

            // Always in VBox
            return new VBox({
                items: [control],
                width: "100%"
            });
        },

        _addShItem: function (shItem) {
            this.shItems.push(shItem);
        },

        findAllSh: function () {
            if (this.shItems.length === 0)
                return;

            // Search SH for fields
            f4Helper.addSearchHelps(this.shItems);

            // For next call
            this.shItems = [];
        },

        // What control to return
        checkIsReady: function (control, params, result) {
            // Input -> ComboBox
            var parent = control.getParent();
            if (params.field.UI_TYPE !== "range" && parent instanceof VBox && result && result.TABLE && result.TABLE.length > 0) {
                parent.removeAllItems();

                // Create combo box instead
                control = new ComboBox({
                    selectedKey: "{fld>" + params.valueField + "}",
                    width: "100%",
                    enabled: params.isEnabled,
                    items: {
                        path: "cb>/",
                        templateShareable: false,
                        template: new CoreItem({
                            key: "{cb>_LOW}",
                            text: "({cb>_LOW}) - {cb>_TEXT}"
                        })
                    }
                });
                control.setModel(new JSONModel(result.TABLE), "cb");
                parent.addItem(control);
            }

            if (params.controlIsReady)
                params.controlIsReady(control);
        },

        getFieldValue: function (params) {
            var _this = this;
            var getMethod = null;
            var value = null;

            // Get first child of VBox in HBox
            params.control = params.control.getItems()[1].getItems()[0];
            switch (params.field.UI_TYPE) {
                case "string":
                case "date":
                case "time":
                case "datetime":
                case "char":
                case "numc":
                case "numeric":
                    getMethod = "getValue";

                    // Get key of combo
                    if (params.control instanceof ComboBox)
                        getMethod = "getSelectedKey";
                    break;

                case "boolean":
                    getMethod = "getSelected";
                    break;

                case "range":
                    value = _this._getTokensAsRanges(params.control, true);
                    break;

                case "table":
                    var object = params.control.getBindingContext('fld').getObject();
                    value = object[params.control.__valueField];
                    break;

                default:
                    throw  "No case for UI_TYPE: " + field.UI_TYPE;
            }

            // Just call by name
            if (getMethod)
                value = params.control[getMethod]();

            return value;
        },

        getUiIcon: function (uiType) {
            switch (uiType) {
                case "char":
                    return IconPool.getIconURI("text");
                case "numc":
                case "numeric":
                    return IconPool.getIconURI("number-sign");
                case "string":
                    return IconPool.getIconURI("request");
                case "boolean":
                    return IconPool.getIconURI("complete");
                case "date":
                    return IconPool.getIconURI("calendar");
                case "time":
                    return IconPool.getIconURI("history");
                case "datetime":
                    return IconPool.getIconURI("date-time");
                case "range":
                    return IconPool.getIconURI("multi-select");
                case "table":
                    return IconPool.getIconURI("table-view");
                default:
                    throw  new Error("No case for UI_TYPE: " + uiType);
            }
        },

        getOType: function (field) {
            function addFormatter(dateTimeType) {
                dateTimeType.getModelFormat = function () {
                    return {
                        parse: function (value) {
                            if (typeof value === "object")
                                return value;
                            return new Date(Date.parse(value));
                        }
                    }
                };

                return dateTimeType;
            }

            switch (field.UI_TYPE) {

                case "time":
                    return addFormatter(new typeTime({
                        UTC: false,
                        style: "short",
                        strictParsing: true
                    }));

                case "date":
                    return addFormatter(new typeDate({
                        strictParsing: true
                    }));

                case "datetime":
                    return addFormatter(new typeDate({ // StringDate typeOdataDateTime typeOdataDateTimeOffset
                        UTC: false,
                        strictParsing: true,
                        format: "yMMMdHms"
                    }));

                case "numc":
                    return new typeString({}, {
                        isDigitSequence: true,
                        maxLength: field.LENGTH
                    });

                case "numeric":
                    return new typeDouble({
                        scale: field.LENGTH,
                        precision: field.DECINALS
                    });
            }

            return null;
        },

        _getRangeControl: function (params) {
            var _this = this;

            var tokenInput = new MultiInput({
                showValueHelp: false,
                showSuggestion: true,
                showTableSuggestionValueHelp: true,
                enableSuggestionsHighlighting: true,
                tokens: {
                    path: "fld>" + params.valueField, // params.controlPath + "/"
                    factory: function (index, oContext) {
                        // Previous tokens
                        var range = oContext.getObject();
                        var text = range.LOW;
                        switch (range.OPTION) {
                            case "EQ":
                                text = "=" + text;
                                break;

                            case "BT":
                                text = range.LOW + "..." + range.HIGH;
                                break;

                            case "LT":
                                text = "<" + text;
                                break;

                            case "LE":
                                text = "<=" + text;
                                break;

                            case "GT":
                                text = ">" + text;
                                break;

                            case "GE":
                                text = ">=" + text;
                                break;

                            case "NE":
                                text = "!" + text;
                                break;

                            case "CP":
                                // Already has *
                                break;

                            default:
                                throw "Unknown SELECT.OPTION=" + range.OPTION
                        }

                        return new Token({text: text}).data("range", {
                            keyField: params.field.NAME,
                            exclude: false,
                            operation: range.OPTION,
                            value1: range.LOW,
                            value2: range.HIGH
                        });
                    }
                },
                enabled: params.isEnabled,
                tooltip: '{i18n>range_tooltip}'
            });

            // instead of constructor
            _this._initializeRange(tokenInput, params);

            // If in table
            var superClone = MultiInput.prototype.clone;
            tokenInput.clone = function (index, oContext) {
                var that = this;
                that = superClone.apply(that, arguments);

                // Init copy
                _this._initializeRange(that, params, true);
                return that;
            };

            return tokenInput;
        },

        _onRangeChanged: function (oEvent) {
            var _this = this;
            var tokenInput = oEvent.getSource();

            // No need in ordinary ranges
            if (!tokenInput._inTable)
                return;

            // Path and model
            var subPath = tokenInput.getBindingContext('fld').sPath;
            var fldModel = tokenInput._params.owner.getModel('fld');

            // Delete or insert and then update
            setTimeout(function () {
                // update in model
                fldModel.setProperty(subPath + "/" + tokenInput._params.valueField,
                    _this._getTokensAsRanges(tokenInput, true));
            }, 300);
        },

        _initializeRange: function (tokenInput, params, inTable) {
            var _this = this;
            tokenInput._params = params;
            tokenInput._inTable = inTable;

            // For table in table
            tokenInput.attachTokenUpdate(_this._onRangeChanged.bind(_this));
            tokenInput.attachLiveChange(_this._onRangeChanged.bind(_this));

            // When press enter without any sign use  '='
            var oTokenParser = new TokenParser("EQ");
            oTokenParser.associateInput(tokenInput);

            switch (params.field.UI_TYPE) {
                case "char":
                case "range":
                    oTokenParser.addKeyField({
                        key: params.field.NAME,
                        label: "",
                        type: "string",
                        maxLength: params.field.LENGTH,
                        //displayFormat: "UpperCase"
                        oType: _this.getOType(params.field)
                    });
                    break;

                case "numc":
                    oTokenParser.addKeyField({
                        key: params.field.NAME,
                        label: "",
                        type: "numc",
                        maxLength: params.field.LENGTH,
                        oType: _this.getOType(params.field)
                    });
                    break;

                case "string":
                    oTokenParser.addKeyField({
                        key: params.field.NAME,
                        label: "",
                        type: "string"
                    });
                    break;

                case "numeric":
                    oTokenParser.addKeyField({
                        key: params.field.NAME,
                        label: "",
                        type: "numeric",
                        oType: _this.getOType(params.field)
                    });
                    break;

                case "boolean":
                    oTokenParser.addKeyField({
                        key: params.field.NAME,
                        label: "",
                        type: "boolean",
                        oType: new typeBoolean()
                    });
                    break;

                case "date":
                    oTokenParser.addKeyField({
                        key: params.field.NAME,
                        label: "",
                        type: "date", // "stringdate"
                        oType: _this.getOType(params.field)
                    });
                    break;

                case "time":
                    oTokenParser.addKeyField({
                        key: params.field.NAME,
                        label: "",
                        type: "time",
                        oType: _this.getOType(params.field)
                    });
                    break;

                case "datetime":
                    oTokenParser.addKeyField({
                        key: params.field.NAME,
                        label: "",
                        type: "datetime",
                        oType: _this.getOType(params.field)
                    });
                    break;

                default:
                    throw "UI TYPE " + params.field.UI_TYPE;
            }
        },

        _getTokensAsRanges: function (tokenInput, forSave) {
            var tokens = tokenInput.getTokens();

            var result = [];

            for (var t = 0; t < tokens.length; t++) {
                var range = tokens[t].getCustomData()[0].getValue();
                var item = {
                    FIELD: forSave ? undefined : range.keyField,
                    SIGN: range.exclude ? 'E' : 'I', // Always 'I'
                    OPTION: range.operation,
                    LOW: range.value1,
                    HIGH: range.value2
                };

                switch (tokenInput._params.field.UI_TYPE) {
                    case "date": //TODO ALL ?
                        item.LOW = formatter.toSapDate(item.LOW);
                        item.HIGH = formatter.toSapDate(item.HIGH);
                        break;

                    case "time":
                        item.LOW = formatter.toSapTime(item.LOW);
                        item.HIGH = formatter.toSapTime(item.HIGH);
                        break;

                    case "datetime":
                        item.LOW = formatter.toSapDateTime(item.LOW);
                        item.HIGH = formatter.toSapDateTime(item.HIGH);
                        break;
                }

                if (typeof item.LOW === "string") {
                    // Use mask
                    if (item.LOW.indexOf('*') >= 0) {
                        item.HIGH = '';
                        item.OPTION = 'CP';
                    }

                    // Between
                    var values = item.LOW.split('..');
                    if (values.length === 2) {
                        item.LOW = values[0];
                        item.HIGH = values[1];
                        item.OPTION = 'BT';
                    }

                    if (item.LOW.substr(0, 1) === '!') {
                        var bDel = false;
                        switch (item.OPTION) {
                            case "EQ":
                                bDel = true;
                                item.OPTION = "NE"; // Or exclude ?
                                break;

                            case "BT":
                            case "CP":
                                bDel = true;
                                break;
                        }
                        if (bDel)
                            item.LOW = item.LOW.substr(1);
                    }
                }
                // And add
                result.push(item);
            }
            return result;
        },

        _getTableControl: function (params) {
            var _this = this;

            // Do not use formula
            var appData = params.owner.getModel("appView").getProperty("/");

            // Replace typeof "string"
            if (typeof params.isEnabled !== "boolean")
                params.isEnabled = appData.IS_READ_ONLY ? false : (params.field.IS_EDITABLE || appData.IS_DEV);

            var button = new Button({
                width: "100%",
                text: {
                    path: "fld>" + params.valueField,
                    formatter: function (value) {
                        return _this._getCountText(params.owner, value);
                    }
                },
                press: function (oEvent) {
                    var curButton = oEvent.getSource();
                    var subPath = curButton.getBindingContext('fld').sPath;

                    // Columns and rows
                    var columns = [];

                    var initCount = 0;
                    for (var c = 0; c < params.field.SUB_COLUMNS.length; c++) {
                        var subColumn = params.field.SUB_COLUMNS[c];

                        // Create each control and wait for F4
                        var cell = _this.createControl({
                            owner: params.owner,
                            field: subColumn,
                            valueField: subColumn.NAME,
                            isEnabled: params.isEnabled,
                            subPath: subPath,

                            controlIsReady: function (control) {
                                if (params.owner.setChangeListener)
                                    params.owner.setChangeListener(control);

                                if (++initCount === params.field.SUB_COLUMNS.length)
                                    _this._columnsIsReady(params, subPath, curButton, columns);
                            }
                        });

                        columns.push(new Column({
                            label: subColumn.LABEL,

                            template: cell
                        }));
                    }

                    _this.findAllSh();
                }
            });

            // Just add to UI control for simplicity
            button.__valueField = params.valueField;
            return button;
        },

        _getCountText: function (owner, value) {
            if (!this.__countText)
                this.__countText = owner.getResourceBundle().getText('count');
            return this.__countText + " " + (value ? value.length : "0");
        },

        _columnsIsReady: function (params, subPath, button, columns) {
            var _this = this;
            var rowsPath = subPath + "/" + params.valueField;
            var fldModel = params.owner.getModel('fld');

            function updateCountTable(rows) {
                // New rows
                fldModel.setProperty(rowsPath, rows);

                // Add or delete row in table
                params.owner.on_set_changed();

                // Update manually
                button.setText(_this._getCountText(params.owner, rows));
            }

            var oTable = new Table({

                // Select mode for deleting rows
                selectionMode: params.isEnabled ? 'MultiToggle' : 'None',

                columns: columns,

                title: new Toolbar({
                    content: [
                        // Delete selected rows
                        new Button({
                            icon: 'sap-icon://negative',
                            enabled: params.isEnabled,

                            press: function () {
                                var rows = fldModel.getProperty(rowsPath);
                                var indices = oTable.getSelectedIndices();
                                oTable.setSelectedIndex(-1);

                                for (var i = indices.length - 1; i >= 0; i--) {
                                    var index = indices[i];
                                    rows.splice(index, 1);
                                }
                                updateCountTable(rows);
                            }
                        }).addStyleClass("sapUiTinyMarginEnd"),

                        // Insert new row
                        new Button({
                            icon: 'sap-icon://positive',
                            enabled: params.isEnabled,

                            press: function () {
                                var rows = fldModel.getProperty(rowsPath);

                                var box = new VBox({
                                    items: [
                                        new Text({
                                            text: params.owner.getResourceBundle().getText("insertPos", [1, rows.length + 1])
                                        }),
                                        new StepInput("edPosition", {
                                            value: 1,
                                            min: 1,
                                            max: rows.length + 1
                                        })
                                    ]
                                });

                                MessageBox.show(
                                    box, {
                                        icon: MessageBox.Icon.INFORMATION,
                                        title: "{i18n>/NEW_ROW}",
                                        actions: [MessageBox.Action.YES, MessageBox.Action.NO],
                                        onClose: function (oAction) {
                                            if (oAction !== MessageBox.Action.YES)
                                                return;
                                            var pos = params.owner.findById("edPosition").getValue();

                                            var new_row = {};
                                            for (var i = 0; i < params.field.SUB_COLUMNS.length; i++)
                                                new_row[params.field.SUB_COLUMNS[i].NAME] = '';

                                            if (pos === 1)
                                                rows.unshift(new_row);
                                            else if (pos === rows.length + 1)
                                                rows.push(new_row);
                                            else
                                                rows.splice(pos, 0, new_row);

                                            updateCountTable(rows);
                                        }
                                    }
                                );
                            }
                        }).addStyleClass("sapUiMediumMarginEnd"),

                        new Button({
                            icon: 'sap-icon://download',
                            enabled: params.isEnabled,

                            press: function () {
                                var option = params.owner.getModel('mstr').getProperty("/");
                                var result = {
                                    file_name: option.PACKAGE_ID + '-' + option.OPTION_ID + '-' + params.field.NAME + '.csv',
                                    charset: '4103', // utf-16le
                                    content: ""
                                };

                                // All rows
                                var rows = fldModel.getProperty(rowsPath);

                                // Header
                                var columns = [];
                                for (var c = 0; c < params.field.SUB_COLUMNS.length; c++) {
                                    var subColumn = params.field.SUB_COLUMNS[c];

                                    columns.push(subColumn.NAME);
                                }
                                result.content = columns.join("\t");

                                // rows
                                for (var r = 0; r < rows.length; r++) {
                                    var oneRow = [];
                                    for (c = 0; c < columns.length; c++) {
                                        var value = rows[r][columns[c]];
                                        if (typeof value === "object")
                                            value = JSON.stringify(value);
                                        oneRow.push(value);
                                    }
                                    result.content += ("\r\n" + oneRow.join("\t"));
                                }

                                // Save as file
                                if (getHttpType() === HttpType.SAP)
                                    postAction("SAP_DOWNLOAD_FILE", result);
                                else
                                    saveFileAs(result);
                            }
                        }).addStyleClass("sapUiTinyMarginEnd"),

                        new FileUploader({
                            icon: 'sap-icon://upload',
                            iconOnly: true,
                            buttonOnly: true,
                            sameFilenameAllowed: true,
                            enabled: params.isEnabled,
                            tooltip: "*.csv (UTF-16LE)",

                            change: function (oEvent) {
                                var file = oEvent.getParameters('files').files[0];
                                if (!file)
                                    return;

                                var reader = new FileReader();
                                reader.onload = function () {
                                    var bytes = new Uint8Array(reader.result);
                                    var data = decodeUTF16LE(bytes, true);

                                    // Result data
                                    var rows = [];

                                    // Form text
                                    data = data.split("\r\n");
                                    for (var d = 0; d < data.length; d++) {
                                        var cols = data[d].split("\t");

                                        // Header first
                                        if (d === 0) {
                                            var header = cols;
                                            continue;
                                        }

                                        var obj = {};
                                        for (var c = 0; c < cols.length; c++) {
                                            var val = cols[c];

                                            // Array ?
                                            try {
                                                if (val[val.length - 1] === '"' && val[0] === '"')
                                                    val = val.replace(/""/g, '"').slice(1, -1);
                                                val = JSON.parse(val)
                                            } catch (e) {

                                            }
                                            obj[header[c]] = val;
                                        }
                                        rows.push(obj)
                                    }

                                    // Set data
                                    fldModel.setProperty(rowsPath, rows);

                                    var option = params.owner.getModel('mstr').getProperty("/");
                                    params.owner.showMessage({
                                        i18n: 'data_uploaded',
                                        i18n_param: [option.PACKAGE_ID + "-" + option.OPTION_ID, params.field.NAME]
                                    });

                                    // On load new data
                                    params.owner.on_set_changed();
                                };
                                reader.readAsArrayBuffer(file);
                            }
                        }).addStyleClass("sapUiTinyMarginEnd")
                    ]
                })
            });

            // Just set template
            oTable.bindRows("fld>" + rowsPath);

            var dialog = new Dialog({
                contentWidth: "80%",
                title: params.field.LABEL,
                content: [
                    oTable
                ],

                buttons: [new Button({
                    icon: "sap-icon://accept",
                    press: function () {
                        dialog.close();
                    }
                })],

                afterClose: function () {
                    dialog.destroy();
                }
            });

            dialog.setModel(fldModel, 'fld');
            dialog.addStyleClass(params.owner.getOwnerComponent().getContentDensityClass());
            dialog.open();
        }
    };

});