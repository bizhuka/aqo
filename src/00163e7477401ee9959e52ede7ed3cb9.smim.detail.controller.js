/*global location */
sap.ui.define([
    "./BaseController",
    "com/modekz/aqo/model/f4Helper",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",

    "../model/formatter",
    "sap/m/library",
    "sap/m/Button",
    "sap/m/MessageBox",
    "sap/m/MessageToast",
    "sap/m/Input",
    "com/modekz/aqo/model/uiWrapper"
], function (BaseController, f4Helper, JSONModel, Filter, FilterOperator, formatter, mobileLibrary, Button, MessageBox, MessageToast, Input, uiWrapper) {
    "use strict";

    return BaseController.extend("com.modekz.aqo.controller.Detail", {

        formatter: formatter,
        uiWrapper: uiWrapper,

        /* =========================================================== */
        /* lifecycle methods                                           */
        /* =========================================================== */

        onInit: function () {
            // Model used to manipulate control states. The chosen values make sure,
            // detail page is busy indication immediately so there is no break in
            // between the busy indication for loading the view's meta data
            var oViewModel = new JSONModel({
                busy: false,
                delay: 0,
                lineItemListTitle: this.getResourceBundle().getText("detailLineItemTableHeading")
            });
            this.setModel(oViewModel, "detailView");

            // Binds the view to the object path and expands the aggregated line items.
            this.getRouter().getRoute("object").attachPatternMatched(function (oEvent) { // oEvent pattern match event in route 'object'
                var appModel = this.getModel("appView");
                appModel.setProperty("/layout", "TwoColumnsMidExpanded");

                // Update with new filter
                this.getModel().metadataLoaded().then(function () {
                    this._bindView();
                }.bind(this));
            }, this);

            this.getOwnerComponent().getModel().metadataLoaded().then(
                function () {
                    // Store original busy indicator delay for the detail view
                    var iOriginalViewBusyDelay = this.getView().getBusyIndicatorDelay(),
                        oViewModel = this.getModel("detailView"),
                        oLineItemTable = this.byId("id_edit_table"),
                        iOriginalLineItemTableBusyDelay = oLineItemTable.getBusyIndicatorDelay();

                    // Make sure busy indicator is displayed immediately when
                    // detail view is displayed for the first time
                    oViewModel.setProperty("/delay", 0);
                    oViewModel.setProperty("/lineItemTableDelay", 0);

                    oLineItemTable.attachEventOnce("updateFinished", function () {
                        // Restore original busy indicator delay for line item table
                        oViewModel.setProperty("/lineItemTableDelay", iOriginalLineItemTableBusyDelay);
                    });

                    // Binding the view will set it to not busy - so the view is always busy if it is not bound
                    oViewModel.setProperty("/busy", true);
                    // Restore original busy indicator delay for the detail view
                    oViewModel.setProperty("/delay", iOriginalViewBusyDelay);
                }.bind(this));
        },

        /* =========================================================== */
        /* event handlers                                              */
        /* =========================================================== */

        // Event handler when the share by E-Mail button has been clicked
        onSendEmailPress: function () {
            var oBundle = this.getResourceBundle();
            // shortcut for sap.m.URLHelper
            var URLHelper = mobileLibrary.URLHelper;

            URLHelper.triggerEmail(
                'modekz@gmail.com',
                oBundle.getText("emailSubject"),
                oBundle.getText("emailMessage")
            );
        },


        /**
         * Updates the item count within the line item table's header
         * @param {object} oEvent an event containing the total number of items in the list
         */
        onListUpdateFinished: function (oEvent) {
            var sTitle,
                iTotalItems = oEvent.getParameter("total"),
                oViewModel = this.getModel("detailView");

            // only update the counter if the length is final
            var oTable = this.byId("id_edit_table");
            if (oTable.getBinding("items").isLengthFinal()) {
                if (iTotalItems) {
                    sTitle = this.getResourceBundle().getText("detailLineItemTableHeadingCount", [iTotalItems]);
                } else {
                    //Display 'Line Items' instead of 'Line items (0)'
                    sTitle = this.getResourceBundle().getText("detailLineItemTableHeading");
                }
                oViewModel.setProperty("/lineItemListTitle", sTitle);

                // Create new last column
                if (oEvent.getParameter("reason") === 'Change')
                    this.createValueControl(oTable.getItems())
            }
        },

        /* =========================================================== */
        /* begin: internal methods                                     */
        /* =========================================================== */

        // Table event
        onUpdateStartedFields: function () {
            // TODO !!!!!!!!!  this._bindView();
        },

        // Set the full screen mode to false and navigate to master page
        onCloseDetailPress: function () {
            this.getModel("appView").setProperty("/actionButtonsInfo/midColumn/fullScreen", false);
            // No item should be selected on master after detail page is closed
            this.getOwnerComponent().oListSelector.clearMasterListSelection();
            this.getRouter().navTo("master");
        },

        // Toggle between full and non full screen mode.
        toggleFullScreen: function () {
            var bFullScreen = this.getModel("appView").getProperty("/actionButtonsInfo/midColumn/fullScreen");
            this.getModel("appView").setProperty("/actionButtonsInfo/midColumn/fullScreen", !bFullScreen);
            if (!bFullScreen) {
                // store current layout and go full screen
                this.getModel("appView").setProperty("/previousLayout", this.getModel("appView").getProperty("/layout"));
                this.getModel("appView").setProperty("/layout", "MidColumnFullScreen");
            } else {
                // reset to previous layout
                this.getModel("appView").setProperty("/layout", this.getModel("appView").getProperty("/previousLayout"));
            }
        },

        /**
         * Binds the view to the object path. Makes sure that detail view displays
         * a busy indicator while data for the corresponding element binding is loaded.
         */
        _bindView: function () {
            var _this = this;

            // Current filter
            var curKey = {};
            var appModel = this.getModel("appView");

            if (appModel) {
                var curOption = appModel.getProperty("/curOption");
                curKey.packageId = curOption.packageId;
                curKey.optionId = curOption.optionId;
            }

            // Try to read from url
            if (!curKey.packageId) {
                var pos = window.location.href.indexOf('AqoOptions/');
                var remain = decodeURI(window.location.href.substr(pos + 'AqoOptions/'.length));
                remain = remain.split('/');
                curKey.packageId = remain[0];
                curKey.optionId = remain[1];
            }

            // Create key
            var sKeyPath = "/" + _this.getModel().createKey("AqoOptions", {
                PACKAGE_ID: curKey.packageId,
                OPTION_ID: curKey.optionId
            });

            // Bind and wait
            _this.getView().bindElement({
                path: sKeyPath,
                events: {
                    change: _this._onBindingChange.bind(_this)
                }
            });
        },

        _onBindingChange: function () {
            var _this = this,
                oView = _this.getView(),
                oElementBinding = oView.getElementBinding();

            // No data for the binding
            if (!oElementBinding.getBoundContext()) {
                _this._notFoundTimerId = setTimeout(function () {
                    _this.getRouter().getTargets().display("detailObjectNotFound");
                    // if object could not be found, the selection in the master list does not make sense anymore.
                    _this.getOwnerComponent().oListSelector.clearMasterListSelection();
                }, 1500);
                return;
            }

            // No need
            if (_this._notFoundTimerId)
                clearTimeout(_this._notFoundTimerId);

            // If found after 1500 msec
            _this.getRouter().getTargets().display("object");

            var sPath = oElementBinding.getPath(),
                oSelOption = oView.getModel().getObject(sPath),
                oViewModel = _this.getModel("detailView"); // Set busy indicator during view binding

            // Don't wait
            oViewModel.setProperty("/busy", false);

            // Init previous filter
            if (!_this._prevFilter)
                _this._prevFilter = {
                    packageId: null,
                    optionId: null
                };

            // Show info of master
            _this.setModel(new JSONModel(oSelOption), 'mstr');

            // Already filtered?
            var curFilter = {
                packageId: oSelOption.PACKAGE_ID,
                optionId: oSelOption.OPTION_ID
            };
            if (curFilter.packageId === this._prevFilter.packageId && curFilter.optionId === this._prevFilter.optionId)
                return;
            this._prevFilter = curFilter;

            // And set filter
            postAction("SAP_READ_OPTION", {
                package_id: curFilter.packageId,
                option_id: curFilter.optionId,

                onBack: function (data) {
                    data = data.DATA;

                    // Error
                    if (data.KIND && data.INFO_TEXT) {
                        _this.showMessage(data);
                        return
                    }

                    // Warning
                    if (data.WARNING_TEXT)
                        _this.showMessage({
                            INFO_TEXT: data.WARNING_TEXT
                        });

                    // Prepare view
                    var appModel = _this.getModel("appView");
                    var appData = appModel.getProperty("/");

                    // Just show option
                    appData.IS_READ_ONLY = data.IS_READ_ONLY;
                    if (window.location.href.indexOf('viewer=') > 0)
                        appData.IS_READ_ONLY = true;

                    if (_this.getModel('mstr').getProperty('/IS_NEW_OPTION'))
                        appData.techMode = true;
                    appModel.setProperty("/", appData);

                    var fldModel = _this.getModel('fld');
                    if (fldModel)
                        fldModel.setProperty('/field_rows', null);

                    // Set new data for model
                    data = {
                        field_rows: data.ROWS
                    };
                    _this.setModel(new JSONModel(data), 'fld');
                }
            });
        },

        on_toggle_edit: function () {
            var appModel = this.getModel("appView");
            var newMode = !appModel.getProperty("/techMode");
            appModel.setProperty("/techMode", newMode);
        },

        on_press_drill_down: function (oEvent) {
            var oContext = oEvent.getSource().getBindingContext('fld');
            var item = this.getModel('fld').getProperty(oContext.sPath);

            // Show field catalog
            if (item.UI_TYPE === "table") {
                this.editFieldCatalog(oContext);
                return;
            }

            var isSap = getHttpType() === HttpType.SAP;
            if (isSap && item.ROLLNAME.indexOf('-') > 0)
                postAction("SAP_DRILL_DOWN", {
                    datatype: item.ROLLNAME
                });

            if (!isSap)
                this.showMessage({
                    i18n: "se11_drill_down",
                    i18n_param: [item.ROLLNAME]
                });
        },

        editFieldCatalog: function (oContext) {
            var dialog = this.findById("id_fc_dialog");
            if (!dialog)
                dialog = this.createFragment("com.modekz.aqo.view.frag.FieldCatalogDialog");

            // Which field
            this.findById("id_fc_table").bindRows("fld>" + oContext.sPath + "/SUB_COLUMNS");

            // Path for 'fld' model
            dialog.bindElement({path: oContext.sPath, model: "fld"});

            dialog.open();
        },

        onFieldCatalogClose: function (oEvent) {
            this.findById("id_fc_dialog").close();
        },

        createValueControl: function (items) {
            var _this = this;

            // Create new cells
            for (var i = 0; i < items.length; i++) {
                var item = items[i];
                var context = item.getBindingContext('fld');
                var field = context.getObject();

                // 6-th cell
                var oHBox = item.getCells()[5];

                // delete prev controls (if exist) and add new one
                while (oHBox.getItems().length > 1)
                    oHBox.removeItem(1);

                oHBox.addItem(uiWrapper.createControl({
                    owner: this,
                    field: field,
                    valueField: "CUR_VALUE",

                    controlIsReady: function (control) {
                        _this.setChangeListener(control);
                    }
                }));
            }
            uiWrapper.findAllSh();
        },

        on_prev_val_click: function (oEvent) {
            var oButton = oEvent.getSource();
            var fldModel = this.getModel('fld');
            var oContext = oEvent.getSource().getBindingContext('fld');

            // Change order
            var item = fldModel.getProperty(oContext.sPath);

            var prevItems = [];
            // Don't show 1-st item ? '-2'
            for (var i = item.VALUE.length - 1; i >= 0; i--) {
                var prevItem = item.VALUE[i];
                var data = JSON.parse(prevItem.H_VALUE).DATA;
                if (item.UI_TYPE === "range" || item.UI_TYPE === "table") {
                    prevItem.TEXT_VALUE = "";
                    for (var r = 0; r < data.length; r++) {
                        var row = data[r];
                        prevItem.TEXT_VALUE += ((r === 0 ? "" : "\n\r") +
                            // encodeURIComponent unintelligible
                            JSON.stringify(row).slice(1, -1).replace(/"/g, "'"))
                    }
                } else
                    prevItem.TEXT_VALUE = data;
                prevItems.push(prevItem);
            }

            // Reverse order
            fldModel.setProperty(oContext.sPath + "/prevItems", prevItems);

            var prevQuickView = this.findById("id_previous_values");
            if (!prevQuickView)
                prevQuickView = this.createFragment("com.modekz.aqo.view.frag.PreviousValues");

            // Path for 'fld' model
            prevQuickView.bindElement({path: oContext.sPath, model: "fld"});

            // delay because addDependent will do a async rerendering and the actionSheet will immediately close without it.
            jQuery.sap.delayedCall(0, this, function () {
                prevQuickView.openBy(oButton);
            });
        },

        setChangeListener: function (control) {
            // If MultiInput
            if (control.attachTokenUpdate)
                control.attachTokenUpdate(this.on_set_changed.bind(this));

            // From Input
            if (control.attachLiveChange) {
                control.setValueLiveUpdate(true);
                control.attachLiveChange(this.on_set_changed.bind(this));
            } else if (control.attachChange) {
                control.attachChange(this.on_set_changed.bind(this));
            } else if (control.attachSelect) {
                control.attachSelect(this.on_set_changed.bind(this))
            } else if (control instanceof Button) {
                // Button for table
            } else
                throw "Unknown control";
        },

        on_set_changed: function () {
            var viewModel = this.getModel("appView");
            viewModel.setProperty("/is_changed", true);
        },

        on_save_press: function (oEvent) {
            var _this = this;
            var option = _this.getModel('mstr').getProperty("/");

            // Ask from a user
            MessageBox.confirm(_this.getResourceBundle().getText("save_option", [option.PACKAGE_ID, option.OPTION_ID]), {
                onClose: function (oAction) {
                    if (oAction !== MessageBox.Action.OK)
                        return;

                    var fields = _this._getCurrentFields(true);

                    // Save in SAP
                    var edtOption = {
                        DESCRIPTION: option.DESCRIPTION,
                        PREV_VALUE_CNT: option.PREV_VALUE_CNT
                    };

                    postAction("SAP_SAVE_OPTION", {
                        package_id: option.PACKAGE_ID,
                        option_id: option.OPTION_ID,
                        cur_option: edtOption,
                        cur_fields: fields,

                        mandt: oEvent.mandt ? oEvent.mandt : "",

                        onBack: function (message) {
                            message = message.DATA;
                            _this.showMessage(message);

                            if (message.KIND !== 'E')
                                _this.getModel("appView").setProperty("/is_changed", false);
                        }
                    })
                }
            });
        },

        _getCurrentFields: function (forSave) {
            var _this = this;
            // Collect data from every field
            var items = _this.byId("id_edit_table").getItems();

            var fields = [];
            for (var i = 0; i < items.length; i++) {
                var item = items[i];
                var context = item.getBindingContext('fld');
                var field = context.getObject();

                // Js data
                var value = uiWrapper.getFieldValue({
                    field: field,
                    control: item.getCells()[5]
                });

                // For ABAP convenience
                delete field.__metadata;

                if (forSave) {
                    // delete field.CUR_VALUE;
                    field.NEW_VALUE = JSON.stringify({
                        DATA: value
                    });

                    // TODO recursion ?
                    if (field.SUB_COLUMNS)
                        field.SUB_FDESC = JSON.stringify({
                            DATA: field.SUB_COLUMNS
                        });
                }

                fields.push(field);
            }

            // Send result back
            return fields;
        },

        on_download_press: function () {
            var option = this.getModel('mstr').getProperty("/");

            var result = {
                file_name: option.PACKAGE_ID + '-' + option.OPTION_ID + '.json',
                content: JSON.stringify(this._getCurrentFields(false))
            };

            if (getHttpType() === HttpType.SAP)
                postAction("SAP_DOWNLOAD_FILE", result);
            else
                saveFileAs(result);
        },

        on_file_uploaded: function (oEvent) {
            var _this = this;
            var file = oEvent.getParameters('files').files[0];
            if (!file)
                return;

            var reader = new FileReader();
            reader.onload = function () {
                var bytes = new Uint8Array(reader.result);
                var data = decodeUTF8(bytes, true);

                // As json
                data = JSON.parse(data);
                var oModel = _this.getModel('fld');
                var option = _this.getModel('mstr').getProperty("/");

                oModel.setProperty('/field_rows', data);

                _this.showMessage({
                    i18n: 'data_uploaded',
                    i18n_param: [option.PACKAGE_ID, option.OPTION_ID]
                });

                // On load new data
                _this.on_set_changed();
            };
            reader.readAsArrayBuffer(file);
        },

        on_copy_to_mandt_press: function () {
            var _this = this;
            var input = new Input();

            f4Helper.addSearchHelps([{
                owner: _this,

                control: input,

                isRange: false,

                title_i18n: 'copy2mandt',

                entityName: 'FLD_T001-MANDT',

                uiWrapper: _this.uiWrapper,

                shIsReady: function () {
                    f4Helper.onF4({
                        getSource: function () {
                            return input;
                        },

                        onAfterClose: function () {
                            if (!input.getValue())
                                return;

                            _this.on_save_press({
                                mandt: input.getValue()
                            })
                        }
                    });
                }
            }])
        },

        doDeleteOrTransport: function (request, bDelete) {
            var _this = this;
            var appModel = _this.getModel("appView");
            var curOption = appModel.getProperty("/curOption");

            postAction("SAP_DELETE_OR_TRANSPORT", {
                package_id: curOption.packageId,
                option_id: curOption.optionId,
                request: request,
                delete: bDelete,

                onBack: function (data) {
                    _this.showMessage(data.DATA)
                }
            });
        },

        on_delete_option: function () {
            var _this = this;
            var option = _this.getModel('mstr').getProperty("/");

            // Ask from a user
            MessageBox.confirm(_this.getResourceBundle().getText("delete_option", [option.PACKAGE_ID, option.OPTION_ID]), {
                onClose: function (oAction) {
                    if (oAction !== MessageBox.Action.OK)
                        return;

                    if (option.PACKAGE_ID.indexOf('$') === 0)
                        _this.doDeleteOrTransport('', true);
                    else
                        _this.on_transport_option(true);
                }
            });
        },

        on_transport_option: function (bDelete) {
            var _this = this;
            var input = new Input();

            f4Helper.addSearchHelps([{
                owner: _this,

                control: input,

                isRange: false,

                title_i18n: bDelete === true ? 'delete_opt' : 'transport_opt',

                entityName: 'SHLP_ZHAQO_TRKORR',

                uiWrapper: _this.uiWrapper,

                shIsReady: function () {
                    f4Helper.onF4({
                        getSource: function () {
                            return input;
                        },

                        onAfterClose: function () {
                            // Cancelled
                            if (!input.getValue())
                                return;

                            _this.doDeleteOrTransport(input.getValue(), bDelete === true);
                        }
                    });
                }
            }])
        },

        on_show_last_call: function (params) {
            if (!params._use_index || !params.index) {
                var item = this.getModel('mstr').getProperty("/");
                params = {
                    include: item.INCLUDE,
                    line: item.LINE
                }
            }
            if (getHttpType() === HttpType.SAP)
                postAction("SAP_NAVIGATE_TO", params);
            else
                this.showMessage({
                    i18n: "se38_navigate_to",
                    i18n_param: [params.include ? params.include : params.index]
                });
        },

        on_show_usage: function () {
            var _this = this;
            var input = new Input();

            f4Helper.addSearchHelps([{
                owner: _this,

                control: input,

                isRange: false,

                title_i18n: 'show_usage',

                entityName: 'SHLP_ZHAQO_USAGE',

                uiWrapper: _this.uiWrapper,

                shIsReady: function () {
                    f4Helper.onF4({
                        getSource: function () {
                            return input;
                        },

                        onAfterClose: function () {
                            // Cancelled
                            if (!input.getValue())
                                return;

                            _this.on_show_last_call({
                                _use_index: true,
                                index: input.getValue()
                            })
                        }
                    });
                }
            }])
        },

        on_delete_field: function () {
            var _this = this;
            var oFldModel = _this.getModel('fld');
            var oTable = _this.byId("id_edit_table");
            var appModel = _this.getModel("appView");
            var curOption = appModel.getProperty("/curOption");

            // Get selected items
            var items = oTable.getSelectedItems();
            if (items.length === 0)
                return;

            oTable.removeSelections();

            var indices = [];
            for (var i = items.length - 1; i >= 0; i--) {
                fields.push(items[i].getBindingContext('fld').getObject().NAME);

                var index = oTable.indexOfItem(items[i]);
                indices.push(index);
            }

            // No confirmation
            var rows = oFldModel.getProperty("/field_rows");
            for (i = 0; i < indices.length; i++)
                rows.splice(indices[i], 1);
            oFldModel.setProperty('/field_rows', rows);

            // Field deleted
            _this.on_set_changed();
        },

        on_add_new_field: function () {
            var _this = this;

            // Async load
            sap.ui.require(["com/modekz/aqo/controller/frag/AddNewFieldDialog"], function (AddNewFieldDialog) {
                var dialogController = new AddNewFieldDialog(_this);
                dialogController.open();
            });
        },

        on_edit_description: function () {
            var _this = this;
            var _model = _this.getModel('mstr');
            var option = _model.getProperty("/");

            // Async load
            sap.ui.require(["com/modekz/aqo/controller/frag/OptionCreateDialog"], function (OptionCreateDialog) {
                var dialogController = new OptionCreateDialog({
                    owner: _this,
                    option: option,
                    optionOkPressed: function (edtObject) {
                        // The same values
                        if (option.DESCRIPTION === edtObject.DESCRIPTION &&
                            option.PREV_VALUE_CNT === edtObject.PREV_VALUE_CNT)
                            return;

                        // Update to fields
                        option.DESCRIPTION = edtObject.DESCRIPTION;
                        option.PREV_VALUE_CNT = edtObject.PREV_VALUE_CNT;

                        _model.setProperty("/", option);

                        // Option description is changed
                        _this.on_set_changed();
                    }
                });
                dialogController.open();
            });
        }
    });

});