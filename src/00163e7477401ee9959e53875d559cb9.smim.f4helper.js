sap.ui.define([
    'sap/ui/comp/valuehelpdialog/ValueHelpDialog',
    'sap/ui/comp/filterbar/FilterBar',
    'sap/ui/comp/filterbar/FilterGroupItem',
    'sap/ui/comp/filterbar/FilterItem',
    'sap/ui/model/Filter',
    'sap/ui/model/FilterOperator',
    'sap/ui/model/json/JSONModel',
    'sap/m/Input',
    'sap/m/MultiInput',
    'sap/m/Token'
], function (ValueHelpDialog, FilterBar, FilterGroupItem, FilterItem, Filter, FilterOperator, JSONModel, Input, MultiInput, Token) {
    "use strict";

    var _this = {

        // Add F4
        addSearchHelps: function (arrParam) {
            var entities = [];
            for (var p = 0; p < arrParam.length; p++) {
                var param = arrParam[p];

                // For mass call
                entities.push(param.entityName);

                // Save fo later use
                param.control.data("F4", param);

                // Save standard
                param.control.attachValueHelpRequest(this.onF4);
            }

            // Get from SAP
            postAction("SAP_GET_SH_FIELDS", {
                entities: entities,

                onBack: function (data) {
                    var results = data.DATA;
                    for (var p = 0; p < arrParam.length; p++) {
                        var param = arrParam[p];
                        var result = results[p];

                        _this[param.entityName] = result;

                        if (result.FIELDS.length > 0)
                            param.control.setShowValueHelp(true);

                        // Ok
                        if (param.shIsReady)
                            param.shIsReady(result);
                    }
                }
            });
        },

        onF4: function (oEvent) {
            // Get from event
            var control = oEvent.getSource();
            var params = control.data("F4");
            var fields = _this[params.entityName].FIELDS;

            var bundle = params.owner.getResourceBundle();

            // Prepare columns & filterGroupItems
            var keyField = null;
            var colModel = {cols: []};
            var filterGroupItems = [];

            for (var i = 0; i < fields.length; i++) {
                var field = fields[i];
                var label = field.LABEL ? field.LABEL : bundle.getText(field.label_i18n);
                if (field.IS_KEY && keyField === null)
                    keyField = field;

                colModel.cols.push({
                    label: label,
                    template: field.NAME,
                    type: field.UI_TYPE,
                    oType: params.uiWrapper.getOType(field)
                });

                // Skip virtual fields
                if (!field.IS_SEARCHABLE)
                    continue;

                // Advanced search
                field.UI_TYPE = "range";

                filterGroupItems.push(new FilterGroupItem({
                    // groupTitle: "foo",
                    groupName: "gn1",
                    name: field.NAME,
                    label: label,
                    control: params.uiWrapper.createControl({
                        owner: params.owner,
                        field: field,
                        isEnabled: true
                    })
                }))
            }
            params.uiWrapper.findAllSh();

            var oValueHelpDialog = new ValueHelpDialog({
                title: params.title ? params.title : bundle.getText(params.title_i18n),
                supportMultiselect: params.isRange,

                // tokens & keys http://plnkr.co/edit/laz2poQbfFQmxdg37f9d?p=previewsetRangeKeyFields
                // supportRanges: params.isRange,

                key: keyField.NAME,
                descriptionKey: keyField.NAME,

                ok: function (oControlEvent) {
                    var aTokens = oControlEvent.getParameter("tokens");

                    if (control instanceof MultiInput) {
                        var prevTokens = control.getTokens();
                        for (var t = 0; t < aTokens.length; t++) {
                            var token = aTokens[t];

                            var value = token.getCustomData()[0].getValue()[keyField.NAME];
                            token = new Token({text: "=" + value}).data("range", {
                                keyField: params.parentKey,
                                exclude: false,
                                operation: 'EQ',
                                value1: value
                            });

                            // Just one
                            // if (control.getMaxTokens() === 1)
                            //     prevTokens = [];

                            prevTokens.push(token);
                        }
                        // With add data
                        control.setTokens(prevTokens);
                    } else { // if (control instanceof Input)
                        var obj = aTokens[0].getCustomData()[0].getValue();
                        control.setValue(obj[keyField.NAME]);
                    }

                    // Input valued iwas changed
                    control.fireLiveChange({});

                    oValueHelpDialog.close();
                },

                cancel: function (oControlEvent) {
                    oValueHelpDialog.close();
                },

                afterClose: function () {
                    oValueHelpDialog.destroy();

                    // Callback
                    if (oEvent.onAfterClose)
                        oEvent.onAfterClose();
                }
            });

            // Prepare main table
            var oTable = oValueHelpDialog.getTable();
            oTable.setThreshold(1000);
            oTable.setEnableBusyIndicator(true);
            oTable.setModel(new JSONModel(colModel), "columns");

            oTable.setModel(params.owner.getModel());
            oTable.bindRows("/" + params.entityName);

            // oValueHelpDialog.setRangeKeyFields([
            //     {
            //         key: keyField.NAME,
            //         label: keyField.LABEL
            //     }
            // ]);
            // if (control instanceof MultiInput)
            //     oValueHelpDialog.setTokens(control.getTokens());

            var oFilterBar = new FilterBar({
                advancedMode: true,

                filterBarExpanded: false,

                // filterItems: [new FilterItem({
                //     name: "s1",
                //     control: new MultiInput({value: ""})
                // })],

                filterGroupItems: filterGroupItems,

                search: function (oEvt) {
                    var oBinding = oValueHelpDialog.getTable().getBinding("rows");
                    var oParams = oEvt.getParameter("selectionSet");
                    var filter = [];

                    for (var p = 0; p < oParams.length; p++) {
                        var ranges = params.uiWrapper._getTokensAsRanges(oParams[p].getItems()[0], false);

                        for (var r = 0; r < ranges.length; r++) {
                            var range = ranges[r];
                            filter.push(new Filter(range.FIELD, range.OPTION, range.LOW, range.HIGH));
                        }
                    }

                    // All as AND
                    oBinding.filter(filter);
                }
            });
            oValueHelpDialog.setFilterBar(oFilterBar);

            // Set style and show
            oValueHelpDialog.addStyleClass(params.owner.getOwnerComponent().getContentDensityClass());
            oValueHelpDialog.open();
        }
    };


    return _this;

});