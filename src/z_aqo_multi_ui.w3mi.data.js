// ENABLED
function isEnabled(tabEnabled) {
    return {
        parts: [{path: 'main>/DATA/READ_ONLY'}, {path: 'main>/DATA/DEV_MANDT'}, {path: 'main>EDIT'}],

        formatter: function (readOnly, dev, edit) {
            if (typeof tabEnabled !== "undefined")
                return tabEnabled;

            if (readOnly)
                return false;

            return dev || edit;
        }
    }
}

// Create control
function createMultiUI(compObj, valueField, tabEnabled) {
    // Based on js type
    switch (compObj.UI_TYPE) {
        case "memo":
            return new sap.m.TextArea({
                value: "{main>" + valueField + "}",
                width: "100%",
                enabled: isEnabled(tabEnabled),
                valueLiveUpdate: true,
                growing: true,
                growingMaxLines: 5
            });

        case "boolean":
            return new sap.m.CheckBox({
                width: "100%",
                selected: "{main>" + valueField + "}",
                enabled: isEnabled(tabEnabled)
            });

        case "number":
            return new sap.m.Input({
                value: "{main>" + valueField + "}",
                type: sap.m.InputType.Number,
                width: "100%",
                enabled: isEnabled(tabEnabled)
            });

        case "date":
            return new sap.m.DatePicker({
                value: "{main>" + valueField + "}",
                valueFormat: "yyyy-MM-dd",
                displayFormat: "long",
                enabled: isEnabled(tabEnabled)
            });

        case "time":
            return new sap.m.TimePicker({
                width: "100%",
                value: "{main>" + valueField + "}",
                valueFormat: "HH:mm:ss",
                displayFormat: "HH:mm:ss",
                enabled: isEnabled(tabEnabled)
            });

        case "datetime":
            // All by default
            return new sap.m.DateTimePicker({
                value: "{main>" + valueField + "}",
                enabled: isEnabled(tabEnabled)
            });
    }

    // If previous conditions false
    // Show input with F4 search help
    if (compObj.UI_TYPE === "" && compObj.KIND === "P")
        return new sap.ui.commons.ValueHelpField({
            value: "{main>" + valueField + "}",
            width: "100%",
            enabled: isEnabled(tabEnabled),

            valueHelpRequest: function (oEvent) {
                var input = this;
                // Show F4
                call_sap("VALUE_REQUEST", {
                    datatype: compObj.ROLLNAME,

                    onBack: function (data) {
                        input.setValue(data);
                    }
                });
                if (getHttpType() !== HttpType.SAP)
                    sap.m.MessageToast.show("Show SAP f4 dialog");
            }
        });

    // Select-option
    if (compObj.KIND === "S")
        return createRangeControl(compObj, valueField, tabEnabled);

    // For tables show button
    if (compObj.KIND === "T")
        return createTableControl(compObj, valueField, tabEnabled);
}

function createTableControl(compObj, valueField, tabEnabled) {
    var button = new sap.m.Button({
        width: "100%",

        press: function (oEvent) {
            // Current model
            var oMainModel = sap.ui.getCore().getModel("main");

            var oContext = oEvent.getSource().getBindingContext("main");
            var item = oMainModel.getProperty(oContext.sPath);

            // Detect enabled for child table
            var _tbEnabled = isEnabled(tabEnabled).formatter(oMainModel.getProperty('/DATA/READ_ONLY'),
                oMainModel.getProperty('/DATA/DEV_MANDT'),
                compObj.EDIT);

            var oTable = new sap.ui.table.Table({
                // Select mode for deleting rows
                selectionMode: _tbEnabled ? sap.ui.table.SelectionMode.MultiToggle : sap.ui.table.SelectionMode.None,

                title: new sap.m.HBox({
                    items: [
                        // Delete selected rows
                        new sap.m.Button({
                            icon: 'sap-icon://negative',
                            visible: _tbEnabled,

                            press: function () {
                                // Delete selected rows
                                var reverse = [].concat(oTable.getSelectedIndices()).reverse();
                                reverse.forEach(function (index) {
                                    item.VALUE.splice(index, 1);
                                });
                                oMainModel.refresh();
                                oTable.setSelectedIndex(-1);

                                button.countChanged()
                            }
                        }).addStyleClass("sapUiTinyMarginEnd"),

                        // Insert new row
                        new sap.m.Button({
                            icon: 'sap-icon://positive',
                            visible: _tbEnabled,

                            press: function () {
                                var box = new sap.m.VBox({
                                    items: [
                                        new sap.m.Text({
                                            text: sap.ui.getCore().getModel("i18n").getProperty("/ROW_POS").formatUnicorn(1, item.VALUE.length + 1)
                                        }),
                                        new sap.m.StepInput("edPosition", {
                                            value: 1,
                                            min: 1,
                                            max: item.VALUE.length + 1
                                        })
                                    ]
                                });

                                sap.m.MessageBox.show(
                                    box, {
                                        icon: sap.m.MessageBox.Icon.INFORMATION,
                                        title: "{i18n>/NEW_ROW}",
                                        actions: [sap.m.MessageBox.Action.YES, sap.m.MessageBox.Action.NO],
                                        onClose: function (oAction) {
                                            if (oAction !== sap.m.MessageBox.Action.YES)
                                                return;
                                            var pos = sap.ui.getCore().byId("edPosition").getValue();

                                            var new_row = {};
                                            for (var i = 0; i < item.SUBCOMPS.length; i++)
                                                new_row[item.SUBCOMPS[i].NAME] = '';

                                            if (pos === 1)
                                                item.VALUE.unshift(new_row);
                                            else if (pos === item.VALUE.length + 1)
                                                item.VALUE.push(new_row);
                                            else
                                                item.VALUE.splice(pos, 0, new_row);

                                            oMainModel.refresh();
                                            button.countChanged()
                                        }
                                    }
                                );
                            }
                        }).addStyleClass("sapUiTinyMarginEnd")
                    ]
                })
            });

            // var lo_columns = model.getProperty(compPath + "/SUBCOMPS");
            oTable.bindColumns("main>" + oContext.sPath + "/SUBCOMPS", function (index, context) {
                // var subcomp = context.getObject();
                var subcomp = oMainModel.getProperty(context.sPath);

                return new sap.ui.table.Column({
                    label: new sap.m.Label({
                        text: subcomp.TEXT
                    }),

                    template: createMultiUI(subcomp, subcomp.NAME, _tbEnabled)
                });
            });
            oTable.bindRows("main>" + oContext.sPath + "/VALUE");

            var dialog = new sap.m.Dialog({
                title: item.TEXT,
                model: true,

                content: [new sap.m.VBox({
                    items: [oTable]
                })],
                buttons: [new sap.m.Button({
                    icon: "sap-icon://accept",
                    press: function () {
                        dialog.close();
                    }
                })]
            });

            dialog.setModel(oMainModel);
            dialog.open();
        }
    });

    // Cannot bind with count
    button.countChanged = function () {
        var txt = sap.ui.getCore().getModel("i18n").getProperty("/COUNT") + compObj.VALUE.length;
        button.setText(txt);
    };
    button.countChanged();

    return button;
}

function createRangeControl(compObj, valueField, tabEnabled) {
    var theTokenInput = new sap.m.MultiInput({
        width: "100%",
        enableMultiLineMode: true,
        enabled: isEnabled(tabEnabled),

        valueHelpRequest: function (oControlEvent) {
            // Set height to minimum
            theTokenInput.closeMultiLine();

            // Convert to SAP format
            var ranges = [];
            var currTokens = this.getTokens();
            for (var key in currTokens)
                ranges.push(currTokens[key].data("range"))

            // Show F4
            call_sap("RANGE_REQUEST", {
                title: compObj.TEXT,
                datatype: compObj.ROLLNAME,
                ranges: JSON.stringify({
                    DATA: ranges
                }),

                onBack: function (value) {
                    theTokenInput._setNewTokens(value.DATA, true);
                }
            });
            if (getHttpType() !== HttpType.SAP)
                sap.m.MessageToast.show("Show SAP selection dialog");
        },

        tokenUpdate: function (oControlEvent) {
            var currTokens = this.getTokens();

            if (oControlEvent.getParameter("type") === sap.m.Tokenizer.TokenChangeType.Removed) {
                var aRemovedTokens = oControlEvent.getParameter("removedTokens");
                for (var j = 0; j < aRemovedTokens.length; j++) {
                    var sKey = aRemovedTokens[j].getKey();

                    for (var i in currTokens) {
                        if (currTokens[i].getKey() === sKey) {
                            currTokens.splice(i, 1);
                            break;
                        }
                    }
                }
            }

            if (oControlEvent.getParameter("type") === sap.m.Tokenizer.TokenChangeType.Added) {
                var aAddedTokens = oControlEvent.getParameter("addedTokens");
                for (j = 0; j < aAddedTokens.length; j++) {
                    currTokens.push(aAddedTokens[j]);
                }
            }

            // Update model
            this._fireChanged(currTokens);
        }

    });

    // Set new tokens & update model
    var oMainModel = sap.ui.getCore().getModel("main");
    theTokenInput._fireChanged = function (currTokens) {
        // Copy databack
        compObj.VALUE = [];
        for (i = 0; i < currTokens.length; i++)
            compObj.VALUE.push(currTokens[i].data("range"));

        // Fire event (set the same value)
        oMainModel.setProperty('/DATA/DEV_MANDT', oMainModel.getProperty('/DATA/DEV_MANDT'));
    };

    // Own setTokens with model update
    theTokenInput._setNewTokens = function (values, fire) {
        result = [];
        for (i = 0; i < values.length; i++) {
            var range = values[i];
            var sKey = (range.SIGN === "E" ? "!" : "") + range.OPTION + "_" + range.LOW + "_" + range.HIGH;
            result.push(new sap.m.Token({
                key: sKey,
                text: sKey
            }).data("range", range));
        }

        this.setTokens(result);

        if (fire)
            this._fireChanged(result);
    };

    // compObj[valueField]   model.getProperty(valuePath + "/" + valueField)
    // Only for 1-st level
    theTokenInput._setNewTokens(compObj.VALUE, false);
    return theTokenInput;
}