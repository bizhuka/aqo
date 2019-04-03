sap.ui.define([
        'sap/ui/base/Object',
        'sap/ui/core/ValueState'
    ], function (Object, ValueState) {
        "use strict";

        return Object.extend("com.modekz.aqo.controller.frag.AddNewFieldDialog", {
            owner: null,
            dialog: null,

            constructor: function (owner) {
                this.owner = owner;
                this.dialog = owner.createFragment("com.modekz.aqo.view.frag.AddNewFieldDialog", this);
            },

            open: function () {
                this.dialog.open();
            },

            onAddNewFieldAfterClose: function () {
                this.dialog.destroy();
            },

            onAddNewFieldClose: function () {
                this.dialog.close();
            },

            onAddNewFieldConfirm: function () {
                var _this = this;
                _this.dialog.close();

                // Current rows
                var fldModel = _this.owner.getModel('fld');
                var rows = fldModel.getProperty("/field_rows");

                var sendField = {
                    name: _this.owner.findById('id_name').getValue().toUpperCase(),
                    rollname: _this.owner.findById('id_rollname').getValue().toUpperCase(),

                    onBack: function (data) {
                        data = data.DATA;

                        if (data.KIND && data.INFO_TEXT) {
                            _this.owner.showMessage(data);
                            return
                        }

                        // New field
                        _this.owner.on_set_changed();

                        // Add new item to the end
                        rows.push(data);
                        fldModel.setProperty('/field_rows', rows);
                    }
                };

                // Already exist?
                var row = $.grep(rows, function (item) {
                    return item.NAME.toUpperCase() === sendField.name;
                })[0];

                if (row) {
                    _this.owner.showMessage({
                        i18n: 'field_exist',
                        i18n_param: [sendField.name],
                        KIND: 'E'
                    });
                    return;
                }

                // Send request
                postAction("SAP_GET_FIELD_DESC", sendField);
            },

            on_check_inputs: function () {
                // Input field
                var edId = this.owner.findById('id_name');
                var edType = this.owner.findById('id_rollname');

                edId.setValueState(/^[a-zA-Z_][_a-zA-Z0-9]*$/.test(edId.getValue()) ? ValueState.None : ValueState.Error);
                edType.setValueState(/^[a-zA-Z_][\-_a-zA-Z0-9]*$/.test(edType.getValue()) ? ValueState.None : ValueState.Error);

                this.owner.findById('id_bt_new_field').setEnabled(
                    edId.getValueState() === ValueState.None &&
                    edType.getValueState() === ValueState.None)
            }
        });
    }
);