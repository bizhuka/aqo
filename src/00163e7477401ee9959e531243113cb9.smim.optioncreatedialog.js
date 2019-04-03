sap.ui.define([
        'sap/ui/base/Object',
        'com/modekz/aqo/model/uiWrapper',
        'com/modekz/aqo/model/f4Helper',
        'sap/ui/model/json/JSONModel'
    ], function (Object, uiWrapper, f4Helper, JSONModel) {
        "use strict";

        return Object.extend("com.modekz.aqo.controller.frag.OptionCreateDialog", {
            params: null,
            dialog: null,

            constructor: function (params) {
                this.params = params;
                this.dialog = params.owner.createFragment("com.modekz.aqo.view.frag.OptionCreateDialog", this);

                var option = params.option;
                var opt = {
                    IS_NEW_OPTION: !option,
                    PACKAGE_ID: option ? option.PACKAGE_ID : "",
                    OPTION_ID: option ? option.OPTION_ID : "",
                    DESCRIPTION: option ? option.DESCRIPTION : "",
                    PREV_VALUE_CNT: option ? option.PREV_VALUE_CNT : 5
                };
                this.dialog.setModel(new JSONModel(opt), "opt");

                f4Helper.addSearchHelps([{
                    owner: params.owner,

                    control: params.owner.findById('id_package_name'),

                    isRange: false,

                    title_i18n: 'package',

                    entityName: 'SHLP_ZHAQO_PACKAGE',
                    // entityName: 'SHLP_DEVCLASS',

                    uiWrapper: uiWrapper
                }]);
            },

            open: function () {
                this.dialog.open();
            },

            onOptionCreateAfterClose: function () {
                this.dialog.destroy();
            },

            onOptionCreateClose: function () {
                this.dialog.close();
            },

            onOptionCreateConfirm: function () {
                this.dialog.close();

                // Everything is ok
                this.params.optionOkPressed(this.dialog.getModel('opt').getProperty("/"));
            }
        });
    }
);