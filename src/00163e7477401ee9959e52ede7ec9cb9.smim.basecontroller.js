/*global history */
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/core/routing/History",
    "sap/m/MessageToast"
], function (Controller, History, MessageToast) {
    "use strict";

    return Controller.extend("com.modekz.aqo.controller.BaseController", {
        /**
         * Convenience method for accessing the router in every controller of the application.
         * @public
         * @returns {sap.ui.core.routing.Router} the router for this component
         */
        getRouter: function () {
            return this.getOwnerComponent().getRouter();
        },

        /**
         * Convenience method for getting the view model by name in every controller of the application.
         * @public
         * @param {string} sName the model name
         * @returns {sap.ui.model.Model} the model instance
         */
        getModel: function (sName) {
            return this.getView().getModel(sName);
        },

        /**
         * Convenience method for setting the view model in every controller of the application.
         * @public
         * @param {sap.ui.model.Model} oModel the model instance
         * @param {string} sName the model name
         * @returns {sap.ui.mvc.View} the view instance
         */
        setModel: function (oModel, sName) {
            return this.getView().setModel(oModel, sName);
        },

        /**
         * Convenience method for getting the resource bundle.
         * @public
         * @returns {sap.ui.model.resource.ResourceModel} the resourceModel of the component
         */
        getResourceBundle: function () {
            return this.getOwnerComponent().getModel("i18n").getResourceBundle();
        },

        /**
         * Event handler for navigating back.
         * It there is a history entry we go one step back in the browser history
         * If not, it will replace the current entry of the browser history with the master route.
         * @public
         */
        onNavBack: function () {
            var sPreviousHash = History.getInstance().getPreviousHash();

            if (sPreviousHash !== undefined) {
                // eslint-disable-next-line sap-no-history-manipulation
                history.go(-1);
            } else {
                this.getRouter().navTo("master", {}, true);
            }
        },

        createFragment: function (fragment, controller) {
            var result = sap.ui.xmlfragment(fragment, controller ? controller : this);

            // For dialogs
            if (result.addStyleClass) {
                result.addStyleClass(this.getOwnerComponent().getContentDensityClass());
                this.getView().addDependent(result);
            }

            return result;
        },

        findById: function (id) {
            return sap.ui.getCore().byId(id);
        },

        showMessage: function (message) {
            if (message.i18n)
                message.INFO_TEXT = this.getResourceBundle().getText(message.i18n, message.i18n_param);

            MessageToast.show(message.INFO_TEXT, {
                duration: 3500
            });

            // Show as read message
            if (message.KIND === 'E')
                try {
                    $('#content').parent().find('.sapMMessageToast').css('background', '#cc1919');
                } finally {
                    console.error(message.INFO_TEXT);
                }
        }
    });

});