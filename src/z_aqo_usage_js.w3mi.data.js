/**
 * Created by user on 18.03.2018.
 */

function getLastCallDialog() {
    var lastCallDialog = sap.ui.getCore().byId("last_call_dialog");

    if (!lastCallDialog)
        lastCallDialog = sap.ui.xmlfragment({
            // Dialog code
            fragmentContent: $("#id_last_call_dialog").text()
        }, {// Controller

            // №1 button
            onShowLastCall: function () {
                lastCallDialog.close();

                call_sap("NAVIGATE_TO", {
                    "INCLUDE": sap.ui.getCore().byId("edInclude").getValue(),
                    "LINE": sap.ui.getCore().byId("edIncludeLine").getValue()
                });
            },

            // №2 button
            onShowUsage: function () {
                lastCallDialog.close();

                if (getHttpType() === HttpType.SAP)
                    call_sap("DEEP_SCAN", {
                        onBack: show_usage
                    });
                else
                    $.getJSON("json/usage.json", show_usage);
            },

            // №3 button
            onGoBack: function () {
                lastCallDialog.close();
            }
        });

    lastCallDialog.setModel(sap.ui.getCore().byId("main_dialog").getModel());
    return lastCallDialog;
}

function show_usage(usages) {
    var f4Usage = sap.ui.getCore().byId("usage_dialog");

    if (!f4Usage)
        f4Usage = sap.ui.xmlfragment({
            // Dialog code
            fragmentContent: $("#id_usage_dialog").text()
        }, {// Controller
            handleSearch: function (oEvent) {
                var oFilter = [],
                    value = oEvent.getParameter("value"),
                    itemsBinding = oEvent.getParameter("itemsBinding");

                if (value)
                    oFilter = new sap.ui.model.Filter({
                        filters: [
                            new sap.ui.model.Filter("INCLUDE", sap.ui.model.FilterOperator.Contains, value),
                            new sap.ui.model.Filter("UNAME", sap.ui.model.FilterOperator.Contains, value)
                        ],
                        and: false
                    });
                itemsBinding.filter(oFilter);
            },

            handleConfirm: function (oEvent) {
                var cells = oEvent.getParameter("selectedItem").getCells();
                call_sap("NAVIGATE_TO", {
                    "INCLUDE": cells[1].getText(),
                    "LINE": cells[0].getText()
                });
            },

            isFound: function (found) {
                return found ? "Bold" : "Standard";
            },

            formatDate: formatDate
        });

    var oModel = new sap.ui.model.json.JSONModel();
    oModel.setData(usages);
    f4Usage.setModel(oModel);

    f4Usage.open();
}