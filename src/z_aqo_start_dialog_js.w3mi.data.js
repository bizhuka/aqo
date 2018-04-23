function getStartInput(name) {
    return sap.ui.getCore().byId("ed" + name)
}

function setFavoriteIcon(fav) {
    sap.ui.getCore().byId("bt_favorite").setIcon(fav ? "sap-icon://favorite" : "sap-icon://add-favorite")
}

function getOptions(httpType, scripts) {
    var startInit = {
        model: new sap.ui.model.json.JSONModel(),
        i18n: new sap.ui.model.json.JSONModel()
    };

    // Text translations
    sap.ui.getCore().setModel(startInit.i18n, "i18n");

    if (httpType !== HttpType.SAP) {
        startInit.model.loadData('json/data.json');
        startInit.i18n.loadData('z_aqo_i18n_en_json.w3mi.data.json');
        return startInit;
    }

    call_sap("GET_OPTIONS", {
        smw0: JSON.stringify({
            DATA: scripts
        }),

        onBack: function (model, i18n, params) {
            // Models
            startInit.model.setData(model);
            startInit.i18n.setData(i18n);

            // Input
            startInit.obj = params.OBJECT;
            startInit.sub = params.SUBOBJECT;

            // Fragments
            for (var i = 0; i < scripts.length; i++) {
                var xml = params[scripts[i].ID.toUpperCase()];
                $("#" + scripts[i].ID).text(xml)
            }
            getStartDialog(startInit).open();
        }
    });
}

function getStartDialog(startInit) {
    var startDialog = sap.ui.getCore().byId("start_dialog");

    if (!startDialog) {
        startDialog = sap.ui.xmlfragment({
            // Dialog code
            fragmentContent: $("#id_start_dialog").text()
        }, {// Controller

            // Alternative for F4
            suggestionItemSelected: function (oEvent) {
                var item = oEvent.getParameter("selectedItem");

                // Change other Input
                var sId = oEvent.getParameter("id") === "edOBJECT" ? "SUBOBJECT" : "OBJECT";
                getStartInput(sId).setValue(item.getAdditionalText());

                // Change icon
                setFavoriteIcon(item.getIcon() == "true");
            },

            valueHelpRequest: function () {
                valueF4Object(startInit.model);
            },

            // button 1
            show_option: function () {
                if (getHttpType() === HttpType.SAP)
                    call_sap("SHOW_OPTION", {
                        object: getStartInput("OBJECT").getValue(),
                        subobject: getStartInput("SUBOBJECT").getValue(),

                        onBack: showMainDialog
                    });
                else
                    $.getJSON('json/opt.json', showMainDialog);
            },

            // button 2
            toggle_old_ui: function () {
                call_sap("CALL_OLD_UI", {
                    object: getStartInput("OBJECT").getValue(),
                    subobject: getStartInput("SUBOBJECT").getValue()
                });

                if (getHttpType() !== HttpType.SAP)
                    sap.m.MessageToast.show("Switch to standard SAP UI");
            },

            // button 3
            set_favorite: function () {
                var object = getStartInput("OBJECT").getValue();
                var subobject = getStartInput("SUBOBJECT").getValue();

                var options = startInit.model.getProperty("/DATA");
                var option = null;
                for (var i = 0; i < options.length; i++)
                    if (options[i].OBJECT === object && options[i].SUBOBJECT === subobject) {
                        option = options[i];
                        break;
                    }

                if (!option) {
                    var mes = sap.ui.getCore().getModel("i18n").getProperty("/NO_OPTION_EXIST");
                    sap.m.MessageToast.show(mes, {
                        duration: 2000
                    });
                    return;
                }

                call_sap("SET_FAVORITE", {
                    object: object,
                    subobject: subobject,
                    favorite: !option.FAV, // Invert

                    onBack: function (ok) {
                        if (ok != "true")
                            return;

                        // Invert in model
                        option.FAV = !option.FAV;
                        setFavoriteIcon(option.FAV)
                    }
                });
                if (getHttpType() !== HttpType.SAP)
                    sap.m.MessageToast.show("Change favorites");
            },

            // button 4
            close_app: function () {
                call_sap("DO_CLOSE");
                if (getHttpType() !== HttpType.SAP)
                    sap.m.MessageToast.show("Close the transaction");
            }
        });

        // Set model & initial values
        startDialog.setModel(startInit.model);
        if (startInit.obj && startInit.sub) {
            getStartInput("OBJECT").setValue(startInit.obj);
            getStartInput("SUBOBJECT").setValue(startInit.sub);
        }
    }

    return startDialog;
}

function valueF4Object(oOptModel) {
    // Create a SelectDialog and display it; bind to the same  model as for the suggested items
    var f4SelectDialog = sap.ui.getCore().byId("f4_dialog");
    if (!f4SelectDialog) {
        f4SelectDialog = sap.ui.xmlfragment({
            // Dialog code
            fragmentContent: $("#id_f4_dialog").text()
        }, {// Controller

            search: function (oEvent) {
                var oFilter = [],
                    value = oEvent.getParameter("value"),
                    itemsBinding = oEvent.getParameter("itemsBinding");

                if (value)
                    oFilter = new sap.ui.model.Filter({
                        filters: [
                            new sap.ui.model.Filter("OBJECT", sap.ui.model.FilterOperator.Contains, value),
                            new sap.ui.model.Filter("SUBOBJECT", sap.ui.model.FilterOperator.Contains, value),
                            new sap.ui.model.Filter("UNAME", sap.ui.model.FilterOperator.Contains, value),
                            new sap.ui.model.Filter("UDATE", sap.ui.model.FilterOperator.StartsWith, value),
                            new sap.ui.model.Filter("UTIME", sap.ui.model.FilterOperator.StartsWith, value)
                        ],
                        and: false
                    });
                itemsBinding.filter(oFilter);
            },

            confirm: function (oEvent) {
                var selectedItem = oEvent.getParameter("selectedItem");
                if (selectedItem) {
                    var oCells = selectedItem.getCells();

                    getStartInput("OBJECT").setValue(oCells[1].getText());
                    getStartInput("SUBOBJECT").setValue(oCells[2].getText());
                    setFavoriteIcon(oCells[0].getText() == "true");
                }
            },

            // Make bold
            isFavorite: function (fav) {
                return fav ? "Bold" : "Standard";
            },

            formatDate: formatDate,

            formatTime: formatTime
        });

        f4SelectDialog.setModel(oOptModel);
    }

    f4SelectDialog.open();

    // Fire search by input fields
    var oDialogDOM = f4SelectDialog.$();
    var oSearchFieldDOM = oDialogDOM.find('.sapMSF');
    var oSFID = oSearchFieldDOM[0].id;
    var oSearchField = sap.ui.getCore().byId(oSFID);
    // What text
    oSearchField.setValue(getStartInput("OBJECT").getValue() || getStartInput("SUBOBJECT").getValue());
    oSearchField.fireSearch();
}