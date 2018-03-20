// Option model
var oOptModel = null;

function getStartInput(name) {
    return sap.ui.getCore().byId("ed_" + name)
}

function setFavoriteIcon(fav) {
    sap.ui.getCore().byId("bt_favorite").setIcon(fav ? "sap-icon://favorite" : "sap-icon://add-favorite")
}

// Make bold
function isFavorite() {
    return {
        path: 'FAV',
        formatter: function (fav) {
            return fav ? "Bold" : "Standard";
        }
    }
}

function createObjectName(name, subname) {
    var oInput = new sap.m.Input("ed_" + name, {
        type: sap.m.InputType.Text,
        width: "24em",
        placeholder: '...',

        showSuggestion: true,

        suggestionItems: {
            path: "/DATA",
            template: new sap.ui.core.ListItem(
                {
                    text: "{" + name + "}",
                    additionalText: "{" + subname + "}",
                    icon: "{FAV}"
                })
        },

        suggestionItemSelected: function (oEvent) {
            var item = oEvent.getParameter("selectedItem");

            getStartInput(subname).setValue(item.getAdditionalText());
            setFavoriteIcon(item.getIcon() == "true");
        },

        showValueHelp: true,

        valueHelpRequest: function (oEvent) {
            valueF4Object(oEvent, oOptModel)
        }
    });

    // set model and return
    oInput.setModel(oOptModel);
    return oInput;
}

function valueF4Object(oEvent, oOptModel) {
    // Create a SelectDialog and display it; bind to the same  model as for the suggested items
    var f4SelectDialog = sap.ui.getCore().byId("f4_select_dialog");
    if (!f4SelectDialog) {
        f4SelectDialog = new sap.m.TableSelectDialog("f4_select_dialog", { // SelectDialog
            title: "{i18n>/SEL_OPTION}",

            columns: [
                new sap.m.Column({hAlign: "Begin", header: new sap.m.Label({text: "{i18n>/FAV}"}), visible: false}),
                new sap.m.Column({hAlign: "Begin", header: new sap.m.Label({text: "{i18n>/OBJECT}"})}),
                new sap.m.Column({hAlign: "Begin", header: new sap.m.Label({text: "{i18n>/SUBOBJ}"})}),
                new sap.m.Column({hAlign: "Begin", header: new sap.m.Label({text: "{i18n>/CHANGED}"})}),
                new sap.m.Column({hAlign: "End", header: new sap.m.Label({text: "{i18n>/DATE}"})}),
                new sap.m.Column({hAlign: "End", header: new sap.m.Label({text: "{i18n>/TIME}"})})
            ],

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

            afterClose: function () {
                removeDialog(this);
            },

            confirm: function (oEvent) {
                var selectedItem = oEvent.getParameter("selectedItem");
                if (selectedItem) {
                    var oCells = selectedItem.getCells();

                    getStartInput("OBJECT").setValue(oCells[1].getText());
                    getStartInput("SUBOBJECT").setValue(oCells[2].getText());
                    setFavoriteIcon(oCells[0].getText() == "true");
                }
            }
        });

        f4SelectDialog.setModel(oOptModel);
        f4SelectDialog.bindAggregation("items", "/DATA", new sap.m.ColumnListItem({
            path: "/DATA",
            type: "Active",
            unread: false,

            cells: [
                new sap.m.Label({text: "{FAV}"}),
                new sap.m.Label({text: "{OBJECT}", design: isFavorite()}),
                new sap.m.Label({text: "{SUBOBJECT}", design: isFavorite()}),
                new sap.m.Label({text: "{UNAME}", design: isFavorite()}),
                new sap.m.Label({
                    text: {
                        path: 'UDATE',
                        formatter: function (udate) {
                            dateObj = new Date(udate.substring(0, 4), udate.substring(4, 6) - 1, udate.substring(6, 8));
                            return dateObj.toLocaleDateString();
                        }
                    }, design: isFavorite()
                }),
                new sap.m.Label({
                    text: {
                        path: 'UTIME',
                        formatter: function (utime) {
                            return utime.substring(0, 2) + ":" + utime.substring(2, 4) + ":" + utime.substring(4, 6);
                        }
                    }, design: isFavorite()
                })]
        }));
    }

    showDialog(f4SelectDialog);

    // Fire search by input fields
    var oDialogDOM = f4SelectDialog.$();
    var oSearchFieldDOM = oDialogDOM.find('.sapMSF');
    var oSFID = oSearchFieldDOM[0].id;
    var oSearchField = sap.ui.getCore().byId(oSFID);
    // What text
    oSearchField.setValue(getStartInput("OBJECT").getValue() || getStartInput("SUBOBJECT").getValue());
    oSearchField.fireSearch();
}
function getStartDialog() {
    var startDialog = sap.ui.getCore().byId("start_dialog");

    if (!startDialog) {
        // Cannot load from SAP json file!
        oOptModel = new sap.ui.model.json.JSONModel();
        var i18nModel = new sap.ui.model.json.JSONModel();
        _core.setModel(i18nModel, "i18n");

        if (!is_sap()) {
            oOptModel.loadData('json/data.json');
            i18nModel.loadData('json/i18n_en.json');
        } else
            call_sap("GET_OPTIONS", {
                onBack: function (data, i18n) {
                    oOptModel.setData(data);
                    i18nModel.setData(i18n);
                }
            });

        startDialog = new sap.m.Dialog("start_dialog", {
            title: "{i18n>/SEL_OPTION}",
            model: true,
            contentWidth: "29em",

            content: [
                new sap.m.VBox({
                    items: [
                        new sap.m.Label({text: "{i18n>/OBJECT}"}),
                        createObjectName("OBJECT", "SUBOBJECT"),
                        new sap.m.Label({text: "{i18n>/SUBOBJ}"}),
                        createObjectName("SUBOBJECT", "OBJECT")
                    ]
                })
            ],
            buttons: [
                new sap.m.Button({
                    icon: "sap-icon://accept",
                    press: function () {

                        if (is_sap())
                            call_sap("SHOW_OPTION", {
                                object: getStartInput("OBJECT").getValue(),
                                subobject: getStartInput("SUBOBJECT").getValue(),

                                onBack: function (data) {
                                    sap.ui.getCore().show_option(data);
                                }
                            });
                        else
                            $.getJSON('json/opt.json', function (data) {
                                sap.ui.getCore().show_option(data);
                            });
                    }
                }),

                new sap.m.Button("bt_favorite", {
                    icon: "sap-icon://unfavorite",
                    press: function () {
                        var object = getStartInput("OBJECT").getValue();
                        var subobject = getStartInput("SUBOBJECT").getValue();

                        var options = oOptModel.getProperty("/DATA");
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
                    }
                }),

                new sap.m.Button({
                    icon: "sap-icon://visits",
                    press: function () {
                        call_sap("DO_CLOSE");
                    }
                })
            ],

            afterClose: function () {
                removeDialog(this);
            }
        });
    }
    return startDialog;
}
