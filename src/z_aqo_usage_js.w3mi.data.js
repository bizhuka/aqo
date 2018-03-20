/**
 * Created by user on 18.03.2018.
 */

function getLastCallDialog(){
    var lastCallDialog = sap.ui.getCore().byId("f4_last_call");
    if (!lastCallDialog)
        lastCallDialog = new sap.m.Dialog("f4_last_call", {
            title: "{i18n>/LAST_CALL}",
            model: true,

            content: [
                new sap.ui.layout.form.SimpleForm({
                    content: [
                        new sap.m.Label({text: "{i18n>/PROG_NAME}"}),
                        new sap.m.Input({ value: "{/DATA/LAST_CALL/MAINPROGRAM}", enabled: false}),

                        new sap.m.Label({text: "{i18n>/INCLUDE}"}),
                        new sap.m.Input("edInclude", {value: "{/DATA/LAST_CALL/INCLUDE}", enabled: false }),

                        new sap.m.Label({text: "{i18n>/LINE_NUMBER}"}),
                        new sap.m.Input("edIncludeLine", { value: "{/DATA/LAST_CALL/LINE}", enabled: false }),

                        new sap.m.Label({text: "{i18n>/TYPE_BLOCK}"}),
                        new sap.m.Input({ value: "{/DATA/LAST_CALL/BLOCKTYPE}", enabled: false }),

                        new sap.m.Label({text: "{i18n>/NAME_BLOCK}"}),
                        new sap.m.Input({ value: "{/DATA/LAST_CALL/BLOCKNAME}", enabled: false })
                    ]
                })
            ],
            buttons: [
                new sap.m.Button({
                    icon: "sap-icon://sys-find",
                    press: function () {
                        closeDialog(lastCallDialog);
                        call_sap("NAVIGATE_TO", {
                            "INCLUDE": sap.ui.getCore().byId("edInclude").getValue(),
                            "LINE": sap.ui.getCore().byId("edIncludeLine").getValue()
                        });
                    }
                }),
                new sap.m.Button({
                    icon: "sap-icon://sys-find-next",
                    press: function () {
                        closeDialog(lastCallDialog);

                        if (is_sap())
                            call_sap("DEEP_SCAN", {
                                onBack: function (data) {
                                    show_deep_scan(data);
                                }
                            });
                        else
                            $.getJSON('usage.json', function (data) {
                                show_deep_scan(data);
                            });
                    }
                }),
                new sap.m.Button({
                    icon: "sap-icon://undo",
                    press: function () {
                        closeDialog(lastCallDialog)
                    }
                })
            ],

            afterClose: function () {
                removeDialog(this);
            }
        }).addStyleClass("sapUiSizeCompact");
    lastCallDialog.setModel(sap.ui.getCore().byId("main_dialog").getModel());
    return lastCallDialog;
}

function show_deep_scan(it_usage) {
    var f4Usage = sap.ui.getCore().byId("f4_usage");
    if (!f4Usage)
        f4Usage = new sap.m.TableSelectDialog("f4_usage", {
            title: "{i18n>/SEL_USAGE}",

            columns: [
                new sap.m.Column({header: new sap.m.Label({text: "{i18n>/LINE}"}), visible: false}),
                new sap.m.Column({header: new sap.m.Label({text: "{i18n>/PROGRAM}"})}),
                new sap.m.Column({header: new sap.m.Label({text: "{i18n>/METH_NAME}"})}),
                new sap.m.Column({header: new sap.m.Label({text: "{i18n>/USER}"})}),
                new sap.m.Column({header: new sap.m.Label({text: "{i18n>/DATE}"})}),
                new sap.m.Column({header: new sap.m.Label({text: "{i18n>/TIME}"})})
            ],

            search: function (oEvent) {
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

            afterClose: function () {
                removeDialog(this);
            },

            confirm: function (oEvent) {
                var cells = oEvent.getParameter("selectedItem").getCells();
                call_sap("NAVIGATE_TO", {
                    "INCLUDE": cells[1].getText(),
                    "LINE": cells[0].getText()
                });
            }
        }).addStyleClass("sapUiSizeCompact");

    var oModel = new sap.ui.model.json.JSONModel();
    oModel.setData(it_usage.DATA);
    f4Usage.setModel(oModel);

    // Make bold
    function isFound() {
        return {
            path: 'FOUND',
            formatter: function (found) {
                return found ? "Bold" : "Standard";
            }
        }
    }

    f4Usage.bindAggregation("items", "/", new sap.m.ColumnListItem({
        cells: [
            new sap.m.Label({text: "{LINE}", design: isFound()}),
            new sap.m.Label({text: "{INCLUDE}", design: isFound()}),
            new sap.m.Label({text: "{METH}", design: isFound()}),
            new sap.m.Label({text: "{UNAME}", design: isFound()}),
            new sap.m.Label({
                design: isFound(),
                text: {
                    path: 'UDATE',
                    formatter: function (udate) {
                        if (!udate)
                            return;
                        udate = udate.replace('-', '');
                        dateObj = new Date(udate.substring(0, 4), udate.substring(4, 6) - 1, udate.substring(6, 8));
                        return dateObj.toLocaleDateString();
                    }
                }
            }),
            new sap.m.Label({text: "{UZEIT}", design: isFound()})
        ]
    }));

    showDialog(f4Usage);
}
