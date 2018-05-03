// Prepare app
function setApplication(startInit) {
    var app = sap.ui.xmlfragment({
        fragmentContent: $("#id_split_app").text()
    }, getMainController(startInit));

    // Place application to the content
    app.placeAt("content");
}

// Main controller
function getMainController(startInit) {
    // Use recursion for nested table
    function fromJson(data) {
        // Change string to data
        for (var i = 0; i < data.length; i++) {
            if (data[i].VALUE)
                data[i].VALUE = JSON.parse(data[i].VALUE).DATA;

            if (data[i].SUBCOMPS) {
                data[i].SUBCOMPS = JSON.parse(data[i].SUBCOMPS).DATA;
                fromJson(data[i].SUBCOMPS)
            }
        }
    }

    function navigate_to(include, line) {
        call_sap("NAVIGATE_TO", {
            "INCLUDE": include,
            "LINE": line
        });

        if (getHttpType() !== HttpType.SAP)
            sap.m.MessageToast.show("Drill down to SE38 " + include);
    }

    function show_usage(usages) {
        startInit.usage.setData(usages);
    }

    // Save to SAP
    function doSave(params) {
        var fld_opt = sap.ui.getCore().getModel("main").getProperty("/DATA/FLD_OPT");

        // Make copy
        fld_opt = JSON.parse(JSON.stringify(fld_opt));

        // To JSON
        for (var i = 0; i < fld_opt.length; i++) {
            fld_opt[i].VALUE = JSON.stringify({
                DATA: fld_opt[i].VALUE
            });

            // recursion ?
            if (fld_opt[i].SUBCOMPS)
                fld_opt[i].SUBCOMPS = JSON.stringify({
                    DATA: fld_opt[i].SUBCOMPS
                });
        }

        // Pass data
        params.option = JSON.stringify({
            DATA: fld_opt
        });

        call_sap("SAVE_OPTION", params);
        if (getHttpType() !== HttpType.SAP)
            sap.m.MessageToast.show("Save option to database");
    }

    // Return controller
    return {
        onSearchOption: function (oEvent) {
            var oFilter = [],
                value = oEvent.getParameter("query");

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

            // Set filtration
            sap.ui.getCore().byId("id_option_list").getBinding("items").filter(oFilter);
        },

        // Select new option
        onOptionClick: function (oEvent) {
            if (startInit.main.getProperty("/is_changed")) {
                this.onSavePress();
                return;
            }

            var selObj = startInit.sel.getProperty(oEvent.getSource().getBindingContext("sel").getPath());
            startInit.sel.setProperty("/current", selObj);
            this.onFavIconPress(true);

            var show_option = this.show_option;
            if (getHttpType() === HttpType.SAP)
                call_sap("SHOW_OPTION", {
                    object: selObj.OBJECT,
                    subobject: selObj.SUBOBJECT,
                    onBack: show_option
                });
            else
                $.getJSON('json/' + selObj.OBJECT + '-' + selObj.SUBOBJECT + '.json', show_option);
        },

        show_option: function (option) {
            // Show details
            var core = sap.ui.getCore();
            core.byId("id_detail_header").setVisible(true);
            core.byId("id_detail_page").setShowFooter(true);
            core.byId("id_main_bar").setExpanded(true);
            core.byId("id_main_bar").setSelectedKey("id_first_tab");

            // Change string to data
            fromJson(option.DATA.FLD_OPT);

            // Set new data
            startInit.setNewData(option);

            // Create second cell dynamically
            var options = startInit.main.getProperty("/DATA/FLD_OPT");
            var oEditTable = sap.ui.getCore().byId("edit_table");
            var items = oEditTable.getItems();

            // Create new cells
            for (var i = 0; i < options.length; i++) {
                var fld_opt = options[i];
                var item = items[i];

                // delete prev cell and add new one
                item.removeCell(1);
                item.addCell(createMultiUI(fld_opt, "VALUE"));
            }
        },

        on_delete_row: function () {
            var oMainTable = sap.ui.getCore().byId("option_table");
            var oModel = oMainTable.getModel("main");
            var oData = oModel.getProperty('/DATA/FLD_OPT');

            var oContexts = oMainTable.getSelectedContexts();
            for (var i = oContexts.length - 1; i >= 0; i--) {
                var curObj = oContexts[i].getObject();
                var index = $.map(oData, function (obj, index) {
                    if (obj === curObj) {
                        return index;
                    }
                });
                oData.splice(index, 1);
            }
            oModel.setProperty('/DATA/FLD_OPT', oData);
            oMainTable.removeSelections(true);
        },

        get_drill_down_icon: function (rollname) {
            return rollname.indexOf('-') !== -1 ? "sap-icon://detail-view" : "sap-icon://status-in-process";
        },

        on_press_drill_down: function (oEvent) {
            var oContext = oEvent.getSource().getBindingContext("main");
            var item = sap.ui.getCore().getModel("main").getProperty(oContext.sPath);

            call_sap("DRILL_DOWN", {
                datatype: item.ROLLNAME
            });
            if (getHttpType() !== HttpType.SAP)
                sap.m.MessageToast.show("Drill down to SE11 " + item.ROLLNAME);
        },

        get_kind_icon: function (kind, is_old) {
            if (is_old)
                return sap.ui.core.IconPool.getIconURI("message-error");

            switch (kind) {
                case "P":
                    return sap.ui.core.IconPool.getIconURI("activity-2");
                case "S":
                    return sap.ui.core.IconPool.getIconURI("multi-select");
                case "T":
                    return sap.ui.core.IconPool.getIconURI("table-view");
            }
        },

        on_press_kind_icon: function (oEvent) {
            var oMain = sap.ui.getCore().getModel("main");
            var oContext = oEvent.getSource().getBindingContext("main");
            var item = oMain.getProperty(oContext.sPath);

            // Only for tables
            if (item.KIND !== "T")
                return;

            var dialog = sap.ui.getCore().byId("field_catalog_dialog");
            if (!dialog)
                dialog = sap.ui.xmlfragment({
                    // Dialog code
                    fragmentContent: $("#id_field_catalog_dialog").text()
                }, {// Controller
                    on_close_click: function () {
                        dialog.close();
                    }
                });
            // Path for fields
            sap.ui.getCore().byId("field_catalog_table").bindRows(oContext.sPath + "/SUBCOMPS");

            dialog.setTitle(item.TEXT);
            dialog.setModel(oMain);
            dialog.open();
        },

        // Show last usage
        on_show_last_call: function () {
            navigate_to(sap.ui.getCore().byId("edInclude").getValue(), sap.ui.getCore().byId("edIncludeLine").getValue());
        },

        on_show_usage: function (oEvent) {
            var cells = sap.ui.getCore().byId(oEvent.getParameter("id")).getParent().getCells();
            navigate_to(cells[1].getText(), cells[2].getText());
        },

        on_tab_select: function (oEvent) {
            // Only for usage tab
            var oTab = oEvent.getParameter("selectedItem");
            if (oTab.sId !== "id_usage_tab")
                return;

            // If something is elected
            var option = startInit.sel.getProperty("/current");
            if (!option || !option.OBJECT || !option.SUBOBJECT)
                return;

            if (getHttpType() === HttpType.SAP)
                call_sap("DEEP_SCAN", {
                    onBack: show_usage
                });
            else
                $.getJSON("json/usage.json", show_usage);
        },

        onGroupSortSelect: function () {
            var sorters = [];

            var sortBy = sap.ui.getCore().getModel("sel").getProperty("/sortBy") || "FAV";
            var arr = sortBy.split("-");
            for (var i = 0; i < arr.length; i++)
                sorters.push(new sap.ui.model.Sorter(arr[i], true))

            // First level
            var groupBy = sap.ui.getCore().getModel("sel").getProperty("/groupBy");
            if (groupBy)
                sorters.unshift(new sap.ui.model.Sorter(groupBy, null, function (oContext) {
                    var v = oContext.getProperty(groupBy);
                    return {key: v, text: v};
                }));

            // And sort
            sap.ui.getCore().byId("id_option_list").getBinding("items").sort(sorters);
        },

        onEmailPress: function () {
            sap.m.URLHelper.triggerEmail(
                "modekz@gmail.com",
                startInit.i18n.getProperty("/BUG_INFO")
            );
        },

        onSwitchPress: function () {
            var currOpt = startInit.sel.getProperty("/current")
            call_sap("CALL_OLD_UI", {
                object: currOpt.OBJECT,
                subobject: currOpt.SUBOBJECT
            });

            if (getHttpType() !== HttpType.SAP)
                sap.m.MessageToast.show("Switch to standard SAP UI");
        },

        onClosePress: closeApp,

        onSavePress: function () {
            var option = startInit.sel.getProperty("/current");
            sap.m.MessageBox.confirm(sap.ui.getCore().getModel("i18n").getProperty("/SAVE_OPTION").formatUnicorn(option.OBJECT, option.SUBOBJECT), {
                onClose: function (oAction) {
                    if (oAction === sap.m.MessageBox.Action.OK)
                        doSave({
                            onBack: function (saved) {
                                // Is saved
                                if (saved)
                                    startInit.setNewData(startInit.main.getData());
                            }
                        })
                }
            });
        },

        onCopyToPress: function () {
            var f4CopyTo = sap.ui.getCore().byId("f4_copy_to_dialog");
            if (!f4CopyTo) {
                f4CopyTo = new sap.m.TableSelectDialog("f4_copy_to_dialog", {
                    title: "{i18n>/COPY_TO}",

                    columns: [
                        new sap.m.Column({header: new sap.m.Label({text: "{i18n>/CLIENT}"})}),
                        new sap.m.Column({header: new sap.m.Label({text: "{i18n>/CLIENT_NAME}"})})
                    ],

                    search: function (oEvent) {
                        var oFilter = [],
                            value = oEvent.getParameter("value"),
                            itemsBinding = oEvent.getParameter("itemsBinding");

                        if (value)
                            oFilter = new sap.ui.model.Filter({
                                filters: [
                                    new sap.ui.model.Filter("MANDT", sap.ui.model.FilterOperator.Contains, value),
                                    new sap.ui.model.Filter("MTEXT", sap.ui.model.FilterOperator.Contains, value)
                                ],
                                and: false
                            });
                        itemsBinding.filter(oFilter);
                    },

                    confirm: function (oEvent) {
                        var client = oEvent.getParameter("selectedItem").getCells()[0].getText();
                        sap.m.MessageBox.confirm(sap.ui.getCore().getModel("i18n").getProperty("/COPY_TO_MANDT").formatUnicorn(client), {
                            onClose: function (oAction) {
                                // Copy to another mandt
                                if (oAction === sap.m.MessageBox.Action.OK)
                                    doSave({
                                        mandt: client
                                    });
                            }
                        });
                    }
                });

                f4CopyTo.bindAggregation("items", "main>/DATA/COPY_TO", new sap.m.ColumnListItem({
                    cells: [
                        new sap.m.Label({text: "{main>MANDT}"}),
                        new sap.m.Label({text: "{main>MTEXT}"})]
                }));
            }

            f4CopyTo.open();
        },

        onTransportPress: function () {
            call_sap("TRANSPORT_OPTION");
            if (getHttpType() !== HttpType.SAP)
                sap.m.MessageToast.show("SAP transport request dialog");
        },

        onDeletePress: function () {
            sap.m.MessageBox.confirm(sap.ui.getCore().getModel("i18n").getProperty("/DELETE_OPTION"), {
                onClose: function (oAction) {
                    if (oAction === sap.m.MessageBox.Action.OK) {
                        call_sap("DELETE_OPTION", {
                            onBack: function (data) {
                                if (data)
                                    closeApp(true);
                            }
                        });
                        if (getHttpType() !== HttpType.SAP)
                            sap.m.MessageToast.show("Delete option from database");
                    }
                }
            });
        },

        formatDate: formatDate,

        datetime: function (udate, utime) {
            return formatDate(udate) + " " + formatTime(utime)
        },

        getFavIcon: function (fav) {
            if (typeof fav === 'undefined')
                return "";
            return sap.ui.core.IconPool.getIconURI(fav ? "favorite" : "unfavorite");
        },

        onFavIconPress: function (notToggle) {
            var option = startInit.sel.getProperty("/current");
            var button = sap.ui.getCore().byId("id_favorite_button");

            // Just init
            if (notToggle === true) {
                button.setPressed(option.FAV);
                return;
            }

            function doToggle() {
                // Invert in model
                startInit.sel.setProperty("/current/FAV", !option.FAV);

                sap.m.MessageToast.show(sap.ui.getCore().getModel("i18n").getProperty("/" +
                    (option.FAV ? "SET_FAVORITE" : "UNSET_FAVORITE")));

                button.setPressed(option.FAV);
            }

            if (getHttpType() !== HttpType.SAP)
                doToggle();
            else
                call_sap("SET_FAVORITE", {
                    object: option.OBJECT,
                    subobject: option.SUBOBJECT,
                    favorite: !option.FAV, // Invert

                    onBack: function (ok) {
                        if (ok != "true")
                            return;

                        doToggle();
                    }
                });
        }
    }
}