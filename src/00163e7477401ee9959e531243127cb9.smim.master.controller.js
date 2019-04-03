/*global history */
sap.ui.define([
    "./BaseController",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Filter",
    "sap/ui/model/Sorter",
    "sap/ui/model/FilterOperator",
    "sap/m/GroupHeaderListItem",
    "sap/ui/Device",
    "sap/ui/core/Fragment",
    "../model/formatter"
], function (BaseController, JSONModel, Filter, Sorter, FilterOperator, GroupHeaderListItem, Device, Fragment, formatter) {
    "use strict";

    return BaseController.extend("com.modekz.aqo.controller.Master", {

        formatter: formatter,

        /* =========================================================== */
        /* lifecycle methods                                           */
        /* =========================================================== */

        // Called when the master list controller is instantiated. It sets up the event handling for the master/detail communication and other lifecycle tasks.
        onInit: function () {
            // Control state model
            var oList = this.byId("list"),

                // Put down master list's original value for busy indicator delay,
                // so it can be restored later on. Busy handling on the master list is
                // taken care of by the master list itself.
                iOriginalBusyDelay = oList.getBusyIndicatorDelay();

            this._oList = oList;

            // keeps the filter and search state
            this._oListFilterState = {
                aFilter: [],
                aSearch: []
            };

            // Model for Master view
            var oViewModel = new JSONModel({
                isFilterBarVisible: false,
                filterBarLabel: "",
                delay: 0,
                title: this.getResourceBundle().getText("masterTitleCount", [0]),
                noDataText: this.getResourceBundle().getText("masterListNoDataText")
            });
            this.setModel(oViewModel, "masterView");

            // Make sure, busy indication is showing immediately so there is no
            // break after the busy indication for loading the view's meta data is
            // ended (see promise 'oWhenMetadataIsLoaded' in AppController)
            oList.attachEventOnce("updateFinished", function () {
                // Restore original busy indicator delay for the list
                oViewModel.setProperty("/delay", iOriginalBusyDelay);
            });

            this.getView().addEventDelegate({
                onBeforeFirstShow: function () {
                    this.getOwnerComponent().oListSelector.setBoundMasterList(oList);
                }.bind(this)
            });

            // Set the layout property of the FCL control to 'OneColumn'
            this.getRouter().getRoute("master").attachPatternMatched(function () {
                this.getModel("appView").setProperty("/layout", "OneColumn");
            }, this);

            // Event handler for the bypassed event, which is fired when no routing pattern matched.
            // If there was an object selected in the master list, that selection is removed.
            this.getRouter().attachBypassed(function () {
                this._oList.removeSelections(true);
            }, this);
        },

        /* =========================================================== */
        /* event handlers                                              */
        /* =========================================================== */

        // After list data is available, this handler method updates the master list counter
        onUpdateFinished: function (oEvent) {
            // update the master list object counter after new data is loaded
            var iTotalItems = oEvent.getParameter("total"); // iTotalItems the total number of items in the list

            // only update the counter if the length is final
            if (this._oList.getBinding("items").isLengthFinal()) {
                // Sets the item count on the master list header
                var sTitle = this.getResourceBundle().getText("masterTitleCount", [iTotalItems]);
                this.getModel("masterView").setProperty("/title", sTitle);
            }
        },

        /**
         * Event handler for the master search field. Applies current
         * filter value and triggers a new search. If the search field's
         * 'refresh' button has been pressed, no new search is triggered
         * and the list binding is refresh instead.
         */
        onSearch: function (oEvent) {
            if (oEvent.getParameters().refreshButtonPressed) {
                // Search field's 'refresh' button has been pressed.
                // This is visible if you select any master list item.
                // In this case no new search is triggered, we only refresh the list binding.
                this.onRefresh();
                return;
            }

            var sQuery = oEvent.getParameter("query");
            if (sQuery) {
                this._oListFilterState.aSearch = [
                    new Filter({
                        filters: [
                            new Filter("PACKAGE_ID", FilterOperator.Contains, sQuery),
                            new Filter("OPTION_ID", FilterOperator.Contains, sQuery),
                            new Filter("DESCRIPTION", FilterOperator.Contains, sQuery),
                            new Filter("CREATED_UNAME", FilterOperator.Contains, sQuery),
                            new Filter("CREATED_NAME_TEXT", FilterOperator.Contains, sQuery),
                            new Filter("MAINPROGRAM", FilterOperator.Contains, sQuery)
                        ],
                        and: false
                    })
                ];
            } else {
                this._oListFilterState.aSearch = [];
            }
            this._applyFilterSearch();
        },

        // Event handler for refresh event. Keeps filter, sort  and group settings and refreshes the list binding.
        onRefresh: function () {
            this._oList.getBinding("items").refresh();
        },

        // Event handler for the filter, sort and group buttons to open the ViewSettingsDialog.
        onOpenViewSettings: function (oEvent) {
            // Detect active tab
            var sDialogTab = "filter";
            if (oEvent.getSource() instanceof sap.m.Button) {
                var sButtonId = oEvent.getSource().sId;
                if (sButtonId.match("sort")) {
                    sDialogTab = "sort";
                } else if (sButtonId.match("group")) {
                    sDialogTab = "group";
                }
            }

            // load asynchronous XML fragment (dialog)
            if (!this.byId("viewSettingsDialog")) {
                Fragment.load({
                    id: this.getView().getId(),
                    name: "com.modekz.aqo.view.ViewSettingsDialog",
                    controller: this
                }).then(function (oDialog) {
                    // connect dialog to the root view of this component (models, lifecycle)
                    this.getView().addDependent(oDialog);
                    oDialog.addStyleClass(this.getOwnerComponent().getContentDensityClass());
                    oDialog.open(sDialogTab);
                }.bind(this));
            } else {
                this.byId("viewSettingsDialog").open(sDialogTab);
            }
        },

        /**
         * Event handler called when ViewSettingsDialog has been confirmed, i.e.
         * has been closed with 'OK'. In the case, the currently chosen filters, sorters or groupers
         * are applied to the master list, which can also mean that they
         * are removed from the master list, in case they are
         * removed in the ViewSettingsDialog.
         */
        onConfirmViewSettingsDialog: function (oEvent) {
            var aFilterItems = oEvent.getParameters().filterItems,
                aFilters = [],
                aCaptions = [];

            // update filter state:
            // combine the filter array and the filter string
            aFilterItems.forEach(function (oItem) {
                switch (oItem.getKey()) {
                    case "LT30" :
                        aFilters.push(new Filter("CREATED_DATE", FilterOperator.GE,
                            formatter.toSapDate(formatter.addDays(new Date(), -30))));
                        break;
                    case "LT90" :
                        aFilters.push(new Filter("CREATED_DATE", FilterOperator.GE,
                            formatter.toSapDate(formatter.addDays(new Date(), -90))));
                        break;
                    default :
                        break;
                }
                aCaptions.push(oItem.getText());
            });

            this._oListFilterState.aFilter = aFilters;

            this._updateFilterBar(aCaptions.join(", "));
            this._applyFilterSearch();
            this._applySortGroup(oEvent);
        },

        // Internal helper method that sets the filter bar visibility property and the label's caption to be shown
        _updateFilterBar: function (sFilterBarText) { // the selected filter value
            var oViewModel = this.getModel("masterView");
            oViewModel.setProperty("/isFilterBarVisible", (this._oListFilterState.aFilter.length > 0));
            oViewModel.setProperty("/filterBarLabel", this.getResourceBundle().getText("masterFilterBarText", [sFilterBarText]));
        },

        // Internal helper method to apply both filter and search state together on the list binding
        _applyFilterSearch: function () {
            var aFilters = this._oListFilterState.aSearch.concat(this._oListFilterState.aFilter),
                oViewModel = this.getModel("masterView");
            this._oList.getBinding("items").filter(aFilters, "Application");

            // changes the 'noDataText' of the list in case there are no filter results
            if (aFilters.length !== 0) {
                oViewModel.setProperty("/noDataText", this.getResourceBundle().getText("masterListNoDataWithFilterOrSearchText"));
            } else if (this._oListFilterState.aSearch.length > 0) {
                // only reset the no data text to default when no new search was triggered
                oViewModel.setProperty("/noDataText", this.getResourceBundle().getText("masterListNoDataText"));
            }
        },

        // Apply the chosen sorter and grouper to the master list
        _applySortGroup: function (oEvent) {
            var mParams = oEvent.getParameters(),
                bDescending,
                aSorters = [];

            // apply sorter to binding
            // (grouping comes before sorting)
            if (mParams.groupItem) {
                var groupBy = mParams.groupItem.getKey();
                bDescending = mParams.groupDescending;

                aSorters.push(new Sorter(groupBy, bDescending, function (oContext) {
                    var v = oContext.getProperty(groupBy);
                    return {key: v, text: v};
                }))
            }
            var sortBy = mParams.sortItem.getKey();
            bDescending = mParams.sortDescending;
            aSorters.push(new Sorter(sortBy, bDescending));
            this._oList.getBinding("items").sort(aSorters);
        },

        // Event handler for the list selection event
        onSelectionChange: function (oEvent) {
            var oList = oEvent.getSource(),
                bSelected = oEvent.getParameter("selected");

            // skip navigation when deselecting an item in multi selection mode
            if (!(oList.getMode() === "MultiSelect" && !bSelected)) {
                // get the list item, either from the listItem parameter or from the event's source itself (will depend on the device-dependent mode).
                var oItem = oEvent.getParameter("listItem") || oEvent.getSource();
                var option = oItem.getBindingContext().getObject();
                this._showDetail(option);
            }
        },

        /**
         * Used to create GroupHeaders with non-capitalized caption.
         * These headers are inserted into the master list to
         * group the master list's items.
         * @param {Object} oGroup group whose text is to be displayed
         * @returns group header with non-capitalized caption.
         */
        createGroupHeader: function (oGroup) {
            return new GroupHeaderListItem({
                title: oGroup.key,
                upperCase: false
            });
        },

        /* =========================================================== */
        /* begin: internal methods                                     */
        /* =========================================================== */

        //Shows the selected item on the detail page. On phones a additional history entry is created
        _showDetail: function (oOption) {
            var _this = this;

            // set the layout property of FCL control to show two columns
            var appModel = this.getModel("appView");
            appModel.setProperty("/layout", "TwoColumnsMidExpanded");

            // Just leave
            if (appModel.getProperty("/is_changed")) {
                var prevOpt = appModel.getProperty("/curOption");
                this.showMessage({
                    KIND: 'E',
                    INFO_TEXT: _this.getResourceBundle().getText('save_prev_option', [
                        prevOpt.packageId,
                        prevOpt.optionId
                    ])
                });
                return;
            }

            // Pass params additionally via model
            var params = {
                packageId: oOption.PACKAGE_ID,
                optionId: oOption.OPTION_ID
            };
            appModel.setProperty("/curOption", params);

            this.getRouter().navTo("object", params, !Device.system.phone);
        },

        // Create new option
        onOptionCreate: function () {
            var _this = this;

            // Async load
            sap.ui.require(["com/modekz/aqo/controller/frag/OptionCreateDialog"], function (OptionCreateDialog) {
                var dialogController = new OptionCreateDialog({
                    owner: _this,

                    optionOkPressed: function (newObject) {
                        _this._showDetail(newObject)
                    }
                });
                dialogController.open();
            });
        }
    });
});