$.sap.require("sap.m.MessageBox");

// For SAP callbacks
var callBack = [];

// For SAP
var dialogStack = [];

String.prototype.formatUnicorn = String.prototype.formatUnicorn ||
    function () {
        "use strict";
        var str = this.toString();
        if (arguments.length) {
            var t = typeof arguments[0];
            var args = ("string" === t || "number" === t) ?
                Array.prototype.slice.call(arguments)
                : arguments[0];

            for (var key in args)
                str = str.replace(new RegExp("\\{" + key + "\\}", "gi"), args[key]);
        }

        return str;
    };

function showDialog(dialog) {
    dialogStack.push(dialog);
    dialog.open();
}

function closeDialog(dialog) {
    if (dialogStack.length == 1 && dialogStack[0].sId == "start_dialog")
        return;

    if (dialog)
        removeDialog(dialog);
    else
        dialog = dialogStack.pop();

    if (dialog.sId == "f4_select_dialog")
        dialog._dialog.close();
    else
        dialog.close();
}

function removeDialog(dialog) {
    var index = dialogStack.indexOf(dialog);

    if (index > -1)
        dialogStack.splice(index, 1);
}

function is_sap() {
    return window.location.href.toLowerCase().lastIndexOf("sap", 0) === 0;
}

// Create hidden form and submit with sapevent
function call_sap(action, params) {
    var form = document.createElement("form");
    form.setAttribute("method", "post"); //method ||
    form.setAttribute("action", "sapevent:" + action);

    if (params) {
        // Call back
        if (params.onBack) {
            params.guid = (((1 + Math.random()) * 0x10000000) | 0).toString(16);
            callBack[params.guid] = params.onBack
        }

        for (var key in params)
            if (typeof params[key] !== "function") {
                var hiddenField = document.createElement("input");
                hiddenField.setAttribute("type", "hidden");
                hiddenField.setAttribute("name", key);
                hiddenField.setAttribute("value", params[key]);
                form.appendChild(hiddenField);
            }
    }

    document.body.appendChild(form);
    form.submit();
}

function call_back(guid, data1, data2, data3) {
    var fm = callBack[guid];
    fm(data1, data2, data3);
    delete callBack[guid];
}

function printPath(path){
    console.log(_core.byId("main_dialog").getModel().getProperty(path))
}