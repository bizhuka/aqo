$.sap.require("sap.m.MessageBox");

// For SAP callbacks
var callBack = [];

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

var HttpType = {
    SAP: 1,
    BROWSER: 2,
    GITHUB: 3
};

function getHttpType() {
    var url = window.location.href.toLowerCase();

    if (url.lastIndexOf("sap", 0) === 0)
        return HttpType.SAP;

    if (url.lastIndexOf("https://htmlpreview.github.io") === 0)
        return HttpType.GITHUB;

    return HttpType.BROWSER;
}

function formatDate(date) {
    if (typeof date !== "string")
        return;

    date = date.replace(/-/g, '');
    if (date.length !== 8)
        return;

    var dateObj = new Date(date.substring(0, 4), date.substring(4, 6) - 1, date.substring(6, 8));
    return dateObj.toLocaleDateString();
}

function formatTime(time) {
    if (typeof time !== "string" && time.length !== 6)
        return;
    return time.substring(0, 2) + ":" + time.substring(2, 4) + ":" + time.substring(4, 6);
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

function closeDialog() {
    $("div.sapMDialog").last().each(function (i, diag) {
        // No need
        if (diag.id === "start_dialog") {
            call_sap("DO_CLOSE");
            return;
        }

        var arr = diag.id.split("-");
        var dialog = sap.ui.getCore().byId(arr[0]);

        if (arr.length === 2)
            dialog = dialog["_" + arr[1]];

        dialog.close();
    });
}

function printPath(path) {
    console.log(sap.ui.getCore().byId("main_dialog").getModel().getProperty(path))
}
