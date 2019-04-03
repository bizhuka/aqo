// Detect mode
var HttpType = {
    SAP: 1,
    SAP_BSP: 2,
    BROWSER: 3,
    GITHUB: 4
};

// Detect where to start
var _httpType = getHttpType();
var _isBusy = false;

// For SAP callbacks
var callBack = {};

function getHttpType() {
    var url = window.location.href.toLowerCase();

    if (url.lastIndexOf("saphtmlp://", 0) === 0)
        return HttpType.SAP;

    if (url.indexOf('sap/bc/bsp/sap/zbsp_aqo') > 0)
        return HttpType.SAP_BSP;

    if (url.lastIndexOf("https://htmlpreview.github.io", 0) === 0)
        return HttpType.GITHUB;

    return HttpType.BROWSER;
}

// Create hidden form and submit with sapevent
function postAction(action, params) {
    // Trouble in SAP
    if (_isBusy) {
        setTimeout(function () {
            postAction(action, params);
        }, getDelayTime(50));
        return;
    }
    _isBusy = true;

    // Always wait for response
    if (!params.guid)
        params.guid = action + " " + (((1 + Math.random()) * 0x10000000) | 0).toString(16);

    // Add to callbacks
    if (params.onBack)
        callBack[params.guid] = params;

    // Send url
    var url = "/" + action;
    switch (_httpType) {
        case HttpType.SAP:
            url = "sapevent:" + action;
            break;

        case HttpType.SAP_BSP:
            url = "../../../zaqo_service?action=" + action;
            break;

        case HttpType.BROWSER:
            url = "http://localhost:3000/" + action;
            break;
    }

    // How will send requests
    var postBody = {};
    if (_httpType === HttpType.SAP) {
        var postForm = document.createElement("form");
        postForm.setAttribute("method", "post");
        postForm.setAttribute("action", url);
        document.body.appendChild(postForm);
    }

    // Send params
    if (params)
        for (var key in params)
            if (params.hasOwnProperty(key)) {
                var sTypeOf = typeof params[key];
                var value = params[key];

                // Or just Array.isArray ?
                if (sTypeOf === "object")
                    value = JSON.stringify({
                        DATA: value
                    });

                // Functions and private fields
                if (key.charAt(0) !== '_' && sTypeOf !== "function")
                    if (_httpType !== HttpType.SAP)
                        postBody[key] = value;
                    else {
                        var hiddenField = document.createElement("input");
                        hiddenField.setAttribute("type", "hidden");
                        hiddenField.setAttribute("name", key);
                        hiddenField.setAttribute("value", value);
                        postForm.appendChild(hiddenField);
                    }
            }

    if (_httpType === HttpType.SAP)
        postForm.submit();
    else
        $.ajax({
            type: "POST",
            url: url,
            data: postBody,
            success: function (result) {
                call_back(params.guid, result);
                _isBusy = false;
            }
        });
}

// Response from sap gui
function call_back(guid, data1, data2, data3, data4, data5, data6, data7) {
    setTimeout(function () {
        // Call fm
        var fm = callBack[guid];
        if (fm)
            fm.onBack(data1, data2, data3, data4, data5, data6, data7);

        // delete from history
        delete callBack[guid];

        // Can call again
        _isBusy = false;

    }, getDelayTime(150));
}

function getDelayTime(mSec) {
    return parseInt(mSec + Math.random() * mSec);
}


function saveFileAs(params) {
    var sType = 'data:text/plain';
    var oBlob = null;
    if (!params.charset) {
        sType += ';charset=charset=utf-8';
        oBlob = new Blob([new Uint8Array([0xEF, 0xBB, 0xBF]), params.content], {type: sType});
    } else if (params.charset === '4103') {
        sType += ';charset=UTF-16LE';
        var charCode, byteArray = [];

        // BOM = 'FFFE'
        byteArray.push(255, 254);

        for (var i = 0; i < params.content.length; ++i) {
            charCode = params.content.charCodeAt(i);

            // LE Bytes
            byteArray.push(charCode & 0xff);
            byteArray.push(charCode / 256 >>> 0);
        }
        oBlob = new Blob([new Uint8Array(byteArray)], {type: sType});
    }

    if (window.navigator.msSaveOrOpenBlob) {
        window.navigator.msSaveOrOpenBlob(oBlob, params.file_name);
    } else {
        var oURL = window.URL || window.webkitURL;
        var sBlobUrl = oURL.createObjectURL(oBlob);

        var oLink = window.document.createElement('a');

        // use an anchor link with download attribute for download
        var $body = jQuery(document.body);
        var $link = jQuery(oLink).attr({
            download: params.file_name,
            href: sBlobUrl,
            style: 'display:none'
        });
        $body.append($link);
        $link.get(0).click();

        $link.remove();
    }
}

function decodeUTF16LE(binaryStr, skipBom) {
    var cp = [];
    for (var i = skipBom ? 2 : 0; i < binaryStr.length; i += 2)
        cp.push(
            binaryStr[i] | (binaryStr[i + 1] << 8)
        );
    return String.fromCharCode.apply(String, cp);
}

/**
 * @return {string}
 */
function decodeUTF8(array, skipBom) {
    var out = "", i, len, c;
    var char2, char3;

    len = array.length;
    i = skipBom ? 3 : 0;
    while (i < len) {
        c = array[i++];
        switch (c >> 4) {
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
                // 0xxxxxxx
                out += String.fromCharCode(c);
                break;
            case 12:
            case 13:
                // 110x xxxx   10xx xxxx
                char2 = array[i++];
                out += String.fromCharCode(((c & 0x1F) << 6) | (char2 & 0x3F));
                break;
            case 14:
                // 1110 xxxx  10xx xxxx  10xx xxxx
                char2 = array[i++];
                char3 = array[i++];
                out += String.fromCharCode(((c & 0x0F) << 12) |
                    ((char2 & 0x3F) << 6) |
                    ((char3 & 0x3F) << 0));
                break;
        }
    }

    return out;
}