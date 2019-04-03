sap.ui.define([
        'sap/ui/base/Object'
    ], function (Object) {
        "use strict";

        return Object.extend("com.modekz.aqo.controller.libMockServer", {

            // Detect odata type
            oBoundaryRegex: new RegExp("--batch_[a-z0-9-]*"),
            rPut: new RegExp("PUT (.*) HTTP"),
            rPost: new RegExp("POST (.*) HTTP"),
            rDelete: new RegExp("DELETE (.*) HTTP"),
            rGet: new RegExp("GET (.*) HTTP"),

            // Just those set of filter operations
            rMethod1: new RegExp("(.*) (eq|ne|gt|lt|le|ge) (.*)"),
            rMethod2: new RegExp("(substringof|startswith|endswith)\\(([^\\)]*),(.*)\\)"),

            constructor: function (sinon, files) {
                var _this = this;

                // Init sinon fake server
                var server = sinon.fakeServer.create();
                server.xhr.useFilters = true;
                server.autoRespond = false;

                // Special case for sap
                var httpType = getHttpType();

                // For files in 'webapp' only
                server.xhr.addFilter(function (method, url) {
                    // result = false use sinon
                    var result = url.indexOf("odata.svc/") < 0;

                    // If in IE in SAP
                    if (httpType === HttpType.SAP && url.indexOf("webapp/") > 0)
                        result = false;

                    // if (result === false)
                    //     console.log(url);
                    // else
                    //     console.warn(url);
                    return result;
                });

                // Just ignore header
                server.respondWith("HEAD", "/odata.svc/", function (oXhr) {
                    var mHeaders = {
                        "Content-Type": "application/json;charset=utf-8"
                    };
                    oXhr.respond(200, mHeaders);
                    return true;
                });

                // Respond with strings
                for (var i = 0; i < files.length; i++) {
                    (function (file) {
                        var urlPattern = "./" + file.NAME.replace(/\\/g, '/');

                        // Special case for annotation
                        if ("webapp/localService/metadata.xml" === file.NAME)
                            urlPattern = /\/odata.svc\/\$metadata(.*)/;

                        server.respondWith(urlPattern, function (oXhr) {
                            var ext = file.NAME.split('.').pop();
                            var mHeaders = {
                                "Content-Type": _this.getContentType(ext)
                            };
                            oXhr.respond(200, mHeaders, file.FILE);
                        });
                    })(files[i]);
                }

                // Check periodically queue
                setInterval(function () {
                    if (server.queue)
                        for (var i = 0; i < server.queue.length; i++) {
                            var request = server.queue[i];
                            var url = request.url;

                            // For own oData fake server
                            if (url === "/odata.svc/$batch") {
                                server.queue.splice(i, 1);
                                _this.processItem(request);
                            }
                        }

                    // Response first
                    server.respond();

                }, 10);
            },

            prepareParams: function (params) {
                var _this = this;
                var isCount = false;
                var skipCnt = 0;

                var result = [];
                for (var p = 0; p < params.length; p++) {
                    var pair = params[p].split('=');
                    var newParam = {
                        KEY: pair[0],
                        VALUE: pair[1]
                    };
                    result.push(newParam);

                    switch (newParam.KEY) {
                        case "$count":
                            isCount = true;
                            break;

                        case "$skip":
                            skipCnt = parseInt(newParam.VALUE);
                            break;

                        case "$top":
                            break;

                        case "$orderby":
                            // console.log(newParam.VALUE)

                            // For tables only, difficult to implement for SH (only for 'ZHAQO_OPTION')
                            var sorts = newParam.VALUE.split(',');

                            // 2 kind of sort
                            newParam.VALUE = {
                                DB_SORT: '',
                                SH_SORT: []
                            };

                            // Add one by one
                            for (var s = 0; s < sorts.length; s++) {
                                var sort = sorts[s].split(' ');
                                var bDesc = sort[1] === 'desc';

                                // Sort internal table
                                newParam.VALUE.SH_SORT.push({
                                    NAME: sort[0],
                                    DESCENDING: bDesc ? 'X' : ' '
                                });

                                // DB sort
                                newParam.VALUE.DB_SORT += (
                                    (s === 0 ? '' : ' ') +
                                    sort[0] +
                                    (bDesc ? ' DESCENDING' : ''));
                            }

                            // For abap
                            newParam.VALUE = JSON.stringify({
                                DATA: newParam.VALUE
                            });

                            break;

                        case "$filter":
                            // 2 kind of filtration
                            var filter = {
                                DB_FILTER: '',
                                SH_FILTER: []
                            };
                            _this._createFilter(filter, newParam.VALUE);

                            // For abap
                            newParam.VALUE = JSON.stringify({
                                DATA: filter
                            });
                            break;

                        default:
                            throw "Unknown param " + pair[0];
                    }
                }

                // console.log(result);

                // // Show tip as alert! (not Toast)
                // if (skipCnt > 0 && !isCount && getHttpType() === HttpType.SAP)
                //     alert(this.textBundle.getText('useFilter'));

                return result;
            },

            _createFilter: function (filter, condition) {
                var _this = this;
                var delimiter = ' AND ';
                var allCond = condition.split(' and ');
                if (allCond.length === 1) {
                    delimiter = ' OR ';
                    allCond = condition.split(' or ');
                }

                var arrEmptyText = ['', ''];
                for (var c = 0; c < allCond.length; c++) {
                    var cond = allCond[c];

                    // Sub condition
                    if (cond.substr(0, 1) === "(" && cond.slice(-1) === ")") {
                        var subFilter = {
                            DB_FILTER: '',
                            SH_FILTER: []
                        };

                        this._createFilter(subFilter, cond.slice(1, -1));
                        filter.SH_FILTER = filter.SH_FILTER.concat(subFilter.SH_FILTER);
                        filter.DB_FILTER += ("(" + subFilter.DB_FILTER + ")");

                        continue;
                    }

                    // Detect method
                    var sFilterMethod = null;

                    var aFilterValues = _this.rMethod1.exec(cond);
                    if (aFilterValues) {
                        sFilterMethod = aFilterValues[2];
                    } else {
                        aFilterValues = _this.rMethod2.exec(cond);
                        if (aFilterValues) {
                            sFilterMethod = aFilterValues[1];
                        }
                    }

                    // Oops
                    if (!sFilterMethod)
                        throw "Unknown operation";

                    var lv_val_pref = arrEmptyText;
                    var lv_val_post = arrEmptyText;
                    var lv_field = '';
                    var lv_option = '';
                    var lv_low = '';
                    switch (sFilterMethod) {
                        case "substringof":
                            lv_val_pref = ['%', '*'];
                            lv_val_post = ['%', '*'];
                            lv_field = aFilterValues[3];
                            lv_low = aFilterValues[2];
                            break;

                        case "startswith":
                            lv_val_post = ['%', '*'];
                            lv_field = aFilterValues[2];
                            lv_low = aFilterValues[3];

                            break;

                        case "endswith":
                            lv_val_pref = ['%', '*'];
                            lv_field = aFilterValues[2];
                            lv_low = aFilterValues[3];
                            break;

                        case "eq":
                        case "ne":
                        case "gt":
                        case "lt":
                        case "ge":
                        case "le":
                            lv_option = sFilterMethod.toUpperCase();
                            sFilterMethod = ' ' + lv_option + ' ';
                            lv_field = aFilterValues[1];
                            lv_low = aFilterValues[3];
                            break;

                        default:
                            throw "Unknown method: " + sFilterMethod;
                    }

                    // String operation
                    if (lv_val_pref !== arrEmptyText || lv_val_post !== arrEmptyText) {
                        sFilterMethod = ' LIKE ';
                        lv_option = 'CP';
                    }

                    // TODO Check delete '---'
                    if ((lv_option === 'CP' || lv_option === 'EQ') && lv_low.substr(0, 1) === "'" && lv_low.slice(-1) === "'")
                        lv_low = lv_low.slice(1, -1);

                    // SQL filter
                    if (filter.SH_FILTER.length > 0)
                        filter.DB_FILTER += delimiter;
                    filter.DB_FILTER += (lv_field + sFilterMethod + "'" +
                        // remaining `*` -> `%`
                        lv_val_pref[0] + (lv_option === 'CP' ? lv_low.replace('*', '%') : lv_low) + lv_val_post[0] + "'");

                    filter.SH_FILTER.push({
                        SHLPFIELD: lv_field,
                        SIGN: 'I',
                        OPTION: lv_option,
                        LOW: lv_val_pref[1] + lv_low + lv_val_post[1]
                    });
                }
            },

            // oData request
            processItem: function (oXhr) {
                var _this = this;
                var sBoundary = _this.oBoundaryRegex.exec(oXhr.requestBody)[0];

                // boundary is defined in request header
                if (!sBoundary)
                    return;

                var aBatchRequests = oXhr.requestBody.split(sBoundary);

                // Data to backend
                var query = {
                    requests: [],

                    add2Requests: function (oneRequest) {
                        // Separate entity and params
                        var parts = oneRequest.split('/', 2);
                        if (parts.length === 1)
                            parts = oneRequest.split('?', 2);

                        // For node & abap
                        var request = {
                            URL: oneRequest,
                            ENTITY: parts[0],
                            PARAMS: _this.prepareParams(decodeURIComponent(parts[1]).replace("?", "&").split("&")),
                            COUNT_ONLY: parts[1].indexOf('$count') !== -1
                        };

                        // Add info about request
                        this.requests.push(request);
                    },

                    onBack: function (responses) {
                        var data = [];
                        responses = responses.DATA;

                        for (var i = 0; i < responses.length; i++) {
                            var request = query.requests[i];
                            var response = responses[i];

                            // Json path
                            if (!request.COUNT_ONLY) {
                                response.DATA = {
                                    d: {
                                        results: response.DATA
                                    }
                                };

                                var rows = response.DATA.d.results;

                                // fill metadata
                                for (var r = 0; r < rows.length; r++) {
                                    var row = rows[r];

                                    var uri = "";
                                    for (var f = 0; f < response.FIELDS.length; f++) {
                                        var field = response.FIELDS[f];

                                        var value = row[field.NAME];
                                        var typePref = '';

                                        var newValue = undefined;
                                        switch (field.UI_TYPE) {
                                            case "date":
                                            case "datetime":
                                                newValue = null;
                                                typePref = "datetime";
                                                if (value !== '0000-00-00' || value !== '')
                                                    newValue = new Date(Date.parse(value));
                                                break;

                                            case "time":
                                                newValue = null;
                                                typePref = "datetime";
                                                if (value !== '00:00:00')
                                                    newValue = new Date(Date.parse('1970-01-01T' + value));
                                                break;
                                        }

                                        // Set new date
                                        if (newValue !== undefined)
                                            row[field.NAME] = newValue;

                                        if (!field.IS_KEY)
                                            continue;

                                        uri += (
                                            (uri ? "," : "/odata.svc/" + request.ENTITY + "(") +
                                            field.NAME + "=" + typePref + "'" + encodeURIComponent(value) + "'");
                                    }
                                    uri += ")";
                                    // console.log(row);

                                    row.__metadata = {
                                        id: uri,
                                        type: "ODATA_SRV." + request.ENTITY.slice(0, -1),
                                        uri: uri
                                    };
                                }
                            }

                            // $count or json
                            var sResponseString = _this.fnBuildResponseString(
                                response,
                                request.COUNT_ONLY ? "text/plain" : "application/json");

                            data.push(
                                "\r\nContent-Type: application/http\r\n" +
                                "Content-Length: " + sResponseString.length + "\r\n" +
                                "content-transfer-encoding: binary\r\n\r\n" + sResponseString)
                        }

                        //CREATE BATCH RESPONSE
                        var sRespondData = "--ejjeeffe0";
                        for (var m = 0; m < data.length; m++) {
                            sRespondData += data[m] + "--ejjeeffe0";
                        }
                        sRespondData += "--";
                        var mHeaders = {
                            'Content-Type': "multipart/mixed; boundary=ejjeeffe0"
                        };

                        // console.log(sRespondData);
                        //alert(JSON.stringify(query.requests) + "\n\r" + sRespondData.length)
                        oXhr.respond(202, mHeaders, sRespondData);
                    }
                };

                // console.log(aBatchRequests);
                for (var i = 1; i < aBatchRequests.length; i++) {
                    var sBatchRequest = aBatchRequests[i];
                    //GET Handling
                    if (_this.rGet.test(sBatchRequest) && sBatchRequest.indexOf("multipart/mixed") === -1) {
                        //In case of POST, PUT or DELETE not in ChangeSet
                        if (_this.rPut.test(sBatchRequest) || _this.rPost.test(sBatchRequest) || _this.rDelete.test(sBatchRequest)) {
                            oXhr.respond(400, null, "The Data Services Request could not be understood due to malformed syntax");
                            $.sap.log.debug("MockServer: response sent with: 400");
                            return true;
                        }

                        // Could be several requests
                        var oneRequest = _this.rGet.exec(sBatchRequest)[1];
                        query.add2Requests(oneRequest);
                    }
                }

                // Call backend
                postAction("SAP_ODATA_QUERY", query);
            },

            fnBuildResponseString: function (oResponse, sContentType) {
                // create the response data string => convert to JSON if possible or use the content directly
                var sResponseData;
                if (oResponse.SUCCESS) {
                    sResponseData = JSON.stringify(oResponse.DATA) || "";
                } else {
                    sResponseData = oResponse.ERROR;
                }

                // default the content type to application/json
                sContentType = sContentType || "application/json";

                // by default the dataserviceversion header will be attached to the response headers
                if (oResponse.responseHeaders) {
                    // if the response contains the headers we include them into the
                    // response string which will be added to the BATCH
                    return "HTTP/1.1 " + this.fnResovleStatus(oResponse) + "\r\n" + oResponse.responseHeaders +
                        "dataserviceversion: 2.0\r\n\r\n" + sResponseData + "\r\n";
                } else {
                    // if a content type is defined we override the incoming response content type
                    return "HTTP/1.1 " + this.fnResovleStatus(oResponse) + "\r\nContent-Type: " + sContentType + "\r\nContent-Length: " +
                        sResponseData.length + "\r\ndataserviceversion: 2.0\r\n\r\n" + sResponseData + "\r\n";
                }
            },

            fnResovleStatus: function (oResponse) {
                switch (oResponse.STATUS) { // statusCode
                    case 200:
                        return "200 OK";
                    case 201:
                        return "201 Created";
                    case 204:
                        return "204 No Content";
                    case 400:
                        return "400 Bad Request";
                    case 401:
                        return "401 Unauthorized";
                    case 403:
                        return "403 Forbidden";
                    case 404:
                        return "404 Not Found";
                    case 405:
                        return "405 Method Not Allowed";
                    case 409:
                        return "409 Conflict";
                    case 412:
                        return "412 Precondition Failed";
                    case 415:
                        return "415 Unsupported Media Type";
                    case 500:
                        return "500 Internal Server Error";
                    case 501:
                        return "501 Not Implemented";
                    case 503:
                        return "503 Service Unavailable";
                    default:
                        return oResponse.statusCode + " " + oResponse.status;
                }
            },

            // Detect by extension
            getContentType: function (ext) {
                switch (ext) {
                    case "html":
                        return "text/html";
                    case "js":
                        return "application/javascript";

                    // Error in BSP
                    case "txt":
                    case "json":
                        return "application/json";
                    case "xml":
                    case "svc":
                        return "application/xml";
                    case"properties":
                        return "application/octet-stream";
                }

                throw "No case for ext:" + ext;
            }
        });
    }
);