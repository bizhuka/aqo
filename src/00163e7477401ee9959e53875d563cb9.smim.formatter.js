sap.ui.define([], function () {
    "use strict";

    return {
        /**
         * Rounds the currency value to 2 digits
         *
         * @public
         * @param {string} sValue value to be formatted
         * @returns {string} formatted currency value with 2 digits
         */
        currencyValue: function (sValue) {
            if (!sValue) {
                return "";
            }

            return parseFloat(sValue).toFixed(2);
        },

        toSapDate: function (date) {
            return date ?
                date.getFullYear() +
                ('0' + (date.getMonth() + 1)).slice(-2) +
                ('0' + date.getDate()).slice(-2) : "";
        },

        toSapTime: function (time) {
            return time ?
                ('0' + time.getHours()).slice(-2) +
                ('0' + time.getMinutes()).slice(-2) +
                ('0' + time.getSeconds()).slice(-2) : "";
        },

        toSapDateTime: function (date) {
            return date ? this.toSapDate(date) + this.toSapTime(date) : "";
        },

        toLocaleDate: function (date) {
            if (!date)
                return "";

            if (typeof date === "string")
                date = new Date(Date.parse(date));

            return date.toLocaleDateString({
                year: 'numeric',
                month: '2-digit',
                day: '2-digit'
            });
        },

        toLocaleDateTime: function (date) {
            if (!date)
                return "";

            if (typeof date === "string")
                date = new Date(Date.parse(date));

            return date.toLocaleDateString({
                year: 'numeric',
                month: '2-digit',
                day: '2-digit',
                hour: '2-digit',
                minute: '2-digit'
                //second: '2-digit'
            });
        },

        addDays: function (date, cnt) {
            return new Date(date.getTime() + cnt * 3600 * 24 * 1000)
        }
    };

});