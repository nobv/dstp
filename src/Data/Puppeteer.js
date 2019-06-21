'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports._close = exports._goto = exports._newPage = exports._launch = void 0;

var puppeteer = require('puppeteer');

var _launch = function _launch(options) {
  return function () {
    return puppeteer.launch(options);
  };
};

exports._launch = _launch;

var _newPage = function _newPage(browser) {
  return function () {
    return browser.newPage();
  };
};

exports._newPage = _newPage;

var _goto = function _goto(page) {
  return function (url) {
    return function () {
      return page["goto"](url);
    };
  };
};

exports._goto = _goto;

var _close = function _close(browser) {
  return function () {
    return browser.close();
  };
};

exports._close = _close;