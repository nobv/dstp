'use strict';

var puppeteer = require('puppeteer');

var _launch = function _launch(options) {
  return function () {
    return puppeteer.launch(options);
  };
};

var _newPage = function _newPage(browser) {
  return function () {
    return browser.newPage();
  };
};

var _goto = function _goto(page, url) {
  return function () {
    return page["goto"](url);
  };
};

var _waitForSelector = function _waitForSelector(page, selector, options) {
  return function () {
    return page.waitForSelector(selector, options);
  };
};

var _type = function _type(page, selector, content, options) {
  return function () {
    return page.type(selector, content, options);
  };
};

var _click = function _click(page, selector) {
  return function () {
    return page.click(selector);
  };
};

var _waitForNavigation = function _waitForNavigation(page, options) {
  return function () {
    return page.waitForNavigation(options);
  };
};

var _close = function _close(browser) {
  return function () {
    return browser.close();
  };
};

module.exports = {
  puppeteer: puppeteer,
  _launch: _launch,
  _newPage: _newPage,
  _goto: _goto,
  _waitForSelector: _waitForSelector,
  _type: _type,
  _c1ick: _click,
  _waitForNavigation: _waitForNavigation,
  _close: _close
};