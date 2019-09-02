'use strict';

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.waitForSelectorImpl = exports.waitForNavigationImpl = exports.submitImpl = exports.screenshotImpl = exports.clickImpl = exports.closeImpl = exports.gotoImpl = exports.newPageImpl = exports.launchImpl = void 0;

var _objectDestructuringEmpty2 = _interopRequireDefault(require("@babel/runtime/helpers/objectDestructuringEmpty"));

var puppeteer = require('puppeteer');

var launchImpl = function launchImpl(options) {
  return puppeteer.launch(options);
};

exports.launchImpl = launchImpl;

var newPageImpl = function newPageImpl(browser) {
  return browser.newPage();
};

exports.newPageImpl = newPageImpl;

var gotoImpl = function gotoImpl(page) {
  return function (url) {
    return page["goto"](url);
  };
};

exports.gotoImpl = gotoImpl;

var closeImpl = function closeImpl(browser) {
  return browser.close();
};

exports.closeImpl = closeImpl;

var clickImpl = function clickImpl(page) {
  return function (selector) {
    return page.click(selector);
  };
};

exports.clickImpl = clickImpl;

var screenshotImpl = function screenshotImpl(page) {
  return function (screenshotOptions) {
    return page.screenshot(screenshotOptions);
  };
};

exports.screenshotImpl = screenshotImpl;

var submitImpl = function submitImpl(page) {
  return function (selector) {
    return page.evaluate(function (_ref) {
      (0, _objectDestructuringEmpty2["default"])(_ref);
      $(selector).parent('form').submit();
    }, selector);
  };
};

exports.submitImpl = submitImpl;

var waitForNavigationImpl = function waitForNavigationImpl(page) {
  return page.waitForNavigation();
};

exports.waitForNavigationImpl = waitForNavigationImpl;

var waitForSelectorImpl = function waitForSelectorImpl(page) {
  return function (selector) {
    return page.waitForSelector(selector);
  };
};

exports.waitForSelectorImpl = waitForSelectorImpl;