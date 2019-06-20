'use strict';

var puppeteer = require('puppeteer');

var Launch = function Launch(options) {
  return function () {
    return puppeteer.launch(options);
  };
};

var NewPage = function NewPage(browser) {
  return function () {
    return browser.newPage();
  };
};

var Goto = function Goto(page, url) {
  return function () {
    return page["goto"](url);
  };
};

var WaitForSelector = function WaitForSelector(page, selector, options) {
  return function () {
    return page.waitForSelector(selector, options);
  };
};

var Typing = function Typing(page, selector, content, options) {
  return function () {
    return page.type(selector, content, options);
  };
};

var Click = function Click(page, selector) {
  return function () {
    return page.click(selector);
  };
};

var WaitForNavigation = function WaitForNavigation(page, options) {
  return function () {
    return page.waitForNavigation(options);
  };
};

var Close = function Close(browser) {
  return function () {
    return browser.close();
  };
};

module.exports = {
  puppeteer: puppeteer,
  callLaunch: Launch,
  callNewPage: NewPage,
  callGoto: Goto,
  callWaitForSelector: WaitForSelector,
  callType: Typing,
  callClick: Click,
  callWaitForNavigation: WaitForNavigation,
  callClose: Close
};