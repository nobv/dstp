'use strict';

var puppeteer = require('puppeteer');

var Launch = function Launch(options) {
  return puppeteer.launch(options);
};

var NewPage = function NewPage(browser) {
  return browser.newPage();
};

var Goto = function Goto(page, url) {
  return page["goto"](url);
};

var WaitForSelector = function WaitForSelector(page, selector, options) {
  return page.waitForSelector(selector, options);
};

var Typing = function Typing(page, selector, content, options) {
  return page.type(selector, content, options);
};

var Click = function Click(page, selector) {
  return page.click(selector);
};

var WaitForNavigation = function WaitForNavigation(page, options) {
  return page.waitForNavigation(options);
};

var Close = function Close(browser) {
  return browser.close();
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