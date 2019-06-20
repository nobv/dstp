'use strict';
const puppeteer = require('puppeteer');

const Launch = (options) => {
  return () => {
    return puppeteer.launch(options);
  };
};

const NewPage = (browser) => {
  return () => {
    return browser.newPage();
  };
};

const Goto = (page, url) => {
  return () => {
    return page.goto(url);
  };
};

const WaitForSelector = (page, selector, options) => {
  return () => {
    return page.waitForSelector(selector, options);
  };
};

const Typing = (page, selector, content, options) => {
  return () => {
    return page.type(selector, content, options);
  };
};

const Click = (page, selector) => {
  return () => {
    return page.click(selector);
  };
};

const WaitForNavigation = (page, options) => {
  return () => {
    return page.waitForNavigation(options);
  };
};

const Close = (browser) => {
  return () => {
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
  callClose: Close,
};
