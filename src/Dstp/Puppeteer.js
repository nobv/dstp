var puppeteer = require('puppeteer');

exports.launchImpl = function(options) {
  return puppeteer.launch(options);
};

exports.newPageImpl = function(browser) {
  return browser.newPage();
};

exports.gotoImpl = function(page, url) {
  return page.goto(url);
};

exports.closeImpl = function(browser) {
  return browser.close();
};

exports.clickImpl = function(page, selector) {
  return page.click(selector);
};

exports.screenshotImpl = function(page, options) {
  return page.screenshot(options);
};

exports.submitImpl = function(page, selector) {
  return page.evaluate("$(" + selector + ").parent('form').submit()");
};

exports.waitForNavigationImpl = function(page) {
  return page.waitForNavigation();
};


exports.waitForSelectorImpl = function(page, selector) {
  return page.waitForSelector(selector);
};
