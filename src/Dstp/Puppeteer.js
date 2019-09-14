'use struct'

const puppeteer = require('puppeteer');

exports.launchImpl = (options) => puppeteer.launch(options);

exports.newPageImpl = (browser) => browser.newPage();

exports.gotoImpl = (page, url) => page.goto(url);

exports.closeImpl = (browser) => browser.close();

exports.clickImpl = (page, selector) => page.click(selector);

exports.screenshotImpl = (page, options) => page.screenshot(options);

exports.submitImpl = (page, selector) => page.evaluate("$(" + selector + ").parent('form').submit()");

exports.waitForNavigationImpl = (page) => page.waitForNavigation();

exports.waitForSelectorImpl = (page, selector) => page.waitForSelector(selector);
