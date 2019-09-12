'use strict';
const puppeteer = require('puppeteer');

export const launchImpl = (options) => puppeteer.launch(options);

export const newPageImpl = (browser) => browser.newPage();

export const gotoImpl = (page, url) => page.goto(url);

export const closeImpl = (browser) => browser.close();

export const clickImpl = (page, selector) => page.click(selector);

export const screenshotImpl = (page, screenshotOptions) => page.screenshot(screenshotOptions);

export const submitImpl = (page, selector) => page.evaluate(({}) => {
    $(selector).parent('form').submit();
}, selector);

export const waitForNavigationImpl = (page) => page.waitForNavigation();

export const waitForSelectorImpl = (page, selector) => page.waitForSelector(selector);
