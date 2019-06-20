'use strict';
const puppeteer = require('puppeteer');

const _launch = (options) => () => puppeteer.launch(options);

const _newPage = (browser) => () => browser.newPage();

const _goto = (page, url) => () => page.goto(url);

const _waitForSelector = (page, selector, options) => () => page.waitForSelector(selector, options);

const _type = (page, selector, content, options) => () => page.type(selector, content, options);

const _click = (page, selector) => () => page.click(selector);

const _waitForNavigation = (page, options) => () => page.waitForNavigation(options);

const _close = (browser) => () => browser.close();

module.exports = {
  puppeteer: puppeteer,
  _launch: _launch,
  _newPage: _newPage,
  _goto: _goto,
  _waitForSelector: _waitForSelector,
  _type: _type,
  _c1ick: _click,
  _waitForNavigation: _waitForNavigation,
  _close: _close,
};
