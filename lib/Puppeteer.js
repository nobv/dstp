'use strict';
const puppeteer = require('puppeteer');

export const _launch = (options) => () => puppeteer.launch(options);

export const _newPage = (browser) => () => browser.newPage();

export const _goto = (page) => (url) => () => page.goto(url);

export const _close = (browser) => () => browser.close();
