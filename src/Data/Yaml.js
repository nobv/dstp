'use struct'

const yaml = require('js-yaml');

exports.safeLoadImpl = (target) => yaml.safeLoad(target);
