'use struct'

const yaml = require('js-yaml');

exports.safeLoadImpl = function (target) {
    yaml.safeLoad(target);
};
