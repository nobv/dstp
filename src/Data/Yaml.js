var yaml = require('js-yaml');

exports.safeLoadImpl = function(target) {
  return yaml.safeLoad(target);
};
