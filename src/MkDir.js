const fs = require("fs");

exports.mkdirRec = (path) => (cb) => () => fs.mkdir(path, { recursive: true }, cb);
