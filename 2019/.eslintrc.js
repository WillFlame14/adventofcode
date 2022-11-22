/* eslint-disable */

module.exports = {
    "env": {
        "browser": true,
        "node": true,
        "es6": true
    },
    "extends": [
        "eslint:recommended",
    ],
    "globals": {
        "Atomics": "readonly",
        "SharedArrayBuffer": "readonly"
    },
    "parserOptions": {
        "ecmaVersion": 2018
    },
    "rules": {
        "eol-last": "error",
        "eqeqeq": ["error", "smart"],
        "indent" : "off",
        "max-len": ["warn", { "code": 200 }],
        "no-confusing-arrow": "error",
        "no-constant-condition": ["error", { "checkLoops": false }],
        "no-multi-spaces": ["error", { "ignoreEOLComments": true }],
        "no-shadow": "error",
        "no-trailing-spaces": "error",
        "no-unused-vars": ["error", { "argsIgnorePattern": "^_" }],
        "no-unused-expressions": "error",
        "no-useless-computed-key": "error",
        "no-var": "error",
        "prefer-const": "error",
        "semi": "error",
    }
};
