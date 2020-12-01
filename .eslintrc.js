/* eslint-disable */

module.exports = {
    "env": {
        "browser": true,
        "node": true,
        "es6": true
    },
    "extends": [
        "eslint:recommended",
        "plugin:@typescript-eslint/recommended"
    ],
    "globals": {
        "Atomics": "readonly",
        "SharedArrayBuffer": "readonly"
    },
    "parser": "@typescript-eslint/parser",
    "parserOptions": {
        "ecmaVersion": 2018,
        "tsconfigRootDir": __dirname,
        "project": ["./tsconfig.json"],
    },
    "plugins": [
        "@typescript-eslint",
    ],
    "rules": {
        "eol-last": "error",
        "eqeqeq": ["error", "smart"],
        "indent" : ["error", "tab", { "SwitchCase": 1}],
        "max-len": ["warn", { "code": 200 }],
        "no-confusing-arrow": "error",
        "no-multi-spaces": ["error", { "ignoreEOLComments": true }],
        "@typescript-eslint/no-var-requires": "off",
        "no-shadow": "off",
        "@typescript-eslint/no-shadow": ["error"],
        "no-trailing-spaces": "error",
        "no-unused-vars": ["error", { "argsIgnorePattern": "^_" }],
        "@typescript-eslint/no-unused-vars": ["error", { "argsIgnorePattern": "^_" }],
        "no-unused-expressions": "error",
        "no-useless-computed-key": "error",
        "no-var": "error",
        "prefer-const": "error",
        "semi": "error",
    }
};
