import resolve from "rollup-plugin-node-resolve";
import commonjs from "rollup-plugin-commonjs";
import babel from "rollup-plugin-babel";
import {uglify} from "rollup-plugin-uglify";

export default {
  input: "lib/js/src/cli.js",
  output: {
    file: "dist/cli.js",
    format: "cjs",
  },
  plugins: [
    resolve({jsnext: true}),
    commonjs(),
    babel({
      plugins: ["@babel/external-helpers"],
      externalHelpers: true
    }),
    uglify()
  ],
  external: [
    "@cowlick/analyzer",
    "commandpost",
    "fs",
    "path"
  ]
};
