import bucklescript from "rollup-plugin-bucklescript";
import resolve from "rollup-plugin-node-resolve";
import uglify from "rollup-plugin-uglify";

export default {
  input: "src/cli.re",
  output: {
    file: "dist/cli.js",
    format: "cjs",
  },
  plugins: [
    bucklescript({
      module: "es6"
    }),
    resolve(),
    uglify()
  ],
  external: [
    "@cowlick/analyzer"
  ]
};
