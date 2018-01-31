let parse = Minimist.parse(
  ~alias = [
    ("h", "help"),
    ("o", "output"),
  ],
  ~presence = [
    "help"
  ],
  ~multi = [],
  ~strings = [
    "output"
  ]
);

let help = {|
Usage: cowlick-sexpr-compiler [options] inputDir

  -o, --output (default: ./script)
      output directory
  -h, --help
      print this help
|};

let fail = (msg) => {
  print_endline(msg);
  print_endline(help);
  exit(1);
};

switch (parse(List.tl(Array.to_list(Sys.argv)))) {
| Minimist.Error(err) =>
  fail(Minimist.report(err));
| Ok(opts) =>
  if (Minimist.StrSet.mem("help", opts.presence)) {
    print_endline(help);
    exit(0);
  } else switch (opts.rest) {
    | [] => fail("Expected entry directory")
    | [entry, ..._] =>
      entry
      |> Filename.concat (Sys.getcwd ())
      |> Compiler.compile
  }
};
