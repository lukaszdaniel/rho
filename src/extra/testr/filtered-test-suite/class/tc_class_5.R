expected <- eval(parse(text="structure(\"integer(0)\", .Names = \"c0\")"));
test(id=0, code={
argv <- eval(parse(text="list(structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = structure(\"integer(0)\", .Names = \"c0\")))"));
do.call(`class`, argv);
}, o=expected);