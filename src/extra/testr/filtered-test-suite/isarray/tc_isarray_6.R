expected <- eval(parse(text="TRUE"));
test(id=0, code={
argv <- eval(parse(text="list(structure(c(3+2i, 3+2i, NA, 3+2i, 3+2i, 3+2i, 3+2i, 3+2i, 4-5i, 3-5i, NA, NA, 2-5i, 3-5i, 4-5i, 5-5i), .Dim = c(8L, 2L), .Dimnames = list(NULL, c(\"x1\", \"x2\"))))"));
do.call(`is.array`, argv);
}, o=expected);