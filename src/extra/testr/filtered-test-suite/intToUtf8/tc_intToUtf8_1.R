expected <- eval(parse(text="\"\""));
test(id=0, code={
argv <- eval(parse(text="list(NULL, FALSE, FALSE)"));
.Internal(intToUtf8(argv[[1]], argv[[2]], argv[[3]]));
}, o=expected);