expected <- eval(parse(text="logical(0)"));
test(id=0, code={
argv <- eval(parse(text="list(character(0))"));
.Internal(file.remove(argv[[1]]));
}, o=expected);