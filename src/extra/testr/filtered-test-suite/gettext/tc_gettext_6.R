expected <- eval(parse(text="NULL"));
test(id=0, code={
argv <- eval(parse(text="list(NULL, NULL)"));
.Internal(gettext(argv[[1]], argv[[2]]));
}, o=expected);