expected <- eval(parse(text="-1"));
test(id=0, code={
argv <- eval(parse(text="list(-1, 3)"));
.Internal(`choose`(argv[[1]], argv[[2]]));
}, o=expected);