expected <- eval(parse(text="FALSE"));
test(id=0, code={
argv <- eval(parse(text="list(FALSE, structure(1L, class = c(\"terminal\", \"connection\")), 69)"));
.Internal(dput(argv[[1]], argv[[2]], argv[[3]]));
}, o=expected);