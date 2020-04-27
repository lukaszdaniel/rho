expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE), .Dim = c(4L, 4L))"));
test(id=0, code={
argv <- eval(parse(text="list(c(FALSE, TRUE, TRUE, TRUE), 4L, 4L)"));
.Internal(diag(argv[[1]], argv[[2]], argv[[3]]));
}, o=expected);