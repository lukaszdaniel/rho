expected <- eval(parse(text="TRUE"));
test(id=0, code={
argv <- eval(parse(text="list(structure(list(), .Names = character(0), row.names = integer(0), class = \"data.frame\"), structure(list(), .Names = character(0), row.names = integer(0), class = \"data.frame\"), TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)"));
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));
}, o=expected);