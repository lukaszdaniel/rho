expected <- eval(parse(text="TRUE"));
test(id=0, code={
argv <- eval(parse(text="list(structure(c(\"dgTMatrix\", \"matrix.coo\"), .Names = c(\"from\", \"to\"), package = c(\"Matrix\", \"\"), class = structure(\"signature\", package = \"methods\")), structure(c(\"dgTMatrix\", \"matrix.coo\"), .Names = c(\"from\", \"to\"), package = c(\"Matrix\", \"\"), class = structure(\"signature\", package = \"methods\")), TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)"));
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));
}, o=expected);