expected <- eval(parse(text="c(3L, 2L, 1L, 1L, 1L, 0L, 0L, 0L, 0L)"));
test(id=0, code={
argv <- eval(parse(text="list(c(3L, 2L, 1L, 2L, 1L, 0L, 4L, 3L, 2L))"));
do.call(`cummin`, argv);
}, o=expected);