expected <- eval(parse(text="0"));
test(id=0, code={
argv <- eval(parse(text="list(FALSE)"));
do.call(`asinh`, argv);
}, o=expected);