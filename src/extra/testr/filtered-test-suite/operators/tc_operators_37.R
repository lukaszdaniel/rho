expected <- eval(parse(text="logical(0)"));
test(id=0, code={
argv <- eval(parse(text="list(logical(0))"));
do.call(`+`, argv);
}, o=expected);