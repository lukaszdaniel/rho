expected <- eval(parse(text="FALSE"));
test(id=0, code={
argv <- eval(parse(text="list(\"• \")"));
do.call(`is.list`, argv);
}, o=expected);