expected <- eval(parse(text="NULL"));
test(id=0, code={
argv <- eval(parse(text="list(NULL, NULL)"));
do.call(`levels<-`, argv);
}, o=expected);