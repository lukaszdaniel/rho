expected <- eval(parse(text="c(0.510763209393394, Inf, 1, 1, Inf, 1, 0, 1.95785440613009, 48.49854545454, Inf, 1, 1, 0.342969776609699, 0.00707175387211123)"));
test(id=0, code={
argv <- eval(parse(text="list(c(-0.510763209393394, Inf, 1, 1, Inf, 1, 0, -1.95785440613009, -48.49854545454, -Inf, 1, 1, 0.342969776609699, 0.00707175387211123))"));
do.call(`abs`, argv);
}, o=expected);