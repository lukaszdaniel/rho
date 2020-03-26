test(id=3, code={
argv <- list(NaN)
do.call('seq.int', argv);
}, e = "'from' must be a finite number");