test(id=0, code={
argv <- list(NaN, NaN)
do.call('seq.int', argv);
}, e = "'from' must be a finite number");