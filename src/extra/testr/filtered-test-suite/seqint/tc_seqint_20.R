test(id=4, code={
argv <- structure(list(to = NaN), .Names = "to")
do.call('seq.int', argv);
}, e = "'to' must be a finite number");