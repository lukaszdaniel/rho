expected <- eval(parse(text="numeric(0)"));
test(id=0, code={
argv <- eval(parse(text="list(structure(list(sec = numeric(0), min = integer(0), hour = integer(0), mday = integer(0), mon = integer(0), year = integer(0), wday = integer(0), yday = integer(0), isdst = integer(0)), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\")), \"\")"));
.Internal(as.POSIXct(argv[[1]], argv[[2]]));
}, o=expected);