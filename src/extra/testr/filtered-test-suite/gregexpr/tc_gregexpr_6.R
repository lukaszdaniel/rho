expected <- list(structure(1:3, match.length = c(0L, 0L, 0L), index.type = "chars", useBytes = TRUE));
test(id=1, code={
argv <- structure(list(pattern = "", text = "abc"), .Names = c("pattern", "text"));
do.call('gregexpr', argv);
},  o = expected);