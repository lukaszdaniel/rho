expected <- eval(parse(text="structure(\"Error in rnorm(2, c(1, NA)) : (converted from warning) NAs produced\\n\", class = \"try-error\", condition = structure(list(message = \"(converted from warning) NAs produced\", call = quote(rnorm(2, c(1, NA)))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\")))"));
test(id=0, code={
argv <- eval(parse(text="list(structure(\"Error in rnorm(2, c(1, NA)) : (converted from warning) NAs produced\\n\", class = \"try-error\", condition = structure(list(message = \"(converted from warning) NAs produced\", call = quote(rnorm(2, c(1, NA)))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))))"));
do.call(`invisible`, argv);
}, o=expected);