expected <- c(18000, 18204.0816326531, 18408.1632653061, 18612.2448979592, 
18816.3265306122, 19020.4081632653, 19224.4897959184, 19428.5714285714, 
19632.6530612245, 19836.7346938776, 20040.8163265306, 20244.8979591837, 
20448.9795918367, 20653.0612244898, 20857.1428571429, 21061.2244897959, 
21265.306122449, 21469.387755102, 21673.4693877551, 21877.5510204082, 
22081.6326530612, 22285.7142857143, 22489.7959183673, 22693.8775510204, 
22897.9591836735, 23102.0408163265, 23306.1224489796, 23510.2040816327, 
23714.2857142857, 23918.3673469388, 24122.4489795918, 24326.5306122449, 
24530.612244898, 24734.693877551, 24938.7755102041, 25142.8571428571, 
25346.9387755102, 25551.0204081633, 25755.1020408163, 25959.1836734694, 
26163.2653061224, 26367.3469387755, 26571.4285714286, 26775.5102040816, 
26979.5918367347, 27183.6734693878, 27387.7551020408, 27591.8367346939, 
27795.9183673469, 28000)
test(id=43, code={
argv <- structure(list(18000, 28000, length = 50L), .Names = c("", "", 
"length"))
do.call('seq', argv);
},  o = expected);