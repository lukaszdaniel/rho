expected <- structure(list(CV = c(4.44258707232128, 1.3448257559694, 0.885694975057761, 
0.838088461472644), mit = structure(list(p = structure(c(0.452549279246557, 
0.13386271764225, 0.267245510599797, 0.146342492511396), .Names = c("cmp1", 
"cmp2", "cmp3", "cmp4")), mu = structure(c(0.381966097098555, 
3.82765024730876, 1.80304880351015, 2.5878804906034, 2.61803339869107, 
0.203368399460934, 1.05601823938856, 0.0596409214659023), .Dim = c(4L, 
2L), .Dimnames = list(c("cmp1", "cmp2", "cmp3", "cmp4"), c("k1", 
"k2"))), Sigma = structure(c(0.22917975838358, 0.847714717429939, 
0.288537968483766, 0.738832302812549, -0.400000241640847, -0.0861897092187198, 
-0.100073467783835, -0.170562219060232, -0.400000241640847, -0.0861897092187198, 
-0.100073467783835, -0.170562219060232, 1.57082072508295, 0.0727738502834565, 
0.219785702621389, 0.217416957416503), .Dim = c(4L, 4L), .Dimnames = list(
    c("cmp1", "cmp2", "cmp3", "cmp4"), c("k1k1", "k1k2", "k2k1", 
    "k2k2"))), df = 1), .Names = c("p", "mu", "Sigma", "df")), 
    summary = structure(list(H = c(1, 2, 3, 4), METHOD.mu = structure(c(1L, 
    1L, 1L, 1L), .Label = "BFGS", class = "factor"), TIME.mu = c(1.301, 
    0.634, 1.148, 0.716000000000001), METHOD.p = structure(c(1L, 
    2L, 2L, 2L), .Label = c("NONE", "NLMINB"), class = "factor"), 
        TIME.p = c(0, 0.00600000000000023, 0.0129999999999981, 
        0.0309999999999988), CV = c(4.44258707232128, 1.3448257559694, 
        0.885694975057761, 0.838088461472644)), .Names = c("H", 
    "METHOD.mu", "TIME.mu", "METHOD.p", "TIME.p", "CV"), row.names = c(NA, 
    4L), class = "data.frame")), .Names = c("CV", "mit", "summary"
))
test(id=53, code={
argv <- structure(list(x = structure(list(CV = c(4.44258707232128, 1.3448257559694, 
0.885694975057761, 0.838088461472644), mit = structure(list(p = structure(c(0.452549279246557, 
0.13386271764225, 0.267245510599797, 0.146342492511396), .Names = c("cmp1", 
"cmp2", "cmp3", "cmp4")), mu = structure(c(0.381966097098555, 
3.82765024730876, 1.80304880351015, 2.5878804906034, 2.61803339869107, 
0.203368399460934, 1.05601823938856, 0.0596409214659023), .Dim = c(4L, 
2L), .Dimnames = list(c("cmp1", "cmp2", "cmp3", "cmp4"), c("k1", 
"k2"))), Sigma = structure(c(0.22917975838358, 0.847714717429939, 
0.288537968483766, 0.738832302812549, -0.400000241640847, -0.0861897092187198, 
-0.100073467783835, -0.170562219060232, -0.400000241640847, -0.0861897092187198, 
-0.100073467783835, -0.170562219060232, 1.57082072508295, 0.0727738502834565, 
0.219785702621389, 0.217416957416503), .Dim = c(4L, 4L), .Dimnames = list(
    c("cmp1", "cmp2", "cmp3", "cmp4"), c("k1k1", "k1k2", "k2k1", 
    "k2k2"))), df = 1), .Names = c("p", "mu", "Sigma", "df")), 
    summary = structure(list(H = c(1, 2, 3, 4), METHOD.mu = structure(c(1L, 
    1L, 1L, 1L), .Label = "BFGS", class = "factor"), TIME.mu = c(1.301, 
    0.634, 1.148, 0.716000000000001), METHOD.p = structure(c(1L, 
    2L, 2L, 2L), .Label = c("NONE", "NLMINB"), class = "factor"), 
        TIME.p = c(0, 0.00600000000000023, 0.0129999999999981, 
        0.0309999999999988), CV = c(4.44258707232128, 1.3448257559694, 
        0.885694975057761, 0.838088461472644)), .Names = c("H", 
    "METHOD.mu", "TIME.mu", "METHOD.p", "TIME.p", "CV"), row.names = c(NA, 
    4L), class = "data.frame")), .Names = c("CV", "mit", "summary"
))), .Names = "x")
do.call('print', argv);
},  o = expected);