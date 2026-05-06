library(testthat)
library(ggdiagram)

test_that(desc = "does set_label_x work on arcs, segments, and bezier curves?", {
a <- ob_point()
b <- ob_point(1,c(0,1))
# segment
ab_seg <- connect(a, b, label = c("1","2"))@set_label_x(.2)
expect_all_equal(ab_seg@label@center@x, ab_seg@label@center@x[1])

# arc
ab_arc <- connect(a, b, label = c("1","2"), arc_bend = .5)@set_label_x(.2)
expect_all_equal(ab_arc@label@center@x, ab_arc@label@center@x[1])

# bezier
ab_bez <- connect(a, b, label = c("1","2"), to_offset = ob_point(1, 1))@set_label_x(.2)
expect_all_equal(ab_bez@label@center@x, ab_bez@label@center@x[1])
})
