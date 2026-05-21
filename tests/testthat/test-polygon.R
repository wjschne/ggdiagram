library(testthat)
library(ggdiagram)

# ── ob_polygon ────────────────────────────────────────────────────────────────

# construction ----
test_that("ob_polygon construction", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  pg <- ob_polygon(pts)
  expect_no_error(pg@p <- pts)
  expect_no_error(pg@style <- ob_style(color = "red"))
  expect_no_error(ob_polygon(pts))
  expect_s3_class(ob_polygon(pts), "ggdiagram::ob_polygon")

  # list of ob_point
  pts2 <- ob_point(c(2, 3, 2.5), c(0, 0, 1))
  expect_no_error(ob_polygon(list(pts, pts2)))

  # at least 3 points required
  expect_error(
    ob_polygon(ob_point(c(0, 1), c(0, 1))),
    "needs at least 3"
  )
  pg <- ob_polygon(pts, label = ob_label("a"))
  expect_identical(pg@p, pg[1]@p)
  pg@p <- pts
  # setter makdes points into list
  expect_identical(list(pts), pg@p)
  expect_error(pg@vertex_radius <- c(2,2), "The vertex_radius property must be of length 1.")
  pg@style <- ob_style(color = "red")
  expect_identical(pg@color, "red")
  expect_identical(pg@point_at(-90), ob_point(.5,0))
  expect_identical(ob_polygon(pts, label = ob_label("a", fill = "red"))@label@fill, "red")
  expect_identical(ob_polygon(pts, label = ob_label(c("a", "b")))@length, 2L)

})

# length ----
test_that("ob_polygon length", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  expect_equal(ob_polygon(pts)@length, 1)

  pts2 <- ob_point(c(2, 3, 2.5), c(0, 0, 1))
  expect_equal(ob_polygon(list(pts, pts2))@length, 2)
})

# center ----
test_that("ob_polygon center", {
  pts <- ob_point(c(0, 2, 1), c(0, 0, 2))
  poly <- ob_polygon(pts)
  expect_equal(poly@center@x, mean(c(0, 2, 1)), tolerance = 1e-10)
  expect_equal(poly@center@y, mean(c(0, 0, 2)), tolerance = 1e-10)

  # two polygons: separate centroids
  pts2 <- ob_point(c(4, 6, 5), c(0, 0, 2))
  poly2 <- ob_polygon(list(pts, pts2))
  expect_equal(poly2@center@x, c(1, 5), tolerance = 1e-10)
  expect_equal(poly2@center@y, c(2/3, 2/3), tolerance = 1e-10)
})

# bounding_box ----
test_that("ob_polygon bounding_box", {
  pts <- ob_point(c(0, 2, 1), c(0, 0, 3))
  poly <- ob_polygon(pts)
  bb <- poly@bounding_box
  expect_s3_class(bb, "ggdiagram::ob_rectangle")
  expect_equal(bb@west@x,  0, tolerance = 1e-10)
  expect_equal(bb@east@x,  2, tolerance = 1e-10)
  expect_equal(bb@south@y, 0, tolerance = 1e-10)
  expect_equal(bb@north@y, 3, tolerance = 1e-10)
})

# segment ----
test_that("ob_polygon segment", {
  pts <- ob_point(c(0, 2, 1), c(0, 0, 2))
  poly <- ob_polygon(pts)
  s <- poly@segment
  expect_s3_class(s, "ggdiagram::ob_segment")

  # triangle -> 3 segments, closing loop: p1 starts at first vertex, p2 ends back at first vertex
  expect_equal(s@length, 3)
  expect_equal(s@p1@x[[1]], pts@x[[1]], tolerance = 1e-10)
  expect_equal(s@p2@x[[3]], pts@x[[1]], tolerance = 1e-10)
})

# style ----
test_that("ob_polygon style", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))

  poly <- ob_polygon(pts, color = "blue", fill = "red", linewidth = 2,
                     alpha = 0.5, linetype = 2)
  expect_equal(poly@color,     "blue")
  expect_equal(poly@fill,      "red")
  expect_equal(poly@linewidth, 2)
  expect_equal(poly@alpha,     0.5)
  expect_equal(poly@linetype,  2)

  sty <- poly@style
  expect_s3_class(sty, "ggdiagram::ob_style")
  expect_equal(sty@color, "blue")
  expect_equal(sty@fill,  "red")

  # style argument
  poly_s <- ob_polygon(pts, style = ob_style(color = "green", fill = "yellow"))
  expect_equal(poly_s@color, "green")
  expect_equal(poly_s@fill,  "yellow")

  poly_s@style <- ob_style(color = "blue")
  expect_identical(poly_s@color, "blue")
})

# label ----
test_that("ob_polygon label", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  poly_lbl <- ob_polygon(pts, label = "A")
  expect_s3_class(poly_lbl@label, "ggdiagram::ob_label")
  expect_equal(poly_lbl@label@label, "A")
  # label placed at centroid
  expect_equal(poly_lbl@label@center@x, mean(c(0, 1, 0.5)), tolerance = 1e-6)
  expect_equal(poly_lbl@label@center@y, mean(c(0, 0, 1)),   tolerance = 1e-6)

  # multiple polygons, multiple labels
  pts2 <- ob_point(c(2, 3, 2.5), c(0, 0, 1))
  poly_ml <- ob_polygon(list(pts, pts2), label = c("A", "B"))
  expect_equal(poly_ml@label@label, c("A", "B"))
  expect_equal(poly_ml@label@center@x, c(0.5, 2.5), tolerance = 1e-6)
})

# vertex_radius ----
test_that("ob_polygon vertex_radius", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  poly <- ob_polygon(pts, vertex_radius = 0.1)
  expect_equal(poly@vertex_radius, 0.1)

  # vertex_radius renamed to 'radius' in get_tibble
  gt <- get_tibble(poly)
  expect_true("radius" %in% colnames(gt))
  expect_false("vertex_radius" %in% colnames(gt))
})

# id ----
test_that("ob_polygon id", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  poly <- ob_polygon(pts, id = "my_tri")
  expect_equal(poly@id, "my_tri")
})

# tibble ----
test_that("ob_polygon tibble", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  tib <- ob_polygon(pts)@tibble
  expect_true("p"     %in% colnames(tib))
  expect_true("group" %in% colnames(tib))
  expect_equal(nrow(tib), 1)

  tib2 <- ob_polygon(pts, color = "blue", fill = "red")@tibble
  expect_true("color" %in% colnames(tib2))
  expect_true("fill"  %in% colnames(tib2))
})

# get_tibble ----
test_that("ob_polygon get_tibble", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  gt <- get_tibble(ob_polygon(pts))
  expect_true("x"     %in% colnames(gt))
  expect_true("y"     %in% colnames(gt))
  expect_true("group" %in% colnames(gt))
  # one polygon with 3 points
  expect_equal(nrow(gt), 3)

  # two polygons
  pts2 <- ob_point(c(2, 3, 2.5, 2), c(0, 0, 1, 1))
  gt2 <- get_tibble(ob_polygon(list(pts, pts2)))
  expect_equal(nrow(gt2), 7)
})

# geom ----
test_that("ob_polygon geom", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  expect_no_error(ob_polygon(pts)@geom())
  pg <- ob_polygon(pts, label = ob_label("a"))
  expect_no_error(as.geom(pg))
})

# polygon connect ----

test_that("ob_polygon connect", {
  pts <- ob_point(c(0, 1, 0.5), c(0, 0, 1))
  pg1 <- ob_polygon(pts)
  pg2 <- ob_polygon(pts + ob_point(3,0))
  expect_no_error(connect(pg1, pg2))

  expect_no_error(connect(ob_point(3,2), pg1))
  expect_no_error(connect(pg1, ob_point(3,2)))
  expect_no_error(connect(pg1, ob_circle(ob_point(3,2))))
  expect_no_error(connect(ob_circle(ob_point(3,2)), pg1))
})

# ── ob_ngon ───────────────────────────────────────────────────────────────────

# construction ----
test_that("ob_ngon construction", {
  expect_no_error(ob_ngon())
  expect_s3_class(ob_ngon(), "ggdiagram::ob_ngon")

  # default values
  ng <- ob_ngon()
  expect_equal(ng@n,      3)
  expect_equal(ng@radius, 1)
  expect_equal(ng@center@x, 0)
  expect_equal(ng@center@y, 0)
  expect_equal(ng@angle@turn, 0)

  # custom construction
  ng2 <- ob_ngon(center = ob_point(1, 2), n = 6, radius = 3)
  expect_equal(ng2@center@x, 1)
  expect_equal(ng2@center@y, 2)
  expect_equal(ng2@n,      6)
  expect_equal(ng2@radius, 3)

  expect_identical(ob_ngon(x = 3)@center@x, 3)
  expect_identical(ob_ngon(y = 3)@center@y, 3)
  ng3 <- ob_ngon(label = ob_label(letters[1:2]))
  expect_length(ng3, 2)
  expect_no_error(as.geom(ng3))
})

# x/y override ----
test_that("ob_ngon x/y override", {
  ng <- ob_ngon(x = 3, y = 4, n = 4, radius = 1)
  expect_equal(ng@center@x, 3)
  expect_equal(ng@center@y, 4)
})

# validation ----
test_that("ob_ngon validation: n >= 3", {
  expect_error(ob_ngon(n = 2), "at least three sides")
  expect_no_error(ob_ngon(n = 3))
})

# alternate constructors ----
test_that("ob_ngon alternate constructors", {
  # from side_length
  ng_sl <- ob_ngon(n = 4, side_length = sqrt(2))
  expect_equal(ng_sl@radius, 1, tolerance = 1e-10)

  # from apothem
  ng_ap <- ob_ngon(n = 4, apothem = cos(pi / 4))
  expect_equal(ng_ap@radius, 1, tolerance = 1e-10)

  # both apothem and side_length give same radius for a triangle
  sl_tri <- 2 * sin(pi / 3)  # side_length of equilateral triangle with radius 1
  ng_sl3 <- ob_ngon(n = 3, side_length = sl_tri)
  expect_equal(ng_sl3@radius, 1, tolerance = 1e-10)
})

# angle rotation ----
test_that("ob_ngon angle rotation", {
  ng <- ob_ngon(n = 4, radius = 1, angle = 45)
  expect_equal(ng@angle@degree, 45, tolerance = 1e-10)

  # angle stored as turn
  ng_turn <- ob_ngon(n = 4, radius = 1, angle = degree(90))
  expect_equal(ng_turn@angle@turn, 0.25, tolerance = 1e-10)
})

# length ----
test_that("ob_ngon length", {
  expect_equal(ob_ngon()@length, 1)
  ng_v <- ob_ngon(center = ob_point(c(0, 1, 2), c(0, 0, 0)), n = c(3, 4, 5), radius = 1)
  expect_equal(ng_v@length, 3)
})

# derived: side_length, apothem, area, perimeter ----
test_that("ob_ngon derived values", {
  # equilateral triangle, radius = 1
  tri <- ob_ngon(n = 3, radius = 1)
  expect_equal(tri@side_length, 2 * sin(pi / 3),       tolerance = 1e-10)
  expect_equal(tri@apothem,     cos(pi / 3),            tolerance = 1e-10)
  expect_equal(tri@area,
    3 * tri@side_length * tri@apothem / 2,              tolerance = 1e-10)
  expect_equal(tri@perimeter,   3 * tri@side_length,    tolerance = 1e-10)

  # square, radius = 1
  sq <- ob_ngon(n = 4, radius = 1)
  expect_equal(sq@side_length, sqrt(2),                 tolerance = 1e-10)
  expect_equal(sq@apothem,     cos(pi / 4),             tolerance = 1e-10)
  expect_equal(sq@area,        2,                        tolerance = 1e-10)
  expect_equal(sq@perimeter,   4 * sqrt(2),             tolerance = 1e-10)
})

# vertices ----
test_that("ob_ngon vertices", {
  # square at angle = 0: vertices at (1,0), (0,1), (-1,0), (0,-1)
  sq <- ob_ngon(n = 4, radius = 1, angle = 0)
  v <- sq@vertices
  expect_s3_class(v, "ggdiagram::ob_point")
  expect_equal(v@length, 4)
  expect_equal(sort(round(v@x, 6)), c(-1, 0, 0, 1))
  expect_equal(sort(round(v@y, 6)), c(-1, 0, 0, 1))

  # all vertices at distance = radius from center
  dists <- sqrt(v@x^2 + v@y^2)
  expect_equal(dists, rep(1, 4), tolerance = 1e-10)
})

# segments ----
test_that("ob_ngon segments", {
  sq <- ob_ngon(n = 4, radius = 1)
  segs <- sq@segment
  expect_s3_class(segs, "ggdiagram::ob_segment")
  expect_equal(segs@length, 4)
})

# circumscribed / inscribed ----
test_that("ob_ngon circumscribed and inscribed circles", {
  sq <- ob_ngon(n = 4, radius = 1)
  expect_s3_class(sq@circumscribed, "ggdiagram::ob_circle")
  expect_equal(sq@circumscribed@radius, 1, tolerance = 1e-10)

  expect_s3_class(sq@inscribed, "ggdiagram::ob_circle")
  expect_equal(sq@inscribed@radius, cos(pi / 4), tolerance = 1e-10)

  # inscribed radius < circumscribed radius
  expect_lt(sq@inscribed@radius, sq@circumscribed@radius)
})

# compass points ----
test_that("ob_ngon compass points", {
  # square at 45 deg: vertices at (±√2/2, ±√2/2)
  # compass points are point_at(0/90/180/270)
  sq45 <- ob_ngon(n = 4, radius = 1, angle = 45)
  expect_equal(sq45@north@y, sq45@south@y * -1, tolerance = 1e-6)
  expect_equal(sq45@east@x,  sq45@west@x  * -1, tolerance = 1e-6)
  expect_equal(sq45@north@x, 0,                  tolerance = 1e-6)
  expect_equal(sq45@east@y,  0,                  tolerance = 1e-6)
})

# bounding_box ----
test_that("ob_ngon bounding_box", {
  sq45 <- ob_ngon(n = 4, radius = 1, angle = 45)
  bb <- sq45@bounding_box
  expect_s3_class(bb, "ggdiagram::ob_rectangle")
  hw <- sqrt(2) / 2
  expect_equal(bb@west@x,  -hw, tolerance = 1e-6)
  expect_equal(bb@east@x,   hw, tolerance = 1e-6)
  expect_equal(bb@south@y, -hw, tolerance = 1e-6)
  expect_equal(bb@north@y,  hw, tolerance = 1e-6)
})

# point_at ----
test_that("ob_ngon point_at", {
  sq <- ob_ngon(n = 4, radius = 1, angle = 0)
  # at angle 0: right-most midpoint = (1, 0) for n=4 r=1
  p0 <- sq@point_at(degree(0))
  expect_equal(p0@x, 1, tolerance = 1e-6)
  expect_equal(p0@y, 0, tolerance = 1e-6)

  # hexagon at 30 deg (edge midpoint): r_poly = apothem = cos(pi/6)
  # point = (cos(pi/6) * cos(pi/6), cos(pi/6) * sin(pi/6))
  hex <- ob_ngon(n = 6, radius = 1, angle = 0)
  p30 <- hex@point_at(degree(30))
  r30 <- cos(pi / 6)
  expect_equal(p30@x, r30 * cos(pi / 6), tolerance = 1e-6)
  expect_equal(p30@y, r30 * sin(pi / 6), tolerance = 1e-6)
})

# angle_at ----
test_that("ob_ngon angle_at", {
  sq <- ob_ngon(n = 4, radius = 1, angle = 0)
  expect_equal(sq@angle_at(ob_point(1, 0))@degree,  0,  tolerance = 1e-10)
  expect_equal(sq@angle_at(ob_point(0, 1))@degree,  90, tolerance = 1e-10)
  expect_equal(sq@angle_at(ob_point(-1, 0))@degree, 180, tolerance = 1e-10)



})

# normal_at ----
test_that("ob_ngon normal_at", {
  sq <- ob_ngon(n = 4, radius = 1, angle = 0, color = "black", fill = NA)
  # normal at 0 deg: point on edge + unit vector away from center
  n_pt <- sq@normal_at(degree(45))
  expect_s3_class(n_pt, "ggdiagram::ob_point")
  # should be to the right of the polygon boundary
  expect_gt(n_pt@x, 1 - 1e-6)
  p <- ob_point(3, 0)

  expect_s3_class(sq@normal_at(p), "ggdiagram::ob_point")
  expect_identical(sq@normal_at(degree(45)), sq@normal_at(45))

  expect_true(S7::S7_inherits(sq@tangent_at(p)), ob_line)
  expect_true(S7::S7_inherits(sq@tangent_at(45)), ob_line)
})

# tangent_at ----
test_that("ob_ngon tangent_at", {
  sq <- ob_ngon(n = 4, radius = 1, angle = 0)
  tl <- sq@tangent_at(degree(0))
  expect_s3_class(tl, "ggdiagram::ob_line")
  # tangent at east midpoint of a square is vertical (slope undefined / infinite)
  # i.e., the line should have near-zero x-coefficient (nearly vertical)
})

# style ----
test_that("ob_ngon style", {
  ng <- ob_ngon(n = 5, radius = 2, color = "red", fill = "blue",
                linewidth = 1.5, alpha = 0.7, linetype = 3)
  expect_equal(ng@color,     "red")
  expect_equal(ng@fill,      "blue")
  expect_equal(ng@linewidth, 1.5)
  expect_equal(ng@alpha,     0.7)
  expect_equal(ng@linetype,  3)

  sty <- ng@style
  expect_s3_class(sty, "ggdiagram::ob_style")
  expect_equal(sty@color, "red")
  expect_equal(sty@fill,  "blue")

  ng@style <- ob_style(color = "green")
  expect_identical(ng@color, "green")
})

# label ----
test_that("ob_ngon label", {
  ng <- ob_ngon(n = 4, radius = 1, label = "Q")
  expect_s3_class(ng@label, "ggdiagram::ob_label")
  expect_equal(ng@label@label, "Q")
  # label placed at centroid (center)
  expect_equal(ng@label@center@x, ng@center@x, tolerance = 1e-6)
  expect_equal(ng@label@center@y, ng@center@y, tolerance = 1e-6)
})

# tibble ----
test_that("ob_ngon tibble", {
  tib <- ob_ngon(n = 4, radius = 2)@tibble
  expect_true("x"      %in% colnames(tib))
  expect_true("y"      %in% colnames(tib))
  expect_true("n"      %in% colnames(tib))
  expect_true("radius" %in% colnames(tib))
  expect_true("angle"  %in% colnames(tib))
})

# get_tibble ----
test_that("ob_ngon get_tibble", {
  gt <- get_tibble(ob_ngon(n = 4, radius = 1))
  expect_true("x"     %in% colnames(gt))
  expect_true("y"     %in% colnames(gt))
  expect_true("group" %in% colnames(gt))
  # n+1 rows per polygon (close the loop)
  expect_equal(nrow(gt), 5)

  # with styles
  gt2 <- get_tibble(ob_ngon(n = 3, radius = 2, color = "red", fill = "blue"))
  expect_true("color" %in% colnames(gt2))
  expect_true("fill"  %in% colnames(gt2))

  # vertex_radius renamed to 'radius'
  gt3 <- get_tibble(ob_ngon(n = 3, radius = 1, vertex_radius = 0.05))
  expect_true("radius" %in% colnames(gt3))
})

# geom ----
test_that("ob_ngon geom", {
  expect_no_error(ob_ngon()@geom())
})

# subsetting ----
test_that("ob_ngon subsetting by index", {
  ng_v <- ob_ngon(
    center = ob_point(c(0, 1, 2), c(0, 0, 0)),
    n      = c(3, 4, 5),
    radius = 1
  )
  sub <- ng_v[2]
  expect_equal(sub@length, 1)
  expect_equal(sub@n,      4)
  expect_equal(sub@center@x, 1)
})

test_that("ob_ngon subsetting by id", {
  ng_id <- ob_ngon(
    center = ob_point(c(0, 1, 2), c(0, 0, 0)),
    n      = c(3, 4, 5),
    radius = 1,
    id     = c("a", "b", "c")
  )
  sub <- ng_id["b"]
  expect_equal(sub@n, 4)
  expect_equal(sub@id, "b")
})

# data2shape round-trip ----
test_that("ob_ngon data2shape round-trip", {
  ng <- ob_ngon(n = 5, radius = 2, color = "red")
  tib <- ng@tibble
  ng_rt <- data2shape(tib, ob_ngon)
  expect_equal(ng_rt@n,      ng@n)
  expect_equal(ng_rt@radius, ng@radius, tolerance = 1e-10)
  expect_equal(ng_rt@color,  ng@color)
})

# vectorised construction ----
test_that("ob_ngon vectorised construction", {
  ng_v <- ob_ngon(
    center = ob_point(c(0, 2, 4), c(0, 0, 0)),
    n      = c(3, 4, 5),
    radius = c(1, 1.5, 2)
  )
  expect_equal(ng_v@length, 3)
  expect_equal(ng_v@n,      c(3, 4, 5))
  expect_equal(ng_v@radius, c(1, 1.5, 2))
  expect_equal(ng_v@center@x, c(0, 2, 4))
})

# connect ngons

test_that("ob_ngon connection", {
  ng1 <- ob_ngon(n = 4)
  ng2 <- ob_ngon(x = 5, n = 6)
  expect_identical(connect(ng1, ng2)@p2@x, 4)
  expect_identical(connect(ob_point(1,0), ng2)@p2@x, 4)
  expect_identical(connect(ng1, ob_point(4,0))@p1@x, 1)
  expect_identical(connect(ng1, ob_circle(x = 4, y = 0))@p2@x, 3)
  expect_identical(connect(ob_circle(x = -4, y = 0), ng1)@p2@x, -1)
})


# ── ob_intercept ──────────────────────────────────────────────────────────────

# construction ----
test_that("ob_intercept construction", {
  expect_no_error(ob_intercept())
  expect_s3_class(ob_intercept(), "ggdiagram::ob_intercept")

  # defaults
  ic <- ob_intercept()
  expect_equal(ic@center@x, 0)
  expect_equal(ic@center@y, 0)
  expect_equal(ic@width,    1)

  # custom
  ic2 <- ob_intercept(center = ob_point(1, 2), width = 2)
  expect_equal(ic2@center@x, 1)
  expect_equal(ic2@center@y, 2)
  expect_equal(ic2@width,    2)

  # x/y shortcut
  ic3 <- ob_intercept(x = 3, y = 4)
  expect_equal(ic3@center@x, 3)
  expect_equal(ic3@center@y, 4)

  ic4 <- ob_intercept(x = 3)
  expect_equal(ic4@center@y, 0)

  ic5 <- ob_intercept(y = 3, label = ob_label("a"))
  expect_equal(ic5@center@x, 0)

  ic6 <- ob_intercept(y = 3, label = ob_label(c("a", "b")))
  expect_identical(ic6@length, 2L)

})

# equilateral triangle geometry ----
test_that("ob_intercept vertex geometry", {
  ic <- ob_intercept(width = 1)
  r_expected <- 0.5 / cos(pi / 6)  # width/2 / cos(30°)

  # all three vertices equidistant from center
  d_top   <- sqrt(ic@top@x^2   + ic@top@y^2)
  d_left  <- sqrt(ic@left@x^2  + ic@left@y^2)
  d_right <- sqrt(ic@right@x^2 + ic@right@y^2)
  expect_equal(d_top,   r_expected, tolerance = 1e-6)
  expect_equal(d_left,  r_expected, tolerance = 1e-6)
  expect_equal(d_right, r_expected, tolerance = 1e-6)

  # top vertex at (0, r), left/right symmetric about y-axis
  expect_equal(ic@top@x,   0,   tolerance = 1e-6)
  expect_equal(ic@top@y,   r_expected, tolerance = 1e-6)
  expect_equal(ic@left@x,  -ic@right@x, tolerance = 1e-6)
  expect_equal(ic@left@y,   ic@right@y, tolerance = 1e-6)
})

# polygon property ----
test_that("ob_intercept polygon property", {
  ic <- ob_intercept()
  poly <- ic@polygon
  expect_s3_class(poly, "ggdiagram::ob_polygon")
  expect_equal(poly@length, 1)
})

# bounding_box ----
test_that("ob_intercept bounding_box", {
  ic <- ob_intercept(width = 1)
  bb <- ic@bounding_box
  expect_s3_class(bb, "ggdiagram::ob_rectangle")
  expect_equal(bb@west@x,  -0.5,                 tolerance = 1e-6)
  expect_equal(bb@east@x,   0.5,                 tolerance = 1e-6)
  expect_equal(bb@south@y, ic@left@y,             tolerance = 1e-6)
  expect_equal(bb@north@y, 0.5 / cos(pi / 6),    tolerance = 1e-6)
})

# style ----
test_that("ob_intercept style", {
  ic <- ob_intercept(color = "blue", fill = "red", linewidth = 1.2)
  expect_equal(ic@color,     "blue")
  expect_equal(ic@fill,      "red")
  expect_equal(ic@linewidth, 1.2)
})

# label ----
test_that("ob_intercept label", {
  ic <- ob_intercept(label = "T")
  expect_s3_class(ic@label, "ggdiagram::ob_label")
  expect_equal(ic@label@label, "T")
})

# geom ----
test_that("ob_intercept geom", {
  expect_no_error(ob_intercept()@geom())
})


# ── ob_reuleaux ───────────────────────────────────────────────────────────────

# construction ----
test_that("ob_reuleaux construction", {
  expect_no_error(ob_reuleaux())
  expect_s3_class(ob_reuleaux(), "ggdiagram::ob_reuleaux")

  # defaults
  rel <- ob_reuleaux()
  expect_equal(rel@n,      5L)
  expect_equal(rel@radius, 1)
  expect_equal(rel@center@x, 0)
  expect_equal(rel@center@y, 0)
  # default angle is 90 degrees
  expect_equal(rel@angle@degree, 90, tolerance = 1e-10)

  # custom
  rel2 <- ob_reuleaux(n = 3, radius = 2)

  expect_equal(rel2@n,      3L)
  expect_equal(rel2@radius, 2)
  expect_error(ob_reuleaux(vertex_radius = c(1,2)), "The vertex_radius property must be of length 1.")
  expect_no_error(get_tibble(ob_reuleaux(vertex_radius = 1)))

  expect_no_error(as.geom(ob_reuleaux()))
})

# angles ----
test_that("ob_reuleaux central and inscribed angles", {
  rel5 <- ob_reuleaux(n = 5)
  expect_equal(rel5@central_angle@degree, 360 / 5, tolerance = 1e-10)
  expect_equal(rel5@inscribed_angle@degree, 180 / 5, tolerance = 1e-10)

  rel3 <- ob_reuleaux(n = 3)
  expect_equal(rel3@central_angle@degree, 120, tolerance = 1e-10)
  expect_equal(rel3@inscribed_angle@degree, 60, tolerance = 1e-10)

  expect_equal(rel3@angle_at(ob_point(0, 3)), radian(0.5 * pi))
  expect_no_error(rel3@normal_at(30))
  expect_no_error(rel3@normal_at(ob_point(3,3)))
  expect_no_error(rel3@normal_at(degree(30)))
  expect_equal(rel3@point_at(90), ob_point(0, 1), tolerance = 1e-06)

  rel4 <- ob_reuleaux(n = 3, label = ob_label(c("hello", "goodbye")), vertex_radius = 2, angle = -10)
  expect_identical(rel4@label@label, c("hello", "goodbye"))

})

# arcs ----
test_that("ob_reuleaux arcs", {
  rel <- ob_reuleaux(n = 5)
  arcs <- rel@arcs
  expect_s3_class(arcs, "ggdiagram::ob_arc")
  expect_equal(arcs@length, 5)

  rel3 <- ob_reuleaux(n = 3)
  expect_equal(rel3@arcs@length, 3)

  rel@arc_at(60)
})

# arc_radius ----
test_that("ob_reuleaux arc_radius", {
  # equilateral triangle: chord between two vertices is sqrt(3) * radius (for radius=1)
  rel3 <- ob_reuleaux(n = 3, radius = 1)
  expect_equal(rel3@arc_radius, sqrt(3), tolerance = 1e-6)
})

# chord_length ----
test_that("ob_reuleaux chord_length", {
  rel5 <- ob_reuleaux(n = 5, radius = 1)
  expected <- sqrt(2 * 1^2 * (1 - cos(rel5@inscribed_angle)))
  expect_equal(rel5@chord_length, expected, tolerance = 1e-6)

  # pentagon chord should equal phi - 1 = 0.618... (golden ratio minus 1)
  expect_equal(rel5@chord_length, (sqrt(5) - 1) / 2, tolerance = 1e-4)
})

# circumscribed ----
test_that("ob_reuleaux circumscribed", {
  rel <- ob_reuleaux(n = 5, radius = 2)
  circ <- rel@circumscribed
  expect_s3_class(circ, "ggdiagram::ob_circle")
  expect_equal(circ@radius, 2, tolerance = 1e-10)
})

# inscribed ----
test_that("ob_reuleaux inscribed", {
  rel <- ob_reuleaux(n = 5, radius = 1)
  ins <- rel@inscribed
  expect_s3_class(ins, "ggdiagram::ob_circle")
  # inscribed radius < radius
  expect_lt(ins@radius, rel@radius)
  # n=5: inscribed radius ≈ 0.902
  expect_equal(ins@radius, 0.902113, tolerance = 1e-4)
})

# circumference ----
test_that("ob_reuleaux circumference", {
  rel5 <- ob_reuleaux(n = 5, radius = 1)
  expect_gt(rel5@circumference, 0)
  # circumference = n * arc_length_per_arc
  expect_equal(rel5@circumference, sum(rel5@arcs@arc_length), tolerance = 1e-6)
})

# style ----
test_that("ob_reuleaux style", {
  rel <- ob_reuleaux(color = "blue", fill = "red", linewidth = 1.5)
  expect_equal(rel@color,     "blue")
  expect_equal(rel@fill,      "red")
  expect_equal(rel@linewidth, 1.5)

  sty <- rel@style
  expect_s3_class(sty, "ggdiagram::ob_style")
  expect_equal(sty@color, "blue")
})

# bounding_box ----
test_that("ob_reuleaux bounding_box", {
  rel <- ob_reuleaux(n = 5, radius = 1)
  bb <- rel@bounding_box
  expect_s3_class(bb, "ggdiagram::ob_rectangle")
  # bounding box must contain the shape
  expect_lte(bb@west@x,  -0.9)
  expect_gte(bb@east@x,   0.9)
})

# geom ----
test_that("ob_reuleaux geom via as.geom", {
  expect_no_error(as.geom(ob_reuleaux(label = ob_label("hello"))))
  expect_no_error(ob_reuleaux()@geom())
})
