library(testthat)
library(ggdiagram)

# ── ob_path ───────────────────────────────────────────────────────────────────

# construction ----
test_that("ob_path construction", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  expect_no_error(ob_path(pts))
  expect_s3_class(ob_path(pts), "ggdiagram::ob_path")

  # minimum 2 points
  expect_no_error(ob_path(ob_point(c(0, 1), c(0, 1))))

  # list of ob_point
  pts2 <- ob_point(c(3, 4, 5), c(0, 1, 0))
  expect_no_error(ob_path(list(pts, pts2)))

  # Assignment errors
  p <- ob_path(pts)
  expect_error(p@p <- ob_circle(), "Control points must be an ob_point object or a list of ob_point objects.")
  expect_error(p@p <- list(1), "All items must be ggdiagram::ob_point.")
  expect_error(ob_path(ob_point(integer(0))), "A path cannot have 0 points.")
  expect_identical(p@midpoint(), ob_point(1,1))

  expect_true(is.na(p@midpoint(2)@x))


  expect_equal(ob_path(pts, label = letters[1:2])@length, 2L)

  p1 <- ob_path(
    pts,
    label = ob_label("a",
                     label.padding = ggplot2::margin(1, 1, 1, 1),
                     hjust = .45,
                     label.color = "blue"),
    arrowhead_length = 2
  )
  expect_no_error(as.geom(p1))


  p2 <- ob_path(
    pts,
    label = ob_label("a",
                     label.padding = ggplot2::margin(1, 1, 1, 1),
                     position = NA_real_,
                     hjust = .45,
                     label.color = "blue"),
    arrowhead_length = 2
  )
  expect_no_error(as.geom(p2))



  p3 <- ob_path(
    pts,
    label = ob_label("a",
                     # label.padding = ggplot2::margin(1, 1, 1, 1),
                     position = NA_real_,
                     label.color = "blue"),
    arrowhead_length = 2
  )
  expect_no_error(as.geom(p3))







})



# validation ----
test_that("ob_path validation: at least 2 points required", {
  expect_error(
    ob_path(ob_point(0, 0)),
    "at least 2 points"
  )
  expect_error(
    ob_path(ob_point(numeric(0), numeric(0))),
    "A path cannot have 0 points."
  )
})

# length ----
test_that("ob_path length", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  expect_equal(ob_path(pts)@length, 1)

  pts2 <- ob_point(c(3, 4, 5), c(0, 1, 0))
  expect_equal(ob_path(list(pts, pts2))@length, 2)
})

# p property ----
test_that("ob_path p property stores points correctly", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path <- ob_path(pts)
  # p is stored as a list
  expect_type(path@p, "list")
  expect_length(path@p, 1)
  expect_equal(path@p[[1]]@x, c(0, 1, 2))
  expect_equal(path@p[[1]]@y, c(0, 1, 0))
})

# style ----
test_that("ob_path style properties", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))

  path <- ob_path(pts, color = "red", linewidth = 2, alpha = 0.5, linetype = 2,
                  fill = "blue")
  expect_equal(path@color,     "red")
  expect_equal(path@linewidth, 2)
  expect_equal(path@alpha,     0.5)
  expect_equal(path@linetype,  2)
  expect_equal(path@fill,      "blue")

  sty <- path@style
  expect_s3_class(sty, "ggdiagram::ob_style")
  expect_equal(sty@color,     "red")
  expect_equal(sty@linewidth, 2)
})

test_that("ob_path style via ob_style argument", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path <- ob_path(pts, style = ob_style(color = "green", fill = "yellow"))
  expect_equal(path@color, "green")
  expect_equal(path@fill,  "yellow")
})

test_that("ob_path style setter", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path <- ob_path(pts)
  path@style <- ob_style(color = "purple", linewidth = 2)
  expect_equal(path@color,     "purple")
  expect_equal(path@linewidth, 2)
})

# arrow-specific style properties ----
test_that("ob_path arrow style properties", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))

  path <- ob_path(pts,
    resect       = 5,
    resect_head  = 3,
    resect_fins  = 2,
    length_head  = 8,
    length_fins  = 4,
    stroke_color = "black",
    stroke_width = 0.5
  )
  expect_equal(path@resect,       5)
  expect_equal(path@resect_head,  3)
  expect_equal(path@resect_fins,  2)
  expect_equal(path@length_head,  8)
  expect_equal(path@length_fins,  4)
  expect_equal(path@stroke_color, "black")
  expect_equal(path@stroke_width, 0.5)
})

# label_sloped ----
test_that("ob_path label_sloped", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  expect_true(ob_path(pts)@label_sloped)
  expect_false(ob_path(pts, label_sloped = FALSE)@label_sloped)
})

# label ----
test_that("ob_path label", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path_lbl <- ob_path(pts, label = "A")
  expect_s3_class(path_lbl@label, "ggdiagram::ob_label")
  expect_equal(path_lbl@label@label, "A")

  # no label by default
  expect_identical(ob_path(pts)@label, character(0))
})

# id ----
test_that("ob_path id", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path <- ob_path(pts, id = "mypath")
  expect_equal(path@id, "mypath")
})

# tibble ----
test_that("ob_path tibble", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  tib <- ob_path(pts)@tibble
  expect_true("p"     %in% colnames(tib))
  expect_true("group" %in% colnames(tib))
  expect_equal(nrow(tib), 1)

  # styles appear when set
  tib2 <- ob_path(pts, color = "red")@tibble
  expect_true("color" %in% colnames(tib2))
})

# get_tibble ----
test_that("ob_path get_tibble", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  gt <- get_tibble(ob_path(pts))
  expect_true("x"     %in% colnames(gt))
  expect_true("y"     %in% colnames(gt))
  expect_true("group" %in% colnames(gt))
  expect_equal(nrow(gt), 3)

  # styles appear in get_tibble
  gt2 <- get_tibble(ob_path(pts, color = "blue"))
  expect_true("color" %in% colnames(gt2))

  # two paths: rows sum
  pts2 <- ob_point(c(3, 4, 5), c(0, 1, 0))
  gt3 <- get_tibble(ob_path(list(pts, pts2)))
  expect_equal(nrow(gt3), 6)
})


test_that("ob_path bounding_box", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  bb <- ob_path(pts)@bounding_box
  expect_equal(bb@center@x, 1)
  expect_equal(bb@center@y, .5)
  expect_equal(bb@width, 2)
  expect_equal(bb@height, 1)
})

# segments ----
test_that("ob_path segments", {
  # 3-point path -> 2 segments
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path <- ob_path(pts)
  segs <- path@segment
  expect_s3_class(segs, "ggdiagram::ob_segment")
  expect_equal(segs@length, 2)
  expect_equal(segs@p1@x, c(0, 1))
  expect_equal(segs@p2@x, c(1, 2))

  # 2-point path -> 1 segment
  pts2 <- ob_point(c(0, 3), c(0, 4))
  segs2 <- ob_path(pts2)@segment
  expect_equal(segs2@length, 1)
  expect_equal(segs2@p1@x, 0)
  expect_equal(segs2@p2@x, 3)

  # segments inherit style
  segs_col <- ob_path(pts, color = "red")@segment
  expect_equal(segs_col@color, c("red", "red"))
})

# vertex_angle ----
test_that("ob_path vertex_angle: basic cases", {
  # straight path — interior vertex angle = 180°
  pts_str <- ob_point(c(0, 1, 2), c(0, 0, 0))
  expect_equal(ob_path(pts_str)@vertex_angle@degree, 180, tolerance = 1e-10)

  # right-angle turn at (1,0) going upward
  pts_rt <- ob_point(c(0, 1, 1), c(0, 0, 1))
  expect_equal(ob_path(pts_rt)@vertex_angle@degree, 90, tolerance = 1e-10)

  # 270-degree turn
  pts_270 <- ob_point(c(0, 1, 1), c(1, 1, 0))
  expect_equal(ob_path(pts_270)@vertex_angle@degree, 270, tolerance = 1e-10)
})

test_that("ob_path vertex_angle: 2-point path returns NULL", {
  pts2 <- ob_point(c(0, 1), c(0, 1))
  expect_null(ob_path(pts2)@vertex_angle)
})

test_that("ob_path vertex_angle: 4-point path has 2 interior vertices", {
  pts4 <- ob_point(c(0, 1, 2, 3), c(0, 1, 1, 0))
  va4 <- ob_path(pts4)@vertex_angle
  expect_s3_class(va4, "ggdiagram::ob_angle")
  expect_equal(length(va4@degree), 2)
})

test_that("ob_path vertex_angle: closed path includes closing vertex", {
  # closed rectangle: 4 right-angle turns
  pts_closed <- ob_point(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
  va_closed <- ob_path(pts_closed)@vertex_angle
  expect_equal(length(va_closed@degree), 4)
  expect_equal(va_closed@degree, c(90, 90, 90, 90), tolerance = 1e-6)
})

test_that("ob_path vertex_angle: multi-path returns list", {
  pts_a <- ob_point(c(0, 1, 1), c(0, 0, 1))  # 90-deg turn
  pts_b <- ob_point(c(0, 1, 1), c(1, 1, 0))  # 270-deg turn
  path2 <- ob_path(list(pts_a, pts_b))
  va <- path2@vertex_angle
  expect_type(va, "list")
  expect_length(va, 2)
  expect_equal(va[[1]]@degree, 90,  tolerance = 1e-6)
  expect_equal(va[[2]]@degree, 270, tolerance = 1e-6)
})

# geom ----
test_that("ob_path geom", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  expect_no_error(ob_path(pts)@geom())
})

# aesthetics ----
test_that("ob_path aesthetics", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  aes <- ob_path(pts)@aesthetics
  expect_s3_class(aes, "ggdiagram::class_aesthetics_list")
  expect_true("x"     %in% aes@required_aes)
  expect_true("y"     %in% aes@required_aes)
  expect_true("group" %in% aes@required_aes)
})

# subsetting by index ----
test_that("ob_path subsetting by index", {
  pts1 <- ob_point(c(0, 1, 2), c(0, 1, 0))
  pts2 <- ob_point(c(3, 4, 5), c(0, 1, 0))
  path_v <- ob_path(list(pts1, pts2), color = c("red", "blue"))

  sub1 <- path_v[1]
  expect_equal(sub1@length, 1)
  expect_equal(sub1@color,  "red")
  expect_equal(sub1@p[[1]]@x, c(0, 1, 2))

  sub2 <- path_v[2]
  expect_equal(sub2@color, "blue")
  expect_equal(sub2@p[[1]]@x, c(3, 4, 5))
})

# subsetting by id ----
test_that("ob_path subsetting by id", {
  pts1 <- ob_point(c(0, 1, 2), c(0, 1, 0))
  pts2 <- ob_point(c(3, 4, 5), c(0, 1, 0))
  path_id <- ob_path(list(pts1, pts2), id = c("a", "b"))

  sub_b <- path_id["b"]
  expect_equal(sub_b@id, "b")
  expect_equal(sub_b@p[[1]]@x, c(3, 4, 5))
})

# data2shape round-trip ----
test_that("ob_path data2shape round-trip", {
  pts <- ob_point(c(0, 1, 2), c(0, 1, 0))
  path <- ob_path(pts, color = "red", linewidth = 1.5)
  tib <- path@tibble
  path_rt <- data2shape(tib, ob_path)
  expect_equal(path_rt@color,     "red")
  expect_equal(path_rt@linewidth, 1.5)
  expect_equal(path_rt@length,    1)
})

# vectorised construction ----
test_that("ob_path vectorised construction", {
  pts1 <- ob_point(c(0, 1, 2), c(0, 1, 0))
  pts2 <- ob_point(c(3, 4, 5), c(0, 1, 0))
  pts3 <- ob_point(c(6, 7),    c(0, 1))
  path_v <- ob_path(list(pts1, pts2, pts3), color = c("red", "blue", "green"))
  expect_equal(path_v@length, 3)
  expect_equal(path_v@color,  c("red", "blue", "green"))
})
