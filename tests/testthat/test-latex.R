library(ggdiagram)
library(testthat)

# ── ob_latex ──────────────────────────────────────────────────────────────────
# ob_latex calls xelatex at construction time, so every test that needs a real
# object is guarded by `skip_if_not(latex_available, ...)`.
# A single shared base object (lx_base) is built here to avoid repeated LaTeX
# runs across property tests that don't require unique inputs.

latex_available <- nzchar(Sys.which("xelatex")) &&
  requireNamespace("pdftools",  quietly = TRUE) &&
  requireNamespace("magick",    quietly = TRUE) &&
  requireNamespace("janitor",   quietly = TRUE)



# construction ----
test_that("ob_latex: basic construction produces correct S7 class", {
  skip_if_not(latex_available, "xelatex not available")
  fn <- withr::local_tempfile()
  lx_base <- ob_latex("x^2", width = 1, filename = fn)
  expect_s3_class(lx_base, "ggdiagram::ob_latex")
  expect_equal(lx_base@hjust, 0.5) # default hjust / vjust are 0.5
  expect_equal(lx_base@vjust, 0.5)
  expect_equal(lx_base@angle@degree, 0) # angle is 0 degrees
  expect_equal(lx_base@density, 300) # density is 300
  expect_true(lx_base@math_mode) # math_mode is TRUE
  expect_identical(lx_base@fill, "white") # fill is 'white'
  expect_equal(lx_base@aspect_ratio, 1) # aspect_ratio is 1
  expect_true(lx_base@delete_files) #  delete_files TRUE
  expect_true(lx_base@force_recompile) # force_recompile TRUE
  expect_true(grepl("x^2", lx_base@tex, fixed = TRUE)) # @tex property contains the original equation
  expect_true(grepl("$x^2$", lx_base@tex, fixed = TRUE)) # @tex property wraps equation in math mode dollar signs
  expect_equal(lx_base@center@x, 0) # center defaults to (0, 0)
  expect_equal(lx_base@center@y, 0)
  expect_equal(lx_base@width, 1) # width is set to requested value
  expect_gt(lx_base@height, 0) # height is positive
  lx <- ob_latex("x", center = ob_point(3, 4), width = 1, filename = fn)
  expect_equal(lx@center@x, 3) # custom center is stored correctly
  expect_equal(lx@center@y, 4)
  lx2 <- ob_latex("x^2", width = 2, color = "red", fill = "grey80", filename = fn)
  expect_identical(lx2@color, "red") # color stored as given character string
  expect_identical(lx2@fill, "grey80") # fill stored as given character string
  lx2 <- ob_latex(
    "x^2",
    width = 2,
    filename = fn,
    latex_packages = "bm")

  expect_false(grepl("$Hello$", lx2@tex, fixed = TRUE))
  expect_equal(lx2@height / lx_base@height, 2, tolerance = 0.01)
  expect_no_error(as.geom(lx2)) # as.geom
  expect_no_error(update_ggplot(lx2, ggdiagram())) # update
  lx3 <- ob_latex("x", height = 2, math_mode = FALSE, family = "Times New Roman",
                  angle = 45, border = c(2,3,4,5), fill = class_color("grey80"), latex_packages = "bm")
  expect_false(lx3@math_mode) # math_mode = FALSE omits dollar signs from tex
  expect_equal(lx3@height, 2, filename = fn) # height-based sizing respected when width is omitted
  expect_equal(lx3@angle@degree, 45) # angle
  expect_identical(lx3@fill, "#CCCCCCFF")
  expect_true(grepl("\\usepackage\\{bm\\}", lx3@tex)) #latex_packages injected into tex document
  expect_type(lx_base@image, "list") # @image is a list of length 1 for a scalar input
  expect_length(lx_base@image, 1)
  r <- lx_base@rectangle
  expect_s3_class(r, "ggdiagram::ob_rectangle") # @rectangle getter returns an ob_rectangle
  expect_equal(r@center@x, lx_base@center@x, tolerance = 1e-6) # @rectangle center matches @center
  expect_equal(r@center@y, lx_base@center@y, tolerance = 1e-6)

  expect_equal(r@width,  lx_base@width) # @rectangle dimensions match @width and @height
  expect_equal(r@height, lx_base@height)
  lx <- lx_base
  lx@rectangle <- ob_rectangle(center = ob_point(5, 5), width = 3, height = 2, angle = degree(30)) # @rectangle setter updates center, width, height, angle
  expect_equal(lx@center@x, 5)
  expect_equal(lx@center@y, 5)
  expect_equal(lx@width,    3)
  expect_equal(lx@height,   2)
  expect_equal(lx@angle@degree, 30)
  lx <- lx_base
  expect_error(lx@rectangle <- ob_point(1, 1), "must be the output of the function `ob_rectangle`") # @rectangle setter errors on non-ob_rectangle input
  g <- as.geom(lx_base) # as.geom returns a list
  expect_type(g, "list")
  expect_length(g, 1)
  expect_no_error(ggdiagram() + lx_base) # ob_latex: + adds image layer to a ggplot
  expect_s3_class(ggdiagram() + lx_base, "ggplot")
  placed <- place(lx_base, ob_point(0, 0), where = "right", sep = 0.5) # place(ob_latex, ob_point) returns ob_latex
  expect_s3_class(placed, "ggdiagram::ob_latex")

  placed <- place(lx_base, ob_point(0, 0), where = "right", sep = 0.5)
  expect_gt(placed@center@x, 0) # place to the right shifts center rightward

  placed <- place(lx_base, ob_circle(), where = "right", sep = 0.5)
  expect_s3_class(placed, "ggdiagram::ob_latex") # place(ob_latex, centerpoint) returns ob_latex

})




