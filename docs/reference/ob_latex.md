# ob_latex class

make a latex equation

## Usage

``` r
ob_latex(
  tex = character(0),
  center = ob_point(0, 0),
  width = numeric(0),
  height = numeric(0),
  hjust = 0.5,
  vjust = 0.5,
  angle = 0,
  aspect_ratio = 1,
  border = numeric(0),
  family = character(0),
  math_mode = TRUE,
  filename = character(0),
  color = character(0),
  fill = "white",
  density = 300,
  latex_packages = character(0),
  preamble = character(0),
  force_recompile = TRUE,
  delete_files = TRUE,
  id = character(0)
)
```

## Arguments

- tex:

  LaTeX equation

- center:

  An
  [ob_point](https://wjschne.github.io/ggdiagram/reference/ob_point.md)

- width:

  width (specify width or height but not both)

- height:

  height (specify width or height but not both)

- hjust:

  horizontal adjustment. 0 means left justified, 1 means right
  justified, 0.5 means centered

- vjust:

  vertical justification. 0 means bottom aligned, 1 means top aligned,
  0.5 means vertically centered

- angle:

  angle of text

- aspect_ratio:

  alters the aspect ratio of the image

- border:

  border space (in points) around image

- family:

  font family (installed on system) of plain text

- math_mode:

  include dollar signs automatically. Set to `FALSE` when the latex
  command is not in math mode

- filename:

  bare file name without extension (e.g., `myequation`)

- color:

  set color of equation text

- fill:

  set color of background rectangle

- density:

  image quality (dots per inch)

- latex_packages:

  load latex packages

- preamble:

  additional latex commands to load in preamble

- force_recompile:

  Will re-run xelatex even if .pdf file exists already

- delete_files:

  Delete .tex and .pdf files after image is generated.

- id:

  character string to identify object

## Value

ob_latex object

## Additional properties

- `@rectangle`:

  gets or sets rectangle that contains the image

- `@image`:

  raster bitmap
