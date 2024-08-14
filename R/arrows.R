ar_styles <- c(
  "alpha",
  "arrow_head",
  "arrow_fins",
  "arrowhead_length",
  "color",
  "length_head",
  "length_fins",
  "lineend",
  "linejoin",
  "linewidth",
  "linewidth_fins",
  "linewidth_head",
  "linetype",
  "resect",
  "resect_fins",
  "resect_head",
  "stroke_color",
  "stroke_width"
)

ar_props <- list(
  # primary ----
  primary = list(p1 = new_property(class = point),
                 p2 = new_property(class = point)),
  styles = style@properties[sg_styles],
  # derived ----
  derived = list(
    length = new_property(
      getter = function(self) {
        length(self@p1@x)
      }
    ),
    line = new_property(
      getter = function(self) {
        line(
          a = self@p1@y - self@p2@y,
          b = self@p2@x - self@p1@x,
          c = self@p1@x * self@p2@y - self@p2@x * self@p1@y,
          style = self@style
        )
      }
    ),
    style = new_property(
      getter = function(self) {
        pr <- purrr::map(sg_styles, prop, object = self) %>%
          `names<-`(sg_styles)
        rlang::inject(style(!!!get_non_empty_list(pr)))
      },
      setter = function(self, value) {
        segment(self@p1, self@p2, style = self@style + value)
      }
    ),
    tibble = new_property(
      getter = function(self) {
        d <- list(
          x = self@p1@x,
          y = self@p1@y,
          xend = self@p2@x,
          yend = self@p2@y,
          alpha = self@alpha,
          arrow_head = self@arrow_head,
          arrow_fins = self@arrow_fins,
          arrowhead_length = self@arrowhead_length,
          length_head = self@length_head,
          length_fins = self@length_fins,
          color = self@color,
          lineend = self@lineend,
          linejoin = self@linejoin,
          linewidth = self@linewidth,
          linewidth_fins = self@linewidth_fins,
          linewidth_head = self@linewidth_head,
          linetype = self@linetype,
          resect = self@resect,
          resect_fins = self@resect_fins,
          resect_head = self@resect_head,
          stroke_color = self@stroke_color,
          stroke_width = self@stroke_width
        )
        get_non_empty_tibble(d)
      }
    )
  ),
  # functions ----
  funs = list(
    geom = new_property(class_function, getter = function(self) {
      \(...) {
        as.geom(self, ...)
      }
    }),
    label = new_property(
      class_function,
      getter = function(self) {
        \(
          label,
          position = .5,
          # offset_angle  = self@angle + degree(90),
          # polar_multiplier = 1.2,
          # polar_just = polar(self@angle, r = multiplier),
          # hjust = NULL,
          vjust = class_missing,
          angle = self@line@angle,
          ...
        ) {


          label(
            p = midpoint(self, position = position),
            label = purrr::map_chr(label, \(l) ifelse(is.numeric(l),signs::signs(l, accuracy = .01),l)),
            vjust = vjust,
            angle = angle,
            ...
          )
        }

      }
    ),
    midpoint = new_property(
      class_function,
      getter = function(self) {
        \(position = .5, ...) midpoint(self, position = position, ...)
      }
    ),
    nudge = new_property(
      class_function,
      getter = function(self) {
        \(x = 0, y = 0) nudge(self, x, y)
      }
    )
    # arrow_segment = new_property(
    #   class_function,
    #   getter = function(self) {
    #     purrr::partial(arrow_segment, p1 = self, p2 = class_missing)
    #   }
    # )
  ),
  # info ----
  info = list(
    aesthetics = new_property(class_function, getter = function(self) {
      class_aesthetics_list(
        geom = ggarrow::geom_arrow_segment,
        mappable_bare = character(0),
        mappable_identity = c(
          "color",
          "linewidth",
          "linewidth_head",
          "linewidth_fins",
          "linetype",
          "arrow_head",
          "arrow_fins",
          "arrow_mid",
          "resect_head",
          "resect_fins",
          "alpha",
          "stroke_colour",
          "stroke_width"),
        not_mappable = c(
          "lineend",
          "linejoin",
          "arrow_head",
          'arrow_fins',
          "length",
          "length_head",
          "length_fins",
          "length_mid",
          "resect",
          "resect_fins",
          "resect_head",
          "linemitre"
        ),
        required_aes = c("x", "y", "xend", "yend"),
        omit_names = c("linejoin", "rule", "group"),
        inherit.aes = FALSE,
        style = sg_styles
      )
    })
  )
)
