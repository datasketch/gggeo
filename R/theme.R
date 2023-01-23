

theme_nothing <- function (base_size = 12, legend = TRUE) {
  if (legend) {
    theme(axis.text = element_blank(), axis.title = element_blank(),
          panel.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.ticks.length = unit(0,
                                                                       "cm"), panel.spacing = unit(0, "lines"), plot.margin = unit(c(0,
                                                                                                                                     0, 0, 0), "lines"))
  }
  else {
    theme(line = element_blank(), rect = element_blank(),
          text = element_blank(), axis.ticks.length = unit(0,
                                                           "cm"), legend.position = "none", panel.spacing = unit(0,
                                                                                                                 "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines"))
  }
}


gggeo_theme <- function(){

  opts <- dsvizopts::dsviz_default_opts()

  thm <- opts

  theme(
    line = element_line(
      colour = thm$line_colour,
      size = thm$line_size,
      linetype = 1,
      lineend = "butt"),
    rect = element_rect(
      fill = "white",
      colour = thm$rect_colour,
      size = 0.5,
      linetype = 1),
    text = element_text(
      debug=FALSE,
      margin=margin(),
      family = '',
      face = "plain",
      colour = thm$text_colour,
      size = thm$text_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 1.2),
    axis.text = element_text(
      debug=FALSE,
      margin=margin(6, 0, 6, 0),
      size = rel(0.8),
      colour = thm$axis_text_color),
    axis.line = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(
      colour = thm$legend_background_colour,
      fill = thm$legend_background_fill),
    #legend.margin = #grid::unit(0.2 * spacing, "cm"),
    legend.key = element_rect(
      colour = thm$legend_key_colour,
      fill = thm$legend_key_fill),
    legend.key.size = grid::unit( 1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(
      debug=FALSE,
      colour = thm$legend_text_colour,
      margin=margin(),
      size = rel(0.8)),
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(
      fill = thm$panel_background_fill,
      colour = NA),
    panel.border = element_rect(
      size = thm$panel_border_size,
      fill = 'transparent',
      colour = thm$panel_border_colour),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major = element_line(
      size = thm$grid_size,
      linetype = grid_line_type,
      colour = thm$grid_color),
    panel.grid.minor = element_line(
      linetype= thm$grid_line_type,
      size = thm$grid_size,
      colour = thm$grid_color),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    panel.ontop = FALSE,
    strip.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    strip.background = element_rect(
      fill = thm$strip_background_fill,
      colour = NA),
    strip.text.x = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(1),
      face = 'plain'),
    strip.text.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = -90,
      face = 'plain',
      size = rel(1)),
    strip.switch.pad.grid = grid::unit(0, 'cm'),
    strip.switch.pad.wrap = grid::unit(0, 'cm'),
    plot.background = element_rect(
      colour = thm$plot_background_colour,
      fill = thm$plot_background_fill),
    plot.title.position = "plot",
    plot.title = element_text(#ggtext::element_textbox_simple(
      # plot.title = ggtext::element_markdown(
      debug=FALSE,
      family = thm$plot_title_family,
      colour = thm$plot_title_colour,
      margin=margin(10, 0, title_bottom, 0),
      size = rel(1.2),
      hjust = thm$title_align,
      vjust = 1,
      face='plain'),
    plot.subtitle = element_text(
      debug=FALSE,
      family = thm$plot_subtitle_family,
      colour = thm$plot_subtitle_colour,
      margin=margin(3, 0, subtitle_bottom, 0),
      size = rel(1),
      hjust = thm$subtitle_align,
      vjust = 1,
      face='plain'),
    plot.caption.position = "plot",
    plot.caption = ggtext::element_markdown(
      debug=FALSE,
      family = thm$plot_subtitle_family,
      colour = thm$plot_caption_colour,
      margin=margin(30, 30, 5, 15),
      size = rel(0.8),
      hjust = thm$plot_caption_hjust,
      vjust = 1,
      valign = 0.5,
      align_heights = TRUE,
      lineheight = 1.5,
      face='plain'),
    plot.margin = margin(10, 0, plot_margin_bottom+15, 0),
    complete = TRUE
  )
}
