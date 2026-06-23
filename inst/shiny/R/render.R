# Figure-building internals. The public API is invert_report().

#' Wrap text to a width, returning a single string with embedded "\n"
#'
#' Drop-in replacement for stringr::str_wrap using base strwrap.
#' @keywords internal
.wrap <- function(x, width = 50) {
  if (length(x) == 0) return(character(0))
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return("")
    paste(strwrap(s, width = width), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

#' Apply alpha to a hex colour by blending with white
#'
#' Drop-in replacement for scales::alpha for the white-bg case used here.
#' @keywords internal
.alpha_on_white <- function(hex, alpha) {
  rgb_vals <- grDevices::col2rgb(hex)[, 1] / 255
  blended <- rgb_vals * alpha + (1 - alpha)
  grDevices::rgb(blended[1], blended[2], blended[3])
}


# Vertical budgets, all expressed in "body line-heights" so the
# whole figure scales together. One body line ~= 0.18" in the saved
# figure (see save_report()). Header/footer/strip text is larger than
# body text, so their line budgets are bigger than they look on paper.
# Card budgets are tuned so the patchwork allocation matches the actual
# rendered title + content height; over-budgeting creates trailing
# whitespace inside each card.
.ITEM_GAP_LINES <- 0.45   # breathing room between items within a card
.TITLE_LINES    <- 1.6    # vertical share of each card's domain title
.STRIP_LINES    <- 1.5    # the "WELFARE DOMAINS" / "ESSENTIALS" strip
.HEADER_LINES   <- 4.5    # paper title + authors + framework version
.FOOTER_LINES   <- 1.2    # legend (single line)


#' Count wrapped lines in a string produced by .wrap()
#'
#' Empty strings count as one line (a single placeholder line).
#' @keywords internal
.count_lines <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) 1L
    else as.integer(length(strsplit(s, "\n", fixed = TRUE)[[1]]))
  }, integer(1), USE.NAMES = FALSE)
}


#' Compute per-card content geometry
#'
#' Returns a data frame with one row per item plus attributes describing
#' the card's total line budget. Used both for laying out items inside
#' a card and for sizing cards in the patchwork stack.
#'
#' @keywords internal
.card_geometry <- function(domain_name, data, value_wrap = 75) {
  d <- data[data$domain == domain_name, , drop = FALSE]
  if (nrow(d) == 0) {
    return(structure(d, total_lines = 0, n_items = 0))
  }
  d <- d[order(d$order), , drop = FALSE]
  d$wrapped_value <- .wrap(d$display_value, width = value_wrap)
  d$value_lines   <- .count_lines(d$wrapped_value)
  d$item_lines    <- d$value_lines + 1L          # +1 for the bold label
  total_lines     <- sum(d$item_lines) +
                     max(0, nrow(d) - 1) * .ITEM_GAP_LINES
  structure(d, total_lines = total_lines, n_items = nrow(d))
}


#' Build the per-domain "card" ggplot
#' @keywords internal
.make_card <- function(domain_name, data, accent, value_wrap = 75) {
  d <- .card_geometry(domain_name, data, value_wrap = value_wrap)
  if (nrow(d) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  total_lines <- attr(d, "total_lines")

  # Markdown content rendered via ggtext::geom_richtext:
  # bold item label on first line; value below.
  bold_label <- paste0(
    "<span style='color:", .palette$label,
    ";font-size:7.4pt;'><b>", d$item, "</b></span>"
  )
  value_html <- gsub("\n", "<br/>", d$wrapped_value, fixed = TRUE)
  value_face_open  <- ifelse(d$face == "italic", "<i>", "")
  value_face_close <- ifelse(d$face == "italic", "</i>", "")
  value_span <- paste0(
    "<span style='color:", d$colour, ";font-size:8.2pt;'>",
    value_face_open, value_html, value_face_close,
    "</span>"
  )
  d$content <- paste0(bold_label, "<br/>", value_span)

  # Position each item at the top of its allotted slot. y is measured in
  # line-heights; with vjust = 1 the label top is anchored to y, so the
  # first item sits flush at the top of the panel and each subsequent
  # item begins (item_lines[i-1] + gap) lines below the previous.
  n <- nrow(d)
  cum_before <- cumsum(c(0, utils::head(d$item_lines, -1))) +
                (seq_len(n) - 1) * .ITEM_GAP_LINES
  d$y <- total_lines - cum_before

  ggplot2::ggplot(d, ggplot2::aes(x = 0, y = y)) +
    ggtext::geom_richtext(
      ggplot2::aes(label = content),
      hjust = 0, vjust = 1, fill = NA, label.colour = NA,
      label.padding = grid::unit(c(0, 0, 0, 0), "pt"),
      label.margin  = grid::unit(c(0, 0, 0, 0), "pt"),
      lineheight = 1.05
    ) +
    ggplot2::scale_x_continuous(limits = c(-0.02, 1), expand = c(0, 0)) +
    # Panel y range matches the computed content height exactly so there
    # is no slack at top or bottom. A tiny bottom expansion guards the
    # last wrapped value line from clipping.
    ggplot2::scale_y_continuous(
      limits = c(0, total_lines),
      expand = ggplot2::expansion(add = c(0.15, 0))
    ) +
    ggplot2::labs(title = domain_name) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = 10, colour = accent, hjust = 0,
        margin = ggplot2::margin(b = 1, l = 4, t = 5)
      ),
      plot.background = ggplot2::element_rect(
        fill = .alpha_on_white(accent, 0.07),
        colour = .palette$panel_edge, linewidth = 0.3
      ),
      plot.margin = ggplot2::margin(0, 3, 1, 3)
    )
}


#' Column header strip
#' @keywords internal
.header_strip <- function(label, accent) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                      fill = accent, colour = NA) +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = label,
                      fontface = "bold", size = 4, colour = "white") +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
}


#' Build the full figure
#' @keywords internal
.build_figure <- function(paper, data, value_wrap = 75) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Install patchwork: install.packages('patchwork')", call. = FALSE)
  }

  # Card height = content line budget + title overhead. This makes each
  # card take exactly the vertical space its text needs.
  card_h <- function(domain_name) {
    g <- .card_geometry(domain_name, data, value_wrap = value_wrap)
    attr(g, "total_lines") + .TITLE_LINES
  }
  w_heights <- vapply(.welfare_domains,   card_h, numeric(1))
  e_heights <- vapply(.essential_domains, card_h, numeric(1))

  welfare_cards <- lapply(.welfare_domains, .make_card,
                          data = data, accent = .palette$welfare,
                          value_wrap = value_wrap)
  essential_cards <- lapply(.essential_domains, .make_card,
                            data = data, accent = .palette$essential,
                            value_wrap = value_wrap)

  # Scale the shorter column's cards proportionally so both columns
  # end flush at the same vertical position. The "extra" line-units
  # are spread evenly across every card in the shorter column, so no
  # single card carries a visible blob of trailing whitespace.
  w_body <- sum(w_heights)
  e_body <- sum(e_heights)
  target_body <- max(w_body, e_body)
  if (w_body < target_body) w_heights <- w_heights * (target_body / w_body)
  if (e_body < target_body) e_heights <- e_heights * (target_body / e_body)
  col_total <- .STRIP_LINES + target_body

  left_col  <- .header_strip("WELFARE DOMAINS", .palette$welfare) /
               patchwork::wrap_plots(welfare_cards,
                                     ncol = 1, heights = w_heights) +
               patchwork::plot_layout(
                 heights = c(.STRIP_LINES, target_body)
               )
  right_col <- .header_strip("ESSENTIALS", .palette$essential) /
               patchwork::wrap_plots(essential_cards,
                                     ncol = 1, heights = e_heights) +
               patchwork::plot_layout(
                 heights = c(.STRIP_LINES, target_body)
               )

  # Anchor with explicit vjust so text always sits inside the panel
  # regardless of how patchwork sizes it. Title hangs from the top,
  # version sits on the bottom, authors line centred between.
  header <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 1,
                      label = paper$title %||% "",
                      fontface = "bold", size = 5,
                      hjust = 0, vjust = 1) +
    ggplot2::annotate(
      "text", x = 0, y = 0.5,
      label = paste(
        paper$authors %||% "",
        if (!is.null(paper$journal)) paste("  -  ", paper$journal) else ""
      ),
      size = 3.4, hjust = 0, vjust = 0.5, colour = "#444444"
    ) +
    ggplot2::annotate("text", x = 0, y = 0,
                      label = paper$version %||% "INSTAR v1.0",
                      size = 2.8, hjust = 0, vjust = 0,
                      colour = "#888888", fontface = "italic") +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_void()

  footer <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0.5,
                      label = paste0(
                        "Empty fields = item not reported    ",
                        "·    \"Not applicable\" = not relevant to ",
                        "this study"
                      ),
                      size = 2.8, hjust = 0, vjust = 0.5,
                      colour = "#666666") +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_void()

  # Header/footer use the larger-text line budgets defined at the top
  # of this file so their text never gets clipped, regardless of how
  # much body content the figure carries. The trailing `&` theme call
  # zeroes the default per-plot outer margins so the body fills the
  # full page width with only a small gutter between the two columns.
  # Essentials on the left, welfare domains on the right.
  fig <- header / (right_col | left_col) / footer +
    patchwork::plot_layout(
      heights = c(.HEADER_LINES, col_total, .FOOTER_LINES)
    ) &
    ggplot2::theme(plot.margin = ggplot2::margin(0, 2, 0, 2, "pt"))

  # Stash the natural total height (in line-units) on the figure so
  # save_report() can pick a sensible default page height.
  attr(fig, "natural_lines") <- .HEADER_LINES + col_total + .FOOTER_LINES
  fig
}

# Internal: %||%
`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
}

# Silence R CMD check NOTEs about non-standard evaluation in aes()
utils::globalVariables(c("y", "content"))
