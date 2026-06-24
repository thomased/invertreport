# --------------------------------------------------------------------
# invertreport — Shiny web tool
#
# Lets users fill out the 18-item welfare reporting framework
# interactively, preview the figure, and download as PDF or PNG.
# --------------------------------------------------------------------

library(shiny)

local({
  # Source the bundled R files into globalenv. The same R/ directory
  # ships inside inst/shiny/ both when the app is run locally via
  # invertreport::run_shiny_app() (working directory points at the
  # installed package's inst/shiny) and when it's run under shinylive
  # (where the directory is bundled into the static export). Sourcing
  # always avoids any library() call for the package — which is
  # important because shinylive's static scanner attempts to install
  # any package named in a library() call from the webR binary repo,
  # fails for non-CRAN packages, and on Safari cascades into a stack
  # overflow.
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
    sys.source(f, envir = globalenv())
  }
})

use_bslib <- requireNamespace("bslib", quietly = TRUE)

domains_ordered <- unique(framework$domain)

# Internal helper
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || identical(a, "")) b else a
nzchar_or <- function(x, default) if (is.null(x) || !nzchar(x)) default else x

# ---------- UI ----------
sidebar_content <- function() {
  tagList(
    textInput("paper_title",   "Title",          value = ""),
    textInput("paper_authors", "Authors",        value = ""),
    textInput("paper_journal", "Journal / venue", value = ""),
    selectInput("study_type", "Study type",
                choices = c("Both" = "both", "Laboratory" = "lab",
                            "Field" = "field"),
                selected = "both"),
    actionButton("apply_template", "Reset items to template",
                 class = "btn-sm"),
    tags$hr(),
    # Build the item inputs ONCE on app start.
    lapply(domains_ordered, function(dom) {
      dom_items <- framework[framework$domain == dom, , drop = FALSE]
      controls <- lapply(seq_len(nrow(dom_items)), function(i) {
        id    <- dom_items$item_id[i]
        label <- dom_items$item[i]
        desc  <- dom_items$description[i]
        tagList(
          tags$label(label, style = "font-weight:600; font-size: 0.9em;"),
          tags$small(desc,
                     style = "display:block; color:#666; margin-bottom:4px;"),
          textAreaInput(
            inputId = paste0("val_", id),
            label = NULL,
            value = "",
            rows = 2, width = "100%",
            placeholder = "Leave blank if not reported; type 'NA' if not applicable"
          )
        )
      })
      tagList(
        tags$h4(dom, style = "margin-top: 14px; color: #2E5F8E;"),
        controls
      )
    }),
    tags$hr(),
    downloadButton("download_pdf", "Download PDF", class = "btn-primary"),
    downloadButton("download_png", "Download PNG")
  )
}

ui <- if (use_bslib) {
  bslib::page_sidebar(
    title = "Invertebrate welfare reporting",
    sidebar = bslib::sidebar(width = 440, sidebar_content()),
    bslib::card(
      bslib::card_header("Preview"),
      plotOutput("preview", height = "1000px")
    )
  )
} else {
  fluidPage(
    titlePanel("Invertebrate welfare reporting"),
    sidebarLayout(
      sidebarPanel(width = 5, sidebar_content()),
      mainPanel(width = 7,
                h3("Preview"),
                plotOutput("preview", height = "1000px"))
    )
  )
}

# ---------- Server ----------
server <- function(input, output, session) {

  # Read all item inputs into a data frame. This reactive depends on every
  # val_<id> input, so it invalidates whenever any text area changes.
  current_items <- reactive({
    vals <- vapply(framework$item_id, function(id) {
      v <- input[[paste0("val_", id)]]
      if (is.null(v)) "" else v
    }, character(1))
    data.frame(item_id = framework$item_id, value = vals,
               stringsAsFactors = FALSE)
  })

  # Push study-type defaults into the existing inputs when the dropdown
  # changes (or on initial load).
  observeEvent(input$study_type, ignoreInit = FALSE, {
    tmpl <- framework_template(input$study_type)
    for (i in seq_len(nrow(tmpl))) {
      id <- tmpl$item_id[i]
      # Only overwrite items that are currently empty, so the user doesn't
      # lose typed content on a study-type change.
      current_val <- isolate(input[[paste0("val_", id)]]) %||% ""
      if (current_val == "" || current_val == "NA") {
        updateTextAreaInput(session, paste0("val_", id),
                            value = tmpl$value[i])
      }
    }
  })

  # Reset all inputs to template defaults
  observeEvent(input$apply_template, {
    tmpl <- framework_template(input$study_type)
    for (i in seq_len(nrow(tmpl))) {
      updateTextAreaInput(session, paste0("val_", tmpl$item_id[i]),
                          value = tmpl$value[i])
    }
  })

  # Build the report. Wrapped in tryCatch so render errors surface as
  # notifications rather than blanking the preview silently.
  current_report <- reactive({
    paper <- list(
      title   = nzchar_or(input$paper_title,   "Untitled study"),
      authors = nzchar_or(input$paper_authors, "Author(s) not given"),
      journal = nzchar_or(input$paper_journal, NULL)
    )
    tryCatch(
      invert_report(paper = paper, items = current_items(), strict = FALSE),
      error = function(e) {
        showNotification(paste("Error building figure:",
                               conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      }
    )
  })

  # Render the figure. CRITICAL: strip the `invert_report` S3 class so the
  # default patchwork print method draws to the graphics device. If we
  # leave the class attached, print() dispatches to print.invert_report,
  # which only writes a coverage summary to the console — and the preview
  # ends up blank.
  #
  # Safari is sensitive to two things here: (1) the default `res` value
  # was too high and pushed the rendered bitmap past Safari's canvas
  # limits, leaving the preview area blank; (2) the initial-load
  # reactive sometimes does not flush in Safari if every input is NULL,
  # so we tap input$study_type to force at least one non-NULL dependency.
  output$preview <- renderPlot({
    input$study_type  # force a non-NULL reactive dep for Safari
    rep <- current_report()
    if (is.null(rep)) return(NULL)
    class(rep) <- setdiff(class(rep), "invert_report")
    print(rep)
  }, bg = "white")

  output$download_pdf <- downloadHandler(
    filename = function() "welfare_reporting.pdf",
    content = function(file) {
      rep <- current_report()
      if (!is.null(rep)) save_report(rep, file)
    }
  )

  output$download_png <- downloadHandler(
    filename = function() "welfare_reporting.png",
    content = function(file) {
      rep <- current_report()
      if (!is.null(rep)) save_report(rep, file)
    }
  )
}

shinyApp(ui, server)
