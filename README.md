# invertreport

A small R toolkit implementing **INSTAR** (**IN**vertebrate **S**tandards
for **T**reatment **A**nd **R**eporting), an 18-item reporting framework
for invertebrate welfare in research (White et al., in prep). The
empirical baseline and primary intended audience are ecological and
evolutionary research, but the framework applies wherever invertebrates
are used.

The package produces a standardised, publication-ready summary figure in
which each cell carries the substantive content for the corresponding
item (species and provenance, housing conditions, ethics permits, 3Rs
reasoning, and so on), in the same spirit as the PRISMA and ROSES flow
diagrams for evidence synthesis.

## Installation

```r
# install.packages("remotes")
remotes::install_github("thomased/invertreport")
```

For local development, open `invertreport.Rproj` in RStudio (which sets
the working directory to the package root), then:

```r
devtools::document()    # regenerate man/ files from roxygen comments
devtools::install()     # install locally
devtools::test()        # run tests
devtools::check()       # full R CMD check
```

If `devtools::document()` errors with "Could not find package root", your
R session's working directory is not inside the package. Either open the
`.Rproj` file, run `setwd("path/to/invertreport")`, or pass the path
explicitly: `devtools::document("path/to/invertreport")`.

## Three ways to fill out the framework

Pick whichever fits your workflow. All three produce the same kind of
`items` object that `invert_report()` consumes.

### 1. Interactive prompt (recommended for first-time users)

```r
library(invertreport)
items <- fill_items(save_to = "my_study_items.csv")
```

This walks through the 18 items in order, showing each one's name,
description, and current value, and prompting you for input. At any
prompt you can type:

- the value, or just press **enter** to keep the current one
- `NA` to mark the item as not applicable to your study
- `skip` to leave it blank (it'll render as "Not reported")
- `back` to return to the previous item
- `save` (or `save other_path.csv`) to write progress to disk
- `quit` to stop and return what you have so far

To resume later, load the saved CSV back in:

```r
items <- fill_items(load_items("my_study_items.csv"),
                    save_to = "my_study_items.csv")
```

To tweak a single item afterwards:

```r
items <- edit_item(items, "subjects_n")   # by item_id
items <- edit_item(items)                 # numbered menu of all 18 items
```

To check your progress at any point:

```r
show_items(items)
```

### 2. Fill out a CSV template

For collaborators who'd rather work in Excel, or when items are spread
across several people, write a blank template and fill the `value` column:

```r
save_template("my_study_items.csv", study_type = "field")
# ...edit the file in Excel / Numbers / a text editor...
items <- load_items("my_study_items.csv")
```

The template CSV has one row per item with `item_id`, `item`, `domain`,
`description`, and an empty `value` column. The `item`, `domain`, and
`description` columns are there as on-page reminders of what each item
asks for; they're ignored when the CSV is loaded back, so only `item_id`
and `value` are strictly required.

A bundled blank template is also available without writing anything:

```r
template_path <- system.file("extdata", "template_items.csv",
                             package = "invertreport")
file.copy(template_path, "my_study_items.csv")
```

A filled example (a notional bumblebee study) is at:

```r
system.file("extdata", "example_items.csv", package = "invertreport")
```

### 3. Programmatic fill (for scripts)

```r
items <- framework_template(study_type = "field")
items$value[items$item_id == "subjects_taxon"]  <- "Bombus terrestris (worker female); COI"
items$value[items$item_id == "subjects_source"] <- "Wild-collected, Royal NP, May 2025"
items$value[items$item_id == "subjects_n"]      <- "n=80; n=72 analysed (10% attrition)"
# ...and so on
```

## Building the figure

Once `items` is filled, the rest is the same regardless of how you got
there:

```r
report <- invert_report(
  paper = list(
    title   = "My study title",
    authors = "Smith et al. (2026)",
    journal = "Some Journal"
  ),
  items = items
)

report
#> <invert_report>
#>   14 of 17 applicable items reported (82%); 1 items not applicable.
#>   Use plot() or save_report() to render.

save_report(report, "fig_S1_welfare_reporting.pdf")
```

## Web tool

For a clickable interface with live preview, launch the bundled Shiny app:

```r
run_shiny_app()
```

## The framework

The 18 items are split into five welfare domains adapted from Mellor *et al.*
(2020) (Nutrition, Environment, Health, Behaviour, Affective state) and
three cross-cutting essentials (Subjects, Procedures, Ethics & compliance).
End-of-study disposition lives within the Health welfare domain. See
`?framework` for the full table, or:

```r
table(framework$domain, framework$group)
```

## Conventions

- Leave `value` as `""` (empty) for items not reported by the study; the
  figure renders those cells as *Not reported* in muted italic.
- Set `value` to `"NA"` for items not applicable to the study; the figure
  renders *Not applicable* in grey italic.
- Otherwise, write a concise sentence or two of substantive content per
  cell, exactly as you would in the methods paragraph.

## Citation

If you use `invertreport`, please cite:

> White, T. E., Lynch, K. E., Forster, C., Latty, T., Umbers, K. D. L.,
> & Drinkwater, E. (in prep). INSTAR: reporting items for invertebrate
> welfare in research.

## Licence

MIT. See `LICENSE`.
