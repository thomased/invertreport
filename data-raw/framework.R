# --------------------------------------------------------------------
# Build the package's framework dataset from the canonical spreadsheet.
# Run this from the package root with:  source("data-raw/framework.R")
# --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tibble)

src <- "../data/standards_v1.xlsx"   # canonical source, relative to package root
stopifnot(file.exists(src))

raw <- readxl::read_xlsx(src, sheet = "Reporting items v1.0", skip = 2)

# The spreadsheet has merged Domain cells; fill down.
raw <- raw %>%
  rename(domain = Domain, item = Item, description = Description,
         lab = Lab, field = Field, notes = Notes) %>%
  mutate(domain = vctrs::vec_fill_missing(domain, direction = "down")) %>%
  filter(!is.na(item))

welfare_set <- c("Nutrition", "Environment", "Health",
                 "Behaviour", "Affective state")

# Generate short snake_case item_ids deterministically; this mapping is
# stable and hand-curated to keep IDs short and meaningful.
id_map <- c(
  "Taxonomic identification, life stage, and sex"       = "subjects_taxon",
  "Source and culture history"                          = "subjects_source",
  "Sample size and attrition"                           = "subjects_n",
  "Diet, feeding, and water"                            = "nutrition_diet",
  "Housing and abiotic conditions"                      = "env_housing",
  "Acclimation"                                         = "env_acclimation",
  "Field site and collection"                           = "env_field",
  "Health monitoring"                                   = "health_monitoring",
  "Injury and mortality"                                = "health_injury",
  "Behavioural opportunities, enrichment, and disturbance" = "behaviour_general",
  "Indicators and precautionary measures"               = "affect_indicators",
  "Capture, transport, and handling"                    = "proc_handling",
  "Anaesthesia, analgesia, and invasive procedures"     = "proc_anaesthesia",
  "Containment and biosecurity"                         = "proc_biosecurity",
  "End of study"                                        = "fate_end",
  "Ethics review, permits, and conservation status"     = "ethics_review",
  "Humane endpoints and non-target impacts"             = "ethics_endpoints",
  "Welfare and 3Rs statement"                           = "ethics_statement"
)

framework <- raw %>%
  mutate(
    item_id = unname(id_map[item]),
    group = ifelse(domain %in% welfare_set, "welfare", "essential"),
    order = row_number()
  ) %>%
  select(order, group, domain, item_id, item, description, lab, field) %>%
  as_tibble()

stopifnot(!any(is.na(framework$item_id)))
stopifnot(!any(duplicated(framework$item_id)))

usethis::use_data(framework, overwrite = TRUE)
