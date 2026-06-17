#' The invertebrate welfare reporting framework
#'
#' A data frame describing the 18 reporting items grouped into eight
#' domains: five welfare domains adapted from the Mellor five-domains
#' model (Nutrition, Environment, Health, Behaviour, Affective state),
#' and three cross-cutting essentials (Subjects, Procedures, Ethics &
#' compliance). End-of-study disposition (`fate_end`) lives within the
#' Health welfare domain.
#'
#' @format A data frame with 18 rows and the following columns:
#' \describe{
#'   \item{order}{integer; canonical display order}
#'   \item{group}{`"welfare"` or `"essential"`}
#'   \item{domain}{one of eight domain names}
#'   \item{item_id}{short snake_case identifier; the join key for user input}
#'   \item{item}{display name of the item}
#'   \item{description}{prompt text describing what to report}
#'   \item{lab}{applicability in laboratory studies: `"Y"`, `"C"`, or `"-"`}
#'   \item{field}{applicability in field studies: `"Y"`, `"C"`, or `"-"`}
#' }
#'
#' @source White et al. (in prep), \emph{Reporting items for invertebrate welfare}.
#' @export
framework <- (function() {
  # Order: essentials block first (Subjects, Procedures, Ethics &
  # compliance), then welfare block (Nutrition, Environment, Health,
  # Behaviour, Affective state). This matches the left-to-right reading
  # order of the figure produced by invert_report().
  cols <- list(
    order = 1:18,
    group = c(
      "essential", "essential", "essential",
      "essential", "essential", "essential",
      "essential", "essential", "essential",
      "welfare",
      "welfare", "welfare", "welfare",
      "welfare", "welfare", "welfare",
      "welfare",
      "welfare"
    ),
    domain = c(
      "Subjects", "Subjects", "Subjects",
      "Procedures", "Procedures", "Procedures",
      "Ethics & compliance", "Ethics & compliance", "Ethics & compliance",
      "Nutrition",
      "Environment", "Environment", "Environment",
      "Health", "Health", "Health",
      "Behaviour",
      "Affective state"
    ),
    item_id = c(
      "subjects_taxon", "subjects_source", "subjects_n",
      "proc_handling", "proc_anaesthesia", "proc_biosecurity",
      "ethics_review", "ethics_endpoints", "ethics_statement",
      "nutrition_diet",
      "env_housing", "env_acclimation", "env_field",
      "health_monitoring", "health_injury", "fate_end",
      "behaviour_general",
      "affect_indicators"
    ),
    item = c(
      "Taxonomic ID, life stage, & sex",
      "Source & culture history",
      "Sample size & attrition",
      "Capture, transport, & handling",
      "Anaesthesia, analgesia, & invasive procedures",
      "Containment & biosecurity",
      "Ethics review, permits, & conservation status",
      "Humane endpoints & non-target impacts",
      "Welfare & 3Rs statement",
      "Diet, feeding, & water",
      "Housing & abiotic conditions",
      "Acclimation",
      "Field site & collection",
      "Health monitoring",
      "Injury & mortality",
      "End of study",
      "Behavioural opportunities, enrichment, & disturbance",
      "Indicators & precautionary measures"
    ),
    description = c(
      "Species identification to the lowest practicable taxonomic level (with method); life stage(s) and sex where determinable or relevant; voucher specimens or imagery deposited as appropriate.",
      "Origin: wild-collected (with locality and date), laboratory colony (founding stock, source, date of establishment), or commercial supplier (named). For captive stock: generations in captivity, rearing conditions, and selection or inbreeding history.",
      "Total individuals collected or used and number contributing to analysis, with attrition accounted for. Justification of sample size (a priori power, pilot data, or stated convention). For bulk-sampling or mass-rearing work, report order-of-magnitude counts or ranges, and the unit of replication (colony, cycle, trap-day, batch), rather than individual totals.",
      "Capture method; transport duration, conditions, and mortality. Routine handling and restraint. Marking or tagging method, tag mass where relevant, retention checks. Where individual handling is not practicable (pitfall, Malaise, light, or sticky trapping and similar), report sampling effort (trap-days, trap-nights, deployment volume), trap design and check routine, and measures taken to reduce by-catch, retention time, and trapped-animal suffering.",
      "Anaesthetic or immobilisation agent or method; induction and recovery times; justification. Whether post-procedure analgesia was used, agent and dose, or explicit justification for omission. For surgical/invasive procedures: procedure, instruments, sterility, duration, recovery.",
      "Measures to prevent escape (particularly for non-native taxa) and procedures for disposal of waste and contaminated material.",
      "Institutional or regulatory ethics review and permit numbers, or an explicit statement that none was required with the welfare reasoning applied. Collection or import permits. IUCN, national, or regional conservation status of focal taxa.",
      "Predefined criteria for terminating procedures or experiments early in response to welfare concerns, and any instances triggered. For field work: anticipated and observed non-target impacts, with mitigation.",
      "Brief statement summarising welfare considerations and how the three Rs (Replacement, Reduction, Refinement) informed study design.",
      "Composition and source of diet or bait; preparation; feeding frequency and access; provision of water or moisture; any pre-experimental fasting with justification.",
      "Enclosure materials, dimensions, substrate, and structural complexity. Stocking density and grouping. Temperature, humidity, ventilation, photoperiod and lighting, and water parameters for aquatic species.",
      "Duration and conditions of any acclimation period before experimental procedures.",
      "Habitat type, location, abiotic conditions, and seasonality. Trap design, placement, deployment duration, checking frequency, and measures to limit injury, predation, exposure, or desiccation.",
      "Methods and criteria for assessing physical condition (responsiveness, posture, integument, autotomy). Any screening for disease or parasites and quarantine procedures. Frequency of welfare checks.",
      "Number and timing of injuries and unexpected deaths, suspected causes, and any interventions or protocol adjustments. For colony or industrial-scale work where individual death counts are not meaningful (because of scale, cannibalism, or routine attrition), report the disease-screening and prevention regime, density and condition monitoring, and any conditions under which cohort losses triggered intervention or protocol change.",
      "Slaughter or euthanasia method and justification; release protocols for field-collected animals; continued holding, rehoming, or transfer arrangements; voucher specimen deposition. For mass-rearing work, use the welfare-community terminology for the context (slaughter method for farmed insects). For studies considering release, note the reasoning behind the choice between release, continued holding, rehoming, and slaughter, including disease-spread risk to wild conspecifics and any maladaptation incurred during captivity.",
      "Aspects of the setup that supported or constrained species-typical behaviour. Provision of refugia or enrichment. Measures to reduce ambient disturbance. For social or gregarious species, access to conspecifics and the structure of social grouping.",
      "Taxon-appropriate behavioural and physiological indicators of stress, pain, or distress monitored, and how interpreted. Where capacity for affective experience is uncertain, the precautionary measures adopted."
    ),
    lab = c(
      "Y", "Y", "Y",
      "Y", "C", "Y",
      "Y", "Y", "Y",
      "Y",
      "Y", "Y", "-",
      "Y", "Y", "Y",
      "Y",
      "Y"
    ),
    field = c(
      "Y", "Y", "Y",
      "Y", "C", "C",
      "Y", "Y", "Y",
      "C",
      "C", "C", "Y",
      "C", "Y", "Y",
      "Y",
      "Y"
    )
  )
  data.frame(cols, stringsAsFactors = FALSE)
})()


#' Return an empty items template for a study
#'
#' Convenience helper that returns a data frame with one row per framework
#' item and an empty `value` column for the user to fill in.
#'
#' Leave `value` as `""` for items not reported. Set to `"NA"` (the
#' character string) for items that are not applicable to the study.
#'
#' @param study_type Optional. One of `"lab"`, `"field"`, or `"both"`.
#'   If supplied, items flagged as not applicable in that context have
#'   their `value` pre-set to `"NA"` as a default.
#'
#' @return A data frame with columns `item_id`, `item`, `domain`,
#'   `description`, `value`.
#'
#' @examples
#' tmpl <- framework_template("lab")
#' head(tmpl)
#'
#' @export
framework_template <- function(study_type = c("both", "lab", "field")) {
  study_type <- match.arg(study_type)
  out <- framework[, c("order", "domain", "item_id", "item", "description",
                       "lab", "field")]
  out$value <- ""
  if (study_type == "lab") {
    out$value[out$lab == "-"] <- "NA"
  } else if (study_type == "field") {
    out$value[out$field == "-"] <- "NA"
  }
  out[, c("item_id", "item", "domain", "description", "value")]
}


#' Load items from a CSV file
#'
#' Reads a CSV file with at minimum `item_id` and `value` columns and
#' returns a data frame suitable for [invert_report()].
#'
#' @param path Path to a CSV file.
#' @param ... Additional arguments passed to [utils::read.csv()].
#'
#' @return A data frame with `item_id` and `value` columns.
#'
#' @examples
#' \dontrun{
#' items <- load_items("my_items.csv")
#' }
#'
#' @export
load_items <- function(path, ...) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, ...)
  required <- c("item_id", "value")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("File `", path, "` is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  df$value[is.na(df$value)] <- ""
  df
}
