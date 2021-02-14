#' Create a RData file with all the necessary data structure for a complete review of different kinds of sources and preparation of meta-analyses
#' @return A RData file.
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#' @export


gen_srcrev <- function(){
  
  levels <- list()
  levels$srctype <- c("Article","Book","Webpage","Interview","Document")
  levels$levels <- c("transaction","individual","dyad","team","department","business unit","organization","community","country")
  levels$design <- c("mixed","formal theory","computer simulation","field study","field experiment","experimental simulation","laboratory experiment","judgment task","sample survey")
  levels$sampling <- c("statistical","extreme","typical","maximum variation","homogenous","convenience")
  levels$timing <- c("cross-sectional","longitudinal")
  levels$cpt_type <- c("attribute","perception","attitude","behavior")
  levels$cpt_dim <- c("unidimensional","multidimensional")
  levels$cpt_stab <- c("transient","temporary","durable","immuable")
  levels$periods <- c("constant","second","minute","hour","day","week","month","quarter","semester","year","several years")
  levels$rel_nat <- c("hierarchical","causal")
  levels$rel_sign <- c("positive","negative","contingent")
  levels$rel_dir <- c("none","forward","backward","bi-directional")
  levels$rel_shape <- c("none","linear","concave","convex","u-sahped","inverted-u-shaped")
  levels$mod_shape <- c("none","accentuate","attenuate","u-sahped","inverted-u-shaped")
  levels$discipline <- c("economics","psychology","sociology","behavioral economics","social psychology","other")
  levels$method <- c("self-administered","interview","observation","archive")
  levels$format <- c("quantitative","structured","qualitative","unstructured","mixed")
  levels$test <- c("none","classical-test","generalizability","item-response","aggregate","cluster","bloc-variable","partition","set","code")
  levels$scale <- c("none","categorical","ordinal","scale","ratio")
  levels$specif <- c("endogenous","exogenous","controlled","invariant","randomized","omitted")
  
  #######################################
  # Mapping
  
  sources <- tibble::tibble(
    key = as.character(NA),
    type = factor(NA, levels = levels$srctype),
    informant = as.character(NA),
    design = factor(NA, levels = levels$design),
    sampling = factor(NA, levels = levels$sampling),
    region = as.character(NA),
    industry = as.character(NA),
    timing = factor(NA, levels = levels$timing),
    date_start = as.Date(NA),
    date_end = as.Date(NA),
    pages = as.numeric(NA),
    duration = as.numeric(NA)
  )
  
  concepts <- tibble::tibble(
    key = as.character(NA),
    concept_id = as.integer(NA),
    concept_name = as.character(NA),
    concept_acronym = as.character(NA)
  )
  
  relations <- tibble::tibble(
    key = as.character(NA),
    relation_id = as.integer(NA),
    concept_A = as.character(NA),
    concept_B = as.character(NA)
  )
  
  moderations <- tibble::tibble(
    key = as.character(NA),
    moderation_id = as.integer(NA),
    concept_id = as.character(NA),
    relation_id = as.integer(NA)
  )
  
  #######################################
  # Documenting theoretical level
  
  definitions <- tibble::tibble(
    key = as.character(NA),
    concept_id = as.integer(NA),
    concept_definition = as.character(NA),
    concept_type = factor(NA, levels = levels$cpt_type),
    concept_dimensionality = factor(NA, levels = levels$cpt_dim),
    concept_source_level = factor(NA, levels = levels$levels),
    concept_target_level = factor(NA, levels = levels$levels),
    concept_stability = factor(NA, levels = levels$cpt_stab),
    concept_periodicity = factor(NA, levels = levels$periods)
  )
  
  explanations_rel <- tibble::tibble(
    key = as.character(NA),
    explanation_id = as.integer(NA),
    relation_id = as.integer(NA),
    mechanism = as.character(NA),
    discipline = factor(NA, levels = levels$discipline),
    theory = as.character(NA),
    nature = factor(NA, levels = levels$rel_nat),
    sign = factor(NA, levels = levels$rel_sign),
    direction = factor(NA, levels = levels$rel_dir),
    shape = factor(NA, levels = levels$rel_shape),
    time = factor(NA, levels = levels$periods)
  )
  
  explanations_mod <- tibble::tibble(
    key = as.character(NA),
    explanation_id = as.integer(NA),
    relation_id = as.integer(NA),
    mechanism = as.character(NA),
    discipline = factor(NA, levels = levels$discipline),
    theory = as.character(NA),
    nature = factor(NA, levels = levels$rel_nat),
    sign = factor(NA, levels = levels$rel_sign),
    shape = factor(NA, levels = levels$mod_shape)
  )
  
  #######################################
  # Documenting empirical level
  
  operationalizations <- tibble::tibble(
    key = as.character(NA),
    concept_id = as.integer(NA),
    operationalization_name = as.character(NA),
    operationalization_description = as.character(NA),
    operationalization_method = factor(NA, levels = levels$method),
    operationalization_level = factor(NA, levels = levels$levels),
    operationalization_format = factor(NA, levels = levels$format),
    aggregation_level = factor(NA, levels = levels$levels),
    aggregation_test = factor(NA, levels = levels$test),
    aggregation_scale = factor(NA, levels = levels$scale),
    specification = factor(NA, levels = levels$specif),
    reliability = as.numeric(NA)
  )
  
  observations_rel <- tibble::tibble(
    key = as.character(NA),
    operationalization_A = as.character(NA),
    operationalization_B = as.character(NA),
    number = as.numeric(NA),
    correlation = as.numeric(NA),
    description = as.character(NA)
  )
  
  observations_mod <- tibble::tibble(
    key = as.character(NA),
    operationalization_A = as.character(NA),
    operationalization_B = as.character(NA),
    operationalization_C = as.character(NA),
    number = as.numeric(NA),
    coefficient = as.numeric(NA),
    description = as.character(NA)
  )
  
  notes <- tibble::tibble(
    key = as.character(NA),
    note = as.character(NA)
  )
  
  ###########################################
  # Create directories and files
  
  if (dir.exists("Sources")) {
    files <- list.files(path = "Sources", recursive = TRUE, include.dirs = TRUE)
    files <- files[stringr::str_detect(files, ".pdf")]
    
    if (length(files) > 0) {
      paths <- tibble::tibble(
        path = files,
        key = as.character(NA)
      )
    } else {
      paths <- tibble::tibble(
        path = as.character(NA),
        key = as.character(NA)
      )
    }
  } else {
    dir.create("Sources")
    paths <- tibble::tibble(
      path = as.character(NA),
      key = as.character(NA)
    )
  }
  if (!dir.exists("Review")) dir.create("Review")
  if (!file.exists("Review/srcrev.RData")) save(paths, levels,
                                                sources,concepts,relations,moderations,
                                                definitions, explanations_rel, explanations_mod,
                                                operationalizations, observations_rel,observations_mod,
                                                notes, file = "Review/srcrev.RData")
}