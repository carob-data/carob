# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The repository description and "Méta données" sheet say 26 surveys, but
#    the data have 22 (BEN-MAR-01, -02, -15 and -20 are absent).
# 2. No coordinates in the source. The 2002 INSAE village census (Cahier des
#    villages, Zou, p. 22) places Zonmon village in arrondissement and commune
#    Zagnanado, Zou department. Zonmon itself is not in GeoNames or OSM, so the
#    coordinates of the Zagnanado arrondissement seat are used, with a 15 km
#    uncertainty.
# 3. "Irrigation" in Facteurs is "pluvial" (rainfed) or "inondé" (flooded).
#    Following the source, inondé -> irrigated = TRUE, irrigation_method =
#    "flood". This is clear for the six rice plots ("riz irrigué" in the title),
#    but the five maize plots marked inondé (BEN-MAR-12, -13, -14, -17, -18)
#    may be flood-prone lowland rather than irrigated fields.
# 4. Topography ("parcelles chem p100" sheet) is mapped to landscape_position:
#    exondé -> "upland", inondé -> "lowland". The three fallow plots are
#    "chemin" (path / track side), left as NA.
# 5. There is no yield or management data (weed survey); yield, fertilizer and
#    date variables are not included.

# weed_species_cover (added to terminag): cover (%) of an individual weed species on the
#    plot, in the long table with weed_species (ground_cover = crop canopy cover;
#    weed_cover is total plot weed cover, as in doi:10.18167/DVN1/VCLIB5)

## NEW VARIABLES
# weed_family_: botanical family of the weed species

carob_script <- function(path) {
  
  "
Weed surveys in food crops and irrigated rice in the Zonmon region, Benin (2013)

Characterization of weed communities of food crops (groundnut, maize, sweet potato, cassava, fallow land) and irrigated rice in the Zonmon region, Benin in 2013. Phytoecological study with abundance scale 1-9. This dataset covers 26 plant surveys for 86 weed species.
"
  
  uri <- "doi:10.18167/DVN1/X98KVY"
  group <- "pest_disease"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
                                  data_organization = "CIRAD",
                                  publication = NA,
                                  project = "Amatrop",
                                  design = NA,
                                  data_type = "survey",
                                  treatment_vars = "none",
                                  response_vars = "weed_cover;weed_species;weed_species_cover",
                                  notes = NA,
                                  carob_contributor = "Mitchelle Njukuya",
                                  carob_date = "2026-11-02",
                                  carob_completion = 100,
                                  carob_effort = 6
  )
  
  f <- ff[basename(ff) == "BEN-MAR-2013-DIV-AD.xlsx"]
  r1 <- carobiner::read.excel(f, sheet = "Facteurs")
  r2 <- carobiner::read.excel(f, sheet = "Floristique")
  r3 <- carobiner::read.excel(f, sheet = "parcelles chem p100", col_names = FALSE)
  
  ## plot level (wide) records
  get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))
  
  crop <- c(arachide = "groundnut", patate = "sweetpotato", manioc = "cassava",
            mais = "maize", riz = "rice", friche = "none")
  
  assoc_fr <- c("ma\u00efs", "manioc", "palmier", "arachide", "gombo", "patate")
  assoc_en <- c("maize", "cassava", "oil palm", "groundnut", "okra", "sweetpotato")
  intercrops <- sapply(tolower(get_row("Cultures_associees")), function(x) {
    if (is.na(x) || x == "-") return(NA)
    paste(assoc_en[match(strsplit(x, " ")[[1]], assoc_fr)], collapse = ";")
  })
  
  irr <- get_row("Irrigation")
  
  d <- data.frame(
    trial_id = names(r1)[-1],
    date = as.character(as.integer(as.numeric(get_row("Annee")))),
    country = "Benin",
    adm1 = "Zou",
    adm2 = "Zagnanado",
    adm3 = "Zagnanado",
    location = get_row("Lieu"),
    crop = unname(crop[get_row("Culture_origine")]),
    intercrops = unname(intercrops),
    irrigated = irr == "inond\u00e9",
    irrigation_method = ifelse(irr == "inond\u00e9", "flood", "none"),
    weed_cover = as.numeric(get_row("Enherbement")),
    stress = "weeds"
  )
  d$intercropped <- !is.na(d$intercrops)
  
  ## topography is in row 2 of r3; plot numbers in row 1, columns 6-27
  topo <- data.frame(
    trial_id = sprintf("BEN-MAR-%02d", as.integer(unlist(r3[1, 6:27]))),
    landscape_position = c("upland", "lowland", NA)[match(unlist(r3[2, 6:27]), c("exond\u00e9", "inond\u00e9", "chemin"))]
  )
  d <- merge(d, topo, by = "trial_id", all.x = TRUE)
  
  # Zagnanado arrondissement seat; Zonmon village is in this arrondissement (INSAE 2002, Zou p. 22)
  d$longitude <- 2.3834 # https://www.geonames.org/2390751
  d$latitude <- 7.2210 # https://www.geonames.org/2390751
  d$geo_uncertainty <- 15000
  d$geo_source <- "GeoNames 2390751 (Zagnanado arrondissement seat)"
  d$geo_from_source <- FALSE
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  
  ## species lookup: rows below the "global" row with a code in column 4
  i <- which(!is.na(r3[[4]]) & r3[[4]] != "x")
  spp <- data.frame(
    code = r3[[4]][i],
    weed_species = trimws(r3[[3]][i]),
    weed_family_ = trimws(r3[[2]][i])
  )
  spp <- spp[spp$code != "PYLNI", ] # merged into PYLAM in Floristique
  
  ## species level (long) records
  score <- as.matrix(r2[, -1])
  j <- which(!is.na(score) & score > 0, arr.ind = TRUE)
  cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)
  long <- data.frame(
    trial_id = colnames(score)[j[, "col"]],
    code = r2[[1]][j[, "row"]],
    weed_species_cover = cover[score[j]]
  )
  long <- merge(long, spp, by = "code", all.x = TRUE)
  long$code <- NULL
  
  carobiner::write_files(path, meta, d, long = long)
}