# R script for "carob"
# license: GPL (>=3)

## NOTES
# 11 site-level weed surveys (Cote d'Ivoire, 2009-2016). One row per weed
# species actually observed per site (absent species not listed). Site
# metadata repeats across every species row for that site. Source is in
# French; labels/values translated to English.

## ISSUES
# Species names/families come from 6 differently-laid-out crop-specific
# Excel sheets, not the coded FLO.txt matrix - counts differ slightly from
# FLO.txt's per-site totals (not exhaustively reconciled).
# crop = "plantain;cassava" for the intercropped site (CDI-IPO-10) - not
# split into intercropped/intercrops; left for review.
# Facteurs sheet had two columns both named "CDI-IPO-10" - second renamed
# to CDI-IPO-11 to match FLO.txt's 11-site numbering.
# adm1 (district) for Adzope/Danane sites is inferred, not confirmed.


carob_script <- function(path) {
  
  "
Synthesis of the inventory of weeds of food crops in Cote d'Ivoire (2009-2016)

Synthesis of weed presence/absence surveys across 11 crop x location
combinations in the humid tropical climate regions of Cote d'Ivoire
(2009-2016), covering 494 weed species in rice, maize, plantain, cassava,
yam, groundnut, and general food crop fields.
"
  
  uri <- "doi:10.18167/DVN1/V0QGKT"
  group <- "pest_disease"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
                                  data_organization = "CIRAD",
                                  publication = NA,
                                  project = "Amatrop",
                                  design = "presence/absence weed survey, 11 crop x location combinations",
                                  data_type = "survey",
                                  treatment_vars = "none",
                                  response_vars = "weed_species",
                                  notes = NA,
                                  carob_contributor = "Stella Muthoni",
                                  carob_date = "2026-07-23",
                                  carob_completion = 75,
                                  carob_effort = 5
  )
  
  f1 <- ff[basename(ff) == "CDI-IPO-2009-DIV-PA.xlsx"]
  f2 <- ff[basename(ff) == "CDI-IPO-2009-DIV-FAC.txt"]
  f3 <- ff[basename(ff) == "CDI-IPO-2009-DIV-PA-DOC.txt"]
  f4 <- ff[basename(ff) == "CDI-IPO-2009-DIV-PA-FLO.txt"]
  
  ## r1: the crop-specific sheets (Riz, CV, MAIS, Cultures vivrieres,
  ## Plantain+Manioc, Riz2) hold a 
  r1a <- carobiner::read.excel(f1, sheet="M\u00e9ta Donn\u00e9es")
  r1b <- carobiner::read.excel(f1, sheet="Facteurs")
  r1c <- carobiner::read.excel(f1, sheet="Floristique")
  r1d <- carobiner::read.excel(f1, sheet="Riz")
  r1e <- carobiner::read.excel(f1, sheet="CV")
  r1f <- carobiner::read.excel(f1, sheet="MAIS")
  r1g <- carobiner::read.excel(f1, sheet="Cultures vivri\u00e8res")
  r1h <- carobiner::read.excel(f1, sheet="Plantain+Manioc")
  r1i <- carobiner::read.excel(f1, sheet="Riz2")
  
  #r3 <- readLines(f3, encoding = "latin1") ; # plain metadata
  
  #r2 <- read.table(f2, header = TRUE) #site information, same as r1b
  #r4 <- read.table(f4, header = TRUE) # 494 species presence absence; used per site instead
  
  # Create the common metadata using r1b file
  ### Fix duplicate CDI-IPO-10 column names in r1b and r1c
  names(r1b)[names(r1b) == "CDI-IPO-10...11"] <- "CDI-IPO-10"
  names(r1b)[names(r1b) == "CDI-IPO-10...12"] <- "CDI-IPO-11"
  
  ### Site-level metadata, from r1b, translated from French
  ### (r1b's rows are labeled via its "Releve" column, not real R rownames)
  get_row <- function(label) unlist(r1b[r1b$Releve == label, -1])
  crop_lookup <- data.frame(
    fr = c("riz", "vivrier", "mais", "plantain", "manioc", "igname", "arachide", "plantain/manioc"),
    crop = c("rice", "unknown", "maize", "plantain", "cassava", "yam", "groundnut", "plantain;cassava")
  )
  culture <- get_row("Culture_origine")
  d1b <- data.frame(
    trial_id = names(r1b)[-1],
    #crop = crop_lookup$crop[match(tolower(culture), crop_lookup$fr)], #defined later when extracting weed species
    crop_local = culture,
    intercropped = culture == "plantain/manioc", # to be decided for sites 4-9 which have weedspecies for general crops
    obs_year = as.character(get_row("Annee")), #suggested term for year of observation
    country = "C\u00f4te d'Ivoire",
    location = get_row("Lieu"),
    herbicide_used = !is.na(get_row("Herbicide")) & tolower(get_row("Herbicide")) != "non",
    herbicide_product = ifelse(tolower(get_row("Herbicide")) == "glyphosate", "glyphosate", NA),
    irrigated = tolower(get_row("Irrigation")) %in% c("inonde", "inond\u00e9"),
    irrigation_method = ifelse(tolower(get_row("Irrigation")) %in% c("inonde", "inond\u00e9"), "flood", NA),
    landuse_intensity = get_row("Intensification"), #suggested term
    aez = get_row("Climat"), #suggested term for climate zone
    crop_group = get_row("Type_culture") #suggested term for categorized crop group
  )
  
  #geo_yamoussoukro <- carobiner::geocode(country = "Cote d'Ivoire", location = "Yamoussoukro")$put
  #geo_issia <- carobiner::geocode(country = "Cote d'Ivoire", location = "Issia")$put
  #geo_mbahiakro <- carobiner::geocode(country = "Cote d'Ivoire", location = "M'Bahiakro")$put
  #geo_adzope <- carobiner::geocode(country = "Cote d'Ivoire", location = "Adzope")$put
  #geo_danane <- carobiner::geocode(country = "Cote d'Ivoire", location = "Danane")$put
  
  location_coords <- data.frame(
    location = c("Yamoussoukro", "Issia (Centre Ouest)", "M'Bahiakro", "Adzop\u00e9, Akoup\u00e9", "Danan\u00e9, Guib\u00e9roua, Ferkess\u00e9dougou"),
    longitude = c(-5.2579, -6.5888, -4.3411, -3.864, -8.1546),
    latitude = c(6.8241, 6.4878, 7.4571, 6.1058, 7.2631),
    adm1 = c("Yamoussoukro", "Sassandra-Marahou\u00e9", "Lacs", "Lagunes", "Montagnes"),   # district; Adzope/Danane less certain
    adm2 = c(NA, "Haut-Sassandra", "Iffou", "La M\u00e9", "Tonkpi"),   # region
    geo_from_source = FALSE
  )
  
  d1b <- merge(d1b, location_coords, by = "location", all.x = TRUE)
  
  #per site weed species extraction
  d1d <- data.frame(
    farm_id = "CDI-IPO-01",
    species_name = r1d[[4]][!is.na(r1d[[4]])],
    weed_family = r1d[[5]][c(2:nrow(r1d), NA)][!is.na(r1d[[4]])]
  )
  d1d <- d1d[-1, ]   # drop header-row artifact ("Nom des espèces") - fixed: was d1a
  d1d$weed_family[d1d$species_name == "Vernonia cinerea L."] <- "Asteraceae"
  d1d$weed_family[d1d$species_name == "Vernonia galamensis (Cass.) Less."] <- "Asteraceae"
  d1d$crop <- "riz"
  
  d1e <- data.frame(
    farm_id = "CDI-IPO-02",
    species_name = r1e[[3]][!is.na(r1e[[3]])],
    weed_family = r1e[[4]][c(2:nrow(r1e), NA)][!is.na(r1e[[3]])])
  d1e <- d1e[-1, ]   # drop header-row artifact
  d1e$weed_family[d1e$species_name == "Vernonia cinerea (L.) Less."] <- "Asteraceae"
  d1e$crop <- "vivrier"
  
  d1f <- data.frame(
    farm_id = "CDI-IPO-03",
    species_name = r1f[[3]][!is.na(r1f[[3]])],
    weed_family = r1f[[4]][c(2:nrow(r1f), NA)][!is.na(r1f[[3]])])
  d1f <- d1f[-1, ]   # drop header-row artifact
  d1f$weed_family[d1f$species_name == "Zornia glochidiata Reichenbach ex de Candolle"] <- "Fabaceae"
  d1f$crop <- "mais"

  # r1g holds multiple sites so needs unlisting
  site_ids <- unlist(r1g[5, 4:9])              # CDI-IPO-04 ... CDI-IPO-09
  crop_ids <- tolower(unlist(r1g[10, 4:9]))    # riz, plantain, maïs, igname, manioc, arachide
  crop_ids <- gsub("ma\u00efs", "mais", crop_ids)
  r1g_clean <- r1g[!is.na(r1g[[2]]) & r1g[[2]] != "Esp\u00e8ces" & !is.na(r1g[[3]]), ]
  d1g <- do.call(rbind, lapply(seq_len(nrow(r1g_clean)), function(i) {
    row <- r1g_clean[i, ]
    present <- which(row[4:9] == "1" & !is.na(row[4:9]))
    if (length(present) == 0) return(NULL)
    data.frame(
      farm_id = site_ids[present],
      species_name = row[[2]],
      weed_family = row[[3]],
      crop = if (length(present) == 6) "vivrier" else crop_ids[present])
  }))
  
  d1h <- data.frame(
    farm_id = "CDI-IPO-10",
    species_name = r1h[[3]][!is.na(r1h[[3]])],
    weed_family = r1h[[4]][c(2:nrow(r1h), NA)][!is.na(r1h[[3]])]
  )
  d1h <- d1h[-1, ]   # drop header-row artifact
  d1h$weed_family[d1h$species_name == "Zea mays"] <- "Poaceae"
  d1h$crop <- "plantain/manioc"   # two crops
 
  d1i <- data.frame(
    farm_id = "CDI-IPO-11",
    species_name = r1i[[3]][!is.na(r1i[[3]])],
    weed_family = r1i[[4]][c(2:nrow(r1i), NA)][!is.na(r1i[[3]])])
  d1i <- d1i[-1, ]   # drop header-row artifact
  d1i$weed_family[d1i$species_name == "Polydora poskeana (Vatke & Hildebr.) H. Rob."] <- "Asteraceae"
  nrow(d1i)
  d1i$crop <- "riz"
  
  #Merge all the sites into a d1 database
  d1 <- rbind(d1d, d1e, d1f, d1g, d1h, d1i)
  d1$crop <- crop_lookup$crop[match(tolower(d1$crop), crop_lookup$fr)]
  
  ### Merge species lookup (d1) with site metadata (d1b)
  d <- merge(d1, d1b, by.x = "farm_id", by.y = "trial_id", all.x = TRUE)
  names(d)[names(d) == "species_name"] <- "weed_species"
  names(d)[names(d) == "farm_id"] <- "trial_id"
  
  d$on_farm <- TRUE
  d$is_survey <- TRUE
  d$yield <- NA
  d$yield_moisture <- NA
  d$yield_part <- NA
  d$K_fertilizer <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$planting_date <- NA
  d$harvest_date <- NA
  d$yield_isfresh <- NA
  
  
  carobiner::write_files(path, meta, d)
}
