# R script for "carob"
# license: GPL (>=3)


## NOTES
# Only NorthernRegion file contains crop data; 
# Upper West and Upper East contain only demographic and livestock data. 
# Data is at subplot level (D2 section): each row is one household-subplot combination with up to 6 subplots per household. 
# Subplot area used for yield calculation (not total plot area). 
# Harvest converted to kg: bags multiplied by 50 (standard 50kg bag, per codebook p.11); 
# kg values used directly; bare numbers set to NA. Fertilizer from free-text field: NPK 15-15-15 (confirmed in data); 
# SA = 21% N; UREA = 46% N; unmarked numbers assumed bags. 
# Crop residue management (D4) not included as subplot numbering does not align with D2. 
# Variety spelling standardized: obatanpa, akomasa, afife, chekopag, laribako, gbena, topsii.


## A lot more could be done. E.g. livestock data. Household characteristics. Labour use...

carob_script <- function(path) {
  
"Baseline household and community survey conducted under the Africa RISING 
project in northern Ghana (Northern Region, Upper West and Upper East) in 2013,
covering the 2012 cropping season. Data includes crop production across multiple 
plots per household, land use, livestock, labor and household demographics. 
Published by Wageningen University and Research (WUR). Maize is the dominant 
crop in the Northern Region, which also includes groundnut, rice, soybean, 
cowpea, yam and cassava in a mixed farming system."
  
  uri <- "doi:10.7910/DVN/ZH8UZI"
  group <- "survey"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
      publication = NA,
      carob_contributor = "Stella Muthoni",
      carob_date = "2026-06-09",
      carob_effort = NA,
      data_type = "survey",
      data_organization = "WUR",
      project = "Africa RISING",
      treatment_vars = "none",
      response_vars = "none",
      notes = "",
      design = NA,
      carob_completion = 30
  )
  
  #Load only Northern Region file that contains crop data
  f <- ff[basename(ff) == "NorthernRegion.csv"]
  nr <- read.csv(f)
  
  # convert harvest text to kg; standard 50kg bag confirmed in survey codebook
  parse_harvest <- function(x) {
    x <- trimws(toupper(x))
    num <- as.numeric(gsub("[^0-9.]", "", x))
    is_bag <- grepl("BAG", x)
    is_kg  <- grepl("KG", x)
    # default NA for unknown units (bare numbers without unit)
    result <- rep(NA_real_, length(x))
    result[is_bag] <- num[is_bag] * 50
    result[is_kg]  <- num[is_kg]
    result
  }
  
  # extract fertilizer quantities and convert to N, P, K, S kg/ha
  # NPK formulation 15-15-15 confirmed from "NPK 15 15 15" entry in data
  # SA (sulphate of ammonia) = 21% N; UREA = 46% N
  # bag weight assumed 50kg where no unit specified
  parse_fertilizer <- function(fert, area_ha) {
    x <- trimws(toupper(fert))
    
    npk_kg <- rep(NA_real_, length(x))
    
    # NPK with KG explicit e.g. "NPK150KG", "NPK 150KG"
    has_npk_kg <- grepl("NPK\\s*[0-9]+\\s*KG", x)
    idx <- which(has_npk_kg)
    matched <- regmatches(x[idx], regexpr("NPK\\s*[0-9]+\\s*KG", x[idx]))
    npk_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched))
    
    # NPK with BAGS explicit e.g. "NPK2BAGS", "NPK 2 BAGS"
    has_npk_bag <- grepl("NPK\\s*[0-9]+\\s*BAG", x)
    idx <- which(has_npk_bag)
    matched <- regmatches(x[idx], regexpr("NPK\\s*[0-9]+\\s*BAG", x[idx]))
    npk_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched)) * 50
    
    # NPK with number only e.g. "NPK1 SA1" — assumed bags
    has_npk_num <- grepl("NPK\\s*[0-9]+\\s*SA", x) & !has_npk_kg & !has_npk_bag
    idx <- which(has_npk_num)
    matched <- regmatches(x[idx], regexpr("NPK\\s*[0-9]+", x[idx]))
    npk_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched)) * 50
    
    sa_kg <- rep(NA_real_, length(x))
    
    # SA with KG explicit
    has_sa_kg <- grepl("SA\\s*[0-9]+\\s*KG", x)
    idx <- which(has_sa_kg)
    matched <- regmatches(x[idx], regexpr("SA\\s*[0-9]+\\s*KG", x[idx]))
    sa_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched))
    
    # SA with BAGS explicit
    has_sa_bag <- grepl("SA\\s*[0-9]+\\s*BAG", x)
    idx <- which(has_sa_bag)
    matched <- regmatches(x[idx], regexpr("SA\\s*[0-9]+\\s*BAG", x[idx]))
    sa_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched)) * 50
    
    # SA with number only — assumed bags
    has_sa_num <- grepl("SA\\s*[0-9]+", x) & !has_sa_kg & !has_sa_bag
    idx <- which(has_sa_num)
    matched <- regmatches(x[idx], regexpr("SA\\s*[0-9]+", x[idx]))
    sa_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched)) * 50
    
    urea_kg <- rep(NA_real_, length(x))
    
    # UREA with KG explicit
    has_urea_kg <- grepl("UREA\\s*[0-9]+\\s*KG", x)
    idx <- which(has_urea_kg)
    if (length(idx) > 0) {
      matched <- regmatches(x[idx], regexpr("UREA\\s*[0-9]+\\s*KG", x[idx]))
      urea_kg[idx] <- as.numeric(gsub("[^0-9]", "", matched))
    }
    
    # calculate N P K only where at least one quantity is known
    has_any  <- !is.na(npk_kg) | !is.na(sa_kg) | !is.na(urea_kg)
    npk_kg0  <- ifelse(is.na(npk_kg),  0, npk_kg) * 0.15  # assuming 15-15-15
    sa_kg0   <- ifelse(is.na(sa_kg),   0, sa_kg)
    urea_kg0 <- ifelse(is.na(urea_kg), 0, urea_kg)  * 0.46
    
    N <- P <- K <- S <- rep(NA_real_, length(x))
    N[has_any] <- (npk_kg0[has_any] + sa_kg0[has_any] * 0.21 + urea_kg0[has_any]) / area_ha[has_any]
    P[has_any] <- (npk_kg0[has_any] * 0.436) / area_ha[has_any]
    K[has_any] <- (npk_kg0[has_any] * 0.83) / area_ha[has_any]
    S[has_any] <- sa_kg0[has_any] * 0.24

	ft <- ifelse(npk_kg0 > 0, "NPK", "")
	## there is no urea with amount
	ft <- ifelse(urea_kg0 > 0, paste0(ft, ";urea"), ft)
	ft <- ifelse(sa_kg0 > 0, paste0(ft, ";AS"), ft)
	ft <- gsub("^;", "", ft)
	ft[ft == ""] <- NA

	# todo:  fertilizer_type
    data.frame(N_fertilizer = N, P_fertilizer = P, K_fertilizer = K, S_fertilizer = S, fertilizer_type=ft)
  }
  
  # standardize crop names to terminag vocabulary
  clean_crop <- function(x) {
	x <- tolower(trimws(x))
    x[x == "corn"] <- "maize"
    x[grep("g nut|gnut", x)] <- "groundnut"
    x[x == "soyabean"] <- "soybean"
    x[x == "b beans"]  <- "bambara groundnut"
    x[x == "g pepper"] <- "chili pepper" # green pepper
    x[x == "casava"]   <- "cassava"
    x[x == "okro"]     <- "okra"
    x[x == "watermel"] <- "watermelon"
    x[x == "own"]  <- NA
    x <- gsub(" 30%", "", x)
    x[x == ""] <- NA	
    x
  }

  
  # standardize variety names
  # spelling variations confirmed by crop context
  clean_variety <- function(x) {
	x <- tolower(trimws(x))
    x[x %in% c("obaatapa", "obataapa", "obatampa", "0batampa")] <- "obatanpa"
    x[x == "okomasa"]                          <- "akomasa"
    x[x %in% c("afefe", "afifi")]              <- "afife"
    x[x %in% c("tops", "topsi", "tox")]        <- "topsii"
    x[x %in% c("chekopa", "chocopag")]         <- "chekopag"
    x[x %in% c("larbako", "labako")]           <- "laribako"
    x[x == "gbeno"]                            <- "gbena"
    # variety entered as crop name - set to NA
    x[x == "maize"] <- NA
    x[x == ""]      <- NA
    x
  }
 
  
  # reshape from wide to long format
  # each household has up to 6 subplots recorded as separate column groups (D2 section)
  # subplot numbering uses numbers (1,2,3) for separate plots and letters (1a,1b) for subplots

  ### RH: this is not what I see, and not consistent with the data. 
  ### We seem to have data on plots OR subplots, I do not see how we can tell

  ## RH first combine, to make it easier to understand processing needs. 
	plot_list <- lapply(1:6, function(i) {
		pattern <- paste0("\\.", i, "$")
		d <- nr[, grep(pattern, names(nr))]
		names(d) <- gsub(pattern, "", names(d))
		cbind(nr[, c("NAME_OF_INTERVIEWER", "QUESTIONNAIRE_ID", "A2")], 
			plot_id=i, d)
	})
	
	r <- do.call(carobiner::bindr, plot_list)

	d <- data.frame(
		trial_id = apply(r[, c("NAME_OF_INTERVIEWER", "QUESTIONNAIRE_ID", "plot_id")], 1, \(x) paste(trimws(x), collapse="-")),
		crop = clean_crop(r$MAIN_CROP),
		maj_crop = clean_crop(r$MAJOR_CROP),
		harvest = parse_harvest(r$TOTAL_HARVEST),
		# subplot area in local units (acres), converted to hectares
		field_size = r$SIZE_OF_SUB_PLOT * 0.405,
		variety = clean_variety(r$INDICATE_VARIETY),
		intercrops = clean_crop(r$MAJOR_INTERCROP),
		fert_text = trimws(r$QUANTITY_AND_TYPE),
		farmer_gender   = r$A2,
		country  = "Ghana",
		adm1 = "Northern Region",
		# coordinates are approximate for Northern Region centroid
		# GPS fields in survey were not captured digitally
		longitude       = -1.0,
		latitude        = 9.5,
		geo_from_source = FALSE,
		planting_date   = "2012",
		harvest_date    = "2012",
		on_farm         = TRUE,
		is_survey       = TRUE,
		irrigated       = FALSE,
		yield_part = "grain",
		yield_moisture = NA_real_,
		yield_isfresh = TRUE
	)

	d <- d[(!is.na(d$crop)) & (trimws(d$crop) != ""), ]
	d$yield <- d$harvest / d$field_size
  
    fert <- parse_fertilizer(d$fert_text, d$field_size)
    d <- cbind(d, fert)
  
  # assign correct yield_part for non-grain crops
  d$yield_part[d$crop == "yam"] <- "tubers"
  d$yield_part[d$crop == "cassava"] <- "roots"
  d$yield_part[d$crop == "groundnut"] <- "pod"


	d$maj_crop <- d$harvest <- d$fert_text <- NULL

  carobiner::write_files(meta, d, path = path)
}
