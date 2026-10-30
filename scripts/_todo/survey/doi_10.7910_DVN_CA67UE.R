# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey (Jan-Feb 2016) of 652 rice farmers in Puri district, Odisha, India, recalling the kharif (wet) season 2015
# (Devkota et al., 2020, J. Cleaner Production 244: 118835). Stratified by establishment method: dry direct-seeded (DSR),
# manual puddled transplanting (PTR) and machine transplanting (MTR). One record per household (selected rice patch).
# Single sheet "Sheet1"; the last row is empty and is dropped.
# Yield is "Grain yield (kg/ha)" (= Total fresh grain yield (kg) / patch area, checked).
# N, P, K, S, Zn (kg/ha) are the authors' elemental columns (checked: P = P2O5/2.29, K = K2O/1.2051 per ha). B = Borax kg x 11% / patch area.
# Dates are Excel serials that the authors derived from reported month & week (1st week -> 4th, 2nd -> 11th, 3rd -> 18th, 4th -> 26th);
#   planting_date = sowing date for DSR and nursery establishment date for PTR/MTR; transplanting_date for PTR/MTR.
# Rain (mm) is rainfall from land preparation to harvest from Odisha meteorological stations (paper, Methods 2.3).
# irrigation_amount = "1.Total depth of irrigation (mm)", computed by the authors from number x approximate depth of irrigations.

## ISSUES
# No coordinates. Human must georeference the 102 villages (location) within blocks (block_) and gram panchayats (gram_panchayat_) of Puri district.
#   Blocks and gram panchayats are not GADM admin units, so they are not put in adm3/adm4.
# Dates are week-level approximations (see NOTES), not exact days.
# 3 duplicated FID values; kept as separate records.
# Not processed (derived/modelled by the authors): straw yield (40% harvest index), methane/CO2 emissions (IPCC), water productivity,
#   NUE/PUE/PFP indicators, all USD/ha cost and profit columns (missing operation costs were imputed with block averages, paper 2.2),
#   SRP scores and percentile categories, "nutrient 0" (farmer-guessed) yield.
# Not processed: per-application fertilizer products, DAP and amounts (basal A-D, topdressings 1-3; some product cells hold numbers or
#   "during puddling"); pesticide product names and amounts (ml or g, mixed); labor days by operation; land preparation machinery;
#   organic amendment amounts (three columns, unclear units); women empowerment, child labour, health and safety items.
# fertilizer_type lists product names; "Gromor", "Anusara", "Multiplex", "Hizinc", "Biozime", "Nagarjuna" are brands of unknown composition (kept as "unknown").
# previous_crop "Others(name)" (44 records) left NA.
# OM_type "green manure" and "straw" are not in the terminag OM list; kept.
# write_files: missing longitude/latitude/geo_from_source; unknown variables (see NEW VARIABLES);
#   out of bounds irrigation_amount up to 1100 mm (authors' number x depth estimate; check).

## NEW VARIABLES
# block_: community development block (Puri district)
# gram_panchayat_: gram panchayat (village council area)
# cropping_pattern_: annual cropping pattern as reported (e.g. "Rice-Mung")
# data_source_: "Field Survey" or "Farmer's Diary"
# water_regime_: water regime during kharif (e.g. "Mostly flooded except 1-2 drying", "Drought")
# water_management_: AWD, Non-AWD or Rainfed
# harvest_method_: combine harvester, manual or reaper
# rice_area_: total rice area of the household, ha


carob_script <- function(path) {

"
Assessing alternative crop establishment methods with a sustainability lens in rice production systems of Eastern India

The dataset includes the raw data of the published manuscript https://doi.org/10.1016/j.jclepro.2019.118835.
"

	uri <- "doi:10.7910/DVN/CA67UE"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1016/j.jclepro.2019.118835",
		project = NA,
		design = "stratified random sampling of blocks and villages; purposive selection of DSR, PTR and MTR households",
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Rdata_Sustainability indicators_Odisha data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet1", na=c("", "NA", "N.A."))
	r <- r[!is.na(r$FID), ]

	tech <- r[["Selected Technology"]]
	exdate <- function(x) as.character(as.Date(as.numeric(x), origin="1899-12-30"))
	sow <- exdate(r[["Rice transplanting or sowing date"]])
	nursery <- exdate(r[["Nursery establishment date"]])
	transplanted <- tech %in% c("Manual transplanting (Random)", "Machine transplanting (MTR)")
	area <- r[["haRice area of selected patch"]]

	d <- data.frame(
		hhid = as.character(r$FID),
		country = "India",
		adm1 = "Odisha",
		adm2 = r$District,
		block_ = r$Block,
		gram_panchayat_ = r$GP,
		location = r$Village,
		data_source_ = r[["Data source"]],
		age = r$Age,
		sex = tolower(r$Gender),
		education = r$Education,
		farmland = r[["haTotal land holding of the farmer"]],
		rice_area_ = r[["haTotal rice cultivated area :"]],
		cropping_pattern_ = r[["Cropping-Pattern"]],
		crop = "rice",
		variety = r[["Rice variety in this parcel"]],
		season = "wet",
		field_size = area,
		planting_method = ifelse(transplanted, "transplanting", ifelse(tech %in% "DSR", "direct seeding", NA)),
		transplanting_method = ifelse(tech %in% "Machine transplanting (MTR)", "mechanized", ifelse(tech %in% "Manual transplanting (Random)", "manual", NA)),
		planting_date = ifelse(transplanted, nursery, sow),
		transplanting_date = ifelse(transplanted, sow, NA),
		harvest_date = exdate(r[["Rice harvesting date"]]),
		yield = r[["Grain yield (kg/ha)"]],
		crop_price = r[["Grain price (per kg)"]],
		seed_rate = r[["Seed rate, kg per ha"]],
		seed_source = r[["Seed source"]],
		seed_treatment = r[["If yes, seed treatment with"]],
		N_fertilizer = r[["Elemental N (kg/ha)"]],
		P_fertilizer = r[["Elemental P (kg/ha)"]],
		K_fertilizer = r[["Elemental K (kg/ha)"]],
		S_fertilizer = r[["Sulphur application (kg/ha)"]],
		Zn_fertilizer = r[["Zinc application (kg/ha)"]],
		B_fertilizer = r[["BoronApplication amount(kg)"]] * 0.11 / area,
		irrigated = r[["Did you apply any irrigation during the rice growing period in the season?"]] == "Yes",
		irrigation_number = as.integer(r[["Total Number of irrigation"]]),
		irrigation_amount = r[["1.Total depth of irrigation (mm)"]],
		irrigation_source = r[["21. Irrigation source"]],
		water_management_ = r[["22. Irrigation method"]],
		water_regime_ = r[["e. Water regime during the cultivation in kharif season...301"]],
		rain = r[["2. Rainfall from the date of land preparation to rice harvest (mm)"]],
		OM_used = r[["b. Did you apply any organic amendment?"]] == "Yes",
		OM_type = r[["c. Type of organic amendments returned to the field (e.g., straw/residue, compost, FYM or green manure"]],
		herbicide_used = r[["HerbicideDo you apply herbicide to control weed?"]] == "Yes",
		insecticide_used = r[["InsecticideDo you apply insecticide to control any insect?"]] == "Yes",
		fungicide_used = r[["FungicideDo you apply fungicide to control any diseases?"]] == "Yes",
		harvest_method_ = r[["25. Method of harvesting"]]
	)
	d$currency <- "INR"
	d$B_fertilizer[is.na(r[["BoronFerilizer name"]])] <- 0
	d$seed_treatment[d$seed_treatment %in% "Chemical (name not known)"] <- "chemical"
	d$seed_treatment <- tolower(d$seed_treatment)

	om <- c("Compost"="compost", "FYM/Compost"="farmyard manure; compost", "Green manure"="green manure", "Straw"="straw")
	d$OM_type <- as.vector(om[d$OM_type])

	prev <- c("Fallow"="none", "Pulses"="pulse", "Vegetables"="vegetable",
		"Rice with continuous flooding throughout season"="rice", "Rice with intermittent flooding"="rice")
	d$previous_crop <- as.vector(prev[r[[grep("crop before kharif", names(r))]]])

	## fertilizer products used (all basal and topdressing columns)
	fn <- grep("Ferilizer name", names(r))
	ftype <- c("Urea"="urea", "DAP"="DAP", "MOP"="KCl", "AmmoniumSulphate"="DAS", "Zincsulphate"="ZnSO4", "Borax"="Borax")
	fert <- apply(r[, fn], 1, function(x) {
		x <- x[!is.na(x) & !grepl("^[0-9]+$|puddling", x)]
		x <- unique(ifelse(x %in% names(ftype), ftype[x], "unknown"))
		if (length(x) == 0) NA else paste(x, collapse="; ")
	})
	d$fertilizer_type <- fert
	d$fertilizer_used <- !is.na(fert)

	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
