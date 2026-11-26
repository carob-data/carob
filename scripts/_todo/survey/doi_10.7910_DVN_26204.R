# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/BIDS "Livelihood Systems in Bangladesh" panel, 2000 round (62 villages; sam#87 links to the 1987 households).
# Parcel_2000.xls and CostRet_2000.xls are Excel 2.1 (BIFF2) worksheets, read via carobiner::read.excel() BIFF2 fallback.
# Two record types are combined in d (record_type_):
#  "parcel": Parcel_2000, one row per parcel x season. Boro season has two crop slots (crpbr1, crpbr2). land in ha, production in tonnes (codebook); yield = production * 1000 / land.
#  "cost_return": CostRet_2000, one parcel per household with inputs and value of output (no production quantity, so no yield). land in decimals (/247 -> ha).
# Crop codes: 1-11 are non-rice crops (codebook Cost&Retrn_val), 12 and higher are rice varieties (names decoded from the codebook). vrty = local / modern (HYV) / hybrid -> variety_type.
# Cost & return: N_fertilizer = urea kg * 0.46 / ha. Costs (Tk) divided by area -> BDT/ha. Labour = family + hired person-days for ploughing, sowing/transplanting, weeding, harvesting, threshing, per ha.
# Seed quantity (kg; "seed/for seedling") -> seed_rate kg/ha. crop loss qdmg in maund -> kg (1 maund = 37.32 kg) per ha.
# zila = adm2 (spellings adjusted to GADM), upazila = adm3, union = adm4, village = location; names from the codebook "Genral_val" sheet.

## ISSUES
# No coordinates; 62 villages (location) and upazila/zila need georeferencing; upazila/union spellings not checked against GADM.
# Crop year/season dates not given; planting_date not set.
# Cost & return records cannot be linked to a specific Parcel_2000 parcel (no parcel id); kept as separate rows without yield. The value of main product (prdr, Tk) is mapped to crop_value; no quantity.
# "Phosphet" (phq) product is not named (probably TSP, as in the 2008 round); kept as P_product_ (kg/ha) and not converted to P_fertilizer. Other chemical fertilizers only as cost (ofcst), so N_fertilizer is urea-N only.
# Crop codes 6 "Potato/Sweet potato", 10 "Mix spring harvest" and 11 "Others" have no single crop: crop left NA (label in crop_label_).
# Production unit for non-rice crops assumed tonnes (codebook says "in ton" for all). yield_part left NA for jute, vegetables, onion, spices, oilseeds/others.
# Parcel "irg" is the irrigation source of the parcel, not necessarily used in every season; irrigated only set to FALSE for "Without irrigation".
# Pest type codes (ptyp) mapped to labels in pest_species_; dmg mapped to stress (pests, flood, drought, hail, other).
# Unmapped: HHDataY2000 parts 1-5 (household aggregates, income), MemDemog_2000 (members), StockExp_2000 (storage, food, expenses); household background columns.
# write_files: no coordinates; season "aus" not in vocabulary; yield NA for cost_return rows and NA yield_part; extreme yields on very small parcels (vegetable, sugarcane > 100 t/ha); seed_rate up to 11 t/ha (probably potato seed tubers or a small-area typo) and N_fertilizer up to 874 kg/ha left as is, check.

## NEW VARIABLES
# record_type_: "parcel" (Parcel_2000) or "cost_return" (CostRet_2000)
# land_type_: flooding land type of the parcel (high, medium, low, very low land)
# crop_label_: original crop or rice variety label from the codebook
# crop_slot_: crop slot within the season (1, 2) for boro season in Parcel_2000
# hhid_1987_: household sample number in the 1987 round
# P_product_: amount of phosphate fertilizer product ("Phosphet"), kg/ha
# crop_loss_: crop loss due to damage, kg/ha
# pest_species_: labelled insect/pest causing damage



## NOT PROCESSED
# HHDataY2000.xls_part1.xls: not read. sheet HHDataY2000.xls_part1: sample, survey, sam#87, splithh, village, union, upazila, ctrl, zila, Ecosystem, religion, selfeco, .... Observation records are missing from d until this file is standardized.
# HHDataY2000.xls_part2.xls: not read. sheet HHDataY2000.xls_part2: sample, survey, sam#87, splithh, village, union, upazila, Ecosystem, zila, edwrkr, edwrm, edwrf, .... Observation records are missing from d until this file is standardized.
# HHDataY2000.xls_part3.xls: not read. sheet HHDataY2000.xls_part3: sample, survey, sam#87, splithh, village, union, upazila, Ecosystem, zila, homsland, grdnland, clvtland, .... Observation records are missing from d until this file is standardized.
# HHDataY2000.xls_part4.xls: not read. sheet HHDataY2000.xls_part4: sample, survey, sam#87, splithh, village, union, upazila, Ecosystem, zila, hyvinfo, ao_know, ao_vilv, .... Observation records are missing from d until this file is standardized.
# HHDataY2000.xls_part5.xls: not read. sheet HHDataY2000.xls_part5 (of 2): Filename:    HHDataY2000_part 5. Observation records are missing from d until this file is standardized.
# Project details.xlsx: not read. sheet PROJECT: Title of the Study, ...2, Livelihood Systems in Bangladesh --for each year of study and the panel households created for all the years of study. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"Livelihood Systems in Bangladesh (2000)

a)  Understand the operation of the Bangladesh rural household economy;  b) Analyze the drivers of change in income distribution and poverty;  c)  Prepare technical background papers to facilitating policy dialogues under the project 'Poverty Elimination Through Rice Research Assistance (PETRRA)' in Bangladesh;  d) Assess the impact of rice research on poverty reduction in Bangladesh using the Sustainable Livelihood Framework; and  e)  Estimating parameters on the determinants of rural household incomes for use in poverty mapping for Bangladesh."

	uri <- "doi:10.7910/DVN/26204"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "PETRRA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 45,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "Documentation of 2000_Bangladesh.xls"]
	# value labels from the codebook: variable name (filled down), code, label
	codes <- function(sheet, var) {
		r <- carobiner::read.excel(f1, sheet=sheet, col_names=FALSE)
		nm <- r[[1]]
		nm <- nm[!is.na(nm)][cumsum(!is.na(nm))]
		k <- nm %in% var & !is.na(r[[4]])
		data.frame(code=as.numeric(r[[2]][k]), label=trimws(r[[4]][k]))
	}
	lab <- function(x, cds) {
		cds$label[match(x, cds$code)]
	}

	# crop codes 1-12 (sheet Cost&Retrn_val); 13 and higher are rice varieties (Parcel_val)
	crops <- data.frame(code=1:11,
		crop = c("wheat", "jute", "sugarcane", "oilseed", "pulse", NA, "onion", "spices", "vegetable", NA, NA),
		yield_part = c("grain", NA, "stems", NA, "seed", NA, "bulbs", NA, NA, NA, NA)
	)
	crplab <- rbind(codes("Cost&Retrn_val", "crp")[1:12, ], codes("Parcel_val", c("crpbr1/crpbr2/", "crpas/crpam")))
	vtype <- c("local", "modern", "hybrid")
	soils <- c("loam", "sandy loam", "clay", "clay loam", "sand")
	stress <- c("none", "pests", "flood", "drought", "hail", "other")

	setcrop <- function(d, code) {
		d$crop_label_ <- lab(code, crplab)
		i <- match(code, crops$code)
		d$crop <- ifelse(code >= 12, "rice", crops$crop[i])
		d$yield_part <- ifelse(code >= 12, "grain", crops$yield_part[i])
		d$variety <- ifelse(code >= 12, d$crop_label_, NA)
		d
	}

	## Parcel_2000
	r1 <- carobiner::read.excel(ff[basename(ff) == "Parcel_2000.xls"])
	slots <- data.frame(season=c("boro", "boro", "early wet", "aman"), sfx=c("br1", "br2", "as", "am"), slot=c(1, 2, 1, 1))
	d1 <- lapply(1:4, function(i) {
		s <- slots$sfx[i]
		data.frame(
			hhid = as.character(r1$sample),
			hhid_1987_ = as.character(r1[["sam#87"]]),
			field_id = as.character(r1$plotnum),
			season = slots$season[i],
			crop_slot_ = slots$slot[i],
			crop_code = r1[[paste0("crp", s)]],
			variety_type = vtype[match(r1[[paste0("vrty", s)]], 1:3)],
			production = r1[[paste0("prd", s)]],
			field_size = r1$land,
			land_ownedby = lab(r1$owner, codes("Parcel_val", "owner")),
			land_tenure = lab(r1$tanr, codes("Parcel_val", "tanr")),
			soil_texture = soils[match(r1$soil, 1:5)],
			land_type_ = lab(r1$fld, codes("Parcel_val", "fld")),
			irrigation_source = lab(r1$irg, codes("Parcel_val", "irg")),
			village = r1$village,
			union = r1$union,
			upazila = r1$upazila,
			zila = r1$zila,
			hh_size = r1$hhsize,
			farmland_owned = r1$ownland,
			cropland = r1$clvt
		)
	})
	d1 <- do.call(rbind, d1)
	d1 <- d1[d1$crop_code > 0, ]
	d1 <- setcrop(d1, d1$crop_code)
	# production in tonnes, area in ha
	d1$yield <- d1$production * 1000 / d1$field_size
	d1$record_type_ <- "parcel"

	## CostRet_2000
	r2 <- carobiner::read.excel(ff[basename(ff) == "CostRet_2000.xls"])
	# land in decimals
	ha <- r2$land / 247
	d2 <- data.frame(
		hhid = as.character(r2$sample),
		hhid_1987_ = as.character(r2[["sam#87"]]),
		season = c("early wet", "aman", "boro", "rabi")[match(r2$season, 1:4)],
		variety_type = vtype[match(r2$vrty, 1:3)],
		field_size = ha,
		land_prep_times = as.integer(r2$pln),
		seed_rate = r2$sdq / ha,
		N_fertilizer = r2$urq * 0.46 / ha,
		P_product_ = r2$phq / ha,
		irrigation_source = lab(r2$irg, codes("Cost&Retrn_val", "irg")),
		weeding_times = as.integer(r2$wdn),
		insecticide_times = as.integer(r2$ppn),
		labour = (r2$flpl + r2$hlpl + r2$flsd + r2$hlsd + r2$flwd + r2$hlwd + r2$flh + r2$hlh + r2$flt + r2$hlt) / ha,
		land_prep_cost = (r2$plcst + r2$ptcst) / ha,
		seed_cost = r2$sdcst / ha,
		fertilizer_cost = (r2$urcst + r2$phcst + r2$ofcst) / ha,
		OM_cost = r2$mncst / ha,
		irrigation_cost = r2$ircst / ha,
		insecticide_cost = r2$ppcst / ha,
		insecticide_app_cost = r2$pacst / ha,
		crop_value = r2$prdr / ha,
		stress = stress[match(r2$dmg, 0:5)],
		crop_loss_ = r2$qdmg * 37.32 / ha,
		pest_species_ = lab(r2$ptyp, codes("Cost&Retrn_val", "ptyp")),
		currency = "BDT",
		village = r2$village,
		union = r2$union,
		upazila = r2$upazila,
		zila = r2$zila,
		hh_size = r2$hhsize,
		farmland_owned = r2$ownland,
		cropland = r2$clvt
	)
	d2 <- setcrop(d2, r2$crp)
	d2$record_type_ <- "cost_return"

	d <- carobiner::bindr(d1, d2)

	d$irrigated <- ifelse(d$irrigation_source == "Without irrigation", FALSE, NA)
	irmap <- c("Without irrigation"="none", "Low lift pump (LLP)"="surface water", "Shallow tube well (STW)"="groundwater",
		"Deep tube well(DTW)"="groundwater", "Irrigation canal project of Govt.(WDB)"="canal", "Local irrigation system"="unknown")
	irlift <- c("Low lift pump (LLP)"="motorized pump", "Shallow tube well (STW)"="motorized pump", "Deep tube well(DTW)"="motorized pump")
	d$irrigation_lifting <- unname(irlift[d$irrigation_source])
	d$irrigation_source <- unname(irmap[d$irrigation_source])

	d$location <- lab(d$village, codes("Genral_val", "village"))
	d$adm4 <- lab(d$union, codes("Genral_val", "union"))
	d$adm3 <- lab(d$upazila, codes("Genral_val", "upazila"))
	d$adm2 <- lab(d$zila, codes("Genral_val", "zila"))
	gadm <- c("B. Baria"="Brahamanbaria", "B.Baria"="Brahamanbaria", "Mouvi-Bazar"="Maulvibazar", "Moulovi-Bazar"="Maulvibazar", "Lakhmipur"="Lakshmipur", "Jhalakhati"="Jhalokati", "Perojpur"="Pirojpur", "Jhenaidaha"="Jhenaidah", "Kustia"="Kushtia", "Nababganj"="Nawabganj", "Panchaghar"="Panchagarh", "Netrokona"="Netrakona", "Munsiganj"="Munshiganj")
	i <- d$adm2 %in% names(gadm)
	d$adm2[i] <- gadm[d$adm2[i]]
	d$country <- "Bangladesh"
	geo <- data.frame(
		adm2 = c("Barisal", "Bhola", "Jhalokati", "Patuakhali", "Pirojpur", "Brahamanbaria", "Chandpur", "Chittagong", "Comilla", "Cox's Bazar", "Feni", "Lakshmipur", "Noakhali", "Habiganj", "Maulvibazar", "Sunamganj", "Sylhet", "Dhaka", "Faridpur", "Gopalganj", "Jamalpur", "Kishoreganj", "Madaripur", "Manikganj", "Munshiganj", "Mymensingh", "Narsingdi", "Netrakona", "Rajbari", "Shariatpur", "Sherpur", "Tangail", "Bagerhat", "Chuadanga", "Jessore", "Jhenaidah", "Khulna", "Kushtia", "Magura", "Meherpur", "Narail", "Satkhira", "Bogra", "Dinajpur", "Gaibandha", "Joypurhat", "Kurigram", "Lalmonirhat", "Naogaon", "Natore", "Nawabganj", "Nilphamari", "Pabna", "Panchagarh", "Rajshahi", "Rangpur", "Sirajganj", "Thakurgaon"),
		latitude = c(22.70, 22.69, 22.64, 22.36, 22.58, 23.96, 23.23, 22.36, 23.46, 21.43, 23.02, 22.94, 22.87, 24.38, 24.48, 25.07, 24.90, 23.81, 23.60, 23.01, 24.92, 24.44, 23.16, 23.86, 23.54, 24.75, 23.93, 24.88, 23.76, 23.24, 25.02, 24.25, 22.65, 23.64, 23.17, 23.54, 22.82, 23.90, 23.49, 23.76, 23.17, 22.72, 24.85, 25.63, 25.33, 25.10, 25.81, 25.92, 24.81, 24.42, 24.60, 25.93, 24.01, 26.34, 24.37, 25.75, 24.45, 26.03),
		longitude = c(90.37, 90.65, 90.20, 90.33, 89.97, 91.11, 90.66, 91.81, 91.18, 92.00, 91.40, 90.83, 91.10, 91.41, 91.77, 91.40, 91.87, 90.41, 89.84, 89.83, 89.95, 90.78, 90.20, 90.00, 90.53, 90.40, 90.72, 90.73, 89.64, 90.35, 90.02, 89.92, 89.79, 88.84, 89.21, 89.17, 89.54, 89.12, 89.42, 88.63, 89.50, 89.07, 89.37, 88.64, 89.53, 89.02, 89.65, 89.45, 88.94, 88.99, 88.27, 88.85, 89.24, 88.55, 88.60, 89.25, 89.70, 88.46)
	)
	d <- merge(d, geo, by="adm2", all.x=TRUE)
	d$geo_from_source <- FALSE

	d$crop_code <- d$production <- NULL
	d$village <- d$union <- d$upazila <- d$zila <- NULL

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
