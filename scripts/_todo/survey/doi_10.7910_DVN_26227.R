# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/FASID/TODAI survey on the livelihood system of rural households in the Philippines, 2000-01 (Laguna, Nueva Ecija and Iloilo provinces).
# Codes from "Coding Manual LIVELIHOOD Phils_2000-01.docx".
# One row per household x season rice crop from the costs and returns files COSRET4-M (harvest, threshing, production in kg, price), joined with
#  COSRET1-M (land preparation, crop establishment, seed), COSRET2-M (fertilizer, irrigation) and COSRET3-M (weeding, pesticide) by household, village and season (sheet "All").
# Area in ha.
# Fertilizer: NKG is elemental N from straight N fertilizers (NPR, its price per kg, equals the urea price / 0.46), NPKG is N + P2O5 from NP fertilizers (16-20-0: 18 kg per 50 kg bag) and NPKKG is N + P2O5 + K2O from NPK fertilizers (14-14-14). Kept as new variables (kg/ha).
# Negative values (-99) are missing values.

## ISSUES
# No coordinates. Village codes 1-3 are not documented (the manual only names Pandan, Dingle, Iloilo, here assumed to be code 4); probably the TIDP villages (Tubuan, Gabaldon, Signe), see doi:10.7910/DVN/26294. Kept as village_code_.
# Season codes (SSN 1, 2, 3) are not documented: season not mapped (season_code_).
# N/P/K rates not computed: NP and NPK nutrient totals would need the product grades (16-20-0 and 14-14-14 are the most common FRTYPE entries, but others are listed).
# Rice variety codes (RVAR) have no code list; kept as variety_code.
# COSRET2-M has a separate "PANDAN" sheet with a different layout; the "All" sheet is used. Duplicate household x village x season records are reduced to the first.
# OPAR (parcel characteristics and wet/dry/third crop harvests) not used: crop codes are undocumented and it cannot be linked to seasons in COSRET.
# Labour (family and hired days per operation) not mapped: most entries are empty, the per ha sum has a median of 1.5 days/ha, and some values are very large (1000 days).
# Extreme values not removed: yield up to 57.6 t/ha and seed rate up to 5333 kg/ha on small parcels.
# write_files: no coordinates; out of bounds seed_rate; high rice yield; NA yield.
# Unmapped: all other modules (HUCAP, ASSETS, BORROW, LENDNG, EMPINC1-4, INCHMSTD, INCLDMCH, IPM, LABMDYS, PRODISP, MKTCRP, LIVESTK, TRUST, ...).

## NEW VARIABLES
# village_code_: village code (VIL)
# season_code_: season code (SSN), not documented
# N_straight_: elemental N from straight N fertilizers (kg/ha)
# NP_nutrients_: N + P2O5 from NP fertilizers (kg/ha)
# NPK_nutrients_: N + P2O5 + K2O from NPK fertilizers (kg/ha)
# pesticide_times_: number of sprays


carob_script <- function(path) {

"
A Survey on Livelihood System of Rural Household in the Philippines

The study examines the changes in the livelihood of the rural household that comprises data on household's basic information, migration, landholdings, costs and return of rice production, sources of income, borrowing, lending, assets holding, decision making in farming, calamity and accident and the coping mechanism, Integrated Pest Management (IPM), formal and informal relationship with different agencies/institutions, and about trust and sociability.
"

	uri <- "doi:10.7910/DVN/26227"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 30,
		carob_effort = 0.5
	)

	# header row is the row starting with HHNO
	rd <- function(fname) {
		x <- carobiner::read.excel(ff[basename(ff) == fname], sheet="All", col_names=FALSE, .name_repair="minimal")
		h <- which(grepl("^HHNO", x[[1]]))[1]
		nms <- make.unique(as.character(unlist(x[h, ])))
		x <- x[-(1:h), ]
		names(x) <- nms
		x <- x[grepl("^[0-9]+$", x$HHNO), ]
		x[!duplicated(paste(x$HHNO, x$VIL, x$SSN)), ]
	}
	num <- function(x) {
		x <- trimws(as.character(x))
		x[!grepl("^-?[0-9]*\\.?[0-9]+$", x)] <- NA
		x <- as.numeric(x)
		x[x < 0] <- NA
		x
	}
	sumNA <- function(x) {
		x <- sapply(x, num)
		ifelse(rowSums(!is.na(x)) > 0, rowSums(x, na.rm=TRUE), NA)
	}

	r4 <- rd("COSRET4-M.xls")
	k <- paste(r4$HHNO, r4$VIL, r4$SSN)
	m <- function(x) x[match(k, paste(x$HHNO, x$VIL, x$SSN)), ]
	r1 <- m(rd("COSRET1-M.xls"))
	r2 <- m(rd("COSRET2-M.xls"))
	r3 <- m(rd("COSRET3-M.xls"))

	ha <- num(r4$AREA)
	ha[ha == 0] <- NA
	vil <- num(r4$VIL)

	d <- data.frame(
		hhid = paste(vil, num(r4$HHNO), sep="_"),
		village_code_ = vil,
		season_code_ = num(r4$SSN),
		crop = "rice",
		variety_code = as.character(num(r1$RVAR)),
		field_size = ha,
		yield = num(r4$PRODKG) / ha,
		crop_price = num(r4$PRICEKG),
		land_prep_times = as.integer(num(r1$NUMTILL)),
		seed_rate = num(r1$AMTSEED) / ha,
		seed_cost = num(r1$SEDCOST) / ha,
		N_straight_ = num(r2$NKG) / ha,
		NP_nutrients_ = num(r2$NPKG) / ha,
		NPK_nutrients_ = num(r2$NPKKG) / ha,
		irrigation_cost = sumNA(r2[, c("IRGCOST", "IRGGOV")]) / ha,
		weeding_times = as.integer(num(r3$NOWEED)),
		herbicide_cost = num(r3$HRBCOST) / ha,
		pesticide_cost = num(r3$PSTICOST) / ha,
		pesticide_times_ = num(r3$NUMSP),
		currency = "PHP",
		yield_part = "grain",
		country = "Philippines"
	)
	d$irrigation_source <- c("gravity", "pump", "none")[match(num(r2$IRGSRC), 1:3)]
	d$irrigated <- ifelse(is.na(d$irrigation_source), NA, d$irrigation_source != "none")
	d$location <- ifelse(vil %in% 4, "Pandan", NA)
	d$adm2 <- ifelse(vil %in% 4, "Dingle", NA)
	d$adm1 <- ifelse(vil %in% 4, "Iloilo", NA)
	d$crop_price[d$crop_price %in% 0] <- NA
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
