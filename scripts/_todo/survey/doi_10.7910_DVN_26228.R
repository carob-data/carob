# R script for "carob"
# license: GPL (>=3)

## NOTES
# PORIA survey of 151 rice farm households in Laguna, Nueva Ecija and Quezon (Philippines), wet season 2006 (files without suffix) and dry season 2006/07 (files with suffix "_1"), interviews March and May 2007.
# Codes from "Manual of documentation PORIA.doc": CROPES / Methodcrop 1 transplanting, 2 direct seeding; SEEDBED 1 dapog, 2 wet bed, 3 dry bed.
# One record per household and season. Area (ha), harvest (kg, fresh palay) and YIELD (kg/ha = TotHarvestkg / area) from PRODUCTION; PRICE is the palay price (PHP/kg).
# Fertilizer files give N, P and K (elemental, e.g. 16-20-0 has P% 8.728) in kg/ha per application; these are summed per household and season.
# Pesticide files give active ingredient per ha (AIha, kg or L per ha) per application; these are summed per household and season. Households absent from a fertilizer or pesticide file are taken as not having used that input (amount 0).
# The wet season fungicide file has no application number (FU1APLCN empty); each row is counted as one application.
# Wet season harvest dates (HARVMO) have no year; 2006 is used (the survey season). Dry season planting dates are from GENERAL INFO_1 (m/d/y).
# Ageseedling (dry season, days) is used as transplanting_days for transplanted fields.
# Farmer characteristics (age, sex, household size, education) are from the wet season GENERAL INFO and attached to the wet season records only.

## ISSUES
# No coordinates; province (adm1), municipality (adm2) and barangay (location) given. Human must georeference.
# Fertilizer: applications with unknown grade (e.g. "Granular", "Nitrogen 45") have 0 or NA nutrients in the source; a household-season total is NA if any application is NA.
# Pesticide AI amounts mix kg and L of active ingredient as computed by the data providers; not verified.
# Pesticide active ingredients are kept as text in *_ai_ variables; they are not yet standardised to the herbicide/insecticide/fungicide_product vocabularies (wet season herbicide/fungicide/molluscicide/rodenticide files only have brand names).
# Variety names are raw text; several varieties per household are joined with "; " while production is per household.
# Dates are free text in mixed formats; dates outside Jan 2006 - May 2007 (e.g. "12/16/07", "3006-08") are set to NA rather than corrected.
# Three dry season records have area 0, so yield is NA.
# write_files: missing longitude/latitude; new variables listed below.
# Not mapped: labour and costs per activity, land preparation, irrigation, seedbed, weeding, handpicking, capital, farm investment, parcels/tenure, pest knowledge/management/health questions, training.

## NEW VARIABLES
# seedbed_method_: method of seedbed preparation (dapog, wet bed, dry bed)
# education_years_: education of the respondent (years)
# insecticide_ai_, herbicide_ai_, fungicide_ai_, molluscicide_ai_, rodenticide_ai_: active ingredients (or brand names) as given
# molluscicide_used_, rodenticide_used_: was a molluscicide / rodenticide used
# molluscicide_amount_, rodenticide_amount_: active ingredient applied (kg/ha), summed over applications


carob_script <- function(path) {

"
Policy Oriented Research Impact Assessment (PORIA):  Economic Assessment of a Change in Chemical Registration Policy Due to IRRI Research on Pesticide Use and Philippine Farmers' Health

This is a follow-up survey on Rola & Pingali's 1989/90 pesticide health cost study. The survey documented changes in farmers' knowledge about, attitude towards, and practices in pesticide use.
"

	uri <- "doi:10.7910/DVN/26228"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = "quota sample of 151 rice farm households, randomly selected from lists of the Municipal Agriculturists",
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.6
	)

	rd <- function(name) carobiner::read.excel(ff[basename(ff) == name])

	vsum <- function(id, x, ids) {
		s <- tapply(x, id, sum)
		out <- as.vector(s[as.character(ids)])
		out[!(ids %in% id)] <- 0
		out
	}
	vtimes <- function(id, app, ids) {
		if (all(is.na(app))) app <- seq_along(id)
		n <- tapply(app, id, function(i) length(unique(i)))
		out <- as.vector(n[as.character(ids)])
		out[is.na(out)] <- 0L
		as.integer(out)
	}
	vtext <- function(id, x, ids) {
		x <- trimws(as.character(x))
		x[x %in% c("", "0", "dk")] <- NA
		s <- tapply(x, id, function(i) {
			i <- unique(i[!is.na(i)])
			if (length(i) == 0) NA else paste(i, collapse="; ")
		})
		as.vector(s[as.character(ids)])
	}

	pest <- function(ids, ins, herb, fun, mol, rod, ins_ai, herb_ai, fun_ai, mol_ai, rod_ai, apps) {
		data.frame(
			insecticide_used = ids %in% ins$ID,
			insecticide_times = vtimes(ins$ID, ins[[apps[1]]], ids),
			insecticide_amount = vsum(ins$ID, ins$AIha, ids),
			insecticide_ai_ = vtext(ins$ID, ins[[ins_ai]], ids),
			herbicide_used = ids %in% herb$ID,
			herbicide_times = vtimes(herb$ID, herb[[apps[2]]], ids),
			herbicide_amount = vsum(herb$ID, herb$AIha, ids),
			herbicide_ai_ = vtext(herb$ID, herb[[herb_ai]], ids),
			fungicide_used = ids %in% fun$ID,
			fungicide_times = vtimes(fun$ID, fun[[apps[3]]], ids),
			fungicide_amount = vsum(fun$ID, fun$AIha, ids),
			fungicide_ai_ = vtext(fun$ID, fun[[fun_ai]], ids),
			molluscicide_used_ = ids %in% mol$ID,
			molluscicide_amount_ = vsum(mol$ID, mol$AIha, ids),
			molluscicide_ai_ = vtext(mol$ID, mol[[mol_ai]], ids),
			rodenticide_used_ = ids %in% rod$ID,
			rodenticide_amount_ = vsum(rod$ID, rod$AIha, ids),
			rodenticide_ai_ = vtext(rod$ID, rod[[rod_ai]], ids)
		)
	}

	pdate <- function(x, year=NA) {
		x <- tolower(trimws(gsub(",", " ", x)))
		x <- gsub("nobember", "november", gsub("^sept ", "sep ", x))
		x <- gsub("([a-z])([0-9])", "\\1 \\2", gsub("\\s+", " ", x))
		if (!is.na(year)) {
			i <- grepl("^[a-z]+ [0-9]{1,2}$", x)
			x[i] <- paste(x[i], year)
		}
		out <- as.character(as.Date(x, "%b %d %Y"))
		i <- is.na(out)
		out[i] <- as.character(as.Date(x[i], "%m/%d/%Y"))
		i <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$", x)
		out[i] <- as.character(as.Date(x[i], "%m/%d/%y"))
		i <- grepl("^[a-z]+ [0-9]{4}$", x)
		out[i] <- format(as.Date(paste("1", x[i]), "%d %b %Y"), "%Y-%m")
		out[!is.na(out) & (substr(out, 1, 7) < "2006-01" | substr(out, 1, 7) > "2007-05")] <- NA
		out
	}

	estab <- function(x) unname(c("1"="transplanting", "2"="direct seeding")[as.character(x)])
	seedbed <- function(x) unname(c("1"="dapog", "2"="wet bed", "3"="dry bed")[as.character(x)])

	## wet season 2006
	g <- rd("GENERAL INFO.xlsx")
	p <- rd("PRODUCTION.xlsx")
	s <- rd("SEED.xlsx")
	f <- rd("FERTILIZER.xlsx")
	ids <- p$ID

	vars <- apply(s[, c("RICEVAR1", "RICEVAR2", "RICEVAR3", "RICEVAR4")], 1, function(v) {
		v <- trimws(v[!is.na(v) & v != "0"])
		if (length(v) == 0) NA else paste(v, collapse="; ")
	})
	gi <- match(ids, g$ID)
	si <- match(ids, s$ID)

	dw <- data.frame(
		hhid = as.character(ids),
		season = "wet",
		adm1 = g$PROV[gi],
		adm2 = g$MUN[gi],
		location = g$BRGY[gi],
		age = g$AGE[gi],
		farmer_gender = unname(c(M="male", F="female")[g$SEX[gi]]),
		hh_size = g$HHSIZE[gi],
		education_years_ = g$EDUC[gi],
		field_size = p$AREA,
		yield = p$YIELD,
		crop_price = p$PRICE,
		harvest_date = pdate(p$HARVMO, 2006),
		planting_date = pdate(s$CROPDATE)[si],
		planting_method = estab(s$CROPES[si]),
		seedbed_method_ = seedbed(s$SEEDBED[si]),
		variety = vars[si],
		N_fertilizer = vsum(f$ID, f$Nha, ids),
		P_fertilizer = vsum(f$ID, f$Pha, ids),
		K_fertilizer = vsum(f$ID, f$Kha, ids)
	)
	dw <- cbind(dw, pest(ids, rd("INSECTICIDE.xlsx"), rd("HERBICIDE.xlsx"), rd("FUNGICIDE.xlsx"),
		rd("MOLLUSCICIDE.xlsx"), rd("RODENTICIDE.xlsx"),
		"Active Ing", "H1TYPE", "FU1TYPE", "M1TYPE", "R1TYPE", c("I1APLCN", "H1APLCN", "FU1APLCN")))

	## dry season 2006/07
	g1 <- rd("GENERAL INFO_1.xlsx")
	p1 <- rd("PRODUCTION_1.xlsx")
	s1 <- rd("SEED_1.xlsx")
	f1 <- rd("FERTILIZER_1.xlsx")
	ids <- p1$ID
	gi <- match(ids, g1$ID)

	mcrop <- c("Transplanting (tanim)"="transplanting", "Direct seeding (sabog tanim)"="direct seeding")[g1$Methodcrop[gi]]
	ageseed <- g1$Ageseedling[gi]
	ageseed[mcrop != "transplanting"] <- NA

	dd <- data.frame(
		hhid = as.character(ids),
		season = "dry",
		adm1 = g1$Prov[gi],
		adm2 = g1$Mun[gi],
		location = g1$Bgy[gi],
		age = NA,
		farmer_gender = NA,
		hh_size = NA,
		education_years_ = NA,
		field_size = p1$Area,
		yield = p1$YIELD,
		crop_price = p1$PRICE,
		harvest_date = pdate(p1$prodate),
		planting_date = pdate(g1$Dateplanting)[gi],
		planting_method = unname(mcrop),
		transplanting_days = ageseed,
		seedbed_method_ = tolower(trimws(gsub("\\(.*", "", g1$Methodbed[gi]))),
		variety = vtext(s1$ID, s1$Ricevar, ids),
		N_fertilizer = vsum(f1$ID, f1$Nha, ids),
		P_fertilizer = vsum(f1$ID, f1$Pha, ids),
		K_fertilizer = vsum(f1$ID, f1$Kha, ids)
	)
	dd <- cbind(dd, pest(ids, rd("INSECTICIDE_1.xlsx"), rd("HERBICIDE_1.xlsx"), rd("FUNGICIDE_1.xlsx"),
		rd("MOLLUSCICIDE_1.xlsx"), rd("RODENTICIDE_1.xlsx"),
		"ActiveIng", "ActiveIng", "ActiveIng", "ActiveIng", "ActiveIng", c("iapplication", "happlication", "fapplication")))

	dw$transplanting_days <- NA
	d <- rbind(dw, dd)

	d$trial_id <- paste(d$hhid, d$season, sep="_")
	d$country <- "Philippines"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	d$currency <- "PHP"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$geo_from_source <- NA

	carobiner::write_files(path, meta, d)
}
