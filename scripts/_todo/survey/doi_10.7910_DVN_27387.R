# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI / Zhanghe Irrigation System (ZIS) farm survey, Hubei, China, 2008 summer(-autumn) rice season; 137 farms in Jingmen, Shayang and Zhongxiang counties.
# Main data: "China 2008 I-O and HH Data (HH Level).xls" (one row per farm, input-output already per ha; codebook in "Variable description_China.xls").
# Variety, establishment date and seedling age from CROPESTAB_WA_08.xls sheet CESeeds (header row with codes located by "FARMID"); farms with several parcels/plots get varieties joined with ";" and the earliest establishment date.
# areapltdha is area planted in ha (mu x m2 per mu / 10000). yield is kg/ha; pyuankg is paddy price (CNY/kg).
# Seed rate qtyseedha (kg/ha), seed price (CNY/kg), manure maqtykgha (kg/ha), herbicide and insecticide active ingredient (kg ai/ha) taken as given.
# irrigation_number = number of irrigation events during land preparation, transplanting and crop growth (irfreqlp + irfreqtp + irfreqcg).
# Paper on this survey: Kajisa, Moya et al. (Variable description title page); no DOI in the metadata.

## ISSUES
# kgpha and kgkha are described as "Chem fertilizer P" and "K"; not clear whether these are elemental or P2O5/K2O. Mapped as elemental without conversion; verify (e.g. against FERTILIZER_WA_08.xls).
# 21 farms have no input-output data (all I-O columns NA) but have yield.
# netdepth* (net irrigation depth) is labelled "in cm" but values up to 1225 suggest mm; not mapped to irrigation_amount. Kept as new variables with the unit unresolved.
# Crop establishment METHOD1 codes (1, 2) are undocumented; seedlings ages are given for both, so CEDATE is mapped to transplanting_date. Verify (2 may be seedling throwing). planting_date is only the year (2008); sowing date not given.
# Season is "Summer-Autumn"/"Summer" in the files; kept as "summer" (not in season vocabulary).
# write_files: missing longitude/latitude/geo_from_source; out of bounds transplanting_days (seedling age up to 55 d) and fertilizer_amount (one farm 4000 kg/ha product; check); invalid season "summer"; new variables.
# No coordinates; county/town/village names given (adm2 = county; town and village in location). Human must georeference the 29 villages.
# "pricing" categories (I, ANA, VS, VNA, VM ...) of irrigation water pricing are undocumented; kept as irrigation_pricing_.
# Not mapped: labour and all cost/return columns (except seed, fertilizer, herbicide, insecticide, irrigation cost), household characteristics (nohh, schhh etc. mapped partly), parcel geometry/wells/pumps/ponds, surrounding parcel variables, and all parcel-level _WA_08 files (fertilizer, chemicals, irrigation schedule, land prep, harvest dates).

## NEW VARIABLES
# awd_score_: AWD score of the farm (awd, 0-1)
# irrigation_pricing_: water pricing category code (pricing)
# irrigation_depth_cg_: net irrigation depth during crop growth (netdepthcg; unit unclear, cm or mm)
# irrigation_depth_lp_: net irrigation depth during land preparation (netdepthlp; unit unclear)
# irrigation_depth_tp_: net irrigation depth at transplanting (netdepthtp; unit unclear)
# irrigation_hours_: total duration of irrigation (durationcg + durationlp + durationtp, h)
# water_user_group_: water user group code (wucode)
# education_years_: schooling of the household head (schhh, years)


carob_script <- function(path) {

"
Role of Institution and Policies for adoption of water saving technology in China

The farm survey data was used to explore the roles of pricing policy, institutions and infrastructures for rice farmers water saving behavior using the water users group, farmers input output and water use/management data in Zhanghe Irrigation System Hubei China.
"

	uri <- "doi:10.7910/DVN/27387"
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
		carob_completion = 35,
		carob_effort = 0.6
	)

	f1 <- ff[basename(ff) == "China 2008 I-O and HH Data (HH Level).xls"]
	r1 <- carobiner::read.excel(f1)

	f2 <- ff[basename(ff) == "CROPESTAB_WA_08.xls"]
	r2 <- carobiner::read.excel(f2, sheet="CESeeds", col_names=FALSE)
	h <- which(apply(r2, 1, function(x) "FARMID" %in% x))[1]
	names(r2) <- as.character(unlist(r2[h, ]))
	r2 <- r2[-(1:h), 1:18]
	r2 <- r2[!is.na(r2$FARMID), ]
	r2$CEDATE <- as.numeric(r2$CEDATE)
	r2 <- r2[order(r2$FARMID, r2$CEDATE), ]

	vars <- tapply(trimws(r2$VARIETY), r2$FARMID, function(x) paste(unique(x[!is.na(x)]), collapse=";"))
	r2 <- r2[!duplicated(r2$FARMID), ]
	i <- match(r1$farmid, r2$FARMID)

	d <- data.frame(
		hhid = r1$farmid,
		country = "China",
		adm1 = "Hubei",
		adm2 = r1$county,
		location = paste(r1$town, r1$village, sep=", "),
		water_user_group_ = as.character(r1$wucode),
		irrigation_pricing_ = r1$pricing,
		season = "summer",
		planting_date = "2008",
		crop = "rice",
		field_size = r1$areapltdha,
		variety = as.vector(vars[r1$farmid]),
		transplanting_date = as.character(as.Date(r2$CEDATE[i], origin="1899-12-30")),
		transplanting_days = as.numeric(r2$SEEDAGE[i]),
		seed_rate = r1$qtyseedha,
		seed_price = r1$price,
		seed_cost = r1$seedcostha,
		fertilizer_amount = r1$cfqtykgha,
		fertilizer_price = r1$cfpricekg,
		fertilizer_cost = r1$cfcostha,
		N_fertilizer = r1$kgnha,
		P_fertilizer = r1$kgpha,
		K_fertilizer = r1$kgkha,
		OM_used = r1$maqtykgha > 0,
		OM_amount = r1$maqtykgha,
		herbicide_amount = r1$hekgaiha,
		herbicide_cost = r1$hecostha,
		insecticide_amount = r1$inskgaiha,
		insecticide_cost = r1$inscostha,
		irrigated = TRUE,
		irrigation_number = as.integer(r1$irfreqlp + r1$irfreqtp + r1$irfreqcg),
		irrigation_hours_ = r1$durationlp + r1$durationtp + r1$durationcg,
		irrigation_depth_lp_ = r1$netdepthlp,
		irrigation_depth_tp_ = r1$netdepthtp,
		irrigation_depth_cg_ = r1$netdepthcg,
		irrigation_cost = r1$irrcostha,
		awd_score_ = r1$awd,
		yield = r1$yield,
		crop_price = r1$pyuankg,
		currency = "CNY",
		hh_size = r1$nohh,
		education_years_ = r1$schhh
	)

	d$variety[d$variety == ""] <- NA
	d$herbicide_used <- d$herbicide_amount > 0
	d$insecticide_used <- d$insecticide_amount > 0
	d$trial_id <- d$hhid
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
