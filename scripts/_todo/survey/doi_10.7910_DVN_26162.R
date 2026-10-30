# R script for "carob"
# license: GPL (>=3)

## NOTES
# CPWF (Challenge Program on Water and Food) benchmark household survey in salt-affected (sodic) areas of Uttar Pradesh, India.
# Mapped: "CPWF Final Baseline Survey - DTA MGT.xls", sheet "OWNLAND" = parcel-level kharif and rabi crops for the cropping year 2004-2005
# (sheet title; the codebook says 2003-04), 50 households: 25 in Faizabad (DISTRICT 1) and 25 in Sultanpur (DISTRICT 2) (sheet "GENERAL").
# OWNLAND includes rented-in parcels (OWNARNT 2; the same parcels are repeated in sheet "RLAND", which is not used).
# One record per household x parcel x season (kharif, rabi). Fallow season-parcels (crop code 20) carry no production and are not included.
# Codes from "Faizabad CPWF-Code book.doc": crop codes (KCROP/RCROP), rice varieties (KVAR), wheat varieties (RBVAR), soil, land type, irrigation, tenure.
# Area: the authors' ha columns (KAREAHA, RAREAHA) are used; they equal bigha / 10 in this file.
# yield = output (quintal) * 100 / area (ha). Output of intercrops (e.g. wheat + mustard) is the main crop output (ROUTPUT); MUSTARD column not used.
# Household head sex and age from sheet "HHHead_Info + HHInc".
# Crop code 17 "Curbi (kharif fodder)" -> "forage crop"; 21 linseed -> "flax"; 10 "pea+mustard/gram+mustard" -> "pea_mustard".

## ISSUES
# Village codes (1 in Faizabad, 2 in Sultanpur) have no names in the files; the codebook lists other villages (Zivpur, Pipari, ...) and
#   district 2 as Siddharthnagar, not Sultanpur. Block is empty. Human must find village names and georeference.
# Bigha conversion: this file uses 1 bigha = 0.1 ha, but the Lucknow file's "Conversions" sheet says 1.5 bigha = 1 acre (1 bigha = 0.27 ha). Verify.
# Some yields are very high (kharif to 60 t/ha, rabi to 44 t/ha), e.g. sugarcane (code 3) and possible area/output entry errors.
# Codebook RBVAR code 2 "Sambha Mahsuri" is a rice variety but is used for rabi (wheat) crops; set to NA.
# Variety codes are decoded only for rice (kharif) and wheat (rabi); other crops' variety codes are not documented.
# Not processed: "CPWF-Lucknow Dataset (Updated) - Jul. 28 DTA. MGT.xlsx" (50 households; columns are numbered variables and there is
#   no codebook for its codes, e.g. CROPNAME, CULFCRPG, VARTYPE; probably Unnao district). It has parcel crops/yields (quintals/bigha) and
#   costs and returns incl. urea and phosphate kg - worth mapping if a codebook is found.
# Not processed: COSTRTN (costs and returns, urea/phosphate kg for the best parcel; phosphate product type unknown), labor, marketing,
#   assets, basic needs, economic change sheets. "C_Guide.xls" describes an IRRI-CRRI Jagatsinghpur (Odisha) survey whose data are not in this dataset.
# Iran data mentioned in the metadata are not in the files.
# write_files: missing longitude/latitude/geo_from_source; unknown variables (see NEW VARIABLES).

## NEW VARIABLES
# land_type_: upland, midland, shallow lowland, lowland
# sodic_percent_: percentage of the parcel affected by sodicity (PRCNTSDC), %
# soil_saline_: TRUE if soil type is "saline alkaline" (STYPE 6)


carob_script <- function(path) {

"
Development of Technologies to Harness the Productivity Potential of the Salt-affected areas of the Indo-Gangetic, Mekong, and Nile River Basins

The benchmark survey was used to develop a systematic understanding of target environments and the livelihood of people as bases for exploring opportunities for improving water productivity and farmer's livelihood.   A paper was made to model the technical efficiency of the farmers using human capital (education), other socio-economic factors, degree of salinity/sodicity, and technology use, in explaining inefficiency of rice production system in selected districts of India.
"

	uri <- "doi:10.7910/DVN/26162"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "CPWF",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 20,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "CPWF Final Baseline Survey - DTA MGT.xls"]
	r <- carobiner::read.excel(f, sheet="OWNLAND", skip=2)
	r <- r[!is.na(r$IDNO), ]
	rh <- carobiner::read.excel(f, sheet="HHHead_Info + HHInc")

	crops <- c("1"="rice", "2"="wheat", "3"="sugarcane", "4"="oilseed", "5"="pigeon pea", "6"="potato", "7"="onion", "8"="spices",
		"9"="vegetable", "10"="pea_mustard", "11"="wheat_mustard", "12"="berseem clover", "13"="maize", "14"="other", "15"="pea",
		"16"="lentil", "17"="forage crop", "18"="maize_pigeon pea", "19"="black gram", "21"="flax")
	ricevar <- c("Sarjoo-52", "Sambha Mahsuri", "Swarna Mahsuri", "hybrid", "PVS line", "Pant 10", "Mahsuri", "Usar Dhan-3", "Pant-12", "Basmati", "NDR-97", "NDR-359")
	wheatvar <- c("M-234", NA, "HD2285", "Kundan", "LOK-1", "PBW", "Sonalika")

	## codes 0 are missing
	lk <- function(v, i) v[ifelse(i %in% seq_along(v), i, NA)]

	land <- data.frame(
		hhid = as.character(r$IDNO),
		field_id = as.character(r$IOPN),
		adm2 = lk(c("Faizabad", "Sultanpur"), r$DISTRICT),
		soil_texture = lk(c("loam", "sandy loam", "clay", "clay loam", "sand"), r$STYPE),
		soil_saline_ = ifelse(r$STYPE == 0, NA, r$STYPE == 6),
		land_type_ = lk(c("upland", "midland", "shallow lowland", "lowland"), r$LTYPE),
		sodic_percent_ = r$PRCNTSDC,
		irrigation_source = lk(c("own diesel pump", "rented diesel pump", "own electric tubewell", "rented electric tubewell",
			"government tubewell", "canal", NA, "shared diesel pump"), r$SOIRRI),
		irrigated = ifelse(r$SOIRRI %in% c(1:6, 8), TRUE, ifelse(r$SOIRRI == 7, FALSE, NA)),
		land_tenure = lk(c("self-cultivated", "sharecropped-rented out", NA, NA, "mortgaged-in", "share-in"), r$TSTATUS)
	)

	dk <- data.frame(land,
		season = "kharif",
		crop_code = r$KCROP,
		variety = ifelse(r$KCROP == 1, lk(ricevar, r$KVAR), NA),
		field_size = r$KAREAHA,
		yield = r$KOUTPUT * 100 / r$KAREAHA
	)
	dr <- data.frame(land,
		season = "rabi",
		crop_code = r$RCROP,
		variety = ifelse(r$RCROP %in% c(2, 11), lk(wheatvar, r$RBVAR), NA),
		field_size = r$RAREAHA,
		yield = r$ROUTPUT * 100 / r$RAREAHA
	)
	d <- rbind(dk, dr)
	d <- d[!d$crop_code %in% c(0, 20), ]
	d$crop <- as.vector(crops[as.character(d$crop_code)])
	d$crop_code <- NULL

	d$country <- "India"
	d$adm1 <- "Uttar Pradesh"
	d$planting_date <- "2004"
	d$yield_part <- NA
	d$yield_part[d$crop %in% c("rice", "wheat", "maize", "wheat_mustard", "maize_pigeon pea")] <- "grain"
	d$yield_part[d$crop %in% c("oilseed", "pigeon pea", "pea_mustard", "pea", "lentil", "black gram", "flax")] <- "seed"
	d$yield_part[d$crop == "sugarcane"] <- "stems"
	d$yield_part[d$crop == "potato"] <- "tubers"
	d$yield_part[d$crop %in% c("berseem clover", "forage crop")] <- "aboveground biomass"

	## household head
	j <- match(d$hhid, rh$IDNO)
	d$sex <- c("male", "female")[rh$SEX[j]]
	d$age <- rh$AGE[j]

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- paste(d$hhid, d$field_id, d$season, sep="_")

	carobiner::write_files(path, meta, d)
}
