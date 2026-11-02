# R script for "carob"
# license: GPL (>=3)

## NOTES
# Nationwide survey of 769 rice farmers in 38 villages, 19 municipalities in 19 provinces (Hossain & Cabrera). "Project Details.docx" says it uses "the same data set from the Survey on rice diversity and yield losses from pests": Landholdings.xls is a cleaned and coded version of the LANDHOLDING sheet of doi:10.7910/DVN/26291 (same RECNO, same parcels, crop areas filled in, yields computed). The two datasets are therefore duplicates for the parcel x season data; a human should decide which one to keep (this one is cleaner).
# Records are parcel x season (wet season 2003, dry season 2004, from "Project Details.docx") for parcels planted with rice (crop code 1). Fallow and non-rice parcel-seasons are not included.
# yield = production (cavans) x kg per cavan (both reported by the farmer) / crop area; this equals wsyield/dsyield (t/ha) in the file. Moisture content not given.
# yield_loss_ = reported crop loss (cavans) x kg per cavan / crop area.
# Province, municipality and village names from the codebook ("Land use, livelihoods and rice variety diversityCM.doc"); municipality code equals province code, which was used for the one record without province.
# Codes: ELEV 1 upland, 2 medium land, 3 lowland, 4 rolling (landscape_position); IRRCODE 1 NIA, 2 pump, 3 communal/CIS, 4 rainfed; variety type 1 hybrid, 2 modern (MV), 3 traditional (TV), 4 unknown; TENCODE 1 owned, 2 CLT holder, 3 sharecropped in, 4 fixed rent, 5 mortgaged in.
# Reasons for crop loss (free text) mapped to stress with simple keyword rules (e.g. insufficient water, drought -> drought; stemborer, black bug, rice bug, snails, GLH, BPH, army worm -> pests; rats -> rodents; tungro, BLB, blast, neck rot -> disease; typhoon, wind -> wind; flooded, submergence -> flood); raw text in loss_reason_.

## ISSUES
# No coordinates. Locations are villages (location) in municipalities (adm2) and provinces (adm1); see the list of 38 villages in the codebook. Georeference manually.
# Year: "Project Details.docx" says 2003 wet and 2004 dry seasons; the Dataverse metadata of doi:10.7910/DVN/26291 gives 2002-2003. harvest_date set to 2003 (wet) and 2004 (dry): check.
# kg per cavan is 0 for 24 wet season and 5 dry season rice records; yield NA there.
# Reason codes (RSON1COD/RSON2COD) are not documented; only the text was used. "LACK OF FERT", "LACK OF INPUTS", "POOR SEEDS", "LODGING", "SOIL PROBLEM", "BAD WEATHER" etc. not mapped to stress.
# "count" (used as field_id) is not unique (12 repeated values) and one record is an exact duplicate (write_files "duplicate records detected"); not removed, as it may be a second identical parcel.
# Third-season crops (6 records, mostly mungbean) and non-rice crops not included.
# Unmapped: Source of seeds.xls (several rows per farmer, not linked to season or parcel: seed type, source, kg/ha, price, quality), total land area.xls, member's information.xls (household members and incomes), land use summaries per farmer (owned, clt, wsRarea, ...).
# Publication: Hossain & Cabrera 2007, paper at the CURE annual meeting, Vientiane (no DOI).
# write_files: missing longitude/latitude/geo_from_source; unknown variables yield_loss_, loss_reason_, village_type_; duplicate records detected; NA detected: yield (kg per cavan 0).

## NEW VARIABLES
# yield_loss_: reported crop loss, kg/ha
# loss_reason_: reason for crop losses as reported (text)
# village_type_: village irrigation type (irrigated, rainfed, partially irrigated; NVILTYP)


carob_script <- function(path) {

"
Land use, livelihoods and rice variety diversity in irrigated and rainfed ecosystems in the Philippines

Conducted a nationwide survey to collect yield loss in 19 provinces across the major rice-producing provinces in the Philippines
"

	uri <- "doi:10.7910/DVN/EA8PTP"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = "multi-stage random sample of municipalities, two villages per municipality",
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Landholdings.xls"]
	r <- carobiner::read.excel(f, na=".")

	prov <- c("Ilocos Norte", "Pangasinan", "Isabela", "Nueva Vizcaya", "Nueva Ecija", "Bulacan", "Camarines Sur", "Albay", "Oriental Mindoro", "Palawan", "Capiz", "Iloilo", "Antique", "Bohol", "Leyte", "Zamboanga del Sur", "Bukidnon", "North Cotabato", "Davao del Sur")
	mun <- c("Banna", "Infanta", "Cauayan", "Solano", "Zaragoza", "San Ildefonso", "Nabua", "Camalig", "Naujan", "Brooke's Point", "Dumarao", "Balasan", "Sibalom", "San Miguel", "Palo", "Mahayag", "Valencia", "Kabacan", "Bansalan")
	vil <- c("Bugasi", "Tabtabagan", "Nayom", "Bamban", "Alinam", "San Francisco", "Wacal", "Roxas", "Valeriana", "Sto Rosario (Old)", "Telepatio", "Umpucan", "Paloyon", "Sta Lucia", "Libod", "Tumpa", "Pinagsabangan 1", "Sampaguita", "Maasin", "Salogon", "Bayog", "Salcedo", "Mahmut North", "Tinguian", "Pis-anan", "Egaña", "Mahayag", "San Vicente", "Castilla", "Teraza", "San Isidro", "Kabuhi", "Batangan", "Nabag-o", "Katidtuan", "Magatos", "Dolo", "New Clarin")

	irr <- c("NIA canal", "pump", "communal (CIS)", NA)
	vtype <- c("1"="hybrid", "2"="modern", "3"="traditional")

	ssn <- function(seas, area, code, var, vtyp, prod, loss, kgcav, reason) {
		kg <- as.numeric(r[[kgcav]])
		kg[kg == 0] <- NA
		a <- as.numeric(r[[area]])
		data.frame(
			hhid = as.character(r$recno),
			field_id = as.character(r$count),
			adm1 = prov[as.integer(r$mun)],
			adm2 = mun[as.integer(r$mun)],
			location = vil[as.integer(r$vil)],
			field_size = as.numeric(r$parea),
			landscape_position = c("upland", "medium land", "lowland", "rolling")[as.integer(r$elev)],
			land_tenure = c("owned", "CLT holder", "sharecropped in", "fixed rent", "mortgaged in")[as.integer(r$tencode)],
			irrigated = ifelse(is.na(r$irrcode), NA, as.integer(r$irrcode) %in% 1:3),
			irrigation_source = irr[as.integer(r$irrcode)],
			village_type_ = c("irrigated", "rainfed", "partially irrigated")[as.integer(r$nviltyp)],
			season = seas,
			harvest_date = ifelse(seas == "wet", "2003", "2004"),
			crop_code = r[[code]],
			variety = r[[var]],
			variety_type = unname(vtype[as.character(r[[vtyp]])]),
			yield = as.numeric(r[[prod]]) * kg / a,
			yield_loss_ = as.numeric(r[[loss]]) * kg / a,
			loss_reason_ = r[[reason]]
		)
	}
	d <- rbind(
		ssn("wet", "cparea1", "cp1code", "wsvar", "wsvartyp", "prod1", "loss1", "kgcav1", "reason1"),
		ssn("dry", "cparea2", "cp2code", "dsvar", "dsvartyp", "prod2", "loss2", "kgcav2", "reason2")
	)
	d <- d[d$crop_code %in% "1", ]
	d$crop_code <- NULL
	d$loss_reason_[d$loss_reason_ %in% c("NA", "0")] <- NA

	rs <- toupper(d$loss_reason_)
	st <- cbind(
		ifelse(grepl("INSUFF|DROUGHT|LACK OF WATER|WATER SHORTAGE|NO WATER", rs), "drought", NA),
		ifelse(grepl("STEM ?BORER|BLA?CK +BUG|RICE BUG|SNAIL|ARMY|GLH|BPH|HOPPER|HARABAS|WORM|INSECT", rs), "pests", NA),
		ifelse(grepl("\\bRATS?\\b", rs), "rodents", NA),
		ifelse(grepl("TUNGRO|BLB|BLAST|NECK ROT|FUNGUS|SHEATH", rs), "disease", NA),
		ifelse(grepl("TYPHOON|WIND", rs), "wind", NA),
		ifelse(grepl("FLOOD|SUBMERG", rs), "flood", NA),
		ifelse(grepl("WEED", rs), "weeds", NA),
		ifelse(grepl("COLD", rs), "cold", NA),
		ifelse(grepl("HEAT", rs), "heat", NA)
	)
	d$stress <- apply(st, 1, function(x) if (all(is.na(x))) NA else paste(x[!is.na(x)], collapse="; "))

	d$country <- "Philippines"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$trial_id <- paste(d$hhid, d$season, sep="_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
