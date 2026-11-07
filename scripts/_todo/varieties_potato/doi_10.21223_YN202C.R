# R script for "carob"
# license: GPL (>=3)

## NOTES
# Open-field late blight trial of the HERLB population at Santa Catalina, Quito, Ecuador, rainy season 2023. Elevation 3050 m and the augmented row-column design are in the description. The Dataverse box publishes only south -0.36897 and west -78.55568.
# LB1 to LB4 are percent foliage. The crop-management log dates LB1 on 2023-05-02 at 92 DAP, then LB2, LB3, and LB4 at 98, 112, and 130 DAP. Planting date is that LB1 date minus 92 days. The harvest row has no date.
# AUDPC is stored as recorded. "-" is missing. Some LB cells say Muerta (dead). That word is kept in disease_severity and is not changed to 100. TTWP_Kg is kilograms per plot. The log says 3-hill plots and does not give plot area, so yield in kg/ha is not calculated.
# Fungicide and insecticide names are in the log without a dose.

## ISSUES
# No plot area, so tuber weight stays in kilograms per plot.
# No fertilizer rate and no harvest date.
# write_files: missing carob_contributor, yield, yield_part, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: ttwp_kg_, ttwp_g_, tuber_number_. out of bounds: AUDPC (0, 4065), rAUDPC (0, 1.07).

## NEW VARIABLES
# ttwp_kg_: total tuber weight, kg per plot.
# ttwp_g_: TTWP_gr as recorded. The dictionary says tuber weight and does not give the unit in the unit column.
# tuber_number_: TNTP, tuber number per plot.

## NOT PROCESSED
# Tuber skin and flesh color and shape codes.
# Crop-management product names (Pentacloro, Rugby, Farmathe / Dithane, coadyuvante) because no dose is recorded.
# 04_Data_dictionary_HERLB_ECU_2023_Lateblight_StaCatalina.xlsx: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Open field assessment for late blight evaluation in HERLB population (First trial_Ecuador)

29 families  potato  were exposed to late blight under natural infection in Quito, Eucador (3050 masl Quito-Ecuador) during the rainy season in 2023. Statistical Augmented row-column design without replication of 3 hill plots was properly established, three varieties with known late blight response were included as controls. After 40 days after planting (DAP), the percentage of leaf area affected by late blight infection was recorded by plot throughout the season to subsequently compute the area under the disease progress curve (AUDPC).
"

	uri <- "doi:10.21223/YN202C"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "augmented row-column",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "AUDPC;rAUDPC",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.45
	)

	num <- function(x) {
		z <- trimws(as.character(x))
		z[z %in% c("-", "", "NA")] <- NA_character_
		as.numeric(z)
	}

	f1 <- ff[basename(ff) == "01_Fieldbook_HERLB_ECU_2023_Lateblight_StaCatalina.xlsx"]
	r1 <- carobiner::read.excel(f1, na=c("", "NA", "-"))
	f2 <- ff[basename(ff) == "02_Material list_HERLB_ECU_2023_Lateblight_StaCatalina.xlsx"]
	r2 <- carobiner::read.excel(f2, na=c("", "NA", "-"))
	f3 <- ff[basename(ff) == "03_Crop managment_HERLB_ECU_2023_Lateblight_StaCatalina.xlsx"]
	r3 <- carobiner::read.excel(f3)

	lb <- r3[trimws(as.character(r3[["Tipo de Intervención"]])) == "LB1", ]
	fecha <- lb[["Fecha (MM/DD/YYYY)"]][1]
	if (inherits(fecha, "POSIXt") || inherits(fecha, "Date")) {
		fecha <- as.Date(format(fecha, "%Y-%m-%d"))
	} else {
		fecha <- as.Date(fecha)
	}
	planting <- format(fecha - as.integer(lb$DAP[1]), "%Y-%m-%d")

	variety <- trimws(as.character(r1[["CIP Number"]]))
	d <- data.frame(
		record_id = seq_len(nrow(r1)),
		plot_id = as.character(r1$Row),
		variety = variety,
		variety_code = variety,
		treatment = variety,
		crop = "potato",
		country = "Ecuador",
		location = "Santa Catalina",
		elevation = 3050,
		latitude = -0.36897,
		longitude = -78.55568,
		geo_from_source = TRUE,
		geo_uncertainty = 2000,
		geo_source = "CIP Dataverse bounding box (south and west only)",
		planting_date = planting,
		trial_id = "SantaCatalina_2023",
		AUDPC = num(r1$AUDPC),
		rAUDPC = num(r1$rAUDPC),
		ttwp_kg_ = num(r1$TTWP_Kg),
		ttwp_g_ = num(r1$TTWP_gr),
		tuber_number_ = num(r1$TNTP),
		pathogen = "Phytophthora infestans",
		stress = "disease",
		on_farm = NA,
		is_survey = FALSE
	)

	daps <- c(LB1=NA_integer_, LB2=NA_integer_, LB3=NA_integer_, LB4=NA_integer_)
	for (nm in names(daps)) {
		hit <- r3$DAP[trimws(as.character(r3[["Tipo de Intervención"]])) == nm]
		daps[nm] <- as.integer(hit[1])
	}
	pieces <- lapply(names(daps), function(nm) {
		data.frame(
			record_id = d$record_id,
			DAP = as.integer(daps[[nm]]),
			disease_severity = {
				z <- trimws(as.character(r1[[nm]]))
				z[z %in% c("-", "", "NA")] <- NA_character_
				z
			},
			severity_scale = "0-100",
			disease = "potato late blight"
		)
	})
	dlong <- do.call(rbind, pieces)
	dlong <- dlong[!is.na(dlong$disease_severity), ]
	rownames(dlong) <- NULL

	# Female and male columns, if the material list has them.
	fem <- grep("emale|Female|Madre", names(r2), value=TRUE)[1]
	mal <- grep("ale|Male|Padre", names(r2), value=TRUE)
	mal <- mal[!grepl("Female|emale", mal)][1]
	code <- grep("CIP|cip", names(r2), value=TRUE)[1]
	if (!is.na(fem) && !is.na(mal) && !is.na(code)) {
		ped <- paste(trimws(as.character(r2[[fem]])), "x", trimws(as.character(r2[[mal]])))
		ped[grepl("^NA x NA$|^x$", ped)] <- NA_character_
		alias <- data.frame(
			variety = trimws(as.character(r2[[code]])),
			variety_pedigree = ped
		)
		alias <- alias[!duplicated(alias$variety) & nzchar(alias$variety), ]
		d <- merge(d, alias, by="variety", all.x=TRUE, sort=FALSE)
	}

	carobiner::write_files(path, meta, d, long=dlong)
}
