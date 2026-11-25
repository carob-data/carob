# R script for "carob"
# license: GPL (>=3)

## NOTES
# Multi-country banana vascular disease diagnostic survey from the INDICANTS project (2022-2024; Robene et al., 2026, doi:10.1371/journal.pone.0337387).
# Two tables are written: d (wide, 203 unique plant tissue samples) and long (370 LAMP and reference PCR/qPCR assay records linked by record_id, with order indexing multiple assays and technical replicates per sample). All 370 source rows are preserved.
# Crop is "plantain" when Variety is "Plantain" (2 Ugandan plants); all other samples are "banana". Generic variety descriptors (Cooking_type, Landrace/cooking_type, Plantain, Musa) are stored in variety_type (or crop) rather than variety.
# Per README, negative LAMP results are coded as TTR = 31 min (1 min above the 30 min threshold) and negative qPCR results as Ct = 41 (Rincon-Flores et al., 2022) or Ct = 36 (Matthews et al., 2020); these sentinel values are kept verbatim.
# Thirteen Ecuador samples (ECU-RS-*) are bacterial strains isolated from symptomatic field tissues; ten Jamaica UWI_Mona negative controls (plant J4) are university campus samples (on_farm = FALSE).

## ISSUES
# Consider moving this script from agronomy to pest_disease (or survey): this is a banana disease diagnostic survey with no yield, fertilizer, irrigation, or planting/harvest dates, so those missing-variable messages are expected.
# No coordinates are provided in the data or publication. Estimated coordinates (geo_from_source = FALSE) are added at subdistrict/canton/parish/campus level and should be checked; Mozambique Jacaranda_farm,Nampula_province is placed at Fazenda Rio Monapo (Metocheria, Monapo district), where Formosana was replanted after Foc TR4, though Jacaranda also operates in Erati district.
# Target banana diseases (banana blood disease, banana xanthomonas wilt, fusarium wilt, moko disease) and pathogens (Ralstonia syzygii subsp. celebesensis, Xanthomonas vasicola pv. musacearum, Fusarium oxysporum f. sp. cubense, Ralstonia solanacearum) are not yet in terminag values_disease / values_pathogen (triggering invalid terms messages).
# Collection date is missing for Comoros (8) and Vietnam (8). LAMP_date precedes Date for Mozambique plant MP4 (2022-09-10 vs 2022-09-12; 18 rows) and 4 Indonesian rows (2023-02-22 vs 2023-10-25, likely a typo for 2024-02-22); kept verbatim in long.
# Indonesia sample I25 / 25.5 has Date = 29/10/2023 on the BBD_indic row (after LAMP_date = 26/10/2023) vs 25/10/2023 on the other 2 assay rows; 2023-10-25 is used in d.
# Indonesia sample I11 / 11.7 has Tissue_type = Pseudostem on the BBD_indic row vs Peduncule/peduncule on the other 2 rows (while 11.5 is already Pseudostem); "peduncle" is used in d.
# Indonesia Rincon-Flores qPCR dates fall in October 2025 (2025-10-26/28/29); Indonesia sample 15.1 has negative Matthews et al., 2020 Ct = 41 instead of 36.
# Four positive LAMP rows have TTR = 31 (CAV5786, CAV5839, Uganda 1.1.s, CAV5529); Indonesia 24.1 (Ordonez_et_al.,2019) has negative LAMP with TTR = 32; Mozambique Pl4mac2 (Cand1) has negative LAMP with TTR = 29.25.
# Uganda plant UP5 has two different varieties recorded across its two samples (3.1.1: Kayinja; 3.1.2: Gros Michel).
# Raw Location "Hano\ufffd" (corrupted encoding of Hanoi) is fixed to "Hanoi"; raw Suspected_pathogen "Rasltonia_solanacearum" is fixed to "Ralstonia solanacearum".

## NOT PROCESSED
# None; all 22 columns of Multi_country_field_data.csv are processed.

## NEW VARIABLES
# plant_id_: plant code in the survey (e.g. C1, E1, I1, J1, MP1, UP1, V1).
# tissue_: plant tissue sampled (or "strain isolated from symptomatic tissues").
# suspected_disease_: disease suspected from field symptoms (as coded, including Foc VCG codes and ambiguous diagnoses).
# suspected_pathogen_: pathogen suspected from field symptoms (or "none" for negative controls).
# lamp_extraction_: DNA extraction method used for the LAMP assay.
# lamp_assay_: LAMP assay protocol/primer set used.
# lamp_date_: date the LAMP assay was performed (YYYY-MM-DD).
# lamp_result_: qualitative LAMP result (positive, negative, or doubtful).
# lamp_ttr_: LAMP time-to-result (minutes; negative sentinel = 31 or 32).
# ref_method_type_: reference diagnostic method type (PCR or qPCR).
# ref_method_: reference PCR/qPCR protocol citation.
# ref_extraction_: DNA extraction method used for the reference assay.
# ref_date_: date the reference PCR/qPCR assay was performed (YYYY-MM-DD).
# ref_result_: qualitative reference PCR/qPCR result (positive or negative).
# qpcr_ct_: qPCR cycle threshold (Ct; negative sentinel = 36 or 41).
# remarks_: additional assay/sample notes from the source Remarks column.


carob_script <- function(path) {

"
Multi-country field data from the INDICANTS project from 2022 to 2024

These data were collected as part of the INDICANTS project, which received funding from the European Union's Horizon 2020 research and innovation programme under grant agreement No. 890856. The project aimed to develop point-of-care LAMP (Loop-mediated isothermal amplification) assays for field diagnosis of four vascular banana diseases: Fusarium wilt, caused by Fusarium oxysporum f. sp. cubense (Foc); Moko disease and banana blood disease (BDB), caused by Ralstonia solanacearum and Ralstonia syzygii subsp. celebesensis, respectively; Xanthomonas wilt of banana (BXW), caused by Xanthomonas vasicola pv. musacearum. The four LAMP protocols have been validated on field samples. MOKO disease was diagnosed by a MOKO duplex-LAMP in Jamaica and from DNA samples supplied by Ecuador. Missions in Mozambique and Indonesia have been carried out for FocTR4 LAMP, in Uganda for BXW-LAMP, Indonesia for BDB-LAMP. Samples collected in banana plantations were directly tested by LAMP in the field or in non-laboratory environment after a simplified extraction method. Reference PCR/qPCR detection were carried out by the local partners in their laboratory to compare the results. Additionally, samples from Vietnam and Comoros have been collected by the Stellenbosch University in collaboration with local partners. Stellenbosch University determined the Vegetative Compatibility Group (VCG) of the isolated fungi, and performed both LAMP and qPCR on the samples. This table includes data for all countries on banana cultivars, their locations, the tissues sampled, and the results of LAMP and PCR/qPCR tests performed on the banana samples.
"

	uri <- "doi:10.18167/DVN1/SBZSUP"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD;SUN",
		publication = "doi:10.1371/journal.pone.0337387",
		project = "INDICANTS",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "lamp_result_;lamp_ttr_;ref_result_;qpcr_ct_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 100,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Multi_country_field_data.csv"]
	r <- read.csv(f, na.strings=c("NA", ""), check.names=FALSE, fileEncoding="UTF-8")
	names(r) <- trimws(names(r))

	trim_clean <- function(x) {
		x <- trimws(as.character(x))
		x[x == ""] <- NA_character_
		x
	}

	country <- trim_clean(r$Country)
	raw_loc <- trim_clean(r$Location)
	# Fix corrupted UTF-8 replacement character in "Hano\ufffd" (JSON metadata: "Hanoi")
	raw_loc[grepl("^Hano", raw_loc)] <- "Hanoi"

	plant <- trim_clean(r$Plant)
	sample_id <- trim_clean(r$Sample_name)
	plot_id <- trim_clean(r$Plot)
	date_col <- as.character(as.Date(trim_clean(r$Date), "%d/%m/%Y"))
	# Indonesia I25 / 25.5 has a single row with 2023-10-29 (after LAMP_date 2023-10-26) vs 2023-10-25 on other rows
	date_col[plant == "I25" & sample_id == "25.5" & date_col == "2023-10-29"] <- "2023-10-25"

	# Normalize variety names and move generic descriptors to crop / variety_type
	raw_var <- trim_clean(r$Variety)
	crop <- ifelse(!is.na(raw_var) & raw_var == "Plantain", "plantain", "banana")
	variety <- gsub("_", " ", raw_var)
	variety[variety == "Ambon hijau"] <- "Ambon Hijau"
	variety[variety %in% c("Cooking type", "Landrace/cooking type", "Plantain", "Musa")] <- NA_character_

	variety_type <- rep(NA_character_, nrow(r))
	variety_type[raw_var == "Cooking_type"] <- "cooking"
	variety_type[raw_var == "Landrace/cooking_type"] <- "landrace; cooking"
	# Remarks column notes Ambon_hijau is "local cavendish"
	variety_type[raw_var == "Ambon_hijau"] <- "local"

	# Normalize tissue type (case, underscores, and French spelling "peduncule" -> "peduncle")
	tissue <- tolower(gsub("_", " ", trim_clean(r$Tissue_type)))
	tissue[tissue == "peduncule"] <- "peduncle"
	# Indonesia I11 / 11.7 is recorded as "Pseudostem" on 1 row vs "peduncle" on 2 rows (11.5 is already pseudostem)
	tissue[plant == "I11" & sample_id == "11.7"] <- "peduncle"

	# Normalize suspected disease and suspected pathogen (field symptom-based)
	sus_dis <- trim_clean(r$Suspected_disease)
	sus_dis[tolower(sus_dis) == "control"] <- "control"
	sus_dis[tolower(sus_dis) == "healthy"] <- "healthy"
	# Indonesia I32 / 32.1 has "Healthy" on 2 rows and "CONTROL" on 1 row (all Negative_control)
	sus_dis[plant == "I32" & sample_id == "32.1"] <- "control"

	sus_path <- trim_clean(r$Suspected_pathogen)
	sus_path_map <- c(
		"Fusarium_oxysporum_f.sp.cubense" = "Fusarium oxysporum f. sp. cubense",
		"Ralstonia_syzygii_subsp.celebesensis" = "Ralstonia syzygii subsp. celebesensis",
		"Rasltonia_solanacearum" = "Ralstonia solanacearum",
		"Xanthomonas_vasicola_pv.musacearum" = "Xanthomonas vasicola pv. musacearum",
		"Negative_control" = "none"
	)
	sus_path <- unname(sus_path_map[sus_path])

	stress <- rep(NA_character_, nrow(r))
	stress[sus_dis %in% c("healthy", "control")] <- "none"
	stress[sus_dis %in% c("BBD", "BXW", "MOKO", "Foc_VCG0124/5/22", "FocTR4_VCG01213/16_VCG 0121", "FocTR4_or_BBD")] <- "disease"
	stress[sus_dis %in% c("FocTR4_or_Borer", "Borer_or_TR4_or_BBD")] <- "disease;pests"

	# Sample-level wide table (one row per unique Country x Plant x Sample_name = 203 samples)
	skey <- paste(country, plant, sample_id, sep = "|")
	first_idx <- !duplicated(skey)

	d <- data.frame(
		record_id = seq_len(sum(first_idx)),
		country = country[first_idx],
		raw_loc = raw_loc[first_idx],
		plot_id = plot_id[first_idx],
		plant_id_ = plant[first_idx],
		sample_id = sample_id[first_idx],
		date = date_col[first_idx],
		crop = crop[first_idx],
		variety = variety[first_idx],
		variety_type = variety_type[first_idx],
		tissue_ = tissue[first_idx],
		suspected_disease_ = sus_dis[first_idx],
		suspected_pathogen_ = sus_path[first_idx],
		stress = stress[first_idx],
		on_farm = raw_loc[first_idx] != "UWI_Mona",
		is_survey = TRUE
	)

	# Estimated coordinates and GADM administrative divisions (no coordinates in data or publication)
	# Mozambique Jacaranda_farm placed at Fazenda Rio Monapo / Metocheria (Monapo district, Nampula)
	geo <- data.frame(
		country = c(
			"Comoros", "Ecuador", "Ecuador", "Ecuador",
			"Indonesia", "Indonesia", "Jamaica", "Jamaica",
			"Mozambique", "Uganda", "Uganda", "Uganda",
			"Vietnam", "Vietnam"
		),
		raw_loc = c(
			"Grand_Comoros", "Valencia_Los Rios", "Quevedo_Los Rios", "El_Carmen_Manabi",
			"Mande_Cianjur", "Cikalongkulon_Cianjur", "ST_James", "UWI_Mona",
			"Jacaranda_farm,Nampula_province", "Mukono", "Bugahya(Hoima)", "Kayunga",
			"Hanoi", "Hung_Yen"
		),
		adm1 = c(
			"Njazidja", "Los R\u00edos", "Los R\u00edos", "Manab\u00ed",
			"Jawa Barat", "Jawa Barat", "Saint James", "Saint Andrew",
			"Nampula", "Mukono", "Hoima", "Kayunga",
			"H\u00e0 N\u1ed9i", "H\u01b0ng Y\u00ean"
		),
		adm2 = c(
			NA, "Valencia", "Quevedo", "El Carmen",
			"Cianjur", "Cianjur", NA, NA,
			"Monapo", NA, "Bugahya", NA,
			NA, NA
		),
		adm3 = c(
			NA, NA, NA, NA,
			"Mande", "Cikalongkulon", NA, NA,
			NA, NA, NA, NA,
			NA, NA
		),
		location = c(
			"Grande Comore", "Valencia", "Quevedo", "El Carmen",
			"Mande", "Cikalongkulon", "Saint James", "UWI Mona",
			"Jacaranda Farm", "Mukono", "Bugahya", "Kayunga",
			"Hanoi", "Hung Yen"
		),
		longitude = c(
			43.3333, -79.3531, -79.4604, -79.4616,
			107.2601, 107.1495, -77.8833, -76.7458,
			40.1250, 32.7553, 31.2491, 32.9036,
			105.8412, 106.0511
		),
		latitude = c(
			-11.5833, -0.9525, -1.0225, -0.2726,
			-6.7536, -6.6836, 18.3833, 18.0054,
			-14.8700, 0.3533, 1.5536, 0.7033,
			21.0245, 20.6464
		),
		geo_uncertainty = c(
			35000, 15000, 15000, 15000,
			10000, 10000, 20000, 2000,
			15000, 20000, 20000, 20000,
			25000, 20000
		),
		geo_source = c(
			"Grande Comore island center", "Valencia canton town center", "Quevedo canton town center", "El Carmen canton town center",
			"Mande subdistrict center", "Cikalongkulon subdistrict center", "Saint James parish center", "UWI Mona campus",
			"Metocheria / Fazenda Rio Monapo", "Mukono town center", "Bugahya county center", "Kayunga town center",
			"Hanoi municipality center", "Hung Yen province center"
		)
	)

	d <- merge(d, geo, by = c("country", "raw_loc"), all.x = TRUE, sort = FALSE)
	d <- d[order(d$record_id), ]
	rownames(d) <- NULL
	d$raw_loc <- NULL
	d$geo_from_source <- FALSE
	# One trial_id per country x location sampling mission (14 distinct locations)
	d$trial_id <- as.character(as.integer(as.factor(paste(d$country, d$location))))

	# Assay-level long table (all 370 diagnostic records linked to d via record_id)
	lamp_assay <- trim_clean(r$LAMP_assay)

	assay_disease <- c(
		"BBD_indic" = "banana blood disease",
		"BXW_indic" = "banana xanthomonas wilt",
		"Duplex-MOKO_indic" = "moko disease",
		"FocTR4_indic" = "fusarium wilt",
		"FocTR4_indic(primers/2)" = "fusarium wilt",
		"Ordonez_et_al.,2019" = "fusarium wilt",
		"Cand1" = "fusarium wilt"
	)
	assay_pathogen <- c(
		"BBD_indic" = "Ralstonia syzygii subsp. celebesensis",
		"BXW_indic" = "Xanthomonas vasicola pv. musacearum",
		"Duplex-MOKO_indic" = "Ralstonia solanacearum",
		"FocTR4_indic" = "Fusarium oxysporum f. sp. cubense",
		"FocTR4_indic(primers/2)" = "Fusarium oxysporum f. sp. cubense",
		"Ordonez_et_al.,2019" = "Fusarium oxysporum f. sp. cubense",
		"Cand1" = "Fusarium oxysporum f. sp. cubense"
	)
	res_map <- c(pos = "positive", neg = "negative", doub = "doubtful")
	ref_method_map <- c(
		"Cellier et al.,2015" = "Cellier et al., 2015",
		"Mattews_and_al.,2020" = "Matthews et al., 2020",
		"Nakato_et_al.,2018" = "Nakato et al., 2018",
		"Prior_and_Fegan, 2005 version PHA, 2006" = "Prior and Fegan, 2005 (PHA, 2006)",
		"Rincon-Flores_and_al.,2022" = "Rincon-Flores et al., 2022"
	)
	ref_ext_map <- c(
		"simplified" = "simplified",
		"bead_based_extraction" = "bead-based extraction",
		"extraction_kit" = "extraction kit"
	)

	long <- data.frame(
		record_id = match(skey, unique(skey)),
		order = as.integer(ave(seq_len(nrow(r)), skey, FUN = seq_along)),
		disease = unname(assay_disease[lamp_assay]),
		pathogen = unname(assay_pathogen[lamp_assay]),
		lamp_extraction_ = trim_clean(r$LAMP_extraction),
		lamp_assay_ = lamp_assay,
		lamp_date_ = as.character(as.Date(trim_clean(r$LAMP_date), "%d/%m/%Y")),
		lamp_result_ = unname(res_map[tolower(trim_clean(r$LAMP_result))]),
		lamp_ttr_ = as.numeric(r$TTR),
		ref_method_type_ = trim_clean(r$Method),
		ref_method_ = unname(ref_method_map[trim_clean(r$Reference_method)]),
		ref_extraction_ = unname(ref_ext_map[trim_clean(r$Extraction_reference_method)]),
		ref_date_ = as.character(as.Date(trim_clean(r$Reference_methode_date), "%d/%m/%Y")),
		ref_result_ = unname(res_map[tolower(trim_clean(r$Reference_methode_result))]),
		qpcr_ct_ = as.numeric(r$Ct_qPCR),
		remarks_ = trim_clean(r$Remarks)
	)

	carobiner::write_files(path, meta, d, long = long)
}
