# R script for "carob"
# license: GPL (>=3)

## NOTES
# INDICANTS field samples from banana plantations, 2022-2024. Country is the Country column. It is not copied from the coverage list onto other rows.
# Crop is plantain only when Variety is Plantain. Every other row, including a missing variety, is banana. The file is a banana-disease sample table.
# Collection dates are dd/mm/yyyy (README). Several assays of one sample share a trial_id.
# Six rows repeat an earlier row on every column kept here and are dropped.
# README: a negative LAMP result is stored as TTR 31 minutes, and some negative qPCR results are stored as Ct 36 or 41. Those values are kept.

## ISSUES
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variable: suspected_disease_ (disease?)
# unknown variable: pathogen_ (pathogen?)
# unknown variable: lamp_date_ (date?)
# unknown variables: tissue_, lamp_assay_, lamp_result_, lamp_ttr_, ref_result_, qpcr_ct_

## NOT PROCESSED
# LAMP extraction method, reference-method name, extraction used for the reference method, and the remarks column.


## NEW VARIABLES
# lamp_date_: date of the LAMP assay. Collection date stays in date.
# tissue_: tissue sampled.
# suspected_disease_: disease suspected from symptoms, as coded.
# pathogen_: suspected pathogen, as coded.
# lamp_assay_: LAMP assay name.
# lamp_result_: pos, neg, or doub.
# lamp_ttr_: time to result, minutes.
# ref_result_: reference PCR or qPCR result.
# qpcr_ct_: qPCR cycle threshold.


carob_script <- function(path) {

"
Multi-country field data from the INDICANTS project from 2022 to 2024

These data were collected as part of the INDICANTS project, which received funding from the European Union's Horizon 2020 research and innovation programme under grant agreement No. 890856. The project aimed to develop point-of-care LAMP (Loop-mediated isothermal amplification) assays for field diagnosis of four vascular banana diseases: Fusarium wilt, caused by Fusarium oxysporum f. sp. cubense (Foc); Moko disease and banana blood disease (BDB), caused by Ralstonia solanacearum and Ralstonia syzygii subsp. celebesensis, respectively; Xanthomonas wilt of banana (BXW), caused by Xanthomonas vasicola pv. musacearum. The four LAMP protocols have been validated on field samples. MOKO disease was diagnosed by a MOKO duplex-LAMP in Jamaica and from DNA samples supplied by Ecuador. Missions in Mozambique and Indonesia have been carried out for FocTR4 LAMP, in Uganda for BXW-LAMP, Indonesia for BDB-LAMP. Samples collected in banana plantations were directly tested by LAMP in the field or in non-laboratory environment after a simplified extraction method. Reference PCR/qPCR detection were carried out by the local partners in their laboratory to compare the results. Additionally, samples from Vietnam and Comoros have been collected by the Stellenbosch University in collaboration with local partners. Stellenbosch University determined the Vegetative Compatibility Group (VCG) of the isolated fungi, and performed both LAMP and qPCR on the samples. This table includes data for all countries on banana cultivars, their locations, the tissues sampled, and the results of LAMP and PCR/qPCR tests performed on the banana samples.
"

	uri <- "doi:10.18167/DVN1/SBZSUP"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1371/journal.pone.0337387",
		project = "INDICANTS",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "lamp_result_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Multi_country_field_data.csv"]
	r <- read.csv(f, na.strings=c("NA", ""), check.names=FALSE)
	names(r) <- trimws(names(r))

	variety <- trimws(r$Variety)
	variety[variety == ""] <- NA_character_
	# Country column, not the coverage list.
	country <- trimws(r$Country)
	location <- trimws(r$Location)
	sample <- trimws(r$Sample_name)
	sus <- trimws(r$Suspected_disease)
	healthy <- tolower(sus) %in% c("healthy", "control")

	d <- data.frame(
		trial_id = as.character(as.integer(as.factor(paste(country, sample)))),
		plot_id = sample,
		country = country,
		location = location,
		crop = ifelse(!is.na(variety) & variety == "Plantain", "plantain", "banana"),
		variety = variety,
		tissue_ = trimws(r$Tissue_type),
		suspected_disease_ = sus,
		pathogen_ = trimws(r$Suspected_pathogen),
		date = as.character(as.Date(trimws(r$Date), "%d/%m/%Y")),
		lamp_assay_ = trimws(r$LAMP_assay),
		lamp_date_ = as.character(as.Date(trimws(r$LAMP_date), "%d/%m/%Y")),
		lamp_result_ = tolower(trimws(r$LAMP_result)),
		lamp_ttr_ = as.numeric(trimws(r$TTR)),
		ref_result_ = tolower(trimws(r$Reference_methode_result)),
		qpcr_ct_ = as.numeric(trimws(r$Ct_qPCR)),
		stress = ifelse(healthy, NA_character_, "disease"),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d$tissue_[d$tissue_ == ""] <- NA_character_
	d$location[d$location == ""] <- NA_character_
	d$suspected_disease_[d$suspected_disease_ == ""] <- NA_character_
	d$pathogen_[d$pathogen_ == ""] <- NA_character_
	d$lamp_assay_[d$lamp_assay_ == ""] <- NA_character_
	d$lamp_result_[d$lamp_result_ == ""] <- NA_character_
	d$ref_result_[d$ref_result_ == ""] <- NA_character_
	d <- d[!duplicated(d), ]

	carobiner::write_files(path, meta, d)
}
