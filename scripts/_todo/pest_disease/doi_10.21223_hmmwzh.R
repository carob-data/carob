# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The previous pending script assigned longitude -75.383342, latitude -10.591977,
# elevation 1813, adm2 Oxapampa, adm3 Miraflores, planting_date 1999-11-27 and
# harvest_date 2000-04-05. Those values are not columns in the workbooks. Confirm
# them before using them. File 02 does record Emergency_date 1999-12-07.
# LB1-LB27 are repeated late-blight scores. File 02 gives the date of each score.
# They are not yet reshaped into a long disease table.
# 05_TotalYield_Oxapampa.xls stores four total-biomass dry-matter evaluations
# (TBDMC_kg_ha_Ev1-4), not fresh tuber yield. Do not treat them as yield.
# 07 sprays, and 08/09 weather, have header rows and Excel serial dates.



carob_script <- function(path) {

"
Dataset for: Effect of nitrogen fertilization on potato late blight in the field in Oxapampa, Pasco, Peru

The effect of nitrogen (N) fertilization on the development of potato late blight disease, caused by Phytophthora infestans (Mont.) de Bary, was studied through field experiments in two locations in Peru (Huancayo and Oxapampa). These datasets correspond to the field experiments in Oxapampa. Three levels of N (0, 160, and 320 kg/ha) and three levels of fungicide were applied in all combinations on two potato (Solanum tuberosum L.) varieties with different levels of resistance (18 treatments), and in four repetitions.
"

	uri <- "doi:10.21223/HMMWZH"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;variety",
		response_vars = "disease_severity",
		carob_completion = 20,
		carob_effort = 0,
		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		notes = "Oxapampa field experiment only. Huancayo is described in the abstract and is not in these files.",
		
	)

	f <- ff[basename(ff) == "01_Data_LateBlightSeverity_Oxapampa.xls"]
	r <- carobiner::read.excel(f)

	# Nitrogen_rate is coded N0, N160, N320. The dataset text says these are kg N/ha.
	d <- data.frame(
		trial_id = "Oxapampa",
		country = "Peru",
		location = "Oxapampa",
		adm1 = "Pasco",
		crop = "potato",
		variety = r$Variety,
		rep = as.integer(r$Repetition),
		treatment = as.character(r$Treatment),
		N_fertilizer = as.numeric(sub("^N", "", r$Nitrogen_rate)),
		pathogen = "Phytophthora infestans",
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = NA
	)

	carobiner::write_files(path, meta, d)
}
