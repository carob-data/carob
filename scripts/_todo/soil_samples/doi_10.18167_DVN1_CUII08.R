# R script for "carob"
# license: GPL (>=3)

## NOTES
# Laboratory phosphorus extractions on soil collected from five field trials along a toposequence in Réunion. The table has soil type and replicate, not a field-site name and not the fertilizer rate, so this is recorded as a survey and the design is not inferred.
# The 230323 file is used. The 180919 file is the same extractions without P-total.
# Pi-Olsen (mg/kg) is soil_P. Olsen-Dabin is a separate extraction and is not forced into soil_P (values exceed 200 mg/kg).
# Pr-24h and Pr-1min are omitted: the readme says they are calculated from Eq. (2) in the paper.
# Pi-water and Pi-DGT are mg/L in the 230323 readme, not mg/kg.
# P-total is g/kg, converted to mg/kg.
# Country is geographicCoverage Réunion. No coordinates in the table.
# Publication DOI is 10.1111/sum.12457.

## ISSUES
# Fertilizer history and trial site names are not columns. No coordinates.
# Olsen P and total P both exceed the terminag maxima on these Andosols. Olsen-Dabin is kept out of soil_P.
# write_files: missing latitude, longitude; unknown variable soil_P_Olsen_Dabin_; unknown variable soil_P_CaCl2_; unknown variable soil_P_water_mg_L_; unknown variable soil_P_DGT_mg_L_; out of bounds soil_P (2.5, 282.432), soil_P_total (1830, 5830).

## NEW VARIABLES
# soil_P_Olsen_Dabin_: inorganic P by the Olsen-Dabin extraction (mg/kg)
# soil_P_CaCl2_: inorganic P extracted in 0.001 M CaCl2 (mg/kg)
# soil_P_water_mg_L_: water-extractable inorganic P (mg/L)
# soil_P_DGT_mg_L_: DGT inorganic P (mg/L)

## NOT PROCESSED
# Dataverse_Nobile et al_SUM_180919.xlsx: same extractions as the 230323 file, without P-total
# Pr-24h and Pr-1min: calculated isotopic-exchange values

carob_script <- function(path) {

"
Replication Data for: \"Phosphorus sorption capacity and availability along a toposequence of agricultural soils: distinct effects of soil type and decadal fertilizer applications\"

Dataset of the paper entitled \"Phosphorus sorption capacity and availability along a toposequence of agricultural soils: distinct effects of soil type and decadal fertilizer applications\".
Five field trials where mineral and organic fertilizers were applied for a decade along a toposequence (two andosols, one andic cambisol, one nitisol and one arenosol) were investigated. Legacy phosphorus (i‧e. phosphorus accumulated in soil with fertilizer applications minus phosphorus lost by plant uptake, erosion and leaching) was used to study the effect of fertilizer application rate. Phosphorus availability was determined by classic extractions and by the diffusive gradients in thin films technique. The solid-solution partitioning coefficient of inorganic phosphorus was determined to assess the soil Pi sorption capacity.
"

	uri <- "doi:10.18167/DVN1/CUII08"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1111/sum.12457",
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 60,
		carob_effort = 0.5
	)

	num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "na")] <- NA
		as.numeric(x)
	}

	f <- ff[basename(ff) == "Dataverse_Nobile et al_SUM_230323.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")

	d <- data.frame(
		country = "Réunion",
		sample_id = paste(r$Number, r$Replicate, sep="-"),
		rep = as.integer(r$Replicate),
		soil_type = r[["Soil type"]],
		soil_P = num(r[["Pi-Olsen"]]),
		soil_P_method = "Olsen",
		soil_P_Olsen_Dabin_ = num(r[["Pi-Olsen-Dabin"]]),
		soil_P_CaCl2_ = num(r[["Pi-CaCl2"]]),
		soil_P_water_mg_L_ = num(r[["Pi-water"]]),
		soil_P_DGT_mg_L_ = num(r[["Pi-DGT"]]),
		soil_P_total = num(r[["P-total"]]) * 1000,
		on_farm = FALSE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	# country from geographicCoverage. geo_from_source FALSE: no coordinates in the table.

	carobiner::write_files(path, meta, d)
}
