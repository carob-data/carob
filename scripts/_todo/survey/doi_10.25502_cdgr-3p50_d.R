# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot-level analysis file for a soybean-district household survey in Malawi, 2016/17 (dictionary text on intercropping and the dataset title). One row per plot.
# District is recovered from the mutually exclusive dummy columns Lilongwe, Mchinji, Dedza, Ntchisi, Kasungu and Mzimba (each matches one District_Malawi code). Village_Malawi is location. EPA and Section are extension units, not adm3.
# AREA is hectares and Qty_fertilizer_per is kg/ha of fertiliser (dictionary) but the product is not named, so the rate is fertilizer_rate_kg_ha_ and elemental N/P/K are not calculated.
# TLcrop_value_plot105_ha_w is total crop value in MWK/ha (dictionary), stored as crop_value with currency MWK. It is not a yield.
# Main_crop is an undocumented numeric code. It is main_crop_code_. soybeanBI means the household grew soybean, not that Main_crop equals soybean, so crop is not filled from that flag. TabaccoDummy is "grew tobacco", not a variety.
# planting_date 2016 is the 2016/17 season named in the dictionary, not a day of planting.
# Sex of the plot manager is 0/1 with no legend, so sex is not set.

## ISSUES
# Main_crop codes (1, 2, 3, 5, 22 and others) have no value labels. Do not guess them from soybeanBI. Yield in kg/ha is not in the file.
# No coordinates. Georeference villages in Lilongwe, Mchinji, Dedza, Ntchisi, Kasungu and Mzimba.
# Fertiliser product is unknown, so a single kg/ha rate cannot be split into N, P and K.
# The abstract reads like a journal paper, but the deposit has no paper DOI.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude; unknown crop_rotation_used_, minimum_tillage_, fertilizer_rate_kg_ha_, main_crop_code_, improved_seed_, residue_retention_.

## NEW VARIABLES
# main_crop_code_: Main_crop numeric code
# fertilizer_rate_kg_ha_: Qty_fertilizer_per, kg/ha of unnamed fertiliser
# crop_rotation_used_: crop rotation on the plot (0/1). Not mapped to crop_rotation, which is a list of crop names.
# improved_seed_: improved seed used (0/1)
# residue_retention_: residue retention (0/1)
# minimum_tillage_: zero/minimum tillage (0/1)

## NOT PROCESSED
# Household economics and network columns: age_manger, Agehh2, Joint_decision, Main_occu, Mar_system, Marital_stat, Member_org, trader and relative-reliance counts, totallabor, asset values, per_cap_exp, TLU, Ext_contact, credit_access, Dst_districtmrkt, Plot_manager, Decision_gender, Plot_owner, owner_managedplot.
# Soil perception dummies Poorfertility, Mfertile, Vfertile, shallowsoil, Mdeepsoil, deepsoil, Slope_*.
# soybeanBI, maize_cbeansInt, TabaccoDummy: household-level crop flags, not the plot crop code (see NOTES).
# LNTLcrop_value_plot105_ha_w: log of the value already stored.
# data_dictionary.csv, aside from the descriptions cited above.


carob_script <- function(path) {

"Household Survey data in major soybean growing districts in Malawi – 2016/17

It is widely recognized that female farmers have considerably less access to productive assets and support services than male farmers. There is limited evidence of gender gaps in technology adoption and agricultural productivity after accounting for the differential access to factors of production between males and females. This study investigates the gender differences in the adoption of improved technologies and agricultural productivity in Malawi using nationally representative data collected from 1600 households and 5238 plots. We used a multivariate probit model to analyze the gender differences in the adoption of improved technologies, including intercropping, use of improved varieties, crop rotation and residue retention, manure use, and minimum tillage. To analyze gender differences in agricultural productivity, we used an exogenous switching regression (ESR) model and re-centered influence function decomposition. We found that female plot managers were more likely to adopt intercropping and minimum tillage but less likely to adopt crop rotation and use improved varieties than male plot managers. The ESR model estimation results showed that female-managed plots were 14.6–23.1% less productive than male- managed plots. The gender productivity gaps also indicated that female plot managers had an 8.2% endowment advantage but a 23.1% structural disadvantage than male plot managers. The importance of structural effects in accounting for the gender productivity gap highlights the need for policies and agricultural development programs that consider the underlying factors shaping gender productivity gaps rather than focusing solely on agricultural production factors.
"

	uri <- "doi:10.25502/cdgr-3p50/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.4
	)

	r <- read.csv(ff[basename(ff) == "gender_data_final.csv"], stringsAsFactors=FALSE)

	adm2 <- rep(NA_character_, nrow(r))
	adm2[r$Lilongwe == 1] <- "Lilongwe"
	adm2[r$Mchinji == 1] <- "Mchinji"
	adm2[r$Dedza == 1] <- "Dedza"
	adm2[r$Ntchisi == 1] <- "Ntchisi"
	adm2[r$Kasungu == 1] <- "Kasungu"
	adm2[r$Mzimba == 1] <- "Mzimba"

	d <- data.frame(
		hhid = as.character(r$Household_malawi),
		plot_id = as.character(r$ID),
		country = "Malawi",
		adm2 = adm2,
		location = carobiner::fix_name(r$Village_Malawi, "title"),
		field_size = r$AREA,
		fertilizer_rate_kg_ha_ = r$Qty_fertilizer_per,
		crop_value = r$TLcrop_value_plot105_ha_w,
		currency = "MWK",
		main_crop_code_ = as.character(r$Main_crop),
		OM_used = r$Manure_app == 1,
		intercropped = r$Intercropping == 1,
		crop_rotation_used_ = r$Crop_rotation,
		improved_seed_ = r$improved_seed,
		residue_retention_ = r$Residue,
		minimum_tillage_ = r$Zero_min,
		hh_size = r$HH_size,
		planting_date = "2016",
		stringsAsFactors = FALSE
	)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
