# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cassava preliminary yield trial, 84 plots (42 clones x Peg_No 1 and 2). Peg_No is used as rep.
# Yield_t_ha equals (Rt_Wt_kg / No_of_Pl_Harvtd) * 10 (correlation 1). That is fresh root kg per harvested plant times 10, i.e. t/ha at 10,000 plants/ha. The published column was used (x1000 to kg/ha), not recomputed.
# DM is dry matter of roots, percent (about 10-45, the usual cassava range). yield is fresh. yield_moisture = 100 - DM. dmy_storage = fresh yield x DM/100.
# Shoot fresh weight was scaled with the same 10,000 plants/ha factor as the root-yield column: Sht_W_kg / plants harvested x 10000.
# Dataset subject list includes Rainfed, so irrigated is FALSE.
# Group (Yellow/White) is kept as pulp_group_; it is not a separate agronomic treatment.

## ISSUES
# Site is not in the CSV. Dataset coverage point (lon 3.87427, lat 11.462) falls outside Cameroon and was not used. Georeference the Cameroon 2013-2014 PYT site.
# Planting and harvest months are not given. planting_date 2013 and harvest_date 2014 are the season in the title only.
# Root size, shape, peeling, skin and pulp columns are integer codes with no legend.
# Plot area is not given. The 10,000 plants/ha factor is implied by Yield_t_ha, not measured in this file.
# N/P/K rates are not reported.
# write_files: missing longitude, latitude, N/P/K. High cassava yield (up to 99 t/ha, left as Yield_t_ha). fwy_residue exceeds 100000 kg/ha on some plots because shoot kg per harvested plant was scaled to 10000 plants/ha.

## NOT PROCESSED
# metadata xlsx: dataset dictionary, not plot records. It does not name a village.
# RT_rot: rotten root count per harvested plot. Not converted to per ha because the plot area is unknown (see root_rot_count_).
# ID: row number.

## NEW VARIABLES
# pulp_group_: Yellow or White clone group in the file.
# root_size_score_, root_shape_score_, ease_of_peeling_, outer_skin_color_, inner_skin_color_, pulp_color_: undocumented integer scores.
# root_rot_count_: RT_rot, rotten roots in the harvested plot.
# storage_root_number_: No_of_Root / plants harvested x 10000, using the same plant density as Yield_t_ha.


carob_script <- function(path) {

"
Cassava preliminary yield trial in Cameroon 2013-2014

RTB is a partnership collaboration of five research centers, led by the International Potato Center, with decades of experience in these crops on different continents, including four CGIAR research centers (Bioversity International, the International Center for Tropical Agriculture, the International institute of Tropical Agriculture and the International Potato Center) and the French Agricultural Research Centre for International Development (CIRAD). By working with more than 360 other partners, these five centers mobilize complementary expertise and resources; avoid duplication of efforts; and create synergies to increase the benefits of their research for smallholder farmers, consumers and other stakeholders.
"

	uri <- "doi:10.25502/adjg-v174/d"
	group <- "varieties_cassava"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "RTB",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_storage",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.6,
		notes = NA
	)

	f <- ff[basename(ff) == "cassava_pyt_2013_2014.csv"]
	r <- read.csv(f)

	# Yield_t_ha is fresh t/ha. DM is root dry-matter percent.
	d <- data.frame(
		country = "Cameroon",
		trial_id = "Cameroon_PYT_2013",
		planting_date = "2013",
		harvest_date = "2014",
		rep = as.integer(r$Peg_No),
		plot_id = as.character(r$Peg_No),
		variety = r$Clone,
		treatment = r$Clone,
		pulp_group_ = r$Group,
		yield = r$Yield_t_ha * 1000,
		yield_moisture = 100 - r$DM,
		dmy_storage = r$Yield_t_ha * 1000 * r$DM / 100,
		fwy_residue = r$Sht_W_kg / r$No_of_Pl_Harvtd * 10000,
		storage_root_number_ = r$No_of_Root / r$No_of_Pl_Harvtd * 10000,
		root_size_score_ = r$Rt_SZ,
		root_shape_score_ = r$Rt_Shap,
		ease_of_peeling_ = r$Ease_of_pl,
		outer_skin_color_ = r$Out_sk_col,
		inner_skin_color_ = r$Inn_sk_col,
		pulp_color_ = r$Pulp_col,
		root_rot_count_ = r$RT_rot,
		crop = "cassava",
		yield_part = "roots",
		yield_isfresh = TRUE,
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
