# R script for "carob"
# license: GPL (>=3)

## NOTES
# Kericho LDSF field protocol only (156 plots, GPS, landform, cultivation).
# Plot area 1000 m2 (field guide). Soil samples were collected (abstract) but
# no IR spectra or wet chemistry are in this deposit.

## ISSUES
# No lab soil nutrient data in the files (ppm chemistry not available).
# Tree/shrub subplot counts and erosion codes not mapped; see LDSF codebook
# 4. ldsf_description of codesv4.tab.


carob_script <- function(path) {

"
Land and Soil Health Assessment in the Nile-Congo Sentinel Landscape

The LDSF was carried out at two-100 km2 sites within the Nile-Congo Sentinel Landscape: Kericho in Kenya.  
The LDSF is a spatially stratified, randomized sampling design, developed to provide a biophysical baseline at landscape level and a monitoring and evaluation framework for assessing processes of land degradation and effectiveness of rehabilitation measures over time. 
Measured variables include: land cover, tree and shrub densities, tree biodiversity, erosion prevalence, infiltration capacity, along with an assessment of impact to habitat and occurrence of soil conservation structures. Soil samples were also collected (320 top (0-20 cm) and sub (20-50 cm) soil samples per site) and were processed in Burkina Faso.  Processed samples were subjected to infrared spectroscopy and wet chemistry analysis. These combined data sets will be used to assess soil and ecosystem health for the landscape in more detail.
"

	uri <- "doi:10.34725/DVN/BOFN8B"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=2,
		data_organization = "ICRAF",
		publication = NA,
		project = "LDSF Nile-Congo Sentinel Landscape",
		design = "LDSF spatially stratified randomized plots",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Kericho ldsf for database.csv"]
	r <- read.csv(f, check.names=FALSE)

	cult <- as.integer(r$PlotCultMgd)
	d <- data.frame(
		country = carobiner::fix_name(r$Country, "title"),
		location = carobiner::fix_name(r$Site, "title"),
		plot_id = as.character(r$Plot),
		sample_id = paste(r$Site, r$Cluster, r$Plot, sep="_"),
		latitude = as.numeric(r$Latitude),
		longitude = as.numeric(r$Longitude),
		elevation = as.numeric(r$Altitude),
		geo_uncertainty = as.numeric(r$Accuracy),
		geo_from_source = TRUE,
		date = as.character(as.Date(r$Date, tryFormats=c("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"))),
		land_form = r$MajLform,
		landscape_position = r$PosTopoSeq,
		on_farm = cult %in% 1,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
