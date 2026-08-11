# R script for "carob"
# license: GPL (>=3)

## ISSUES
#land_use was a main factor on the dataset and it was included on the metadata, but is not yet found in carob
#soil_SOC had out of bounds warnings.

carob_script <- function(path) {

"
Data for 'Drivers of soil organic carbon stocks at village scale in a sub-humid region of Zimbabwe'

These are the raw data of the paper 'Understanding drivers of soil organic carbon stocks at village scale in a sub-humid region of Zimbabwe' authored by Rumbidzai W. Nyawasha, Gatien N. Falconnier, Pierre Todoroff, Alexandre M.J.-C. Wadoux, Regis Chikowo, Adrien Coquereau, Louise Leroux, Camille Jahel, Marc Corbeels, Rémi Cardinael
"

	uri <- "doi:10.18167/DVN1/KVKWFL"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.catena.2025.108843",
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "none",
		response_vars = "none",
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-21",
		carob_completion = 80,	
		carob_effort = 9
	)
	
	f <- ff[basename(ff) == "Final dataset_Nyawasha Rumbidzai W.xlsx"]
	r <- carobiner::read.excel(f, sheet="Final dataset_Nyawasha Rumbidza")
	#r1 <- carobiner::read.excel(f1, sheet="Legend")
	
	d <- data.frame(
	  country = "Zimbabwe",
	  adm1 = "Mashonaland East",
	  adm2 = "Murehwa",
	  adm3 = "Ward 28",
	  location = r$Village,
	  sample_id = as.character(r$Spectroscopy_number),
	  soil_type = r$Soil_Type,
	  soil_texture = tolower(r$Texture_Class),
	  soil_clay = as.numeric(r$`Clay %`),
	  soil_silt = as.numeric(r$`Silt %`),
	  soil_sand = as.numeric(r$`Sand %`),
	  land_use = tolower(r$Landuse),
	  soil_SOC = as.numeric(r$`Total C g kg`),
	  soil_N_total = as.numeric(r$`Total N g kg`),
	  soil_bd = as.numeric(r$`Bulk Density g cm3`),
	  soil_C_stock = as.numeric(r$`C Stocks Mg ha`)
	)
	depth <- do.call(rbind, strsplit(r$Depth_1, "-"))
	d$depth_top <- as.numeric(depth[,1])
	d$depth_bottom <- as.numeric(depth[,2])

	texture <- c(loamysand="loamy sand", sandyclay = "sandy clay", sandyclayloam = "sandy clay loam", sandyloam="sandy loam")
	d$soil_texture <- texture[d$soil_texture]
	
	d$on_farm <- TRUE
	d$is_survey <- TRUE
 
	geo <-  data.frame(
		location = c("Makombe", "Chitemerere", "Manjonjo"),
		latitude = c(-17.818, -17.822, -17.801),
		longitude = c(31.591, 31.619, 31.599)
	)
 
	d <- merge(d, geo, by = "location", all.x = TRUE)
	d$geo_from_source <- FALSE #!!

	carobiner::write_files(path, meta, d)
}


