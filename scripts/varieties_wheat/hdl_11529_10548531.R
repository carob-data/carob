# R script for "carob"


carob_script <- function(path) {

"The Semi-Arid Wheat Screening Nursery (SAWSN) is a single replicate trial that contains diverse spring bread wheat (Triticum aestivum) germplasm adapted to low rainfall, drought prone, semi-arid environments typically receiving less than 500 mm of water available during the cropping cycle. CIMMYT's breeding approach attempts to combine high yield potential with drought resistance for ME4. The combination of water-use efficiency and water responsive traits plus yield potential is important in drought environments where rainfall is frequently erratic across years. When rains are significantly above average in certain years, the crop must respond appropriately (water responsive) with higher yields, while expressing resistance to the wider suite of diseases that appear under more favorable conditions. Constrains including leaf, stem and yellow rusts, andSeptoria spp., Fusarium spp., Pyrenophora tritici-repentis tan spot, nematodes and root rots must be considered. It is distributed to 120 locations, and contains 150-250 entries. (2019)"

	uri <- "hdl:11529/10548531"
	group <- "varieties_wheat"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication= NA,
		project="Semi-Arid Wheat Screening Nursery",
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety_code",
		carob_contributor= "Blessing Dzuda",
		carob_effort = NA,
		carob_date="2024-04-25"
	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)	
	
	carobiner::write_files(path, meta, d)
}


