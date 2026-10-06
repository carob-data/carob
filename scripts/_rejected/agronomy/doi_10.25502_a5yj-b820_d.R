# R script for "carob"
# license: GPL (>=3)

## REJECTED
# data was standardised as part of doi:10.25502/bf6e-0181/d

carob_script <- function(path) {

"
Datasets on yield components of fertilized improved and local varieties of Cassava grown in the highlands of South Kivu, DR Congo

The use of mineral fertilizer and organic inputs with an improved and local variety of cassava allow firstly to identify nutrient limitations to cassava production, and secondly to investigate the effects of variety and combined application of mineral and organic inputs on cassava growth and yields in the highland conditions of the Democratic Republic of Congo (DR Congo). Data on growth parameters, yields and yield components of the improved and local varieties of cassava, economic analysis and soil parameters, collected during two growing cycles of cassava are presented. The data support a research article which is under review “Increased cassava growth and yields through improved variety use and fertilizer application in the highlands of South Kivu, Democratic Republic of Congo” [1]. Data on plant height and diameter was measured throughout the growing period of the crop while the data on the storage root, stem, tradable storage root and non-tradable storage root was determined at 12 months after planting (MAP) of the field experiments. The economic analysis was performed using a simplified financial analysis where the additional benefits were calculated relative to the respective control treatments while the total costs included the purchasing prices of fertilizer and the additional net benefits, the revenue from the increased storage root yields due to fertilizer application. The value cost ratio (VCR) was calculated as the additional net benefits over the cost of fertilizer purchase.
"
	uri <- "doi:10.25502/a5yj-b820/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		notes = "",
		carob_contributor = "Your Name",
		carob_date = "2026-10-05",
		carob_completion = 0,	
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "varietyfertilizer_effect_data.csv"]
	f2 <- ff[basename(ff) == "nutrient-response_data.csv"]
	f3 <- ff[basename(ff) == "dataset_cassava-growth_data_dictionary.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		location = r1[["Site"]],
		season = r1[["Season"]],
		rep = r1[["Replicate"]],
		variety = r1[["Variety"]],
		yield = r1[["Total_yield_Root_stem"]]
	)
##r1: "ID", "Village", "Fertilizer", "Germination", "H3_4MAP", "H6MAP", "H8MAP", "H10MAP", "H12MAP", "D3_4MAP", "D6MAP", "D8MAP", "D10MAP", "D12MAP", "FW_StorageRoot", "FW_Stem", "Harvest_Index_HI", "Nr_tradRoot", "Nr_nontradRoot", "FW_TradRoot", "FW_nontradRoot", "X", "X.1", "X.2"


	d2 <- data.frame(
		location = r2[["Site"]],
		season = r2[["Season"]],
		rep = r2[["Replicate"]],
		variety = r2[["Variety"]],
		yield = r2[["Total_yield_Root_stem"]]
	)
##r2: "ID", "Village", "Fertilizer", "Germination", "H3_4MAP", "H6MAP", "H8MAP", "H10MAP", "H12MAP", "D3_4MAP", "D6MAP", "D8MAP", "D10MAP", "D12MAP", "FW_StorageRoot", "FW_Stem", "Harvest_Index_HI", "Nr_tradRoot", "Nr_nontradRoot", "FW_TradRoot", "FW_nontradRoot"


	d3 <- data.frame(
		country = r3[["coverage.country"]]
	)
##r3: "Tab", "Column", "description_abstract", "title", "data_type", "Measurement", "creator", "contributors", "source", "source.date", "source.file", "identifier", "subject", "subject.agrovoc", "format", "language", "relation", "coverage", "rights"

	carobiner::write_files(path, meta, d)
}
