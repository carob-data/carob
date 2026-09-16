# R script for "carob"
# license: GPL (>=3)

## ISSUES: Suggest to REJECT

# This dataset is about roots being stored for propagation, not tuber yield
# It is not a field trial - storage/propagation physiology experiment done in containers
# Without seed-bed area not possible to calculate vine_density
# Reported outcomes on sprouting rate, survival rate, drying rate, and seed-bed vine measurements (length/diameter/internode length)
# The 8 source files are heavily redundant and inconsistent: many sheets appear to be reorganized/re-derived summaries of the same 2 core datasets


carob_script <- function(path) {

"
Dataset for: The 'Performance of Different Orange-Fleshed- Sweetpotato Varieties under Triple S Technology'
 
Triple S (Storage in Sand and Sprouting) has emerged as future technology to conserve sweetpotato planting material in areas with long dry season. There has been a growing interest to scale up Triple S technology to sweetpotato farmers that live in dry areas of Sub-Saharan Africa. Although, the varieties in different countries are different, no evidence on the storage roots of different sweetpotato varieties performance in Triple S. Therefore, this study was conducted to understand sprouting, survival and planting material multiplication performance of roots of eight sweetpotato varieties in Triple S. Small, medium and large size root from each variety is stored in the standard Triple S during December, 2017 to End of March, 2018. From each variety, 60 roots (twenty roots from each size) are stored in one container. The experiment evaluated two factors, variety with 8 levels and root size with three levels in randomized complete block design (RCBD).
"

	uri <- "doi:10.21223/P3/BYYGSK"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
	                                data_organization = "CIP",
	                                publication = NA,
	                                project = NA,
	                                design = NA,
	                                data_type = NA,
	                                treatment_vars = "",
	                                response_vars = "",
	                                notes = "Rejected - storage container experiment",
	                                carob_contributor = "Stella Muthoni",
	                                carob_date = "2026-09-15",
	                                carob_completion = 100,
	                                carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "Data Dictionary_Participatory.xlsx"]     ## documents only the seed-bed file (f6)
	f2 <- ff[basename(ff) == "dried.xlsx"]                             ## drying rate by root size
	f3 <- ff[basename(ff) == "effect of size on sprouting.xlsx"]       ## sprouting rate by size x replicate
	f4 <- ff[basename(ff) == "Metadata setup.xlsx"]                    ## core storage-phase data (counts by variety)
	f5 <- ff[basename(ff) == "none sprouting.xlsx"]                    ## non-sprouted rate by root size
	f6 <- ff[basename(ff) == "performance of roots on root seed bed.xlsx"]  ## core seed-bed data (vine measurements)
	f7 <- ff[basename(ff) == "Size Vs sprouting_analyzed.xlsx"]        ## duplicate of f3 + statistical test output
	f8 <- ff[basename(ff) == "sproutablity.xlsx"]                      ## sprouting rate by variety, some sheets inconsistent with f3

# all scripts must end like this
	#carobiner::write_files(path, meta, d)
}


