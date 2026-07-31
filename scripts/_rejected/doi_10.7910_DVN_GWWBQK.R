# REJECTED 

# Source .xls contains no real spreadsheet cells - all 3 sheets hold WMF
# screenshot images of published tables


# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"
Plant characteristics associated with weed competitiveness of rice under upland and lowland conditions in West Africa

Weeds are a major constraint to rice (Oryza spp.) production in West Africa. Superior weed competitive rice genotypes may reduce weed pressure and improve rice productivity. Two upland and two lowland experiments were conducted in southern Benin to examine genotypic variations in weed-suppressive ability and grain yield under weedy conditions, and to identify plant characteristics that could be used as selection criteria for improved weed competitiveness.
"
  uri <- "doi:10.7910/DVN/GWWBQK"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "",
		carob_contributor = "Stella Muthoni",
		carob_date = "2026-07-29",
		notes = "",
		carob_completion = 100,
		carob_effort = 1
  )
  
  f1 <- ff[basename(ff) == "Plant characteristics associated with weed competitiveness of rice saito et al 2010a.xls"]
  r1a <- carobiner::read.excel(f1, sheet="Sheet1")
  r1b <- carobiner::read.excel(f1, sheet="Sheet2")
  r1c <- carobiner::read.excel(f1, sheet="Sheet3")
  ## r1a/r1b/r1c: all three sheets contain only embedded WMF screenshot
  ## images, no real tabular data - see ISSUES.
  
  return(FALSE)
}