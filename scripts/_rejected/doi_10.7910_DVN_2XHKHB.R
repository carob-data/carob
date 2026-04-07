# R script for "carob"
# license: GPL (>=3)

#
# REJECTED
# no data in the files, only images
#


carob_script <- function(path) {

"
Performance of diverse upland rice cultivars in low and high soil fertility conditions in West Africa

Traditional tropical japonica (Oryza sativa) and Oryzaglaberrimacultivars are typically grown in lowinput, subsistence production systems in the uplands of West Africa by resource-poor farmers. In these systems, low soil fertility (LF), which is generally associated with lower organic carbon content, and N and P availability, is one of the major constraints to rice productivity. Thus, cultivars adapted to LF are needed for the food security of farmers, who would otherwise be solely reliant on nutrient inputs to increase productivity. This study evaluated the performance of six diverse cultivars grown in LF and high soil fertility (HF) conditions with supplemental irrigation over two seasons. Average grain yield across all cultivars in LF was 54% of that in HF (156 vs. 340 g m_2). Three improved indicarice cultivars and CG 14 (O. glaberrima) out-yielded Morobe´ re´kan (traditional tropical japonica) and WAB450-IBP-38-HB (progeny from interspecific hybridization of tropical japonica and O. glaberrima) in LF (181 vs. 105 g m_2 on average). The high grain yield in LF was the result of large spikelet number m_2 due to superior tillering ability and high harvest index rather than biomass production. The high-yielding cultivars in LF consistently had lower leaf chlorophyll content and higher specific leaf area during the period from the early vegetative stage through the reproductive stage. Among them, two indicacultivars (B6144F-MR-6-0-0 and IR 55423-01) were also high yielding in HF. The use of improved indicacultivars adapted to LF, but also with input-responsiveness, appears to offer an attractive and economical approach to improving upland rice productivity and widening genetic diversity in this region.
"

	uri <- "doi:10.7910/DVN/2XHKHB"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "Africa Rice Center (WARDA), 01 B.P. 2031, Cotonou, Benin",
		publication = "",
		project = NA,
		carob_date = "2026-04-06",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Aniruddha Gosh",
		completion = 0,	
		notes = ""
	)
	

	f1 <- ff[basename(ff) == "Performance of diverse upland rice cultivars in low and high soil fertility condition.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Sheet1")
	r1b <- carobiner::read.excel(f1, sheet="Sheet2")
	r1c <- carobiner::read.excel(f1, sheet="Sheet3")
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
