# REJECTED 
# Reason: no source columns matched draft() terminag heuristics ,dataset is a picture
# R script for "carob"
# license: GPL (>=3)



carob_script <- function(path) {

"
Genotypic  adaptation of rice to lowland hydrology in West Africa

In West Africa, the lowlands comprise a wide range of hydrological environments—from permanently flooded to permanently non-flooded conditions. Rice breeding programs must develop genotypes that are expected to perform well either across all hydrological environments or for a specific target population of environments. We evaluated 14 rice (Oryzaspp.) genotypes in seven experiments during 2 years (2007 and 2008) in Benin to investigate genotype×environment (G×E) interaction for grain yield, and to identify high-yielding genotypes and plant characteristics associated with high yield. The genotypes consisted of O. sativa indicagenotypes, including 'aerobic rice genotypes' and interspecific genotypes, developed from crossing O. sativa and O. glaberrimafor upland ('NERICA' genotypes) and lowland conditions ('NERICA-L'). The mean grain yield ranged from 231 to 588 gm−2 across experiments, with the highergrain yields from flooded lowland conditions. The G×E interaction accounted for 22% of the total sum of squares, with environment and genotype responsible for 71 and 8%, respectively. Three environment groups were identified from a pattern analysis on grain yield. Grouping was related to water availability, distinguishing (i) an aerobic environment, with rice grown under aerobic conditions with supplemental irrigation, (ii) a hydromorphic environment, with rice grown under rainfed conditions with drought spells at the vegetative stage, and (iii) a permanently flooded environment. An interspecific genotype WAB1159-4-10-15-1-3 produced high yields in both flooded and hydromorphic environments,while two interspecific genotypes, NERICA-L-6 and NERICA-L-54 performed well only in a flooded environment. An aerobic rice genotype B 6144F-MR-6-0-0 outyielded these three interspecific genotypes in the aerobic environment. High grain yields in the flooded and hydromorphic environments resulted from biomass accumulation rather than harvest index, whereas the higher yield in the aerobic environment was the result of harvest index rather than biomass accumulation. Genotypes with high growth vigor at 42 and 63 days after sowing tended to have higher yields in the flooded and aerobic environments. Grain yield in the hydromorphic environment was positively correlated with growth duration. We conclude that while interspecific breeding appears to offer an effective approach to improving lowland rice productivity, a systematic effort is needed to screen a wide range of O. sativa and interspecific genotypes across hydrology gradients in West Africa to identify genotypes that perform well across or within a specific target population of environments.
"

	uri <- "doi:10.7910/DVN/CWG4UZ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "Africa Rice Center (AfricaRice), 01 BP 2031, Cotonou, Benin; Japan International Cooperation Agency (JICA), Tokyo, Japan",
		publication = "",
		project = NA,
		carob_date = "2026-10-01",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "Genotypic adaptation of rice to lowland hydrology Saito et al 2010b.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Sheet1")
	r1b <- carobiner::read.excel(f1, sheet="Sheet2")
	r1c <- carobiner::read.excel(f1, sheet="Sheet3")
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
