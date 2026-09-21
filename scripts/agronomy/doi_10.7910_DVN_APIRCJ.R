# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# data cannot be processed because it is in form of an image


carob_script <- function(path) {

"
Combined effects of Stylosanthesguianensisfallow and tillage management on upland rice yield, weeds and soils in southern Benin

Intensifying upland rice cultivation has resulted in increased weed pressure and declining soil fertility and rice yield in West Africa. Integrated crop management technologies are needed for enhancing rice productivity. A field experiment was conducted from 2006 to 2008 in a TypicHaplustult soil in southern Benin to identify the optimal seeding date of stylo relay-cropped into upland rice, and to evaluate the effects of fallow treatment and tillage management on rice yield, weed biomass and soil properties. Stylosanthesguianensis(stylo), a legume species, was used as a short-term fallow crop. Rice was grown once each year and stylo was seeded during the wet season and grown until the next rice-growing season. The effects of fallow treatment and tillage management (no-tillage vs. manual-tillage) on weed biomass during the rice-growing season were evaluated in 2007 and 2008, whereas the effects on rice yield were examined in 2007 alone. Results indicated that stylo can be established as a relay crop withupland rice about 10 days after rice seeding. Stylo fallow reduced weed biomass by 71% and 95% and increased total biomass (weed + stylo + litter) by 594% and 107% at the end of the dry seasons in 2007 and 2008, respectively. No-tillage without stylo fallow increased weed biomass by 62–202% over manual-tillage during the rice-growing seasons, whereas stylo fallow reduced weed biomass by 45–83% and 11–36%, respectively, under no-tillage and manual-tillage management. There were no significant effects of fallow treatment and tillagemanagement on soil organic C, total N, inorganic N and extractable P. Rice yields following stylofallowwere 0.7 Mg ha_1 higher than after the natural fallow. Manual-tillage increased rice yield by 0.6 Mg ha_1 over no-tillage. Manual-tillage combined with stylo fallow can be recommended to smallholder farmers for improving upland rice productivity.
"

	uri <- "doi:10.7910/DVN/APIRCJ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "(Africa Rice Center (AfricaRice), 01 BP 2031 Cotonou, Benin, Tel.: +229 21 35 01 88; fax: +229 21 35 05 56.E-mail address: k.saito@cgiar.org.); (Africa Rice Center (AfricaRice), 01 BP 2031 Cotonou, Benin); ( African Agricultural Technology Foundation (AATF), P.O. Box 30709-00100, Nairobi, Kenya)",
		publication = "",
		project = NA,
		carob_date = "2026-09-21",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Mitchelle Njukuya",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "Combined effects of Stylosanthes guianensis fallow and tillage management saito et al Soil Tillage 2010.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Sheet1")
	r1b <- carobiner::read.excel(f1, sheet="Sheet2")
	r1c <- carobiner::read.excel(f1, sheet="Sheet3")
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
