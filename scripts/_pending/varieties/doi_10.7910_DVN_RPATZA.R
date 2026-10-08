# R script for "carob"
# license: GPL (>=3)

## ISSUES
# rejected because dataset has no meaningful variables to standardize


carob_script <- function(path) {

"
Common Bean variety releases in Africa

The Pan Africa Bean Research Alliance is a network of national agricultural research centers (NARS), and private and public sector institutions that work to deliver better beans with consumer and market preferred traits to farmers. The datasets presented here draw from 17 Sub Saharan countries that are members of PABRA. The dataset on released bean varieties is a collection of 513 bean varieties released by NARS and there characteristics. The dataset on bean varieties and the relationship to constraints provides the 513 bean varieties on the basis of resistance to constraints such as fungal, bacterial, viral, diseases and tolerance to abiotic stresses. There is also a dataset of bean varieties that have been released in more than one country, useful for moving seed from one country to another and facilitating regional trade. The dataset on Niche market traits provides the market defined classifications for bean trade in Sub Saharan Africa as well as varieties that fall into these classifications. 

The datasets are an update to the 2011 discussion on PABRAs achievement in breeding and delivery of bean varieties in Buruchara et. 2011 in pages 236 and 237 here:  http://www.ajol.info/index.php/acsj/article/view/74168 . It is also an update to a follow up to this discussion in Muthoni, R. A., Andrade, R. 2015 on the performance of bean improvement programmes in sub-Saharan Africa from the perspectives of varietal output and adoption in chapter 8. here: http://dx.doi.org/10.1079/9781780644011.0148.   The data is extracted from the PABRA M&E database available here (http://database.pabra-africa.org/?location=breeding).
"


	uri <- "doi:10.7910/DVN/RPATZA"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "agronomy",
		treatment_vars = "",
		response_vars = "", 
		notes = "",
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-09-15",
		carob_completion = 0,	
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "1. Number of varieties released.xlsx"]
	f2 <- ff[basename(ff) == "2b.Codebook.xls"]
	f3 <- ff[basename(ff) == "3. Constraints, growth habit and Days to mautrity.xlsx"]
	f4 <- ff[basename(ff) == "4. Multiple bean variety releases across PABRA Countries.xlsx"]
	f5 <- ff[basename(ff) == "5. Niche traits.xlsx"]
	f6 <- ff[basename(ff) == "2a.Bean varieties and their details.csv"]

	r1a <- carobiner::read.excel(f1, sheet="Sheet1")
	r1b <- carobiner::read.excel(f1, sheet="Sheet2")
	r1c <- carobiner::read.excel(f1, sheet="Sheet3")
	r2 <- carobiner::read.excel(f2)
	r3a <- carobiner::read.excel(f3, sheet="Sheet1")
	r3b <- carobiner::read.excel(f3, sheet="Sheet2")
	r4a <- carobiner::read.excel(f4, sheet="Sheet1")
	r4b <- carobiner::read.excel(f4, sheet="Sheet2")
	r4c <- carobiner::read.excel(f4, sheet="Sheet3")
	r5a <- carobiner::read.excel(f5, sheet="Niche traits")
	r5b <- carobiner::read.excel(f5, sheet="Sheet3")
	r6 <- read.csv(f6)

		carobiner::write_files(path, meta, d)
}

