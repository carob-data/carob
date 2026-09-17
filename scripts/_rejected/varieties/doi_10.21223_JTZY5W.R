# R script for "carob"
# license: GPL (>=3)

## REJECTED

# No outcomes that standardizeable for carob
# f2: breeding-population parent lists - not a trial, no measurements
# f3: released varieties only at country level; yield column mixed text/range/qualitative, not parseable
# f4, f5: real trial structure (design/location/dates in places), but core measurement columns (NOPH, VW, NOCR, CRW, HI, etc.) are undocumented
# f6: No location data and abbreviations are undocumented
# f7: Fe/Zn/beta-carotene/dry matter data; some values are averaged across multiple unnamed locations - not attributable to a single site
# f8: file named "Dictionary" - confirmed empty, unfilled template


carob_script <- function(path) {

"
Breeding Progress with Vitamin A, Iron and Zinc Biofortification, Drought Tolerance, and Sweetpotato Virus Disease Resistance in Sweetpotato

The data was generated between 2009 and 2020 in 14 collaborating African countries using different sets of study materials to design different experiments using both introduced and local genotypes for: 1) Adapting population and variety improvement schemes designed to accelerate release of sweetpotato varieties 2) Development of a procedure to characterize germplasm for sweetpotato virus disease resistance 3) Development of a procedure to characterize drought tolerance in sub-Saharan Africa 4) Development of a procedure of breeding for high iron and zinc, and  5) Development of genomic and digital tools for sweetpotato breeding.
"

	uri <- "doi:10.21223/JTZY5W"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
	                                data_organization = "CIP; NCSU",
	                                publication = "doi:10.3389/fsufs.2021.616674",
	                                project = NA,
	                                design = "Multiple sub-studies across 14 African countries, 2009-2020: breeding population lists, released-variety characterization, SPVD (virus disease) resistance trials (RCBD, Westcott Design), drought tolerance trials, and Fe/Zn/beta-carotene mineral characterization (NIRS, XRF, ICP).",
	                                data_type = "on-station experiment",
	                                treatment_vars = "variety",
	                                response_vars = "yield; harvest_index; flesh_color",
	                                notes = NA,
	                                carob_contributor = "Stella Muthoni",
	                                carob_date = "2026-09-15",
	                                carob_completion = 100,
	                                carob_effort = 3
	)

	f1 <- ff[basename(ff) == "00_Data_Biofortification_list.xls"]                  ## index/manifest listing the other 17 files
	f2 <- ff[basename(ff) == "01_Data_Biofortification_Population_Uganda.xlsx"]    ## breeding-population parent lists (50 male + 80 female parents), qualitative trait tags only
	f3 <- ff[basename(ff) == "01_Data_Biofortification_Released varieties.xlsx"]   ## 326 released varieties - yield, flesh/skin color, beta-carotene, dry matter, disease reactions, pedigree
	f4 <- ff[basename(ff) == "02_Data_Biofortification_SPVD_T5.xlsx"]              ## virus disease resistance trials, Namulonge Uganda
	f5 <- ff[basename(ff) == "03_Data_Biofortification_SPVD_T6.xlsx"]              ## virus disease resistance trials, larger scale (6800+ rows) - no dictionary of its own
	f6 <- ff[basename(ff) == "04_Data_Biofortification_drought.xlsx"]              ## drought tolerance trial - irrigation treatment, yield, harvest index, minerals/sugars/starch
	f7 <- ff[basename(ff) == "05_Data_Biofortification_Minerals.xlsx"]             ## Fe/Zn/beta-carotene characterization via NIRS/XRF/ICP - full mineral panel
	f8 <- ff[basename(ff) == "06_Dictionary_Biofortification_2020.xlsx"]           ## intended to define the SPVD T5/T6 column codes but its empty

	## f2 database has breeding-population parent with qualitative trait tags only;no measurements
	r2 <- carobiner::read.excel(f2)
	
	## f3 database has released varieties only at country level
	## Yield column is mixed between character, range and text; not standardizeable for Carob
	r3 <- carobiner::read.excel(f3)
	
	# f4 mutiple datasheets of trials done in Namulonge
	# no interpretable dictionary of what the abbreviation means
	r4a <- carobiner::read.excel(f4, sheet="Minimal_data")              
	r4b <- carobiner::read.excel(f4, sheet="Material_list_2SPVD_T5")
	r4c <- carobiner::read.excel(f4, sheet="2SPVD_T5")               
	r4d <- carobiner::read.excel(f4, sheet="3SPVD_T5")                  
	r4e <- carobiner::read.excel(f4, sheet="4SPVD_T5")                 
	r4f <- carobiner::read.excel(f4, sheet="5a_definiations")  
	
	# f5 also has multiple sheets with undocumented abbreviations and no concrete location
	r5a <- carobiner::read.excel(f5, sheet="5SPVDParents_20216B_T6")      
	r5b <- carobiner::read.excel(f5, sheet="6SPVDParents_2017A_T6")       
	r5c <- carobiner::read.excel(f5, sheet="7SPVD_Freq_2018A_T6")         
	r5d <- carobiner::read.excel(f5, sheet="8SPVD2018BA50xB80site2_T6")   
	r5e <- carobiner::read.excel(f5, sheet="9SPVD2019BA50x80site1_T6") 
	
	# f6 Single drought tolerance sheet but no location data and most abbreviation are undocumented
	r6 <- carobiner::read.excel(f6, sheet="10Drought_T7")
	
	# f7 minerals from dry matter weight from different locations
	# sometimes averaged across multiple sites - not standardizeable for carob
	r7a <- carobiner::read.excel(f7, sheet="11aFe_Zn_T9")
	r7b <- carobiner::read.excel(f7, sheet="11b_BC_Fe_Zn_Sup")
	r7c <- carobiner::read.excel(f7, sheet="11c_DM_BC_Fe_Zn_NIRS_XRF")
	r7d <- carobiner::read.excel(f7, sheet="12NIRS_XRF_T10")
	r7e <- carobiner::read.excel(f7, sheet="13Raw_T11")
	r7f <- carobiner::read.excel(f7, sheet="14Means_T11")
	r7g <- carobiner::read.excel(f7, sheet="15Weights")               
	
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}


