# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. no credible yield, some values are provided as range and some are missing


carob_script <- function(path) {

"
Breeding Progress with Vitamin A, Iron and Zinc Biofortification, Drought Tolerance, and Sweetpotato Virus Disease Resistance in Sweetpotato

The data was generated between 2009 and 2020 in 14 collaborating African countries using different sets of study materials to design different experiments using both introduced and local genotypes for: 1) Adapting population and variety improvement schemes designed to accelerate release of sweetpotato varieties 2) Development of a procedure to characterize germplasm for sweetpotato virus disease resistance 3) Development of a procedure to characterize drought tolerance in sub-Saharan Africa 4) Development of a procedure of breeding for high iron and zinc, and  5) Development of genomic and digital tools for sweetpotato breeding.
"

	uri <- "doi:10.21223/JTZY5W"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIP; NCSU",
		publication = "",
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		notes = "",
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-09-15",
		carob_completion = 0,	
		carob_effort = 1
	)

	f1 <- ff[basename(ff) == "00_Data_Biofortification_list.xls"]
	f2 <- ff[basename(ff) == "01_Data_Biofortification_Population_Uganda.xlsx"]
	f3 <- ff[basename(ff) == "01_Data_Biofortification_Released varieties.xlsx"]
	f4 <- ff[basename(ff) == "02_Data_Biofortification_SPVD_T5.xlsx"]
	f5 <- ff[basename(ff) == "03_Data_Biofortification_SPVD_T6.xlsx"]
	f6 <- ff[basename(ff) == "04_Data_Biofortification_drought.xlsx"]
	f7 <- ff[basename(ff) == "05_Data_Biofortification_Minerals.xlsx"]
	f8 <- ff[basename(ff) == "06_Dictionary_Biofortification_2020.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="1a_PopUg_A_50parents")
	r2b <- carobiner::read.excel(f2, sheet="1b_PopUg_B_80parents")
	r2c <- carobiner::read.excel(f2, sheet="Dictionary")
	r3 <- carobiner::read.excel(f3)
	r4a <- carobiner::read.excel(f4, sheet="Minimal_data")
	r4b <- carobiner::read.excel(f4, sheet="Material_list_2SPVD_T5")
	r4c <- carobiner::read.excel(f4, sheet="2SPVD_T5")
	r4d <- carobiner::read.excel(f4, sheet="3SPVD_T5")
	r4e <- carobiner::read.excel(f4, sheet="4SPVD_T5")
	r4f <- carobiner::read.excel(f4, sheet="5a_definiations")
	r5a <- carobiner::read.excel(f5, sheet="5SPVDParents_20216B_T6")
	r5b <- carobiner::read.excel(f5, sheet="6SPVDParents_2017A_T6")
	r5c <- carobiner::read.excel(f5, sheet="7SPVD_Freq_2018A_T6")
	r5d <- carobiner::read.excel(f5, sheet="8SPVD2018BA50xB80site2_T6")
	r5e <- carobiner::read.excel(f5, sheet="9SPVD2019BA50x80site1_T6")
	r6 <- carobiner::read.excel(f6)
	r7a <- carobiner::read.excel(f7, sheet="11aFe_Zn_T9")
	r7b <- carobiner::read.excel(f7, sheet="11b_BC_Fe_Zn_Sup")
	r7c <- carobiner::read.excel(f7, sheet="11c_DM_BC_Fe_Zn_NIRS_XRF")
	r7d <- carobiner::read.excel(f7, sheet="12NIRS_XRF_T10")
	r7e <- carobiner::read.excel(f7, sheet="13Raw_T11")
	r7f <- carobiner::read.excel(f7, sheet="14Means_T11")
	r7g <- carobiner::read.excel(f7, sheet="15Weights")
	r8a <- carobiner::read.excel(f8, sheet="Sheet2")
	r8b <- carobiner::read.excel(f8, sheet="Sheet3")

	carobiner::write_files(path, meta, d)
}

