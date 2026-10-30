# R script for "carob"


carob_script <- function(path) {

"The historic crossing block (HCB) is a collection of all the parents of spring wheat pre-breeding crossing block that include landraces, synthetics, synthetic derivatives, elite lines, high biomass lines, lines with rust resistance, and some varieties. Every year, to incorporate novel alleles into elite back grounds several strategic crosses-source× sink, landrace × elite, synthetic × elite-are made based on the physiological breeding approach (Reynolds et al. 2017b). The HCB is comprised of three panels: germplasm used for the crosses for yield potential (CB-YP), drought (CB-D), and heat stress (CB-H). The number of lines in each panel increased from 128 to 311 in CB-YP, from 123 to 219 in CB-D, and from 80 to 195 in CB-H during 2012-2017. A total of 119,144 SNP markers were identified on the germplasm. After quality control (removing markers with missing data > 30%, and minor allele frequency < 5%), 13135, 11717, and 14441 polymorphic SNPs for CB-YP, CB-D, and CB-H, respectively were used for further analysis.

    

"
	uri <- "hdl:11529/10548509"
	group <- "snp"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		project=NA,
		#data_citation="Sukumaran, Sivakumar, 2020, Physiological pre-breeding crossing block dataset, https://hdl.handle.net/11529/10548509, CIMMYT Research Data & Software Repository Network, V1, UNF:6:zXWmVpPwNsYZNjlM1Pd6sQ== [fileUNF]",
		publication=NA,
		data_organization = "CIMMYT",
		data_type="experiment", 
		carob_contributor="Shumirai Manzvera",
		carob_date="2024-02-08"
	)


#	carobiner::write_files(meta, d, path=path)
}
	
