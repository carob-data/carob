# R script for "carob"

carob_script <- function(path) {
  
"This dataset contains a subset of the samples collected during the AfSIS Phase I project and was a collaborative effort between World Agroforestry (ICRAF) and Rothamsted Research. The soil samples were retrieved from ICRAF Soil Archive: https://worldagroforestry.org/output/icraf-soil-archive-physical-archive-systematically-collected-soil-samples and subject to wet chemical analysis at Rothamsted Research in the UK under a Global Challenges Research Fund project, 'BBS/OS/GC/000014B: Chemical and Biological Assessment of AfSIS soils funded through the UK Biotechnology and Biological Sciences Research Council. This dataset includes the Site, Cluster, Plot as well as the GPS coordinates and wet chemistry data from 2002 samples collected from 18 countries and 51 LDSF sites. The original data collection was part of the AfSIS Phase I project, funded by the Bill and Melinda Gates Foundation (BMGF) and took place between 2009-2013. ICRAF and CIAT contributed the Site, Cluster, Plot and GPS coordinates for the soil samples, ICRAF organized the sub-sampling of the soil samples from the ICRAF physical archive in Nairobi and Rothamsted analysed the soil samples in the UK in 2017 and 2018. Visit our websites here: https://worldagroforestry.org/landhealth and https://www.rothamsted.ac.uk/. The AfSIS Phase I project funded by the Bill and Melinda Gates Foundation (BMGF) from 2009-2013, aimed to provide a consistent baseline of soil information across sub-Saharan Africa (SSA). Led by CIAT-TSBF, partners included: ISRIC, CIESIN, The Earth Institute at Columbia University and World Agroforestry (ICRAF). ICRAF led the systematic assessments of soil health using the Land Degradation Surveillance Framework (LDSF), which was developed at ICRAF, http://landscapeportal.org/blog/2015/03/25/the-land-degradation-surveillance-framework-ldsf/. LDSF sites were randomized using spatial stratification based on Koeppen-Geiger Climate zones across 19 countries in SSA. In total 60 LDSF sites were sampled. Soil samples were collected using the LDSF at two depths, 0-20 cm (labelled Topsoil) and 20-50 cm (labelled Subsoil). In each LDSF site, approximately 320 standard soil samples were collected. All of these were also scanned using MIR Spectroscopy and are available on Dataverse here: Vågen, Tor-Gunnar;Winowiecki, Leigh Ann;Desta, Luseged;Tondoh, Ebagnerin Jérôme;Weullow, Elvis;Shepherd, Keith;Sila, Andrew, 2020, 'Mid-Infrared Spectra (MIRS) from ICRAF Soil and Plant Spectroscopy Laboratory: Africa Soil Information Service (AfSIS) Phase I 2009-2013', https://doi.org/10.34725/DVN/QXCWP1, World Agroforestry - Research Data Repository, V1."


	uri <- "doi:10.34725/DVN/66BFOB"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIAT",
		publication= "doi:10.5194/soil-2020-69",
		project="AfSIS",
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_date="2024-06-10"
	)
  
	f1 <- ff[basename(ff) == "African archived samples September 2018.csv"]
	r <- read.csv(f1)
	#f2 <- ff[basename(ff) == "Variables_Description.xlsx"]
	#r2 <- carobiner::read.excel(f2, TRUE)
#	r2[1:15,1:4]

	
	d <- data.frame(
		country = r$Country,
		location = r$Site,
		longitude = r$Longitude,
		latitude = r$Latitude,
		geo_from_source= TRUE,
		#trial_id = r$SSN,
		soil_pH = as.numeric(r$pH),
		soil_ECEC = as.numeric(r$eCEC),
		soil_Ca = as.numeric(r$Caex),
		soil_K = as.numeric(r$Kex),
		soil_Mg = as.numeric(r$Mgex),
		soil_Na = as.numeric(r$Naex),
		soil_P_total = as.numeric(r$P),
		soil_P = as.numeric(r$Olsen.P),
		soil_C = r$Percent_C,
		soil_SOC = r$Percent_Org_C,
		soil_N = r$Percent_N,
		# Needs to be added into the termsinAg
		soil_extr_Al = r$Am.Ox.Al,
		soil_extr_Fe = r$Am.Ox.Fe,
		soil_extr_Mn = r$AmOx.Mn,
		soil_extr_P = r$AmOx.P,
		soil_PBI = r$pbi,
		soil_Al_total = as.numeric(r$Al),
		soil_Ca_total = as.numeric(r$Ca),
		soil_Co_total = as.numeric(r$Co),
		soil_Cu_total = as.numeric(r$Cu),
		soil_Fe_total = as.numeric(r$Fe),
		soil_K_total = as.numeric(r$K),
		soil_Mg_total = as.numeric(r$Mg),
		soil_Mn_total = as.numeric(r$Mn),
		soil_Na_total = as.numeric(r$Na),
		soil_Ni_total = as.numeric(r$Ni),
		soil_Pb_total = as.numeric(r$Pb),
		soil_S_total = as.numeric(r$S),
		soil_Zn_total = as.numeric(r$Zn),
		soil_As = as.numeric(r$As.75),
		soil_Se = as.numeric(r$Se.78),
		soil_Mo = as.numeric(r$Mo.95),
		soil_Cd = as.numeric(r$Cd.114)
	)

	i <- r$Depth=="Subsoil"
	d$depth_top <- ifelse(i, 20, 0)
	d$depth_bottom  <- ifelse(i, 50, 20)


	d$country[d$country == "SAfrica"] <- "South Africa"
	d$country[d$country == "Zimbambwe"] <- "Zimbabwe"

	# Replace -ve reported values with a small value equivalent to lowest detection limit
## or are they wrong if < 0?
	d$soil_ECEC[d$soil_ECEC <= 0] <- 0.0001
	d$soil_Ca[d$soil_Ca <= 0] <- 0.0001
	d$soil_K[d$soil_K <= 0] <- 0.0001
	d$soil_Mg[d$soil_Mg <= 0] <- 0.0001
	d$soil_Na[d$soil_Na <= 0] <- 0.0001
	d$soil_P_total[d$soil_P_total <= 0] <- 0.0001
	d$soil_SOC[d$soil_SOC <= 0] <- 0.001

## better not georeference by looking up names as the other locations seem quite precise.

	d$longitude[d$location == "Shoshong"] <- NA

	
	carobiner::write_files(path, meta, d)
}


