# R script for "carob"


carob_script <- function(path){
  
"Title: Impact of NPK fertilization on upland rice yield, Nicaragua
  
Description: This dataset contains information of experiments carried out upland rice in two regions of Nicaragua (Caribbean and Pacific Region), as well as a compilation of soils data from different regions in Nicaragua collected during 2019 in seed banks of rice and beans. The experiments were designed to explore the effects of N, P and K in the yield of upland rice. The experiments were carried out on farmer’s field during the 2019 production cycle, the dataset contains yield and aerial biomass of the experiments."
 
	uri <- "doi:10.7910/DVN/H0HUSY"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
 
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		publication= NA,
		data_organization = "CIAT;ICDF",
		carob_contributor="Jean-Martial Johnson",
		carob_date="2022-12-09",
		data_type="experiment",
		project=NA,
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer"		
    )
  
	

	dfun <- function(r) {
		data.frame(
			adm1 = r$Departamento,
			adm2 = r$Municipio,
			adm3 = r$Comunidad,
	#		location = r1$Localidad,
			treatment = as.character(r$ttos),
			N_fertilizer = r$N,
			P_fertilizer = r$P,
			K_fertilizer = r$K,
			yield = as.numeric(r$rto_grano_kgha),
			dmy_total= as.numeric(r$rto_biom_kgha),
			trial_id = as.character(r$Localidad),
			rep = as.integer(r$rep)
		)
	}

	# processing Rice Data - Caribbean.tab
	f1 <- ff[basename(ff) == "03. Rice Data - Caribbean.xlsx"]
	r1 <- carobiner::read.excel(f1)
	d1 <- dfun(r1)
	d1$adm1 <- "Región Autónoma de la Costa Caribe Sur"
	
	# processing 04. Rice Data - Pacific.tab
	f2 <- ff[basename(ff) == "04. Rice Data - Pacific.xlsx"]
	r2 <- carobiner::read.excel(f2)
	d2 <- dfun(r2)
	
	# processing 02. Soils Data.xlsx
	f3 <- ff[basename(ff) == "02. Soils Data.xlsx"]
	r3 <- data.frame(readxl::read_xlsx(f3))
	d3 <- data.frame(
		adm1 = r3$Departamento,
		adm2 = r3$Municipio,
		adm3 = r3$Comunidad,
		soil_pH = as.numeric(r3$pH),
		soil_SOC = as.numeric(r3$MO)/1.724, # This is a coefficient to convert SOC into OM
		soil_sand = as.numeric(r3$Arena),
		soil_clay = as.numeric(r3$Arcilla),
		soil_N = as.numeric(r3$N),
		soil_P_available = as.numeric(r3$P),
		soil_K = as.numeric(r3$K) * 39 * 10
	)
	d3$adm2[grep("Kuka hill",d1$Municipio)] <- 'Kukrahill'

	d <- carobiner::bindr(d1, d2)
	d <- merge(x=d, y=d3, by = c("adm1", "adm2", "adm3"), all.x = TRUE)

	geo <- data.frame(
		adm3 = c("La Rebusca", "El Ensayo", "Rio chiquito", "El Tololar #1", "El Recreo", "El Panchon", "La Tortuga", "Montivideo"), 
		longitude = c(-85.764, -87.17, -86.909579, -85.764, -84.312, -83.863, -84.469, -84.609), 
		latitude = c(13.08, 12.587, 12.318, 13.08, 12.17, 12.323, 11.999, 11.808),
		geo_from_source = FALSE
	)

	d <- merge(d, geo, by="adm3", all.x=TRUE)

	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$country <- "Nicaragua"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$planting_date <- "2019"
	d$irrigated <- NA
	
	carobiner::write_files(meta, d, path=path)
}

