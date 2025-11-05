# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Fertilizer Microdosing
Most farmers and extension officers in Tanzania use blanket fertilizer recommendations, which can be ineffective in sustaining crops productions. The most recent fertilizer recommendation report (Marandu et al., 2014) do not have guidelines for semiarid zones.  Besides, developing agronomic rates (Mkoma, 2015), our work in 2016 seasons addressed the aspects of use efficiency and costs of fertilizer by refining the recommended rates into micro-dose rate. Fertilizer micro dosing involves application of small doses of fertilizer 5-6 kg P/ha (2-4g/hill as NPK) at sowing or shortly after germination to improve uptake or use efficiency and crops yields. The technique also holds high potential to reduce inputs costs because the amount of fertilizer is reduced substantially compared to recommended rate. Fertilizer micro-dosing trials were established in Mlali, Molet and Njoro villages during the 2016 growing seasons using the randomized complete block design (RCBD) with the three replications. Treatments include N (0, 15, 30 and 60 kg/ha) and P (0, 7.5, 15, and 30kg P/ha) in a factorial combination and the 16 treatment combinations were allocated randomly to each block. The test crop was maize, variety Staha. Maize was planted at 90 cm x 60 cm in Mlali and Molet villages and at a spacing of 75 cm x 60 cm in Njoro village. The plot size in both sites was 5 m x 6 m. This study includes data generated from this study trial.  About the project  Project title: Intensification of Maize-Legume Based Systems in the Semi-Arid Areas of Tanzania to Increase Farm Productivity and Improve Farming Natural Resource Base   Project abstract  The aim of the Africa RISING project in Kongwa and Kiteto Districts, Tanzania is to provide a scientific basis for sustainably intensifying agricultural production in semi-arid areas of central Tanzania. The project activities are falls under 4 thematic areas that address three critical elements of sustainable intensification (SI), i.e. genetic, ecological and socio-economic intensification technologies. The scope of activities being implemented include: packaging of new legume and cereal varieties with over 120% yield advantage, packaging and validation of integrated productivity enhancing technologies for cereals, legumes, legume trees and soil health technologies, food safety primarily to reduce aflatoxin contamination and integration of livestock into the cropping systems. The innovation platform is used to set R4D priority in the action sites.  The project team is comprised of national partners (e.g. ARI-Hombolo, District Agricultural Officers, SUA and UDOM) and CG Partners (CIMMYT and ICRAF) under the leadership of ICRISAT.  Project website:    http://africa-rising.net/where-we-work/west-africa/  Project start date: 2012-05-01  Project end date : 2016-09-30
"

	uri <- "doi:10.7910/DVN/4DDPFG"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "ICRAF; SUA", #"World Agroforestry Center (ICRAF); Sokoine University of Agriculture",
		publication = NA,
		project = "AfricaRISING",
		carob_date = "2025-07-29",
		design = "RCBD",
		data_type ="experiment",
		treatment_vars = "N_fertilizer;P_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
   ff <- ff[grepl("csv", basename(ff))]
	
   
   proc <- function(f){
          
      r1 <- read.csv(f)
      names(r1) <- gsub("Plot.No", "Plot.no", names(r1))
     
        data.frame(
             plot_id = as.character(r1$Plot.no),
             location= r1$Community ,
             crop = tolower(r1$Crop),
             variety= "staha",
             rep= r1$Block,
             P_fertilizer= r1$P.Trt..kg.P.ha.,
             N_fertilizer= r1$N.Trt..kg.N.ha.,
             K_fertilizer=0,
             plot_area= 5*6/10000, ## ha
             yield= r1$Grain..t.ha.* 1000, ## kg/ha
             country= "Tanzania",
             planting_date= "2016",
             yield_part= "grain",
             on_farm= TRUE,
             is_survey= FALSE,
             geo_from_source= FALSE,
             irrigated= NA,
             trial_id= paste(r1$Plot.no, r1$Community, sep = "-")
          )
       }
	d <- lapply(ff, proc)
	d <- do.call(rbind, d)

	### Adding long and lat coordinate
	
	geo <- data.frame(
	   location= c("Moleti", "Mlali", "Njoro"),
	   longitude= c(36.8131, 36.7501, 36.666),
	   latitude= c(-6.1766, -6.2816, -5.498)
	) 

	d <- merge(d, geo, by="location", all.x = TRUE)	
	
	d$row_spacing <- ifelse(grepl("Moleti|Mlali", d$location), 90, 75)
	d$plant_spacing <- 60 

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}


