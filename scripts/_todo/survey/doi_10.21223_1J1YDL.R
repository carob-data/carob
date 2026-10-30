# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Dataset for Determination of smallholder dairy farmers feeding practices and their perceptions on the use of sweetpotato vines silage (SPVS) as a feed resource in their production cycle.

Smallholder dairy farmers constitute up to 85% of dairy farmers in Uganda (Atuhaire, 2014). However, this category of farmers is challenged and faced by unstable feed quality and quantity supply year-round (Kiggundu et al., 2014). This therefore has led to unstable milk production and productivity which has dare effects on the milk prices and incomes of this category of dairy farmers. Studies have indicated that crop residues have a high potential to bridge the feed gap and provide a cheaper option to maintain a stable feed supply when well preserved (Dione et al., 2015). A study funded by International Potato Center (CIP) was conducted in three districts of Wakaiso, Masaka, and Kamuli to determine prevalent smallholder dairy farmers feeding practices and their perceptions on the use of sweetpotato vines silage (SPVS) as a feed resource in their production cycle. The local government structures were used to select potential farmer respondents. A list of smallholder dairy farmers in a district was obtained from the District Veterinary Officer (DVO), five sub-counties in Wakiso and two each from Masaka and Kamuli were selected based on the population density of the smallholder dairy farmers in these districts.  Systematically using an interval of five, farmers were selected and a structured questionnaire designed in the CSEntry data tool was administered to the selected farmers. A total of 147 farmers with the help of a local language translator were interviewed, on-farm observations were made, data were screened for any misrepresentations and follow-up questions were posed and saved. Preliminary findings during the study indicated that zero-grazing (58.59%) is most commonly practiced. Farmers reported contamination of sweetpotato vines with soil that hinders the use of the feed resource for feeding livestock. Significant amounts of vines are lost by some farmers after being brought to the farm for the luck of chopping and other forms of feed manipulations to facilitate intake. The majority of women (76.2%) and men (80.9%) agree to the statement 'I have a say over how household income from dairy products is spent' Data analysis is underway and a complete report will be produced documenting findings from the study.  Financial support from (CIP) is highly appreciated.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/1J1YDL"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "Makerere University, College of Agricultural and Environmental Sciences - MUK - CAES; CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Livestock farmers; universe: District Wakiso, Masaka and Kamuli; timeMethod: cross sectional; samplingProcedure: The local government structures were used to select potential farmer respondents. A list of smallholder dairy farmers in a district was obtained from the District Veterinary Officer (DVO), five sub-counties in Wakiso and two each from Masaka and Kamuli were selected based on the population density of the smallholder dairy farmers in these districts. 
Systematically using an interval of five, farmers were selected and a structured questionnaire designed in the CSEntry data tool was administered to the selected farmers.",
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "02_dictionary.xlsx"]
	f2 <- ff[basename(ff) == "01_Sweetpotato_Vine_Silage_Dataset.csv"]

	r1 <- carobiner::read.excel(f1)
	r2 <- read.csv(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "abbreviature", "variable_name", "description"


	d1 <- data.frame(
		country = r1[["country_uganda"]],
		adm1 = carobiner::fix_name(r1[["region"]], "title"),
		adm2 = carobiner::fix_name(r1[["district"]], "title"),
		location = r1[["village"]],
		season = r1[["e4_crp_season"]],
		rep = r1[["e14c_land_prep_price"]]
	)
##r1: "qnid", "interviewdate", "starttime", "subcounty", "parish", "villageo", "code_name", "resp_sex", "consent", "resp_age", "resp_educ", "resp_relationship", "resp_marital", "a1_farm_exp", "a2_hhh_sex", "a3_household_size", "a4_inceome_src", "b1_land_owned_qty", "b1_land_owned_unit", "b2_land_tenure", "b3a_land_pasture_qty", "b3b_land_pasture_unit", "b4a_land_fodder_qty", "b4b_land_foddeer_unit", "b5a_land_food_qty", "b5b_land_food_unit", "b6a_land_cash_qty", "b6b_land_cash_unit", "b7a_land_fallow_qty", "b7b_land_fallow_unit", "c1a_lact_local_qty", "c1b_lact_local_wet_qty", "c1c_lact_local_dry_qty", "c2a_lact_cross_qty", "c2b_lact_cross_wet_qty", "c2c_lact_cross_dry_qty", "c3a_lact_exotic_qty", "c3b_lact_exotic_wet_qty", "c3c_lact_exotic_dry_qty", "c4a_milk_price_wet", "c4b_milk_price_dry", "c5_dry_cow_local_qty", "c6_dry_cow_cross_qty", "c7_dry_cow_exotic_qty", "c8_heifer_local_qty", "c9_heifer_cross_qty", "c10_heifer_exotic_qty", "c11_calves_local_qty", "c12_calves_cross_qty", "c13_calves_exotic_qty", "c14_bull_local_qty", "c15_bull_cross_qty", "c16_bull_exotic_qty", "c17_steer_local_qty", "c18_steer_cross_qty", "c19_steer_exotic_qty", "c20_income_prop_dairy", "d1_feeding_system01", "d1_feeding_system02", "d1_feeding_system03", "d1_feeding_systemo", "d2_feed_type01", "d2_feed_type02", "d2_feed_type03", "d2_feed_type04", "d2_feed_type05", "d2_feed_typeo", "d3a_use_pgrass_yesno", "d3b_pgrass_type01", "d3b_pgrass_type02", "d3b_pgrass_type03", "d3b_pgrass_typeo", "d3c_src_napier01", "d3c_src_napier02", "d3c_src_napier03", "d3c_src_napier04", "d3c_src_napier05", "d3b_src_brachiaria01", "d3b_src_brachiaria02", "d3e_src_setaria01", "d3e_src_setaria02", "d3f_src_chloris01", "d3f_src_chloris02", "d3g_src_other_pasture01", "d4a_use_plegume_yesno", "d4b_plegume_type01", "d4b_plegume_type02", "d4b_plegume_typeo", "d4c_src_lablab01", "d4c_src_lablab02", "d4d_src_mucuna01", "d4d_src_mucuna02", "d4e_src_desmodium01", "d4e_src_desmodium02", "d4f_src_alfalfa01", "d4g_src_other_legume01", "d5a_use_fodde_yesno", "d5b_fodder_type01", "d5b_fodder_type02", "d5c_src_calliandria01", "d5e_src_gliricidia01", "d5f_src_other_foddero", "d6a_use_residue_yesno", "d6b_residue_type01", "d6b_residue_type02", "d6b_residue_type03", "d6b_residue_typeo", "d6c_src_banana01", "d6c_src_banana02", "d6c_src_banana03", "d6d_src_stover01", "d6d_src_stover02", "d6d_src_stover03", "d6e_src_sp_vines01", "d6e_src_sp_vines02", "d6e_src_sp_vines03", "d6e_src_sp_vineso", "d6f_src_other_residue01", "d6f_src_other_residue02", "d7a_use_concentrate_yesno", "d7b_src_concentrate01", "d8a_use_agro_byprod_yesno", "d8b_agro_byprod_type01", "d8b_agro_byprod_type02", "d8b_agro_byprod_type03", "d8c_src_molasses01", "d8d_src_breweries01", "e1_crp_grown01", "e1_crp_grown02", "e1_crp_grown03", "e1_crp_grown04", "e1_crp_grown05", "e1_crp_grown06", "e1_crp_grown07", "e1_crp_grown08", "e1_crp_grown09", "e1_crp_grown10", "e1_crp_growno", "e2_grow_sp_yesno", "e2_sp_land_qty", "sp_land_unit", "e3_sp_var01", "e3_sp_var02", "e3_sp_var03", "e3_sp_var04", "e3_sp_varo", "e5_crp_hvst_mnth01", "e5_crp_hvst_mnth02", "e5_crp_hvst_mnth03", "e5_crp_hvst_mnth04", "e5_crp_hvst_mnth05", "e6_use_sp_residue01", "e6_use_sp_residue02", "e6_use_sp_residue03", "e6_use_sp_residue04", "e7_sp_vine_constitute", "e8_sp_vine_form", "e9a_rsn_freshvine01", "e9a_rsn_freshvine02", "e9a_rsn_freshvine03", "e9a_rsn_freshvineo", "e9a_rnk1_rsn_freshvine", "e9a_rnk1_rsn_freshvineo", "e9a_rnk2_rsn_freshvine", "e9a_rnk2_rsn_freshvineo", "e9a_rnk3_rsn_freshvine", "e9a_rnk3_rsn_freshvineo", "e9b_rsn_not_silage01", "e9b_rsn_not_silage02", "e9b_rsn_not_silage03", "e9b_rsn_not_silageo", "e9b_rnk1_not_silage", "e9b_rnk1_not_silageo", "e9b_rnk2_not_silage", "e9b_rnk2_not_silageo", "e9b_rnk3_not_silage", "e9b_rnk3_not_silageo", "e10a_vine_chlnge_yesno", "e10b_vine_chlnge01", "e10b_vine_chlnge02", "e10b_vine_chlnge03", "e10b_vine_chlnge04", "e10b_vine_chlngeo", "e11_rsn_using_silage01", "e11_rsn_using_silage02", "e11_rsn_using_silage03", "e11_rsn_using_silageo", "e11_rnk1_using_silage", "e11_rnk1_using_silageo", "e11_rnk2_using_silage", "e11_rnk2_using_silageo", "e11_rnk3_using_silage", "e11_rnk3_using_silageo", "e12a_silage_chlnge_yesno", "e12b_silage_chlnge01", "e12b_silage_chlnge02", "e12b_silage_chlnge03", "e13_prod_silage_sp_yesno", "e14a_silage_land_qty", "e14a_silage_land_unit", "e14b_land_hiring_price", "e14d_material_price", "e14e_labour_price", "e14f_fertlizer_price", "e14g_weeding_price", "e14h_times_weeded_qty", "e14i_hrvst_price", "e14j_transport_price", "e14k_chop_price", "e14l_silage_making_price", "e14m_bran_qty", "e14n_silage_prod_qty", "e15a_vine_purchased", "e15a_vine_pchase_unit", "e15b_vine_unit_price", "e16_silage_qty_feed_cow", "e17_rsn_not_residue01", "e17_rsn_not_residue02", "e17_rsn_not_residue03", "e17_rsn_not_residueo", "f1_src_silage_info01", "f1_src_silage_info02", "f1_src_silage_info03", "f1_src_silage_info04", "f1_src_silage_info05", "f1_src_silage_infoo", "f2a_belong_group_yesno", "f2b_group_name", "f2c_grp_benefits01", "f2c_grp_benefits02", "f2c_grp_benefits03", "f2c_grp_benefits04", "f2c_grp_benefits05", "f2c_grp_benefitso", "f3_poultry_qty", "f4_pig_qty", "f5_goat_qty", "f6_sheep_qty", "f7_animal_qty", "f8_dcsn_making", "g11_income_spent", "g12_income_consult", "g13_income_have_say", "g14_income_women_have_say", "g15_income_making_say", "g16_income_use_say", "g21_tech_consult", "g22_tech_adoption_say", "g23_tech_animal_rear_say", "g24_tech_animal_adoption_say", "g25_tech_new_feeding", "g31_sp_feeding_consult", "g32_sp_utilization", "g33_sp_spouse_make_dcsn", "g34_sp_use_say", "picture1", "picture1_caption", "picture2", "picture2_caption", "picture3", "picture3_caption", "enum_remarks", "gps_lat", "gps_long", "gps_alt", "endtime", "duration"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

