# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline household survey (2008) of the "Dissemination of submergence-tolerant varieties" project (STRASA), exported from Access as one
# set of tables per country. Files without suffix, with "_1", "_2" and with prefix "1" are different countries (not duplicates):
#   Laos: "Tbl_Part 04 Rice Varieties Grow.xlsx" + "Tbl_Part 0 Intro.xlsx"
#   Central Vietnam: "..._1.xlsx"; South Vietnam: "..._2.xlsx"
#   Cambodia: "Tbl_Part 04 Rice Varieties Grow.xls" + "Tbl_Part 0 Intro.xls"
#   Thailand: "Tbl_Part 04 Rice Varieties Grown_1.xls" + "Tbl_Part 0 Intro_2.xls"
#   Indonesia: "1Tbl_Part 04 Rice Varieties Grow.xls" (numerically coded)
# Mapped: Part 04 "Rice varieties grown" = household x flood/normal year x season x variety, with land type, crop establishment method,
# area, production, planting and harvest month. One record per row.
# yield = Prod / Area (the files' own Yield column equals Prod / Area).
# Reference year (planting_date) is the household's "NormalYear" or "FloodYear" from the Intro table.
# Provinces (adm1) from "Project details.xlsx" and the district names; Cambodia from the Province column.
# Vietnamese seasons: Winter-Spring -> dry, Summer-Autumn -> early wet, Autumn-Winter -> late wet.
# season_constraint is "flood" for the flood-year records.
# planting_method: TPR = transplanting; DSR and WSR = direct seeding (crop_establishment_ keeps dry/wet seeding).

## ISSUES
# Units of Area and Prod are not given in the variable list ("BaselineVarList.xls"); values imply ha and kg (yields 0-7.9 t/ha). Verify.
# Indonesia (676 households) is numerically coded (Landtype, Season, MVTV, CEM, PlMonth) and there is no code list; only district,
#   variety, flood/normal, area and yield are mapped; its codes are kept as "code n" in land_type_ and crop_establishment_ and in
#   season_code_, so a human can decode them. Indonesia Intro has duplicated FarmerIDs and no Normal/Flood years.
# Thailand province is unknown (districts "Sahatart" and "Sampanta"; the project lists Prachinburi and Ubon Ratchathani).
# Many Cambodian households have implausible or missing NormalYear/FloodYear (1996 - 2008); used as given.
# planting_date is a year only. Dry season / Winter-Spring crops may have been planted in the previous calendar year (planting_month_ Dec).
# The Philippines (Nueva Ecija and North Cotabato) data are in a different, questionnaire-page layout
#   ("... raw data_Sub1.xls" with "Documentation_Sub1 ...xls") and are not processed.
# Not processed: Part 20 (largest parcel: soil type, tenure, price), 20A (material inputs incl. fertilizer quantities for the largest
#   parcel; product names in free text), 20B/20C (power, labor), Part 03 (other crops), 05 (crop disposal), 06 (coping practices),
#   income, assets, consumption, relief, information modules. Fertilizer N/P/K are therefore not mapped.
# Several varieties are recorded together ("RD10 and TDK1"); kept as given.
# No coordinates (GeogDegree/GeogMins are empty).

## NEW VARIABLES
# land_type_: land type as recorded (lowland, upland, medium, intermediate, highland)
# crop_establishment_: TPR (transplanted), DSR (dry direct-seeded), WSR (wet direct-seeded)
# planting_month_, harvest_month_: month of planting and harvest (with the week in planting_week_, harvest_week_)
# variety_choice_reason_: reason for growing the variety
# season_code_: undocumented numeric season code (Indonesia only)



## NOT PROCESSED
# 1Tbl_Part 0 Intro.xls: not read. sheet Tbl_Part 0 Intro: FarmerID, Country, Respondent, InterviewDate, Village, Ethnic, District, GeogDegree, GeogMins, PVS, NormalYear, FloodYear, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 01 HH_Info.xls: not read. sheet Tbl_Part 01 HH_Info: FarmerID, District, Village, Relation, Respondent, Age, Sex, EducLevel, EducYear, PrimOccu, SecOccu, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 02A2B Landhold_Submerg.xls: not read. sheet Tbl_Part 02A2B Landhold_Submerg: FarmerID, District, Village, Landtype, Parcel, Plot, Area, SoilType, Tenure, Irrigation, AreaWS, AreaDS, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 03 Land Use.xls: not read. sheet Tbl_Part 03 Land Use: FarmerID, District, Village, FloodNormal, Landtype, Season, Crop, Area, MProd, Yield, MValue, MPrice, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 05 Crop Disposal.xls: not read. sheet Tbl_Part 05 Crop Disposal: FarmerID, District, Village, FloodNormal, Crop, TotalProd, QtySold, SaleValue, Price, QtySeeds, QtyFeeds, QtyPayment, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 06 Mgt_Practice.xls: not read. sheet Tbl_Part 06 Mgt_Practice: FarmerID, District, Village, UseCEMFlood, UseCEMFloodDateFr, UseCEMFloodDateTo, LaborUseFrom, LaborUseTo, UseCEMFloodYes, AdjAreaPlantedF, AdjAreaPlantedFChg, AdjAreaPlantedYes, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 07A Farm Income.xls: not read. sheet Tbl_Part 07A Farm Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueKind, ValueCash, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 07B Non-Farm Income.xls: not read. sheet Tbl_Part 07B Non-Farm Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueCash, DailyWage, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 07B NonFarm_wage.xls: not read. sheet Tbl_Part 07B NonFarm_wage: FarmerID, District, Village, WageRate, ChangeWage, WageMuch. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 07C Animal Prod_Sales.xls: not read. sheet Tbl_Part 07C Animal Prod_Sales: FarmerID, District, Village, FloodNormal, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 08A Animal Sale_Mortga.xls: not read. sheet Tbl_Part 08A Animal Sale_Mortga: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 08B Sell Farm_Implemen.xls: not read. sheet Tbl_Part 08B Sell Farm_Implemen: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 08C Sell Other_Asset.xls: not read. sheet Tbl_Part 08C Sell Other_Asset: FarmerID, District, Village, FloodNormal, ItemType, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 08D Mortgaging of_Asse.xls: not read. sheet Tbl_Part 08D Mortgaging of_Asse: FarmerID, District, Village, FloodNormal, Item, Value, Source, IntRate, Purpose, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 09 Borrowing.xls: not read. sheet Tbl_Part 09 Borrowing: FarmerID, District, Village, FloodNormal, ItemType, Item, Qty, Value, Price, Source, IntRate, Purpose, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 10 Income from_other.xls: not read. sheet Tbl_Part 10 Income from_other: FarmerID, District, Village, Item, Normal, Flood, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 12 Relief_Programs.xls: not read. sheet Tbl_Part 12 Relief_Programs: FarmerID, District, Village, Govt cash donation, Govt free food, Govt input subsidy, Govt free seeds, Govt free fodder, Govt free fingerlings, Other Gov't Relief, Amount of Gov't Cash Relief, NGO cash donation, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 13 Asset_Acquisition.xls: not read. sheet Tbl_Part 13 Asset_Acquisition: FarmerID, District, Village, Item, MonthsAsset, AmountPaid, IncomeSource, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 14 Consumption_Pattern.xls: not read. sheet Tbl_Part 14 Consumption_Pattern: FarmerID, District, Village, Items, DONormal, DOFlood, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 15 Consumption_Behavio.xls: not read. sheet Tbl_Part 15 Consumption_Behavio: FarmerID, District, Village, EatNormal, EatFlood, EatLess, EatExtra, ExtraItems, DeferPayments, DepleteSavings, ConsumeSeed, SchoolDropout, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 16A Food_purchase.xls: not read. sheet Tbl_Part 16A Food_purchase: FarmerID, District, Village, FloodNormal, Item, Cost, Source, Amount, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 16B Food Price_Change.xls: not read. sheet Tbl_Part 16B Food Price_Change: FarmerID, District, Village, FoodPriceInc, HowMuch, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 17A Animal_Nutrition.xls: not read. sheet Tbl_Part 17A Animal_Nutrition: FarmerID, District, Village, NFeeding, FFeeding, AnimalLost, Fishpond, OtherResponse, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 17B Animal_Lost.xls: not read. sheet Tbl_Part 17B Animal_Lost: FarmerID, District, Village, Animals, AnimalQty, AnimalValue, Price, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 18 Community mgt_pract.xls: not read. sheet Tbl_Part 18 Community mgt_pract: FarmerID, District, Village, Efforts, EffortsYes, FarmersShare, GovtAsst, GovtAsstYes, Insurance, Participate, ParticipateYes, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 19A Access to_info.xls: not read. sheet Tbl_Part 19A Access to_info: FarmerID, District, Village, Rice, Soil, Animal, OtherCrops, NewRice, RicePest, Aqua, Weather, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 19B Other info_source.xls: not read. sheet Tbl_Part 19B Other info_source: FarmerID, District, Village, OtherInfo, OtherInfoSource. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 20 Largest_Parcel.xls: not read. sheet Tbl_Part 20 Largest_Parcel: FarmerID, District, Village, Landtype, Season, Cem, Variety, VarietyType, Parcel, Plot, SoilType, Tenure, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 20A Input_use.xls: not read. sheet Tbl_Part 20A Input_use: FarmerID, District, Village, Season, CEM, VarietyType, Specify, Inputs, Unit, Area, PriceRent, PricePerUnit, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 20B Power_use.xls: not read. sheet Tbl_Part 20B Power_use: FarmerID, District, Village, Season, CEM, VarietyType, Inputs, Specify, Unit, Area, PriceRent, CPriceRent, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 20C Labor_inputhrs.xls: not read. sheet Tbl_Part 20C Labor_inputhrs: FarmerID, District, Village, HoursWorkperDay, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 20C Labor_use.xls: not read. sheet Tbl_Part 20C Labor_use: FarmerID, District, Village, Season, CEM, VarietyType, Inputs, Specify, Area, FamilyDays, HiredDays, TotalDays, .... Observation records are missing from d until this file is standardized.
# 1Tbl_Part 21A Division_of_labor.xls: not read. sheet Tbl_Part 21A Division_of_labor: FarmerID, District, Village, RiceFarmAct, HOwnedFarm, WOwnedFarm, HOthersFarm, WOthersFarm, Comments. Observation records are missing from d until this file is standardized.
# 1Tbl_Part 21B Other_Income.xls: not read. sheet Tbl_Part 21B Other_Income: FarmerID, District, Village, OtherIncomeGen, Husband, Wife, Comments. Observation records are missing from d until this file is standardized.
# Documentation_Sub1 north cotabato.xls: not read. sheet P1 (of 12): ID name, Description. Observation records are missing from d until this file is standardized.
# documentation_Sub1 Nueva Ecija.xls: not read. sheet p1 (of 12): id, ...2. Observation records are missing from d until this file is standardized.
# North cotabato raw data_Sub1.xls: not read. sheet Page1 (of 12): id, p1I1, p1I2, p1I3, p1I4, p1I5, p1I6, p1I7, p1I8, p1I9, p1Iqualifier1, p1Iqualifier2, .... Observation records are missing from d until this file is standardized.
# Nueva Ecija raw data_Sub1.xls: not read. sheet Page1 (of 12): id, p1q1, p1q2, norm_yr, fld_yr, p111_1, p111_2, p111_3, p111_4, p111_5, p111_6, p122_1...12, .... Observation records are missing from d until this file is standardized.
# Tbl_farmercode.xlsx: not read. sheet Tbl_farmercode: FarmerID, Country, Respondent, InterviewDate, Village, Ethnic, District, GeogDegree, GeogMins, PVS, NormalYear, FloodYear, .... Observation records are missing from d until this file is standardized.
# Tbl_farmercode_1.xlsx: not read. sheet Tbl_farmercode: FarmerID, Country, Respondent, InterviewDate, Village, Ethnic, District, GeogDegree, GeogMins, PVS, NormalYear, FloodYear, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 0 Intro_1.xls: same table as the file of this name that is read.
# Tbl_Part 01 HH_Info.xls: not read. sheet Tbl_Part 01 HH_Info: FarmerID, Country, Province, District, Relation, Respondent, Age, Sex, EducLevel, EducYear, PrimOccu, SecOccu, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 01 HH_Info.xlsx: not read. sheet Tbl_Part_01_HH_Info: FarmerID, District, Village, Relation, Respondent, Age, Sex, EducLevel, EducYear, PrimOccu, SecOccu, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 01 HH_Info_1.xls: not read. sheet Tbl_Part 01 HH_Info: FarmerID, District, Village, Relation, Respondent, Age, Sex, EducLevel, EducYear, PrimOccu, SecOccu, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 01 HH_Info_1.xlsx: not read. sheet Tbl_Part_01_HH_Info: FarmerID, District, Village, Relation, Respondent, Age, Sex, EducLevel, EducYear, PrimOccu, SecOccu, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 01 HH_Info_2.xls: not read. sheet Tbl_Part 01 HH_Info: FarmerID, District, Village, Relation, Respondent, Age, Sex, EducLevel, EducYear, PrimOccu, SecOccu, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 01 HH_Info_2.xlsx: not read. sheet Tbl_Part_01_HH_Info: FarmerID, District, Village, Relation, Respondent, Age, Sex, EducLevel, EducYear, PrimOccu, SecOccu, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 02A2B Landhold_Submerg.xls: not read. sheet Tbl_Part 02A2B Landhold_Submerg: FarmerID, Country, Province, District, Landtype, Parcel, Plot, Area, SoilType, Tenure, Irrigation, AreaWS, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 02A2B Landhold_Submerg.xlsx: not read. sheet Tbl_Part_02A2B_Landhold_Submerg: FarmerID, District, Village, Landtype, Parcel, Plot, Area, SoilType, Tenure, Irrigation, AreaWS, AreaDS, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 02A2B Landhold_Submerg_1.xlsx: not read. sheet Tbl_Part_02A2B_Landhold_Submerg: FarmerID, District, Village, Landtype, Parcel, Plot, Area, SoilType, Tenure, Irrigation, AreaWS, AreaDS, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 02A2B Landhold_Submerg_2.xlsx: not read. sheet Tbl_Part_02A2B_Landhold_Submerg: FarmerID, District, Village, Landtype, Parcel, Plot, Area, SoilType, Tenure, Irrigation, AreaWS, AreaDS, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 02A2B Landhold_Submerge.xls: not read. sheet Tbl_Part 02A2B Landhold_Submerg: FarmerID, District, Village, Landtype, Parcel, Plot, Area, SoilType, Tenure, Irrigation, AreaWS, AreaDS, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 02A2B Landhold_Submerge_1.xls: not read. sheet Tbl_Part 02A2B Landhold_Submerg: FarmerID, District, Village, Landtype, Parcel, Plot, Area, SoilType, Tenure, Irrigation, AreaWS, AreaDS, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 03 Land Use.xls: not read. sheet Tbl_Part 03 Land Use: FarmerID, Country, Province, District, FloodNormal, Landtype, Season, Crop, Area, MProd, Yield, MValue, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 03 Land Use.xlsx: not read. sheet Tbl_Part_03_Land_Use: FarmerID, District, Village, FloodNormal, Landtype, Season, Crop, Area, MProd, Yield, MValue, MPrice, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 03 Land Use_1.xls: not read. sheet Tbl_Part 03 Land Use: FarmerID, District, Village, FloodNormal, Landtype, Season, Crop, Area, MProd, MValue, ByProd, ByValue, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 03 Land Use_1.xlsx: not read. sheet Tbl_Part_03_Land_Use: FarmerID, District, Village, FloodNormal, Landtype, Season, Crop, Area, MProd, Yield, MValue, MPrice, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 03 Land Use_2.xls: not read. sheet Tbl_Part 03 Land Use: FarmerID, District, Village, FloodNormal, Landtype, Season, Crop, Area, MProd, Yield, MValue, MPrice, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 03 Land Use_2.xlsx: not read. sheet Tbl_Part_03_Land_Use: FarmerID, District, Village, FloodNormal, Landtype, Season, Crop, Area, MProd, Yield, MValue, MPrice, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 05 Crop Disposal.xls: not read. sheet Tbl_Part 05 Crop Disposal: FarmerID, Country, Province, District, FloodNormal, Crop, TotalProd, QtySold, SaleValue, Price, QtySeeds, QtyFeeds, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 05 Crop Disposal.xlsx: not read. sheet Tbl_Part_05_Crop_Disposal: FarmerID, District, Village, FloodNormal, Crop, TotalProd, QtySold, SaleValue, Price, QtySeeds, QtyFeeds, QtyPayment, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 05 Crop Disposal_1.xls: not read. sheet Tbl_Part 05 Crop Disposal: FarmerID, District, Village, FloodNormal, Crop, TotalProd, QtySold, SaleValue, QtySeeds, QtyFeeds, QtyPayment, QtyOther, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 05 Crop Disposal_1.xlsx: not read. sheet Tbl_Part_05_Crop_Disposal: FarmerID, District, Village, FloodNormal, Crop, TotalProd, QtySold, SaleValue, Price, QtySeeds, QtyFeeds, QtyPayment, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 05 Crop Disposal_2.xls: not read. sheet Tbl_Part 05 Crop Disposal: FarmerID, District, Village, FloodNormal, Crop, TotalProd, QtySold, SaleValue, Price, QtySeeds, QtyFeeds, QtyPayment, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 05 Crop Disposal_2.xlsx: not read. sheet Tbl_Part_05_Crop_Disposal: FarmerID, District, Village, FloodNormal, Crop, TotalProd, QtySold, SaleValue, Price, QtySeeds, QtyFeeds, QtyPayment, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 06 Mgt_Practice.xls: not read. sheet Tbl_Part 06 Mgt_Practice: FarmerID, Country, Province, District, UseCEMFlood, UseCEMFloodDateFr, UseCEMFloodDateTo, LaborUseFrom, LaborUseTo, UseCEMFloodYes, AdjAreaPlantedF, AdjAreaPlantedFChg, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 06 Mgt_Practice.xlsx: not read. sheet Tbl_Part_06_Mgt_Practice: FarmerID, District, Village, UseCEMFlood, UseCEMFloodDateFr, UseCEMFloodDateTo, LaborUseFrom, LaborUseTo, UseCEMFloodYes, AdjAreaPlantedF, AdjAreaPlantedFChg, AdjAreaPlantedYes, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 06 Mgt_Practice_1.xls: not read. sheet Tbl_Part 06 Mgt_Practice: FarmerID, District, Village, UseCEMFlood, UseCEMFloodDateFr, UseCEMFloodDateTo, LaborUseFrom, LaborUseTo, UseCEMFloodYes, AdjAreaPlantedF, AdjAreaPlantedFChg, AdjAreaPlantedYes, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 06 Mgt_Practice_1.xlsx: not read. sheet Tbl_Part_06_Mgt_Practice: FarmerID, District, Village, UseCEMFlood, UseCEMFloodDateFr, UseCEMFloodDateTo, LaborUseFrom, LaborUseTo, UseCEMFloodYes, AdjAreaPlantedF, AdjAreaPlantedFChg, AdjAreaPlantedYes, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 06 Mgt_Practice_2.xls: not read. sheet Tbl_Part 06 Mgt_Practice: FarmerID, District, Village, UseCEMFlood, UseCEMFloodDateFr, UseCEMFloodDateTo, LaborUseFrom, LaborUseTo, UseCEMFloodYes, AdjAreaPlantedF, AdjAreaPlantedFChg, AdjAreaPlantedYes, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 06 Mgt_Practice_2.xlsx: not read. sheet Tbl_Part_06_Mgt_Practice: FarmerID, District, Village, UseCEMFlood, UseCEMFloodDateFr, UseCEMFloodDateTo, LaborUseFrom, LaborUseTo, UseCEMFloodYes, AdjAreaPlantedF, AdjAreaPlantedFChg, AdjAreaPlantedYes, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07A Farm Income.xls: not read. sheet Tbl_Part 07A Farm Income: FarmerID, Country, Province, District, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueKind, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07A Farm Income.xlsx: not read. sheet Tbl_Part_07A_Farm_Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueKind, ValueCash, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07A Farm Income_1.xls: not read. sheet Tbl_Part 07A Farm Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueKind, ValueCash, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07A Farm Income_1.xlsx: not read. sheet Tbl_Part_07A_Farm_Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueKind, ValueCash, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07A Farm Income_2.xls: not read. sheet Tbl_Part 07A Farm Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueKind, ValueCash, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07A Farm Income_2.xlsx: not read. sheet Tbl_Part_07A_Farm_Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueKind, ValueCash, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07B Non-Farm Income.xls: not read. sheet Tbl_Part 07B Non-Farm Income: FarmerID, Country, Province, District, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueCash, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07B Non-Farm Income.xlsx: not read. sheet Tbl_Part_07B_Non_Farm_Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueCash, DailyWage, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07B Non-Farm Income_1.xls: not read. sheet Tbl_Part 07B Non-Farm Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueCash, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 07B Non-Farm Income_1.xlsx: not read. sheet Tbl_Part_07B_Non_Farm_Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueCash, DailyWage, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07B Non-Farm Income_2.xls: not read. sheet Tbl_Part 07B Non-Farm Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueCash, DailyWage, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07B Non-Farm Income_2.xlsx: not read. sheet Tbl_Part_07B_Non_Farm_Income: FarmerID, District, Village, FloodNormal, IncomeType, Sex, Worktype, HHAct, Workplace, Dayswork, ValueCash, DailyWage, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 07B NonFarm_wage.xls: not read. sheet Tbl_Part 07B NonFarm_wage: FarmerID, Country, Province, District, WageRate, ChangeWage, WageMuch. Observation records are missing from d until this file is standardized.
# Tbl_Part 07B NonFarm_wage.xlsx: not read. sheet Tbl_Part_07B_NonFarm_wage: FarmerID, District, Village, WageRate, ChangeWage, WageMuch. Observation records are missing from d until this file is standardized.
# Tbl_Part 07B NonFarm_wage_1.xls: not read. sheet Tbl_Part 07B NonFarm_wage: FarmerID, District, Village, WageRate, ChangeWage, WageMuch. Observation records are missing from d until this file is standardized.
# Tbl_Part 07B NonFarm_wage_1.xlsx: not read. sheet Tbl_Part_07B_NonFarm_wage: FarmerID, District, Village, WageRate, ChangeWage, WageMuch. Observation records are missing from d until this file is standardized.
# Tbl_Part 07B NonFarm_wage_2.xls: not read. sheet Tbl_Part 07B NonFarm_wage: FarmerID, District, Village, WageRate, ChangeWage, WageMuch. Observation records are missing from d until this file is standardized.
# Tbl_Part 07B NonFarm_wage_2.xlsx: not read. sheet Tbl_Part_07B_NonFarm_wage: FarmerID, District, Village, WageRate, ChangeWage, WageMuch. Observation records are missing from d until this file is standardized.
# Tbl_Part 07C Animal Prod_Sales.xls: not read. sheet Tbl_Part 07C Animal Prod_Sales: FarmerID, District, Village, FloodNormal, Items, Qty, Unit, VaIue, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 07C Animal Prod_Sales.xlsx: not read. sheet Tbl_Part_07C_Animal_Prod_Sales: FarmerID, District, Village, FloodNormal, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 07C Animal Prod_Sales_1.xls: not read. sheet Tbl_Part 07C Animal Prod_Sales: FarmerID, District, Village, FloodNormal, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 07C Animal Prod_Sales_1.xlsx: not read. sheet Tbl_Part_07C_Animal_Prod_Sales: FarmerID, District, Village, FloodNormal, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 07C Animal Prod_Sales_2.xlsx: not read. sheet Tbl_Part_07C_Animal_Prod_Sales: FarmerID, District, Village, FloodNormal, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 07C8C Sold_asset.xls: not read. sheet Tbl_Part 07C8C Sold_asset: FarmerID, Country, Province, District, FloodNormal, ItemType, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08A Animal Sale_Mortga.xlsx: not read. sheet Tbl_Part_08A_Animal_Sale_Mortga: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08A Animal Sale_Mortga_1.xlsx: not read. sheet Tbl_Part_08A_Animal_Sale_Mortga: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08A Animal Sale_Mortga_2.xlsx: not read. sheet Tbl_Part_08A_Animal_Sale_Mortga: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08A Animal Sale_Mortgage.xls: not read. sheet Tbl_Part 08A Animal Sale_Mortga: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08A Animal Sale_Mortgage_1.xls: not read. sheet Tbl_Part 08A Animal Sale_Mortga: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08AB Sold_Mortgaged.xls: not read. sheet Tbl_Part 08AB Sold_Mortgaged: FarmerID, Country, Province, District, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 08B Sell Farm_Implemen.xlsx: not read. sheet Tbl_Part_08B_Sell_Farm_Implemen: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08B Sell Farm_Implemen_1.xlsx: not read. sheet Tbl_Part_08B_Sell_Farm_Implemen: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08B Sell Farm_Implemen_2.xlsx: not read. sheet Tbl_Part_08B_Sell_Farm_Implemen: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08B Sell Farm_Implements.xls: not read. sheet Tbl_Part 08B Sell Farm_Implemen: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08B Sell Farm_Implements_1.xls: not read. sheet Tbl_Part 08B Sell Farm_Implemen: FarmerID, District, Village, FloodNormal, ItemType, Stock, SoldMort, Qty, VaIue, Price, SoldWhen, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08C Sell Other_Asset.xls: not read. sheet Tbl_Part 08C Sell Other_Asset: FarmerID, District, Village, FloodNormal, ItemType, Items, Qty, Unit, VaIue, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08C Sell Other_Asset.xlsx: not read. sheet Tbl_Part_08C_Sell_Other_Asset: FarmerID, District, Village, FloodNormal, ItemType, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08C Sell Other_Asset_1.xls: not read. sheet Tbl_Part 08C Sell Other_Asset: FarmerID, District, Village, FloodNormal, ItemType, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08C Sell Other_Asset_1.xlsx: not read. sheet Tbl_Part_08C_Sell_Other_Asset: FarmerID, District, Village, FloodNormal, ItemType, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08C Sell Other_Asset_2.xlsx: not read. sheet Tbl_Part_08C_Sell_Other_Asset: FarmerID, District, Village, FloodNormal, ItemType, Items, Qty, Unit, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08D Mortgaging of_Asse.xls: not read. sheet Tbl_Part 08D Mortgaging of_Asse: FarmerID, Country, Province, District, FloodNormal, Item, Value, Source, IntRate, Purpose, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08D Mortgaging of_Asse.xlsx: not read. sheet Tbl_Part_08D_Mortgaging_of_Asse: FarmerID, District, Village, FloodNormal, Item, Value, Source, IntRate, Purpose, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08D Mortgaging of_Asse_1.xlsx: not read. sheet Tbl_Part_08D_Mortgaging_of_Asse: FarmerID, District, Village, FloodNormal, Item, Value, Source, IntRate, Purpose, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08D Mortgaging of_Asse_2.xlsx: not read. sheet Tbl_Part_08D_Mortgaging_of_Asse: FarmerID, District, Village, FloodNormal, Item, Value, Source, IntRate, Purpose, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08D Mortgaging of_Asset.xls: not read. sheet Tbl_Part 08D Mortgaging of_Asse: FarmerID, District, Village, FloodNormal, Item, Value, Source, IntRate, Purpose, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 08D Mortgaging of_Asset_1.xls: not read. sheet Tbl_Part 08D Mortgaging of_Asse: FarmerID, District, Village, FloodNormal, Item, Value, Source, IntRate, Purpose, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 09 Borrowing.xls: not read. sheet Tbl_Part 09 Borrowing: FarmerID, Country, Province, District, FloodNormal, ItemType, Item, Qty, Value, Price, Source, IntRate, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 09 Borrowing.xlsx: not read. sheet Tbl_Part_09_Borrowing: FarmerID, District, Village, FloodNormal, ItemType, Item, Qty, Value, Price, Source, IntRate, Purpose, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 09 Borrowing_1.xls: not read. sheet Tbl_Part 09 Borrowing: FarmerID, District, Village, FloodNormal, ItemType, Item, Qty, Value, Source, IntRate, Purpose, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 09 Borrowing_1.xlsx: not read. sheet Tbl_Part_09_Borrowing: FarmerID, District, Village, FloodNormal, ItemType, Item, Qty, Value, Price, Source, IntRate, Purpose, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 09 Borrowing_2.xls: not read. sheet Tbl_Part 09 Borrowing: FarmerID, District, Village, FloodNormal, ItemType, Item, Qty, Value, Price, Source, IntRate, Purpose, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 09 Borrowing_2.xlsx: not read. sheet Tbl_Part_09_Borrowing: FarmerID, District, Village, FloodNormal, ItemType, Item, Qty, Value, Price, Source, IntRate, Purpose, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 10 Income from_other.xls: not read. sheet Tbl_Part 10 Income from_other: FarmerID, Country, Province, District, Item, Normal, Flood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 10 Income from_other.xlsx: not read. sheet Tbl_Part_10_Income_from_other: FarmerID, District, Village, Item, Normal, Flood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 10 Income from_other_1.xls: not read. sheet Tbl_Part 10 Income from_other: FarmerID, District, Village, Item, Normal, Flood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 10 Income from_other_1.xlsx: not read. sheet Tbl_Part_10_Income_from_other: FarmerID, District, Village, Item, Normal, Flood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 10 Income from_other_2.xls: not read. sheet Tbl_Part 10 Income from_other: FarmerID, District, Village, Item, Normal, Flood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 10 Income from_other_2.xlsx: not read. sheet Tbl_Part_10_Income_from_other: FarmerID, District, Village, Item, Normal, Flood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 12 Relief_Programs.xls: not read. sheet Tbl_Part 12 Relief_Programs: FarmerID, Country, Province, District, Govt cash donation, Govt free food, Govt input subsidy, Govt free seeds, Govt free fodder, Govt free fingerlings, Other Gov't Relief, Amount of Gov't Cash Relief, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 12 Relief_Programs.xlsx: not read. sheet Tbl_Part_12_Relief_Programs: FarmerID, District, Village, Govt cash donation, Govt free food, Govt input subsidy, Govt free seeds, Govt free fodder, Govt free fingerlings, Other Gov't Relief, Amount of Gov't Cash Relief, NGO cash donation, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 12 Relief_Programs_1.xls: not read. sheet Tbl_Part 12 Relief_Programs: FarmerID, District, Village, Govt cash donation, Govt free food, Govt input subsidy, Govt free seeds, Govt free fodder, Govt free fingerlings, Other Gov't Relief, Amount of Gov't Cash Relief, NGO cash donation, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 12 Relief_Programs_1.xlsx: not read. sheet Tbl_Part_12_Relief_Programs: FarmerID, District, Village, Govt cash donation, Govt free food, Govt input subsidy, Govt free seeds, Govt free fodder, Govt free fingerlings, Other Gov't Relief, Amount of Gov't Cash Relief, NGO cash donation, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 12 Relief_Programs_2.xls: not read. sheet Tbl_Part 12 Relief_Programs: FarmerID, District, Village, Govt cash donation, Govt free food, Govt input subsidy, Govt free seeds, Govt free fodder, Govt free fingerlings, Other Gov't Relief, Amount of Gov't Cash Relief, NGO cash donation, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 12 Relief_Programs_2.xlsx: not read. sheet Tbl_Part_12_Relief_Programs: FarmerID, District, Village, Govt cash donation, Govt free food, Govt input subsidy, Govt free seeds, Govt free fodder, Govt free fingerlings, Other Gov't Relief, Amount of Gov't Cash Relief, NGO cash donation, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 13 Asset_Acquisition.xls: not read. sheet Tbl_Part 13 Asset_Acquisition: FarmerID, Country, Province, District, Item, MonthsAsset, AmountPaid, IncomeSource, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 13 Asset_Acquisition.xlsx: not read. sheet Tbl_Part_13_Asset_Acquisition: FarmerID, District, Village, Item, MonthsAsset, AmountPaid, IncomeSource, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 13 Asset_Acquisition_1.xls: not read. sheet Tbl_Part 13 Asset_Acquisition: FarmerID, District, Village, Item, MonthsAsset, AmountPaid, IncomeSource, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 13 Asset_Acquisition_1.xlsx: not read. sheet Tbl_Part_13_Asset_Acquisition: FarmerID, District, Village, Item, MonthsAsset, AmountPaid, IncomeSource, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 13 Asset_Acquisition_2.xls: not read. sheet Tbl_Part 13 Asset_Acquisition: FarmerID, District, Village, Item, MonthsAsset, AmountPaid, IncomeSource, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 13 Asset_Acquisition_2.xlsx: not read. sheet Tbl_Part_13_Asset_Acquisition: FarmerID, District, Village, Item, MonthsAsset, AmountPaid, IncomeSource, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 14 Consumption_Pattern.xls: not read. sheet Tbl_Part 14 Consumption_Pattern: FarmerID, Country, Province, District, Items, DONormal, DOFlood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 14 Consumption_Pattern.xlsx: not read. sheet Tbl_Part_14_Consumption_Pattern: FarmerID, District, Village, Items, DONormal, DOFlood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 14 Consumption_Pattern_1.xls: not read. sheet Tbl_Part 14 Consumption_Pattern: FarmerID, District, Village, Items, DONormal, DOFlood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 14 Consumption_Pattern_1.xlsx: not read. sheet Tbl_Part_14_Consumption_Pattern: FarmerID, District, Village, Items, DONormal, DOFlood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 14 Consumption_Pattern_2.xls: not read. sheet Tbl_Part 14 Consumption_Pattern: FarmerID, District, Village, Items, DONormal, DOFlood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 14 Consumption_Pattern_2.xlsx: not read. sheet Tbl_Part_14_Consumption_Pattern: FarmerID, District, Village, Items, DONormal, DOFlood, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 15 Consumption_Behavio.xls: not read. sheet Tbl_Part 15 Consumption_Behavio: FarmerID, Country, Province, District, EatNormal, EatFlood, EatLess, EatExtra, ExtraItems, DeferPayments, DepleteSavings, ConsumeSeed, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 15 Consumption_Behavio.xlsx: not read. sheet Tbl_Part_15_Consumption_Behavio: FarmerID, District, Village, EatNormal, EatFlood, EatLess, EatExtra, ExtraItems, DeferPayments, DepleteSavings, ConsumeSeed, SchoolDropout, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 15 Consumption_Behavio_1.xlsx: not read. sheet Tbl_Part_15_Consumption_Behavio: FarmerID, District, Village, EatNormal, EatFlood, EatLess, EatExtra, ExtraItems, DeferPayments, DepleteSavings, ConsumeSeed, SchoolDropout, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 15 Consumption_Behavio_2.xlsx: not read. sheet Tbl_Part_15_Consumption_Behavio: FarmerID, District, Village, EatNormal, EatFlood, EatLess, EatExtra, ExtraItems, DeferPayments, DepleteSavings, ConsumeSeed, SchoolDropout, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 15 Consumption_Behavior.xls: not read. sheet Tbl_Part 15 Consumption_Behavio: FarmerID, District, Village, EatNormal, EatFlood, EatLess, EatExtra, ExtraItems, DeferPayments, DepleteSavings, ConsumeSeed, SchoolDropout, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 15 Consumption_Behavior_1.xls: not read. sheet Tbl_Part 15 Consumption_Behavio: FarmerID, District, Village, EatNormal, EatFlood, EatLess, EatExtra, ExtraItems, DeferPayments, DepleteSavings, ConsumeSeed, SchoolDropout, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 16A Food_purchase.xls: not read. sheet Tbl_Part 16A Food_purchase: FarmerID, Country, Province, District, FloodNormal, Item, Cost, Source, Amount, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16A Food_purchase.xlsx: not read. sheet Tbl_Part_16A_Food_purchase: FarmerID, District, Village, FloodNormal, Item, Cost, Source, Amount, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16A Food_purchase_1.xls: not read. sheet Tbl_Part 16A Food_purchase: FarmerID, District, Village, FloodNormal, Item, Cost, Source, Amount, VaIue, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16A Food_purchase_1.xlsx: not read. sheet Tbl_Part_16A_Food_purchase: FarmerID, District, Village, FloodNormal, Item, Cost, Source, Amount, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16A Food_purchase_2.xls: not read. sheet Tbl_Part 16A Food_purchase: FarmerID, District, Village, FloodNormal, Item, Cost, Source, Amount, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16A Food_purchase_2.xlsx: not read. sheet Tbl_Part_16A_Food_purchase: FarmerID, District, Village, FloodNormal, Item, Cost, Source, Amount, VaIue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16B Food Price_Change.xls: not read. sheet Tbl_Part 16B Food Price_Change: FarmerID, Country, Province, District, FoodPriceInc, HowMuch, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16B Food Price_Change.xlsx: not read. sheet Tbl_Part_16B_Food_Price_Change: FarmerID, District, Village, FoodPriceInc, HowMuch, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16B Food Price_Change_1.xls: not read. sheet Tbl_Part 16B Food Price_Change: FarmerID, District, Village, FoodPriceInc, HowMuch, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16B Food Price_Change_1.xlsx: not read. sheet Tbl_Part_16B_Food_Price_Change: FarmerID, District, Village, FoodPriceInc, HowMuch, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16B Food Price_Change_2.xls: not read. sheet Tbl_Part 16B Food Price_Change: FarmerID, District, Village, FoodPriceInc, HowMuch, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 16B Food Price_Change_2.xlsx: not read. sheet Tbl_Part_16B_Food_Price_Change: FarmerID, District, Village, FoodPriceInc, HowMuch, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17A Animal_Nutrition.xls: not read. sheet Tbl_Part 17A Animal_Nutrition: FarmerID, Country, Province, District, NFeeding, FFeeding, AnimalLost, Fishpond, OtherResponse, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17A Animal_Nutrition.xlsx: not read. sheet Tbl_Part_17A_Animal_Nutrition: FarmerID, District, Village, NFeeding, FFeeding, AnimalLost, Fishpond, OtherResponse, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17A Animal_Nutrition_1.xls: not read. sheet Tbl_Part 17A Animal_Nutrition: FarmerID, District, Village, NFeeding, FFeeding, AnimalLost, Fishpond, OtherResponse, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17A Animal_Nutrition_1.xlsx: not read. sheet Tbl_Part_17A_Animal_Nutrition: FarmerID, District, Village, NFeeding, FFeeding, AnimalLost, Fishpond, OtherResponse, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17A Animal_Nutrition_2.xls: not read. sheet Tbl_Part 17A Animal_Nutrition: FarmerID, District, Village, NFeeding, FFeeding, AnimalLost, Fishpond, OtherResponse, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17A Animal_Nutrition_2.xlsx: not read. sheet Tbl_Part_17A_Animal_Nutrition: FarmerID, District, Village, NFeeding, FFeeding, AnimalLost, Fishpond, OtherResponse, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17B Animal_Lost.xls: not read. sheet Tbl_Part 17B Animal_Lost: FarmerID, Country, Province, District, Animals, AnimalQty, AnimalValue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17B Animal_Lost.xlsx: not read. sheet Tbl_Part_17B_Animal_Lost: FarmerID, District, Village, Animals, AnimalQty, AnimalValue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17B Animal_Lost_1.xls: not read. sheet Tbl_Part 17B Animal_Lost: FarmerID, District, Village, Animals, AnimalQty, AnimalValue, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17B Animal_Lost_1.xlsx: not read. sheet Tbl_Part_17B_Animal_Lost: FarmerID, District, Village, Animals, AnimalQty, AnimalValue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17B Animal_Lost_2.xls: not read. sheet Tbl_Part 17B Animal_Lost: FarmerID, District, Village, Animals, AnimalQty, AnimalValue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 17B Animal_Lost_2.xlsx: not read. sheet Tbl_Part_17B_Animal_Lost: FarmerID, District, Village, Animals, AnimalQty, AnimalValue, Price, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 18 Community mgt_pract.xls: not read. sheet Tbl_Part 18 Community mgt_pract: FarmerID, Country, Province, District, Efforts, EffortsYes, FarmersShare, GovtAsst, GovtAsstYes, Insurance, Participate, ParticipateYes, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 18 Community mgt_pract.xlsx: not read. sheet Tbl_Part_18_Community_mgt_pract: FarmerID, District, Village, Efforts, EffortsYes, FarmersShare, GovtAsst, GovtAsstYes, Insurance, Participate, ParticipateYes, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 18 Community mgt_pract_1.xlsx: not read. sheet Tbl_Part_18_Community_mgt_pract: FarmerID, District, Village, Efforts, EffortsYes, FarmersShare, GovtAsst, GovtAsstYes, Insurance, Participate, ParticipateYes, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 18 Community mgt_pract_2.xlsx: not read. sheet Tbl_Part_18_Community_mgt_pract: FarmerID, District, Village, Efforts, EffortsYes, FarmersShare, GovtAsst, GovtAsstYes, Insurance, Participate, ParticipateYes, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 18 Community mgt_practices.xls: not read. sheet Tbl_Part 18 Community mgt_pract: FarmerID, District, Village, Efforts, EffortsYes, FarmersShare, GovtAsst, GovtAsstYes, Insurance, Participate, ParticipateYes, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 18 Community mgt_practices_1.xls: not read. sheet Tbl_Part 18 Community mgt_pract: FarmerID, District, Village, Efforts, EffortsYes, FarmersShare, GovtAsst, GovtAsstYes, Insurance, Participate, ParticipateYes, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 19A Access to_info.xls: not read. sheet Tbl_Part 19A Access to_info: FarmerID, Country, Province, District, Rice, Soil, Animal, OtherCrops, NewRice, RicePest, Aqua, Weather, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 19A Access to_info.xlsx: not read. sheet Tbl_Part_19A_Access_to_info: FarmerID, District, Village, Rice, Soil, Animal, OtherCrops, NewRice, RicePest, Aqua, Weather, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 19A Access to_info_1.xls: not read. sheet Tbl_Part 19A Access to_info: FarmerID, District, Village, Rice, Soil, Animal, OtherCrops, NewRice, RicePest, Aqua, Weather, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 19A Access to_info_1.xlsx: not read. sheet Tbl_Part_19A_Access_to_info: FarmerID, District, Village, Rice, Soil, Animal, OtherCrops, NewRice, RicePest, Aqua, Weather, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 19A Access to_info_2.xls: not read. sheet Tbl_Part 19A Access to_info: FarmerID, District, Village, Rice, Soil, Animal, OtherCrops, NewRice, RicePest, Aqua, Weather, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 19A Access to_info_2.xlsx: not read. sheet Tbl_Part_19A_Access_to_info: FarmerID, District, Village, Rice, Soil, Animal, OtherCrops, NewRice, RicePest, Aqua, Weather, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 19B Other info_source.xls: not read. sheet Tbl_Part 19B Other info_source: FarmerID, Country, Province, District, OtherInfo, OtherInfoSource. Observation records are missing from d until this file is standardized.
# Tbl_Part 19B Other info_source.xlsx: not read. sheet Tbl_Part_19B_Other_info_source: FarmerID, District, Village, OtherInfo, OtherInfoSource. Observation records are missing from d until this file is standardized.
# Tbl_Part 19B Other info_source_1.xls: not read. sheet Tbl_Part 19B Other info_source: FarmerID, District, Village, OtherInfo, OtherInfoSource. Observation records are missing from d until this file is standardized.
# Tbl_Part 19B Other info_source_1.xlsx: not read. sheet Tbl_Part_19B_Other_info_source: FarmerID, District, Village, OtherInfo, OtherInfoSource. Observation records are missing from d until this file is standardized.
# Tbl_Part 19B Other info_source_2.xls: not read. sheet Tbl_Part 19B Other info_source: FarmerID, District, Village, OtherInfo, OtherInfoSource. Observation records are missing from d until this file is standardized.
# Tbl_Part 19B Other info_source_2.xlsx: not read. sheet Tbl_Part_19B_Other_info_source: FarmerID, District, Village, OtherInfo, OtherInfoSource. Observation records are missing from d until this file is standardized.
# Tbl_Part 20 Largest_Parcel.xls: not read. sheet Tbl_Part 20 Largest_Parcel: FarmerID, Country, Province, District, Landtype, Season, Cem, VarietyType, Variety, Parcel, Plot, SoilType, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20 Largest_Parcel.xlsx: not read. sheet Tbl_Part_20_Largest_Parcel: FarmerID, District, Village, Landtype, Season, Cem, Variety, VarietyType, Parcel, Plot, SoilType, Tenure, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20 Largest_Parcel_1.xls: not read. sheet Tbl_Part 20 Largest_Parcel: FarmerID, District, Village, Landtype, Season, Cem, Variety, VarietyType, Parcel, Plot, SoilType, Tenure, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20 Largest_Parcel_1.xlsx: not read. sheet Tbl_Part_20_Largest_Parcel: FarmerID, District, Village, Landtype, Season, Cem, Variety, VarietyType, Parcel, Plot, SoilType, Tenure, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20 Largest_Parcel_2.xls: not read. sheet Tbl_Part 20 Largest_Parcel: FarmerID, District, Village, Landtype, Season, Cem, Variety, VarietyType, Parcel, Plot, SoilType, Tenure, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20 Largest_Parcel_2.xlsx: not read. sheet Tbl_Part_20_Largest_Parcel: FarmerID, District, Village, Landtype, Season, Cem, Variety, VarietyType, Parcel, Plot, SoilType, Tenure, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20A Input_use.xls: not read. sheet Tbl_Part 20A Input_use: FarmerID, Country, Province, District, Landtype, Season, Cem, Variety, VarietyType, SoilType, Tenure, Inputs, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20A Input_use.xlsx: not read. sheet Tbl_Part_20A_Input_use: FarmerID, District, Village, Season, CEM, VarietyType, Specify, Inputs, Unit, Area, PriceRent, PricePerUnit, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20A Input_use_1.xls: not read. sheet Tbl_Part 20A Input_use: FarmerID, District, Village, Inputs, Unit, Area, PriceRent, Qty, Cost, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 20A Input_use_1.xlsx: not read. sheet Tbl_Part_20A_Input_use: FarmerID, District, Village, Season, CEM, VarietyType, Specify, Inputs, Unit, Area, PriceRent, PricePerUnit, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20A Input_use_2.xls: not read. sheet Tbl_Part 20A Input_use: FarmerID, District, Village, Season, Cem, VarietyType, Specify, Inputs, Unit, Area, PriceRent, PricePerUnit, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20A Input_use_2.xlsx: not read. sheet Tbl_Part_20A_Input_use: FarmerID, District, Village, Season, CEM, VarietyType, Specify, Inputs, Unit, Area, PriceRent, PricePerUnit, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20B Power_use.xls: not read. sheet Tbl_Part 20B Power_use: FarmerID, Country, Province, District, Landtype, Season, Cem, Variety, VarietyType, SoilType, Tenure, Inputs, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20B Power_use.xlsx: not read. sheet Tbl_Part_20B_Power_use: FarmerID, District, Village, Season, CEM, VarietyType, Inputs, Specify, Unit, Area, PriceRent, CPriceRent, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20B Power_use_1.xls: not read. sheet Tbl_Part 20B Power_use: FarmerID, District, Village, Inputs, Specify, Unit, Area, PriceRent, DaysHrs, Cost, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 20B Power_use_1.xlsx: not read. sheet Tbl_Part_20B_Power_use: FarmerID, District, Village, Season, CEM, VarietyType, Inputs, Specify, Unit, Area, PriceRent, CPriceRent, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20B Power_use_2.xls: not read. sheet Tbl_Part 20B Power_use: FarmerID, District, Village, Season, Cem, VarietyType, Inputs, Specify, Unit, Area, PriceRent, CPriceRent, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20B Power_use_2.xlsx: not read. sheet Tbl_Part_20B_Power_use: FarmerID, District, Village, Season, CEM, VarietyType, Inputs, Specify, Unit, Area, PriceRent, CPriceRent, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_inputhrs.xls: not read. sheet Tbl_Part 20C Labor_inputhrs: FarmerID, Country, Province, District, HoursWorkperDay, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_inputhrs.xlsx: not read. sheet Tbl_Part_20C_Labor_inputhrs: FarmerID, District, Village, HoursWorkperDay, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_inputhrs_1.xls: not read. sheet Tbl_Part 20C Labor_inputhrs: FarmerID, District, Village, HoursWorkperDay, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_inputhrs_1.xlsx: not read. sheet Tbl_Part_20C_Labor_inputhrs: FarmerID, District, Village, HoursWorkperDay, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_inputhrs_2.xls: not read. sheet Tbl_Part 20C Labor_inputhrs: FarmerID, District, Village, HoursWorkperDay, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_inputhrs_2.xlsx: not read. sheet Tbl_Part_20C_Labor_inputhrs: FarmerID, District, Village, HoursWorkperDay, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_use.xls: not read. sheet Tbl_Part 20C Labor_use: FarmerID, Country, Province, District, Landtype, Season, Cem, Variety, VarietyType, SoilType, Tenure, Inputs, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_use.xlsx: not read. sheet Tbl_Part_20C_Labor_use: FarmerID, District, Village, Season, CEM, VarietyType, Inputs, Specify, Area, FamilyDays, HiredDays, TotalDays, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_use_1.xls: not read. sheet Tbl_Part 20C Labor_use: FarmerID, District, Village, Season, Cem, VarietyType, Inputs, Specify, Area, FamilyDays, HiredDays, TotalDays, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_use_1.xlsx: not read. sheet Tbl_Part_20C_Labor_use: FarmerID, District, Village, Season, CEM, VarietyType, Inputs, Specify, Area, FamilyDays, HiredDays, TotalDays, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_use_2.xls: not read. sheet Tbl_Part 20C Labor_use: FarmerID, District, Village, Season, Cem, VarietyType, Inputs, Specify, Area, FamilyDays, HiredDays, TotalDays, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 20C Labor_use_2.xlsx: not read. sheet Tbl_Part_20C_Labor_use: FarmerID, District, Village, Season, CEM, VarietyType, Inputs, Specify, Area, FamilyDays, HiredDays, TotalDays, .... Observation records are missing from d until this file is standardized.
# Tbl_Part 21A Division_of_labor.xls: not read. sheet Tbl_Part 21A Division_of_labor: FarmerID, Country, Province, District, RiceFarmAct, Farm, WhosLabor. Observation records are missing from d until this file is standardized.
# Tbl_Part 21A Division_of_labor.xlsx: not read. sheet Tbl_Part_21A_Division_of_labor: FarmerID, District, Village, RiceFarmAct, HOwnedFarm, WOwnedFarm, HOthersFarm, WOthersFarm, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21A Division_of_labor_1.xls: not read. sheet Tbl_Part 21A Division_of_labor: FarmerID, District, Village, RiceFarmAct, HOwnedFarm, WOwnedFarm, HOthersFarm, WOthersFarm, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21A Division_of_labor_1.xlsx: not read. sheet Tbl_Part_21A_Division_of_labor: FarmerID, District, Village, RiceFarmAct, HOwnedFarm, WOwnedFarm, HOthersFarm, WOthersFarm, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21A Division_of_labor_2.xls: not read. sheet Tbl_Part 21A Division_of_labor: FarmerID, District, Village, RiceFarmAct, HOwnedFarm, WOwnedFarm, HOthersFarm, WOthersFarm, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21A Division_of_labor_2.xlsx: not read. sheet Tbl_Part_21A_Division_of_labor: FarmerID, District, Village, RiceFarmAct, HOwnedFarm, WOwnedFarm, HOthersFarm, WOthersFarm, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21B Other_Income.xls: not read. sheet Tbl_Part 21B Other_Income: FarmerID, Country, Province, District, OtherIncomeGen, Husband, Wife, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21B Other_Income.xlsx: not read. sheet Tbl_Part_21B_Other_Income: FarmerID, District, Village, OtherIncomeGen, Husband, Wife, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21B Other_Income_1.xls: not read. sheet Tbl_Part 21B Other_Income: FarmerID, District, Village, OtherIncomeGen, Husband, Wife, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21B Other_Income_1.xlsx: not read. sheet Tbl_Part_21B_Other_Income: FarmerID, District, Village, OtherIncomeGen, Husband, Wife, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21B Other_Income_2.xls: not read. sheet Tbl_Part 21B Other_Income: FarmerID, District, Village, OtherIncomeGen, Husband, Wife, Comments. Observation records are missing from d until this file is standardized.
# Tbl_Part 21B Other_Income_2.xlsx: not read. sheet Tbl_Part_21B_Other_Income: FarmerID, District, Village, OtherIncomeGen, Husband, Wife, Comments. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"
Dissemination of Submergence-Tolerant Varieties and Associated New Production Practices to Southeast Asia

This is a household baseline survey that will measure ex-post and ex-ante economic impact of submergence to the rice farming communities in Southeast Asia. The survey has 1,173 samples that collected rice, non-rice and non-farm economic activities and production output during flood and non-flood years. Demographic characteristics, land holding, rice and non-rice production, sources of income and production practices are some of the basic variables used in the survey. In addition, mitigating measures during flood year and gender roles were also collected.
"

	uri <- "doi:10.7910/DVN/26165"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "STRASA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 25,
		carob_effort = 0.7
	)

	getf <- function(name) ff[basename(ff) == name]
	b <- data.frame(
		country = c("Laos", "Vietnam", "Vietnam", "Cambodia", "Thailand", "Indonesia"),
		cc = c("LAO", "CVN", "SVN", "KHM", "THA", "IDN"),
		f4 = c("Tbl_Part 04 Rice Varieties Grow.xlsx", "Tbl_Part 04 Rice Varieties Grow_1.xlsx", "Tbl_Part 04 Rice Varieties Grow_2.xlsx",
			"Tbl_Part 04 Rice Varieties Grow.xls", "Tbl_Part 04 Rice Varieties Grown_1.xls", "1Tbl_Part 04 Rice Varieties Grow.xls"),
		f0 = c("Tbl_Part 0 Intro.xlsx", "Tbl_Part 0 Intro_1.xlsx", "Tbl_Part 0 Intro_2.xlsx",
			"Tbl_Part 0 Intro.xls", "Tbl_Part 0 Intro_2.xls", NA)
	)

	proc <- function(i) {
		r <- carobiner::read.excel(getf(b$f4[i]))
		r$FarmerID <- as.character(r$FarmerID)
		if (is.null(r$Village)) r$Village <- NA
		if (is.null(r$Province)) r$Province <- NA
		if (is.null(r$ReasonVar)) r$ReasonVar <- NA
		coded <- b$cc[i] == "IDN"
		d <- data.frame(
			country = b$country[i],
			hhid = paste0(b$cc[i], "-", r$FarmerID),
			adm1 = r$Province,
			adm2 = r$District,
			location = r$Village,
			season_constraint = ifelse(r$FloodNormal == "Flood", "flood", NA),
			flood_year = r$FloodNormal == "Flood",
			land_type_ = if (coded) paste("code", r$Landtype) else tolower(r$Landtype),
			season = if (coded) NA else r$Season,
			season_code_ = if (coded) r$Season else NA,
			variety = r$RiceVar,
			variety_type = if (coded) NA else as.vector(c(MV="modern", TV="traditional")[r$MVTV]),
			crop_establishment_ = if (coded) paste("code", r$CEM) else r$CEM,
			field_size = r$Area,
			yield = ifelse(r$Area > 0, r$Prod / r$Area, NA),
			planting_month_ = if (coded) NA else r$PlMonth,
			planting_week_ = if (coded) NA else r$PlWeek,
			harvest_month_ = if (coded) NA else r$HarMonth,
			harvest_week_ = if (coded) NA else r$HarWeek,
			variety_choice_reason_ = r$ReasonVar
		)
		if (!is.na(b$f0[i])) {
			r1 <- carobiner::read.excel(getf(b$f0[i]))
			r1$FarmerID <- as.character(r1$FarmerID)
			j <- match(r$FarmerID, r1$FarmerID)
			if (all(is.na(d$location))) d$location <- r1$Village[j]
			d$planting_date <- as.character(ifelse(d$flood_year, r1$FloodYear[j], r1$NormalYear[j]))
		} else {
			d$planting_date <- NA
		}
		d
	}
	d <- do.call(rbind, lapply(seq_len(nrow(b)), proc))
	d$flood_year <- NULL

	d$adm1[d$adm2 %in% c("Pakse", "Sanasomboun")] <- "Champasak"
	d$adm1[d$adm2 == "Sebangfai"] <- "Khammouan"
	d$adm1[d$adm2 %in% c("An Nhon", "Tuy Phuoc")] <- "Binh Dinh"
	d$adm1[d$adm2 == "Phung Hiep"] <- "Hau Giang"
	d$adm1[d$adm2 == "Vinh Hung"] <- "Long An"
	d$adm1[d$adm1 == "Kampong Thum"] <- "Kampong Thom"
	d$adm1[d$adm1 == "Pouthisat"] <- "Pursat"
	d$adm1[d$adm2 == "INDRAMAYU"] <- "Jawa Barat"
	d$adm1[d$adm2 == "KAYU AGUNG / OKI"] <- "Sumatera Selatan"
	d$adm2[d$adm2 == "INDRAMAYU"] <- "Indramayu"
	d$adm2[d$adm2 == "KAYU AGUNG / OKI"] <- "Ogan Komering Ilir"

	d$season <- carobiner::replace_values(d$season,
		c("DS", "WS", "Dry season", "Wet season", "Wet Season", "Winter-Spring", "Summer-Autumn", "Autumn-Winter"),
		c("dry", "wet", "dry", "wet", "wet", "dry", "early wet", "late wet"))
	d$variety_choice_reason_ <- trimws(d$variety_choice_reason_)
	d$crop_establishment_[d$crop_establishment_ == "code NA"] <- NA
	d$planting_method <- c(TPR="transplanting", DSR="direct seeding", WSR="direct seeding")[d$crop_establishment_] |> as.vector()

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
