


##############################################################################
## Find commercial to resi conversions
##############################################################################

library(dplyr)
library(stringr)
library(lubridate)
library(sf)
library(geojsonio)

source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach//useful functions/HWE_FUNCTIONS.r")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach//useful functions/-.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach//useful functions/useful minor functions.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach//useful functions/hwe colors.R")

options(scipen=999)


load("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/ad_hoc/44 Wall/office_to_resi_conversions 20180508_1530.RData")

# Read in and mutate pluto ------------------------------------------------


pluto.all.hold <- readRDS("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Project Data/PLUTO/pluto_lean_compressed_2003_2017.rds")

pluto.all <- pluto.all.hold %>%
  mutate(
    borocode = ifelse(Borough=="MN"
                      ,1
                      ,ifelse(Borough=="BX"
                              ,2
                              ,ifelse(Borough=="BK"
                                      ,3
                                      ,ifelse(Borough=="QN"
                                              ,4
                                              ,ifelse(
                                                Borough=="SI"
                                                ,5
                                                ,str_sub(BBL,start=1,end=1)
                                              )
                                      )
                              )
                      )
    )
    ,BBL = as.character(
      paste(as.numeric(borocode)
            ,as.numeric(Block)
            ,as.numeric(Lot)
            ,sep="_"
      )
    )
    ,APPBBL = ifelse(APPBBL==0
                     ,NA
                     ,paste(
                       as.numeric(str_sub(APPBBL,start=1,end=1))
                       ,as.numeric(str_sub(APPBBL,start=2,end=-5))
                       ,as.numeric(str_sub(APPBBL,start=-4,end=-1))
                       ,sep="_"
                     )
    )
    ,APPDate = parse_date_time(
      APPDate
      ,"m!/d!/y!*"
    )
    ,APPYear = year(APPDate)
    
    ## use version of BBL which is either current BBL or if changed at some point APPBBL
    ,BBL.app = ifelse(!is.na(APPBBL)
                      ,APPBBL
                      ,BBL
    )
    ,BldgClass_1 = str_sub(BldgClass,start=1,end=1)
  ) %>%
  arrange(desc(Year))


## which BBLs have changed building classes at some point
changed.bldgclass <- pluto.all %>%
  group_by(BBL.app) %>%
  summarize(BldgClass.levs = length(unique(BldgClass))) %>%
  filter(BldgClass.levs > 1)

## classify residential or commercial by building year
pluto_restr.df <- pluto.all %>%
  semi_join(
    changed.bldgclass
    ,by="BBL.app"
  ) %>%
  mutate(Category = ifelse(((BldgClass_1 %in% "O" | BldgClass %in% c("RB","RC","R5")) & 
                              (OfficeArea > ResArea &
                                 OfficeArea > GarageArea &
                                 OfficeArea > FactryArea &
                                 OfficeArea > RetailArea
                              )) | OfficeArea > (ComArea + ResArea + OfficeArea + RetailArea + GarageArea + StrgeArea + FactryArea)
                           ,"OFF"
                           ,ifelse(
                             (BldgClass_1 %in% c("C","D") | (BldgClass_1 %in% "R" & !BldgClass %in% c("RA","R5","RB","RC","RG","RH","RI","RK","RW","RS"))) & (UnitsRes > 2 | ResArea > 2500)
                             ,"RES"
                             ,"OTHER"
                           )
  )
  )

pluto_restr.df <- pluto_restr.df %>%
  semi_join(pluto_restr.df %>%
              filter(Category %in% c("OFF","RES"))
            ,by="BBL.app"
  )  %>%
  group_by(BBL.app,Year) %>%
  mutate(
    BldgArea = ifelse(BldgArea==0
                      ,NA
                      ,BldgArea)
    ,BldgArea.total = sum(BldgArea)
    ,BBL.count = length(unique(BBL))
  ) %>%
  ungroup()



pluto_tmp.df <- pluto_restr.df %>%
  group_by(BBL.app) %>%
  arrange(Year) %>%
  mutate(order = 1:n()
         ,BldgArea.total = ifelse(BldgArea.total == 0
                            ,NA
                            ,BldgArea.total)
  ) %>%
  summarize(
    RES_CONVERSION.tmp = sum(Category %in% "OFF")>=1 & sum(Category %in% "RES")>=1
    ,sf_med_off = median(BldgArea.total[Category %in% "OFF"],na.rm=T)
    ,sf_off.last = ifelse(sum(Category %in% "OFF")>=1
                          ,BldgArea.total[order==max(order[Category %in% "OFF"])]
                          ,NA
    )
    ,sf_med_res = median(BldgArea.total[Category %in% "RES"],na.rm=T)
    ,sf_res.first = ifelse(sum(Category %in% "RES")>=1
                           ,BldgArea.total[order==min(order[Category %in% "RES"])]
                           ,NA
    )
    ,sfchange_med.prop = (sf_med_res - sf_med_off)/sf_med_off
    ,sfchange.prop = ifelse(sum(Category %in% "OFF")>=1 & sum(Category %in% "RES")>=1
                            ,(BldgArea.total[order==min(order[Category %in% "RES"])] - BldgArea.total[order==max(order[Category %in% "OFF"])])/BldgArea.total[order==max(order[Category %in% "OFF"])]
                            ,NA
    )
    ,contains_vacant = sum(BldgClass_1 %in% "V")>=1
    ,YB_within = sum(YearBuilt[Category %in% "RES"] > 2003) >= 1
    ,YA_within = sum(YearAlter1)
    ,RES_CONVERSION = RES_CONVERSION.tmp == T & max(order[Category %in% "OFF"]) < min(order[Category %in% "RES"])
    ,RES_CONVERSION_no_teardown = RES_CONVERSION.tmp == T & contains_vacant == F & max(order[Category %in% "OFF"]) < min(order[Category %in% "RES"])
  ) %>%
  ungroup()


pluto.conv <- pluto_restr.df %>%
  filter(Borough %in% "MN") %>%
  semi_join(
    pluto_tmp.df %>%
      filter(RES_CONVERSION_no_teardown==T & (abs(sfchange.prop)<=.5 | is.na(sfchange.prop)) & sf_med_res >= 75000 & !YB_within)
    ,by="BBL.app"
  )


pluto.conv <- pluto.conv %>%
  semi_join(
    pluto.conv %>%
      group_by(BBL.app) %>%
      summarize(contains_res = sum(Category %in% "RES")>=1) %>%
      ungroup() %>%
      filter(contains_res==T) 
    ,by="BBL.app"
  ) %>%
  filter(BldgClass_1 %in% c("C","D","O","R")) %>%
  filter(!duplicated(paste(BBL,Year,BldgClass)))


## read in pediacities shapefile
pediashape.url <- "http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson"

pedia.map <- geojson_read(as.location(pediashape.url),
                          method="local",
                          what="sp")

pedia.map <- st_as_sf(pedia.map,crs=4326)

## can only geolocate on those with lat/lon
tmp.sf <- st_as_sf(pluto.conv %>% 
                     filter(!is.na(lat)) %>%
                     select(BBL,lon,lat)
                   ,coords = c("lon", "lat"), crs = 4326)

## spatial join and then re-join with resi.out
tmp.sf <- st_join(tmp.sf
                  ,pedia.map %>%
                    rename(Neighborhood = neighborhood) %>%
                    mutate(BOROUGH.PEDIA = toupper(as.character(borough)))) %>% 
  select(BBL,Neighborhood)


pluto.conv <- left_join(pluto.conv
                        ,tmp.sf %>%
                          select(BBL,Neighborhood)
                        ,by="BBL"
) %>%
  select(-geometry) %>%
  filter(!duplicated(paste(BBL,Year,BldgClass))) %>%
  select(BBL.app,BBL,Address,Year,BldgClass,YearBuilt,Category,UnitsRes,OfficeArea,everything())


pluto.conv <- pluto.conv %>%
  left_join(
    pluto.all %>%
      semi_join(pluto.conv
                ,by="BBL") %>%
      filter(BldgArea > 0) %>%
      arrange(desc(Year)) %>%
      filter(!duplicated(BBL)) %>%
      select(BBL,BldgArea,ResArea,Year) %>%
      rename(BldgArea.tmp = BldgArea
             ,ResArea.tmp = ResArea
             ,Year.pluto_amd = Year
      )
    ,by="BBL"
  ) %>%
  mutate(
    BldgArea = ifelse(BldgArea == 0
                      ,BldgArea.tmp
                      ,BldgArea)
    ,ResArea = ifelse(ResArea==0
                      ,ResArea.tmp
                      ,ResArea)
  ) %>%
  select(-BldgArea.tmp
         ,-ResArea.tmp)


pluto_conv.restr <- pluto.conv %>%
  group_by(BBL.app) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  filter(Neighborhood %in% c("Financial District","Tribeca","Civic Center","Midtown") & !duplicated(BBL.app)) %>%
  arrange(Neighborhood,desc(BldgArea))

out.df <- pluto_conv.restr  %>%
  select(Address,BldgArea,Neighborhood,ZipCode)

write.csv(out.df, "/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/ad_hoc/44 Wall/converted_buildings 20180508_1509.csv"
          ,row.names=F
)


# pluto_conv.restr %>%
#   filter(!duplicated(BBL.app)) %>%
#   group_by(Neighborhood) %>%
#   summarize(
#     count = n()
#     ,BldgArea = sum(BldgArea)
#     ,ResArea = sum(ResArea)
#   )



# pluto.all %>%
#   arrange(desc(Year)) %>%
#   filter(!duplicated(BBL)) %>%
#   filter(Neighborhood %in% c("Financial District","Tribeca","Civic Center","Midtown") & str_sub(BldgClass,start=1,end=1)=="O") %>%
#   group_by(Neighborhood) %>%
#   summarize(count = n()
#             ,BldgArea = sum(BldgArea)
#             )



# Gross SF office non-conversions -----------------------------------------


pluto.mn <- pluto.all %>%
  filter(Year==2017 & Borough == "MN" & !duplicated(BBL))

## where bldgarea is 0 try to recover building area for most recent year
pluto.mn <- pluto.mn %>%
  left_join(
    pluto.all %>%
      semi_join(pluto.mn
                ,by="BBL") %>%
      filter(BldgArea > 0) %>%
      arrange(desc(Year)) %>%
      filter(!duplicated(BBL)) %>%
      select(BBL,BldgArea,ResArea,OfficeArea,Year) %>%
      rename(BldgArea.tmp = BldgArea
             ,ResArea.tmp = ResArea
             ,OfficeArea.tmp = OfficeArea
             ,Year.pluto_amd = Year
      )
    ,by="BBL"
  ) %>%
  mutate(
    BldgArea = ifelse(BldgArea == 0
                      ,BldgArea.tmp
                      ,BldgArea)
    ,ResArea = ifelse(ResArea==0
                      ,ResArea.tmp
                      ,ResArea)
    ,OfficeArea, ifelse(OfficeArea==0
                        ,OfficeArea.tmp
                        ,OfficeArea)
  ) %>%
  select(-BldgArea.tmp
         ,-ResArea.tmp
         ,-OfficeArea.tmp
  )

## same for lat/lon
pluto.mn <- pluto.mn %>%
  left_join(
    pluto.all %>%
      semi_join(pluto.mn
                ,by="BBL") %>%
      filter(!is.na(lat)) %>%
      arrange(desc(Year)) %>%
      filter(!duplicated(BBL)) %>%
      select(BBL,lat,lon) %>%
      rename(lat.tmp = lat
             ,lon.tmp = lon
      )
    ,by="BBL"
  ) %>%
  mutate(
    lat = ifelse(is.na(lat)
                      ,lat.tmp
                      ,lat)
    ,lon = ifelse(is.na(lon)
                  ,lon.tmp
                  ,lon)
  ) %>%
  select(-lat.tmp
         ,-lon.tmp
  )


## attach neighborhood
tmp.sf <- st_as_sf(pluto.mn %>% 
                     filter(!is.na(lat)) %>%
                     select(BBL,lon,lat)
                   ,coords = c("lon", "lat"), crs = 4326)

tmp.sf <- st_join(tmp.sf
                  ,pedia.map %>%
                    rename(Neighborhood = neighborhood) %>%
                    mutate(BOROUGH.PEDIA = toupper(as.character(borough)))) %>% 
  select(BBL,Neighborhood)


pluto.mn <- left_join(pluto.mn
                      ,tmp.sf %>%
                        select(BBL,Neighborhood)
                      ,by="BBL"
) %>%
  select(-geometry)


## summary for both the conversions and non-conversions in the neighborhoods of interest
tmp1.smry <- pluto_conv.restr %>%
  filter(!duplicated(BBL.app)) %>%
  filter(Neighborhood %in% c("Financial District","Tribeca","Civic Center","Midtown") & !duplicated(BBL.app)) %>%
  group_by(Neighborhood) %>%
  summarize(
    count = n()
    ,BldgArea.rescon = sum(BldgArea,na.rm=T)
  )


tmp1.smry <- pluto.mn %>%
  semi_join(pluto_conv.restr %>%
              filter(Neighborhood %in% c("Financial District","Tribeca","Civic Center","Midtown"))
            ,by="BBL") %>%
  group_by(Neighborhood) %>%
  summarize(
    count = n()
    ,BldgArea.rescon = sum(BldgArea,na.rm=T)
  )


tmp2.smry <- pluto.mn %>%
  filter(!duplicated(BBL.app) & BldgClass_1 %in% "O" | BldgClass %in% c("RB","RC","R5")) %>%
  filter(Neighborhood %in% c("Financial District","Tribeca","Civic Center","Midtown") & !duplicated(BBL.app)) %>%
  group_by(Neighborhood) %>%
  summarize(count = n()
            ,BldgArea.off = sum(BldgArea,na.rm=T)
            ,OffArea.off = sum(OfficeArea)
  )

smry <- tmp1.smry %>%
  left_join(
    tmp2.smry %>%
      select(-count)
    ,by="Neighborhood"
  ) %>%
  mutate(Prop_off = BldgArea.rescon/BldgArea.off)


toClip(smry)


alteration_year <- pluto.conv %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(BBL.app) %>%
  arrange(Year) %>%
  mutate(order = 1:n()) %>%
  summarize(
    Conv_year = ifelse(sum(Category %in% "RES") >= 1
                       ,min(Year[Category %in% "RES"])
                       ,NA
    )
    ,Conv_order = min(order[Category %in% "RES"])
    ,Conv_from = ifelse(sum(Category %in% "RES")>=1
                        ,BldgClass[order = Conv_order - 1]
                        ,NA
    )
    ,Conv_to = ifelse(sum(Category %in% "RES")>=1
                      ,BldgClass[order = Conv_order]
                      ,NA
    )
    ,Conv_area = ifelse(sum(Category %in% "RES")>=1
                        ,round(median(BldgArea[Category %in% "RES"],na.rm=T))
                        ,NA
    )
  ) %>%
  left_join(pluto.conv %>%
              filter(!duplicated(BBL.app)) %>%
              select(BBL.app,Address,Neighborhood)
  ) %>%
  select(Address,BBL.app,Conv_year,Conv_from,Conv_to,Conv_area,Neighborhood) %>%
  filter(Neighborhood %in% c("Financial District","Tribeca","Civic Center","Midtown") & !duplicated(BBL.app))

toClip(alteration_year)

# alteration_year <- alteration_year %>%
#   left_join(pluto.conv %>%
#               filter(!duplicated(BBL.app)) %>%
#               select(BBL.app,Address,Neighborhood)
#             )

alteration.out <- alteration_year %>%
  ungroup() %>%
  semi_join(
    pluto.conv %>%
      filter(Neighborhood %in% c("Financial District","Tribeca","Civic Center","Midtown"))
    ,by="BBL.app"
  ) %>%
  left_join(
    pluto.conv %>%
      filter(!duplicated(BBL.app)) %>%
      select(BBL.app,Neighborhood)
    ,by="BBL.app"
  ) %>%
  group_by(Conv_year,Neighborhood) %>%
  summarize(
    Area_Converted = sum(Conv_area)
  )

toClip(alteration.out)



