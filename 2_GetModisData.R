
require(SSOAP)

ornlMODIS = processWSDL("http://daac.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb_subset/MODIS_webservice.wsdl")

ornlMODISFuncs = genSOAPClientInterface(operations=ornlMODIS@operations[[1]], def=ornlMODIS)

#create a list of lat and longs to get data for
sites = read.csv("CaTT_sitelocations.csv")
head(sites)

## use the getsubset function
result = ornlMODISFuncs@functions$getsubset(sites[1,3], sites[1,4], "MOD13Q1", "250m_16_days_NDVI", "A2000049", "A2009353", as.integer(1), as.integer(1))

## print result
print(result)



GetDates(, sites[1,4], "MOD13Q1")

GetSubset(sites[1,3], sites[1,4], "MOD13Q1", "250m_16_days_NDVI", "A2000049", "A2009353", 1, 1)


#run it in a loop and save the data to a dataframe

for(i in 1:length(sites$lat)){
  GetSubset
  
}


