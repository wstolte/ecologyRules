source("ae.R")
source("HABITAT_functions.R")
library("raster")

#Get directory with Knowledge rules
knowledgeRuleDir <- "data\\_knowledgerules"

#Read the knowledge rules of the Barbel
testfile = file.path(knowledgeRuleDir, "species", "Fish", "Barbus_barbus.xml")
ae <- read_ae_xml(testfile)

#See which Modeltypes and Systems are available in the knowledge rules
get_element_ModelTypeNames(ae)
get_system_names(ae, "HSI")

#Scan the file to read in the model (not complete yet)
barbus_model <- scan(ae, modelname = "HSI", systemname = "Voortplanting Barbeel (grindbedden)")
get_system_description(ae, modelname = "HSI", systemname = "Voortplanting Barbeel (grindbedden)")

#Get an overview of all knowledgerules and knowledgerule types available
get_all_knowledgeruleNames(ae = ae, modeltype = "HSI", system = "Voortplanting Barbeel (grindbedden)") %>% 
      data.frame()
get_type_knowledgeruleNames(ae, "HSI", system = "Voortplanting Barbeel (grindbedden)")

#Get a specific response curve
response_curve <- get_element_response_curve(ae = ae, modelname = "HSI", 
                        systemname = "Voortplanting Barbeel (grindbedden)", 
                        rcname = "stroomsnelheid_paai")
data <- get_data_response_curve(response_curve)
summary(data)
data

#Get a specific categorical response curve
response_curve <- get_element_response_curve(ae = ae, modelname = "HSI", 
                       systemname = "Voortplanting Barbeel (grindbedden)", 
                       rcname = "substraattype_ei")
data <- get_data_response_curve(response_curve)
summary(data)


#Apply the response curve to a grid
flow_raster = raster("data\\example_data\\Tenryuu_river\\Q150\\flow_velocity_spawning_mean.asc")
plot(flow_raster)

#NOTE: knowledge rule is in cm/s data is in m/s 
#(raster input data used has higher flow velocities than applicable for test case)
var_vector = as.vector(data$rule[,"input"]) / 100
suit_vector = as.vector(data$rule[,"output"])
var_min = head(var_vector, n = 1)
var_max = tail(var_vector, n = 1)
output_raster = calc_raster_response_curve(flow_raster,
                      var_min = var_min, var_max = var_max, 
                      var_vector = var_vector,
                      suit_vector = suit_vector)

plot(output_raster)
