
## conventions

#=====This is python still... convert to R==================================
#XML build up
# ae.XMLlayers = character()
# ae.XMLlayers["layer1"] = ".//Topic"
# ae.XMLlayers["layer1_1"] = ".//Species"
# ae.XMLlayers["layer1_2"] = ".//Autecology"
# ae.XMLlayers["layer1_2_1"] = ".//KnowledgeRules"
# ae.XMLlayers["layer1_2_1_1"] = ".//KnowledgeRule"
# ae.XMLlayers["layer1_2_1_1_1"] = ".//ModelDescription"
# ae.XMLlayers["layer1_2_1_1_2"] = ".//ModelFlowDiagrams"
# ae.XMLlayers["layer1_2_1_1_3"] = "Model"
# ae.XMLlayers["layer1_3"] = "FysicalCharacteristics"
# ae.XMLlayers["layer1_4"] = "Traits"
# ae.XMLlayers["layer1_5"] = "SpeciesDescription"
# ae.XMLlayers["layer1_6"] = "Documentation"
# ae.XMLlayers["layer1_7"] = "DataSources"

# xml2::xml_path(xml2::xml_find_all())

#XML paths # nakijken of dit de goede methode is... 
topic_path                  = ".//Topic"
species_path                = ".//Species"
# knowledgerule_layers        = ae.XMLlayers[c("layer1_2","layer1_2_1","layer1_2_1_1")]
# hier iets veranderen.. 
ModelTypePath               = ".//Autecology/ModelType"
knowledgerule_path   = ".//KnowledgeRules"
  # "/".join([self.XMLlayers[x] for x in knowledgerule_layers])
description_path     = ".//ContentDescription"
modeldescription_path_spec  = ".//ModelDescription"
model_path_spec             = ".//KnowledgeRule/Model"

#XML_specifics
# ae.XMLconvention = list()
# ae.XMLconvention["modelkey"] = "stage"
# ae.XMLconvention["rc"] = "ResponseCurve"
# ae.XMLconvention["rckey"] = "name"
# ae.XMLconvention["rc_dict_datatable"] = "rule"
# ae.XMLconvention["fb"] = "FormulaBased"
# ae.XMLconvention["fbkey"] = "name"
# ae.XMLconvention["fb_result"] = "result_calculation"
# ae.XMLconvention["-Infvalue"] = -999999.0
# ae.XMLconvention["Infvalue"] = 999999.0

# uitzoeken hoe dit moet
# ae.XMLconvention["allowed_knowledgeRulesNames"] = [self.XMLconvention["rc"],self.XMLconvention["fb"]]
#=====================================================================================

require(tidyverse)
require(xml2)

extract_names <- function(dd) {
  require(xml2)
  fl <- list.files(dd)
  catalogue <- data.frame()
  for(ii in 1:length(fl)){
    # ii = 3
    tempcat <- read_ae_xml(file.path(dd, fl[ii])) %>%
      get_element_species() %>%
      xml2::as_list() %>% unlist() %>% t() %>% as.data.frame(stringsAsFactors = F) %>% mutate(file = fl[ii])
    if(ii == 1) 
    {catalogue <- tempcat} else 
    {catalogue <- catalogue %>% dplyr::bind_rows(tempcat)}
  }
  return(catalogue)
}





#' creates xpath string to search for objects by attribute name
#'
#' @param path path
#' @param attribute attribute name or part of name
#' @return xpath string 
#' @examples
ae_xpath_attr_build <- function(path, attribute){
  paste0(
    path, '[@*[contains(.,"', 
    attribute, '")]]'
  )
}



#' reads an xml file containing knowledge rules
#'
#' @param filename The message body containing criteria for data selection.
#' @return autoecology xml object
#' @examples
read_ae_xml <- function(filename) {
  require(xml2)
  #check if file exists
  stopifnot(file.exists(filename)) 
  ae <- xml2::read_xml(filename)
  ae <- xml_ns_strip(ae)
  return(ae)
}


# write_ae_xml <- function(ae, path){
#   xml2::write_xml(ae, path)
# }



#' gets element topic from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element with topic information
#' @examples
get_element_topic <- function(ae){
  type_tag_topic <- xml_find_first(ae, topic_path)
  return(type_tag_topic)
}

get_modeltypes <- function(ae){
  get_element_ModelType(ae) %>% xml2::xml_attrs() %>% map_chr("name")
}

#' gets element species from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element with species information
#' @examples
get_element_species <- function(ae){
  type_tag_species <- xml_find_first(ae, species_path)
  return(type_tag_species)
}

#' gets element species description from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element with species description
#' @examples
get_element_contentdescription <- function(ae) {
  type_tag_sd <- xml_find_first(ae, description_path)
  return(type_tag_sd)
}


get_element_speciesname <- function(ae) {
  get_element_species(ae) %>% xml2::xml_find_first("LatName") %>% as_list() %>% unlist()
}


#' gets element system  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element knowledge rules
#' @examples
get_element_system <- function(ae, modeltype) {
    get_element_ModelType(ae)[which(get_element_ModelType(ae) %>% xml2::xml_attrs() %>% map_chr("name") == modeltype)] %>% 
    xml2::xml_find_all(xpath = ".//System")
}


get_system_names <- function(ae, modeltype) {
  get_element_ModelType(ae)[which(get_element_ModelType(ae) %>% xml2::xml_attrs() %>% map_chr("name") == modeltype)] %>% 
    xml2::xml_find_all(xpath = ".//System") %>%
    xml2::xml_attrs() %>% map_chr("name")
}


#' gets element knowledgerules  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element knowledge rules
#' @examples
get_element_knowledgerule <- function(ae, modeltype, system) {
  nametag = ae_xpath_attr_build(".//System", system)
  get_element_system(ae, "HSI") %>% xml2::xml_parent() %>% xml2::xml_find_first(nametag) 
}







#' gets element knowledgerules  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element knowledge rules
#' @examples
get_element_ModelType <- function(ae) {
  type_tag_krs <- xml_find_all(ae, ModelTypePath)
  return(type_tag_krs)
}


get_element_ModelTypeNames <- function(ae) {
  type_tag_krs <- xml_find_all(ae, ModelTypePath) %>% xml_attr("name")
  return(type_tag_krs)
}



#' gets knowledgerules attributes from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return attributes to knowledgerules
#' @examples
get_knowledgerule_attributes <- function(ae) {
  get_element_knowledgerules(ae) %>%  xml2::xml_children() %>% xml2::xml_attrs()
}


#' gets knowledgerules attributes from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return attributes to knowledgerules
#' @examples
get_knowledgerule_stage <- function(ae) {
  get_element_knowledgerules(ae) %>%
    # type_tag_krn <- xml_find_all(ae, knowledgerule_path) %>% 
    xml2::xml_attr(ae.XMLconvention["modelkey"])
  # return(type_tag_krn)
}



#' gets element knowledgerules  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @param knowledgerule knowledgerule name
#' @return An xml element knowledge rules
#' @examples
get_responsecurve_keys <- function(ae, knowledgerule) {
  require(magrittr)
  xml_find_all(ae, ae_xpath_attr_build(knowledgerule_path, knowledgerule)) %>% 
    xml2::xml_child(search = "Model") %>%
    xml2::xml_children() %>%
    xml2::xml_attr(unname(unlist(ae.XMLconvention["rckey"])))
}


#' gets element knowledgerules  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @param modelname name of model
#' @return An xml element knowledge rules
#' @examples
#' get_element_knowledgerule(ae, "testmodel")
get_element_knowledgerule <- function(ae, knowledgerule) {
  # check_model_name(modelname) # checkt of responsecurve voorkomt
  ae %>% 
  xml_find_all(xpath = ae_xpath_attr_build(knowledgerule_path, knowledgerule))
}

get_element_functionalresponsecurves <- function(ae, myknowledgerule, myresponsecurve){
  ae %>%
    get_element_knowledgerule(myknowledgerule) %>% 
    xml_child(search = "Model") %>%
    xml_children() %>% 
    xml_find_all(
      xpath = ae_xpath_attr_build(path = "//ResponseCurve", myresponsecurve)
    )
}





#' gets element model description from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element knowledge rules
#' @examples
get_element_modeldescription <- function(ae, modelname) {
  check_model_name(modelname)
  type_tag_kr <- get_element_knowledgerule(ae, modelname)
  type_tag_modeldescription = xml_find_all(type_tag_kr, modeldescription_path_spec)
  return(type_tag_modeldescription)
}






#' scans an xml file containing knowledge rules
#'
#' @param ae autoecology xml object
#' @return An xml structure containing ecological knowledge rules for a species
#' @examples
scan <- function(ae){
  species_root = get_element_species(ae)
  latinname = xml2::xml_find_chr(".//LatName")
  commonname = xml2::xml_find_chr(".//CommonName")
  type_tag_krs = getelement_knowledgerules(ae)
  models_available = NA# get modelkeys? python: [e.get(self.XMLconvention["modelkey"]) for e in type_tag_krs]
  models = models_available
}
  



#' checks if model exists in an xml file containing knowledge rules
#'
#' @param ae autoecology xml object
#' @modelname name of model
#' @return a logical value
#' @examples
check_model_name <- function(ae, modelname){
  if(!modelname %in% ae.models){
    stop("Model not existent and not present in ae.models")}
  if(is.null(ae.models)){
    stop("No scan loaded yet, use ae::scan()")
  }
}





#' short description
#'
#' @param parameter description 
#' @param parameter description
#' @return description
#' @examples
#' short description
#'
#' @param parameter description 
#' @param parameter description
#' @return description
#' @examples
#' short description
#'
#' @param parameter description 
#' @param parameter description
#' @return description
#' @examples
#' short description
#'
#' @param parameter description 
#' @param parameter description
#' @return description
#' @examples
#' short description
#'
#' @param parameter description 
#' @param parameter description
#' @return description
#' @examples
#' short description
#'
#' @param parameter description 
#' @param parameter description
#' @return description
#' @examples
#' short description
#'
#' @param parameter description 
#' @param parameter description
#' @return description
#' @examples



