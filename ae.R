
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
parameter_path              = ".//Parameter"
species_path                = ".//Species"
habitats_path                = ".//Habitats"
wfdind_path                 = ".//WFDindicator"
# knowledgerule_layers        = ae.XMLlayers[c("layer1_2","layer1_2_1","layer1_2_1_1")]
# hier iets veranderen.. 
ModelTypePath               = ".//Autecology/ModelType"
knowledgerule_path   = ".//KnowledgeRules"
  # "/".join([self.XMLlayers[x] for x in knowledgerule_layers])
description_path     = ".//TopicDescription"
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
require(purrr)
require(xml2)
library(rlang)
library(data.table)

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



## idea for function to make mathematical expressions safe. Only allow functions containging whitelist of functions.
## Add all possible variable names? 
## 
interpret <- function(expr_str, 
                      max_length = 32, 
                      whitelist = c("x", "y", "/", "*", "+", "-", "log10", "sqrt", "c")) {
  safer_eval <- function(expr) {
    if (rlang::is_call(expr)) {
      fn_name <- rlang::call_name(expr)
      if (!fn_name %in% whitelist) stop("Disallowed function: ", fn_name)
      do.call(get(fn_name, baseenv()), Map(safer_eval, rlang::call_args(expr)))
    } else if (rlang::is_syntactic_literal(expr)) {
      expr
    } else {
      stop("Unknown expression: ", expr)
    }
  }
  stopifnot(length(expr_str) < max_length)
  safer_eval(rlang::parse_expr(expr_str))
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

#' gets element parameters from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element with species information
#' @examples
get_element_parameter <- function(ae){
  type_tag_parameter <- xml_find_first(ae, parameter_path)
  return(type_tag_parameter)
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

#' gets element habitats from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element with species information
#' @examples
get_element_habitats <- function(ae){
  type_tag_habitats <- xml_find_first(ae, habitats_path)
  return(type_tag_habitats)
}

#' gets element WFD ind from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element with WFD indicator information
#' @examples
get_element_wfdindicator <- function(ae){
  type_tag_wfdind <- xml_find_first(ae, wfdind_path)
  return(type_tag_wfdind)
}

#' gets element content description from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An xml element with content description
#' @examples
get_element_contentdescription <- function(ae) {
  type_tag_sd <- xml_find_first(ae, description_path)
  return(type_tag_sd)
}

#' gets element species LatName from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An XML element with species LatName
#' @examples get_element_speciesname(ae)
get_element_speciesname <- function(ae) {
  type_tag_speciesname <- xml_find_first(ae,"LatName")
  return(type_tag_speciesname)
}

#' gets element CommonNames from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return An XML element with CommonNames
#' @examples get_element_commonnames(ae)
get_element_commonnames <- function(ae) {
  type_tag_commonnames <- xml_find_first(ae,"CommonNames")
  return(type_tag_commonnames)
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
#' @param modeltype name of modeltype
#' @param system name of system
#' @return An xml element knowledge rules
#' @examples
#' get_element_knowledgerule(ae = ae, modeltype = "HSI", system = "Voortplanting Barbeel (grindbedden)")
get_element_knowledgerules <- function(ae, modeltype, system) {
  nametag = ae_xpath_attr_build(".//System", system)
  get_element_system(ae, "HSI") %>% xml2::xml_parent() %>% xml2::xml_find_first(nametag) %>%
    xml2::xml_find_first(".//KnowledgeRules")
}

get_all_knowledgeruleNames <- function(ae, modeltype, system){
get_element_knowledgerules(ae = ae, modeltype = "HSI", system = system) %>%
  xml2::xml_children() %>% xml2::xml_attrs() %>% unlist() %>% unname()
}

get_type_knowledgeruleNames <- function(ae, modeltype, system){
  
  if(!purrr::is_empty(get_element_knowledgerules(ae = ae, modeltype = modeltype, system = system) %>% xml2::xml_find_all(".//FormulaBased") %>% xml2::xml_attrs())){
    FB <- get_element_knowledgerules(ae = ae, modeltype = modeltype, system = system) %>% xml2::xml_find_all(".//FormulaBased") %>% xml2::xml_attrs() %>%
      unlist() %>% unname() %>% tibble(name = .) %>% mutate(type = "FormulaBased")
  } 
    
  if(!purrr::is_empty(get_element_knowledgerules(ae = ae, modeltype = modeltype, system = system) %>% xml2::xml_find_all(".//ResponseCurve") %>% xml2::xml_attrs())){
    RC <- get_element_knowledgerules(ae = ae, modeltype = modeltype, system = system) %>% xml2::xml_find_all(".//ResponseCurve") %>% xml2::xml_attrs() %>%
      unlist() %>% unname() %>% tibble(name = .) %>% mutate(type = "ResponseCurve")
  } 
  if(!exists("FB") & !exists("RC")) return(NULL) else
    if(exists("FB") & exists("RC")) return(FB %>% bind_rows(RC)) else 
      if(exists("FB")) return(FB) else if(exists("RC")) return(RC)
}



# get_formulaBasedNames <- function(ae, modeltype, system){
#   get_element_knowledgerules(ae = ae, modeltype = "HSI", system = "Voortplanting Barbeel (grindbedden)") %>%
#     xml2::xml_children() %>% xml2::xml_attrs() %>% unlist() %>% unname()
# }



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




#' gets element response curve  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @param modelname name of model
#' @param systemname name of system
#' @param rcname name of response curve
#' @return An xml element knowledge rules
#' @examples
#' get_element_response_curve(ae, "testmodel","testresponsecurve")
get_element_response_curve <- function(ae, modelname = "HSI", systemname, rcname){
  ae %>%
    get_element_knowledgerules(modelname, systemname) %>% 
    xml_children() %>% 
    xml_find_all(
      xpath = ae_xpath_attr_build(path = "//ResponseCurve", rcname)
    )
}

#' gets element formula based  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @param modelname name of model
#' @param systemname name of system
#' @param fbname name of formula based
#' @return An xml element knowledge rules
#' @examples
#' get_element_formula_based(ae, "testmodel","testformulabased")
get_element_formula_based <- function(ae, modelname = "HSI", systemname, fbname){
  ae %>%
    get_element_knowledgerules(modelname,systemname) %>% 
    xml_children() %>% 
    xml_find_all(
      xpath = ae_xpath_attr_build(path = "//FormulaBased", fbname)
    )
}

#' gets dataframe content  from Topic specific xml object
#'
#' @param tse topic specific xml object
#' @return A dataframe with commonnames and language
#' @examples
#' get_df_commonnames(tse)
get_df_commonnames <- function(tse){
  name_tag = ".//Name"
  nr = 0
  for( name in list(xml_find_all(tse, xpath = name_tag))){
    if(nr == 0){
      commonnames_df = data.frame("Name" = xml_attr(name,"name"),
                                  "Language" = xml_attr(name,"language"),
                                  stringsAsFactors = FALSE)
    }else{
      commonnames_df_temp = data.frame("Name" = xml_attr(name,"name"),
                                       "Language" = xml_attr(name,"language"),
                                       stringsAsFactors = FALSE)
      commonnames_df = rbind(commonnames_df,commonnames_df_temp)
    }
    nr = nr + 1
  }
  return(commonnames_df) 
}


#' gets data content  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @return A list with the content data
#' @examples
#' get_data_content(ae)
get_data_content <- function(ae){
  content_info = list()
  if(length(get_element_species(ae)) > 0){
    #get_element_species(ae)
    content_info$latinname  = get_element_speciesname(ae) %>% xml_text(trim = TRUE)
  }else if(length(get_element_wfdindicator(ae)) > 0){
    pass
  }else{
    stop(paste("content type is not available.", sep = ""))
  }
  #For all
  content_info$commonnames = get_element_commonnames(ae) %>% get_df_commonnames()
  content_info$contentdes = xml_find_first(get_element_contentdescription(ae), ".//text") %>%
    xml_text(trim = TRUE)
  
  return(content_info)
}

#' gets system description content  from autoecology xml object
#'
#' @param ae autoecology xml object
#' @param modelname name of model
#' @param systemname name of system
#' @return A list with the content data
#' @examples
#' get_system_description(ae)
get_system_description <- function(ae, modelname, systemname){
  nametag = ae_xpath_attr_build(".//System", system)
  tse <- get_element_system(ae, "HSI") %>% xml2::xml_parent() %>% xml2::xml_find_first(nametag) %>%
    xml_find_first(xpath = ".//SystemDescription")
  des_tag = ".//Description"
  nr = 0
  for( sysdescr in list(xml_find_all(tse, xpath = des_tag))){
    if(nr == 0){
      sysdescr_df = data.frame("Language" = xml_attr(sysdescr,"language"),
                               "Description" = xml_find_first(sysdescr,".//text") %>%
                                 xml_text(),
                               stringsAsFactors = FALSE)
    }else{
      sysdescr_df_temp = data.frame("Language" = xml_attr(sysdescr,"language"),
                                    "Description" = xml_find_first(sysdescr,".//text") %>%
                                      xml_text(),
                                    stringsAsFactors = FALSE)
      sysdescr_df = rbind(sysdescr_df,sysdescr_df_temp)
    }
    nr = nr + 1
  }
  return(sysdescr_df[1,"Description"])
}

#' gets data response curve  from autoecology xml object
#'
#' @param rc_element a response curve element
#' @return A list with the response curve element data
#' @examples
#' get_data_response_curve(ae)
get_data_response_curve <- function(rc_element){
  rule_list = list()
  
  rule_list$name = rc_element  %>% xml_attr("name")
  rule_list$KnowledgeruleCategorie = "ResponseCurve"
  content_element = rc_element %>% xml_child(search = "Content")
  rule_list$type =  content_element %>% 
                    xml_child(search = "type") %>% xml_text(trim = TRUE)
  
  rule_list$inputLayers = list()
  input_layer_children = rc_element %>% xml_child(search = "inputLayers") %>% xml_children()
  nr = 0
  for(child in input_layer_children){
    nr = nr + 1
    input_layer <- list()
    input_layer$name = child %>% xml_attr("name")
    input_layer$parameter_name = child %>% xml_child(search = "parameter_name") %>% xml_text(trim = TRUE)
    input_layer$parameter_cat = child %>% xml_child(search = "parameter_cat") %>% xml_text(trim = TRUE)
    input_layer$period = child %>% xml_child(search = "period") %>% xml_text(trim = TRUE)
    input_layer$unit = child %>% xml_child(search = "unit") %>% xml_text(trim = TRUE)
    input_layer$statistic = child %>% xml_child(search = "statistic") %>% xml_text(trim = TRUE)
    input_layer$layer_filename = child %>% xml_child(search = "layer_filename") %>% xml_text(trim = TRUE)
    input_layer$description = child %>% xml_child(search = "description") %>% xml_text(trim = TRUE)
    
    setattr(rule_list$inputLayers,input_layer$name,input_layer) 
  }
  
  rule_list$outputLayers = list()
  output_layer_children = rc_element %>% xml_child(search = "outputLayers") %>% xml_children()
  nr = 0
  for(child in output_layer_children){
    nr = nr + 1
    output_layer <- list()
    output_layer$name = child %>% xml_attr("name")
    output_layer$parameter_name = child %>% xml_child(search = "parameter_name") %>% xml_text(trim = TRUE)
    output_layer$parameter_cat = child %>% xml_child(search = "parameter_cat") %>% xml_text(trim = TRUE)
    output_layer$period = child %>% xml_child(search = "period") %>% xml_text(trim = TRUE)
    output_layer$unit = child %>% xml_child(search = "unit") %>% xml_text(trim = TRUE)
    output_layer$statistic = child %>% xml_child(search = "statistic") %>% xml_text(trim = TRUE)
    output_layer$layer_filename = child %>% xml_child(search = "layer_filename") %>% xml_text(trim = TRUE)
    output_layer$description = child %>% xml_child(search = "description") %>% xml_text(trim = TRUE)
    
    setattr(rule_list$outputLayers,output_layer$name,output_layer)
    
  }
  
  nr = 0
  if(rule_list$type == "scalar"){
    name_tag = ".//valuesScalar/parameter"
    column_names = names(unlist(xml_attrs(xml_find_first(content_element, xpath = name_tag))))
    for(parameter in list(xml_find_all(content_element, xpath = name_tag))){
      if(nr == 0){
        parameter_df = data.frame(val1 = as.numeric(xml_attr(parameter,"input")), 
                                val2 = as.numeric(xml_attr(parameter,"output")),
                                stringsAsFactors = FALSE)
      }else{
        parameter_df_temp = data.frame(val1 = as.numeric(xml_attr(parameter,"input")), 
                                  val2 = as.numeric(xml_attr(parameter,"output")),
                                  stringsAsFactors = FALSE)
        parameter_df = bind_rows(parameter_df,parameter_df_temp)            
      }
      nr = nr + 1  
    }
  }
  else if(rule_list$type == "categorical"){
    name_tag = ".//valuesCategorical/parameter"
    column_names = names(unlist(xml_attrs(xml_find_first(content_element, xpath = name_tag))))
    for(parameter in list(xml_find_all(content_element, xpath = name_tag))){
      if(nr == 0){
        parameter_df = data.frame(val1 = as.numeric(xml_attr(parameter,"input")),
                                  val2 = as.character(xml_attr(parameter,"input_cat")), 
                                  val3 = as.numeric(xml_attr(parameter,"output")),
                                  val3 = as.character(xml_attr(parameter,"output_cat")),
                                  stringsAsFactors = FALSE)
      }else{
        parameter_df_temp = data.frame(val1 = as.numeric(xml_attr(parameter,"input")),
                                       val2 = as.character(xml_attr(parameter,"input_cat")), 
                                       val3 = as.numeric(xml_attr(parameter,"output")),
                                       val3 = as.character(xml_attr(parameter,"output_cat")),
                                       stringsAsFactors = FALSE)
        parameter_df = bind_rows(parameter_df,parameter_df_temp)            
      }
      nr = nr + 1  
    }
  }
  else if(rule_list$type == "ranges"){
    name_tag = ".//valuesRanges/parameter"
    column_names = names(unlist(xml_attrs(xml_find_first(content_element, xpath = name_tag))))
    for(parameter in list(xml_find_all(content_element, xpath = name_tag))){
      if(nr == 0){
        parameter_df = data.frame(val1 = as.numeric(xml_attr(parameter,"rangemin")), 
                                  val2 = as.numeric(xml_attr(parameter,"rangemax")),
                                  val3 = as.numeric(xml_attr(parameter,"HSI")),
                                  stringsAsFactors = FALSE)
      }else{
        parameter_df_temp = data.frame(val1 = as.numeric(xml_attr(parameter,"rangemin")), 
                                       val2 = as.numeric(xml_attr(parameter,"rangemax")),
                                       val3 = as.numeric(xml_attr(parameter,"HSI")),
                                       stringsAsFactors = FALSE)
        parameter_df = bind_rows(parameter_df,parameter_df_temp)            
      }
      nr = nr + 1  
    } 
  }
  else if(rule_list$type == "range / categorical"){
    name_tag = ".//valuesRangeCategorical/parameter"
    column_names = names(unlist(xml_attrs(xml_find_first(content_element, xpath = name_tag))))
    for(parameter in list(xml_find_all(content_element, xpath = name_tag))){
      if(nr == 0){
        parameter_df = data.frame(val1 = as.numeric(xml_attr(parameter,"rangemin")), 
                                  val2 = as.numeric(xml_attr(parameter,"rangemax")),
                                  val3 = as.character(xml_attr(parameter,"cat")),
                                  val4 = as.numeric(xml_attr(parameter,"HSI")),
                                  stringsAsFactors = FALSE)
      }else{
        parameter_df_temp = data.frame(val1 = as.numeric(xml_attr(parameter,"rangemin")), 
                                       val2 = as.numeric(xml_attr(parameter,"rangemax")),
                                       val3 = as.character(xml_attr(parameter,"cat")),
                                       val4 = as.numeric(xml_attr(parameter,"HSI")),
                                       stringsAsFactors = FALSE)
        parameter_df = bind_rows(parameter_df,parameter_df_temp)            
      }
      nr = nr + 1  
    }   
  }else{
    stop(paste("type ",as.character(rule_list$type)," is not available.", sep = ""))
  }
  colnames(parameter_df) <- column_names
  rule_list$rule = parameter_df
  return(rule_list)
}


#' gets data formula based  from autoecology xml object
#'
#' @param fb_element a formula based element
#' @return A list with the formula based element data
#' @examples
#' get_data_formula_based(fbelement)
get_data_formula_based <- function(fb_element){
  rule_list = list()
  
  rule_list$name = fb_element  %>% xml_attr("name")
  rule_list$KnowledgeruleCategorie = "FormulaBased"
  #rule_list$type = fb_element %>% xml_child(search = "type") %>% xml_text(trim = TRUE)
  rule_list$layername = fb_element %>% xml_child(search = "layername") %>% xml_text(trim = TRUE)
  rule_list$unit = fb_element %>% xml_child(search = "unit") %>% xml_text(trim = TRUE)
  rule_list$statistic = fb_element %>% xml_child(search = "statistic") %>% xml_text(trim = TRUE) 
  rule_list$output = fb_element %>% xml_child(search = "output") %>% xml_text(trim = TRUE) 
  rule_list$equation_text = fb_element %>% xml_child(search = "equation") %>% xml_text(trim = TRUE)
  
  nr = 0
  name_tag_scal = ".//Parameters/valuesScalar"
  name_tag_cons = ".//Parameters/valuesConstant"
  fb_values_tags = c(xml_find_all(fb_element, xpath = name_tag_scal), 
                     xml_find_all(fb_element, xpath = name_tag_cons))
  
  for(values in fb_values_tags){
    parameter_list = list()
    parameter_list$dataname = values %>% xml_attr("dataname")
    parameter_list$type = values %>% xml_attr("type")
    parameter_list$unit = values %>% xml_attr("unit")
    nr = 0
    
    if(parameter_list$type == "constant"){
      column_names = names(xml_attrs(xml_find_first(fb_element, xpath = ".//parameter")))
      for(parameter in xml_find_all(values, ".//parameter")){
        if(nr == 0){
          parameter_df = data.frame(val1 = as.character(xml_attr(parameter,"constantset")), 
                                    val2 = as.numeric(xml_attr(parameter,"value")),
                                    stringsAsFactors = FALSE)
        }else{
          parameter_df_temp = data.frame(val1 = as.character(xml_attr(parameter,"constantset")), 
                                         val2 = as.numeric(xml_attr(parameter,"value")),
                                         stringsAsFactors = FALSE)
          parameter_df = bind_rows(parameter_df,parameter_df_temp)            
        }
        nr = nr + 1  
      }
    }else if(parameter_list$type == "scalar"){
      column_names = names(xml_attrs(xml_find_first(fb_element, xpath = ".//parameter")))
      for(parameter in xml_find_all(values, ".//parameter")){
        if(nr == 0){
          parameter_df = data.frame(val1 = as.numeric(xml_attr(parameter,"min")), 
                                  val2 = as.numeric(xml_attr(parameter,"max")),
                                  stringsAsFactors = FALSE)
        }else{
          parameter_df_temp = data.frame(val1 = as.numeric(xml_attr(parameter,"min")), 
                                       val2 = as.numeric(xml_attr(parameter,"max")),
                                       stringsAsFactors = FALSE)
          parameter_df = bind_rows(parameter_df,parameter_df_temp)            
        }
        nr = nr + 1
      }
    }else{
      stop(paste("type ",as.character(parameter_list$type)," is not available.", sep = ""))
    }
    colnames(parameter_df) <- column_names
    parameter_list$data = parameter_df
    rule_list$parameters = c(rule_list$parameters,list(parameter_list))
  }
  
  return(rule_list)
}


#' gets element model description from autoecology xml object
#'
#' @param ae autoecology xml object
#' @modelname name of model
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
#' @modelname name of model
#' @parem systemname name of system 
#' @return An xml structure containing ecological knowledge rules for a species
#' @examples
scan <- function(ae, modelname, systemname){
  if(length(get_element_parameter(ae)) > 0){
    parameter_root = get_element_parameter(ae)
    commonname = parameter_root %>% xml2::xml_find_all(".//CommonNames") %>%
                  xml_children()
    
    #Feedback
    print(paste0("Type of Topic : ","Parameter"))
    
    
  }else if(length(get_element_species(ae)) > 0){
    species_root = get_element_species(ae)
    latinname = species_root %>% xml2::xml_find_all(".//LatName") %>% xml_text()
    commonnames = species_root %>% xml2::xml_find_all(".//CommonNames") %>%
                    xml_children()
    
    #Feedback
    print(paste0("Type of Topic : ","Species"))
    print(paste0("Latin name : ",latinname))
   
    
  }else if(length(get_element_habitats(ae)) > 0){
    habitats_root = get_element_habitats(ae)
    commonnames = habitats_root %>% xml2::xml_find_all(".//CommonNames") %>%
                    xml_children()
    
    #Feedback
    print(paste0("Type of Topic : ","Habitat"))
    
    
  }else if(length(get_element_wfdindicator(ae)) >0){
    wfdind_root = get_element_wfdindicator(ae)
    commonnames = wfdind_root %>% xml2::xml_find_all(".//CommonNames") %>%
                    xml_children()
    
    #Feedback
    print(paste0("Type of Topic : ","WFDindicator"))
    
    
  }else{
    stop(paste0("Unknown topic presented. Valuable topics are 'Parameter',",
                 "'Species','Habitats' and 'WFDindicator'.\n",
                 "Please check the XML file"))
  }
  
  #Feedback commonnames
  for(name in commonnames){
    print(paste0("Commonname : language = ",xml_attr(name, "language"),", name = ",xml_attr(name, "name")))
  }
  
  type_tag_krs = get_element_knowledgerules(ae, model= modelname, system = systemname)
  
  #create flowdiagram
  
  
  #store knowledge rules
  
  
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



