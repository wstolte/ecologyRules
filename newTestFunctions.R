
source("ae.R")

knowledgeRuleDir <- "d:\\Projects\\check_outs\\KnowledgeRules\\_knowledgerules\\"
knowldedgeRuleDir <- "data/_knowledgerules"

# schema <- xml2::read_xml("n:/Projects/11203500/11203758/B. Measurements and calculations/005- habitat modeling/EcologischeDatabase/github_setup/XMLSchema/AutecologyXML.xsd")
testfile = file.path(knowldedgeRuleDir, "species", "Fish", "Barbus barbus.xml")
testfile = file.path(knowldedgeRuleDir, "species", "Macrofytes", "Chara spp.xml")
#testfile = file.path(knowledgeRuleDir, "species", "Fish", "Barbus barbus.xml")
#testfile = file.path(knowledgeRuleDir, "species", "Molluscs","Dreissena_polymorpha.xml")
# testfile = file.path(knowledgeRuleDir, "species", "Macrophytes","Chara spp.xml")
# n:\Projects\11203500\11203758\B. Measurements and calculations\005- habitat modeling\EcologischeDatabase\github_setup\_knowledgerules\species\Macrofytes\Chara spp.xml
# d:\Projects\EcologischeKennisregels\ecoRules\data\_knowledgerules\species\Fish\Barbus barbus.xml

ae <- read_ae_xml(testfile)
# xml2::xml_validate(ae, schema) # does not work anymore, because the xml is stripped from namespaces in the read function

# xml_ns_strip(ae) # strip from namespaces, now included in read_ae_xml function

get_element_topic(ae) #%>% View()
get_element_species(ae) #%>% View()
get_element_contentdescription(ae)
get_element_speciesname(ae) # moet geen element zijn
get_element_ModelType(ae)
get_element_ModelTypeNames(ae)

get_system_names(ae, "HSI")
get_element_system(ae, "HSI")

system = get_element_system(ae, "HSI")[[1]] %>% xml2::xml_attrs() %>% unlist() %>% unname()

get_element_knowledgerules(ae = ae, modeltype = "HSI", system = system) 
get_element_knowledgerules(ae = ae, modeltype = "HSI", system = system) %>%
xml2::xml_children() %>% xml2::xml_attrs() %>% unlist() %>% unname()

get_all_knowledgeruleNames(ae = ae, modeltype = "HSI", system = system) %>% data.frame()


a <- get_type_knowledgeruleNames(ae = ae, modeltype = "HSI", system = system) 
b <-  split(a, a$name)
View(b)
lapply(b, function(x) paste(x$name))



#=================================================


#  //ResponseCurve[@*[contains(.,\"NA\")]]

# zoeken op basis van attribuutwaarden
xml2::xml_find_first(ae, ".//Autecology/ModelType[@*[contains(.,\"HSI\")]]")
HSIpath <- ae_xpath_attr_build(".//Autecology/ModelType","HSI")
xml2::xml_find_first(ae, HSIpath)

xml2::xml_find_first(ae, ".//Autecology/ModelType/System[@*[contains(.,\"Voortplanting\")]]")
systempath <- ae_xpath_attr_build(".//Autecology/ModelType/System","Voortplanting")
xml2::xml_find_first(ae, systempath)

system = "Voortplanting Barbeel (grindbedden)"
modeltype = "HSI"

nametag = ae_xpath_attr_build(".//Autecology/ModelType/System", system)
xml2::xml_find_first(ae, nametag)

nametag2 = ae_xpath_attr_build(".//System", system)
get_element_system(ae, "HSI") %>% xml2::xml_parent() %>% xml2::xml_find_first(nametag2) 



get_element_system(ae)

xml2::xml_find_all(ae, ".//Autecology/ModelType/System")


a <- get_element_ModelType(ae)[which(get_element_ModelType(ae) %>% xml2::xml_attrs() %>% map_chr("name") == "HSI")] %>% 
  xml2::xml_find_all(xpath = ".//System") %>%
  xml2::xml_attrs() %>% map_chr("name")

rule = "Larven en juvenielen Barbeel (oeverzones)"
which(a == rule)


get_element_ModelType(ae) %>% as_list() %>% str(max.level = 3)

get_element_ModelType(ae) %>% xml2::xml_find_first(xpath = "System")

get_element_knowledgerules(ae) %>% as_list() %>% str(max.level = 3)

get_element_ModelType(ae)[1] %>% xml_attr(attr = "name")

get_element_knowledgerules(ae) %>%  xml2::xml_children() %>% xml2::xml_attrs() %>% unlist() %>% unname

get_knowledgerule_attributes(ae)
get_knowledgerule_stage(ae)
get_responsecurve_keys(ae, "testmodel") #is dit de bedoeling?



#####################MARC##########################

system_names = get_element_ModelType(ae) %>% xml2::xml_find_all(xpath = "System") %>% 
                xml_attr(attr = "name")
system = get_element_ModelType(ae) %>% xml2::xml_find_all(xpath = "System")
knowledgeRules = system[[1]] %>% xml2::xml_find_first(xpath = "KnowledgeRules")
#response_curve = get_element_response_curve(ae, "adult", "Chloride")
#response_curve = system[[1]] %>% xml2::xml_find_first(xpath = ".//KnowledgeRules/ResponseCurve")
data_response_curve = get_data_response_curve(response_curve)

formula_based = system[[1]] %>% xml2::xml_find_all(xpath = ".//KnowledgeRules/FormulaBased")     
data_formula_based = get_data_formula_based(formula_based[[2]])








###############MARC##############################
# change to real values
myknowledgerule = "testmodel"
myresponsecurve = get_responsecurve_keys(ae, "testmodel")[2]
get_element_functionalresponsecurves(ae, myknowledgerule, myresponsecurve)



# xml2::xml_attr(get)
# 

xml2::xml_find_first(ae, xpath = './/ResponseCurve[@* = "Substraattype"]')   #%>%  xml2::xml_attrs()

xml2::xml_find_first(ae, xpath = './/ResponseCurve[@* = "Substraattype"]')

myknowledgerule = "Substraattype"



# get_element_knowledgerule(ae, )

type_tag_krs <- xml_find_all(ae, knowledgerules_path)

xml_find_all(ae, ".//KnowledgeRule")
type_tag_krs <- xml_find_all(ae, ".//KnowledgeRules")

xml_find_all(ae, ".//stage") # not 
xml_find_all(ae, ".//Model")

xml2::xml_attr(xml2::xml_child(xml2::xml_find_all(ae, ".//Model")))


