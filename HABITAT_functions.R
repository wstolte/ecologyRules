
# Calculate a response curve
#' calculates the output value by interpolation for a response curve
#' based on input value.
#' 
#' @param value a numeric input value
#' @param var_min minimum allowed numeric input value
#' @param var_max maximum allowed numeric input value
#' @param var_vector vector containing x-axis values
#' @param suit_vector vector containing y-axis values 
#' @return A numeric output value
#' @examples
#' calc_response_curve(value = 15, var_min = 0, var_max = 200,
#'        var_vector = c(0,10,25,75,175,200), suit_vector = c(0,0,1,1,0,0))

calc_response_curve <- function(value, var_min, var_max, var_vector, 
                                suit_vector, log = stdout()){
  if(value < var_min){
    suit_value = head(suit_vector, n = 1)
    writeLines(paste0("lower than minimum value ", as.character(var_min),
    " : ",as.character(value)), con = log)
  }else if(value >= var_max){
    suit_value = tail(suit_vector, n = 1)
    writeLines(paste0("higher than maximum value ", as.character(var_max),
                  " : ",as.character(value)), con = log)
  }else{
    
    ind = which(var_vector <= value)
    #ind = 0
    #nr = 0
    #for(item in var_vector){
    #  nr = nr + 1
    #  if(value  >= item){
    #    ind = c(ind, nr) 
    #  }
    #  if(value < item){
    #    break()
    #  }
    #}
    ind_nr = tail(ind, n=1)
    v1 = var_vector[ind_nr]
    v2 = var_vector[ind_nr + 1]
    diff_var = v2 - v1
    d1 = value - v1
    d2 = v2 - value
    suit_value = suit_vector[ind_nr] * d2 / diff_var + 
      suit_vector[ind_nr + 1] * d1 / diff_var
  }
  return(suit_value)
}

# Calculates the output vector based on a response curve
#' calculates the output value by interpolation for a response curve
#' based on input value.
#' 
#' @param input_vector a numeric vector
#' @param var_min minimum allowed numeric input value
#' @param var_max maximum allowed numeric input value
#' @param var_vector vector containing x-axis values
#' @param suit_vector vector containing y-axis values 
#' @return A numeric vector
#' @examples
#' calc_vector_response_curve(c(1,2,3,20), var_min = 0, var_max = 200,
#'        var_vector = c(0,10,25,75,175,200), suit_vector = c(0,0,1,1,0,0))

calc_vector_response_curve <- function(input_vector, var_min, var_max,
                                       var_vector, suit_vector, log = stdout()){
  output_vector = sapply(input_vector, FUN = function(x) calc_response_curve(x,
                          var_min = var_min, var_max = var_max, 
                          var_vector = var_vector,
                          suit_vector = suit_vector, log = log))
  return(output_vector)
}

# Calculates the output raster based on a response curve
#' calculates the output value by interpolation for a response curve
#' based on input value.
#' 
#' @param input_raster a Raster* object
#' @param var_min minimum allowed numeric input value
#' @param var_max maximum allowed numeric input value
#' @param var_vector vector containing x-axis values
#' @param suit_vector vector containing y-axis values 
#' @return A Raster* object
#' @examples
#' require(raster)
#' input_raster <- raster(ncols = 3, nrows = 3)
#' input_raster[] <- c(0,0,0,20,20,20,200,300,400)
#' plot(input_raster)
#' output_raster <- calc_raster_response_curve(input_raster, var_min = 0, var_max = 200,
#'                       var_vector = c(0,10,25,75,175,200), suit_vector = c(0,0,1,1,0,0))
#' plot(output_raster)

calc_raster_response_curve <- function(input_raster, var_min, var_max,
                                       var_vector, suit_vector, log = stdout()){
  require(raster)

  #function_used <- function(..., var_min, var_max, 
  #                          var_vector,
  #                          suit_vector, log){ 
  #                calc_vector_response_curve(input_vector = x,
  #                             var_min = var_min, var_max = var_max, 
  #                             var_vector = var_vector,
  #                             suit_vector = suit_vector, log = log)
  #}
  #output_raster = raster::calc(input_raster, fun = function(x){ 
  #                              function_used(x, var_min = var_min, 
  #                                var_max = var_max, var_vector = var_vector,
  #                                suit_vector = suit_vector, log = log)  } )
  
  #could not get previous version with calc to work properly, needs further investigation
  output_raster = input_raster
  values_no_na <- values(input_raster)[!is.na(values(input_raster))]
  output_values <- calc_vector_response_curve(input_vector = values_no_na,
                                  var_min = var_min, var_max = var_max, 
                                  var_vector = var_vector,
                                  suit_vector = suit_vector, log = log)
  values(output_raster)[!is.na(values(input_raster))] <- output_values
  
  return(output_raster)
}

