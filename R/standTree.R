##' standTree
##' 
##' @param Data data.table tttt
##' @param Tree data.table tttt
##' @param elements vector tttt
##' @param standParas list tttt
##' @param zeroWeight vector co-product
##'
##'  @export

standTree = function(data, tree, elements,standParams, zeroWeight){
  
  
  ## Assign parameters
  geoVar = standParams$geoVar
  yearVar = standParams$yearVar
  itemVar = standParams$itemVar
  childVar = standParams$childVar
  parentVar = standParams$parentVar
  extractVar = standParams$extractVar
  
  ## Data Quality Checks
  stopifnot(is(data, "data.table"))
  stopifnot(is(tree, "data.table"))
  stopifnot(c(geoVar, yearVar, itemVar,  elements) %in% colnames(data))
  ## I am currently NOT using country-year specific commodity tree, but in the next future we should!  
  ##stopifnot(c(geoVar, yearVar, childVar, parentVar, extractVar, shareVar)
  ##          %in% colnames(tree))
  if(!all(sapply(data[, c(elements), with = FALSE],
                 is.numeric))){
    stop("Some of the elements passed are not numeric!")
  }
  ## Restructure the data for easier standardization
  standardizationData = data.table::melt.data.table(
    data = data, measure.vars = elements,
    id.vars = c(geoVar, yearVar, itemVar),
    variable.name = "measuredElement", value.name = "Value")
  
  
  ## To ensure commodities are standardized up multiple levels, we have to
  ## collapse the tree (otherwise if A -> B -> C in the tree, C may be
  ## standardized only to B and not to A, as desired).
  standKey = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
  if(dim(tree)[1]!=0){
  ##tree = collapseEdges(edges = tree, parentName = standParams$parentVar,
  ##                     childName = standParams$childVar,
  ##                     extractionName = standParams$extractVar,
  ##                     keyCols = standKey)
  
  ## Merge the tree with the node data
    tree[, c(parentVar, childVar) :=
           list(as.character(get(parentVar)), as.character(get(childVar)))]
  }
  setnames(standardizationData, itemVar, childVar)
  standardizationData[, c(childVar, yearVar, geoVar) :=list(as.character(get(childVar)),
                                                            as.character(get(yearVar)),
                                                            as.character(get(geoVar)))]
                        
  
  # To deal with joint byproducts
  standardizationData = merge(standardizationData, tree,
                              by = c(childVar),
                              all.x = TRUE, allow.cartesian = TRUE)
  
  # If an element is not a child in the tree, then "standardize" it to
  # itself with a rate of 1 and a share of 1.
  standardizationData[is.na(get(parentVar)),
                      c(parentVar, extractVar) := list(get(childVar), 1)]
  # Extraction rates of zero will cause us to divide by zero, so we must
  # remove them
  extract0 <- standardizationData[abs(get(extractVar)) < .Machine$double.eps ^ .5]
  if(nrow(extract0) > 0){
    # Check for tricky floating point issues, but checking if equal to 0
    warning(sprintf("Extraction rates of 0 present in commodity codes: {%s} in country {%s}.
                      Ignoring all extraction rates of 0 in backwards standardization",
                    paste0(unique(extract0[, get(parentVar)]), collapse = ", "), unique(extract0[, get(geoVar)])))
    standardizationData[abs(get(extractVar)) < .Machine$double.eps^.5, Value := NA]
  }
  output = standardizationData[, list(
    Value = sum( Value*weight/get(extractVar), na.rm = TRUE)),
    by = c(yearVar, geoVar,
           "measuredElement", parentVar)]
  
  ## Reshape to put back into the same shape as the passed data
  setnames(output, parentVar, itemVar)
  
  form = as.formula(paste(yearVar, "+", geoVar, "+", itemVar, "~ measuredElement"))
  output = dcast.data.table(data = output, formula = form, value.var = "Value",
                            fun.aggregate = mean, na.rm = TRUE)
  return(output)
}

