#' Helper functions that deal with assignment of colour palettes for the overall plots
#' 
#' @description
#' Contains function `apply_palette`.

# Applies palettes to <ggplot> objects
apply_palette <- function(ggplot_object, palette_name) {
  ggplot_object <- switch(
    palette_name,
    
    "npg" =
      ggplot_object + scale_color_npg() + scale_fill_npg(),
    
    "aaas" = 
      ggplot_object + scale_color_aaas() + scale_fill_aaas(),
    
    "nejm" = 
      ggplot_object + scale_color_nejm() + scale_fill_nejm(),
    
    "lancet" = 
      ggplot_object + scale_color_lancet() + scale_fill_lancet(),
    
    "jama" = 
      ggplot_object + scale_color_jama() + scale_fill_jama(),
    
    "jco" = 
      ggplot_object + scale_color_jco() + scale_fill_jco(),
    
    "ucscgb" = 
      ggplot_object + scale_color_ucscgb() + scale_fill_ucscgb(),
    
    "d3" = 
      ggplot_object + scale_color_d3() + scale_fill_d3(),
    
    "locuszoom" = 
      ggplot_object + scale_color_locuszoom() + scale_fill_locuszoom(),
    
    "igv" = 
      ggplot_object + scale_color_igv() + scale_fill_igv(),
    
    "cosmic" = 
      ggplot_object + scale_color_cosmic() + scale_fill_cosmic(),
    
    "uchicago" = 
      ggplot_object + scale_color_uchicago() + scale_fill_uchicago(),
    
    "brewer" = 
      ggplot_object + scale_color_brewer() + scale_fill_brewer(),
    
    "ordinal" = 
      ggplot_object + scale_color_ordinal() + scale_fill_ordinal(),

    "viridis_d" = 
      ggplot_object + scale_color_viridis_d() + scale_fill_viridis_d()
  )
  
  return(ggplot_object)
}