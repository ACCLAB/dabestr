# load_plot_data <- function(contrasts, effect_size, contrast_type)
#   {
# default value for effect_size = "mean_diff"
# default value for contrast_type = "delta2"

#     effect_attr_map <- list(
#         mean_diff = "mean_diff",
#         median_diff = "median_diff",
#         cliffs_delta = "cliffs_delta",
#         cohens_d = "cohens_d",
#         hedges_g = "hedges_g",
#         delta_g = "delta_g"
#     )

#     contrast_attr_map <- list(
#         delta2 = "delta_delta",
#         mini_meta = "mini_meta_delta"
#     )

#     effect_attr <- effect_attr_map[[effect_size]]
#     contrast_attr <- contrast_attr_map[[contrast_type]]

#     if (is.null(effect_attr)) {
#         stop(paste0("Invalid effect_size: ", effect_size))
#     }
#     if (is.null(contrast_attr)) {
#         stop(paste0("Invalid contrast_type: ", contrast_type, ". Available options: [delta2, mini_meta]"))
#     }

#     # get effectsize attributes
#     es_attributes <- list()
#     for (i in seq_along(contrasts())){
#         contrast <- contrasts[i]
#         contrast[[effect_attr]]
#     }

#     return [
#         getattr(getattr(contrast, effect_attr), contrast_attr) for contrast in contrasts
#     ]
# }
