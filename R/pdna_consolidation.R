#' PDNA Consolidation
#'
#' It is responsible for all the data manipulation done in Spark.
#' For time purposes the sparklyr package is used and makes the procedure in parallel programming instead of localy on one personal PC.
#'
#' #' requires sparklyr & dplyr package
#'
#' @param technical_settings The required parameters to set for Spark
#' @param analytical_settings The required parameters for the assortment analysis
#' @param sc It provides a remote dplyr data source to the Spark cluster
#' @param articles_in_scope articles ids to process
#'
#' @return A list of data frames. (a) data_table with the raw data of the color & material values for each main part and location of a footwear (b) v_article_full which is a table with the rest attributes for the same articles from the BDP. Those attributes include the gender, age, business unit and so on.
#'
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#'
pdna_consolidation <- function(technical_settings,
                               analytical_settings,
                               sc,
                               articles_in_scope) {

  apdm_bom <- dplyr::tbl(sc, dplyr::sql(analytical_settings$sql_txt_apdm_bom))
  apdm_template <- dplyr::tbl(sc, dplyr::sql(analytical_settings$sql_txt_apdm_template))
  color_grp <- dplyr::tbl(sc, dplyr::sql(analytical_settings$sql_txt_rfd_color_stg1))

  #make appropriate renamings (Note: dplyr::rename (v0.7.4) in "remote mode" does not work with multiple renames)
  final_apdm_bom <-
    apdm_bom %>%
      dplyr::rename('article_no'      := 'article') %>%
      dplyr::rename('season_cd'       := 'season') %>%
      dplyr::rename('factory_cd'      := 'production_factory') %>%
      dplyr::rename('part_name_old'   := 'part_name') %>%
      dplyr::rename('materialclass'   := 'material_class') %>%
      dplyr::rename('adidas_color_cd' := 'adidas_color_code')

  final_apdm_template <-
    apdm_template %>%
      dplyr::rename('tmpl_part_name'  := 'part_name') %>%
      dplyr::rename('tmpl_part_no'    := 'part_number')

  final_apdm_bom_with_locations <-
    final_apdm_bom %>%
      #bring the location id, part area
      dplyr::inner_join(final_apdm_template, by =  c("part_number" = "tmpl_part_no")) %>%

      #bring the color description
      dplyr::left_join(color_grp, by = c("adidas_color_cd" = "color_cd")) %>%

      #Keep only main parts and drop sub parts
      dplyr::filter(rlang::sym('part_name_old') == rlang::sym('tmpl_part_name')) %>%

      #Create season variable into numeric format
      dplyr::mutate(
        season = ifelse(
          substring(rlang::sym('season_cd'), 1, 2) == 'FW',
          paste(substring(rlang::sym('season_cd'), 3, 6), '2', sep = ''),
          paste(substring(rlang::sym('season_cd'), 3, 6), '1', sep = '')
        )
      )

  #Keep the attributes of color and material only for the articles in scope
  pdna_consolidation <-
    final_apdm_bom_with_locations %>%
      dplyr::inner_join(
        articles_in_scope,
        by = c("article_no" = "article_id")
      ) %>%
      dplyr::rename('part_name_loc' := 'part_location') %>%
      dplyr::rename('part_no_other' := 'part_number') %>%
      dplyr::rename('part_name'     := 'tmpl_part_name') %>%
      dplyr::rename('material'      := 'materialclass') %>%
      dplyr::rename('color'         := 'color_grp') %>%
      dplyr::select(
        rlang::UQS(rlang::syms(c(
          'article_no',
          'season',
          'part_name_loc',
          'part_no_other',
          'part_name',
          'color',
          'material'
        )))
      )

  return(
    pdna_consolidation %>% dplyr::collect()
  )
}
