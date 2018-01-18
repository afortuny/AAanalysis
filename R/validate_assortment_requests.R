validate_assortment_requests <- function(technical_settings, analytical_settings, spark_context) {

  report_progress_top <- report_progress_factory()

  report_progress_top$subarea('technical')$info(utils::capture.output(utils::str(technical_settings)))
  report_progress_top$subarea('analytical')$info(utils::capture.output(utils::str(analytical_settings)))

  request_set <- read_requests(
    technical_settings,
    spark_context,
    report_progress_top$subarea('read'),
    STATUS_SUBMITTED
  )

  for (r in seq_len(nrow(request_set))) {
    request <- request_set[r,]

    #FIXME: do real validation of the request here

    # Things to check:
    # - article id list is not empty
    # - article ids are correct
    # - season is correct
    # - market is correct
    # - prod_division is correct

    update_request_status(
      technical_settings,
      spark_context,
      report_progress_top$subarea('update'),
      request[[technical_settings$field$request]],
      STATUS_VALIDATED,
      description = ' '
    )
  }
}
