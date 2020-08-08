/*!
 * Copyright (c) 2020 dreamRs
 *
 * datamods, JavaScript utilities
 * https://github.com/dreamRs/datamods
 *
 * @version 0.0.1
 */

Shiny.addCustomMessageHandler("datamods-toggleWidget", function(data) {
  $("#" + data.id).prop("disabled", !data.enable);
  if ($("#" + data.id).hasClass("selectpicker")) {
    $("#" + data.id).selectpicker("refresh");
  }
});

