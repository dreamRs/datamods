/*!
 * Copyright (c) 2020 dreamRs
 *
 * datamods, JavaScript utilities
 * https://github.com/dreamRs/datamods
 *
 * @version 0.0.1
 */

// Block or unblock an input widget
Shiny.addCustomMessageHandler("datamods-toggleWidget", function(data) {
  $("#" + data.id).prop("disabled", !data.enable);
  if ($("#" + data.id).hasClass("selectpicker")) {
    $("#" + data.id).selectpicker("refresh");
  }
});

// Hide or show UI component
Shiny.addCustomMessageHandler("datamods-showUI", function(data) {
  if (data.inline) {
    $(data.selector).addClass("show-inline");
    $(data.selector).removeClass("hidden");
  } else {
    $(data.selector).addClass("show");
    $(data.selector).removeClass("hidden");
  }
});
Shiny.addCustomMessageHandler("datamods-hideUI", function(data) {
  if (data.inline) {
    $(data.selector).addClass("hidden");
    $(data.selector).removeClass("show-inline");
  } else {
    $(data.selector).addClass("hidden");
    $(data.selector).removeClass("show");
  }
});
