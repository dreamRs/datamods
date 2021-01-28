/*!
 * Copyright (c) 2020 dreamRs
 *
 * datamods, JavaScript utilities
 * https://github.com/dreamRs/datamods
 *
 * @version 0.0.1
 */

/*jshint
  jquery:true
*/
/*global Shiny */

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


function fadeTab(data) {
  var tabId = $("#" + data.id).attr("data-tabsetid");
  $("#" + data.id)
    .parent()
    .find(".tab-pane")
    .each(function(index) {
      if ($(this).parent().attr("data-tabsetid") == tabId) {
        $( this ).addClass("fade");
        if (index < 1) {
          $( this ).addClass("in");
        }
      }
    });
}

function updateTabLabel(data) {
  var el = $("#" + data.id).find("[data-value='" + data.value + "']");
  if (typeof el[0] != "undefined") {
    $(el[0]).html(data.label);
  }
}
Shiny.addCustomMessageHandler("datamods-updateTabLabel", updateTabLabel);

function disableTab(data) {
  var el = $("#" + data.id).find("[data-value='" + data.value + "']");
  if (typeof el[0] != "undefined") {
    $(el[0]).removeAttr("data-toggle");
    $(el[0]).parent().addClass("disabled");
  }
}
Shiny.addCustomMessageHandler("datamods-disableTab", disableTab);
Shiny.addCustomMessageHandler("datamods-enableTab", function(data) {
  var el = $("#" + data.id).find("[data-value='" + data.value + "']");
  if (typeof el[0] != "undefined") {
    $(el[0]).attr("data-toggle", "tab");
    $(el[0]).parent().removeClass("disabled");
  }
});

