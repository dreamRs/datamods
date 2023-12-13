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
  var sel = data.selector;
  if (data.hasOwnProperty("id")) {
    sel = "#" + $.escapeSelector(data.id);
  }
  if (data.inline) {
    $(sel).addClass("show-inline");
    $(sel).removeClass("hidden");
  } else {
    $(sel).addClass("show");
    $(sel).removeClass("hidden");
  }
});
Shiny.addCustomMessageHandler("datamods-hideUI", function(data) {
  var sel = data.selector;
  if (data.hasOwnProperty("id")) {
    sel = "#" + $.escapeSelector(data.id);
  }
  if (data.inline) {
    $(sel).addClass("hidden");
    $(sel).removeClass("show-inline");
  } else {
    $(sel).addClass("hidden");
    $(sel).removeClass("show");
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
    $(el[0]).addClass("disabled");
  }
}
Shiny.addCustomMessageHandler("datamods-disableTab", disableTab);

function enableTab(data) {
  var el = $("#" + data.id).find("[data-value='" + data.value + "']");
  if (typeof el[0] != "undefined") {
    $(el[0]).attr("data-toggle", "tab");
    $(el[0]).parent().removeClass("disabled");
    $(el[0]).removeClass("disabled");
  }
}
Shiny.addCustomMessageHandler("datamods-enableTab", enableTab);

