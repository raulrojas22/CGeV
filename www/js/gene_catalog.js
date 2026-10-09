(function () {
  "use strict";

  function shinyInput(name, value) {
    if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
      window.Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  var floatingTooltip = null;

  function hideCatalogTooltip() {
    if (floatingTooltip && floatingTooltip.parentNode) {
      floatingTooltip.parentNode.removeChild(floatingTooltip);
    }
    floatingTooltip = null;
  }

  function showCatalogTooltip(target) {
    hideCatalogTooltip();
    var text = String(target.getAttribute("data-tooltip") || "").trim();
    if (!text) return;
    floatingTooltip = document.createElement("div");
    floatingTooltip.className = "catalog-floating-tooltip";
    floatingTooltip.setAttribute("role", "tooltip");
    floatingTooltip.textContent = text;
    document.body.appendChild(floatingTooltip);
    var anchor = target.getBoundingClientRect();
    var tip = floatingTooltip.getBoundingClientRect();
    var left = Math.max(12, Math.min(window.innerWidth - tip.width - 12, anchor.left + anchor.width / 2 - tip.width / 2));
    var top = anchor.bottom + 8;
    if (top + tip.height > window.innerHeight - 12) top = Math.max(12, anchor.top - tip.height - 8);
    floatingTooltip.style.left = left + "px";
    floatingTooltip.style.top = top + "px";
  }

  document.addEventListener("mouseover", function (event) {
    var help = event.target && event.target.closest ? event.target.closest(".catalog-column-help[data-tooltip]") : null;
    if (help) showCatalogTooltip(help);
  });
  document.addEventListener("mouseout", function (event) {
    var help = event.target && event.target.closest ? event.target.closest(".catalog-column-help[data-tooltip]") : null;
    if (help) hideCatalogTooltip();
  });
  document.addEventListener("focusin", function (event) {
    var help = event.target && event.target.closest ? event.target.closest(".catalog-column-help[data-tooltip]") : null;
    if (help) showCatalogTooltip(help);
  });
  document.addEventListener("focusout", function (event) {
    var help = event.target && event.target.closest ? event.target.closest(".catalog-column-help[data-tooltip]") : null;
    if (help) hideCatalogTooltip();
  });

  document.addEventListener("click", function (event) {
    var button = event.target && event.target.closest
      ? event.target.closest("[data-catalog-action]")
      : null;
    if (!button) return;
    event.preventDefault();
    event.stopPropagation();
    var action = String(button.getAttribute("data-catalog-action") || "");
    if (action === "multigene") {
      shinyInput("catalog_open_multigene", {
        species_id: String(button.getAttribute("data-species-id") || ""),
        gene: String(button.getAttribute("data-gene") || ""),
        nonce: Date.now()
      });
    }
  });

  if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
    window.Shiny.addCustomMessageHandler("cgv:catalog-open-multigene", function (payload) {
      var speciesId = String((payload && payload.species_id) || "");
      var gene = String((payload && payload.gene) || "");
      shinyInput("preloaded_species_homo", speciesId);
      window.setTimeout(function () {
        shinyInput("catalog_multigene_ready", {
          species_id: speciesId,
          gene: gene,
          request_id: payload.request_id,
          nonce: Date.now()
        });
      }, 220);
    });

    window.Shiny.addCustomMessageHandler("cgv:catalog-open-cross", function (payload) {
      var speciesIds = Array.isArray(payload && payload.species_ids) ? payload.species_ids : [];
      var gene = String((payload && payload.gene) || "");
      shinyInput("preloaded_species_ortho", speciesIds);
      window.setTimeout(function () {
        shinyInput("catalog_cross_ready", {
          species_ids: speciesIds,
          gene: gene,
          request_id: payload.request_id,
          nonce: Date.now()
        });
      }, 260);
    });
  }
})();
