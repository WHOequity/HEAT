$(function () {

  // $(document).ready(function () {
  //   $(".chip .chip-content").map(function (x, i) {
  //     var txt = $(i).html().replaceAll("&nbsp;", "");
  //     console.log(txt);
  //     $(i).html(txt);
  //   });
  // });

  $("nav").toggleClass("navbar-expand-xl navbar-expand-custom");

  Shiny.addCustomMessageHandler("modal-stay-open", function (message) {
    $("body").addClass("modal-open")
  })


Shiny.addCustomMessageHandler("enable_determinants", function(message){

  if(message['is_annual'] === true){
    $("button[value='determinants']").removeClass("disabled");
    $("button[value='determinants']").prop('title', '');
  } else {
    $("button[value='determinants']").addClass("disabled");
    $("button[value='determinants']").prop('title', 'Determinants are only shown for datasets with annual data');
  }

})


Shiny.addCustomMessageHandler("hide-map-button", function(message){


  if(message['display'] === 'true'){
    $("input[value='map'][name='heat-nav_explore_disag']").parent().removeClass("hidestuff")
  } else {
    $("input[value='map'][name='heat-nav_explore_disag']").parent().addClass("hidestuff")
  }

})

  checkForChart();


  Shiny.addCustomMessageHandler("make-invalid-dimension", function (message) {
    add_class_invalid("#heat-explore_disag_line-dimension", message);
    add_class_invalid("#heat-explore_disag_bar-dimension", message);
    add_class_invalid("#heat-explore_disag_table-dimension", message);
    add_class_invalid("#heat-explore_disag_detail-dimension", message);
    add_class_invalid("#heat-explore_summary_bar-dimension", message);
    add_class_invalid("#heat-explore_summary_line-dimension", message);
    add_class_invalid("#heat-explore_summary_table-dimension", message);
    add_class_invalid("#heat-compare_disag_graph-dimension", message);
    add_class_invalid("#heat-compare_disag_table-dimension", message);
    add_class_invalid("#heat-compare_summary_graph-dimension", message);
    add_class_invalid("#heat-compare_summary_table-dimension", message);
  });

  Shiny.addCustomMessageHandler("make-invalid-year", function (message) {
    add_class_invalid("#heat-explore_disag_line-year", message);
    add_class_invalid("#heat-explore_disag_bar-year", message);
    add_class_invalid("#heat-explore_disag_map-year", message);
    add_class_invalid("#heat-explore_disag_detail-year", message);
    add_class_invalid("#heat-explore_summary_bar-year", message);
    add_class_invalid("#heat-explore_summary_line-year", message);
    add_class_invalid("#heat-explore_summary_table-year", message);
    add_class_invalid("#heat-compare_disag_table-year", message);
    add_class_invalid("#heat-compare_disag_graph-year", message);
    add_class_invalid("#heat-compare_summary_graph-year", message);
    add_class_invalid("#heat-compare_summary_table-year", message);
    add_class_invalid("#heat-determinant_graph-year", message);
    add_class_invalid("#heat-determinant_table-year", message);
  });

    Shiny.addCustomMessageHandler("make-invalid-determinant", function (message) {
      add_class_invalid("#heat-determinant_graph-determinant", message);
      add_class_invalid("#heat-determinant_table-determinant", message);
  });

  Shiny.addCustomMessageHandler("chart-timer", function (message) {
      checkForChartTimerDebug();
  });


  $("#heat-header-title").on("click", function () {
    $("#heat-nav>li>button:first").click();
  });
  Shiny.addCustomMessageHandler("heat:activate-plot-output", function (msg) {
    $(document.querySelectorAll(".heat-plot-output.active")).removeClass(
      "active"
    );
    document.getElementById(msg.id).classList.add("active");
  });

  $(document).on(
    "shiny:recalculated",
    ".heat-plot-output.active",
    function (e) {
      $(document.querySelectorAll(".heat-plot-output")).children().remove();
    }
  );

  $(document).on(
    "shiny:recalculating shiny:outputinvalidated shiny:error",
    ".heat-plot-output",
    function (e) {
      if (!e.currentTarget.querySelector(".plot-output__spinner")) {
        e.currentTarget.insertAdjacentHTML(
          "afterbegin",
          '<div class="spinner-border plot-output__spinner" role="status"><span class="sr-only">Loading...</span></div>'
        );
      }
    }
  );

  $(document).on("shiny:recalculated", ".heat-plot-output", function (e) {
    var spinner = e.currentTarget.querySelector(".plot-output__spinner");

    if (spinner) {
      spinner.parentNode.removeChild(spinner);
    }

    $(document.querySelectorAll(".highcharts-tooltip-container")).remove();
  });

  $(document).on(
    "shiny:recalculated shiny:visualchange resized.heat",
    ".heat-plot-output",
    function (e) {
      // Adjust legend heights
      var legends = document.querySelectorAll(
        ".heat-legend-container .highchart"
      );

      for (var i = 0; i < legends.length; i++) {
        var l = legends[i];

        if (!l) {
          continue;
        }

        var lbox = l.querySelector(".highcharts-legend-box");

        if (!lbox) {
          continue;
        }

        var h = lbox.getAttribute("height") + "px";

        l.style.height = h;

        var con = l.querySelector(".highcharts-container");
        con.style.height = h;

        var root = l.querySelector(".highcharts-root");
        root.removeAttribute("viewBox");
        root.setAttribute("height", h);
      }

      // Adjust chart titles
      var paneTitles = e.target.querySelectorAll(".heat-pane-title");

      if (!paneTitles.length) {
        return;
      }

      var titlesArray = Array.prototype.slice.call(paneTitles);
      var titlesHeights = titlesArray.map(function (t) {
        return t.children[0].offsetHeight;
      });
      var maxHeight = titlesHeights.reduce(function (a, b) {
        return Math.max(a, b);
      });

      titlesArray.forEach(function (title) {
        title.style.height = maxHeight + "px";
      });
    }
  );

  // resize event & adjust titles on resize
  var rtime;
  var timeout = false;
  var delta = 200;
  $(window).resize(function () {
    rtime = new Date();
    if (timeout === false) {
      timeout = true;
      setTimeout(resizeend, delta);
    }
  });

  function resizeend() {
    if (new Date() - rtime < delta) {
      setTimeout(resizeend, delta);
    } else {
      timeout = false;
      $(document.querySelectorAll(".heat-plot-output")).trigger("resized.heat");
    }
  }

  Shiny.addCustomMessageHandler("heat:download.image", function (msg) {
    if (msg.selector === undefined || msg.filename === undefined) {
      return;
    }

    var chart = document.querySelector(msg.selector);
    var width = msg.width || chart.offsetWidth * 1.035;
    var height = msg.height || chart.offsetHeight;

    if (!msg.height && chart.querySelector(".heat-chart-disclaimer")) {
      var disHeight = chart.querySelector(
        ".heat-chart-disclaimer"
      ).scrollHeight;
      height += disHeight * 1.5;
    }

    var legend = document.querySelector('.heat-legend-chart');
    var xtraHeight = (legend === null) ? 0 : legend.offsetHeight;
    height += xtraHeight;

    html2canvas(chart, {
      ignoreElements: function (el) {
        return /highcharts-tooltip/.test(el.className.baseVal);
      },
      onclone: function (doc) {
        doc.querySelector(msg.selector).style.paddingTop = "10px";

        var disclaimer = doc.querySelector(
          msg.selector + " .heat-chart-disclaimer"
        );

        if (disclaimer) {
          var legend = document.querySelector('.heat-legend-chart');
          var xtraHeight = (legend === null) ? 0 : legend.offsetHeight;

          disclaimer.style.height = "auto";
          disclaimer.style.visibility = "visible";

          disclaimer.style.paddingTop = xtraHeight + "px";
          disclaimer.style.paddingLeft = "10px";
          disclaimer.style.paddingLeft = "10px";

          var children = disclaimer.children;
          for (var i = 0; i < children.length; i++) {
            children[i].style.visibility = "visible";
          }
        }

        return doc;
      },
      logging: false,
      height: height,
      scale: 2, // msg.type == "pdf" ? 3 : 1.5,
      width: width,
    }).then(function (canvas) {
      saveImage(
        canvas.toDataURL(),
        msg.type,
        msg.filename,
        canvas.width,
        canvas.height
      );
    });
  });

  /*
   * TRANSLATIONS ----
   */
  var Backend = new window.i18nextXHRBackend();

  var i18nOpts = {
    fallbackLng: "en",
    ns: "default",
    defaultNS: "default",
    returnEmptyString: false,
    returnNull: false,
    backend: {
      loadPath: "heat-assets/locales/{{lng}}/{{ns}}.json",
      parse: function (x) {
        return JSON.parse(x);
      },
    },
  };

  var i18nPrep = function () {
    $(".yonder-chip").each(function () {
      var $chip = $(this);
      var $input = $chip.children("input").first();
      var ns = $chip.attr("data-i18n-ns") + ".";

      if (!$input.attr("data-i18n")) {
        $input.attr("data-i18n", "[placeholder]" + $input.attr("placeholder"));
      }
      $input.removeAttr("placeholder");

      $chip.find(".dropdown-item").each(function () {
        var $item = $(this);
        $item.attr("data-i18n", ns + $item.val());
      });

      $chip.find(".chip-content").each(function () {
        var $item = $(this);
        $item.attr("data-i18n", ns + $item.parent().val());
      });
    });

    $(".yonder-select").each(function () {
      var $select = $(this);
      var $input = $select.children("input").first();
      var ns = $select.attr("data-i18n-ns") + ".";

      if (!$input.attr("data-i18n")) {
        $input.attr("data-i18n", "[placeholder]" + $input.attr("placeholder"));
      }
      $input.removeAttr("placeholder");

      // $select.find(".dropdown-item").each(function() {
      //   var $item = $(this);
      //   $item.attr("data-i18n", ns + $item.val());
      // });
    });

    $(".yonder-nav").each(function () {
      var $nav = $(this);
      var ns = $nav.attr("data-i18n-ns") + ".";

      $nav.find(".nav-link, .dropdown-item").each(function () {
        var $link = $(this);
        var $label = $link.find(".nav-label").first();

        if ($label.length) {
          $label.attr("data-i18n", ns + $link.val());
        } else {
          $link.attr("data-i18n", ns + $link.val());
        }
      });
    });

    /*$(".yonder-checkbox").each(function() {
    var $checkbox = $(this);
    var ns = $checkbox.attr("data-i18n-ns") + ".";

    $checkbox.find("label").each(function() {
      var $link = $(this);
      var $input = $link.prev();

      $link.attr("data-i18n", ns + $input.val());
    });
  });*/

    $("[data-i18n-target]").each(function () {
      var $el = $(this);
      var ns = "";

      if ($el.attr("data-i18n-ns")) {
        ns = $el.attr("data-i18n-ns") + ".";
      }

      $el.find($el.attr("data-i18n-target")).each(function () {
        var $child = $(this);
        $child.attr("data-i18n", $el.attr("data-i18n-assign"));
      });
    });

    $("[data-i18n-plural]").each(function () {
      var $el = $(this);
      var opts = JSON.parse($el.attr("i18n-opts") || null) || {};

      opts.count = 2;
      $el.attr("i18n-opts", JSON.stringify(opts));
    });
  };

  var i18nPost = function () {
    $(".yonder-select").each(function () {
      var $select = $(this);
      var $input = $select.children("input").first();
      var $active = $select.find(".dropdown-item.active");

      $input.attr("data-original-placeholder", $input.attr("placeholder"));

      if ($active.length) {
        $input.attr("placeholder", $active.text());
      }
    });
  };

  i18next.use(Backend).init(i18nOpts, function (err, t) {
    if (err) {
      throw err;
    }

    var localize = locI18next.init(i18next, {
      selectorAttr: "data-i18n",
      useOptionsAttr: true,
      parseDefaultValueFromContent: true,
    });

    // on startup
    setTimeout(function () {
      i18nPrep();
      localize("[data-i18n]");
      i18nPost();
    }, 1);

    // when language selector changes
    $("#lang").on("change", function (event) {
      i18next.changeLanguage(event.target.value).then(function (t) {
        i18nPrep();

        localize("[data-i18n]");

        i18nPost();
      });
    });

    $(".shiny-bound-output").on("shiny:value", function (event) {
      setTimeout(function () {
        localize("#" + event.target.id + " " + "[data-i18n]");
      }, 1);
    });

    $(document).on("show.bs.modal", function (event) {
      var id = event.target.id;

      if (id) {
        // "#<modal id> [data-i18n]"
        localize("#" + id + " " + "[data-i18n]");
      }
    });
  });
});

function saveImage(url, type, filename, width, height) {
  // save pdf
  if (type === "pdf") {
    var scalar = 1.332 / 3;
    // set the orientation
    if (width > height) {
      pdf = new jsPDF("l", "px", [width * scalar, height * scalar]);
    } else {
      pdf = new jsPDF("p", "px", [height * scalar, width * scalar]);
    }

    // then we get the dimensions from the 'pdf' file itself
    p_width = pdf.internal.pageSize.getWidth();
    p_height = pdf.internal.pageSize.getHeight();
    pdf.addImage(url, "PNG", 0, 0, p_width, p_height);

    pdf.save(filename);
  } else {
    var link = document.createElement("a");

    if (typeof link.download === "string") {
      link.href = url;
      link.download = filename;

      //Firefox requires the link to be in the body
      document.body.appendChild(link);

      // simulate click
      link.click();

      // remove the link when done
      document.body.removeChild(link);
    } else {
      window.open(url);
    }
  }
}

function add_class_invalid(idval, message, classval = "invalid-val") {
  const invalid_vals = [].concat(message["invalid"]);
  const valid_vals = [].concat(message["valid"]);
  var checkExist1 = setInterval(function () {
    if ($(idval + " .dropdown-item").length) {
      for (let i = 0; i < invalid_vals.length; i++) {
        $(idval + ' .dropdown-item[value="' + invalid_vals[i] + '"]').addClass(
          classval
        );

        $(idval + ' .chip[value="' + invalid_vals[i] + '"]').addClass(
          "invalid-chip-val"
        );
      }

      for (let i = 0; i < valid_vals.length; i++) {
        $(idval + ' .dropdown-item[value="' + valid_vals[i] + '"]').removeClass(
          classval
        );

        $(idval + ' .chip[value="' + valid_vals[i] + '"]').removeClass(
          "invalid-chip-val"
        );
      }

      clearInterval(checkExist1);
    }
  }, 100);
}

function checkForChart() {
  var checkExist3 = setInterval(function () {
    //
    if ($(".highcharts-root").length) {
      Shiny.setInputValue("heat-chartexists", Math.random());
      Shiny.setInputValue("heatplus-chartexists", Math.random());
      clearInterval(checkExist3);
    }
  }, 100);
}


function checkForChartTimerDebug() {
  var checkExist4 = setInterval(function () {
    //
    if ($(".highcharts-root").length) {
      Shiny.setInputValue("heat-chartexists_debug_timer", Math.random());
      clearInterval(checkExist4);
    }
  }, 100);
}

function addAccordionToggle(element) {
  element.classList.toggle("active");
  /* Toggle between hiding and showing the active panel */
  var panel = element.nextElementSibling;
  if (panel.style.display === "block") {
    panel.style.display = "none";
  } else {
    panel.style.display = "block";
  }
}
