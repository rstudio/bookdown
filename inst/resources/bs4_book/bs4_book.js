$(function () {
  var url = new URL(window.location.href);
  var toMark = url.searchParams.get("q");
  var mark = new Mark("main");
  if (toMark) {
    mark.mark(toMark, {
      accuracy: {
        value: "complementary",
        limiters: [",", ".", ":", "/"],
      }
    });
  }

  // Activate popovers
  $('[data-toggle="popover"]').popover({
    container: 'body',
    html: true,
    trigger: 'focus',
    placement: "top",
    sanitize: false,
  });
  $('[data-toggle="tooltip"]').tooltip();
});

// Sticky Nav ------------------------------------------------------------------

$(function () {
  const main = document.getElementById('content');
  const header = document.querySelector('.sidebar-book');
  let dist = 0;

  // calculate distance to the content so we know when to transition to sticky mode
  const setDist = () => {
    dist = main.getBoundingClientRect().top + window.scrollY;
  }

  const toggleStickyLayoutClass = () => {
    if (window.innerWidth > 991) return;
    if (dist < window.scrollY) {
      if (!document.body.classList.contains('scrolled-to-main')) {
        main.style.paddingTop = 
          `${Math.round(header.getBoundingClientRect().height - 56)}px`;
        document.body.classList.add('scrolled-to-main');
      }
    } else if (document.body.classList.contains('scrolled-to-main')) {
      document.body.classList.remove('scrolled-to-main');
      main.style.paddingTop = null;
    }
  };

  // set initial value
  setDist();

  window.addEventListener('scroll', toggleStickyLayoutClass);
  window.addEventListener('resize', setDist);
});

// Search ----------------------------------------------------------------------

var fuse;

$(function () {
  // Initialise search index on focus
  $("#search").focus(async function(e) {
    if (fuse) {
      return;
    }

    $(e.target).addClass("loading");

    var response = await fetch('search.json');
    var data = await response.json();

    var options = {
      keys: ["heading", "text", "code"],
      ignoreLocation: true,
      threshold: 0.1,
      includeMatches: true,
      includeScore: true,
    };
    fuse = new Fuse(data, options);

    $(e.target).removeClass("loading");
  });

  // Use algolia autocomplete
  var options = {
    autoselect: true,
    debug: true,
    hint: false,
    minLength: 2,
  };

  $("#search").autocomplete(options, [
    {
      name: "content",
      source: searchFuse,
      templates: {
        suggestion: (s) => {
          if (s.chapter == s.heading) {
            return `${s.chapter}`;
          } else {
            return `${s.chapter} /<br> ${s.heading}`;
          }
        },
      },
    },
  ]).on('autocomplete:selected', function(event, s) {
    window.location.href = s.path + "?q=" + q + "#" + s.id;
  });
});

var q;
async function searchFuse(query, callback) {
  await fuse;

  var items;
  if (!fuse) {
    items = [];
  } else {
    q = query;
    var results = fuse.search(query, { limit: 20 });
    items = results
      .filter((x) => x.score <= 0.75)
      .map((x) => x.item);
  }

  callback(items);
}

// Copy to clipboard -----------------------------------------------------------

function changeTooltipMessage(element, msg) {
  var tooltipOriginalTitle=element.getAttribute('data-original-title');
  element.setAttribute('data-original-title', msg);
  $(element).tooltip('show');
  element.setAttribute('data-original-title', tooltipOriginalTitle);
}

$(document).ready(function() {
  if(ClipboardJS.isSupported()) {
    // Insert copy buttons
    var copyButton = "<button type='button' class='btn btn-copy' title='Copy to clipboard' aria-label='Copy to clipboard' data-toggle='popover' data-placement='top' data-trigger='hover'><i class='bi'></i></button>";
    $(copyButton).appendTo("div.sourceCode");
    // Initialize tooltips:
    $('.btn-copy').tooltip({container: 'body', boundary: 'window'});

    // Initialize clipboard:
    var clipboard = new ClipboardJS('.btn-copy', {
      text: function(trigger) {
        return trigger.parentNode.textContent;
      }
    });

    clipboard.on('success', function(e) {
      const btn = e.trigger;
      changeTooltipMessage(btn, 'Copied!');
      btn.classList.add('btn-copy-checked');
      setTimeout(function() {
        btn.classList.remove('btn-copy-checked');
      }, 2000);
      e.clearSelection();
    });

    clipboard.on('error', function() {
      changeTooltipMessage(e.trigger,'Press Ctrl+C or Command+C to copy');
    });
  };
});
