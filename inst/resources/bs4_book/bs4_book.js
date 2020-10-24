$(function () {
  // Activate popovers
  $('[data-toggle="popover"]').popover({
    container: 'body',
    html: true,
    trigger: 'focus',
    placement: "top",
    sanitize: false,
  });
  $('[data-toggle="tooltip"]').tooltip();

  // Activate headroom
  var myElement = document.querySelector(".navbar");
  var headroom  = new Headroom(myElement, {tolerance : 10});
  headroom.init();
})

// Initialise search index when search gets focus */
var fuse;

$(function () {
  $("#search").focus(async function(e) {
    if (!fuse) {
      $(e.target).addClass("loading");
      fuse = initFuse()
      await fuse;
     $(e.target).removeClass("loading");
    }
  });

  // Perform search when #search gets keypress
  $("#search").keydown(async function(e) {
    if (event.which == 13) {
      event.preventDefault();
    }

    var fuse = await getFuse();
    if (!fuse) {
      return;
    }

    var results = fuse.search(e.target.value);
    console.log(results);
  });

  // TODO: add error handling
  async function initFuse() {
    response = await fetch('search.json');
    data = await response.json();

    var options = {
      keys: ["heading", "text"],
      ignoreLocation: true,
      threshold: 0.1
    };
    return new Fuse(data, options);
  }
});

var getFuse = runLatest(async () => fuse);
function runLatest(func) {
  var i = 0;
  return async function() {
    i++;
    let this_i = i;
    var value = await func();
    if (i != this_i) {
      return null;
    } else {
      return value;
    }
  };
}

function changeTooltipMessage(element, msg) {
  var tooltipOriginalTitle=element.getAttribute('data-original-title');
  element.setAttribute('data-original-title', msg);
  $(element).tooltip('show');
  element.setAttribute('data-original-title', tooltipOriginalTitle);
}

$(document).ready(function() {
  if(ClipboardJS.isSupported()) {
    // Insert copy buttons
    var copyButton = "<div class='copy'><button type='button' class='btn btn-outline-primary btn-copy' title='Copy to clipboard' aria-label='Copy' data-toggle='popover' data-placement='top' data-trigger='hover'>Copy</button></div>";
    $(copyButton).prependTo("pre");
    // Initialize tooltips:
    $('.btn-copy').tooltip({container: 'body', boundary: 'window'});

    // Initialize clipboard:
    var clipboard = new ClipboardJS('.btn-copy', {
      text: function(trigger) {
        return trigger.parentNode.parentNode.textContent;
      }
    });

    clipboard.on('success', function(e) {
      changeTooltipMessage(e.trigger, 'Copied!');
      e.clearSelection();
    });

    clipboard.on('error', function() {
      changeTooltipMessage(e.trigger,'Press Ctrl+C or Command+C to copy');
    });
  };
});
