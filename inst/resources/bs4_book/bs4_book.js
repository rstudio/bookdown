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
