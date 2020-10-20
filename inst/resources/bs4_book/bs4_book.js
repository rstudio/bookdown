littlefoot.littlefoot()

function changeTooltipMessage(element, msg) {
  var tooltipOriginalTitle=element.getAttribute('data-original-title');
  element.setAttribute('data-original-title', msg);
  $(element).tooltip('show');
  element.setAttribute('data-original-title', tooltipOriginalTitle);
}

if(ClipboardJS.isSupported()) {
  $(document).ready(function() {
    // Insert copy buttons
    var copyButton = "<div class='copy'><button type='button' class='btn btn-outline-primary btn-sm btn-copy' title='Copy to clipboard' aria-label='Copy' data-toggle='popover' data-placement='top' data-trigger='hover'>Copy</button></div>";
    $(copyButton).prependTo("pre");
    // Initialize tooltips:
    $('.btn-copy').tooltip({container: 'body'});

    // Initialize clipboard:
    var clipboard = new ClipboardJS('.btn-copy', {
      text: function(trigger) {
        return trigger.parentNode.parentNode.textContent;
      }
    });

    clipboard.on('success', function(e) {
      console.info('Action:', e.action);
      console.info('Text:', e.text);
      console.info('Trigger:', e.trigger);

      changeTooltipMessage(e.trigger, 'Copied!');
      e.clearSelection();
    });

    clipboard.on('error', function() {
      changeTooltipMessage(e.trigger,'Press Ctrl+C or Command+C to copy');
    });
  });
};
