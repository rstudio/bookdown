gitbook.require(["gitbook", "jQuery"], function(gitbook, $) {

  var copyButton = '<button type="button" class="copy-to-clipboard-button" title="Copy to clipboard" aria-label="Copy to clipboard"><i class="fa fa-copy"></i></button>';
  var clipboard;

  gitbook.events.bind("page.change", function() {

    if (!ClipboardJS.isSupported()) return;

    // the page.change event is thrown twice: before and after the page changes
    // avoid appending multiple event handlers 
    if (clipboard) return;

    $(copyButton).prependTo("div.sourceCode");

    clipboard = new ClipboardJS(".copy-to-clipboard-button", {
      text: function(trigger) {
        return trigger.parentNode.textContent;
      }
    });

  });

});
