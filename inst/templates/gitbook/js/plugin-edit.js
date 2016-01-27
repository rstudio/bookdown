require(["gitbook", "lodash"], function(gitbook, _) {
  gitbook.events.bind("start", function(e, config) {
    var opts = config.edit;
    if (!opts.link) return;
    gitbook.toolbar.createButton({
      icon: 'fa fa-edit',
      label: opts.text || 'Edit',
      position: 'left',
      onClick: function(e) {
        e.preventDefault();
        window.open(opts.link);
      }
    });
  });
});
