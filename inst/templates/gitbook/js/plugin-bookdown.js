require(["gitbook", "lodash"], function(gitbook, _) {
  gitbook.events.bind("start", function(e, config) {

    // add the Edit button (edit on Github)
    var opts = config.edit;
    if (opts.link) gitbook.toolbar.createButton({
      icon: 'fa fa-edit',
      label: opts.text || 'Edit',
      position: 'left',
      onClick: function(e) {
        e.preventDefault();
        window.open(opts.link);
      }
    });

    var down = config.download;
    if (down) gitbook.toolbar.createButton({
      icon: 'fa fa-download',
      label: 'Download',
      position: 'left',
      dropdown: $.map(down, function(link, i) {
        return {
          text: link.replace(/.*[.]/g, '').toUpperCase(),
          onClick: function(e) {
            e.preventDefault();
            window.open(link);
          }
        };
      })
    });

    // highlight the current section in TOC
    var href = window.location.pathname;
    href = href.substr(href.lastIndexOf('/') + 1);
    if (href === '') href = 'index.html';
    $('a[href^="' + href + location.hash + '"]').parent('li.chapter').first()
      .addClass('active');
    var summary = $('ul.summary'), chaps = summary.find('li.chapter');
    chaps.on('click', function(e) {
      e.stopPropagation();
      chaps.removeClass('active');
      $(this).addClass('active');
      gitbook.storage.set('tocScrollTop', summary.scrollTop());
    });

    // add tooltips to the <a>'s that are truncated
    $('a').each(function(i, el) {
      if (el.offsetWidth >= el.scrollWidth) return;
      if (typeof el.title === 'undefined') return;
      el.title = el.text;
    });

    // restore TOC scroll position
    var pos = gitbook.storage.get('tocScrollTop');
    if (typeof pos !== 'undefined') summary.scrollTop(pos);
  });

  gitbook.events.bind("page.change", function(e) {
    // store TOC scroll position
    var summary = $('ul.summary');
    gitbook.storage.set('tocScrollTop', summary.scrollTop());
  });
});
