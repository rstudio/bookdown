require(["gitbook", "lodash"], function(gitbook, _) {

  var gs = gitbook.storage;

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
    if (down) if (down.length === 1 && /[.]pdf$/.test(down[0])) {
      gitbook.toolbar.createButton({
        icon: 'fa fa-file-pdf-o',
        label: 'PDF',
        position: 'left',
        onClick: function(e) {
          e.preventDefault();
          window.open(down[0]);
        }
      });
    } else {
      gitbook.toolbar.createButton({
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
    };

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
      gs.set('tocScrollTop', summary.scrollTop());
    });

    // add tooltips to the <a>'s that are truncated
    $('a').each(function(i, el) {
      if (el.offsetWidth >= el.scrollWidth) return;
      if (typeof el.title === 'undefined') return;
      el.title = el.text;
    });

    // restore TOC scroll position
    var pos = gs.get('tocScrollTop');
    if (typeof pos !== 'undefined') summary.scrollTop(pos);
  });

  gitbook.events.bind("page.change", function(e) {
    // store TOC scroll position
    var summary = $('ul.summary');
    gs.set('tocScrollTop', summary.scrollTop());
  });

  var bookBody = $('.book-body'), bookInner = bookBody.find('.body-inner');
  $(document).on('servr:reload', function(e) {
    // save scroll position before page is reloaded via servr
    gs.set('bookBodyScrollTop',  bookBody.scrollTop());
    gs.set('bookInnerScrollTop', bookInner.scrollTop());
  });

  $(document).ready(function(e) {
    var pos1 = gs.get('bookBodyScrollTop');
    var pos2 = gs.get('bookInnerScrollTop');
    if (pos1) bookBody.scrollTop(pos1);
    if (pos2) bookInner.scrollTop(pos2);
    // clear book body scroll position
    gs.remove('bookBodyScrollTop');
    gs.remove('bookInnerScrollTop');
  });
});
