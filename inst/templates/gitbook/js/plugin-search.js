require([
    "gitbook",
    "lodash"
], function(gitbook, _) {
    var index = null;
    var $searchInput, $searchForm;

    // Use a specific index
    function loadIndex(data) {
        // [Yihui] In bookdown, I use a character matrix to store the chapter
        // content, and the index is dynamically built on the client side.
        // Gitbook prebuilds the index data instead: https://github.com/GitbookIO/plugin-search
        // We can certainly do that via R packages V8 and jsonlite, but let's
        // see how slow it really is before improving it. On the other hand,
        // lunr cannot handle non-English text very well, e.g. the default
        // tokenizer cannot deal with Chinese text, so we may want to replace
        // lunr with a dumb simple text matching approach.
        index = lunr(function () {
          this.ref('url');
          this.field('title', { boost: 10 });
          this.field('body');
        });
        data.map(function(item) {
          index.add({
            url: item[0],
            title: item[1],
            body: item[2]
          });
        });
    }

    // Fetch the search index
    function fetchIndex() {
        $.getJSON(gitbook.state.basePath+"/search_index.json")
        .then(loadIndex);
    }

    // Search for a term and return results
    function search(q) {
        if (!index) return;

        var results = _.chain(index.search(q))
        .map(function(result) {
            var parts = result.ref.split("#");
            return {
                path: parts[0],
                hash: parts[1]
            };
        })
        .value();

        return results;
    }

    // Create search form
    function createForm(value) {
        if ($searchForm) $searchForm.remove();
        if ($searchInput) $searchInput.remove();

        $searchForm = $('<div>', {
            'class': 'book-search',
            'role': 'search'
        });

        $searchInput = $('<input>', {
            'type': 'text',
            'class': 'form-control',
            'val': value,
            'placeholder': 'Type to search'
        });

        $searchInput.appendTo($searchForm);
        $searchForm.prependTo(gitbook.state.$book.find('.book-summary'));
    }

    // Return true if search is open
    function isSearchOpen() {
        return gitbook.state.$book.hasClass("with-search");
    }

    // Toggle the search
    function toggleSearch(_state) {
        if (isSearchOpen() === _state) return;

        gitbook.state.$book.toggleClass("with-search", _state);

        // If search bar is open: focus input
        if (isSearchOpen()) {
            gitbook.sidebar.toggle(true);
            $searchInput.focus();
        } else {
            $searchInput.blur();
            $searchInput.val("");
            gitbook.sidebar.filter(null);
        }
    }

    // Recover current search when page changed
    function recoverSearch() {
        var keyword = gitbook.storage.get("keyword", "");

        createForm(keyword);

        if (keyword.length > 0) {
            if(!isSearchOpen()) {
                toggleSearch();
            }
            gitbook.sidebar.filter(_.pluck(search(keyword), "path"));
        }
    }


    gitbook.events.bind("start", function(config) {
        // Pre-fetch search index and create the form
        fetchIndex();
        createForm();

        // Type in search bar
        $(document).on("keyup", ".book-search input", function(e) {
            var key = (e.keyCode ? e.keyCode : e.which);
            var q = $(this).val();

            if (key == 27) {
                e.preventDefault();
                toggleSearch(false);
                return;
            }
            if (q.length === 0) {
                gitbook.sidebar.filter(null);
                gitbook.storage.remove("keyword");
            } else {
                var results = search(q);
                gitbook.sidebar.filter(
                    _.pluck(results, "path")
                );
                gitbook.storage.set("keyword", q);
            }
        });

        // Create the toggle search button
        gitbook.toolbar.createButton({
            icon: 'fa fa-search',
            label: 'Search',
            position: 'left',
            onClick: toggleSearch
        });

        // Bind keyboard to toggle search
        gitbook.keyboard.bind(['f'], toggleSearch);
    });

    // [Yihui] do not try to recover search; always start fresh
    // gitbook.events.bind("page.change", recoverSearch);
});
