$(function() {
  $('#latex_input').on('keydown keypress', function(e) {
    var code = $(this).mathquill('latex');
    $('textarea#latex_source').val(code).trigger('input');
    Shiny.onInputChange('latex_source', code);
  });
  $('textarea#latex_source').on('input', function (e) {
    $(this).outerHeight(40).outerHeight(this.scrollHeight + 10);
  });
});
