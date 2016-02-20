$(function() {
  var MQ = MathQuill.getInterface(2);
  var answer = MQ.MathField($('#latex_input')[0], {
    handlers: {
      edit: function() {
        var code = answer.latex();
        $('textarea#latex_source').val(code).trigger('input');
        Shiny.onInputChange('latex_source', code);
      }
    }
  });
  $('textarea#latex_source').on('input', function (e) {
    $(this).outerHeight(40).outerHeight(this.scrollHeight + 10);
  });
});
