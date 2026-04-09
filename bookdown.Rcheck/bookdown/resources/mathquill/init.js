$(function() {
  var MQ = MathQuill.getInterface(2);
  var answer = MQ.MathField($('#latex_input')[0], {
    handlers: {
      edit: update_latex
    }
  });
  function update_latex() {
    var code = answer.latex();
    $('textarea#latex_source').val(code).trigger('input');
  }
  update_latex();
  $('textarea#latex_source').on('input', function (e) {
    $(this).outerHeight(40).outerHeight(this.scrollHeight + 10);
  });
});
