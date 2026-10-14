// Keep the <html lang> attribute in sync with the shiny.i18n language selector

// Default to Dutch immediately so the page never starts without a lang attribute
document.documentElement.lang = 'nl';

$(document).on('click', '#selected_language .btn', function () {
  document.documentElement.lang = $(this).find('input').val();
});

$(document).on('shiny:connected', function () {
  var checked = $('#selected_language input:checked').val();
  if (checked) {
    document.documentElement.lang = checked;
  }
});
