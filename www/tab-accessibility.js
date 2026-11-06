// Fix: Bootstrap removes tab links from the keyboard tab order when they are
// not the active tab, so Tab/Shift+Tab cannot reach other tabs (e.g. Home,
// Start, Ontdekken, Kiezen). Force all tab links to stay keyboard-focusable.
function fixTabFocusOrder() {
  $('a[data-toggle="tab"]').attr('tabindex', 0);
}

$(document).ready(fixTabFocusOrder);
$(document).on('shown.bs.tab', 'a[data-toggle="tab"]', fixTabFocusOrder);
