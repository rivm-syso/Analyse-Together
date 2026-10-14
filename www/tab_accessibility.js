// Accessibility fix: tab-pane containers are not interactive and should not receive tab focus.
// Shiny/Bootstrap re-add tabindex="0" to the active pane on every tab switch, so a MutationObserver
// is used to strip it again immediately, regardless of when/how it gets (re)added.
$(function () {
  function stripTabIndex(el) {
    if (el.classList && el.classList.contains('tab-pane') && el.hasAttribute('tabindex')) {
      el.removeAttribute('tabindex');
    }
  }

  document.querySelectorAll('.tab-pane').forEach(stripTabIndex);

  new MutationObserver(function (mutations) {
    mutations.forEach(function (m) { stripTabIndex(m.target); });
  }).observe(document.body, { attributes: true, attributeFilter: ['tabindex'], subtree: true });
});
