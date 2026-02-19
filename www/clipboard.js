// clipboard.js
// Client-side helper used by Shiny to copy text to clipboard.

Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
  var text = message.text || '';
  var inputId = message.inputId || 'txt_final_url';

  function fallbackCopy() {
    var el = document.getElementById(inputId);
    if (!el) { alert('Copy failed: input not found'); return; }

    el.focus();
    el.select();

    try {
      var ok = document.execCommand('copy');
      if (!ok) alert('Copy failed (execCommand returned false).');
    } catch (e) {
      alert('Copy failed: ' + e);
    }
  }

  // Try modern Clipboard API first (requires secure context in most browsers)
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text).catch(function() {
      fallbackCopy();
    });
  } else {
    fallbackCopy();
  }
});
