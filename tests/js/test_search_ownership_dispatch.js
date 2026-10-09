// Execute the actual transport handlers with a controlled DOM/Shiny surface.
const fs = require('fs');
const vm = require('vm');
const assert = require('assert/strict');
const path = require('path');
const root = path.resolve(__dirname, '../..');
const ui = fs.readFileSync(path.join(root, 'ui.R'), 'utf8');
const handlers = {};
const inputs = [];
const timers = [];
let modalClosed;
let modalAccepted;
const context = {
  $(document) { return {on(event, selector, callback) {
    if (event === 'hidden.bs.modal') {
      assert.equal(selector, '#shiny-modal'); modalClosed = callback;
    } else {
      assert.equal(event, 'click');
      assert.equal(selector, '.app-confirmation-btn-confirm, #confirm_delete_plot_btn'); modalAccepted = callback;
    }
  }}; },
  Shiny: {
    addCustomMessageHandler(name, callback) { handlers[name] = callback; },
    setInputValue(name, value, options) { inputs.push({ name, value, options }); }
  },
  document: { addEventListener() {}, getElementById() { throw Error('untagged button click'); } },
  setWorkflowPanel() {}, Date,
};
context.window = { Shiny: context.Shiny, setTimeout(callback) { timers.push(callback); } };
const acceptStart = ui.indexOf("$(document).on('click', '.app-confirmation-btn-confirm");
const acceptEnd = ui.indexOf('          });', acceptStart) + '          });'.length;
assert(acceptStart >= 0 && acceptEnd > acceptStart);
vm.runInNewContext(ui.slice(acceptStart, acceptEnd), context);
modalAccepted.call({closest() { return null; }});
assert.equal(inputs.length, 0);
const searchDialog = id => ({querySelector(selector) {
  if (selector === '[data-cgv-delete-intent]') return null;
  return {getAttribute(name) { return name === 'data-cgv-search-mode' ? 'orthologous' : id; }};
}});
modalAccepted.call({closest() { return searchDialog('search_A'); }});
modalAccepted.call({closest() { return searchDialog('search_B'); }});
assert.equal(inputs[0].name, 'cgv_search_confirmation_accepted');
assert.equal(inputs[0].value.request_id, 'search_A');
assert.equal(inputs[1].value.request_id, 'search_B');
assert.equal(inputs[0].options.priority, 'event');
inputs.length = 0;
modalAccepted.call({closest() { return {querySelector() {
  return {getAttribute() { return 'delete_1'; }};
}}; }});
assert.equal(inputs[0].name, 'cgv_plot_delete_confirmed');
assert.equal(inputs[0].value.intent_id, 'delete_1');
inputs.length = 0;
const modalStart = ui.indexOf("$(document).on('hidden.bs.modal'");
const modalEnd = ui.indexOf('          });', modalStart) + '          });'.length;
assert(modalStart >= 0 && modalEnd > modalStart);
vm.runInNewContext(ui.slice(modalStart, modalEnd), context);
modalClosed.call({querySelector() { return null; }});
assert.equal(inputs.length, 0);
modalClosed.call({querySelector() { return {getAttribute(name) {
  return name === 'data-cgv-search-mode' ? 'homologous' : 'search_41';
}}; }});
const closed = inputs.pop();
assert.equal(closed.name, 'cgv_search_confirmation_closed');
assert.equal(closed.value.request_id, 'search_41');
assert.equal(closed.value.mode, 'homologous');
for (const name of ['cgv_trigger_homo_search', 'cgv_trigger_ortho_search']) {
  const start = ui.indexOf(`Shiny.addCustomMessageHandler('${name}'`);
  const end = ui.indexOf('          });', start) + '          });'.length;
  assert(start >= 0 && end > start);
  vm.runInNewContext(ui.slice(start, end), context);
}
for (const [mode, input] of [['homo', 'cgv_homo_search_dispatch'], ['ortho', 'cgv_ortho_search_dispatch']]) {
  handlers[`cgv_trigger_${mode}_search`]({request_id: 'search_42', dispatch_id: 7, origin: 'local'});
  const emitted = inputs.pop();
  assert.equal(emitted.name, input);
  assert.equal(emitted.value.request_id, 'search_42');
  assert.equal(emitted.value.dispatch_id, 7);
  assert.equal(emitted.options.priority, 'event');
}
vm.runInNewContext(fs.readFileSync(path.join(root, 'www/js/gene_catalog.js'), 'utf8'), context);
for (const [handler, ack, payload] of [
  ['cgv:catalog-open-multigene', 'catalog_multigene_ready', {species_id: 'a', gene: 'G', request_id: 'search_43'}],
  ['cgv:catalog-open-cross', 'catalog_cross_ready', {species_ids: ['a', 'b'], gene: 'G', request_id: 'search_44'}]
]) {
  handlers[handler](payload);
  timers.shift()();
  const emitted = inputs.pop();
  assert.equal(emitted.name, ack);
  assert.equal(emitted.value.request_id, payload.request_id);
  assert.equal(emitted.value.gene, payload.gene);
}
console.log('search-ownership-browser-transport: 6 actual handlers passed, including tagged search/delete acceptance');
