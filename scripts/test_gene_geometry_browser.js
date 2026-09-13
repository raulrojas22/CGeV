/* Browser regression harness. Load after jQuery and gene_geometry_update.js. */
(async function () {
  'use strict';
  var results = document.getElementById('results');
  function check(value, label) {
    if (!value) throw new Error(label);
    results.textContent += 'PASS ' + label + '\n';
  }
  function send(el, html, extra) {
    var doc = new DOMParser().parseFromString(html, 'image/svg+xml');
    var x = Object.assign({html: html, uid: doc.documentElement.id, settings: {}, ratio: 1}, extra);
    var event = jQuery.Event('shiny:value', {value: {x: x, deps: [], evals: [], jsHooks: {}}});
    jQuery(el).trigger(event);
    if (!event.isDefaultPrevented()) el.innerHTML = html;
    return event.isDefaultPrevented();
  }
  function normalized(svg) {
    var clone = svg.cloneNode(true);
    var uid = clone.id;
    return new XMLSerializer().serializeToString(clone).split(uid).join('SVG_UID');
  }
  try {
    var fixtures = await (await fetch('geometry-fixtures.json')).json();
    for (var fixture of fixtures) {
      for (var context of ['homo', 'ortho']) {
        var card = document.createElement('section');
        card.innerHTML = '<header>Gene information</header><div id="plot_' + context + '_1-plot"></div><footer><button>Transcripts</button></footer>';
        document.body.appendChild(card);
        var el = card.children[1], header = card.firstChild, footer = card.lastChild;
        check(!send(el, fixture.old), fixture.name + ' first card renders immediately (' + context + ')');
        var svg = el.querySelector('svg'), nodes = Array.from(svg.querySelectorAll('*'));
        var titleBefore = nodes.map(n => n.getAttribute('title'));
        check(send(el, fixture.next), fixture.name + ' scale uses geometry update (' + context + ')');
        check(svg === el.querySelector('svg') && nodes.every((n,i) => n === svg.querySelectorAll('*')[i]), 'SVG and every feature retain DOM identity');
        check(card.firstChild === header && card.lastChild === footer, 'header/footer retain DOM identity');
        check(nodes.every((n,i) => n.getAttribute('title') === titleBefore[i]), 'tooltips and GC remain untouched');
        var expected = new DOMParser().parseFromString(fixture.next, 'image/svg+xml').documentElement;
        check(normalized(svg) === normalized(expected), 'updated SVG equals full final render');
        check(send(el, fixture.old), 'scale decreases also preserve the SVG');
        check(send(el, fixture.next), 'repeated scale increases preserve the SVG');
        check(!send(el, fixture.next.replace('stroke-width=', 'data-test="changed" stroke-width=')), 'metadata/topology changes fall back before patching');
        send(el, fixture.old);
        check(!send(el, fixture.next, {settings:{theme:'dark'}}), 'changed settings use normal rendering');
        send(el, fixture.old);
        el.querySelector('svg').appendChild(document.createElementNS('http://www.w3.org/2000/svg', 'g'));
        check(!send(el, fixture.next), 'unexpected client topology uses normal rendering');
        if (fixture.large) {
          send(el, fixture.old);
          send(el, fixture.large);
          var largeExpected = new DOMParser().parseFromString(fixture.large, 'image/svg+xml').documentElement;
          check(normalized(el.querySelector('svg')) === normalized(largeExpected), 'large scale change keeps the full renderer’s ruler and geometry');
          check(card.firstChild === header && card.lastChild === footer, 'large scale change preserves header/footer');
        }
        card.remove();
      }
    }
    results.textContent += 'ALL PASSED\n';
    document.title = 'Geometry tests: ALL PASSED';
  } catch (err) {
    results.textContent += 'FAIL ' + err.stack;
    document.title = 'Geometry tests: FAILED';
    throw err;
  }
})();
