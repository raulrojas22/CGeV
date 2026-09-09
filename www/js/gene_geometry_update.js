/* Keep existing gene SVG nodes when a new scale changes only their geometry. */
(function () {
  'use strict';

  var previous = new WeakMap();
  var geometry = new Set(['x', 'x1', 'x2', 'cx', 'width', 'points', 'd', 'transform']);

  function parseSvg(html) {
    var doc = new DOMParser().parseFromString(html, 'image/svg+xml');
    var svg = doc.documentElement;
    if (svg.localName !== 'svg' || doc.querySelector('parsererror')) return null;
    // ggiraph's nearest-point handler caches bounds. Such plots need its normal render.
    if (svg.querySelector('[nearest]')) return null;
    return svg;
  }

  function elements(svg) {
    return [svg].concat(Array.from(svg.querySelectorAll('*')));
  }

  function attributes(node) {
    return Array.from(node.attributes).map(function (attr) { return attr.name; }).sort();
  }

  function planUpdate(oldSvg, nextSvg, liveSvg) {
    var oldNodes = elements(oldSvg);
    var nextNodes = elements(nextSvg);
    var liveNodes = elements(liveSvg);
    if (oldNodes.length !== nextNodes.length || oldNodes.length !== liveNodes.length) return null;
    var changes = [];
    for (var i = 0; i < oldNodes.length; i++) {
      var oldNode = oldNodes[i], nextNode = nextNodes[i], liveNode = liveNodes[i];
      if (oldNode.localName !== nextNode.localName || oldNode.localName !== liveNode.localName ||
          oldNode.namespaceURI !== nextNode.namespaceURI || oldNode.namespaceURI !== liveNode.namespaceURI ||
          oldNode.childElementCount !== nextNode.childElementCount || oldNode.childElementCount !== liveNode.childElementCount ||
          oldNode.textContent !== nextNode.textContent ||
          oldNode.getAttribute('id') !== liveNode.getAttribute('id') ||
          oldNode.getAttribute('data-id') !== liveNode.getAttribute('data-id')) return null;
      var names = attributes(oldNode);
      if (names.join('|') !== attributes(nextNode).join('|')) return null;
      for (var j = 0; j < names.length; j++) {
        var name = names[j];
        var oldValue = oldNode.getAttribute(name), nextValue = nextNode.getAttribute(name);
        if (oldValue === nextValue) continue;
        // Changes to labels, GC, colors, IDs, viewBox or interactions use the full renderer.
        if (i === 0 || !geometry.has(name)) return null;
        // Do not overwrite geometry another client feature has changed independently.
        if (liveNode.getAttribute(name) !== oldValue) return null;
        changes.push([liveNode, name, nextValue]);
      }
    }
    return changes;
  }

  function install() {
    if (!window.jQuery) return;
    window.jQuery(document).on('shiny:value.cgvGeneGeometry', function (event) {
      var el = event.target;
      var value = event.value;
      var x = value && value.x;
      if (!el || !/^plot_(homo|ortho)_.*-plot$/.test(el.id) || !x ||
          typeof x.html !== 'string' || typeof x.uid !== 'string') return;
      // Preserve htmlwidgets' normal path whenever it has executable hooks to run.
      if (x.js || (value.evals && value.evals.length) ||
          (value.jsHooks && Object.keys(value.jsHooks).length)) {
        previous.delete(el);
        return;
      }
      var settings = JSON.stringify([x.settings, x.ratio, value.deps]);
      var old = previous.get(el);
      var uid = old ? old.uid : x.uid;
      var nextSvg = parseSvg(x.html.split(x.uid).join(uid));
      if (!nextSvg) { previous.delete(el); return; }
      var liveSvg = el.querySelector('svg.ggiraph-svg');
      if (old && settings === old.settings && liveSvg && liveSvg.id === uid &&
          !event.isDefaultPrevented()) {
        var changes = planUpdate(old.svg, nextSvg, liveSvg);
        if (changes) {
          // Validate the entire SVG before touching it; no partial updates on fallback.
          changes.forEach(function (change) { change[0].setAttribute(change[1], change[2]); });
          previous.set(el, { svg: nextSvg, uid: uid, settings: settings });
          event.preventDefault();
          el.dispatchEvent(new CustomEvent('cgv:gene-geometry-updated', {
            bubbles: true, detail: { attributes: changes.length }
          }));
          return;
        }
      }
      previous.set(el, { svg: parseSvg(x.html), uid: x.uid, settings: settings });
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', install, { once: true });
  } else {
    install();
  }
})();
