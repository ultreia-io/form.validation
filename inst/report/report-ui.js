/* Shared loading, accessible tab navigation and card-filter shortcuts. */
'use strict';
window.ValidationReportUI = (() => {
  const workerURL = document.querySelector('script[data-csv-worker]').dataset.csvWorker;
  const yieldPaint = () => new Promise(resolve => setTimeout(resolve, 0));
  const paintFeedback = () => new Promise(resolve => requestAnimationFrame(() => setTimeout(resolve, 0)));
  function loadCSV(path, progress = () => {}) {
    return new Promise((resolve, reject) => {
      const worker = new Worker(workerURL);
      worker.onmessage = event => {
        if (event.data.status) progress(event.data.status);
        else {
          worker.terminate();
          if (event.data.error) reject(new Error(`${path}: ${event.data.error}`));
          else resolve(event.data.data);
        }
      };
      worker.onerror = () => { worker.terminate(); reject(new Error(`Cannot read ${path}`)); };
      worker.postMessage({url: new URL(path, document.baseURI).href});
    });
  }
  function feedback(container) {
    const node = document.getElementById('loading-state').content.firstElementChild.cloneNode(true);
    node.hidden = true;
    container.append(node);
    const message = node.querySelector('[data-loading-message]');
    const retry = node.querySelector('button');
    let timer, visibleSince;
    function cancelTimer() { clearTimeout(timer); timer = undefined; }
    return {
      start(text, immediate = false) {
        cancelTimer();
        const keepVisible = !node.hidden && !node.classList.contains('is-error');
        node.hidden = !keepVisible;
        if (!keepVisible) visibleSince = undefined;
        node.classList.remove('is-error'); node.setAttribute('role', 'status');
        retry.hidden = true; container.setAttribute('aria-busy', 'true'); message.textContent = text;
        if (immediate) { node.hidden = false; visibleSince = performance.now(); }
        else if (!keepVisible) timer = setTimeout(() => {
          timer = undefined; node.hidden = false; visibleSince = performance.now();
        }, 200);
      },
      update(text) { message.textContent = text; },
      settle() {
        cancelTimer();
        // Once shown, keep the indicator readable instead of flashing it briefly.
        const remaining = visibleSince === undefined ? 0 : Math.max(0, 300 - (performance.now() - visibleSince));
        return remaining ? new Promise(resolve => setTimeout(resolve, remaining)) : Promise.resolve();
      },
      finish() { cancelTimer(); visibleSince = undefined; node.hidden = true; container.setAttribute('aria-busy', 'false'); },
      fail(error, action) {
        cancelTimer();
        container.setAttribute('aria-busy', 'false'); node.hidden = false;
        node.classList.add('is-error'); node.setAttribute('role', 'alert');
        message.textContent = `Unable to load this section: ${error.message}`;
        retry.hidden = false; retry.onclick = action;
      }
    };
  }
  // One controller for top-level details and nested sheet tabs. Panels retain state.
  function tabs(parent, definitions, id, label = 'Sheets', heading = false) {
    if (heading) {
      const title = document.createElement('h2'); title.className = 'details-heading';
      title.textContent = label; parent.append(title);
      const frame = document.createElement('div'); frame.className = 'details-frame';
      parent.append(frame); parent = frame;
    }
    const bar = document.createElement('div'); bar.className = heading ? 'tabs detail-tabs' : 'tabs';
    bar.setAttribute('role', 'tablist'); bar.setAttribute('aria-label', label); parent.append(bar);
    const viewport = document.createElement('div');
    viewport.className = heading ? 'tab-panels details-content' : 'tab-panels';
    parent.append(viewport);
    const status = feedback(viewport);
    let active, visible, transition = 0, ready = Promise.resolve();
    const scope = heading ? undefined : filterScope(
      () => entries.forEach(entry => { entry.filters = entry.controller ? null : {}; }),
      () => entries.some(entry => !entry.controller && Object.keys(entry.filters || {}).length > 0));
    const entries = definitions.map((definition, index) => {
      const button = document.createElement('button'); button.type = 'button';
      button.textContent = definition.label;
      if (definition.decorate) definition.decorate(button);
      button.disabled = Boolean(definition.disabled);
      if (button.disabled && definition.disabledHint) button.title = definition.disabledHint;
      button.id = `${id}-tab-${index}`; button.setAttribute('role', 'tab');
      const panel = document.createElement('div'); panel.id = `${button.id}-panel`;
      panel.hidden = true; panel.setAttribute('role', 'tabpanel');
      panel.setAttribute('aria-labelledby', button.id); button.setAttribute('aria-controls', panel.id);
      button.setAttribute('aria-selected', 'false'); bar.append(button); viewport.append(panel);
      const content = document.createElement('div'); panel.append(content);
      return {definition, button, panel, content, controller: null, pending: null, filters: null};
    });
    async function ensure(entry) {
      if (entry.controller) return;
      if (entry.pending) return entry.pending;
      entry.pending = (async () => {
        try {
          await yieldPaint();
          entry.controller = await entry.definition.render(entry.content, text => {
            if (active === entry) status.update(text);
          }, scope) || {};
        } catch (error) {
          entry.content.replaceChildren();
          throw error;
        } finally { entry.pending = null; }
      })();
      return entry.pending;
    }
    function open(key, filters, scroll = false) {
      const entry = entries.find(item => item.definition.id === key);
      if (!entry || entry.button.disabled) return;
      const current = ++transition;
      active = entry;
      entries.forEach(item => {
        item.button.setAttribute('aria-selected', String(item === entry));
      });
      if (filters !== undefined) entry.filters = filters;
      if (visible) visible.panel.inert = true;
      viewport.classList.toggle('is-empty', !visible);
      // Reuse completed panels without rendering, filtering or loading feedback.
      if (entry.loaded && entry.filters === null) {
        entries.forEach(item => {
          item.panel.hidden = item !== entry;
          item.panel.inert = false;
        });
        visible = entry;
        viewport.classList.remove('is-empty');
        status.finish();
        ready = Promise.resolve();
        if (scroll) { bar.scrollIntoView({block: 'start'}); entry.button.focus({preventScroll: true}); }
        return ready;
      }
      entry.loaded = false;
      const heavy = !entry.controller && entry.definition.largeTable;
      status.start(`Loading ${entry.definition.label.toLowerCase()}…`, Boolean(heavy));
      // Keep the previous panel visible until the selected tab and its sheet are ready.
      ready = (async () => {
        try {
          if (heavy) await paintFeedback();
          if (current !== transition) return;
          await ensure(entry);
          if (current !== transition) return;
          const pendingFilters = entry.filters;
          if (pendingFilters !== null) entry.controller.setFilters?.(pendingFilters);
          await entry.controller.ready;
          if (current !== transition) return;
          if (entry.filters === pendingFilters) entry.filters = null;
          entry.loaded = true;
          await status.settle();
          if (current !== transition) return;
          entries.forEach(item => {
            item.panel.hidden = item !== entry;
            item.panel.inert = false;
          });
          visible = entry;
          viewport.classList.remove('is-empty');
          if (heavy) await paintFeedback();
          if (current !== transition) return;
          status.finish();
        } catch (error) {
          if (current === transition) status.fail(error, () => open(key));
        }
      })();
      if (scroll) { bar.scrollIntoView({block: 'start'}); entry.button.focus({preventScroll: true}); }
      return ready;
    }
    entries.forEach(entry => {
      entry.button.onclick = () => open(entry.definition.id);
      entry.button.onkeydown = event => {
        const delta = event.key === 'ArrowRight' ? 1 : event.key === 'ArrowLeft' ? -1 : 0;
        if (!delta && !['Home', 'End'].includes(event.key)) return;
        event.preventDefault();
        const enabled = entries.filter(item => !item.button.disabled);
        if (!enabled.length) return;
        const index = enabled.indexOf(entry);
        const next = event.key === 'Home' ? 0 : event.key === 'End' ? enabled.length - 1 : (index + delta + enabled.length) % enabled.length;
        open(enabled[next].definition.id); enabled[next].button.focus();
      };
    });
    const initial = entries.find(entry => !entry.button.disabled);
    if (initial) open(initial.definition.id);
    return {
      open,
      enable(key) {
        const entry = entries.find(item => item.definition.id === key);
        if (!entry || !entry.button.disabled) return;
        entry.button.disabled = false;
        if (entry.definition.disabledHint) entry.button.removeAttribute('title');
      },
      get ready() { return ready; },
      setFilters(filters) {
        entries.forEach(entry => {
          if (entry.controller) {
            entry.filters = null;
            entry.controller.setFilters?.(filters);
          }
          else entry.filters = filters;
        });
        scope?.refresh();
        const match = entries.find(entry => !entry.button.disabled &&
          (!entry.definition.matches || entry.definition.matches(filters)));
        if (match) return open(match.definition.id);
      }
    };
  }
  function bindCard(card, count, action, label, actionLabel) {
    if (!card || count <= 0) return;
    const button = document.createElement('button'); button.type = 'button';
    button.className = card.className + ' overview-shortcut';
    for (const [key, value] of Object.entries(card.dataset)) button.dataset[key] = value;
    button.append(...card.childNodes); button.setAttribute('aria-label', label); button.title = label;
    const note = button.querySelector('.overview-notes[id]');
    if (note) button.setAttribute('aria-describedby', note.id);
    const hint = document.createElement('small'); hint.className = 'overview-action';
    const text = document.createElement('span'); text.textContent = actionLabel;
    const arrow = document.createElement('span'); arrow.textContent = '→'; arrow.setAttribute('aria-hidden', 'true');
    hint.append(text, arrow); button.append(hint);
    button.onclick = action; card.replaceWith(button);
  }
  function applyFilters(search, inputs, filters) {
    search.value = '';
    for (const [key, input] of Object.entries(inputs)) {
      input.value = filters[key] ?? '';
      input.dataset.exact = key in filters ? 'true' : 'false';
    }
  }
  function matchesFilter(input, value) {
    const text = String(value ?? '').toLowerCase(), query = input.value.toLowerCase();
    return input.dataset.exact === 'true' ? text === query : text.includes(query);
  }
  function hasFilters(search, inputs) {
    return search.value !== '' || Object.values(inputs).some(input => input.value !== '');
  }
  function paginationSummary(page, limit, matching, total, filtered) {
    const format = new Intl.NumberFormat('en');
    const range = matching
      ? `${format.format(page * limit + 1)}–${format.format(Math.min((page + 1) * limit, matching))} of ${format.format(matching)} ${filtered ? 'matching rows' : 'rows'}`
      : filtered ? 'No matching rows' : '0 rows';
    return filtered ? `${range} (${format.format(total)} total)` : range;
  }
  function unfinishedTab(button, incomplete, failed) {
    const format = new Intl.NumberFormat('en');
    const counts = [{kind: 'incomplete', count: incomplete}, {kind: 'failed', count: failed}];
    button.textContent = 'Unfinished checks';
    const present = counts.filter(item => item.count > 0);
    if (present.length) {
      button.append(document.createTextNode(' ('));
      present.forEach((item, index) => {
        if (index) button.append(document.createTextNode(' · '));
        const count = document.createElement('span'); count.className = 'severity-count';
        const icon = document.createElement('span'); icon.setAttribute('aria-hidden', 'true');
        icon.textContent = unfinishedIcon(item.kind);
        count.append(icon, document.createTextNode(` ${format.format(item.count)}`));
        button.append(count);
      });
      button.append(document.createTextNode(')'));
    }
    button.title = `Unfinished checks — ${counts.map(item => `${unfinishedIcon(item.kind)} ${format.format(item.count)} ${item.kind}`).join(', ')}`;
    button.setAttribute('aria-label', `Unfinished checks — ${counts.map(item => `${format.format(item.count)} ${item.kind}`).join(', ')}`);
  }
  function unfinishedIcon(kind) {
    return {incomplete: '⏳', failed: '❌'}[kind] || '';
  }
  function unfinishedStatus(cell, kind) {
    const icon = unfinishedIcon(kind);
    if (!icon) return;
    const marker = document.createElement('span'); marker.textContent = icon;
    marker.setAttribute('aria-hidden', 'true');
    cell.replaceChildren(marker, document.createTextNode(` ${kind === 'incomplete' ? 'Incomplete' : 'Failed'}`));
    cell.title = kind === 'incomplete'
      ? 'The check could not finish.'
      : 'An execution error prevented the check from completing.';
  }
  function checkMessage(row) {
    const message = String(row.message ?? '');
    if (!['incomplete', 'failed'].includes(row.kind) || !row.sheet || !row.field) return message;
    const field = String(row.field);
    const location = field.startsWith(`${row.sheet}.`) ? field : `${row.sheet}.${field}`;
    const prefix = `${location}: `;
    if (message.startsWith(prefix)) return message.slice(prefix.length);
    // Remove a source category only when followed by the exact structured location.
    const category = message.match(/^[a-z_]+: /);
    if (category && message.slice(category[0].length).startsWith(prefix)) {
      return message.slice(category[0].length + prefix.length);
    }
    return message;
  }
  // A sheet group shares reset state, including filters waiting for an unloaded sheet.
  function filterScope(clearPending = () => {}, hasPending = () => false) {
    const members = [];
    function refresh() {
      const active = hasPending() || members.some(member => member.active());
      members.forEach(member => { member.button.disabled = !active; });
    }
    return {
      refresh,
      register(button, active, clear) {
        members.push({button, active, clear});
        button.onclick = () => {
          clearPending();
          members.forEach(member => member.clear());
          refresh();
        };
      }
    };
  }
  function resetControl(controls, search, inputs, clear, scope = filterScope()) {
    const button = document.createElement('button'); button.type = 'button';
    button.textContent = 'Reset filters'; controls.append(button);
    scope.register(button, () => hasFilters(search, inputs), clear);
    return scope.refresh;
  }
  // Keep choices useful for the current result count without rendering unbounded tables.
  function pageSizes(select, count, preferred = 20) {
    const maximum = Math.max(1, Math.min(count, 1000));
    const choices = [...[10, 20, 50, 100, 200, 500, 1000].filter(n => n < maximum), maximum];
    const signature = `${count <= 1000 ? count : 'large'}:${choices.join(',')}`;
    if (select.dataset.sizes !== signature) {
      select.replaceChildren(...choices.map(n => {
        const option = document.createElement('option');
        option.value = n;
        option.textContent = count === 0 ? '0' : n === count ? `All (${count})` : String(n);
        return option;
      }));
      select.dataset.sizes = signature;
    }
    const chosen = choices.find(n => n >= Math.min(preferred, maximum));
    select.value = chosen;
    select.disabled = count === 0 || choices.length === 1;
    return chosen;
  }
  return {pageSizes, loadCSV, feedback, tabs, bindCard, applyFilters, matchesFilter, hasFilters, paginationSummary, unfinishedTab, unfinishedStatus, checkMessage, filterScope, resetControl, yieldPaint};
})();
