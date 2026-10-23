/* Completed-report aggregation. Local CSV data; bounded, paginated DOM. */
'use strict';
(() => {
  const countFormat = new Intl.NumberFormat('en');
  const el = (tag, text, parent) => {
    const node = document.createElement(tag); if (text !== undefined) node.textContent = text;
    if (parent) parent.append(node); return node;
  };
  function severityDot(parent, severity) {
    const dot = el('span', undefined, parent);
    dot.className = 'severity-dot'; dot.dataset.severity = severity;
    dot.setAttribute('aria-hidden', 'true');
  }
  function severityTab(button, name, counts) {
    button.textContent = name;
    const present = counts.filter(item => item.count > 0);
    const description = `${name} — ${counts.map(item => `${countFormat.format(item.count)} ${item.severity === 'info' ? (item.count === 1 ? 'information finding' : 'information findings') : (item.count === 1 ? item.severity : `${item.severity}s`)}`).join(', ')}`;
    button.title = description;
    button.setAttribute('aria-label', description);
    if (!present.length) return;
    button.append(document.createTextNode(' ('));
    present.forEach((item, i) => {
      if (i) button.append(document.createTextNode(' · '));
      const count = el('span', undefined, button); count.className = 'severity-count';
      severityDot(count, item.severity);
      count.append(document.createTextNode(countFormat.format(item.count)));
    });
    button.append(document.createTextNode(')'));
  }
  const ui = window.ValidationReportUI;
  const load = ui.loadCSV;
  const table = (parent, data, columns, fileLinks = false, pageSize = 20, filename = 'files', scope) =>
    window.ValidationReportTable(parent, data, columns, {fileLinks, pageSize, filename, scope});
  function findings(parent, data, id, sheetOrder = []) {
    const columns = ['field', 'model_version', 'severity', 'stage', 'value', 'message', 'occurrences', 'affected_files'];
    if (!data.rows.length) { el('p', 'None.', parent); return; }
    const groups = new Map();
    data.rows.forEach(row => {
      if (!groups.has(row.sheet)) groups.set(row.sheet, []);
      groups.get(row.sheet).push(row);
    });
    const rank = sheet => { const index = sheetOrder.indexOf(sheet); return index < 0 ? Infinity : index; };
    return ui.tabs(parent, [...groups].sort((a, b) => rank(a[0]) - rank(b[0])).map(([sheet, rows], index) => ({
      id: String(index), label: sheet || 'Form',
      decorate(button) {
        severityTab(button, sheet || 'Form', ['error', 'warning', 'info'].map(severity => ({severity,
          count: rows.reduce((total, row) => total + (row.severity === severity ? Number(row.occurrences) : 0), 0)})));
      },
      matches: filters => rows.some(row => Object.entries(filters).every(([key, value]) => row[key] === value)),
      render: (content, update, scope) => table(content, rows, columns, false, 20, `${id}-sheet-${index + 1}`, scope)
    })), id);
  }
  async function main() {
    const root = document.getElementById('aggregate');
    const response = await fetch('aggregate.json', {credentials: 'same-origin', cache: 'no-store'});
    if (!response.ok) throw Error(`aggregate.json: HTTP ${response.status}`);
    const meta = await response.json();
    const nav = document.getElementById('breadcrumbs');
    meta.breadcrumbs.forEach((item, i) => {
      if (i) nav.append(document.createTextNode(' / '));
      if (i === meta.breadcrumbs.length - 1) {
        el('span', item.label, nav).setAttribute('aria-current', 'page');
      } else el('a', item.label, nav).href = item.href;
    });
    const overview = document.getElementById('overview');
    const totals = meta.totals[0];
    const statuses = Object.fromEntries(meta.statuses.map(row => [row.Status, row.Files]));
    const format = new Intl.NumberFormat('en');
    function setCount(node, count) {
      node.textContent = format.format(count);
      node.closest('.overview-card').dataset.empty = String(count === 0);
    }
    overview.querySelectorAll('[data-total]').forEach(node => setCount(node, Number(totals[node.dataset.total] || 0)));
    overview.querySelectorAll('[data-status]').forEach(node => setCount(node, Number(statuses[node.dataset.status] || 0)));
    const versions = document.getElementById('overview-versions');
    const template = document.getElementById('overview-version-card');
    meta.versions.forEach(version => {
      const card = template.content.firstElementChild.cloneNode(true);
      setCount(card.querySelector('strong'), Number(version.files));
      card.querySelector('span').textContent = `V${version.version}`;
      versions.append(card);
    });
    meta.notes.forEach(note => el('p', note, document.getElementById('overview-notes')));
    if (meta.children.length) {
      const navigation = document.getElementById('aggregate-children');
      el('h2', 'Browse reports', navigation);
      const list = el('ul', undefined, navigation);
      meta.children.forEach(item => { el('a', item.label, el('li', undefined, list)).href = item.href; });
    }

    progress.finish();
    const definitions = [
      {id: 'files', label: `Forms (${countFormat.format(Number(totals.files))})`,
        decorate(button) {
          const count = Number(totals.files);
          button.title = `Forms — ${countFormat.format(count)} ${count === 1 ? 'form' : 'forms'} included in this aggregate report`;
          button.setAttribute('aria-label', button.title);
        },
        render: async (content, update) => table(content, (await load('files.csv', update)).rows,
        ['input', 'model_version', 'status', 'errors', 'warnings'], true, 10)},
      {id: 'findings', label: 'Validation findings',
        decorate: button => severityTab(button, 'Validation findings', [{severity: 'error', count: Number(totals.errors)}, {severity: 'warning', count: Number(totals.warnings)}, {severity: 'info', count: Number(totals.infos || 0)}]),
        render: async (content, update) => findings(content, await load('issues.csv', update), 'findings', meta.sheet_order)}
    ];
    if (Number(totals.incomplete_checks) + Number(totals.failed_checks)) definitions.push(
      {id: 'execution', label: 'Unfinished checks',
        decorate: button => ui.unfinishedTab(button, Number(totals.incomplete_checks), Number(totals.failed_checks)),
        render: async (content, update) => table(content, (await load('execution.csv', update)).rows.map(row => ({
          ...row, sheet: row.sheet || 'Form', check: row.rule_id.replaceAll('_', ' ').replaceAll('/', ' / ')
        })), ['check', 'model_version', 'sheet', 'field', 'stage', 'kind', 'message', 'affected_files'], false, 20, 'execution-checks')});
    const details = ui.tabs(root, definitions, 'details', 'Details', true);
    overview.querySelectorAll('[data-status]').forEach(node => {
      const status = node.dataset.status;
      ui.bindCard(node.closest('.overview-card'), Number(statuses[status] || 0),
        () => details.open('files', {status}, true), `Show ${status} forms`, 'View forms');
    });
    for (const [count, target, key, value] of [
      ['errors', 'findings', 'severity', 'error'], ['warnings', 'findings', 'severity', 'warning'], ['infos', 'findings', 'severity', 'info'],
      ['incomplete_checks', 'execution', 'kind', 'incomplete'], ['failed_checks', 'execution', 'kind', 'failed']]) {
      ui.bindCard(overview.querySelector(`[data-total="${count}"]`).closest('.overview-card'), Number(totals[count] || 0),
        () => details.open(target, {[key]: value}, true), `Show ${value} ${key === 'severity' ? 'findings' : 'checks'}`,
        key === 'severity' ? 'View findings' : 'View checks');
    }
    [...versions.children].forEach((card, index) => {
      const version = meta.versions[index];
      ui.bindCard(card, Number(version.files), () => details.open('files', {model_version: version.version}, true), `Show forms for model ${version.version}`, 'View forms');
    });
  }

  const progress = ui.feedback(document.getElementById('loading'));
  async function run() {
    progress.start('Loading summary…');
    document.getElementById('breadcrumbs').replaceChildren();
    document.getElementById('overview-versions').replaceChildren();
    document.getElementById('overview-notes').replaceChildren();
    document.getElementById('aggregate-children').replaceChildren();
    document.getElementById('aggregate').replaceChildren();
    try { await main(); } catch (error) { progress.fail(error, run); }
  }
  run();
})();
