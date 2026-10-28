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
    const overview = document.getElementById('overview');
    const totals = Object.fromEntries([...overview.querySelectorAll('[data-total]')]
      .map(node => [node.dataset.total, Number(node.dataset.value)]));
    totals.files = Number(document.querySelector('main').dataset.files);
    const statuses = Object.fromEntries([...overview.querySelectorAll('[data-status]')]
      .map(node => [node.dataset.status, Number(node.dataset.value)]));
    const versions = document.getElementById('overview-versions');
    const sheetOrder = [...document.getElementById('aggregate-sheets').content.querySelectorAll('[data-sheet]')]
      .map(node => node.dataset.sheet);

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
        render: async (content, update) => findings(content, await load('issues.csv', update), 'findings', sheetOrder)}
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
    [...versions.children].forEach(card => {
      const version = {version: card.dataset.version, files: Number(card.querySelector('strong').dataset.value)};
      ui.bindCard(card, Number(version.files), () => details.open('files', {model_version: version.version}, true), `Show forms for model ${version.version}`, 'View forms');
    });
  }

  const progress = ui.feedback(document.getElementById('loading'));
  async function run() {
    progress.start('Loading tables…');
    document.getElementById('aggregate').replaceChildren();
    try { await main(); } catch (error) { progress.fail(error, run); }
  }
  run();
})();
