/* CSV-backed validation report. No external runtime dependencies. */
'use strict';
(() => {
  const countFormat = new Intl.NumberFormat('en');
  const labels = {severity: 'Severity', stage: 'Stage', field_label: 'Field', column: 'Column',
    row: 'Row', value: 'Value', message: 'Message', count: 'Count', sheet: 'Sheet',
    field: 'Field', rule_id: 'Check', kind: 'Status'};
  const el = (tag, text, parent) => {
    const node = document.createElement(tag);
    if (text !== undefined) node.textContent = text;
    if (parent) parent.append(node);
    return node;
  };
  function severityDot(parent, severity) {
    const dot = el('span', undefined, parent);
    dot.className = 'severity-dot'; dot.dataset.severity = severity;
    dot.setAttribute('aria-hidden', 'true');
  }
  function severityTab(button, name, counts) {
    button.textContent = name;
    const present = counts.filter(item => item.count > 0);
    const description = `${name} — ${counts.map(item => `${countFormat.format(item.count)} ${item.severity === 'error' ? (item.count === 1 ? 'error' : 'errors') : (item.count === 1 ? 'warning' : 'warnings')}`).join(', ')}`;
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
  function table(parent, data, columns, filename, initial = 20, scope) {
    let filterTimer;
    function scheduleFilter() { clearTimeout(filterTimer); filterTimer = setTimeout(render, 250); }
    const controls = el('div', undefined, parent); controls.className = 'controls';
    const searchLabel = el('label', 'Search ', controls);
    const search = el('input', undefined, searchLabel); search.type = 'search';
    const sizeLabel = el('label', 'Rows ', controls);
    const size = el('select', undefined, sizeLabel);
    let preferredSize = initial;
    const download = el('button', 'Export CSV', controls);
    const wrap = el('div', undefined, parent); wrap.className = 'table-scroll';
    const t = el('table', undefined, wrap), head = el('thead', undefined, t);
    const headers = el('tr', undefined, head), filters = el('tr', undefined, head);
    const filterInputs = {}; let sort = null, descending = false, page = 0, selected = data;
    columns.forEach(key => {
      const th = el('th', undefined, headers); th.scope = 'col';
      const button = el('button', labels[key] || key, th);
      button.onclick = () => { descending = sort === key ? !descending : false; sort = key; render(); };
      const input = el('input', undefined, el('th', undefined, filters)); input.type = 'search';
      input.setAttribute('aria-label', `Filter ${labels[key] || key}`);
      input.oninput = () => { delete input.dataset.exact; page = 0; scheduleFilter(); }; filterInputs[key] = input;
    });
    const body = el('tbody', undefined, t), nav = el('div', undefined, parent); nav.className = 'controls';
    const previous = el('button', 'Previous', nav), info = el('span', '', nav), next = el('button', 'Next', nav);
    previous.onclick = () => { page--; render(); }; next.onclick = () => { page++; render(); };
    search.oninput = () => { page = 0; scheduleFilter(); };
    size.onchange = () => { preferredSize = Number(size.value); page = 0; render(); };
    download.onclick = () => {
      if (filterTimer !== undefined) render();
      const quote = value => '"' + String(value ?? '').replaceAll('"', '""') + '"';
      const content = [columns.map(k => quote(labels[k] || k)).join(','),
        ...selected.map(row => columns.map(k => quote(row[k])).join(','))].join('\r\n');
      const url = URL.createObjectURL(new Blob([content], {type: 'text/csv;charset=utf-8'}));
      const link = el('a'); link.href = url; link.download = filename + '.csv'; link.click();
      setTimeout(() => URL.revokeObjectURL(url), 1000);
    };
    const refreshReset = ui.resetControl(controls, search, filterInputs, () => setFilters({}), scope);
    function setFilters(filters) { ui.applyFilters(search, filterInputs, filters); page = 0; render(); }
    function render() {
      clearTimeout(filterTimer); filterTimer = undefined;
      refreshReset();
      const query = search.value.toLowerCase();
      selected = data.filter(row => columns.some(k => String(row[k] ?? '').toLowerCase().includes(query)) &&
        columns.every(k => ui.matchesFilter(filterInputs[k], row[k])));
      if (sort) selected.sort((a, b) => String(a[sort] ?? '').localeCompare(String(b[sort] ?? ''), undefined,
        {numeric: ['row', 'column', 'count'].includes(sort)}) * (descending ? -1 : 1));
      const limit = ui.pageSizes(size, selected.length, preferredSize);
      page = Math.min(page, Math.max(0, Math.ceil(selected.length / limit) - 1));
      body.replaceChildren();
      selected.slice(page * limit, (page + 1) * limit).forEach(row => {
        const tr = el('tr', undefined, body);
        columns.forEach(key => {
          const td = el('td', row[key] ?? '', tr);
          if (key === 'count') { td.textContent = countFormat.format(Number(row[key])); td.className = 'report-count'; }
          if (key === 'severity') {
            td.replaceChildren(); severityDot(td, row[key]);
            td.append(document.createTextNode(row[key] ?? ''));
          }
          if (key === 'value') {
            const raw = String(row[key] ?? '');
            td.replaceChildren();
            const value = el('code', raw === '' ? '(empty)' : raw.replace(/\r\n|\r|\n/g, '↵').replace(/\t/g, '⇥').replace(/ /g, '·'), td);
            value.className = 'report-value';
            value.title = 'Visible whitespace: · space, ⇥ tab, ↵ line break. CSV exports retain the original value.';
          }
          if (key === 'stage') td.textContent = `${{Structure: '📐', Field: '🔎', Business: '📋'}[row[key]] || ''} ${row[key]}`;
          if (key === 'kind') ui.unfinishedStatus(td, row[key]);
          if (key === 'message') td.textContent = ui.checkMessage(row);
          if (key === 'sheet' && !row[key]) td.textContent = 'Form';
          if (key === 'field') td.textContent = row[key] || '—';
        });
      });
      info.textContent = ui.paginationSummary(page, limit, selected.length, data.length, ui.hasFilters(search, filterInputs));
      previous.disabled = page === 0; next.disabled = (page + 1) * limit >= selected.length;
    }
    render();
    return {setFilters};
  }
  function sections(parent, groups, summary) {
    if (!groups.length) { el('p', 'No validation messages in this section.', parent); return {}; }
    return ui.tabs(parent, groups.map(([name, rows], index) => ({
      id: String(index), label: name || 'Form',
      decorate(button) {
        severityTab(button, name || 'Form', ['error', 'warning'].map(severity => ({severity,
          count: rows.filter(row => row.severity === severity).length})));
      },
      matches: filters => rows.some(row => Object.entries(filters).every(([key, value]) => row[key] === value)),
      async render(content, update, scope) {
        update('Preparing table…');
        let columns = ['severity', 'stage', 'field_label', 'column', 'row', 'value', 'message'], data = rows;
        if (summary) {
          columns = columns.filter(key => key !== 'row');
          if (rows[0].layout === 'cells') columns = columns.filter(key => key !== 'column');
          else {
            const grouped = new Map();
            for (let i = 0; i < rows.length; i++) {
              if (i % 2000 === 0) await ui.yieldPaint();
              const row = rows[i], key = JSON.stringify(columns.map(column => row[column]));
              if (!grouped.has(key)) grouped.set(key, {...row, count: 0});
              grouped.get(key).count++;
            }
            data = [...grouped.values()]; columns.push('count');
          }
        }
        return table(content, data, columns, `${summary ? 'summary' : 'details'}-sheet-${index + 1}`, 20, scope);
      }
    })), summary ? 'summary' : 'messages');
  }
  async function main() {
    const summaries = await load('summary.csv', progress.update);
    const columns = ['input', 'status', 'structure_passed', 'generated_at', 'model_version', 'rules_version', 'errors', 'warnings', 'incomplete', 'failed'];
    columns.forEach(c => { if (!summaries.header.includes(c)) throw new Error(`Missing CSV column: ${c}`); });
    if (summaries.rows.length !== 1) throw new Error('Expected one summary row');
    const s = summaries.rows[0], root = document.getElementById('report');
    const overview = document.getElementById('findings-overview').content.firstElementChild.cloneNode(true);
    const number = new Intl.NumberFormat('en');
    overview.querySelectorAll('[data-count]').forEach(node => {
      const count = Number(s[node.dataset.count]);
      node.textContent = number.format(count);
      node.closest('.overview-card').dataset.empty = String(count === 0);
    });
    const unfinishedCount = Number(s.incomplete) + Number(s.failed);
    const descriptions = {
      valid: 'This form passed validation.',
      invalid: 'This form has not passed validation.',
      incomplete: `${number.format(unfinishedCount)} checks could not finish. This form has not passed validation.`,
      failed: 'Validation failed to complete. This form has not passed validation.'
    };
    const metadata = {
      generated_at: `Generated ${s.generated_at.replace('T', ' ').replace('Z', ' UTC')}`,
      status: s.status === 'valid' ? 'Validation passed' : `Validation ${s.status}`,
      explanation: s.structure_passed !== 'TRUE'
        ? 'Structure validation did not pass. Field and business checks were not run.'
        : descriptions[s.status],
      structure: s.structure_passed === 'TRUE' ? 'Passed' : 'Not passed',
      model_version: s.model_version, rules_version: s.rules_version
    };
    document.getElementById('report-generated').textContent = metadata.generated_at;
    overview.querySelectorAll('[data-meta]').forEach(node => { node.textContent = metadata[node.dataset.meta]; });
    const statusIcon = {valid: '✅', invalid: '🔴', incomplete: '⏳', failed: '❌'}[s.status];
    if (statusIcon) {
      const icon = document.createElement('span'); icon.textContent = statusIcon;
      icon.setAttribute('aria-hidden', 'true');
      overview.querySelector('[data-meta="status"]').prepend(icon, document.createTextNode(' '));
    }
    overview.querySelector('.overview-outcome').dataset.status = s.status;
    document.getElementById('report-overview').replaceChildren(overview);
    progress.finish();
    let prepared;
    async function getFindings(update) {
      if (!prepared) prepared = (async () => {
        const findings = await load('findings.csv', update);
        ['rule_id', 'kind', 'severity', 'stage', 'sheet', 'field', 'row', 'column', 'value', 'message', 'sheet_order', 'layout', 'field_label'].forEach(key => {
          if (!findings.header.includes(key)) throw new Error(`Missing CSV column: ${key}`);
        });
        if (findings.rows.some(row => !['cells', 'table'].includes(row.layout))) throw new Error('Report metadata is missing');
        const groups = new Map();
        findings.rows.filter(row => row.kind === 'validation').forEach(row => {
          if (!groups.has(row.sheet)) groups.set(row.sheet, []); groups.get(row.sheet).push(row);
        });
        return {groups: [...groups].sort((a, b) => (Number(a[1][0].sheet_order) || Infinity) - (Number(b[1][0].sheet_order) || Infinity)),
          unfinished: findings.rows.filter(row => ['incomplete', 'failed'].includes(row.kind))};
      })().catch(error => { prepared = null; throw error; });
      return prepared;
    }
    const definitions = [
      {id: 'summary', label: 'Summary',
        decorate: button => severityTab(button, 'Summary', [{severity: 'error', count: Number(s.errors)}, {severity: 'warning', count: Number(s.warnings)}]),
        render: async (content, update) => sections(content, (await getFindings(update)).groups, true)},
      {id: 'messages', label: 'Validation messages',
        decorate: button => severityTab(button, 'Validation messages', [{severity: 'error', count: Number(s.errors)}, {severity: 'warning', count: Number(s.warnings)}]),
        render: async (content, update) => sections(content, (await getFindings(update)).groups, false)}
    ];
    if (unfinishedCount) definitions.push({id: 'execution', label: 'Unfinished checks',
      decorate: button => ui.unfinishedTab(button, Number(s.incomplete), Number(s.failed)),
      render: async (content, update) =>
      table(content, (await getFindings(update)).unfinished, ['severity', 'stage', 'sheet', 'field', 'rule_id', 'kind', 'message'], 'unfinished-checks')});
    const details = ui.tabs(root, definitions, 'details', 'Details', true);
    for (const [count, target, key, value] of [
      ['errors', 'summary', 'severity', 'error'], ['warnings', 'summary', 'severity', 'warning'],
      ['incomplete', 'execution', 'kind', 'incomplete'], ['failed', 'execution', 'kind', 'failed']]) {
      ui.bindCard(overview.querySelector(`[data-count="${count}"]`).closest('.overview-card'), Number(s[count]),
        () => details.open(target, {[key]: value}, true), `Show ${value} ${key === 'severity' ? 'findings' : 'checks'}`,
        key === 'severity' ? 'View findings' : 'View checks');
    }
  }

  const progress = ui.feedback(document.getElementById('loading'));
  async function run() {
    progress.start('Loading summary…');
    try { await main(); } catch (error) { progress.fail(error, run); }
  }
  run();
})();
