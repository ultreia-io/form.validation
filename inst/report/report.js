/* CSV-backed validation report. No external runtime dependencies. */
'use strict';
(() => {
  const labels = {severity: 'Severity', stage: 'Stage', field_label: 'Field', column: 'Column',
    row: 'Row', value: 'Value', message: 'Message', count: 'Count', sheet: 'Sheet',
    field: 'Field', rule_id: 'Check', kind: 'Status'};
  const el = (tag, text, parent) => {
    const node = document.createElement(tag);
    if (text !== undefined) node.textContent = text;
    if (parent) parent.append(node);
    return node;
  };
  // RFC-style CSV: preserve strings, escaped quotes, commas and multiline cells.
  function csv(text) {
    const rows = []; let row = [], value = '', quoted = false;
    text = text.replace(/^\uFEFF/, '');
    for (let i = 0; i < text.length; i++) {
      const c = text[i];
      if (c === '"') {
        if (quoted && text[i + 1] === '"') { value += '"'; i++; }
        else quoted = !quoted;
      } else if (!quoted && (c === ',' || c === '\n' || c === '\r')) {
        row.push(value); value = '';
        if (c !== ',') {
          rows.push(row); row = [];
          if (c === '\r' && text[i + 1] === '\n') i++;
        }
      } else value += c;
    }
    if (quoted) throw new Error('Unclosed quoted CSV value');
    if (value || row.length) { row.push(value); rows.push(row); }
    const header = rows.shift();
    if (!header) throw new Error('CSV header is missing');
    return {header, rows: rows.map(values => {
      if (values.length !== header.length) throw new Error('CSV column count differs from header');
      return Object.fromEntries(header.map((key, i) => [key, values[i]]));
    })};
  }
  async function load(name) {
    const response = await fetch(name, {credentials: 'same-origin', cache: 'no-cache'});
    if (!response.ok) throw new Error(`${name}: HTTP ${response.status}`);
    return csv(await response.text());
  }
  function table(parent, data, columns, filename, initial = 20) {
    const controls = el('div', undefined, parent); controls.className = 'controls';
    const searchLabel = el('label', 'Search ', controls);
    const search = el('input', undefined, searchLabel); search.type = 'search';
    const sizeLabel = el('label', 'Rows ', controls);
    const size = el('select', undefined, sizeLabel);
    [10, 20, 50, 100, -1].forEach(n => { const o = el('option', n < 0 ? 'All' : String(n), size); o.value = n; });
    size.value = initial;
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
      input.oninput = () => { page = 0; render(); }; filterInputs[key] = input;
    });
    const body = el('tbody', undefined, t), nav = el('div', undefined, parent); nav.className = 'controls';
    const previous = el('button', 'Previous', nav), info = el('span', '', nav), next = el('button', 'Next', nav);
    previous.onclick = () => { page--; render(); }; next.onclick = () => { page++; render(); };
    search.oninput = size.onchange = () => { page = 0; render(); };
    download.onclick = () => {
      const quote = value => '"' + String(value ?? '').replaceAll('"', '""') + '"';
      const content = [columns.map(k => quote(labels[k] || k)).join(','),
        ...selected.map(row => columns.map(k => quote(row[k])).join(','))].join('\r\n');
      const url = URL.createObjectURL(new Blob([content], {type: 'text/csv;charset=utf-8'}));
      const link = el('a'); link.href = url; link.download = filename + '.csv'; link.click();
      setTimeout(() => URL.revokeObjectURL(url), 1000);
    };
    function render() {
      const query = search.value.toLowerCase();
      selected = data.filter(row => columns.some(k => String(row[k] ?? '').toLowerCase().includes(query)) &&
        columns.every(k => String(row[k] ?? '').toLowerCase().includes(filterInputs[k].value.toLowerCase())));
      if (sort) selected.sort((a, b) => String(a[sort] ?? '').localeCompare(String(b[sort] ?? ''), undefined,
        {numeric: ['row', 'column', 'count'].includes(sort)}) * (descending ? -1 : 1));
      const limit = Number(size.value) < 0 ? Math.max(selected.length, 1) : Number(size.value);
      page = Math.min(page, Math.max(0, Math.ceil(selected.length / limit) - 1));
      body.replaceChildren();
      selected.slice(page * limit, (page + 1) * limit).forEach(row => {
        const tr = el('tr', undefined, body);
        columns.forEach(key => {
          const td = el('td', row[key] ?? '', tr);
          if (key === 'severity') {
            const icons = {error: '🔴', warning: '🟠', info: '🔵'};
            td.textContent = `${icons[row[key]] || ''} ${row[key]}`;
          }
          if (key === 'stage') td.textContent = `${{Structure: '📐', Field: '🔎', Business: '📋'}[row[key]] || ''} ${row[key]}`;
        });
      });
      info.textContent = `${selected.length ? page * limit + 1 : 0}–${Math.min((page + 1) * limit, selected.length)} of ${selected.length}`;
      previous.disabled = page === 0; next.disabled = (page + 1) * limit >= selected.length;
    }
    render();
  }
  function sections(parent, title, groups, summary) {
    el('h2', title, parent);
    if (!groups.length) { el('p', 'No validation messages in this section.', parent); return; }
    const tabs = el('div', undefined, parent); tabs.className = 'tabs'; tabs.setAttribute('role', 'tablist');
    const panels = [], buttons = [];
    groups.forEach(([name, rows], i) => {
      const counts = ['error', 'warning'].map(level => {
        const count = rows.filter(r => r.severity === level).length;
        return count ? `${level === 'error' ? 'E' : 'W'}:${count}` : '';
      }).filter(Boolean).join(' ');
      const button = el('button', `${name || 'Workbook'}${counts ? ` (${counts})` : ''}`, tabs); buttons.push(button);
      const panel = el('div', undefined, parent); panels.push(panel); panel.hidden = i !== 0;
      button.setAttribute('role', 'tab'); button.setAttribute('aria-selected', String(i === 0));
      button.id = `${summary ? 'summary' : 'details'}-tab-${i}`;
      panel.id = `${button.id}-panel`; panel.setAttribute('role', 'tabpanel');
      panel.setAttribute('aria-labelledby', button.id); button.setAttribute('aria-controls', panel.id);
      button.onclick = () => panels.forEach((p, j) => { p.hidden = i !== j; buttons[j].setAttribute('aria-selected', String(i === j)); });
      let columns = ['severity', 'stage', 'field_label', 'column', 'row', 'value', 'message'], data = rows;
      if (summary) {
        columns = columns.filter(c => c !== 'row');
        if (rows[0].layout === 'cells') columns = columns.filter(c => c !== 'column');
        else {
          const grouped = new Map();
          rows.forEach(row => {
            const key = JSON.stringify(columns.map(c => row[c]));
            if (!grouped.has(key)) grouped.set(key, {...row, count: 0});
            grouped.get(key).count++;
          });
          data = [...grouped.values()]; columns.push('count');
        }
      }
      table(panel, data, columns, `${summary ? 'summary' : 'details'}-sheet-${i + 1}`, summary ? -1 : 20);
    });
  }
  async function main() {
    const [summaries, findings] = await Promise.all([load('summary.csv'), load('findings.csv')]);
    const required = [
      [summaries, ['input', 'status', 'structure_passed', 'generated_at', 'model_version', 'rules_version', 'errors', 'warnings', 'incomplete', 'failed']],
      [findings, ['rule_id', 'kind', 'severity', 'stage', 'sheet', 'field', 'row', 'column', 'value', 'message', 'sheet_order', 'layout', 'field_label']]
    ];
    required.forEach(([data, columns]) => columns.forEach(c => { if (!data.header.includes(c)) throw new Error(`Missing CSV column: ${c}`); }));
    if (summaries.rows.length !== 1) throw new Error('Expected one summary row');
    if (findings.rows.some(r => !['cells', 'table'].includes(r.layout))) {
      throw new Error('Report metadata is missing; export validation_report_data() rather than a raw result');
    }
    const s = summaries.rows[0], root = document.getElementById('report');
    el('p', `Last updated: ${s.generated_at}`, root); el('h2', 'Abstract', root);
    el('p', `Checks done on file: ${s.input}`, root);
    el('p', s.status === 'valid' ? 'Validation passed.' : `Status: ${s.status}`, root);
    el('p', `Model version: ${s.model_version} — Rules version: ${s.rules_version}`, root);
    const counts = el('ul', undefined, root);
    ['errors', 'warnings', 'incomplete', 'failed'].forEach(k => { if (Number(s[k]) > 0) el('li', `${s[k]} ${k}`, counts); });
    if (s.structure_passed !== 'TRUE') el('strong',
      'Field and business checks were not run because structure validation did not pass.', el('p', undefined, root));
    if (['incomplete', 'failed'].includes(s.status)) el('p', 'Validation could not finish. This input has not passed validation.', root);
    const groups = new Map();
    findings.rows.filter(r => r.kind === 'validation').forEach(r => {
      if (!groups.has(r.sheet)) groups.set(r.sheet, []); groups.get(r.sheet).push(r);
    });
    const ordered = [...groups].sort((a, b) => (Number(a[1][0].sheet_order) || Infinity) - (Number(b[1][0].sheet_order) || Infinity));
    sections(root, 'Summary', ordered, true); sections(root, 'Validation messages', ordered, false);
    const unfinished = findings.rows.filter(r => ['incomplete', 'failed'].includes(r.kind));
    if (unfinished.length) {
      el('h2', 'Checks that could not finish', root);
      table(root, unfinished, ['severity', 'stage', 'sheet', 'field', 'rule_id', 'kind', 'message'], 'unfinished-checks');
    }
    document.getElementById('loading').remove();
  }
  main().catch(error => {
    const loading = document.getElementById('loading'); loading.className = 'error';
    loading.textContent = `Cannot load report: ${error.message}. Serve the HTML, CSV files and assets together over HTTP.`;
  });
})();
