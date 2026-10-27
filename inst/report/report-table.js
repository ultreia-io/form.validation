/* Shared review tables: filtering, sorting, pagination and CSV export. */
'use strict';
window.ValidationReportTable = (() => {
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
  const ui = window.ValidationReportUI;
  function table(parent, data, columns, options = {}) {
    const {fileLinks = false, pageSize: initialSize = 20, filename = 'files', scope, labels: customLabels = {}, renderCell,
      exportColumns = columns, exportRows = rows => rows} = options;
    let pageSize = initialSize;
    if (!data.length) { el('p', 'None.', parent); return; }
    let filterTimer;
    function scheduleFilter() { clearTimeout(filterTimer); filterTimer = setTimeout(render, 250); }
    const controls = el('div', undefined, parent); controls.className = 'controls';
    const label = el('label', 'Search ', controls), search = el('input', undefined, label); search.type = 'search';
    const sizeLabel = el('label', 'Rows ', controls), size = el('select', undefined, sizeLabel);
    let preferredSize = initialSize;
    const download = el('button', 'Export CSV', controls);
    const labels = {input: 'Reporting form', model_version: 'Model version', status: 'Status', errors: 'Errors', warnings: 'Warnings', sheet: 'Sheet', check: 'Check', field: 'Field', severity: 'Severity', stage: 'Stage', kind: 'Status', value: 'Value', message: filename.startsWith('execution-') ? 'Reason' : 'Message', occurrences: 'Occurrences', affected_files: 'Affected forms', reference: 'Code list', cells: 'Cells', ...customLabels};
    const labelFor = key => labels[key] || key.replaceAll('_', ' ').replace(/^./, value => value.toUpperCase());
    const wrap = el('div', undefined, parent); wrap.className = 'table-scroll';
    const t = el('table', undefined, wrap), thead = el('thead', undefined, t);
    const head = el('tr', undefined, thead), filters = el('tr', undefined, thead), filterInputs = {};
    let sort = '', descending = false, selected = data;
    columns.forEach(key => {
      const button = el('button', labelFor(key), el('th', undefined, head));
      const input = el('input', undefined, el('th', undefined, filters)); input.type = 'search';
      input.setAttribute('aria-label', `Filter ${labelFor(key)}`);
      input.oninput = () => { delete input.dataset.exact; page = 0; scheduleFilter(); }; filterInputs[key] = input;
      button.onclick = () => { descending = sort === key ? !descending : false; sort = key; render(); };
    });
    const body = el('tbody', undefined, t), nav = el('div', undefined, parent); nav.className = 'controls';
    const previous = el('button', 'Previous', nav), status = el('span', '', nav), next = el('button', 'Next', nav);
    status.setAttribute('role', 'status'); let page = 0;
    const refreshReset = ui.resetControl(controls, search, filterInputs, () => setFilters({}), scope);
    function setFilters(filters) { ui.applyFilters(search, filterInputs, filters); page = 0; render(); }
    function render() {
      clearTimeout(filterTimer); filterTimer = undefined;
      refreshReset();
      const query = search.value.toLocaleLowerCase();
      selected = data.filter(row => columns.some(key => String(row[key] ?? '').toLocaleLowerCase().includes(query)) &&
        columns.every(key => ui.matchesFilter(filterInputs[key], row[key])));
      if (sort) selected.sort((a, b) => String(a[sort] ?? '').localeCompare(String(b[sort] ?? ''), undefined,
        {numeric: ['occurrences', 'affected_files', 'errors', 'warnings', 'failed', 'incomplete'].includes(sort)}) * (descending ? -1 : 1));
      pageSize = ui.pageSizes(size, selected.length, preferredSize);
      const pages = Math.max(1, Math.ceil(selected.length / pageSize)); page = Math.min(page, pages - 1); body.replaceChildren();
      selected.slice(page * pageSize, (page + 1) * pageSize).forEach(row => {
        const tr = el('tr', undefined, body);
        columns.forEach(key => {
          const td = el('td', undefined, tr);
          if (renderCell?.(td, key, row)) return;
          if (fileLinks && key === 'input') {
            const link = el('a', row[key], td); link.href = row.report.split('/').map(encodeURIComponent).join('/');
          } else if (['occurrences', 'affected_files', 'errors', 'warnings', 'failed', 'incomplete'].includes(key)) {
            td.textContent = countFormat.format(Number(row[key])); td.className = 'report-count';
          } else if (key === 'severity') {
            severityDot(td, row[key]);
            td.append(document.createTextNode(row[key] ?? ''));
          } else if (key === 'field') {
            const field = String(row[key] ?? '');
            const prefix = row.sheet ? `${row.sheet}.` : '';
            td.textContent = (prefix && field.startsWith(prefix) ? field.slice(prefix.length) : field) || '—';
          } else td.textContent = row[key] ?? '';
          if (key === 'status') {
            const icons = {valid: '✅', invalid: '🔴', incomplete: '⏳', failed: '❌'};
            const status = String(row[key] ?? '');
            if (icons[status]) {
              const icon = document.createElement('span'); icon.textContent = icons[status];
              icon.setAttribute('aria-hidden', 'true');
              td.replaceChildren(icon, document.createTextNode(` ${status[0].toUpperCase()}${status.slice(1)}`));
            }
          }
          if (key === 'stage') td.textContent = `${{Structure: '📐', Field: '🔎', Business: '📋'}[row[key]] || ''} ${row[key]}`;
          if (key === 'kind') ui.unfinishedStatus(td, row[key]);
          if (key === 'message') td.textContent = ui.checkMessage(row);
          if (key === 'value') {
            const raw = String(row[key] ?? '');
            td.replaceChildren();
            const value = el('code', raw === '' ? '(empty)' : raw.replace(/\r\n|\r|\n/g, '↵').replace(/\t/g, '⇥').replace(/ /g, '·'), td);
            value.className = 'report-value';
            value.title = 'Visible whitespace: · space, ⇥ tab, ↵ line break. CSV exports retain the original value.';
          }
        });
      });
      previous.disabled = page === 0; next.disabled = page === pages - 1;
      status.textContent = ui.paginationSummary(page, pageSize, selected.length, data.length, ui.hasFilters(search, filterInputs));
    }
    search.oninput = () => { page = 0; scheduleFilter(); };
    size.onchange = () => { preferredSize = Number(size.value); page = 0; render(); };
    download.onclick = () => {
      if (filterTimer !== undefined) render();
      const quote = value => '"' + String(value ?? '').replaceAll('"', '""') + '"';
      const content = [exportColumns.map(key => quote(labelFor(key))).join(','),
        ...exportRows(selected).map(row => exportColumns.map(key => quote(row[key])).join(','))].join('\r\n');
      const url = URL.createObjectURL(new Blob(['\uFEFF', content], {type: 'text/csv;charset=utf-8'}));
      const link = el('a'); link.href = url; link.download = filename + '.csv'; link.click();
      setTimeout(() => URL.revokeObjectURL(url), 1000);
    };
    previous.onclick = () => { page--; render(); }; next.onclick = () => { page++; render(); }; render();
    return {setFilters};
  }
  return table;
})();
