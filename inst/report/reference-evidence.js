/* Source CSVs retain form locations for downloads; browser tables group them for review. */
'use strict';
(() => {
  const ui = window.ValidationReportUI, table = window.ValidationReportTable;
  const format = new Intl.NumberFormat('en');
  const progress = ui.feedback(document.getElementById('loading'));
  const definitions = {
    'code-list': [{id: 'values', label: 'Codes to review', file: 'codes.csv'}],
    registry: [{id: 'values', label: 'Values to review', file: 'values.csv'}],
    conversion: [{id: 'values', label: 'Values to review', file: 'values.csv'}],
    'primary-key': [
      {id: 'missing', label: 'Missing components', file: 'missing-components.csv'},
      {id: 'duplicates', label: 'Duplicate keys', file: 'duplicate-keys.csv'},
      {id: 'parents', label: 'Missing parent keys', file: 'missing-parent-keys.csv'}
    ]
  };
  // Keep readable CSV headings in the UI while using the shared table's field keys.
  const columnKey = heading => ({'Submitted value': 'value', 'Code list': 'reference',
    Registry: 'reference', 'Affected forms': 'affected_files'}[heading]
    || heading.toLowerCase().replaceAll(' ', '_'));
  async function main() {
    const counts = document.querySelector('main').dataset;
    const root = document.getElementById('reference-review');
    root.replaceChildren();
    const panels = definitions[document.querySelector('main').dataset.reportKind].map(tab => ({
      id: tab.id, largeTable: Number(counts[tab.id]) >= 10000,
      label: `${tab.label} (${format.format(Number(counts[tab.id]))})`,
      async render(content, update) {
        const data = await ui.loadCSV(tab.file, update);
        const exportColumns = data.header.map(columnKey);
        const labels = Object.fromEntries(data.header.map((heading, i) => [exportColumns[i], heading]));
        const sourceRows = data.rows.map(row => Object.fromEntries(
          data.header.map((heading, i) => [exportColumns[i], row[heading]])));
        const kind = document.querySelector('main').dataset.reportKind;
        const sourceOnly = ['reporting_form', 'cells', 'rows'];
        if (kind === 'code-list' || kind === 'registry') sourceOnly.push('sheet', 'field');
        const columns = exportColumns.filter(key => !sourceOnly.includes(key));
        const groupKeys = columns.filter(key => kind === 'code-list' || kind === 'registry'
          ? ['reference', 'value', 'severity', 'reason'].includes(key)
          : key !== 'occurrences');
        const groups = new Map();
        for (const source of sourceRows) {
          const key = JSON.stringify(groupKeys.map(column => source[column]));
          if (!groups.has(key)) groups.set(key, {...source, occurrences: 0, sources: []});
          const row = groups.get(key);
          row.occurrences += Number(source.occurrences);
          row.sources.push(source);
        }
        const rows = [...groups.values()];
        for (const row of rows) {
          row.affected_files = new Set(row.sources.map(source => source.reporting_form)).size;
          for (const column of columns.filter(key => !groupKeys.includes(key) && key !== 'occurrences')) {
            row[column] = [...new Set(row.sources.map(source => source[column]))].sort().join(', ');
          }
        }
        columns.push('affected_files');
        labels.affected_files = 'Affected forms';
        return table(content, rows, columns,
          {filename: tab.file.replace(/\.csv$/, ''), pageSize: 20, labels, exportColumns,
            exportRows: selected => selected.flatMap(row => row.sources)});
      }
    }));
    const details = ui.tabs(root, panels, 'reference-details', 'Details', true);
    document.querySelectorAll('[data-filter]').forEach(card => {
      const label = card.querySelector('span').textContent;
      ui.bindCard(card, Number(card.dataset.value),
        () => details.open('values', {[card.dataset.filter]: card.dataset.filterValue}, true),
        `Show ${label} conversion findings`, 'View findings');
    });
    await details.ready;
    progress.finish();
  }
  async function run() {
    progress.start(`Loading ${document.querySelector('main').dataset.evidenceLabel.toLowerCase()}…`);
    try { await main(); } catch (error) { progress.fail(error, run); }
  }
  run();
})();
