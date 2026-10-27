/* Summary CSVs serve both interactive review and downloads; counts are rendered in HTML. */
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
        const columns = data.header.map(columnKey);
        const labels = Object.fromEntries(data.header.map((heading, i) => [columns[i], heading]));
        const rows = data.rows.map(row => Object.fromEntries(
          data.header.map((heading, i) => [columns[i], row[heading]])));
        return table(content, rows, columns,
          {filename: tab.file.replace(/\.csv$/, ''), pageSize: 20, labels});
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
