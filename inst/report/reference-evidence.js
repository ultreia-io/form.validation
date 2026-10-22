/* Review saved findings, then load only the selected case's source evidence. */
'use strict';
(() => {
  const ui = window.ValidationReportUI, table = window.ValidationReportTable;
  const format = new Intl.NumberFormat('en');
  const progress = ui.feedback(document.getElementById('loading'));
  const visibleValue = value => value === '' ? '(empty)'
    : value.replace(/\r\n|\r|\n/g, '↵').replace(/\t/g, '⇥').replace(/ /g, '·');
  async function main() {
    const response = await fetch('metadata.json', {credentials: 'same-origin'});
    if (!response.ok) throw new Error(`metadata.json: HTTP ${response.status}`);
    const metadata = await response.json();
    const cases = metadata.cases;
    const caseHeading = item => (metadata.case_heading_columns || ['reference']).map(key => item[key]).join(' · ');
    const labels = {reference: metadata.labels.reference, value: 'Submitted value'};
    const overview = document.getElementById('overview');
    overview.querySelectorAll('[data-total]').forEach(node => {
      const count = Number(metadata.totals[node.dataset.total]);
      node.textContent = format.format(count);
      node.closest('.overview-card').dataset.empty = String(count === 0);
    });
    const root = document.getElementById('reference-review');
    root.replaceChildren();
    let details;
    function evidence(content, update) {
      let ready = Promise.resolve(), requested, revision = 0;
      const prompt = document.createElement('p');
      prompt.textContent = `Select a submitted value in ${metadata.labels.cases}.`;
      content.append(prompt);
      return {
        get ready() { return ready; },
        setFilters(filters) {
          const item = cases.find(row => row.case_id === filters.case_id);
          if (!item || item.case_id === requested) return;
          requested = item.case_id;
          const current = ++revision;
          ready = (async () => {
            update('Loading source evidence…');
            const response = await fetch(item.evidence_file, {credentials: 'same-origin'});
            if (!response.ok) throw new Error(`${item.evidence_file}: HTTP ${response.status}`);
            const rows = await response.json();
            if (current !== revision) return;
            const section = document.getElementById('reference-case').content.firstElementChild.cloneNode(true);
            section.querySelector('[data-case-reference]').textContent = caseHeading(item);
            section.querySelector('[data-case-value]').textContent = visibleValue(item.value);
            table(section.querySelector('[data-case-table]'), rows,
              metadata.detail_columns,
              {fileLinks: true, filename: 'evidence', pageSize: 20, labels,
                exportColumns: metadata.evidence_columns,
                renderCell(cell, key, row) {
                  if (key !== 'cells' || row.cells.length <= 80) return false;
                  const disclosure = document.createElement('details'), caption = document.createElement('summary');
                  caption.textContent = `${row.cells.slice(0, 77)}…`;
                  const locations = document.createElement('p'); locations.className = 'reference-cells';
                  locations.textContent = row.cells;
                  disclosure.append(caption, locations); cell.append(disclosure);
                  return true;
                }});
            content.replaceChildren(section);
          })().catch(error => {
            if (current === revision) requested = undefined;
            throw error;
          });
        }
      };
    }
    details = ui.tabs(root, [
      {id: 'values', largeTable: cases.length >= 10000, label: `${metadata.labels.cases} (${format.format(cases.length)})`,
        render(content) {
          const valuesTable = table(content, cases,
            metadata.case_columns,
            {filename: metadata.labels.filename, pageSize: 20, labels,
              renderCell(cell, key, row) {
                if (key !== 'value') return false;
                const button = document.createElement('button'); button.type = 'button';
                button.className = 'reference-code-link report-value'; button.textContent = visibleValue(row.value);
                button.title = `Inspect source evidence for ${caseHeading(row)}`;
                button.onclick = () => {
                  details.enable('evidence');
                  details.open('evidence', {case_id: row.case_id}, true);
                };
                cell.append(button); return true;
              }});
          return valuesTable;
        }},
      {id: 'evidence', label: 'Source evidence', disabled: true,
        disabledHint: 'Select a case to view its source evidence.', render: evidence}
    ], 'reference-details', 'Details', true);
    if (metadata.filter_cards) {
      const filters = document.getElementById('evidence-filters');
      filters.hidden = false;
      const names = {error: 'Errors', warning: 'Warnings', boolean: 'Boolean', integer: 'Integer',
        number: 'Number', date: 'Date', datetime: 'Date and time', time: 'Time'};
      for (const [key, cards] of Object.entries(metadata.filter_cards)) {
        const group = filters.querySelector(`[data-filter-cards="${key}"]`);
        for (const {value, count} of cards) {
          const card = document.getElementById('evidence-filter-card').content.firstElementChild.cloneNode(true);
          card.dataset.empty = String(count === 0);
          if (key === 'severity') card.dataset.tone = value === 'error' ? 'error' : 'attention';
          card.querySelector('strong').textContent = format.format(count);
          card.querySelector('span').textContent = names[value] || value;
          group.append(card);
          ui.bindCard(card, count, () => details.open('values', {[key]: value}, true),
            `Show ${names[value] || value} conversion findings`, 'View findings');
        }
      }
    }
    await details.ready;
    progress.finish();
  }
  async function run() {
    progress.start(`Loading ${document.querySelector('main').dataset.evidenceLabel.toLowerCase()}…`);
    try { await main(); } catch (error) { progress.fail(error, run); }
  }
  run();
})();
