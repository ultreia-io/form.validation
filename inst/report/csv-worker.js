'use strict';
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

self.onmessage = async event => {
  try {
    self.postMessage({status: 'Downloading data…'});
    const response = await fetch(event.data.url, {credentials: 'same-origin', cache: 'no-cache'});
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const text = await response.text();
    self.postMessage({status: 'Reading data…'});
    const data = csv(text);
    self.postMessage({data});
  } catch (error) {
    self.postMessage({error: error.message});
  }
};
