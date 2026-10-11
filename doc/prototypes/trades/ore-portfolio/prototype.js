'use strict';
/* ORE import and export. Serves: Import a portfolio from ORE, Export a portfolio to ORE.
 * Both are workflows reached from the trade list's Workflows menu. */

const MODE = P.get('mode') === 'export' ? 'export' : 'import';
const main = shell({
    title: MODE === 'import' ? 'Import a portfolio from ORE' : 'Export a portfolio to ORE', name: 'ore-portfolio',
    question: 'What does a person need to see before an import is written, and after an export is made?',
    crumbs: [['Home', '#'], ['Trades', '../trade-page/'], ['Trades', '../trade-page/'], [MODE === 'import' ? 'Import a portfolio from ORE' : 'Export a portfolio to ORE']],
    states: [['import', { mode: null, trades: null }, MODE === 'import'], ['export', { mode: 'export' }, MODE === 'export']],
    says: MODE === 'import'
        ? 'Import is a workflow because the person must see the plan before anything is written, and must be able to undo it. Read the preview, then confirm. The result says what was refused and why, and offers the rollback.'
        : 'Export is a workflow with two ends: a file for the person, or a file in storage for a run. The check names the trades the engine cannot read before the file is made.'
});

const wrap = (...kids) => h('div', { class: 'dyn' }, kids);
function put(el) { main.querySelectorAll('.dyn').forEach(n => n.remove()); main.append(el); }
const guard = ok => ok ? null : h('div', { class: 'notice warn' }, h('b', null, 'You cannot use this screen. '), 'It needs the right to ' + MODE + ' portfolios. Ask a tenant administrator for the Operations role.');

/* ---------- import ---------- */
const FILES = {
    'portfolio_mixed.xml': { label: 'portfolio_mixed.xml — 12 trades', rows: [
        ['Trade_0001', 'Swap', 'NWCAP'], ['Trade_0002', 'Swap', 'NWCAP'], ['Trade_0003', 'Swap', 'MRDN'], ['Trade_0004', 'CrossCurrencySwap', 'MRDN'],
        ['Trade_0005', 'FxOption', 'CSTL'], ['Trade_0006', 'FxOption', 'CSTL'], ['Trade_0007', 'Bond', 'MRDN'], ['Trade_0008', 'ForwardBond', 'MRDN'],
        ['Trade_0009', 'EquityOption', 'NWCAP'], ['Trade_0010', 'CreditDefaultSwap', 'CSTL'],
        ['Trade_0011', 'Swap', 'ACME_BANK_PLACEHOLDER'], ['Trade_0012', 'FxAsianOption', 'NWCAP']] },
    'portfolio_swaps.xml': { label: 'portfolio_swaps.xml — 4 trades', rows: [['Swap_A', 'Swap', 'NWCAP'], ['Swap_B', 'Swap', 'MRDN'], ['Swap_C', 'CapFloor', 'MRDN'], ['Swap_D', 'Swaption', 'CSTL']] },
    'empty.xml': { label: 'empty.xml — no trades', rows: [] }
};
let I = { file: 'portfolio_mixed.xml', book: 'EMEA-RATES', step: 0, imported: null, rolledBack: false };
function plan() {
    return FILES[I.file].rows.map(([oreId, type, cp]) => {
        const tt = tradeType(type), known = cpty(cp);
        let status = 'ok', why = '';
        if (!known) { status = 'refused'; why = 'The counterparty name ' + cp + ' matches no alias.'; }
        else if (!tt.instrument) { status = 'refused'; why = type + ' has no instrument table yet.'; }
        return { oreId, type, tt, cp, known, status, why, netting: known ? (oreId.endsWith('3') ? 'created' : 'exists') : '' };
    });
}
function renderImport() {
    const g = guard(CAN.import);
    const steps = ['File and book', 'Preview', 'Confirm', 'Result'];
    const rail = h('div', { class: 'steps' }, steps.map((s, i) => h('span', { class: 'step' + (i === I.step ? ' on' : i < I.step ? ' done' : '') }, (i + 1) + ' ' + s)));
    const rows = plan(), ok = rows.filter(r => r.status === 'ok'), bad = rows.filter(r => r.status === 'refused');
    let pane, nav;
    if (I.step === 0) {
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'File and book')), h('div', { class: 'body', style: 'display:grid;gap:.8rem' },
            h('div', { class: 'grid2' }, field('ORE portfolio file', select(Object.entries(FILES).map(([k, v]) => [k, v.label]), I.file, v => { I.file = v; renderImport(); }),
                'A browser must upload the file. The upload path is not defined yet: see the journey, What is missing.'),
                field('Book that receives the trades', select(BOOKS.filter(b => !b.virtual).map(b => [b.code, b.name]), I.book, v => { I.book = v; }))),
            h('div', { class: 'notice' }, 'Reads the file and plans the import. Nothing is written yet. ', h('span', { class: 'mono small' }, 'workflow.v1.ops.ore_import (plan)'))));
        nav = [h('button', { class: 'btn primary', disabled: !!g || !FILES[I.file].rows.length, onclick: () => { I.step = 1; renderImport(); } }, 'Read the file')];
    } else if (I.step === 1) {
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Preview'), h('span', { class: 'sp' }), chip(ok.length + ' to import', 'ok'), bad.length ? chip(bad.length + ' refused', 'bad') : null),
            h('div', { class: 'body' }, h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['ORE trade id', 'Type', 'Counterparty', 'Netting set', 'Result'].map(c => h('th', null, c)))),
                h('tbody', null, rows.map(r => h('tr', null, h('td', { class: 'mono' }, r.oreId), h('td', null, r.tt.description), h('td', null, r.known ? r.known.name : h('span', { class: 'muted' }, r.cp)),
                    h('td', null, r.netting ? chip(r.netting, r.netting === 'created' ? 'acc' : '') : dash()), h('td', null, r.status === 'ok' ? chip('imports', 'ok') : [chip('refused', 'bad'), ' ', h('span', { class: 'small muted' }, r.why)])))))));
        nav = [h('button', { class: 'btn', onclick: () => { I.step = 0; renderImport(); } }, 'Back'), h('button', { class: 'btn primary', disabled: !ok.length, onclick: () => { I.step = 2; renderImport(); } }, 'Continue')];
    } else if (I.step === 2) {
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Confirm')), h('div', { class: 'body', style: 'display:grid;gap:.6rem' },
            h('div', { class: 'notice acc' }, h('b', null, ok.length + ' trades will be booked into ' + book(I.book).name + '.'),
                h('div', null, 'Each trade gets its anchor, booking, first state, ORE identifier, envelope fields and instrument.'), h('div', null, rows.filter(r => r.netting === 'created').length + ' netting set(s) will be created.'),
                bad.length ? h('div', null, bad.length + ' trade(s) will be refused and skipped. The import is partial.') : null),
            h('div', { class: 'mono small' }, 'ore.v1.ops.ore_import_execute')));
        nav = [h('button', { class: 'btn', onclick: () => { I.step = 1; renderImport(); } }, 'Back'), h('button', { class: 'btn primary', onclick: execute }, 'Import ' + ok.length + ' trades')];
    } else {
        pane = I.rolledBack
            ? h('div', { class: 'notice ok' }, h('b', null, 'Import rolled back. '), I.imported.length + ' trades and ' + rows.filter(r => r.netting === 'created').length + ' netting set(s) were removed.', ' ', h('a', { href: '../trade-page/' }, 'Back to the trades'))
            : h('div', { class: 'cards' }, h('div', { class: 'notice ' + (bad.length ? 'warn' : 'ok') }, h('b', null, bad.length ? 'Partial import. ' : 'Import complete. '), I.imported.length + ' imported, ' + bad.length + ' refused.'),
                bad.length ? h('section', { class: 'card' }, h('header', null, h('h2', null, 'Refused')), h('div', { class: 'body' }, bad.map(r => h('div', null, h('span', { class: 'mono' }, r.oreId), ' — ', r.why)))) : null,
                h('div', { class: 'filters' }, h('a', { class: 'btn primary', href: '../trade-page/' }, 'See the imported trades'),
                    h('button', { class: 'btn danger', onclick: rollback }, 'Roll back this import')));
        nav = [];
    }
    put(wrap(pageHead({ title: 'Import a portfolio from ORE', lead: 'Bring the trades of an ORE portfolio file into a book.', actions: [workflowMenu([{ title: 'Export a portfolio to ORE', line: 'Write trades out as a portfolio.', href: qs({ mode: 'export' }) }])] }), g, rail, pane, h('div', { class: 'filters' }, nav)));
}
function execute() {
    const ok = plan().filter(r => r.status === 'ok');
    const made = ok.map((r, i) => ({ id: tradeId(200 + i), code: 'ACME-1004' + String(i).padStart(2, '0'), type: r.type, product: r.tt.product, table: r.tt.instrument, cp: r.cp, book: I.book, state: 'live', auth: false, date: '2026-10-11', notional: 10000000, ccy: 'EUR', ext: 1, ver: 1, nature: 'Real', channel: 'ORE import', scope: 'Bilateral', structure: null, updated: '2026-10-11T16:41:00+01:00' }));
    store('booked', (store('booked') || []).concat(made));
    I.imported = made; I.step = 3; renderImport();
}
function rollback() { const codes = new Set(I.imported.map(t => t.code)); store('booked', (store('booked') || []).filter(t => !codes.has(t.code))); I.rolledBack = true; renderImport(); }

/* ---------- export ---------- */
const PRESET = (P.get('trades') || '').split(',').filter(Boolean);
let X = { scope: PRESET.length ? 'chosen' : 'book', book: 'EMEA-RATES', to: 'file', done: null };
function exportSet() {
    const pool = TRADES.filter(t => X.scope === 'chosen' ? PRESET.includes(t.code) : t.book === X.book);
    return pool.map(t => {
        let why = '';
        if (!t.table) why = 'its trade type has no instrument table';
        else if (t.state === 'draft') why = 'a draft is not an agreement';
        else if (t.state === 'cancelled') why = 'it is cancelled';
        return { t, why };
    });
}
function renderExport() {
    const g = guard(CAN.export);
    const set = exportSet(), good = set.filter(r => !r.why), bad = set.filter(r => r.why);
    let result = null;
    if (X.done) result = h('div', { class: 'cards' }, h('div', { class: 'notice ok' }, h('b', null, X.done.n + ' trades exported. '), X.to === 'file' ? ['The file ', h('span', { class: 'mono' }, X.done.name), ' is ready. ', h('button', { class: 'btn small', onclick: () => toast('The browser would save ' + X.done.name) }, 'Download')] : ['The file is in storage for a run: ', h('span', { class: 'mono' }, X.done.ref)]),
        h('section', { class: 'card' }, h('header', null, h('h2', null, 'What the file holds')), h('div', { class: 'body' }, h('pre', { class: 'mono small', style: 'margin:0;overflow:auto' }, X.done.xml))),
        h('div', { class: 'filters' }, h('a', { class: 'btn', href: '../trade-page/' }, 'Back to the trades'), h('button', { class: 'btn', onclick: () => { X.done = null; renderExport(); } }, 'Export again')));
    const pane = result || h('div', { class: 'cards' },
        h('section', { class: 'card' }, h('header', null, h('h2', null, 'Choose the trades')), h('div', { class: 'body', style: 'display:grid;gap:.8rem' },
            h('div', { class: 'grid2' }, field('Which trades', select([['book', 'All trades in a book'], PRESET.length ? ['chosen', 'The ' + PRESET.length + ' trades chosen in the list'] : null].filter(Boolean), X.scope, v => { X.scope = v; renderExport(); })),
                X.scope === 'book' ? field('Book', select(BOOKS.filter(b => !b.virtual).map(b => [b.code, b.name]), X.book, v => { X.book = v; renderExport(); })) : null),
            field('Send it to', select([['file', 'A file for me'], ['storage', 'Storage, for a run']], X.to, v => { X.to = v; renderExport(); }), X.to === 'storage' ? 'trading.v1.ops.export_trades_to_storage' : 'trading.v1.ops.export_portfolio'))),
        h('section', { class: 'card' }, h('header', null, h('h2', null, 'Check'), h('span', { class: 'sp' }), chip(good.length + ' can be written', 'ok'), bad.length ? chip(bad.length + ' cannot', 'bad') : null),
            h('div', { class: 'body' }, set.length ? h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['Trade', 'Type', 'State', 'Result'].map(c => h('th', null, c)))),
                h('tbody', null, set.slice(0, 12).map(r => h('tr', null, h('td', null, h('a', { href: '../trade-page/?trade=' + r.t.code }, r.t.code)), h('td', null, tradeType(r.t.type).description), h('td', null, chip(r.t.state)),
                    h('td', null, r.why ? [chip('skipped', 'bad'), ' ', h('span', { class: 'small muted' }, r.why)] : chip('written', 'ok'))))))
                : h('span', { class: 'muted' }, 'No trades in this book.'), set.length > 12 ? h('div', { class: 'faint small' }, 'Showing 12 of ' + set.length + '.') : null)),
        h('div', { class: 'filters' }, h('button', { class: 'btn primary', disabled: !!g || !good.length, onclick: () => {
            X.done = { n: good.length, name: 'portfolio_' + (X.scope === 'book' ? X.book.toLowerCase() : 'chosen') + '.xml', ref: 's3://ores-acme/exports/run-0412/portfolio.xml',
                xml: '<Portfolio>\n' + good.slice(0, 3).map(r => '  <Trade id="' + r.t.code.replace('ACME-', 'Trade_') + '">\n    <TradeType>' + r.t.type + '</TradeType>\n    <Envelope><CounterParty>' + cpty(r.t.cp).code + '</CounterParty></Envelope>\n  </Trade>').join('\n') + (good.length > 3 ? '\n  <!-- ' + (good.length - 3) + ' more -->' : '') + '\n</Portfolio>' };
            renderExport(); } }, 'Export ' + good.length + ' trades')));
    put(wrap(pageHead({ title: 'Export a portfolio to ORE', lead: 'Write trades out as the portfolio the engine reads.', actions: [workflowMenu([{ title: 'Import a portfolio from ORE', line: 'Bring a portfolio file into a book.', href: qs({ mode: null, trades: null }) }])] }), g, pane));
}
if (MODE === 'import') renderImport(); else renderExport();
