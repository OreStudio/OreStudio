'use strict';
/* ORE import and export. Serves: Import a portfolio from ORE, Export a portfolio to ORE.
 * Both are workflows reached from the trade list's Workflows menu. */

const MODE = P.get('mode') === 'export' ? 'export' : 'import';
const FAIL = P.get('fail') || '';
const REASONS = REASON_CODES.filter(r => r[0] !== 'trade.no_change');
const PORTFOLIOS = ['ORE examples', 'Rates and credit', 'FX flow', 'Equity'];
/* A form the person has touched is not thrown away without asking. */
let touched = false;
const main = shell({
    title: MODE === 'import' ? 'Import a portfolio from ORE' : 'Export a portfolio to ORE', name: 'ore-portfolio',
    question: 'What does a person need to see before an import is written, and after an export is made?',
    crumbs: [['Home', '#'], ['Trades', '../trade-page/'], [MODE === 'import' ? 'Import a portfolio from ORE' : 'Export a portfolio to ORE']],
    states: [['import', { mode: null, trades: null, fail: null, step: null }, MODE === 'import' && !FAIL], ['export', { mode: 'export', fail: null, step: null }, MODE === 'export' && !FAIL], ['import fails', { mode: null, fail: 'import' }, FAIL === 'import'], ['export fails', { mode: 'export', fail: 'export' }, FAIL === 'export']],
    says: MODE === 'import'
        ? 'Import finds every portfolio file in a directory, lists them, and lets the person choose which to bring in. It is a workflow because the person must see the plan before anything is written, and must be able to undo it.'
        : 'Export is a workflow with two ends: a file for the person, or a file in storage for a run. The check names the trades the engine cannot read before the file is made.'
});

const wrap = (...kids) => h('div', { class: 'dyn' }, kids);
function put(el) { main.querySelectorAll('.dyn').forEach(n => n.remove()); main.append(el); }
const guard = ok => ok ? null : needsRole(MODE === 'import' ? 'Import a portfolio from ORE' : 'Export a portfolio to ORE', MODE + ' portfolios', ['Operations']);
const TITLE = MODE === 'import' ? 'Import a portfolio from ORE' : 'Export a portfolio to ORE';
const LEAD = MODE === 'import' ? 'Find the portfolio files in an ORE directory, choose the ones to bring in, and review the plan before anything is written.' : 'Write trades out as the portfolio the engine reads.';
/* A step has its own place in the history, so Back returns to the earlier step. */
function setStep(n) {
    I.step = n;
    history.pushState(null, '', qs({ step: n || null }));
    renderImport();
}

/* ---------- import ---------- */
/* The real import takes a packed directory. The server scans it, finds the
 * portfolio files, and builds portfolios and books from the folders. The
 * choices it takes today are directory names to skip, the parent portfolio, the
 * re-import mode and the trade defaults. It has no choice of files. The
 * file list below is the change this prototype argues for. */
const SCAN = {
    '/data/ore/Examples': { scanned: 142, ignored: [['Example_1/Input/ore.xml', 'ORE run configuration'], ['Example_1/Input/todaysmarket.xml', 'Market configuration'], ['Example_1/Input/currencies.xml', 'Currency configuration'], ['Example_2/Input/portfolio_notes.xml', 'Root element is Notes, not Portfolio'], ['Example_3/Input/simulation.xml', 'Simulation configuration']], files: [
        { path: 'Example_1/Input/portfolio.xml', size: '38 KB', rows: [
            ['Trade_0001', 'Swap', 'NWCAP'], ['Trade_0002', 'Swap', 'NWCAP'], ['Trade_0003', 'Swap', 'MRDN'], ['Trade_0004', 'CrossCurrencySwap', 'MRDN'],
            ['Trade_0005', 'FxOption', 'CSTL'], ['Trade_0006', 'FxOption', 'CSTL'], ['Trade_0007', 'Bond', 'MRDN'], ['Trade_0008', 'ForwardBond', 'MRDN'],
            ['Trade_0009', 'EquityOption', 'NWCAP'], ['Trade_0010', 'CreditDefaultSwap', 'CSTL'], ['Trade_0011', 'Swap', 'ACME_BANK_PLACEHOLDER'], ['Trade_0012', 'FxAsianOption', 'NWCAP']] },
        { path: 'Example_2/Input/swaps_book.xml', size: '14 KB', rows: [['Swap_A', 'Swap', 'NWCAP'], ['Swap_B', 'Swap', 'MRDN'], ['Swap_C', 'CapFloor', 'MRDN'], ['Swap_D', 'Swaption', 'CSTL']] },
        { path: 'Example_3/Input/fx_trades.xml', size: '9 KB', rows: [['Fx_1', 'FxForward', 'NWCAP'], ['Fx_2', 'FxOption', 'CSTL'], ['Fx_3', 'FxBarrierOption', 'MRDN']] },
        { path: 'Example_Trades/bonds.xml', size: '11 KB', rows: [['Bond_1', 'Bond', 'MRDN'], ['Bond_2', 'BondRepo', 'MRDN'], ['Bond_3', 'CallableBond', 'CSTL']] },
        { path: 'Example_5/Output/result_trades.xml', size: '6 KB', rows: [['Out_1', 'Swap', 'NWCAP'], ['Out_2', 'Swap', 'NWCAP']] },
        { path: 'Example_6/Input/empty_book.xml', size: '1 KB', rows: [] }] },
    '/data/ore/Examples/Example_1': { scanned: 31, ignored: [['Input/ore.xml', 'ORE run configuration']], files: [{ path: 'Input/portfolio.xml', size: '38 KB', rows: [['Trade_0001', 'Swap', 'NWCAP'], ['Trade_0002', 'Swap', 'NWCAP']] }] },
    '/data/ore/Empty': { scanned: 9, ignored: [], files: [] }
};
let I = { dir: '/data/ore/Examples', scanned: false, exclude: ['Output'], sel: new Set(), step: 0, imported: null, rolledBack: false, parent: 'ORE examples', reimport: 'add_trades_only', defCp: '', activity: 'new_booking', reason: 'trade.client_request', note: '', po: 0, pl: 15, busy: false, failed: false };
const stripInput = p => p.split('/').slice(0, -1).filter(s => s !== 'Input').join(' / ') || '(top)';
function matches() { return SCAN[I.dir].files.filter(f => !I.exclude.some(x => f.path.split('/').includes(x))); }
function excludedFiles() { return SCAN[I.dir].files.filter(f => I.exclude.some(x => f.path.split('/').includes(x))); }
function plan() {
    const out = [];
    matches().filter(f => I.sel.has(f.path)).forEach(f => f.rows.forEach(([oreId, type, cp]) => {
        const tt = tradeType(type), known = cpty(cp) || (I.defCp ? cpty(I.defCp) : null);
        let status = 'ok', why = '';
        if (!known) { status = 'refused'; why = 'The counterparty name ' + cp + ' matches no alias, and no default counterparty is set.'; }
        else if (!tt.instrument) { status = 'refused'; why = type + ' has no instrument table yet.'; }
        out.push({ file: f.path, book: stripInput(f.path), oreId, type, tt, cp, known, status, why, netting: known ? (oreId.endsWith('3') ? 'created' : 'exists') : '' });
    }));
    return out;
}
function renderImport() {
    const g = guard(CAN.import);
    if (g) return put(wrap(pageHead({ title: TITLE, lead: LEAD }), g));
    const steps = ['Directory', 'Choices', 'Preview', 'Confirm', 'Result'];
    const rail = h('div', { class: 'steps' }, steps.map((s, i) => h('span', { class: 'step' + (i === I.step ? ' on' : i < I.step ? ' done' : '') }, (i + 1) + ' ' + s)));
    const rows = plan(), ok = rows.filter(r => r.status === 'ok'), bad = rows.filter(r => r.status === 'refused');
    let pane, nav;
    if (I.step === 0) {
        const found = I.scanned ? matches() : [];
        const total = found.reduce((n, f) => n + f.rows.length, 0);
        const sel = found.filter(f => I.sel.has(f.path));
        pane = h('div', { class: 'cards' },
            h('section', { class: 'card' }, h('header', null, h('h2', null, 'Directory')), h('div', { class: 'body', style: 'display:grid;gap:.8rem' },
                h('div', { class: 'grid2' }, field('ORE directory', select(Object.keys(SCAN), I.dir, v => { I.dir = v; I.scanned = false; I.sel = new Set(); renderImport(); }),
                    'The browser packs the directory and uploads it to storage. The server reads it from there.'),
                    field('Directories to skip', h('div', { class: 'filters', style: 'margin:0' }, I.exclude.map(x => h('span', { class: 'chip' }, x, ' ', h('a', { href: '#', 'data-no': 1, onclick: e => { e.preventDefault(); I.exclude = I.exclude.filter(y => y !== x); I.scanned = false; renderImport(); } }, '×'))),
                        h('input', { class: 'field-input', placeholder: 'Add a name', style: 'width:9rem', onkeydown: e => { if (e.key === 'Enter' && e.target.value) { I.exclude.push(e.target.value); I.scanned = false; renderImport(); } } })), 'Folders with these names are not searched.')),
                h('div', { class: 'filters', style: 'margin:0' }, h('button', { class: 'btn primary', onclick: () => { I.scanned = true; I.sel = new Set(matches().map(f => f.path)); renderImport(); } }, 'Find portfolio files'),
                    I.scanned ? h('span', { class: 'muted small' }, SCAN[I.dir].scanned + ' files searched, ' + found.length + ' are portfolios' + (excludedFiles().length ? ', ' + excludedFiles().length + ' skipped in excluded folders' : '') + '. A file is a portfolio when its root element is Portfolio, whatever its name.') : null))),
            I.scanned ? h('section', { class: 'card' }, h('header', null, h('h2', null, 'Portfolio files'), chip(sel.length + ' of ' + found.length + ' chosen', 'v'), h('span', { class: 'sp' }),
                h('button', { class: 'btn small', onclick: () => { I.sel = new Set(found.map(f => f.path)); renderImport(); } }, 'Choose all'), h('button', { class: 'btn small', onclick: () => { I.sel = new Set(); renderImport(); } }, 'Choose none')),
                h('div', { class: 'body' }, found.length ? h('table', { class: 'grid' }, h('thead', null, h('tr', null, [h('th', { class: 'sel' }), 'File', 'Becomes book', 'Trades', 'Size'].map(c => c.nodeType ? c : h('th', null, c)))),
                    h('tbody', null, found.map(f => h('tr', null, h('td', { class: 'sel' }, h('input', { type: 'checkbox', checked: I.sel.has(f.path), 'aria-label': 'Choose ' + f.path, onchange: e => { e.target.checked ? I.sel.add(f.path) : I.sel.delete(f.path); renderImport(); } })),
                        h('td', { class: 'mono small' }, f.path, f.path.toLowerCase().includes('portfolio') ? null : h('span', { class: 'chip', style: 'margin-left:.4rem', title: 'The name does not say portfolio. The root element does.' }, 'found by content')), h('td', null, stripInput(f.path)), h('td', { class: 'num' }, f.rows.length || h('span', { class: 'muted' }, 'none')), h('td', { class: 'num muted' }, f.size)))))
                    : h('span', { class: 'muted' }, 'No portfolio file matches in this directory. Check the folder names to skip.'),
                    SCAN[I.dir].ignored.length ? h('details', { class: 'det' }, h('summary', null, SCAN[I.dir].ignored.length + ' other XML files are not portfolios'), h('table', { class: 'grid' }, h('tbody', null, SCAN[I.dir].ignored.map(([p, why]) => h('tr', null, h('td', { class: 'mono small' }, p), h('td', { class: 'muted' }, why)))))) : null)) : null);
        nav = [h('button', { class: 'btn primary', disabled: !I.scanned || !sel.length, title: !I.scanned ? 'Find the portfolio files first.' : !sel.length ? 'Choose at least one file.' : '', onclick: () => { setStep(1); } }, 'Continue with ' + sel.length + ' file' + (sel.length === 1 ? '' : 's'))];
    } else if (I.step === 1) {
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Choices')), h('div', { class: 'body', style: 'display:grid;gap:.8rem' }, h('div', { class: 'grid2' },
            field('Parent portfolio', select(PORTFOLIOS, I.parent, v => { I.parent = v; }), 'One portfolio wraps every imported portfolio. The folders become portfolios and books under it. A folder named Input is not a level.'),
            field('Portfolios and books that already exist', select([['add_trades_only', 'Add the new trades to them as they are'], ['create_new_versions', 'Save a new version of them, then add the trades']], I.reimport, v => { I.reimport = v; })),
            field('Default counterparty', select([['', 'None: refuse a trade whose counterparty is unknown']].concat(COUNTERPARTIES.filter(c => c.open).map(c => [c.code, c.name])), I.defCp, v => { I.defCp = v; }), 'Used when a trade names a counterparty that matches no alias.'),
            field('Activity of the first booking', select([['new_booking', 'New booking'], ['draft_capture', 'Draft capture']], I.activity, v => { I.activity = v; })))));
        nav = [h('button', { class: 'btn', onclick: () => { setStep(0); } }, 'Back'), h('button', { class: 'btn primary', onclick: () => { setStep(2); } }, 'Preview')];
    } else if (I.step === 2) {
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Preview'), h('span', { class: 'sp' }), chip(ok.length + ' to import', 'ok'), bad.length ? chip(bad.length + ' refused', 'bad') : null),
            h('div', { class: 'body' }, h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['File', 'ORE trade id', 'Type', 'Counterparty', 'Netting set', 'Result'].map(c => h('th', null, c)))),
                h('tbody', null, rows.slice(I.po, I.po + I.pl).map(r => h('tr', null, h('td', { class: 'mono small' }, r.file.split('/').slice(-2).join('/')), h('td', { class: 'mono' }, r.oreId), h('td', null, ref('trade_type', r.type, r.tt.description)), h('td', null, r.known ? ref('counterparty', r.known.code, r.known.name) : h('span', { class: 'muted' }, r.cp)),
                    h('td', null, r.netting ? chip(r.netting, r.netting === 'created' ? 'acc' : '') : dash()), h('td', null, r.status === 'ok' ? chip('imports', 'ok') : [chip('refused', 'bad'), ' ', h('span', { class: 'small muted' }, r.why)]))))),
                pager({ offset: I.po, limit: I.pl, total: rows.length, onChange: v => { I.po = v.offset; I.pl = v.limit; renderImport(); } })));
        nav = [h('button', { class: 'btn', onclick: () => { setStep(1); } }, 'Back'), h('button', { class: 'btn primary', disabled: !ok.length, title: ok.length ? '' : 'No trade can be imported.', onclick: () => { setStep(3); } }, 'Continue')];
    } else if (I.step === 3) {
        const books = [...new Set(ok.map(r => r.book))];
        pane = h('section', { class: 'card' }, h('header', null, h('h2', null, 'Confirm')), h('div', { class: 'body', style: 'display:grid;gap:.6rem' },
            h('div', { class: 'notice acc' }, h('b', null, ok.length + ' trades from ' + I.sel.size + ' files will be booked.'),
                h('div', null, 'Under the portfolio ' + I.parent + ', ' + books.length + ' book(s): ' + books.join(', ') + '.'), h('div', null, rows.filter(r => r.netting === 'created').length + ' netting set(s) will be created.'),
                bad.length ? h('div', null, bad.length + ' trade(s) will be refused and skipped. The import is partial.') : null),
            h('div', { class: 'grid2' }, field('Reason', select(REASONS, I.reason, v => { I.reason = v; })), field('Commentary', h('textarea', { rows: 2, oninput: e => { I.note = e.target.value; } }, I.note))),
            h('div', { class: 'mono small' }, 'ore.v1.ops.ore_import_execute  (the files chosen go in import_choices_json; change intent: reason, commentary)')));
        nav = [h('button', { class: 'btn', onclick: () => { setStep(2); } }, 'Back'), h('button', { class: 'btn primary', onclick: execute }, 'Import ' + ok.length + ' trades')];
    } else {
        pane = I.rolledBack
            ? h('div', { class: 'notice ok' }, h('b', null, 'Import rolled back. '), I.imported.length + ' trades and the portfolios, books and netting sets the import created were removed.', ' ', h('a', { href: '../trade-page/' }, 'Back to the trades'))
            : h('div', { class: 'cards' }, h('div', { class: 'notice ' + (bad.length ? 'warn' : 'ok') }, h('b', null, bad.length ? 'Partial import. ' : 'Import complete. '), I.imported.length + ' imported, ' + bad.length + ' refused.'),
                bad.length ? h('section', { class: 'card' }, h('header', null, h('h2', null, 'Refused')), h('div', { class: 'body' }, bad.map(r => h('div', null, h('span', { class: 'mono' }, r.file.split('/').slice(-2).join('/') + ' ' + r.oreId), ' — ', r.why)))) : null,
                h('div', { class: 'filters' }, h('a', { class: 'btn primary', href: '../trade-page/' }, 'See the imported trades'), h('button', { class: 'btn danger', onclick: rollback }, 'Roll back this import')));
        nav = [];
    }
    if (I.busy) { pane = h('div', { class: 'notice' }, 'Importing…'); nav = []; }
    else if (I.failed) {
        pane = failure('ore.v1.ops.ore_import_execute', 503, 'unavailable', 'e41b7c90d2aa', 'The import was not made. Nothing was written.', h('button', { class: 'btn small', onclick: () => { I.failed = false; execute(); } }, 'Try again'));
        nav = [h('button', { class: 'btn', onclick: () => { I.failed = false; renderImport(); } }, 'Back')];
    }
    put(wrap(pageHead({ title: TITLE, lead: LEAD }), rail, pane, h('div', { class: 'filters' }, nav)));
}
function execute() {
    I.busy = true; renderImport();
    setTimeout(() => { I.busy = false; if (FAIL === 'import') { I.failed = true; renderImport(); } else writeImport(); }, 400);
}
function writeImport() {
    const ok = plan().filter(r => r.status === 'ok');
    const made = ok.map((r, i) => ({ id: tradeId(200 + i), code: 'ACME-1004' + String(i).padStart(2, '0'), type: r.type, product: r.tt.product, table: r.tt.instrument, cp: r.known.code, book: 'EMEA-RATES', state: I.activity === 'draft_capture' ? 'draft' : 'live', auth: I.activity === 'draft_capture' ? null : false, date: '2026-10-11', notional: 10000000, ccy: 'EUR', ext: 1, ver: 1, nature: 'Real', channel: 'ORE import', scope: 'Bilateral', structure: null, updated: '2026-10-11T16:41:00+01:00' }));
    store('booked', (store('booked') || []).concat(made));
    I.imported = made; setStep(4);
}
function rollback() {
    let reason = 'trade.correction', note = '';
    const d = openDialog({ title: 'Roll back this import', loadedAt: LOADED,
        body: h('div', { style: 'display:grid;gap:.8rem' }, h('div', { class: 'notice warn' }, I.imported.length + ' trades and the portfolios, books and netting sets the import created are removed.'),
            h('div', { class: 'grid2' }, field('Reason', select(REASONS, reason, v => { reason = v; })), field('Commentary', h('textarea', { rows: 2, oninput: e => { note = e.target.value; } })))),
        buttons: [h('button', { class: 'btn', onclick: () => d.close() }, 'Cancel'), h('button', { class: 'btn danger', onclick: () => {
            const codes = new Set(I.imported.map(t => t.code)); store('booked', (store('booked') || []).filter(t => !codes.has(t.code))); I.rolledBack = true; d.close(); renderImport(); } }, 'Roll back')] });
}

/* ---------- export ---------- */
const PRESET = (P.get('trades') || '').split(',').filter(Boolean);
let X = { scope: PRESET.length ? 'chosen' : 'book', book: 'EMEA-RATES', to: 'file', done: null, po: 0, pl: 15, busy: false, failed: false };
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
    if (g) return put(wrap(pageHead({ title: TITLE, lead: LEAD }), g));
    const set = exportSet(), good = set.filter(r => !r.why), bad = set.filter(r => r.why);
    let result = null;
    if (X.done) result = h('div', { class: 'cards' }, h('div', { class: 'notice ok' }, h('b', null, X.done.n + ' trades exported. '), X.to === 'file' ? ['The file ', h('span', { class: 'mono' }, X.done.name), ' is ready. ', h('button', { class: 'btn small', onclick: () => toast('The browser would save ' + X.done.name) }, 'Download')] : ['The file is in storage for a run: ', h('span', { class: 'mono' }, X.done.ref)]),
        h('section', { class: 'card' }, h('header', null, h('h2', null, 'What the file holds')), h('div', { class: 'body' }, h('pre', { class: 'mono small', style: 'margin:0;overflow:auto' }, X.done.xml))),
        h('div', { class: 'filters' }, h('a', { class: 'btn', href: '../trade-page/' }, 'Back to the trades'), h('button', { class: 'btn', onclick: () => { X.done = null; renderExport(); } }, 'Export again')));
    const pane = result || h('div', { class: 'cards' },
        h('section', { class: 'card' }, h('header', null, h('h2', null, 'Choose the trades')), h('div', { class: 'body', style: 'display:grid;gap:.8rem' },
            h('div', { class: 'grid2' }, field('Which trades', select([['book', 'All trades in a book'], PRESET.length ? ['chosen', 'The ' + PRESET.length + ' trades chosen in the list'] : null].filter(Boolean), X.scope, v => { X.scope = v; X.po = 0; renderExport(); })),
                X.scope === 'book' ? field('Book', select(BOOKS.filter(b => !b.virtual).map(b => [b.code, b.name]), X.book, v => { X.book = v; X.po = 0; renderExport(); })) : null),
            field('Send it to', select([['file', 'A file for me'], ['storage', 'Storage, for a run']], X.to, v => { X.to = v; renderExport(); }), X.to === 'storage' ? 'trading.v1.ops.export_trades_to_storage' : 'trading.v1.ops.export_portfolio'))),
        h('section', { class: 'card' }, h('header', null, h('h2', null, 'Check'), h('span', { class: 'sp' }), chip(good.length + ' can be written', 'ok'), bad.length ? chip(bad.length + ' cannot', 'bad') : null),
            h('div', { class: 'body' }, set.length ? h('table', { class: 'grid' }, h('thead', null, h('tr', null, ['Trade', 'Type', 'State', 'Result'].map(c => h('th', null, c)))),
                h('tbody', null, set.slice(X.po, X.po + X.pl).map(r => h('tr', null, h('td', null, tradeLink(r.t.code)), h('td', null, ref('trade_type', r.t.type, tradeType(r.t.type).description)), h('td', null, chip(r.t.state)),
                    h('td', null, r.why ? [chip('skipped', 'bad'), ' ', h('span', { class: 'small muted' }, r.why)] : chip('written', 'ok'))))))
                : h('span', { class: 'muted' }, 'No trades in this book.'), set.length ? pager({ offset: X.po, limit: X.pl, total: set.length, onChange: v => { X.po = v.offset; X.pl = v.limit; renderExport(); } }) : null)),
        h('div', { class: 'filters' }, h('button', { class: 'btn primary', disabled: !good.length, title: good.length ? 'Export ' + good.length + ' trades' : 'No trade can be written.', onclick: () => {
            X.busy = true; renderExport();
            setTimeout(() => { X.busy = false; if (FAIL === 'export') { X.failed = true; return renderExport(); }
            X.done = { n: good.length, name: 'portfolio_' + (X.scope === 'book' ? X.book.toLowerCase() : 'chosen') + '.xml', ref: 's3://ores-acme/exports/run-0412/portfolio.xml',
                xml: '<Portfolio>\n' + good.slice(0, 3).map(r => '  <Trade id="' + r.t.code.replace('ACME-', 'Trade_') + '">\n    <TradeType>' + r.t.type + '</TradeType>\n    <Envelope><CounterParty>' + cpty(r.t.cp).code + '</CounterParty></Envelope>\n  </Trade>').join('\n') + (good.length > 3 ? '\n  <!-- ' + (good.length - 3) + ' more -->' : '') + '\n</Portfolio>' };
            renderExport(); }, 400); } }, 'Export ' + good.length + ' trades')));
    const shown = X.busy ? h('div', { class: 'notice' }, 'Exporting…')
        : X.failed ? failure(X.to === 'storage' ? 'trading.v1.ops.export_trades_to_storage' : 'trading.v1.ops.export_portfolio', 503, 'unavailable', '7d03a5c1be42', 'The export was not made.', h('button', { class: 'btn small', onclick: () => { X.failed = false; renderExport(); } }, 'Try again'))
            : pane;
    put(wrap(pageHead({ title: TITLE, lead: LEAD }), shown));
}
window.addEventListener('popstate', () => {
    if (MODE !== 'import') return;
    const n = Number(new URLSearchParams(location.search).get('step')) || 0;
    I.step = I.imported ? 4 : I.scanned ? Math.min(n, 3) : 0;
    renderImport();
});
main.addEventListener('input', () => { touched = true; });
main.addEventListener('change', () => { touched = true; });
leaveGuard(() => touched && !I.imported && !X.done);
if (P.get('step')) history.replaceState(null, '', qs({ step: null }));
if (MODE === 'import') renderImport(); else renderExport();
