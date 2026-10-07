/* Stubbed-DOM render of every screen, tab and variant of the compute
 * prototype. Not a browser: enough of one to catch a throw and to read the
 * markup back. Run with: node verify-compute-prototype.js
 */
'use strict';
const fs = require('fs');
const vm = require('vm');

const file = __dirname + '/prototype.js';
const source = fs.readFileSync(file, 'utf8');

function makeEl(id) {
    return { id: id, innerHTML: '', textContent: '', value: '', getAttribute: () => null, closest: () => null };
}

function run(query) {
    const els = { app: makeEl('app'), 'proto-bar': makeEl('proto-bar'), 'proto-note': makeEl('proto-note') };
    const listeners = {};
    const history = { url: '' };
    const sandbox = {
        console: console, Math: Math, String: String, Number: Number, Object: Object, Array: Array,
        JSON: JSON, parseInt: parseInt, parseFloat: parseFloat, isNaN: isNaN, URLSearchParams: URLSearchParams,
        document: {
            getElementById: (id) => els[id] || null,
            addEventListener: (type, fn) => { (listeners[type] = listeners[type] || []).push(fn); }
        },
        window: {
            location: { search: query, pathname: '/doc/prototypes/compute/index.html' },
            history: { replaceState: (a, b, url) => { history.url = url; } }
        }
    };
    sandbox.window.document = sandbox.document;
    vm.createContext(sandbox);
    vm.runInContext(source, sandbox, { filename: file });
    /* Fire the delegated click listener with the attributes a control carries,
       so the paging and ordering events are exercised and not only the render. */
    return {
        els: els,
        history: history,
        click: function (attrs, value) {
            const el = {
                value: value === undefined ? '' : value,
                getAttribute: (name) => (name in attrs ? attrs[name] : null),
                closest: () => el
            };
            (listeners.click || []).forEach((fn) => fn({ target: el, preventDefault: () => {} }));
        }
    };
}

const TABS = {
    watch: ['dashboard', 'nodes', 'fleet'],
    job: ['jobs', 'lineage', 'timeline', 'spread'],
    failure: ['rates', 'where', 'detail'],
    capacity: ['headroom', 'arrivals'],
    versions: ['flight', 'catalogue']
};
const VARIANTS = ['fleet', 'tenant'];
/* The tables that carry a pager, and the ones whose fixture is longer than one
   page, so that the page size, the order and Load all are shown. */
const PAGED = {
    watch: { nodes: false, fleet: true },
    job: { jobs: true, lineage: true, timeline: false, spread: false },
    failure: { rates: false, where: false, detail: true },
    capacity: { headroom: false, arrivals: false },
    versions: { flight: false, catalogue: true }
};
const LONG = { nodes: true, jobs: true, catalogue: true, failures: false, lineage: false };

let checks = 0, bad = 0;
function ok(cond, label) {
    checks++;
    if (!cond) { bad++; console.log('  FAIL ' + label); }
}
function count(html) {
    const m = html.match(/Showing \d+ to \d+ of [\d,]+/);
    return m ? m[0] : '(no count)';
}
function totalOf(html) {
    const m = html.match(/Showing \d+ to \d+ of ([\d,]+)/);
    return m ? parseInt(m[1].replace(/,/g, ''), 10) : 0;
}
function chartViewBox(html) {
    const m = html.match(/<svg viewBox="0 0 (\d+) (\d+)" class="chart ([a-z]+)"/);
    return m ? Number(m[1]) : 0;
}

Object.keys(TABS).forEach((screen) => {
    VARIANTS.forEach((variant) => {
        TABS[screen].forEach((tab) => {
            const q = '?screen=' + screen + '&variant=' + variant + '&tab=' + tab;
            const label = screen + '/' + tab + '/' + variant;
            let r;
            try { r = run(q); } catch (e) { bad++; checks++; console.log('  FAIL threw on ' + q + ': ' + e.message); return; }
            const html = r.els.app.innerHTML;
            ok(html.length > 500, label + ' renders');
            ok(html.indexOf('<div class="shell">') >= 0, label + ' shell');
            ok(html.indexOf('data-act="refresh"') >= 0, label + ' refresh');
            /* Every chart drawn through svgEl keeps the 1000-wide viewBox, so a
               card scales them all alike. The node sparklines are the one
               deliberate exception: they are drawn inside a table row at
               120 px and are not cards. */
            if (html.indexOf('class="chart ') >= 0)
                ok(chartViewBox(html) === 1000, label + ' chart viewBox is 1000, got ' + chartViewBox(html));

            if (PAGED[screen][tab]) {
                ok(html.indexOf('class="pager"') >= 0, label + ' has a pager');
                ok(/Showing \d+ to \d+ of [\d,]+/.test(html), label + ' count sentence');
                ok(html.indexOf('>First</button>') >= 0, label + ' First');
                ok(html.indexOf('>Previous</button>') >= 0, label + ' Previous');
                ok(html.indexOf('>Next</button>') >= 0, label + ' Next');
                ok(html.indexOf('>Last</button>') >= 0, label + ' Last');
                ok(html.indexOf('Page size') >= 0, label + ' page size label');
                ok(html.indexOf('data-act="page-size"') >= 0, label + ' page size control');
                ok(html.indexOf('data-act="order"') >= 0, label + ' order control');
                /* Load all is offered only when the total is above one page. */
                const total = totalOf(html);
                const hasLoadAll = html.indexOf('>Load all</button>') >= 0;
                ok(hasLoadAll === (total > 15), label + ' Load all offered at total ' + total);
            }
            if (screen === 'job' && tab === 'jobs') {
                ok(html.indexOf('id="job-detail"') >= 0, label + ' job detail panel');
                ok(html.indexOf('Exit code') >= 0, label + ' exit code');
                ok(html.indexOf('Resource requirements') >= 0, label + ' requirements');
                ok(html.indexOf('Inputs and outputs') >= 0, label + ' inputs and outputs');
                ok(html.indexOf('class="stderr"') >= 0, label + ' stderr');
                ok(html.indexOf('Attempts, one result row per run') >= 0, label + ' attempts');
                ok(html.indexOf('download-logs') < 0, label + ' no node logs control');
            }
            if (screen === 'job' && tab === 'lineage') {
                ok(html.indexOf('class="chart lineage"') >= 0, label + ' lineage chart');
                ok(html.indexOf('class="lnode') >= 0, label + ' lineage nodes');
                ok(html.indexOf('class="ledge unmodelled latch"') >= 0, label + ' unmodelled latch edge');
                ok(html.indexOf('scheduler_job_id') >= 0, label + ' verified hop named');
                ok(html.indexOf('workunit.batch_id') >= 0, label + ' batch hop named');
                ok(html.indexOf('result.workunit_id') >= 0, label + ' result hop named');
                ok(html.indexOf('no stored link') >= 0, label + ' footnote names the gap');
                ok(html.indexOf('data-act="job"') >= 0, label + ' job selection in the chart');
                ok(html.indexOf('lnode bgsel') >= 0, label + ' the selected job is highlighted');
            }
            if (screen === 'failure' && tab === 'detail') {
                if (variant === 'fleet') {
                    ok(html.indexOf('data-act="download-logs"') >= 0, label + ' node logs control offered');
                    ok(html.indexOf('super administrator') >= 0, label + ' node logs capability stated');
                } else {
                    ok(html.indexOf('data-act="download-logs"') < 0, label + ' node logs control withheld');
                    ok(html.indexOf('withheld for a tenant administrator') >= 0, label + ' withheld reason stated');
                }
            }
        });
    });
});

// A URL that holds the page, the page size and the order restores them.
const r2 = run('?screen=job&variant=fleet&tab=jobs&order.jobs=duration&dir.jobs=desc&size.jobs=25&page.jobs=25');
/* A page is a whole number of page sizes, so an offset past the last page
   boundary clamps back to it. 31 jobs at 25 a page is two pages. */
ok(/Showing 26 to 31 of 31/.test(r2.els.app.innerHTML), 'offset 25 honoured: ' + count(r2.els.app.innerHTML));
ok(/Page size[\s\S]{0,400}value="25" selected/.test(r2.els.app.innerHTML), 'page size 25 is the selected option');
ok(/Order by Duration/.test(r2.els.app.innerHTML), 'the duration order is offered');
const bar = r2.els['proto-bar'].innerHTML;
ok(bar.indexOf('page size 25') >= 0, 'state bar prints the page size');
ok(bar.indexOf('order duration desc') >= 0, 'state bar prints the order');
/* The address is written when a control is used, which is when the list moves. */
r2.click({ 'data-act': 'page', 'data-table': 'jobs', 'data-page': '0' });
ok(r2.history.url.indexOf('page.jobs=0') >= 0 || r2.history.url.indexOf('page.jobs') < 0,
    'the page is held in the address: ' + r2.history.url);
ok(r2.history.url.indexOf('size.jobs=25') >= 0, 'size held in the address: ' + r2.history.url);
ok(r2.history.url.indexOf('order.jobs=duration') >= 0, 'order held in the address');
ok(r2.history.url.indexOf('dir.jobs=desc') >= 0, 'direction held in the address');

// The order in the address orders the rows.
const asc = run('?screen=watch&variant=fleet&tab=fleet&order.nodes=cores&dir.nodes=asc');
const desc = run('?screen=watch&variant=fleet&tab=fleet&order.nodes=cores&dir.nodes=desc');
const firstRow = (h) => (h.match(/<span class="mono">grid-(\d+)\.example\.com<\/span><\/span><\/td><td class="code">(\d+)</) || ['', '', '']);
ok(firstRow(asc.els.app.innerHTML)[1] !== '' && firstRow(desc.els.app.innerHTML)[1] !== '',
    'a node row and its cores are readable: ' + firstRow(asc.els.app.innerHTML).slice(1).join('/') +
    ' vs ' + firstRow(desc.els.app.innerHTML).slice(1).join('/'));
ok(Number(firstRow(asc.els.app.innerHTML)[2]) <= Number(firstRow(desc.els.app.innerHTML)[2]),
    'the order direction orders the cores: ' + firstRow(asc.els.app.innerHTML)[2] + ' vs ' + firstRow(desc.els.app.innerHTML)[2]);
ok(firstRow(asc.els.app.innerHTML)[1] !== firstRow(desc.els.app.innerHTML)[1],
    'the order direction changes the first row');

// A new order returns the list to its first page.
const r9 = run('?screen=job&variant=fleet&tab=jobs&size.jobs=25&page.jobs=25&order.jobs=duration&dir.jobs=desc');
r9.click({ 'data-act': 'page', 'data-table': 'jobs', 'data-page': '25' });
ok(r9.history.url.indexOf('page.jobs=25') >= 0, 'the offset is in the address before the order changes');
r9.click({ 'data-act': 'order', 'data-table': 'jobs', 'data-order': 'submit', 'data-dir': 'asc' });
ok(r9.history.url.indexOf('page.jobs') < 0, 'a new order returns to the first page: ' + r9.history.url);
ok(r9.history.url.indexOf('order.jobs=submit') >= 0, 'the new order is in the address');

// A new page size returns the list to its first page.
const r10 = run('?screen=job&variant=fleet&tab=jobs&size.jobs=25&page.jobs=25');
r10.click({ 'data-act': 'page-size', 'data-table': 'jobs' }, '50');
ok(r10.history.url.indexOf('page.jobs') < 0, 'a new page size returns to the first page: ' + r10.history.url);
ok(r10.history.url.indexOf('size.jobs=50') >= 0, 'the new page size is in the address');

// Load all marks the table read whole.
const r3 = run('?screen=versions&variant=fleet&tab=catalogue&all.catalogue=1&size.catalogue=4');
ok(r3.els['proto-bar'].innerHTML.indexOf('(Load all)') >= 0, 'Load all is marked in the bar');
ok(r3.els.app.innerHTML.indexOf('>Load all</button>') >= 0, 'Load all is still offered when the list is longer than the page');
ok(/Showing 1 to 4 of \d+/.test(r3.els.app.innerHTML), 'Load all page size honoured: ' + count(r3.els.app.innerHTML));


// A tenant view narrows the job rows.
const r4 = run('?screen=job&variant=tenant&tab=jobs');
ok(r4.els.app.innerHTML.indexOf('Northwind') >= 0, 'tenant view names its tenant');
ok(r4.els.app.innerHTML.indexOf('data-act="download-logs"') < 0, 'tenant job screen has no node logs control');
ok(r4.els['proto-bar'].innerHTML.indexOf('withheld: <b>node logs') >= 0, 'the bar states the withheld control');

// A page past the end returns to the first page.
const r5 = run('?screen=watch&variant=fleet&tab=fleet&size.nodes=1&page.nodes=15');
ok(/Showing 16 to 16 of 16/.test(r5.els.app.innerHTML), 'offset 15 with a size of 1: ' + count(r5.els.app.innerHTML));
const r6 = run('?screen=watch&variant=fleet&tab=fleet&size.nodes=1&page.nodes=999');
ok(/Showing 1 to 1 of 16/.test(r6.els.app.innerHTML), 'an offset past the end returns to page one: ' + count(r6.els.app.innerHTML));

// The lineage picker always offers the selected job.
const r7 = run('?screen=job&variant=fleet&tab=lineage');
const sel = r7.els.app.innerHTML.match(/<option value="(J-\d+)" selected>/);
ok(sel !== null, 'the lineage picker names the selected job');
if (sel) {
    const r8 = run('?screen=job&variant=fleet&tab=lineage&job=' + sel[1]);
    ok(r8.els.app.innerHTML.indexOf('lnode bgsel') >= 0, 'the selected job is highlighted: ' + sel[1]);
    /* Clicking a job in the chart is the same control the rows carry. */
    ok(r8.els.app.innerHTML.indexOf('data-job="' + sel[1] + '"') >= 0, 'the chart carries the job it selected');
}

console.log((checks - bad) + '/' + checks + ' checks passed');
process.exit(bad === 0 ? 0 : 1);
