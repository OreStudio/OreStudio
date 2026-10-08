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
            location: { search: query, pathname: '/doc/prototypes/compute/compute/index.html' },
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
    watch: ['dashboard', 'nodes', 'fleet', 'usage'],
    job: ['jobs', 'lineage', 'timeline', 'spread'],
    failure: ['rates', 'where', 'detail'],
    capacity: ['headroom', 'arrivals'],
    versions: ['flight', 'catalogue']
};
const VARIANTS = ['fleet', 'tenant'];
/* The tables that carry a pager, and the ones whose fixture is longer than one
   page, so that the page size, the order and Load all are shown. */
const PAGED = {
    watch: { nodes: false, fleet: true, usage: true },
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
/* Every card chart on the page, so a tab that carries several is read whole
   and not only its first. The node sparklines are class="chart" with a 120 px
   viewBox and are not cards, so this pattern does not match them. */
function cardCharts(html) {
    const re = /<svg viewBox="0 0 (\d+) (\d+)" class="chart ([a-z]+)"/g;
    const out = [];
    let m;
    while ((m = re.exec(html)) !== null) out.push({ w: Number(m[1]), h: Number(m[2]), name: m[3] });
    return out;
}
function sectionOf(html, id) {
    const m = html.match(new RegExp('<section class="card[^"]*" id="' + id + '">([\\s\\S]*?)<\\/section>'));
    return m ? m[1] : '';
}
function intOf(text) { return parseInt(String(text).replace(/,/g, ''), 10); }
function tenantRows(html) {
    const sec = sectionOf(html, 'tenant-usage');
    const re = /<td><span class="cellgroup"><span class="swatch"[^>]*><\/span>([^<]+)<\/span><\/td><td class="code">([\d,]+)<\/td><td class="code">([^<]+)<\/td><td class="code">([^<]+)<\/td><td class="code">([\d,]+) of ([\d,]+)<\/td><td class="code">([\d,]+)<\/td><td class="code">([\s\S]*?)<\/td>/g;
    const out = [];
    let m;
    while ((m = re.exec(sec)) !== null) {
        const failed = m[8].match(/([\d,]+) failed/);
        out.push({
            name: m[1], jobs: intOf(m[2]), grid: m[3], share: m[4],
            coresNow: intOf(m[5]), coresTotal: intOf(m[6]), peak: intOf(m[7]),
            failed: failed ? intOf(failed[1]) : 0
        });
    }
    return out;
}
function statOf(html, label) {
    const m = html.match(new RegExp('<span class="lbl">' + label + '</span><span class="val"><span class="big">([\\d,]+)</span>'));
    return m ? intOf(m[1]) : null;
}
function coresStat(html) {
    const m = html.match(/<span class="lbl">Capacity, cores<\/span><span class="val"><span class="big">([\d,]+)<\/span><span class="tag">of ([\d,]+) used<\/span>/);
    return m ? { used: intOf(m[1]), total: intOf(m[2]) } : null;
}
/* The rendered share bars, read back as the percentages the chart drew. */
function shareBars(html) {
    const sec = sectionOf(html, 'usage-share');
    const re = /<title>([^·<]+) · ([\d.]+)% of (cores|task-minutes)<\/title>/g;
    const out = {};
    let m;
    while ((m = re.exec(sec)) !== null) {
        const short = m[1].trim();
        out[short] = out[short] || {};
        out[short][m[3]] = parseFloat(m[2]);
    }
    return out;
}
function legendLabels(sec) {
    const block = (sec.match(/<div class="legend">([\s\S]*?)<\/div>/) || ['', ''])[1];
    return (block.match(/<span class="swatch"[^>]*><\/span>([^<]+)<\/span>/g) || [])
        .map((s) => s.replace(/<[^>]*>/g, ''));
}
function lineageEdges(html) {
    const re = /<g class="(ledge[^"]*)" data-from="([^"]*)" data-to="([^"]*)">([\s\S]*?)<\/g>/g;
    const out = [];
    let m;
    while ((m = re.exec(html)) !== null) {
        const t = m[4].match(/<title>([\s\S]*?)<\/title>/);
        out.push({ cls: m[1], from: m[2], to: m[3], title: t ? t[1] : '' });
    }
    return out;
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
            /* The global view is a read of the installation's own record, so no
               panel hedges the work counts as a cross-tenant read or as out of
               reach. */
            ok(html.indexOf('cross-tenant') < 0, label + ' no cross-tenant hedging');
            /* Every chart drawn through svgEl keeps the 1000-wide viewBox, so a
               card scales them all alike. The node sparklines are the one
               deliberate exception: they are drawn inside a table row at
               120 px and are not cards. */
            if (html.indexOf('class="chart ') >= 0)
                ok(chartViewBox(html) === 1000, label + ' chart viewBox is 1000, got ' + chartViewBox(html));
            /* Every card chart on the tab, not only the first, so a chart drawn
               after another is not free to magnify its own text. */
            const cards = cardCharts(html);
            ok(cards.length === 0 || cards.every((c) => c.w === 1000),
                label + ' every card chart is 1000 wide, got ' + cards.map((c) => c.name + ':' + c.w).join(', '));
            /* The tenant variant must not name another tenant in any legend,
               title, axis label, footnote or tooltip on any tab. The match is
               case-insensitive so a lower-cased name is caught too. */
            if (variant === 'tenant') {
                const others = ['Helios', 'Meridian', 'System'];
                const named = others.filter((n) => new RegExp(n, 'i').test(html));
                ok(named.length === 0, label + ' names no other tenant, got ' + named.join(','));
                ok(!/\b(helios|meridian|system)\b/i.test(html), label + ' carries no other tenant id');
            }

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
                /* The report run to the scheduler's own job instance is carried
                   by report_instance.trigger_run_id, so it is a modelled hop and
                   is drawn solid, not described as a missing row. */
                ok(html.indexOf('report_instance.trigger_run_id') >= 0, label + ' trigger_run_id hop named');
                ok(html.indexOf('scheduler job instance') >= 0, label + ' scheduler job instance node drawn');
                ok(html.indexOf('no row holds it') < 0, label + ' footnote no longer denies the trigger_run_id row');
                const edges = lineageEdges(html);
                ok(edges.length > 0, label + ' lineage edges carry their endpoints');
                ok(edges.filter((e) => e.title.indexOf('no stored link') < 0).every((e) => e.cls === 'ledge'),
                    label + ' every modelled edge is solid');
                const trig = edges.filter((e) => e.title.indexOf('report_instance.trigger_run_id') >= 0);
                ok(trig.length === 1, label + ' one trigger_run_id edge');
                ok(trig.length === 1 && trig[0].cls === 'ledge' &&
                    trig[0].from === 'schedinst' && trig[0].to === 'reportinstance',
                    label + ' the trigger_run_id hop is solid from the job instance to the report run');
                const latch = edges.filter((e) => e.title.indexOf('no stored link') >= 0);
                ok(latch.length === 1 && latch[0].cls.indexOf('unmodelled') >= 0,
                    label + ' the batch hop stays dashed');
                /* Every result hangs off the selected job's own workunit, not
                   the first workunit the batch happens to hold. */
                const sel = html.match(/<rect class="lnode bgsel" data-node="([^"]+)"/);
                const resultEdges = edges.filter((e) => e.title === 'ores.compute.result.workunit_id');
                ok(sel !== null, label + ' the selected workunit node names itself: ' + (sel && sel[1]));
                ok(resultEdges.length > 0, label + ' result edges are drawn');
                ok(sel !== null && resultEdges.every((e) => e.from === sel[1]),
                    label + ' every result attaches to the selected workunit, got ' +
                    resultEdges.map((e) => e.from).join(',') + ' vs ' + (sel && sel[1]));
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
            if (screen === 'watch' && tab === 'usage') {
                /* The global view: the four charts and the tenant table, each
                   chart naming the ledger as its source, and the allocation
                   chart moved here rather than standing alone. */
                ok(html.indexOf('id="usage-time"') >= 0, label + ' usage over time chart');
                ok(html.indexOf('id="usage-share"') >= 0, label + ' share of the grid chart');
                ok(html.indexOf('id="usage-jobs"') >= 0, label + ' jobs per tenant chart');
                ok(html.indexOf('id="allocation"') >= 0, label + ' allocation by tenant chart');
                ok(html.indexOf('id="tenant-usage"') >= 0, label + ' tenant table');
                ok(html.indexOf('The series are the usage the installation recorded') >= 0, label + ' ledger source stated');
                ok((html.match(/The series are the usage the installation recorded/g) || []).length >= 4,
                    label + ' every usage chart states its source');
                ok(html.indexOf('class="swatch"') >= 0, label + ' tenant colour in the table');
                if (variant === 'tenant') {
                    ok(html.indexOf('Northwind') >= 0, label + ' tenant view names its tenant');
                    ok(html.indexOf('Helios') < 0, label + ' tenant view withholds other tenants');
                } else {
                    ok(html.indexOf('Northwind') >= 0 && html.indexOf('Helios') >= 0 && html.indexOf('Meridian') >= 0,
                        label + ' the whole ledger names every tenant');
                }
            }
            if (screen === 'capacity' && tab === 'headroom') {
                ok(html.indexOf('id="allocation"') < 0, label + ' allocation no longer stands alone on capacity');
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

// ---------------------------------------------------------------------------
// Cross-panel agreement: one source of truth for each figure, read back from
// separate screens so a chart cannot contradict the panel beside it.
// ---------------------------------------------------------------------------
const dash = run('?screen=watch&variant=fleet&tab=dashboard').els.app.innerHTML;
const usage = run('?screen=watch&variant=fleet&tab=usage').els.app.innerHTML;
const jobsTab = run('?screen=job&variant=fleet&tab=jobs').els.app.innerHTML;
const rates = run('?screen=failure&variant=fleet&tab=rates').els.app.innerHTML;

const dashCores = coresStat(dash);
const ledger = tenantRows(usage);
ok(dashCores !== null, 'the dashboard states cores used of total');
ok(ledger.length === 4, 'the ledger lists four tenants, got ' + ledger.length);
if (dashCores) {
    const ledgerCores = ledger.reduce((a, r) => a + r.coresNow, 0);
    ok(ledgerCores === dashCores.used,
        'cores: the ledger rows sum to the dashboard figure (' + ledgerCores + ' vs ' + dashCores.used + ')');
    ok(ledger.every((r) => r.coresTotal === dashCores.total),
        'cores: every ledger row names the same fleet total');
    const shareCores = sectionOf(usage, 'usage-share').match(/>([\d,]+) cores<\/text>/);
    ok(shareCores !== null && intOf(shareCores[1]) === dashCores.used,
        'cores: the usage share chart total equals the dashboard, got ' + (shareCores && shareCores[1]));
}

const dashJobs = statOf(dash, 'Jobs over the range');
const ledgerJobs = ledger.reduce((a, r) => a + r.jobs, 0);
const listedJobs = totalOf(jobsTab);
ok(dashJobs !== null, 'the dashboard states the job total');
ok(dashJobs === ledgerJobs, 'jobs: the dashboard equals the ledger total (' + dashJobs + ' vs ' + ledgerJobs + ')');
ok(listedJobs === ledgerJobs, 'jobs: the job list total equals the ledger total (' + listedJobs + ' vs ' + ledgerJobs + ')');

const ledgerFailed = ledger.reduce((a, r) => a + r.failed, 0);
const versionSec = sectionOf(rates, 'failversion');
const versionFailed = (versionSec.match(/· \d+ of \d+ jobs failed/g) || [])
    .reduce((a, s) => a + intOf(s.match(/· (\d+) of/)[1]), 0);
const nodeSec = sectionOf(rates, 'failnode');
const nodeFootnote = nodeSec.match(/The (\d+) failed jobs produced the (\d+) failed tasks here/);
const nodeBars = (nodeSec.match(/<text class="tick mid strong"[^>]*>(\d+)<\/text>/g) || [])
    .reduce((a, s) => a + intOf(s.match(/>(\d+)</)[1]), 0);
ok(ledgerFailed === versionFailed,
    'failed jobs: the ledger equals the release panel (' + ledgerFailed + ' vs ' + versionFailed + ')');
ok(nodeFootnote !== null, 'the by-node panel names the failed jobs and failed tasks');
if (nodeFootnote) {
    ok(intOf(nodeFootnote[1]) === versionFailed, 'failed jobs: the by-node panel equals the release panel');
    ok(intOf(nodeFootnote[2]) === nodeBars,
        'failed tasks: the by-node panel states the bar total (' + nodeFootnote[2] + ' vs ' + nodeBars + ')');
    ok(intOf(nodeFootnote[2]) >= intOf(nodeFootnote[1]), 'failed tasks are at least the failed jobs they came from');
}

// The usage-share footnote must follow the bars it sits under, so it is
// re-derived here from the rendered percentages.
const bars = shareBars(usage);
let timeLeader = null, coreLeader = null, timeGap = 0, coreGap = 0;
Object.keys(bars).forEach((short) => {
    const gap = (bars[short]['task-minutes'] || 0) - (bars[short].cores || 0);
    if (gap > timeGap) { timeGap = gap; timeLeader = short; }
    if (-gap > coreGap) { coreGap = -gap; coreLeader = short; }
});
const shareFoot = sectionOf(usage, 'usage-share');
ok(timeLeader !== null && shareFoot.indexOf(timeLeader + ' takes a larger share of the time') >= 0,
    'usage share footnote names the time-heavy tenant the bars show: ' + timeLeader);
ok(coreLeader !== null && shareFoot.indexOf(coreLeader + ' takes the reverse') >= 0,
    'usage share footnote names the core-heavy tenant the bars show: ' + coreLeader);
/* The percentages in the sentence must be the ones the bars drew, not only the
   right tenant name. */
if (timeLeader && coreLeader) {
    const timeClause = timeLeader + ' takes a larger share of the time (' +
        bars[timeLeader]['task-minutes'].toFixed(1) + '%) than of the cores (' +
        bars[timeLeader].cores.toFixed(1) + '%)';
    const coreClause = coreLeader + ' takes the reverse, a larger share of the cores (' +
        bars[coreLeader].cores.toFixed(1) + '%) than of the time (' +
        bars[coreLeader]['task-minutes'].toFixed(1) + '%)';
    ok(shareFoot.indexOf(timeClause) >= 0, 'usage share footnote states the time-heavy percentages: ' + timeClause);
    ok(shareFoot.indexOf(coreClause) >= 0, 'usage share footnote states the core-heavy percentages: ' + coreClause);
}

// The arrivals legend names only the tenants the plot draws, in both variants.
['fleet', 'tenant'].forEach((variant) => {
    const cap = run('?screen=capacity&variant=' + variant + '&tab=arrivals').els.app.innerHTML;
    const sec = sectionOf(cap, 'arrivals');
    const rowTenants = Array.from(new Set(
        (sec.match(/<text class="tick rowlabel"[^>]*>[\d:]+ [A-Za-z]+<\/text>/g) || [])
            .map((s) => s.replace(/<[^>]*>/g, '').replace(/^[\d:]+ /, ''))));
    const legend = legendLabels(sec);
    ok(rowTenants.length > 0, 'arrivals/' + variant + ' draws at least one row');
    ok(legend.length === rowTenants.length && rowTenants.every((t) => legend.indexOf(t) >= 0),
        'arrivals/' + variant + ' legend matches the plot: legend ' + legend.join(',') +
        ' vs rows ' + rowTenants.join(','));
});

// The arrivals timeline reads the same clock in its axis, its row labels and
// its footnote, so a label cannot say a different hour from its own axis.
{
    const cap = run('?screen=capacity&variant=fleet&tab=arrivals').els.app.innerHTML;
    const sec = sectionOf(cap, 'arrivals');
    const ticks = (sec.match(/<text class="tick mid"[^>]*>([\d:]+)<\/text>/g) || [])
        .map((s) => s.replace(/<[^>]*>/g, ''));
    const rowTimes = (sec.match(/<text class="tick rowlabel"[^>]*>([\d:]+) [A-Za-z]+<\/text>/g) || [])
        .map((s) => s.replace(/<[^>]*>/g, '').split(' ')[0]);
    const risk = sec.match(/The 16-core risk run at ([\d:]+)/);
    ok(ticks.length > 0 && ticks[0] === '15:00', 'arrivals axis starts at now, 15:00, got ' + ticks[0]);
    ok(rowTimes.length > 0 && rowTimes[0] === '15:25', 'the first arrival is 25 minutes after now, got ' + rowTimes[0]);
    ok(risk !== null && rowTimes.indexOf(risk[1]) >= 0,
        'the arrivals footnote time matches a drawn arrival row: ' + (risk && risk[1]) + ' vs ' + rowTimes.join(','));
}

console.log((checks - bad) + '/' + checks + ' checks passed');
process.exit(bad === 0 ? 0 : 1);
