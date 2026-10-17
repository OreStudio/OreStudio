/* The import register: every attempt in one party, with who ran it, what it
 * moved and what it left. Self-contained: plain JavaScript, mock data, no
 * framework, no build step.
 *
 * The argument: four journeys need a question no per-import screen can answer —
 * "what has happened in this party?" A failed family is found here, a retry is
 * told from a first attempt here, a promotion records its hops here, and a
 * party-scoped user is kept out of another party's rows here. So the register
 * is its own screen, even though no journey names it as its destination.
 *
 * Query string:
 *   ?variant=list|promotion|compare|failed|refused
 *   ?actor=admin|ops            the tenant administrator, or a party-scoped user
 *   ?party=acme-trading|acme-capital-markets   the party the register is scoped to
 *
 * The register does not exist today: there is no list of past imports, only a
 * workflow instance and a shell log.
 */

(function () {
    'use strict';

    var TENANT = 'Acme Corporation';
    var PARTIES = [
        { id: 'acme-trading', name: 'Acme Trading' },
        { id: 'acme-capital-markets', name: 'Acme Capital Markets' }
    ];

    var ACTORS = {
        admin: { id: 'j.smith', name: 'J. Smith', part: 'Tenant administrator', scope: 'tenant' },
        ops: { id: 'k.mori', name: 'K. Mori', part: 'Operations user', scope: 'party' }
    };

    /* The register rows. One row is one import attempt, not one file: a retry
     * is its own row, and so is each promotion hop. */
    var ROWS = [
        { id: 'imp_01J9Z6K4T3QW', at: '2026-10-17 09:14', by: 'J. Smith', trigger: 'Onboard a party', source: 'acme-trading/onboarding/20261017', state: 'applied', parts: 'Conventions 11, trades 64', written: 407, kind: 'onboard' },
        { id: 'imp_01J9X2M8Q1PL', at: '2026-10-16 18:40', by: 'K. Mori', trigger: 'Seed a fresh environment', source: 'ore-samples/Example_1 \u2026 3', state: 'applied', parts: 'Trades 288, conventions 344, quotes 18,442', written: 19074, kind: 'seed', retryOf: 'imp_01J9W8R2N4BC' },
        { id: 'imp_01J9W8R2N4BC', at: '2026-10-16 18:02', by: 'K. Mori', trigger: 'Seed a fresh environment', source: 'ore-samples/Example_1 \u2026 3', state: 'failed', parts: 'Failed at Market (Example_2); 192 trades and 344 conventions stored', written: 536, kind: 'seed' },
        { id: 'imp_01J9V1C7H5DR', at: '2026-10-16 15:38', by: 'D. Osei', trigger: 'Reproduce a defect', source: 'reporting-2026-10-16.orebundle', state: 'applied', parts: 'Trades 1,642, conventions 344, curves 84, quotes 18,442, run 5', written: 20517, kind: 'defect' },
        { id: 'imp_01J9T4F9K2MN', at: '2026-10-16 14:30', by: 'K. Mori', trigger: 'Promote: development \u2192 staging', source: 'promote/dev-to-staging/20261016-1430', state: 'applied', parts: 'Currencies 31, trades 1,642, conventions 344, curves 84, quotes 18,442, run 5', written: 20548, kind: 'promote', manifest: 'man_01J9T4F9K2MN', from: 'development', to: 'staging' },
        { id: 'imp_01J9S7B3C8PQ', at: '2026-10-16 11:05', by: 'J. Smith', trigger: 'Load a vendor feed', source: 'vendor/2026-10-16/market.txt + fixings.txt', state: 'partial', parts: 'Quotes 18,442 kept; fixings 12 refused as duplicates', written: 18442, kind: 'feed' },
        { id: 'imp_01J9P2X6D4RT', at: '2026-10-15 17:02', by: 'A. Tanaka', trigger: 'Retry: replace one dataset', source: 'fixes/swap_conventions_20261016.xml', state: 'applied', parts: 'Swap 214 replaced by key', written: 214, kind: 'one-dataset', retryOf: 'imp_01J9N8K1V7WX' },
        { id: 'imp_01J9N8K1V7WX', at: '2026-10-15 16:44', by: 'A. Tanaka', trigger: 'Replace one dataset', source: 'fixes/swap_conventions_20261016.xml', state: 'failed', parts: 'Refused before writing: 2 rows name values the lists lost', written: 0, kind: 'one-dataset' },
        { id: 'imp_01J9M5H2J6SB', at: '2026-10-14 10:21', by: 'J. Smith', trigger: 'Onboard a party', source: 'acme-trading/onboarding/20261014', state: 'discarded', parts: 'Discarded at review: 1 trade refused', written: 0, kind: 'onboard' },
        { id: 'imp_01J9K3H8D2VF', at: '2026-10-17 08:40', by: 'J. Smith', trigger: 'Load a vendor feed', source: 'vendor/2026-10-17/market.txt + fixings.txt', state: 'applied', parts: 'Quotes 2,104, fixings 96', written: 2200, kind: 'feed', party: 'acme-capital-markets' },
        { id: 'imp_01J9J6C1B9QS', at: '2026-10-16 09:12', by: 'J. Smith', trigger: 'Promote: staging \u2192 production', source: 'promote/staging-to-production/20261016-0912', state: 'applied', parts: 'Conventions 344, curves 84, run 5', written: 433, kind: 'promote', manifest: 'man_01J9J6C1B9QS', from: 'staging', to: 'production', party: 'acme-capital-markets' },
        { id: 'imp_01J9H9R4M7TP', at: '2026-10-15 13:55', by: 'A. Tanaka', trigger: 'Replace one dataset', source: 'fixes/deposit_conventions_20261015.xml', state: 'applied', parts: 'Deposit 96 replaced by key', written: 96, kind: 'one-dataset', party: 'acme-capital-markets' }
    ];

    ROWS.forEach(function (r) {
        if (!r.party) r.party = 'acme-trading';
    });

    /* The sandbox each attempt targeted. A sandbox hangs from an official
     * portfolio and owns a subtree; it is not a node of the official tree.
     * Only books and portfolios carry its id, so a conventions-only attempt
     * names its target and creates no row inside it. */
    var ROW_SANDBOX = {
        imp_01J9Z6K4T3QW: { name: 'Onboard Acme 2026-10-17', path: 'Global Trading \u203a Global Rates \u203a Onboard Acme 2026-10-17', rows: '3 portfolios \u00b7 4 virtual books' },
        imp_01J9X2M8Q1PL: { name: 'Seed samples 2026-10', path: 'Global Trading \u203a Seed samples 2026-10', rows: '3 portfolios \u00b7 12 virtual books' },
        imp_01J9W8R2N4BC: { name: 'Seed samples 2026-10', path: 'Global Trading \u203a Seed samples 2026-10', rows: '3 portfolios \u00b7 12 virtual books' },
        imp_01J9V1C7H5DR: { name: 'Reproduce 2026-10-16', path: 'Global Trading \u203a Global Rates \u203a EUR Rates \u203a Reproduce 2026-10-16', rows: '47 portfolios \u00b7 103 virtual books' },
        imp_01J9T4F9K2MN: { name: 'Reproduce 2026-10-16', path: 'Global Trading \u203a Global Rates \u203a EUR Rates \u203a Reproduce 2026-10-16', rows: '47 portfolios \u00b7 103 virtual books' },
        imp_01J9S7B3C8PQ: { name: 'Vendor feed 2026-10-17', path: 'Global Trading \u203a Global Rates \u203a USD Rates \u203a Vendor feed 2026-10-17', rows: 'no book-tree row: quotes and fixings only' },
        imp_01J9P2X6D4RT: { name: 'Onboard Acme 2026-10-17', path: 'Global Trading \u203a Global Rates \u203a Onboard Acme 2026-10-17', rows: 'no book-tree row: conventions only' },
        imp_01J9N8K1V7WX: { name: 'Onboard Acme 2026-10-17', path: 'Global Trading \u203a Global Rates \u203a Onboard Acme 2026-10-17', rows: 'no book-tree row: conventions only' },
        imp_01J9M5H2J6SB: { name: 'Onboard Acme 2026-10-17', path: 'Global Trading \u203a Global Rates \u203a Onboard Acme 2026-10-17', rows: 'nothing created: discarded at review' },
        imp_01J9K3H8D2VF: { name: 'Vendor feed 2026-10-17', path: 'Global Trading \u203a Global Rates \u203a USD Rates \u203a Vendor feed 2026-10-17', rows: 'no book-tree row: quotes and fixings only' },
        imp_01J9J6C1B9QS: { name: 'Staging promotion 2026-10-16', path: 'Global Trading \u203a Global Rates \u203a Reproduce 2026-10-16', rows: 'no book-tree row: conventions, curves and run documents' },
        imp_01J9H9R4M7TP: { name: 'Onboard Acme Capital 2026-10-15', path: 'Acme Capital Markets \u203a Global Rates \u203a Onboard Acme Capital 2026-10-15', rows: 'no book-tree row: conventions only' }
    };

    ROWS.forEach(function (r) { r.sandbox = ROW_SANDBOX[r.id]; });

    var S = readQuery();

    function readQuery() {
        var q = {};
        window.location.search.replace(/^\?/, '').split('&').forEach(function (pair) {
            if (!pair) return;
            var i = pair.indexOf('=');
            var k = decodeURIComponent(i < 0 ? pair : pair.slice(0, i));
            q[k] = decodeURIComponent(i < 0 ? '' : pair.slice(i + 1));
        });
        return {
            variant: ['list', 'promotion', 'compare', 'failed', 'refused'].indexOf(q.variant) >= 0 ? q.variant : 'list',
            actor: q.actor === 'ops' ? 'ops' : 'admin',
            party: q.party === 'acme-capital-markets' ? 'acme-capital-markets' : 'acme-trading'
        };
    }

    function go(patch) {
        Object.keys(patch).forEach(function (k) { S[k] = patch[k]; });
        var p = [];
        if (S.variant !== 'list') p.push('variant=' + S.variant);
        if (S.actor !== 'admin') p.push('actor=' + S.actor);
        if (S.party !== 'acme-trading') p.push('party=' + S.party);
        window.history.replaceState(null, '', window.location.pathname + (p.length ? '?' + p.join('&') : ''));
        render();
    }

    function esc(v) {
        return String(v === undefined || v === null ? '' : v)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
    }
    function n(v) { return Number(v).toLocaleString('en-GB'); }
    function actor() { return ACTORS[S.actor]; }
    function party() { return PARTIES.filter(function (p) { return p.id === S.party; })[0]; }

    var OTHER_PARTY = PARTIES.filter(function (p) { return p.id !== S.party; })[0];

    function currentParty() {
        return S.actor === 'ops' ? 'acme-trading' : S.party;
    }

    function visible() {
        return ROWS.filter(function (r) { return r.party === currentParty(); });
    }

    /* ------------------------------------------------------------- render */

    function render() {
        document.getElementById('proto-note').textContent =
            'Prototype \u00b7 Data exchange \u00b7 Import register \u00b7 mock data, no backend';
        document.getElementById('app').innerHTML = '<div class="page wide">' + head() + body() + '</div>';
        document.getElementById('proto-bar').innerHTML = bar();
    }

    function head() {
        return '<div class="brand">' + esc(TENANT) + '</div>' +
            '<div class="tenant">Signed in as <b>' + esc(actor().name) + '</b> \u00b7 ' + esc(actor().part) +
            ' \u00b7 ' + (S.actor === 'admin' ? 'the tenant administrator may switch party' : 'the party comes from the session') + '</div>' +
            '<h1>Imports in ' + esc(partyName()) + '</h1>' +
            '<p class="lede">Every attempt in this party, newest first: who ran it, the sandbox it targeted, what it carried, what it wrote and what it refused. ' +
            'A retry is a row of its own, and so is each promotion hop. An import always publishes inside a sandbox, so a row without a sandbox would be a row ' +
            'nobody can explain.</p>' +
            gapbanner() + scopeStrip() + toolbar();
    }

    function partyName() {
        return PARTIES.filter(function (p) { return p.id === currentParty(); })[0].name;
    }

    function gapbanner() {
        return '<div class="gapbanner"><b>Gap</b>No import history exists. The nearest thing is a workflow instance in the shell and a step log. ' +
            'Nothing lists a past import, its file, its counts or its failures per party, so the person reconstructs the first attempt from a log. ' +
            'This register is that list, and it is the promotion record the platform does not keep.</div>';
    }

    function scopeStrip() {
        var rows = visible();
        var applied = rows.filter(function (r) { return r.state === 'applied'; })[0];
        var attention = rows.filter(function (r) { return r.state === 'failed' || r.state === 'partial'; }).length;
        return '<div class="strip">' +
            '<div class="cell"><div class="k">Scope</div><div class="v">' + esc(partyName()) + '</div><div class="s">' +
            (S.actor === 'admin' ? 'switched party, not a wider read' : 'the session\u2019s party, and no selector') + '</div></div>' +
            '<div class="cell"><div class="k">Attempts</div><div class="v">' + rows.length + '</div><div class="s">imports, retries and hops</div></div>' +
            '<div class="cell"><div class="k">Last applied</div><div class="v" style="font-size:14px">' + (applied ? esc(applied.at.slice(5)) : '\u2014') +
            '</div><div class="s">' + (applied ? esc(applied.trigger) : 'none in this party') + '</div></div>' +
            '<div class="cell"><div class="k">Needs attention</div><div class="v">' + attention + '</div><div class="s">failed or partly applied</div></div>' +
            '</div>';
    }

    function toolbar() {
        var out = '<div class="toolbar">';
        if (S.actor === 'admin') {
            out += '<label class="faint">Party</label><select data-act="party">' +
                PARTIES.map(function (p) {
                    return '<option value="' + p.id + '"' + (p.id === S.party ? ' selected' : '') + '>' + esc(p.name) + '</option>';
                }).join('') + '</select>';
        } else {
            out += '<span class="pill info">' + esc(partyName()) + '</span><span class="hint">No party selector: the session decides the scope.</span>';
        }
        out += '<span class="grow"></span><span class="pill">' + visible().length + ' attempts</span></div>';
        return out;
    }

    function body() {
        if (S.variant === 'refused') return refused();
        var out = registerTable();
        if (S.variant === 'promotion') out += promotionRecord();
        if (S.variant === 'compare') out += comparePanel();
        if (S.variant === 'failed') out += failedPanel();
        out += whoCard();
        return out;
    }

    function sandboxCell(sb) {
        if (!sb) return '<span class="faint">none</span>';
        return '<span class="pill info">sandbox</span> <b>' + esc(sb.name) + '</b>' +
            '<span class="sub">' + esc(sb.path) + '<br>' + esc(sb.rows) + '</span>';
    }

    function registerTable() {
        return '<div class="card"><table class="grid"><thead><tr><th>When</th><th>Who</th><th>Why</th><th>Source</th>' +
            '<th>Sandbox targeted</th><th>What it carried</th><th>Written</th><th>State</th><th></th></tr></thead><tbody>' +
            visible().map(function (r) {
                return '<tr' + (S.variant === 'promotion' && r.kind !== 'promote' ? '' : '') + '>' +
                    '<td class="mono">' + esc(r.at) + '</td>' +
                    '<td>' + esc(r.by) + '</td>' +
                    '<td>' + esc(r.trigger) + (r.retryOf ? '<span class="pill info" style="margin-left:6px">retry</span>' : '') + '</td>' +
                    '<td class="mono" style="font-size:11px">' + esc(r.source) + '</td>' +
                    '<td>' + sandboxCell(r.sandbox) + '</td>' +
                    '<td class="sub">' + esc(r.parts) + '</td>' +
                    '<td class="n">' + n(r.written) + '</td>' +
                    '<td>' + pill(r.state) + '</td>' +
                    '<td class="acts">' + action(r) + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<div class="proto-hint">A row is an attempt, not a file. The retry of an attempt keeps the same source; the register shows both, ' +
            'so a first attempt is told from a retry at a glance. That is the one thing the shell cannot do today. Every row also names the sandbox it ' +
            'targeted, because an import publishes inside one; a conventions-only attempt names its sandbox and creates no row in it.</div></div>';
    }

    function action(r) {
        if (r.state === 'failed') {
            return '<button class="btn ghost small" data-act="variant" data-variant="failed">Read the failure</button>';
        }
        if (r.kind === 'promote') {
            return '<button class="btn ghost small" data-act="variant" data-variant="promotion">Read the hop</button>';
        }
        var kind = ['onboard', 'seed', 'defect', 'feed', 'one-dataset'].indexOf(r.kind) >= 0 ? r.kind : 'defect';
        return '<a class="btn ghost small" href="../one-import-run/index.html?case=' + kind + '">Open the run</a>';
    }

    function pill(state) {
        var cls = state === 'applied' ? 'ok' : state === 'failed' ? 'bad' : state === 'partial' ? 'warn' : state === 'discarded' || state === 'expired' ? '' : 'info';
        return '<span class="pill ' + cls + '">' + esc(state) + '</span>';
    }

    function promotionRecord() {
        return '<div class="card mt"><div class="row between"><h2>Promotion record: development \u2192 staging \u2192 production</h2>' +
            '<span class="pill info">one bundle, two hops</span></div>' +
            '<p class="lead">Production never reads development. Each hop is an export to files and an import from files, and each hop is its own record.</p>' +
            '<table class="grid"><thead><tr><th>Hop</th><th>Manifest</th><th>Bundle</th><th>Sandbox</th><th>Landed</th><th>By</th><th>Result</th></tr></thead><tbody>' +
            '<tr><td>development \u2192 staging</td><td class="mono">man_01J9T4F9K2MN</td><td class="mono">promote/dev-to-staging/20261016-1430</td>' +
            '<td><span class="pill info">sandbox</span> Reproduce 2026-10-16</td>' +
            '<td>16 Oct 14:30</td><td>K. Mori</td><td><span class="pill ok">applied</span> 20,548 rows</td></tr>' +
            '<tr><td>staging \u2192 production</td><td class="mono">man_01J9T9Q5W2XZ</td><td class="mono">promote/staging-to-production/20261017-1015</td>' +
            '<td><span class="pill info">sandbox</span> Staging promotion 2026-10-17</td>' +
            '<td>17 Oct 10:15</td><td>K. Mori</td><td><span class="pill warn">waiting</span> checker: R. Alvarez</td></tr>' +
            '</tbody></table>' +
            '<div class="notice warn mt"><span class="title">The platform does not know a promotion happened</span>' +
            'The manifest and this row are the record the journeys ask for. Today the person writes the ticket and the release note; ' +
            'nothing stores which environment received what, when, or by whom. The row is also the answer to "two people promoted the same set": ' +
            'the second one sees the first one here.</div>' +
            '<div class="proto-hint">Gap: no environment bundle, no manifest, no ordering guard and no promotion record. The register does not invent ' +
            'an environment transfer; it records the two file hops.</div></div>';
    }

    function comparePanel() {
        return '<div class="card mt"><h2>First attempt and retry, side by side</h2>' +
            '<p class="lead">The retry of a seed is the case that goes wrong: conventions replace by key, trades do not.</p>' +
            '<table class="grid"><thead><tr><th>Attempt</th><th>Conventions</th><th>Trades saved</th><th>Total trades</th><th>State</th></tr></thead><tbody>' +
            '<tr><td class="mono">imp_01J9W8R2N4BC</td><td class="n">344</td><td class="n">192</td><td class="n">16,612</td><td><span class="pill bad">failed at market</span></td></tr>' +
            '<tr><td class="mono">imp_01J9X2M8Q1PL</td><td class="n">344 replaced by key</td><td class="n">288</td><td class="n">16,900</td><td><span class="pill ok">applied</span></td></tr>' +
            '</tbody></table>' +
            '<div class="notice error mt"><span class="title">The counts do not add up, and nothing warned</span>' +
            '288 trades arrived, not 96. Nothing matched an imported trade by its ORE id, so the retry doubled the trades the first attempt had saved. ' +
            'The convention half converged; the trade half did not. A retry must never apply the same trade twice without the person knowing.</div>' +
            '<div class="proto-hint">Gap: no idempotency key on the import (request_id names the tarball), no trade identity check, no resume and no import history. ' +
            'The owner must decide whether a trade import becomes all-or-nothing or keeps its partial behaviour with an explicit duplicate report.</div></div>';
    }

    function failedPanel() {
        return '<div class="card mt"><h2>What the failed seed left behind</h2>' +
            '<p class="lead">Attempt imp_01J9W8R2N4BC, 16 Oct 18:02, targeted sandbox <b>Seed samples 2026-10</b> ' +
            '(<span class="mono">Global Trading \u203a Seed samples 2026-10</span>) and refused at the market section of Example_2.</p>' +
            '<div class="notice error"><span class="title">536 rows are stored and no rollback reaches them</span>' +
            'The trade import keeps every trade that saved. The run import\u2019s undo covers curves, market data, pricing engines and the run document, ' +
            'and does not cover conventions. Here it is the conventions and the trades that stayed. The trade and portfolio rows are virtual rows in the ' +
            'sandbox; the convention rows are party-wide, because conventions carry no sandbox column.</div>' +
            '<table class="grid mt"><thead><tr><th>Part</th><th>Rows stored</th><th>Undone</th></tr></thead><tbody>' +
            '<tr><td>Trades (Example_1, Example_3)</td><td class="n">192</td><td><span class="pill bad">no: the trade import is partial by design</span></td></tr>' +
            '<tr><td>Conventions (Example_1)</td><td class="n">344</td><td><span class="pill bad">no: the undo skips the conventions document</span></td></tr>' +
            '<tr><td>Curves and market, Example_2</td><td class="n">0</td><td><span class="pill ok">rolled back</span></td></tr>' +
            '</tbody></table>' +
            '<div class="stepfoot"><span class="hint">There is no delete verb for a partial import.</span>' +
            '<span class="ml-auto"><a class="btn" href="../one-import-run/index.html?case=seed&mode=today&state=failed">Open the run</a> ' +
            '<button class="btn primary" data-act="variant" data-variant="compare">Compare with the retry</button></span></div></div>';
    }

    function refused() {
        return '<div class="card"><div class="outcome"><div class="mark bad">\u2715</div>' +
            '<h2>' + esc(OTHER_PARTY.name) + ' is not in this session</h2>' +
            '<p>The register is scoped to the session\u2019s party. ' + esc(OTHER_PARTY.name) + '\u2019s imports are absent, not empty: ' +
            'the read is refused rather than returning a list with no rows.</p></div>' +
            '<div class="notice warn mt"><span class="title">The boundary is the point of this state</span>' +
            'A party must never see another party\u2019s import result or workflow instance. A tenant administrator may switch party, and the register ' +
            'then reads that party and no other. Neither can name a party the session does not act for.</div>' +
            '<div class="stepfoot"><button class="btn primary" data-act="variant" data-variant="list">Back to ' + esc(partyName()) + '</button>' +
            '<span class="ml-auto"><button class="btn ghost" data-act="party" data-party="acme-trading">Switch to Acme Trading</button></span></div></div>';
    }

    function whoCard() {
        var own = visible().filter(function (r) { return r.by === actor().name; });
        return '<div class="card mt"><h2>Who may act on a row</h2>' +
            '<div class="countgrid"><div class="c"><div class="k">May read</div><div class="v" style="font-size:14px">' + esc(partyName()) + ' only</div></div>' +
            '<div class="c"><div class="k">May retry</div><div class="v" style="font-size:14px">the importer or a decider</div>' +
            '<div class="k" style="margin-top:4px">' + own.length + ' rows are ' + esc(actor().name) + '\u2019s own</div></div>' +
            '<div class="c"><div class="k">May switch party</div><div class="v" style="font-size:14px">' +
            (S.actor === 'admin' ? 'yes, as tenant administrator' : 'no') + '</div><div class="k" style="margin-top:4px">the session, not the import</div></div></div>' +
            '<div class="notice info mt mb0"><span class="title">No permission means "may import"</span>' +
            'Each import borrows the write permission of every row it creates, so "may retry" is inferred from being the maker. ' +
            'The owner must decide whether an import permission of its own exists, and whether a party-scoped operations user may import at all.</div>' +
            '</div>';
    }

    /* ---------------------------------------------------------------- bar */

    function bar() {
        var out = '<span class="label">View</span>';
        [['list', 'Register'], ['promotion', 'Promotion record'], ['compare', 'Retry vs first'], ['failed', 'A failure'], ['refused', 'Another party']]
            .forEach(function (v) {
                out += '<button data-act="variant" data-variant="' + v[0] + '"' + (S.variant === v[0] ? ' class="on"' : '') + '>' + v[1] + '</button>';
            });
        out += '<span class="sep">|</span><span class="label">Actor</span>';
        out += '<button data-act="actor" data-actor="admin"' + (S.actor === 'admin' ? ' class="on"' : '') + '>Tenant administrator</button>';
        out += '<button data-act="actor" data-actor="ops"' + (S.actor === 'ops' ? ' class="on"' : '') + '>Operations user</button>';
        return out;
    }

    /* -------------------------------------------------------------- events */

    document.addEventListener('change', function (e) {
        var t = e.target;
        if (t.dataset && t.dataset.act === 'party') go({ party: t.value });
    });

    document.addEventListener('click', function (e) {
        var t = e.target.closest('[data-act]');
        if (!t || t.disabled) return;
        var act = t.dataset.act;
        if (act === 'variant') go({ variant: t.dataset.variant });
        else if (act === 'actor') go({ actor: t.dataset.actor, variant: 'list' });
        else if (act === 'party') go({ party: t.dataset.party });
    });

    render();
}());
