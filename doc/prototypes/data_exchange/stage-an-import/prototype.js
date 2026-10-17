/* Stage an import and review it before it lands. Self-contained: plain
 * JavaScript, mock data, no framework, no build step.
 *
 * The argument: the review is not a step inside the import form. It is its own
 * screen, because a second person decides there and the maker must not. One
 * staged import shows, per family and per dataset, what is new, what replaces a
 * live row, what is unchanged and what is refused — before any live row moves.
 * Publish applies every staged family in one transaction; discard removes the
 * staged rows and touches nothing.
 *
 * States come from the query string:
 *   ?case=onboard|seed|defect|feed|one-dataset|promote|party
 *   ?actor=admin|ops|maintainer|support   the maker who staged the import
 *   ?state=review|decide|declined|applied|expired|refused
 *   ?family=swap                         the family whose rows are open
 *   ?decider=m.risk|o.ops               who answers on the decide state
 *   ?check=lookup|none                   make one check fail on the decide state
 *   ?target=<sandbox id>                 the sandbox the import targets
 *
 * The proposed mechanism is the draft design's: begin, stage per family, commit
 * once. The screen is the one the journeys say does not exist.
 */

(function () {
    'use strict';

    var TENANT = 'Acme Corporation';
    var PARTY = 'Acme Trading';
    var IMPORT_ID = 'imp_01J9Z6K4T3QW';

    var MAKERS = {
        admin: { id: 'j.smith', name: 'J. Smith', part: 'Tenant administrator' },
        ops: { id: 'k.mori', name: 'K. Mori', part: 'Operations user' },
        maintainer: { id: 'a.tanaka', name: 'A. Tanaka', part: 'Reference-data maintainer' },
        support: { id: 'd.osei', name: 'D. Osei', part: 'Support engineer' }
    };

    /* Who decides a part. A convention is read by every report, so Market Risk
     * decides it. Operations decides the trade set. The maker never decides. */
    var DECIDERS = {
        conventions: { id: 'm.risk', name: 'T. Brandt', part: 'Market Risk' },
        trades: { id: 'o.ops', name: 'K. Mori', part: 'Operations' },
        curves: { id: 'm.risk', name: 'T. Brandt', part: 'Market Risk' },
        market: { id: 'm.risk', name: 'T. Brandt', part: 'Market Risk' },
        run: { id: 'o.ops', name: 'K. Mori', part: 'Operations' }
    };

    var FAMILIES = [
        { key: 'swap', name: 'Swap', ent: 'swap_conventions', rows: 214, live: 1891, on: true },
        { key: 'ois', name: 'OIS', ent: 'ois_conventions', rows: 38, live: 402, on: true },
        { key: 'deposit', name: 'Deposit', ent: 'deposit_conventions', rows: 96, live: 733, on: true },
        { key: 'fra', name: 'FRA', ent: 'fra_conventions', rows: 22, live: 168, on: true },
        { key: 'future', name: 'Future', ent: 'future_conventions', rows: 12, live: 94, on: true },
        { key: 'bond_yield', name: 'Bond yield', ent: 'bond_yield_conventions', rows: 18, live: 212, on: true },
        { key: 'cds', name: 'CDS', ent: 'cds_conventions', rows: 9, live: 61, on: true },
        { key: 'cross_currency_basis', name: 'Cross-currency basis', ent: 'cross_currency_basis_conventions', rows: 14, live: 187, on: true },
        { key: 'inflation_swap', name: 'Inflation swap', ent: 'inflation_swap_conventions', rows: 7, live: 54, on: true },
        { key: 'commodity_forward', name: 'Commodity forward', ent: 'commodity_forward_conventions', rows: 5, live: 37, on: true },
        { key: 'average_ois', name: 'Average OIS', ent: 'average_ois_conventions', rows: 3, live: 18, on: true },
        { key: 'fx', name: 'FX', ent: 'fx_conventions', rows: 6, live: 0, on: false, skipped: true }
    ];

    var CASES = {
        onboard: { title: 'Onboard a party', source: 'acme-trading/onboarding/20261017', parts: ['trades', 'conventions'], extras: [], omitted: ['Curves', "Today's market", 'Pricing engines', 'Market data'], omittedWhy: 'the journey must bring conventions and a portfolio, and nothing else' },
        seed: { title: 'Seed a fresh environment', source: 'ore-samples/Example_1 \u2026 Example_3', parts: ['trades', 'conventions', 'curves', 'market'], extras: [], omitted: ['Pricing engines'], omittedWhy: 'no sample tree bound an engine in this run', duplicate: true },
        defect: { title: 'Reproduce a defect', source: 'reporting-2026-10-16.orebundle', parts: ['trades', 'conventions', 'curves', 'market', 'run'], extras: ['Compute job input', 'Engine package'], omitted: [], omittedWhy: '' },
        feed: { title: 'Load a vendor feed', source: 'vendor/2026-10-16/market.txt + fixings.txt', parts: ['market'], extras: [], omitted: ['Conventions', 'Curves', 'Trades'], omittedWhy: 'the file carries quotes and fixings only' },
        'one-dataset': { title: 'Replace one dataset', source: 'fixes/swap_conventions_20261016.xml', parts: ['conventions'], extras: [], omitted: ['Curves', 'Market data', 'Trades', 'Pricing engines'], omittedWhy: 'the file carries one family', only: 'swap' },
        promote: { title: 'Promote an environment', source: 'promote/dev-to-staging/20261016-1430', parts: ['trades', 'conventions', 'curves', 'market', 'run'], extras: ['Currencies (31, added only when missing)'], omitted: [], omittedWhy: '', hop: 'development \u2192 staging' },
        party: { title: 'Import into my party', source: 'acme-trading/backfill/20261017', parts: ['trades', 'conventions'], extras: [], omitted: ['Curves', "Today's market", 'Pricing engines', 'Market data'], omittedWhy: 'the file carries conventions and a portfolio only' }
    };

    var CASE_ORDER = ['onboard', 'seed', 'defect', 'feed', 'one-dataset', 'promote', 'party'];
    var MAKER_ORDER = ['admin', 'ops', 'maintainer', 'support'];

    /* The chosen sandbox, read from the book tree on the entry screen. A
     * sandbox is anchored at an official portfolio and owns a subtree; it is
     * not a node of the official tree. Only books and portfolios carry a
     * sandbox_id, so the rows grouped below say which of them land inside. */
    var TARGETS = {
        'sb-import-repro': { name: 'Reproduce 2026-10-16', anchor: 'Global Trading \u203a Global Rates \u203a EUR Rates',
            path: 'Global Trading \u203a Global Rates \u203a EUR Rates \u203a Reproduce 2026-10-16',
            purpose: 'import', status: 'open', owner: 'D. Osei', portfolios: 2, books: 2 },
        'sb-import-onboard': { name: 'Onboard Acme 2026-10-17', anchor: 'Global Trading \u203a Global Rates',
            path: 'Global Trading \u203a Global Rates \u203a Onboard Acme 2026-10-17',
            purpose: 'import', status: 'open', owner: 'J. Smith', portfolios: 1, books: 1 },
        'sb-sample-seed': { name: 'Seed samples 2026-10', anchor: 'Global Trading',
            path: 'Global Trading \u203a Seed samples 2026-10',
            purpose: 'sample', status: 'open', owner: 'K. Mori', portfolios: 1, books: 2 },
        'sb-import-feed': { name: 'Vendor feed 2026-10-17', anchor: 'Global Trading \u203a Global Rates \u203a USD Rates',
            path: 'Global Trading \u203a Global Rates \u203a USD Rates \u203a Vendor feed 2026-10-17',
            purpose: 'import', status: 'open', owner: 'J. Smith', portfolios: 1, books: 1 }
    };

    var CASE_TARGET = {
        onboard: 'sb-import-onboard', seed: 'sb-sample-seed', defect: 'sb-import-repro',
        feed: 'sb-import-feed', 'one-dataset': 'sb-import-onboard',
        promote: 'sb-import-repro', party: 'sb-import-onboard'
    };

    var S = readQuery();

    function readQuery() {
        var q = {};
        window.location.search.replace(/^\?/, '').split('&').forEach(function (pair) {
            if (!pair) return;
            var i = pair.indexOf('=');
            var k = decodeURIComponent(i < 0 ? pair : pair.slice(0, i));
            q[k] = decodeURIComponent(i < 0 ? '' : pair.slice(i + 1));
        });
        var name = CASES[q['case']] ? q['case'] : 'onboard';
        var st = {
            'case': name,
            maker: MAKERS[q.actor] ? q.actor : (q['case'] === 'party' ? 'ops' : 'admin'),
            state: ['review', 'decide', 'declined', 'applied', 'expired'].indexOf(q.state) >= 0 ? q.state : 'review',
            family: q.family || null,
            decider: DECIDERS.conventions.id === q.decider || DECIDERS.trades.id === q.decider ? q.decider : 'm.risk',
            check: q.check === 'lookup' ? 'lookup' : 'none',
            target: TARGETS[q.target] ? q.target : CASE_TARGET[name]
        };
        return st;
    }

    function go(patch) {
        Object.keys(patch).forEach(function (k) { S[k] = patch[k]; });
        var p = [];
        if (S['case'] !== 'onboard') p.push('case=' + S['case']);
        if (S.maker !== (S['case'] === 'party' ? 'ops' : 'admin')) p.push('actor=' + S.maker);
        if (S.state !== 'review') p.push('state=' + S.state);
        if (S.family) p.push('family=' + S.family);
        if (S.decider !== 'm.risk') p.push('decider=' + S.decider);
        if (S.check !== 'none') p.push('check=' + S.check);
        if (S.target !== CASE_TARGET[S['case']]) p.push('target=' + S.target);
        window.history.replaceState(null, '', window.location.pathname + (p.length ? '?' + p.join('&') : ''));
        render();
    }

    function esc(v) {
        return String(v === undefined || v === null ? '' : v)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
    }
    function n(v) { return Number(v).toLocaleString('en-GB'); }
    function cas() { return CASES[S['case']]; }
    function maker() { return MAKERS[S.maker]; }
    function deciderFor(part) { return DECIDERS[part] || DECIDERS.conventions; }
    function target() { return TARGETS[S.target]; }

    /* A part lands in the sandbox only when it is a book-tree family. The
     * conventions, curve, market and run rows carry no sandbox_id. */
    function landsIn(p) { return p.key === 'trades' ? 'sandbox' : 'party'; }

    /* The staged plan. The numbers are the ones the mock file would produce; the
     * shape is what matters: per family, new, replaced, unchanged, refused. */
    function plan() {
        var c = cas();
        var parts = [];
        if (c.parts.indexOf('trades') >= 0) {
            var trades = S['case'] === 'seed' ? 288 : S['case'] === 'promote' || S['case'] === 'defect' ? 1642 : 64;
            parts.push({
                key: 'trades', name: 'Trades and portfolios', unit: 'trades',
                staged: trades, add: trades, replace: 0, unchanged: 0, refused: S['case'] === 'onboard' ? 1 : 0,
                live: 16420, policy: 'attach, never overwrite', order: 1,
                note: S['case'] === 'seed' ? 'A re-import matches nothing by ORE id, so the trades in the second run would be new rows.' : 'Trades are attached to the named portfolio and never overwrite a live book.'
            });
        }
        if (c.parts.indexOf('conventions') >= 0) {
            FAMILIES.forEach(function (f) {
                if (c.only && f.key !== c.only) return;
                var add = Math.round(f.rows * 0.28);
                var same = Math.round(f.rows * 0.10);
                var refused = f.key === 'swap' ? 2 : f.key === 'deposit' ? 1 : 0;
                var replace = f.rows - add - same - refused;
                parts.push({
                    key: f.key, name: f.name, unit: 'rows', family: true,
                    ent: f.ent, staged: f.rows,
                    add: add, replace: replace, unchanged: same,
                    refused: refused, live: f.live,
                    policy: f.on ? (f.key === 'cds' ? 'add missing world' : 'replace by key') : 'skip',
                    skipped: f.skipped, order: 10
                });
            });
        }
        if (c.parts.indexOf('curves') >= 0) {
            parts.push({ key: 'curves', name: 'Curve configurations', unit: 'curves', staged: 84, add: 75, replace: 9, unchanged: 0, refused: 0, live: 612, policy: 'replace by key', order: 40 });
        }
        if (c.parts.indexOf('market') >= 0) {
            parts.push({ key: 'market', name: 'Quotes and fixings', unit: 'rows', staged: S['case'] === 'feed' ? 19646 : 18442, add: S['case'] === 'feed' ? 19646 : 17889, replace: S['case'] === 'feed' ? 0 : 553, unchanged: 0, refused: 0, live: 218340, policy: 'append by key', order: 50 });
            if (S['case'] === 'feed') parts[parts.length - 1].name = 'Quotes (market.txt)';
            if (S['case'] === 'feed') parts.push({ key: 'fixings', name: 'Fixings (fixings.txt)', unit: 'rows', staged: 1204, add: 1204, replace: 0, unchanged: 0, refused: 0, live: 1190, policy: 'append by key', order: 51 });
        }
        if (c.parts.indexOf('run') >= 0) {
            parts.push({ key: 'run', name: 'Run documents', unit: 'bindings', staged: 5, add: 2, replace: 3, unchanged: 0, refused: 0, live: 47, policy: 'replace by key', order: 60 });
        }
        return parts;
    }

    var REFUSALS = {
        swap: [
            { row: 'GBP-12M-SWAP', field: 'business_day_convention', live: 'MODFOLLOWING', incoming: 'MODFOLLOWINGX', why: 'the list no longer holds MODFOLLOWINGX' },
            { row: 'GBP-6M-SWAP', field: 'fixed_leg_day_count', live: 'ACT/365F', incoming: 'ACT/365', why: 'not a day counter the tenant holds' }
        ],
        deposit: [
            { row: 'JPY-TONAR', field: 'currency', live: 'JPY', incoming: 'XJPY', why: 'the tenant does not hold XJPY' }
        ]
    };

    function refusals() {
        var out = [];
        plan().forEach(function (p) {
            if (p.family && REFUSALS[p.key]) REFUSALS[p.key].forEach(function (r) { out.push(Object.assign({ family: p.name }, r)); });
        });
        return out;
    }

    function totals() {
        var t = { staged: 0, add: 0, replace: 0, unchanged: 0, refused: 0 };
        plan().forEach(function (p) {
            t.staged += p.staged; t.add += p.add; t.replace += p.replace;
            t.unchanged += p.unchanged; t.refused += p.refused;
        });
        return t;
    }

    function skippedFamilies() {
        return cas().parts.indexOf('conventions') >= 0 && S['case'] !== 'one-dataset'
            ? FAMILIES.filter(function (f) { return f.skipped; }) : [];
    }

    /* ------------------------------------------------------------- render */

    function render() {
        document.getElementById('proto-note').textContent =
            'Prototype \u00b7 Data exchange \u00b7 Stage an import \u00b7 mock data, no backend, no live rows written';
        document.getElementById('app').innerHTML = '<div class="page wide">' + head() + body() + '</div>';
        document.getElementById('proto-bar').innerHTML = bar();
    }

    function head() {
        var t = totals();
        return '<div class="brand">' + esc(TENANT) + '</div>' +
            '<div class="tenant">Party <b>' + esc(PARTY) + '</b> \u00b7 source <b>' + esc(cas().source) + '</b> \u00b7 target <b>' +
            esc(target().name) + '</b> \u00b7 staged by ' + esc(maker().name) + ' \u00b7 import <span class="mono">' + IMPORT_ID + '</span></div>' +
            '<h1>Review before it lands</h1>' +
            '<p class="lede">One staged import. Per family: what is new, what replaces a live row, what is unchanged and what is refused. ' +
            'Nothing here is visible to a reader of the party.</p>' +
            gapbanner() +
            stepstrip() +
            stateStrip();
    }

    function gapbanner() {
        return '<div class="gapbanner"><b>Gap</b>There is no staging surface today: no begin, stage, commit or discard verb, no staged rows, ' +
            'no expiry and no sweep. The only rehearsal is to import into an open sample sandbox, which writes real rows and still cannot ' +
            'show a per-family summary before the writes. This screen is exactly that missing surface.</div>';
    }

    function stepstrip() {
        function s(nm, cls) { return '<span class="s ' + cls + '">' + nm + '</span>'; }
        return '<div class="stepstrip">' +
            s('1 Source', 'done') + '<span class="arrow">\u203a</span>' +
            s('2 Families', 'done') + '<span class="arrow">\u203a</span>' +
            s('3 Review', S.state === 'review' ? 'now' : 'done') + '<span class="arrow">\u203a</span>' +
            s('4 Publish', S.state === 'decide' ? 'now' : '') + '<span class="arrow">\u203a</span>' +
            s('5 Result', ['applied', 'declined', 'expired', 'refused'].indexOf(S.state) >= 0 ? 'now' : '') +
            '</div>';
    }

    function stateStrip() {
        var t = totals();
        return '<div class="strip">' +
            '<div class="cell"><div class="k">Import state</div><div class="v">' + stateWord() + '</div><div class="s">' +
            (S.state === 'expired' ? 'staged rows swept, nothing applied' : 'idle expiry 24 h \u00b7 expires in 22 h') + '</div></div>' +
            '<div class="cell"><div class="k">Staged rows</div><div class="v">' + n(t.staged) + '</div><div class="s">' + n(t.add) + ' new \u00b7 ' + n(t.replace) + ' replace</div></div>' +
            '<div class="cell"><div class="k">Refused before write</div><div class="v">' + n(t.refused) + '</div><div class="s">named row and field</div></div>' +
            '<div class="cell"><div class="k">Live rows changed</div><div class="v">' + (S.state === 'applied' ? n(t.staged - t.refused) : '0') + '</div><div class="s">' +
            (S.state === 'applied' ? 'published at 16:02Z' : 'a reader sees no change') + '</div></div>' +
            '</div>';
    }

    function stateWord() {
        return { review: 'staging', decide: 'staging \u00b7 awaiting decision', declined: 'discarded', applied: 'applied', expired: 'expired', refused: 'staging \u00b7 refused' }[S.state];
    }

    function body() {
        var top = targetCard();
        if (S.state === 'expired') return top + expiredState();
        if (S.state === 'declined') return top + declinedState();
        if (S.state === 'applied') return top + appliedState();
        var left = '<div>' + planCard() + untouchedCard() + '</div>';
        var right = '<div>' + (S.state === 'decide' ? decideCard() : decisionCard()) + refusalCard() + '</div>';
        return top + '<div class="split">' + left + right + '</div>';
    }

    function targetCard() {
        var t = target();
        var made = { onboard: [3, 4], seed: [3, 12], defect: [47, 103], promote: [47, 103],
            party: [3, 4], feed: [0, 0], 'one-dataset': [0, 0] }[S['case']];
        var bookRows = plan().filter(function (p) { return p.key === 'trades'; });
        var partyRows = plan().filter(function (p) { return p.key !== 'trades'; });
        var bookCount = bookRows.reduce(function (a, p) { return a + p.staged; }, 0);
        return '<div class="card"><h2>The sandbox these rows land in</h2>' +
            '<p class="lead">The import publishes into the sandbox chosen on the book tree. A sandbox hangs from an official portfolio; ' +
            'it is not a node of the official tree, and the rows inside carry its id.</p>' +
            '<div class="strip">' +
            '<div class="cell"><div class="k">Sandbox</div><div class="v" style="font-size:14px">' + esc(t.name) + '</div>' +
            '<div class="s">' + esc(t.id) + ' \u00b7 ' + esc(t.purpose) + ' \u00b7 ' + esc(t.status) + '</div></div>' +
            '<div class="cell"><div class="k">Anchor</div><div class="v" style="font-size:13px">' + esc(t.anchor) + '</div>' +
            '<div class="s">fixed for the sandbox\u2019s life</div></div>' +
            '<div class="cell"><div class="k">Inside, after</div><div class="v">' + made[0] + ' / ' + made[1] + '</div>' +
            '<div class="s">virtual portfolios / virtual books created</div></div>' +
            '<div class="cell"><div class="k">Official rows changed</div><div class="v">0</div>' +
            '<div class="s">no official book or portfolio moves</div></div>' +
            '</div>' +
            '<div class="split"><div class="notice success mb0"><span class="title">Inside the sandbox</span>' +
            (bookRows.length
                ? 'Book-tree family: <b>Trades and portfolios</b> \u2014 ' + n(bookCount) + ' rows, creating ' + made[0] +
                  ' virtual portfolios and ' + made[1] + ' virtual books under <span class="sbpath"><b>' + esc(t.path) + '</b></span>. ' +
                  'Each book is written with <span class="mono">sandbox_id = ' + esc(S.target) + '</span>, no ledger reference and ' +
                  '<span class="mono">is_sweepable = false</span>. The root portfolio it creates has no parent.'
                : '<b>No book-tree family</b> in this file. The sandbox keeps the ' + t.portfolios + ' portfolios and ' + t.books + ' virtual books it holds.') +
            '</div>' +
            '<div class="notice warn mb0"><span class="title">Outside the sandbox</span>' +
            (partyRows.length
                ? 'The model gives a sandbox column to books and portfolios only. These families still publish to the party and have no sandbox id: ' +
                  esc(partyRows.map(function (p) { return p.name; }).join(', ')) + '. ' +
                  'Nothing else in the party changes: every other book, portfolio and ledger row keeps its values.'
                : 'Nothing else in the party changes. The file carries only book-tree rows.') +
            '</div></div>' +
            '<div class="notice info mt"><span class="title">The two trees cannot mix</span>' +
            'A book and its portfolio belong to the same sandbox or both to none, so an official book never sits under a sandbox portfolio. ' +
            'A sandbox\u2019s root portfolios have no parent. A virtual book carries no ledger reference and is not sweepable.</div>' +
            '</div>';
    }

    function planCard() {
        var c = cas();
        return '<div class="card"><h2>The plan, part by part</h2>' +
            '<p class="lead">Every staged row is counted once. A refused row is named; it is never counted as written.</p>' +
            '<table class="grid famtable"><thead><tr><th>Part</th><th>Lands in</th><th>Staged</th><th>New</th><th>Replace</th><th>Same</th><th>Refused</th><th>Live now</th></tr></thead><tbody>' +
            plan().map(function (p) {
                var slow = p.skipped ? ' class="off"' : '';
                return '<tr class="click' + (S.state !== 'decide' ? '' : '') + '" data-act="family" data-key="' + esc(p.key) + '"' + slow + '>' +
                    '<td class="nm">' + esc(p.name) + '<span class="ent">' + esc(p.family ? 'refdata.' + p.ent : p.policy) + '</span></td>' +
                    '<td>' + (landsIn(p) === 'sandbox'
                        ? '<span class="pill info">sandbox</span><span class="sub">' + esc(target().name) + '</span>'
                        : '<span class="pill">party-wide</span><span class="sub">no sandbox column</span>') + '</td>' +
                    '<td class="n">' + n(p.staged) + '</td><td class="n">' + n(p.add) + '</td><td class="n">' + n(p.replace) + '</td>' +
                    '<td class="n">' + n(p.unchanged) + '</td><td class="n">' + (p.refused ? '<span class="pill bad">' + n(p.refused) + '</span>' : '0') + '</td>' +
                    '<td class="n">' + n(p.live) + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<div class="proto-hint">Every row names where it lands. Only the trades and portfolios family is a book-tree family; ' +
            'the convention, curve, market and run families carry no sandbox column in the model, so they publish to the party. ' +
            'The screen draws that boundary instead of hiding it.</div>' +
            (skippedFamilies().length ? '<div class="notice warn mt"><span class="title">One family is skipped, and the screen says so</span>' +
                'The file carries 6 FX conventions. The target has no FX conventions store, so the family is skipped with merge policy ' +
                '<span class="mono">skip</span>. Today the reply carries <span class="mono">fx_skipped</span> and no screen and no shell line prints it. ' +
                'A skipped family is reported here, not dropped.</div>' : '') +
            (c.duplicate ? '<div class="notice warn mt"><span class="title">This is a second run over the same sample</span>' +
                'Nothing matches an imported trade by its ORE id, so 288 trades would arrive again as new rows. ' +
                'Conventions replace by key, so the convention half is safe to repeat and the trade half is not. ' +
                'The journey says a seed must never double on a second run; this screen cannot promise that today.</div>' : '') +
            (S['case'] === 'party' ? '<div class="notice error mt"><span class="title">This actor cannot publish the trade part</span>' +
                'K. Mori holds <span class="mono">refdata::conventions:write</span> and not the trade write, so the staged conventions can commit ' +
                'and the trades cannot. The import refuses the whole commit rather than writing half. Today the same actor is refused by the ' +
                'owning service with a plain error, and no permission means "may import".</div>' : '') +
            '</div>' + rowDetail();
    }

    function rowDetail() {
        var open = S.family;
        if (!open) {
            return '<div class="card mt"><h2>Row detail</h2><p class="lead">Open a family to read the rows that differ.</p>' +
                '<div class="proto-hint">A preview must never apply a row as a side effect of reading it. Opening a family reads the staged rows only.</div></div>';
        }
        var p = plan().filter(function (x) { return x.key === open; })[0];
        if (!p) return '';
        if (!p.family) {
            return '<div class="card mt"><h2>' + esc(p.name) + '</h2>' +
                '<p class="lead">' + esc(p.unit) + ': ' + n(p.staged) + ' staged. ' + esc(p.note || p.policy) + '</p>' +
                '<div class="proto-hint">Only a conventions family is drawn row by row here. The trade preview compares each trade by its ORE id; ' +
                'today that id is not kept, so the screen cannot say "already here".</div></div>';
        }
        var rows = rowsFor(p.key);
        var refused = REFUSALS[p.key] || [];
        return '<div class="card mt"><div class="row between"><h2>' + esc(p.name) + ' conventions</h2>' +
            '<span class="pill info">' + n(p.staged) + ' staged \u00b7 ' + n(p.live) + ' live</span></div>' +
            '<p class="lead">The staged rows, against the live row with the same key. A matching key is a replacement, not an insert.</p>' +
            '<table class="grid"><thead><tr><th>Row key</th><th>Field</th><th>Live now</th><th>Incoming</th><th>Verdict</th></tr></thead><tbody>' +
            rows.map(function (r) {
                return '<tr><td class="mono">' + esc(r.row) + '</td><td>' + esc(r.field) + '</td>' +
                    '<td class="dim">' + esc(r.live) + '</td><td>' + esc(r.incoming) + '</td><td>' +
                    (r.verdict === 'replace' ? '<span class="pill warn">replace</span>' :
                        r.verdict === 'new' ? '<span class="pill info">new</span>' :
                            r.verdict === 'refused' ? '<span class="pill bad">refused</span>' :
                                '<span class="pill">unchanged</span>') + '</td></tr>';
            }).join('') + '</tbody></table>' +
            (refused.length ? '<div class="notice error mt"><span class="title">Refused, and named before the commit</span>' +
                refused.map(function (r) {
                    return '<span class="mono">' + esc(r.row) + '</span> \u00b7 ' + esc(r.field) + ': ' + esc(r.why) + '.';
                }).join('<br>') + '</div>' : '') +
            '<div class="proto-hint">Gap: the conventions save returns no per-family counts, no family name on failure and no list of rows written. ' +
            'The row comparison this screen draws has no read behind it today.</div>' +
            '</div>';
    }

    function rowsFor(key) {
        if (key === 'swap') {
            return [
                { row: 'EUR-6M-SWAP', field: 'fixed_leg_day_count', live: 'ACT/360', incoming: 'ACT/365F', verdict: 'replace' },
                { row: 'EUR-6M-SWAP', field: 'fixed_leg_frequency', live: 'Semiannual', incoming: 'Annual', verdict: 'replace' },
                { row: 'USD-3M-SWAP', field: 'all fields', live: '\u2014', incoming: 'new row', verdict: 'new' },
                { row: 'GBP-12M-SWAP', field: 'business_day_convention', live: 'MODFOLLOWING', incoming: 'MODFOLLOWINGX', verdict: 'refused' },
                { row: 'JPY-6M-SWAP', field: 'spot_lag', live: '2', incoming: '2', verdict: 'same' }
            ];
        }
        return [
            { row: key.toUpperCase() + '-1', field: 'first field', live: 'unchanged', incoming: 'unchanged', verdict: 'same' },
            { row: key.toUpperCase() + '-2', field: 'first changed field', live: 'live value', incoming: 'incoming value', verdict: 'replace' },
            { row: key.toUpperCase() + '-3', field: 'all fields', live: '\u2014', incoming: 'new row', verdict: 'new' }
        ];
    }

    function untouchedCard() {
        var c = cas();
        return '<div class="card mt"><h2>What this file does not touch</h2>' +
            '<p class="lead">Omission is not a delete. The parts the file leaves out keep their live rows and their versions.</p>' +
            (c.omitted.length ? '<table class="grid"><thead><tr><th>Part</th><th>Live rows now</th><th>After the commit</th></tr></thead><tbody>' +
                c.omitted.map(function (o) {
                    return '<tr><td>' + esc(o) + '</td><td class="n">' + (o === 'Curves' ? '612' : o === 'Trades' ? '16,420' : o === 'Conventions' ? '3,884' : '21,834') +
                        '</td><td class="n">' + (o === 'Curves' ? '612' : o === 'Trades' ? '16,420' : o === 'Conventions' ? '3,884' : '21,834') + '</td></tr>';
                }).join('') + '</tbody></table>' : '<p class="dim">This bundle carries every part the target holds.</p>') +
            (c.omittedWhy ? '<p class="hint">' + esc(c.omittedWhy) + '.</p>' : '') +
            (S['case'] === 'onboard' ? '<div class="notice success mb0"><span class="title">This is how the onboarding journey knows it worked</span>' +
                'The conventions are stored and the portfolio reads, and no market data, curve or pricing engine came with them. ' +
                'The screen proves the negative: the parts it did not touch are counted, not assumed.</div>' :
                '<div class="notice success mb0"><span class="title">The official tree is not a target</span>' +
                'The six official books and six official portfolios keep their rows, their ledger references and their versions. ' +
                'The publish writes only inside ' + esc(target().name) + ', and the rows it writes there are the rows the review counted.</div>') +
            '</div>';
    }

    function refusalCard() {
        var r = refusals();
        if (!r.length) return '';
        return '<div class="card mt"><h2>Refusals, before anything moves</h2>' +
            '<p class="lead">A refused row cannot be applied. The commit would refuse the file whole, so the person repairs the input first.</p>' +
            '<table class="grid"><thead><tr><th>Family</th><th>Row</th><th>Why</th></tr></thead><tbody>' +
            r.map(function (x) {
                return '<tr><td>' + esc(x.family) + '</td><td class="mono">' + esc(x.row) + '</td><td>' + esc(x.why) + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<div class="proto-hint">Today a refused row is discovered while writing it, and a later family\u2019s failure leaves the earlier ' +
            'families stored. This screen moves the discovery before the first write.</div></div>';
    }

    function decisionCard() {
        var makerId = maker().id;
        var partNames = plan().filter(function (p) { return p.family; }).length ? 'conventions' : 'trades';
        var dec = deciderFor(partNames);
        var self = dec.id === makerId;
        return '<div class="card"><h2>Publish or discard</h2>' +
            '<p class="lead">The staged rows, not the file, are the commitment. The file may change; these rows cannot.</p>' +
            '<div class="makerbox"><div class="row between"><span><span class="who">Staged by ' + esc(maker().name) + '</span>' +
            '<div class="what">' + esc(maker().part) + ' \u00b7 15:38Z</div></span>' +
            '<span class="pill info">maker</span></div></div>' +
            '<div class="makerbox mt"><div class="row between"><span><span class="who">Decided by ' + esc(dec.name) + '</span>' +
            '<div class="what">' + esc(dec.part) + ' \u00b7 ' + (self ? 'the maker cannot decide their own import' : 'read the plan, then publish or discard') + '</div></span>' +
            '<span class="pill ' + (self ? 'bad' : 'warn') + '">' + (self ? 'same person' : 'decider') + '</span></div></div>' +
            '<div class="notice warn mt"><span class="title">Who may act is part of the review, not a footnote</span>' +
            'The same subject is scoped to the session\u2019s party. A tenant administrator may switch party and stage it; a party-scoped user ' +
            'may not. A convention that every report reads draws a second person, which is why the decider is Market Risk and not the maker. ' +
            'Today neither an import permission nor a staged decision exists.</div>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="state" data-state="expired">Let it expire</button>' +
            '<span class="ml-auto"><button class="btn danger" data-act="state" data-state="declined">Discard</button> ' +
            '<button class="btn primary" data-act="state" data-state="decide">Send to ' + esc(dec.part) + '</button></span></div>' +
            '</div>';
    }

    function decideCard() {
        var dec = deciderFor(plan().filter(function (p) { return p.family; }).length ? 'conventions' : 'trades');
        var t = totals();
        var blocked = S.check === 'lookup' || S['case'] === 'party';
        var checks = [
            { nm: 'Every staged row is counted', ok: true, why: n(t.add) + ' new, ' + n(t.replace) + ' replace, ' + n(t.unchanged) + ' unchanged.' },
            { nm: 'No refused row is counted as written', ok: true, why: n(t.refused) + ' refused rows are named and held back.' },
            { nm: 'The file did not change after staging', ok: true, why: 'The commit reads the staged rows, not the source path.' },
            { nm: 'The session may write every part it staged', ok: S['case'] !== 'party', why: S['case'] === 'party' ? maker().name + ' holds the convention write but not the trade write, so the whole commit is refused.' : 'Each part names a write the session holds.' },
            { nm: 'Every named value still exists in its list', ok: S.check !== 'lookup', why: S.check === 'lookup' ? 'MODFOLLOWINGX left the business day convention list after the stage.' : 'Checked against the lists at the commit.' },
            { nm: 'The maker is not the decider', ok: dec.id !== maker().id, why: 'Staged by ' + maker().name + '; decided by ' + dec.name + '.' }
        ];
        return '<div class="card"><h2>Decide: ' + esc(dec.part) + '</h2>' +
            '<p class="lead">Read the checks, then publish the staged rows or send them back. A publish applies every family in one transaction.</p>' +
            '<div class="makerbox"><span class="who">' + esc(dec.name) + '</span><div class="what">' + esc(dec.part) + ' \u00b7 the decider</div></div>' +
            '<ul class="checklist mt">' + checks.map(function (c) {
                return '<li><span class="mk ' + (c.ok ? 'ok' : 'bad') + '">' + (c.ok ? '\u2713' : '\u2715') + '</span>' +
                    '<span><b>' + esc(c.nm) + '</b><span class="faint"> \u2014 ' + esc(c.why) + '</span></span></li>';
            }).join('') + '</ul>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="state" data-state="declined">Discard</button>' +
            '<span class="ml-auto"><button class="btn primary" data-act="state" data-state="applied"' + (blocked ? ' disabled' : '') + '>Publish</button></span></div>' +
            '<div class="proto-hint">A commit that fails leaves the staged rows and no live row. A failed family is named in the reply, which the ' +
            'design draft adds and today\u2019s save does not carry. The check that failed here is the reason the button is disabled.</div>' +
            '</div>';
    }

    function appliedState() {
        var t = totals();
        return '<div class="card"><div class="outcome"><div class="mark ok">\u2713</div><h2>Published into ' + esc(target().name) + '</h2>' +
            '<p>' + n(t.staged - t.refused) + ' rows written across ' + plan().length + ' parts, in one transaction, at 16:02Z. ' +
            'The book-tree rows landed in the sandbox and carry its id; the convention, curve, market and run rows published to ' + esc(PARTY) + '. ' +
            'The refused rows were held back; the import applied everything else as one unit.</p></div>' +
            '<div class="countgrid mt"><div class="c"><div class="k">Written</div><div class="v">' + n(t.staged - t.refused) + '</div></div>' +
            '<div class="c"><div class="k">Replaced</div><div class="v">' + n(t.replace) + '</div></div>' +
            '<div class="c"><div class="k">Refused</div><div class="v">' + n(t.refused) + '</div></div></div>' +
            '<div class="notice info mt"><span class="title">The staged rows are now the live rows</span>' +
            'The difference between staged and live is the audit: each written row carries the import id, the party and the actor. ' +
            'Open the run to read it per family.</div>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="state" data-state="review">Back to the plan</button>' +
            '<span class="ml-auto"><a class="btn primary" href="../one-import-run/index.html?case=' + esc(S['case']) +
            '&amp;target=' + esc(S.target) + '&state=applied">Open the run</a></span></div>' +
            '</div>';
    }

    function declinedState() {
        return '<div class="card"><div class="outcome"><div class="mark bad">\u2715</div><h2>Discarded</h2>' +
            '<p>The staged rows were removed and the live tables are exactly as they were. A discard never deletes a live row.</p></div>' +
            '<div class="notice success mt"><span class="title">Discard is not a rollback</span>' +
            'Nothing was written, so there is nothing to roll back. The import record keeps the attempt, its source and its refusal, so the ' +
            'person can say what was considered and not applied.</div>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="state" data-state="review">Back to the plan</button>' +
            '<span class="ml-auto"><a class="btn primary" href="../bring-data-in/index.html?case=' + esc(S['case']) + '">Start again</a></span></div>' +
            '</div>';
    }

    function expiredState() {
        return '<div class="card"><div class="outcome"><div class="mark bad">\u25cb</div><h2>Expired and swept</h2>' +
            '<p>The staged rows were removed by the idle sweep. No live row moved. The import record keeps the attempt so the person can see it.</p></div>' +
            '<div class="notice warn mt"><span class="title">A staged import that nobody decides does not wait forever</span>' +
            'The design draft gives the staging area an expiry and a sweep. Neither exists today, which is one reason no staged surface can be ' +
            'built without them: the rows would accumulate with nothing to remove them.</div>' +
            '<div class="stepfoot"><a class="btn ghost" href="../bring-data-in/index.html?case=' + esc(S['case']) + '">Start again</a>' +
            '<span class="ml-auto"><button class="btn primary" data-act="state" data-state="review">Reopen the attempt</button></span></div>' +
            '</div>';
    }

    /* ---------------------------------------------------------------- bar */

    function bar() {
        var out = '<span class="label">Trigger <b>' + esc(cas().title) + '</b></span>';
        CASE_ORDER.forEach(function (k) {
            out += '<button data-act="case" data-case="' + k + '"' + (k === S['case'] ? ' class="on"' : '') + '>' + esc(CASES[k].title) + '</button>';
        });
        out += '<span class="sep">|</span><span class="label">Maker</span>';
        MAKER_ORDER.forEach(function (k) {
            out += '<button data-act="maker" data-maker="' + k + '"' + (k === S.maker ? ' class="on"' : '') + '>' + esc(MAKERS[k].name) + '</button>';
        });
        out += '<span class="sep">|</span><span class="label">State</span>';
        ['review', 'decide', 'applied', 'declined', 'expired'].forEach(function (k) {
            out += '<button data-act="state" data-state="' + k + '"' + (k === S.state ? ' class="on"' : '') + '>' + k + '</button>';
        });
        out += '<span class="sep">|</span>';
        out += '<button data-act="check" data-check="' + (S.check === 'none' ? 'lookup' : 'none') + '">' +
            (S.check === 'none' ? 'Break a check' : 'Restore the check') + '</button>';
        return out;
    }

    /* -------------------------------------------------------------- events */

    document.addEventListener('click', function (e) {
        var t = e.target.closest('[data-act]');
        if (!t) return;
        var act = t.dataset.act;
        if (act === 'case') go({ 'case': t.dataset.case, state: 'review', family: null, check: 'none', target: CASE_TARGET[t.dataset.case] });
        else if (act === 'maker') go({ maker: t.dataset.maker });
        else if (act === 'state') go({ state: t.dataset.state });
        else if (act === 'family') go({ family: S.family === t.dataset.key ? null : t.dataset.key });
        else if (act === 'check') go({ check: t.dataset.check });
    });

    render();
}());
