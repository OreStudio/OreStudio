/* One import run: the record of one attempt, its per-family outcome and what
 * it left behind. Self-contained: plain JavaScript, mock data, no framework,
 * no build step.
 *
 * The argument: after the commit there is one question, and it is per family —
 * what was written, what was refused, what was skipped, and what a failure
 * left behind. Four journeys ask it (a failed family, a retry, a market feed's
 * sections, a reproduction whose third service failed); they ask it about the
 * same attempt record. So this is one screen in different states, not four.
 *
 * Query string:
 *   ?case=onboard|seed|defect|feed|one-dataset|promote|party
 *   ?state=applied|failed|partial|expired
 *   ?mode=proposed|today     atomic commit, or the per-family writes of today
 *   ?fail=<part key>         which family refuses
 *   ?attempt=1|2             first attempt, or the retry of it
 *
 * The screen is the import record the journeys say does not exist: today the
 * only record is a workflow instance and a service reply.
 */

(function () {
    'use strict';

    var TENANT = 'Acme Corporation';
    var PARTY = 'Acme Trading';
    var RUN_ID = 'imp_01J9Z6K4T3QW';

    var CASES = {
        onboard: { title: 'Onboard a party', source: 'acme-trading/onboarding/20261017', actor: 'J. Smith', parts: ['trades', 'conventions'], state: 'applied' },
        seed: { title: 'Seed a fresh environment', source: 'ore-samples/Example_1 \u2026 Example_3', actor: 'K. Mori', parts: ['trades', 'conventions', 'curves', 'market'], state: 'failed', batch: true },
        defect: { title: 'Reproduce a defect', source: 'reporting-2026-10-16.orebundle', actor: 'D. Osei', parts: ['trades', 'conventions', 'curves', 'market', 'run'], state: 'failed' },
        feed: { title: 'Load a vendor feed', source: 'vendor/2026-10-16/market.txt + fixings.txt', actor: 'J. Smith', parts: ['market'], state: 'partial' },
        'one-dataset': { title: 'Replace one dataset', source: 'fixes/swap_conventions_20261016.xml', actor: 'A. Tanaka', parts: ['conventions'], state: 'failed', only: 'swap' },
        promote: { title: 'Promote an environment', source: 'promote/dev-to-staging/20261016-1430', actor: 'K. Mori', parts: ['trades', 'conventions', 'curves', 'market', 'run'], state: 'failed', hop: 'development \u2192 staging' },
        party: { title: 'Import into my party', source: 'acme-trading/backfill/20261017', actor: 'K. Mori', parts: ['trades', 'conventions'], state: 'failed' }
    };

    var CASE_ORDER = ['onboard', 'seed', 'defect', 'feed', 'one-dataset', 'promote', 'party'];

    var FAMILIES = [
        { key: 'swap', name: 'Swap', rows: 214, live: 1891, refused: 2 },
        { key: 'ois', name: 'OIS', rows: 38, live: 402, refused: 0 },
        { key: 'deposit', name: 'Deposit', rows: 96, live: 733, refused: 1 },
        { key: 'fra', name: 'FRA', rows: 22, live: 168, refused: 0 },
        { key: 'future', name: 'Future', rows: 12, live: 94, refused: 0 },
        { key: 'bond_yield', name: 'Bond yield', rows: 18, live: 212, refused: 0 },
        { key: 'cds', name: 'CDS', rows: 9, live: 61, refused: 0 },
        { key: 'cross_currency_basis', name: 'Cross-currency basis', rows: 14, live: 187, refused: 0 },
        { key: 'inflation_swap', name: 'Inflation swap', rows: 7, live: 54, refused: 0 },
        { key: 'commodity_forward', name: 'Commodity forward', rows: 5, live: 37, refused: 0 },
        { key: 'average_ois', name: 'Average OIS', rows: 3, live: 18, refused: 0 },
        { key: 'fx', name: 'FX', rows: 6, live: 0, skipped: true, refused: 0 }
    ];

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
        return {
            'case': name,
            state: ['applied', 'failed', 'partial', 'expired'].indexOf(q.state) >= 0 ? q.state : CASES[name].state,
            mode: q.mode === 'today' ? 'today' : 'proposed',
            fail: q.fail || defaultFail(name),
            attempt: q.attempt === '2' ? 2 : 1
        };
    }

    function defaultFail(name) {
        return { onboard: 'swap', seed: 'market', defect: 'market', feed: 'fixings',
            'one-dataset': 'swap', promote: 'run', party: 'trades' }[name];
    }

    function go(patch) {
        Object.keys(patch).forEach(function (k) { S[k] = patch[k]; });
        var p = [];
        if (S['case'] !== 'onboard') p.push('case=' + S['case']);
        if (S.state !== CASES[S['case']].state) p.push('state=' + S.state);
        if (S.mode !== 'proposed') p.push('mode=' + S.mode);
        if (S.fail !== defaultFail(S['case'])) p.push('fail=' + S.fail);
        if (S.attempt !== 1) p.push('attempt=' + S.attempt);
        window.history.replaceState(null, '', window.location.pathname + (p.length ? '?' + p.join('&') : ''));
        render();
    }

    function esc(v) {
        return String(v === undefined || v === null ? '' : v)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
    }
    function n(v) { return Number(v).toLocaleString('en-GB'); }
    function cas() { return CASES[S['case']]; }

    function parts() {
        var out = [];
        if (cas().parts.indexOf('trades') >= 0) {
            out.push({ key: 'trades', name: 'Trades and portfolios', unit: 'trades', staged: S['case'] === 'seed' ? 288 : S['case'] === 'promote' || S['case'] === 'defect' ? 1642 : 64, refused: S['case'] === 'party' ? 64 : 1, live: 16420 });
        }
        if (cas().parts.indexOf('conventions') >= 0) {
            FAMILIES.forEach(function (f) {
                if (cas().only && f.key !== cas().only) return;
                out.push({ key: f.key, name: f.name, unit: 'rows', staged: f.rows, refused: f.skipped ? 0 : f.refused, live: f.live, family: true, skipped: f.skipped });
            });
        }
        if (cas().parts.indexOf('curves') >= 0) out.push({ key: 'curves', name: 'Curve configurations', unit: 'curves', staged: 84, refused: 0, live: 612 });
        if (cas().parts.indexOf('market') >= 0) {
            out.push({ key: S['case'] === 'feed' ? 'quotes' : 'market', name: S['case'] === 'feed' ? 'Quotes (market.txt)' : 'Quotes and fixings', unit: 'rows', staged: S['case'] === 'feed' ? 18442 : 18442, refused: 0, live: 218340 });
            if (S['case'] === 'feed') out.push({ key: 'fixings', name: 'Fixings (fixings.txt)', unit: 'rows', staged: 1204, refused: 12, live: 1190 });
        }
        if (cas().parts.indexOf('run') >= 0) out.push({ key: 'run', name: 'Run documents', unit: 'bindings', staged: 5, refused: 0, live: 47 });
        return out;
    }

    function failKey() { return S.fail; }

    /* The outcome of every part. The atomic mode writes every part or none; the
     * today mode writes each part in its own write and stops at the failure. */
    function outcome(p) {
        var idx = parts().map(function (x) { return x.key; }).indexOf(failKey());
        var myIdx = parts().map(function (x) { return x.key; }).indexOf(p.key);
        if (S.state === 'expired') return 'swept';
        if (p.skipped) return 'skipped';
        if (isRetry() || S.state === 'applied') return 'applied';
        if (p.key === failKey()) {
            if (S.state === 'partial') return 'partial';
            return p.key === 'trades' ? 'refused' : 'failed';
        }
        if (S.mode === 'proposed') return 'held back';
        if (myIdx < idx) return 'applied';
        return 'not run';
    }

    function written(p) {
        var o = outcome(p);
        if (o === 'applied' || o === 'partial') return p.staged - p.refused;
        return 0;
    }

    function totals() {
        var t = { staged: 0, written: 0, refused: 0, skipped: 0 };
        parts().forEach(function (p) {
            t.staged += p.staged; t.written += written(p);
            t.refused += outcome(p) === 'failed' ? p.refused + 1 : p.refused;
            if (outcome(p) === 'skipped') t.skipped++;
        });
        return t;
    }

    function isRetry() { return S.attempt === 2; }

    /* ------------------------------------------------------------- render */

    function render() {
        document.getElementById('proto-note').textContent =
            'Prototype \u00b7 Data exchange \u00b7 One import run \u00b7 mock data, no backend';
        document.getElementById('app').innerHTML = '<div class="page wide">' + head() + body() + '</div>';
        document.getElementById('proto-bar').innerHTML = bar();
    }

    function head() {
        var t = totals();
        return '<div class="brand">' + esc(TENANT) + '</div>' +
            '<div class="tenant">Party <b>' + esc(PARTY) + '</b> \u00b7 import <span class="mono">' + RUN_ID + '</span> \u00b7 attempt ' +
            S.attempt + ' \u00b7 source <b>' + esc(cas().source) + '</b></div>' +
            '<h1>' + esc(cas().title) + ': one import run</h1>' +
            '<p class="lede">The record of one attempt, per family: what was written, what was refused, what was skipped, and what a failure left behind. ' +
            'The retry runs against these staged rows, not against the file again.</p>' +
            gapbanner() + runStrip(t) + timeline();
    }

    function gapbanner() {
        return '<div class="gapbanner"><b>Gap</b>No import record exists today. The nearest thing is a workflow instance in the shell with a step log; ' +
            'the run import reports files, not families, and the conventions save names no failing family. The per-family outcome below is the record ' +
            'the journeys ask for, and it is where the mechanism\u2019s behaviour can be compared.</div>';
    }

    function runStrip(t) {
        var o = isRetry() || S.state === 'applied' ? (isRetry() ? 'applied on retry' : 'applied') : S.state === 'partial' ? 'partly applied' : S.state === 'expired' ? 'expired' : 'failed';
        return '<div class="strip">' +
            '<div class="cell"><div class="k">Outcome</div><div class="v">' + o + '</div><div class="s">' +
            (isRetry() ? 'the failed family ran again' : S.state === 'failed' ? 'refused at ' + esc(failKey()) : S.state === 'partial' ? 'some sections kept' : 'finished 16:02Z') + '</div></div>' +
            '<div class="cell"><div class="k">Staged</div><div class="v">' + n(t.staged) + '</div><div class="s">rows across ' + parts().length + ' parts</div></div>' +
            '<div class="cell"><div class="k">Written</div><div class="v">' + n(t.written) + '</div><div class="s">rows now live</div></div>' +
            '<div class="cell"><div class="k">Refused / skipped</div><div class="v">' + n(t.refused) + ' / ' + t.skipped + '</div><div class="s">named, not dropped</div></div>' +
            '</div>';
    }

    function timeline() {
        var rows = [
            ['15:38:04', 'Source received: ' + cas().source + ' (sha256 4b71\u2026c08d).'],
            ['15:38:40', 'Staged: ' + n(totals().staged) + ' rows in ' + parts().length + ' parts, under import ' + RUN_ID + '.'],
            ['15:40:12', 'Validated: every named value checked against its list; ' + n(totals().refused) + ' rows refused before the write.'],
            ['15:41:02', S.mode === 'proposed' ? 'Commit began: every part in one transaction.' : 'Commit began: each family in its own write, as the save does today.'],
            [S.state === 'failed' ? '15:41:12' : '15:41:19', S.state === 'failed' ? 'Commit refused at ' + failKey() + '.' : 'Commit completed.']
        ];
        return '<div class="card mt"><div class="row between"><h2>The attempt</h2><span class="pill info">' +
            (S.mode === 'proposed' ? 'proposed: one commit' : 'today: one write per family') + '</span></div>' +
            '<ul class="timeline">' + rows.map(function (r) {
                return '<li><span class="t">' + esc(r[0]) + '</span><span>' + esc(r[1]) + '</span></li>';
            }).join('') + '</ul></div>';
    }

    function body() {
        return (cas().batch ? batchCard() : '') + outcomeCard() + leftBehind() +
            (S.state === 'expired' ? expiredCard() : retryCard()) + whoCard();
    }

    function outcomeCard() {
        var rows = parts().map(function (p) {
            var o = outcome(p);
            return '<tr' + (o === 'failed' ? ' class="noright"' : '') + '>' +
                '<td class="nm">' + esc(p.name) + (p.family ? '<span class="ent">refdata.' + esc(p.key) + '</span>' : '') + '</td>' +
                '<td class="n">' + n(p.staged) + '</td>' +
                '<td class="n">' + n(p.refused) + '</td>' +
                '<td class="n">' + n(written(p)) + '</td>' +
                '<td>' + statePill(o) + '</td>' +
                '<td class="n">' + n(p.live) + '</td></tr>';
        }).join('');
        return '<div class="card mt"><div class="row between"><h2>Per-family outcome</h2>' +
            '<span class="hint">' + (isRetry() ? 'This is the retry. The failed family ran again; the applied families did not.' : 'Attempt ' + S.attempt) + '</span></div>' +
            '<p class="lead">A family is a row here. No family is missing, including the one that refused and the one that was skipped.</p>' +
            '<table class="grid famtable"><thead><tr><th>Family or part</th><th>Staged</th><th>Refused</th><th>Written</th><th>State</th><th>Live now</th></tr></thead><tbody>' +
            rows + '</tbody></table>' +
            skippedNote() + familyGap() +
            '</div>';
    }

    function statePill(o) {
        var cls = o === 'applied' ? 'ok' : o === 'failed' || o === 'refused' ? 'bad' : o === 'skipped' || o === 'swept' || o === 'partial' ? 'warn' : o === 'held back' ? 'info' : '';
        return '<span class="pill ' + cls + '">' + esc(o) + '</span>';
    }

    function skippedNote() {
        if (S['case'] !== 'onboard' && S['case'] !== 'seed' && S['case'] !== 'defect' && S['case'] !== 'promote') return '';
        return '<div class="notice warn mt"><span class="title">The skipped family is reported</span>' +
            'The file carries 6 FX conventions. The target has no FX conventions store, so the family is skipped with merge policy ' +
            '<span class="mono">skip</span>. Today <span class="mono">fx_skipped</span> is in the reply and no screen and no shell line prints it. ' +
            'A skipped family must never be silent.</div>';
    }

    function familyGap() {
        return '<div class="proto-hint">Gap: the conventions save returns no per-family counts, no family name on failure and no list of rows ' +
            'written. The design draft adds a per-family reply and names the failing family. This screen cannot be built without that reply.</div>';
    }

    /* What a failure left behind: the question the failing-family journey asks.
     * In the proposed mechanism the answer is nothing. In today's mechanism the
     * answer is every family written before the failure. */
    function leftBehind() {
        if (S.state === 'applied' || S.state === 'expired' || isRetry()) return '';
        var applied = parts().filter(function (p) { return outcome(p) === 'applied'; });
        var writtenRows = applied.reduce(function (a, p) { return a + written(p); }, 0);
        var later = parts().filter(function (p) { return ['held back', 'not run'].indexOf(outcome(p)) >= 0; });
        if (S.mode === 'proposed') {
            return '<div class="card mt"><h2>What the failed family left behind</h2>' +
                '<div class="notice success"><span class="title">Nothing. The commit is one transaction.</span>' +
                'The failure at <span class="mono">' + esc(failKey()) + '</span> rolled the whole commit back. The staged rows are still there, ' +
                'the live tables are unchanged, and the retry resumes from the same staged rows.</div>' +
                '<div class="countgrid"><div class="c"><div class="k">Live rows written</div><div class="v">0</div></div>' +
                '<div class="c"><div class="k">Staged rows kept</div><div class="v">' + n(totals().staged) + '</div></div>' +
                '<div class="c"><div class="k">Family named</div><div class="v mono" style="font-size:14px">' + esc(failKey()) + '</div></div></div>' +
                '<div class="proto-hint">The draft design makes the commit atomic and names the failing family. The journeys demand both. ' +
                'The owner has to decide whether the trade import keeps its deliberately partial behaviour.</div>' +
                '</div>';
        }
        return '<div class="card mt"><h2>What the failed family left behind, today</h2>' +
            '<div class="notice error"><span class="title">' + n(writtenRows) + ' rows are stored, and no rollback reaches them</span>' +
            'The save writes each family in its own write. The families before <span class="mono">' + esc(failKey()) + '</span> are stored; ' +
            'the families after it never ran. The conventions document is the one thing the run import\u2019s undo does not cover.</div>' +
            '<table class="grid"><thead><tr><th>Family</th><th>Rows written before the failure</th><th>State now</th></tr></thead><tbody>' +
            (applied.length ? applied.map(function (p) {
                return '<tr><td>' + esc(p.name) + '</td><td class="n">' + n(written(p)) + '</td><td><span class="pill bad">stored, not undone</span></td></tr>';
            }).join('') : '<tr><td colspan="3" class="dim">No family was written before the failure.</td></tr>') +
            '</tbody></table>' +
            '<div class="notice warn mt"><span class="title">The gap the journey makes visible</span>' +
            'There is no delete verb for a partial import. The person deletes rows one at a time, imports a corrected file over them, or asks for a ' +
            'database restore. A later retry must not be treated as if the earlier families had not been written.</div>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="mode" data-mode="proposed">Compare with the atomic commit</button>' +
            '<span class="ml-auto"><span class="hint">' + (later.length ? later.length + ' families are held back' : '') + '</span></span></div>' +
            '</div>';
    }

    function batchCard() {
        var samples = [
            { name: 'Example_1', root: 'Input/portfolio.xml', trades: 96, conv: 344, market: 18442, state: 'applied' },
            { name: 'Example_2', root: 'Input/portfolio.xml', trades: 96, conv: 344, market: 0, state: 'failed', why: 'market.txt holds a duplicate fixings row at 2026-10-15' },
            { name: 'Example_3', root: 'Input/portfolio.xml', trades: 96, conv: 344, market: 18442, state: 'applied' }
        ];
        return '<div class="card mt"><h2>The batch, sample by sample</h2>' +
            '<p class="lead">One root folder, three sample trees, three sub-runs. A failed sample must not hide the ones that landed.</p>' +
            '<table class="grid"><thead><tr><th>Sample</th><th>Trades</th><th>Conventions</th><th>Quotes</th><th>State</th><th></th></tr></thead><tbody>' +
            samples.map(function (s) {
                return '<tr><td class="mono">' + esc(s.name) + '</td><td class="n">' + n(s.trades) + '</td><td class="n">' + n(s.conv) +
                    '</td><td class="n">' + (s.market ? n(s.market) : '0') + '</td><td>' + statePill(s.state) +
                    '</td><td class="sub">' + esc(s.why || '') + '</td></tr>';
            }).join('') + '</tbody></table>' +
            '<div class="notice warn mt"><span class="title">A second run would double the trades</span>' +
            'Nothing matches an imported trade by its ORE id, so the 288 trades from the first seed would arrive again as new rows. ' +
            'The conventions replace by key and are safe to repeat. This is the seed journey\u2019s central gap, and it sits on this screen.</div>' +
            '</div>';
    }

    function retryCard() {
        var failedPart = parts().filter(function (p) { return p.key === failKey(); })[0];
        var retried = isRetry();
        return '<div class="card mt"><h2>Retry or abandon</h2>' +
            '<p class="lead">A retry resumes the same import from the family that refused. The families that already applied do not run again.</p>' +
            '<div class="makerbox"><div class="row between"><span><span class="who">' + esc(cas().actor) + '</span>' +
            '<div class="what">the importer \u00b7 signed in to ' + esc(PARTY) + '</div></span>' +
            '<span class="pill info">may retry</span></div></div>' +
            '<div class="notice ' + (retried ? 'success' : 'info') + ' mt"><span class="title">' +
            (retried ? 'This attempt is the retry' : 'The retry is the same staged import, not a new file') + '</span>' +
            (retried ? 'The failed family was staged again with the corrected value and the commit ran once. The applied families were not written twice.' :
                'The staged rows are durable, so the person presses retry instead of re-uploading. In the shell a re-run starts a new workflow instance; ' +
                'the file is still in the bucket, so the upload is skipped, but the import starts from the beginning.') + '</div>' +
            (failedPart && failedPart.key === 'trades' ? '<div class="notice warn mt"><span class="title">What a trade retry would do today</span>' +
                'The trade import keeps every trade that saved and reports the failures, so it is deliberately partial. A second run creates new trade ids: ' +
                'the total rises by the number that saved in both, and nothing warns. The counts must be compared by hand.</div>' : '') +
            '<table class="grid mt"><thead><tr><th>Attempt</th><th>Failed at</th><th>Trades saved</th><th>Convention rows written</th><th>Total trades after</th></tr></thead><tbody>' +
            '<tr><td>Attempt 1</td><td class="mono">' + esc(failKey()) + '</td><td class="n">' + n(S['case'] === 'seed' ? 192 : 0) + '</td>' +
            '<td class="n">' + n(S.mode === 'today' ? 344 : 0) + '</td><td class="n">' + n(S['case'] === 'seed' ? 16612 : 16420) + '</td></tr>' +
            '<tr><td>Attempt 2</td><td class="mono">' + (retried ? '\u2014' : 'not run') + '</td><td class="n">' + (retried ? n(S['case'] === 'seed' ? 288 : 0) : '\u2014') + '</td>' +
            '<td class="n">' + (retried ? '0 (replaced by key)' : '\u2014') + '</td><td class="n">' + (retried ? n(S['case'] === 'seed' ? 16900 : 16420) : '\u2014') + '</td></tr>' +
            '</tbody></table>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="state" data-state="expired">Abandon</button>' +
            '<span class="ml-auto"><button class="btn" data-act="attempt" data-attempt="1"' + (retried ? '' : ' disabled') + '>Read attempt 1</button> ' +
            '<button class="btn primary" data-act="attempt" data-attempt="2"' + (retried ? ' disabled' : '') + '>Retry the failed family</button></span></div>' +
            '<div class="proto-hint">Gap: no idempotency key on the trade import (request_id names the tarball, not the import), no trade identity check, ' +
            'no resume, no import history, and no post-failure summary. The retry control exists only on the workflow progress screen today.</div>' +
            '</div>';
    }

    function whoCard() {
        return '<div class="card mt"><h2>Who may read or act on this run</h2>' +
            '<div class="countgrid"><div class="c"><div class="k">Reads it</div><div class="v" style="font-size:14px">' + esc(cas().actor) + ', the decider</div>' +
            '<div class="k" style="margin-top:4px">same party only</div></div>' +
            '<div class="c"><div class="k">A party-scoped user</div><div class="v" style="font-size:14px">own party only</div><div class="k" style="margin-top:4px">another party is absent, not empty</div></div>' +
            '<div class="c"><div class="k">Tenant administrator</div><div class="v" style="font-size:14px">every party</div><div class="k" style="margin-top:4px">after switching party</div></div></div>' +
            '<div class="notice info mt mb0">The party is the scope of every row the import wrote. A refusal is never reported as an empty success, ' +
            'and another party\u2019s run is never shown. Today no import permission exists: each import borrows the write permission of every row it creates.</div>' +
            '</div>';
    }

    function expiredCard() {
        return '<div class="card mt"><h2>Expired and swept</h2>' +
            '<div class="notice warn"><span class="title">The attempt is kept; its staged rows are gone</span>' +
            'The import record survives so the person can say what was tried and what it refused. The staged rows were removed by the idle sweep, ' +
            'and no live row moved.</div></div>';
    }

    /* ---------------------------------------------------------------- bar */

    function bar() {
        var out = '<span class="label">Trigger <b>' + esc(cas().title) + '</b></span>';
        CASE_ORDER.forEach(function (k) {
            out += '<button data-act="case" data-case="' + k + '"' + (k === S['case'] ? ' class="on"' : '') + '>' + esc(CASES[k].title) + '</button>';
        });
        out += '<span class="sep">|</span><span class="label">State</span>';
        ['applied', 'failed', 'partial', 'expired'].forEach(function (k) {
            out += '<button data-act="state" data-state="' + k + '"' + (k === S.state ? ' class="on"' : '') + '>' + k + '</button>';
        });
        out += '<span class="sep">|</span><span class="label">Mechanism</span>';
        out += '<button data-act="mode" data-mode="proposed"' + (S.mode === 'proposed' ? ' class="on"' : '') + '>One commit</button>';
        out += '<button data-act="mode" data-mode="today"' + (S.mode === 'today' ? ' class="on"' : '') + '>Per-family writes</button>';
        if (S.attempt === 2) out += '<span class="sep">|</span><button data-act="attempt" data-attempt="1">Attempt 1</button>';
        return out;
    }

    /* -------------------------------------------------------------- events */

    document.addEventListener('click', function (e) {
        var t = e.target.closest('[data-act]');
        if (!t || t.disabled) return;
        var act = t.dataset.act;
        if (act === 'case') go({ 'case': t.dataset.case, state: CASES[t.dataset.case].state, fail: defaultFail(t.dataset.case), attempt: 1 });
        else if (act === 'state') go({ state: t.dataset.state });
        else if (act === 'mode') go({ mode: t.dataset.mode });
        else if (act === 'attempt') go({ attempt: Number(t.dataset.attempt) });
    });

    render();
}());
