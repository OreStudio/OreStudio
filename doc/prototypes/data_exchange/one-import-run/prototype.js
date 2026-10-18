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
 *   ?wf=staged|in_progress|waiting|failed|retrying|completed|warnings|compensated
 *   ?play=1                  simulate the run on a timer
 *   ?target=<sandbox id>     the sandbox the attempt targeted
 *
 * The screen is the import record the journeys say does not exist: today the
 * only record is a workflow instance and a service reply.
 *
 * The live region is driven by the workflow instance, and its states are the
 * engine's: in_progress, completed, failed, compensating and compensated on the
 * instance; pending, in_progress, completed, completed_with_warnings, failed and
 * compensated on a step. It is simulated: the page holds a script and a clock,
 * never a server.
 */

(function () {
    'use strict';

    var TENANT = 'Acme Corporation';
    var PARTY = 'Acme Trading';
    var RUN_ID = 'imp_01J9Z6K4T3QW';

    /* The proposed definition this import would run under. The engine holds no
     * such workflow today: the two import definitions it registers,
     * ore_import_workflow and run_configuration_import_workflow, declare one
     * step each. The step list below follows the design draft: one stage
     * message per family, then one atomic commit. */
    var WF_TYPE = 'data_exchange_import_workflow';
    var INSTANCE_ID = '7f3c2a1e-9b4d-4c6e-8a12-5d0e9f4b7c31';
    var RETRY_ID = '3b8e4d21-6a0f-4f7c-9c55-2e1d8a6b0f93';

    /* The budgets a definition states, from the workflow API. */
    var DATA_STEP = 900;
    var WRITE_STEP = 120;

    var WF_STATES = ['staged', 'in_progress', 'waiting', 'failed', 'retrying',
        'completed', 'warnings', 'compensated'];

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

    /* The sandbox the attempt targeted. A sandbox is anchored at an official
     * portfolio, owns a subtree, and is not a node of the official tree. Only
     * books and portfolios carry its id, so only those rows appear below. */
    var TARGETS = {
        'sb-import-repro': { name: 'Reproduce 2026-10-16', anchor: 'Global Trading \u203a Global Rates \u203a EUR Rates',
            purpose: 'import', status: 'open', owner: 'D. Osei',
            existing: ['EUR_SWAPS_SB_01', 'EUR_VOL_SB_02'], portfolios: 2 },
        'sb-import-onboard': { name: 'Onboard Acme 2026-10-17', anchor: 'Global Trading \u203a Global Rates',
            purpose: 'import', status: 'open', owner: 'J. Smith',
            existing: ['ACME_EUR_SWAPS_SB_01'], portfolios: 1 },
        'sb-sample-seed': { name: 'Seed samples 2026-10', anchor: 'Global Trading',
            purpose: 'sample', status: 'open', owner: 'K. Mori',
            existing: ['SAMPLE_EUR_SWAPS_SB_01', 'SAMPLE_USD_SWAPS_SB_02'], portfolios: 1 },
        'sb-import-feed': { name: 'Vendor feed 2026-10-17', anchor: 'Global Trading \u203a Global Rates \u203a USD Rates',
            purpose: 'import', status: 'open', owner: 'J. Smith',
            existing: ['USD_FEED_SB_01'], portfolios: 1 }
    };

    var CASE_TARGET = {
        onboard: 'sb-import-onboard', seed: 'sb-sample-seed', defect: 'sb-import-repro',
        feed: 'sb-import-feed', 'one-dataset': 'sb-import-onboard',
        promote: 'sb-import-repro', party: 'sb-import-onboard'
    };

    var CREATES = {
        onboard: { portfolios: 3, books: 4, sample: ['ACME_EUR_SWAPS_IMP_01'] },
        seed: { portfolios: 3, books: 12, sample: ['SAMPLE_EUR_SWAPS_IMP_01', 'SAMPLE_USD_SWAPS_IMP_02'] },
        defect: { portfolios: 47, books: 103, sample: ['EUR_SWAPS_IMP_01', 'EUR_VOL_IMP_02'] },
        promote: { portfolios: 47, books: 103, sample: ['EUR_SWAPS_IMP_01', 'EUR_VOL_IMP_02'] },
        party: { portfolios: 3, books: 4, sample: ['ACME_EUR_SWAPS_IMP_01'] },
        feed: { portfolios: 0, books: 0, sample: [] },
        'one-dataset': { portfolios: 0, books: 0, sample: [] }
    };

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
            attempt: q.attempt === '2' ? 2 : 1,
            wf: WF_STATES.indexOf(q.wf) >= 0 ? q.wf : null,
            play: q.play === '1',
            target: TARGETS[q.target] ? q.target : CASE_TARGET[name]
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
        if (S.wf) p.push('wf=' + S.wf);
        if (S.play) p.push('play=1');
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
    function target() { return TARGETS[S.target]; }

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

    /* -------------------------------------------------- the workflow run */

    /* The step list the definition declares. Built from the request alone, as
     * the engine builds it: the same list on every advance. */
    function wfSteps() {
        var c = cas();
        var out = [{
            kind: 'begin', name: 'begin_import', label: 'Begin the import',
            subject: 'refdata.v1.steps.begin_conventions_import', budget: WRITE_STEP
        }];
        if (S['case'] === 'feed') {
            ['quotes', 'fixings'].forEach(function (key) {
                out.push({
                    kind: 'stage', key: key, name: 'stage_' + key,
                    label: 'Stage ' + (key === 'quotes' ? 'quotes (market.txt)' : 'fixings (fixings.txt)'),
                    subject: 'marketdata.v1.steps.stage_market_' + key, budget: DATA_STEP
                });
            });
        } else {
            FAMILIES.forEach(function (f) {
                if (c.only && f.key !== c.only) return;
                out.push({
                    kind: 'stage', key: f.key, name: 'stage_' + f.key,
                    label: 'Stage ' + f.name,
                    subject: 'refdata.v1.steps.stage_conventions_' + f.key,
                    budget: DATA_STEP, skipped: f.skipped
                });
            });
        }
        if (c.parts.indexOf('trades') >= 0) out.push({
            kind: 'stage', key: 'trades', name: 'stage_trades',
            label: 'Stage trades and portfolios',
            subject: 'trading.v1.steps.stage_trades', budget: DATA_STEP
        });
        if (c.parts.indexOf('curves') >= 0) out.push({
            kind: 'stage', key: 'curves', name: 'stage_curves',
            label: 'Stage curve configurations',
            subject: 'refdata.v1.steps.stage_curve_configurations', budget: DATA_STEP
        });
        if (c.parts.indexOf('market') >= 0 && S['case'] !== 'feed') out.push({
            kind: 'stage', key: 'market', name: 'stage_market',
            label: 'Stage quotes and fixings',
            subject: 'marketdata.v1.steps.stage_market_data', budget: DATA_STEP
        });
        if (c.parts.indexOf('run') >= 0) out.push({
            kind: 'stage', key: 'run', name: 'stage_run',
            label: 'Stage run documents',
            subject: 'configure.v1.steps.stage_run_documents', budget: DATA_STEP
        });
        out.push({
            kind: 'commit', name: 'commit_import',
            label: 'Commit every family in one transaction',
            subject: 'refdata.v1.steps.commit_conventions_import', budget: DATA_STEP
        });
        out.push({
            kind: 'record', name: 'record_import_outcome',
            label: 'Record the outcome and tell the person',
            subject: 'refdata.v1.steps.record_import_outcome', budget: WRITE_STEP
        });
        return out;
    }

    /* Which state the region shows. The query string wins; otherwise the
     * outcome the rest of the screen shows decides, so an old link still
     * reads. */
    function wfKey() {
        if (S.wf) return S.wf;
        if (S.attempt === 2 || S.state === 'applied') return 'completed';
        if (S.state === 'partial') return 'warnings';
        return 'failed';
    }

    function stepRows(st) {
        var p = parts().filter(function (x) { return x.key === st.key; })[0];
        return p ? p.staged : 0;
    }

    /* What the commit's reply carries when it succeeds: every staged row but
     * the refused ones, and a skipped family contributes nothing. */
    function wfCommitted() {
        var t = { written: 0, refused: 0, fxSkipped: false };
        parts().forEach(function (p) {
            if (p.skipped) { t.fxSkipped = true; return; }
            t.written += p.staged - p.refused;
            t.refused += p.refused;
        });
        return t;
    }

    function mmss(seconds) {
        var m = Math.floor(seconds / 60), s = seconds % 60;
        return m + ' min ' + (s < 10 ? '0' : '') + s + ' s';
    }

    /* One frame of the simulation: the instance status, the state of each
     * declared step, and what the run has said so far. No state here is
     * invented: each name is the engine's own. */
    function wfFrame() {
        var key = wfKey(), steps = wfSteps();
        var states = steps.map(function () { return 'pending'; });
        var commitIdx = steps.length - 2;
        /* The step the screen's own failure names, so the rail and the
         * per-family table tell the same story. */
        var running = steps.map(function (s) { return s.key; }).indexOf(failKey());
        if (running < 1) running = Math.max(1, commitIdx - 1);
        var f = {
            key: key, steps: states, status: 'in_progress', running: -1,
            elapsed: 0, error: '', created: '15:41:12Z', lastEvent: '15:41:12Z'
        };

        function doneTo(n) { for (var i = 0; i < n; i++) states[i] = 'completed'; }

        if (key === 'staged') {
            f.status = 'none';
            f.created = null;
            f.lastEvent = null;
            return f;
        }
        if (key === 'in_progress') {
            doneTo(running);
            states[running] = 'in_progress';
            f.running = running;
            f.elapsed = 268;
            f.lastEvent = '15:45:40Z';
        } else if (key === 'waiting') {
            doneTo(running);
            states[running] = 'in_progress';
            f.running = running;
            f.elapsed = 942;
            f.lastEvent = '15:41:12Z';
        } else if (key === 'failed' || key === 'retrying') {
            doneTo(running);
            states[running] = 'in_progress';
            f.running = running;
            if (key === 'failed') {
                states[running] = 'failed';
                f.status = 'failed';
                f.elapsed = 942;
                f.lastEvent = '15:56:54Z';
                f.error = steps[running].name + ' did not report within ' + steps[running].budget +
                    ' s (data_step_timeout). Last heartbeat from ores.refdata.service at 15:41:02Z, ' +
                    'before the command was published.';
            } else {
                f.elapsed = 41;
                f.lastEvent = '15:58:10Z';
            }
        } else if (key === 'completed' || key === 'warnings') {
            doneTo(steps.length);
            if (key === 'warnings') states[commitIdx] = 'completed_with_warnings';
            f.status = 'completed';
            f.elapsed = 812;
            f.lastEvent = '15:54:44Z';
            f.running = -1;
        } else if (key === 'compensated') {
            doneTo(commitIdx);
            /* Only a step whose definition declares a compensation subject is
             * unwound. The stage steps declare one; the begin step declares
             * none, so it keeps its completed state. */
            for (var i = 1; i < commitIdx; i++) states[i] = 'compensated';
            states[commitIdx] = 'failed';
            f.status = 'compensated';
            f.elapsed = 604;
            f.running = commitIdx;
            f.lastEvent = '15:51:16Z';
            f.error = '';
        }
        return f;
    }

    /* ------------------------------------------------------------- render */

    function wfPill(state) {
        var cls = state === 'completed' ? 'ok' : state === 'failed' ? 'bad' :
            state === 'completed_with_warnings' || state === 'compensated' ? 'warn' :
                state === 'in_progress' ? 'info' : '';
        return '<span class="pill ' + cls + '">' + esc(state) + '</span>';
    }

    function wfStatus(f) {
        if (f.status === 'none') return '<span class="pill warn">no instance</span>';
        var cls = f.status === 'completed' ? 'ok' : f.status === 'failed' ? 'bad' :
            f.status === 'in_progress' ? 'info' : 'warn';
        return '<span class="pill ' + cls + '">' + esc(f.status) + '</span>';
    }

    function wfHead(f) {
        var created = f.status === 'none' ? 0 : f.steps.filter(function (s) {
            return s !== 'pending';
        }).length;
        var none = f.status === 'none';
        return '<div class="wfhead">' +
            '<span>Instance ' + (none ? '<b>none</b>' : '<b class="mono">' + INSTANCE_ID + '</b>') + '</span>' +
            '<span>Type <b class="mono">' + WF_TYPE + '</b></span>' +
            '<span>Target <b class="mono">data_exchange_import \u00b7 ' + RUN_ID + '</b></span>' +
            '<span>Sandbox <b class="mono">' + esc(S.target) + '</b></span>' +
            '<span>Steps <b>' + created + ' of ' + f.steps.length + '</b> created</span>' +
            '<span>Created <span class="mono">' + (f.created || '\u2014') + '</span></span>' +
            '<span>Last event <span class="mono">' + (f.lastEvent || '\u2014') + '</span></span>' +
            '<span>Policy <b>stop</b></span>' +
            '</div>';
    }

    function wfResult(st, state, f) {
        if (state === 'pending' || state === 'compensated' && st.kind !== 'stage') return '';
        if (st.kind === 'begin')
            return state === 'in_progress' ? 'opening the import' : 'import ' + RUN_ID + ' opened';
        if (st.kind === 'record')
            return state === 'completed' ? 'record written \u00b7 ' + RUN_ID :
                state === 'in_progress' ? 'writing the record' : '';
        if (st.kind === 'commit') {
            if (state === 'completed' || state === 'completed_with_warnings')
                return 'one transaction \u00b7 ' + n(wfCommitted().written) + ' rows live in ' + esc(target().name) +
                    (state === 'completed_with_warnings' ? ' \u00b7 warnings' : '');
            if (state === 'in_progress') return 'no family counts until it commits';
            if (state === 'failed') return 'refused at ' + esc(failKey()) + ' \u00b7 rolled back';
            return '';
        }
        if (state === 'completed') return n(stepRows(st)) + ' rows staged';
        if (state === 'completed_with_warnings') return n(stepRows(st)) + ' rows staged \u00b7 warnings';
        if (state === 'in_progress') return 'staging\u2026';
        if (state === 'failed') return 'no completion \u00b7 deadline expired';
        if (state === 'compensated') return 'staged rows discarded';
        return '';
    }

    function wfRail(f) {
        return '<div class="wfsteps">' +
            '<div class="wfstep wfcols"><span></span><span>Step</span><span>State</span>' +
            '<span>Deadline from dispatch</span><span>Result</span></div>' +
            wfSteps().map(function (st, i) {
            var state = f.steps[i];
            var cls = state === 'in_progress' ? ' run' : state === 'failed' ? ' bad' :
                state === 'compensated' ? ' comp' : '';
            var budget = st.budget === DATA_STEP ? '15 min' : '2 min';
            if (i === f.running && f.status !== 'compensated')
                budget = mmss(f.elapsed) + ' / ' + budget;
            return '<div class="wfstep' + cls + '">' +
                '<span class="ix">' + (i + 1) + '</span>' +
                '<span class="lbl">' + esc(st.label) +
                '<span class="nm">' + esc(st.name) + ' \u00b7 ' + esc(st.subject) + '</span></span>' +
                wfPill(state) +
                '<span class="bud">' + esc(budget) + '</span>' +
                '<span class="res">' + wfResult(st, state, f) + '</span>' +
                '</div>';
        }).join('') + '</div>';
    }

    function wfNote(f) {
        if (f.key === 'staged') {
            return '<div class="notice info mt"><span class="title">Staged and awaiting review. Nothing is live, and nothing is running.</span>' +
                'No workflow instance exists yet. The engine creates one only when ' +
                '<span class="mono">workflow.v1.ops.start_workflow</span> is published, so there is no progress to follow: ' +
                'the staged rows are the record, and the publish is what starts the run.</div>';
        }
        if (f.key === 'in_progress') {
            return '<div class="notice info mt"><span class="title">One step is in progress, and only one.</span>' +
                'The engine dispatches the next step when the last completion arrives, so the run is never working on two steps at once. ' +
                'A step reports once, at the end: there is no row-level progress inside it, and the panel cannot invent one.</div>';
        }
        if (f.key === 'waiting') {
            return '<div class="notice warn mt"><span class="title">No completion has arrived, and the budget is spent.</span>' +
                'The engine measures silence from the dispatch, not from the start of the run. A pass every 30 s fails a step whose command was ' +
                'published more than its budget ago; the pass is why the state can read ' +
                '<span class="mono">in_progress</span> a moment after the budget is gone.</div>';
        }
        if (f.key === 'failed') {
            return '<div class="notice error mt"><span class="title">The run stopped on its failed step.</span>' +
                esc(f.error) + '<br>Policy <span class="mono">stop</span> keeps every completed step: the staged rows are durable and the run\'s error stands. ' +
                '<span class="mono">workflow.v1.ops.retry_workflow_instance</span> may resume it, and that is legitimate only because the definition declared stop. ' +
                'A definition that declared compensate would unwind the completed steps and offer no retry.</div>';
        }
        if (f.key === 'retrying') {
            return '<div class="notice info mt"><span class="title">The retry resumes this instance, not a new one.</span>' +
                'The verb re-dispatches the failed step under its own identity, clears the run\'s error and keeps every completed step. ' +
                'The instance stays <span class="mono">' + INSTANCE_ID + '</span>; the shell re-run would instead start a new instance, for example ' +
                '<span class="mono">' + RETRY_ID + '</span>. The engine counts no attempt number, so "attempt 2" is this screen\'s label.</div>';
        }
        if (f.key === 'completed') {
            return '<div class="notice success mt"><span class="title">Completed, and completion is positional.</span>' +
                'Every declared step reached a terminal state, so the engine marks the instance complete with the last step\'s result. ' +
                'There is no completion hook: the last step is where an import says what it did.</div>';
        }
        if (f.key === 'warnings') {
            var c = wfCommitted();
            return '<div class="notice warn mt"><span class="title">Success with warnings is success, not failure.</span>' +
                'The commit reported <span class="mono">completed_with_warnings</span> with one log entry per item it could not place: ' +
                n(c.refused) + ' rows refused' + (c.fxSkipped ? ' and the FX family skipped (<span class="mono">fx_skipped</span>)' : '') +
                '. The engine treats it as success, ' +
                'dispatches the next step, and never compensates it. The run is ' +
                '<span class="mono">completed</span>: there is no partial instance state.</div>';
        }
        if (f.key === 'compensated') {
            return '<div class="notice warn mt"><span class="title">The definition declared compensate, so the run rolled back.</span>' +
                'The commit failed and the engine walked the completed forward steps in reverse, compensating those that declare a compensation ' +
                'subject: the staged rows are discarded. The begin step declares none, so it keeps its completed state. The run reaches ' +
                '<span class="mono">compensated</span> and both its result and its error are cleared. No live row moved, and there is no retry: ' +
                'only a run parked in <span class="mono">failed</span> by a stop policy can be resumed.</div>';
        }
        return '';
    }

    function wfFoot(f) {
        var act = '';
        if (f.key === 'failed') act = '<button class="btn primary" data-act="wf" data-wf="retrying">Retry from failed step</button>';
        if (f.key === 'staged') act = '<button class="btn primary" data-act="wf" data-wf="in_progress">Start the import</button>';
        var reads = f.key === 'staged'
            ? 'Nothing exists to read: the step list is the definition\'s, from <span class="mono">workflow.v1.definitions.list</span>, ' +
              'and the engine creates no instance until <span class="mono">workflow.v1.ops.start_workflow</span> is published.'
            : 'The region reads <span class="mono">workflow.v1.ops.get_workflow_steps</span> for one instance, and a watch on the ' +
              'workflow component and the instance entity says when to read it again. The notification carries no step detail, so the ' +
              'screen follows it with a read.';
        return '<div class="stepfoot"><span class="hint">' + reads + '</span>' +
            '<span class="ml-auto">' + act + '</span></div>' +
            '<div class="proto-hint">Mock: the states, clocks and counts are a script in this page. No engine runs, and the ' +
            'workflow type, its step subjects and its per-family replies do not exist yet. The state names and the read are the ' +
            'engine\'s own.</div>';
    }

    function wfPanel() {
        var f = wfFrame();
        return '<div class="card mt"><div class="row between"><h2>The workflow run</h2>' + wfStatus(f) + '</div>' +
            '<p class="lead">The import runs as one instance of a workflow definition. A family is staged by its own step and the ' +
            'commit is one step, so the rail is the progress the engine actually has.</p>' +
            wfHead(f) + wfRail(f) + wfNote(f) + wfFoot(f) + '</div>';
    }

    /* The sandbox the run fills. The tree changes only at the commit: a stage
     * step holds its rows in the import's own area, and the commit writes the
     * portfolios and virtual books under the sandbox. */
    function sandboxPanel() {
        var t = target(), made = CREATES[S['case']], key = wfKey();
        var regionDone = key === 'completed' || key === 'warnings';
        var live = regionDone && (S.state === 'applied' || isRetry());
        var unwound = key === 'compensated';
        var rows = '<div class="trow sandbox on"><span class="twisty leaf">\u25b8</span>' +
            '<span class="tname">' + esc(t.name) + '</span><span class="tag sb">sandbox</span>' +
            '<span class="tmeta">' + esc(t.purpose) + ' \u00b7 ' + esc(t.status) + '</span></div>';
        t.existing.forEach(function (b) {
            rows += '<div class="trow bk sandbox" style="padding-left:24px"><span class="twisty leaf">\u25b8</span>' +
                '<span class="tname">' + esc(b) + '</span>' +
                '<span class="tmeta">virtual \u00b7 no ledger ref \u00b7 not sweepable</span></div>';
        });
        if (live) {
            rows += '<div class="trow sandbox" style="padding-left:24px"><span class="twisty leaf">\u25b8</span>' +
                '<span class="tname">' + esc(RUN_ID) + '_root</span>' +
                '<span class="tag off">created \u00b7 root \u00b7 no parent</span>' +
                '<span class="tmeta">sandbox_id = ' + esc(S.target) + '</span></div>';
            made.sample.forEach(function (b) {
                rows += '<div class="trow bk sandbox" style="padding-left:40px"><span class="twisty leaf">\u25b8</span>' +
                    '<span class="tname">' + esc(b) + '</span>' +
                    '<span class="tag sb">new</span>' +
                    '<span class="tmeta">virtual \u00b7 sandbox_id = ' + esc(S.target) + ' \u00b7 no ledger ref \u00b7 not sweepable</span></div>';
            });
        }
        var created = live ? made.portfolios + ' / ' + made.books : '0 / 0';
        var note;
        if (key === 'staged') {
            note = 'No workflow instance exists, so the sandbox has no new row. The staged rows are not live; the tree is unchanged.';
        } else if (live) {
            note = 'The commit completed. ' + made.portfolios + ' virtual portfolios and ' + made.books +
                ' virtual books are now live under the sandbox, each written with its id. No official row moved.';
        } else if (unwound) {
            note = 'The run compensated. The staged rows were discarded, so the sandbox is exactly as it was. Nothing was created.';
        } else if (regionDone) {
            note = 'The workflow region reads completed, but the attempt record still reads ' + S.state + '. The tree follows the record, not the region.';
        } else {
            note = 'The commit has not completed, so no portfolio and no virtual book is live yet. The staged rows are held in the import\u2019s own area; ' +
                'only the commit writes them into the sandbox.';
        }
        return '<div class="card mt"><h2>The sandbox this run fills</h2>' +
            '<p class="lead">The attempt targeted <span class="sbpath"><b>' + esc(t.anchor + ' \u203a ' + t.name) + '</b></span>. ' +
            'The sandbox is anchored at an official portfolio; it is not a node of the official tree.</p>' +
            '<div class="strip">' +
            '<div class="cell"><div class="k">Sandbox</div><div class="v" style="font-size:14px">' + esc(t.name) + '</div><div class="s">' + esc(S.target) + '</div></div>' +
            '<div class="cell"><div class="k">Anchor</div><div class="v" style="font-size:13px">' + esc(t.anchor) + '</div><div class="s">fixed for its life</div></div>' +
            '<div class="cell"><div class="k">Created by this run</div><div class="v">' + created + '</div><div class="s">virtual portfolios / virtual books</div></div>' +
            '<div class="cell"><div class="k">Official rows changed</div><div class="v">0</div><div class="s">no official book or portfolio moves</div></div>' +
            '</div>' +
            '<div class="tree">' + rows + '</div>' +
            '<div class="notice ' + (live ? 'success' : unwound ? 'warn' : 'info') + ' mt"><span class="title">' +
            (live ? 'The imported tree is live under the sandbox' : unwound ? 'The compensated run left no row' : 'The tree fills at the commit, not before') +
            '</span>' + note + '</div>' +
            '<div class="proto-hint">A stage step creates no row in the tree. The commit is the step that makes the portfolios and virtual books live. ' +
            'Move the workflow state in the bar to see the tree fill. Mock: the rows are the case\u2019s; the invariants are the model\u2019s \u2014 ' +
            'a sandbox root portfolio has no parent, and every row above carries the sandbox id.</div>' +
            '</div>';
    }

    function render() {
        document.getElementById('proto-note').textContent =
            'Prototype \u00b7 Data exchange \u00b7 One import run \u00b7 mock data, no backend';
        document.getElementById('app').innerHTML = '<div class="page wide">' + head() + body() + '</div>';
        document.getElementById('proto-bar').innerHTML = bar();
        if (S.play) startPlay(); else stopPlay();
    }

    function head() {
        var t = totals();
        return '<div class="brand">' + esc(TENANT) + '</div>' +
            '<div class="tenant">Party <b>' + esc(PARTY) + '</b> \u00b7 import <span class="mono">' + RUN_ID + '</span> \u00b7 attempt ' +
            S.attempt + ' \u00b7 sandbox <b>' + esc(target().name) + '</b> \u00b7 source <b>' + esc(cas().source) + '</b></div>' +
            '<h1>' + esc(cas().title) + ': one import run</h1>' +
            '<p class="lede">The record of one attempt, per family: what was written, what was refused, what was skipped, and what a failure left behind. ' +
            'The retry runs against these staged rows, not against the file again.</p>' +
            gapbanner() + wfPanel() + sandboxPanel() + runStrip(t) + timeline();
    }

    function gapbanner() {
        return '<div class="gapbanner"><b>Gap</b>No import record exists today. The nearest thing is a workflow instance with a step log, and the region below ' +
            'draws the instance this import would run. The run import reports files, not families, and the conventions save names no failing family. ' +
            'The per-family outcome is the record the journeys ask for, and it is where the mechanism\u2019s behaviour can be compared.</div>';
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
            'no resume, no import history, and no post-failure summary. The retry control exists only on the workflow progress screen today, ' +
            'and this run screen is where it belongs: beside the record it resumes.</div>' +
            '</div>';
    }

    function whoCard() {
        return '<div class="card mt"><h2>Who may read or act on this run</h2>' +
            '<div class="countgrid"><div class="c"><div class="k">Reads it</div><div class="v" style="font-size:14px">' + esc(cas().actor) + ', the decider</div>' +
            '<div class="k" style="margin-top:4px">same party only</div></div>' +
            '<div class="c"><div class="k">A party-scoped user</div><div class="v" style="font-size:14px">own party only</div><div class="k" style="margin-top:4px">another party is absent, not empty</div></div>' +
            '<div class="c"><div class="k">Tenant administrator</div><div class="v" style="font-size:14px">every party</div><div class="k" style="margin-top:4px">after switching party</div></div></div>' +
            '<div class="notice info mt mb0">The party is the scope of every row the import wrote. A refusal is never reported as an empty success, ' +
            'and another party\u2019s run is never shown. A sandbox\u2019s portfolios and virtual books are read only by the account that may see the sandbox, ' +
            'which is why the tree on this screen can be empty for a reader who may not see it. Today no import permission exists: each import borrows ' +
            'the write permission of every row it creates.</div>' +
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
        out += '<span class="sep">|</span><span class="label">Workflow</span>';
        WF_STATES.forEach(function (k) {
            out += '<button data-act="wf" data-wf="' + k + '"' + (k === wfKey() ? ' class="on"' : '') + '>' + esc(k) + '</button>';
        });
        out += '<button data-act="play">' + (S.play ? 'Pause' : '\u25b6 Simulate') + '</button>';
        if (S.attempt === 2) out += '<span class="sep">|</span><button data-act="attempt" data-attempt="1">Attempt 1</button>';
        return out;
    }

    /* -------------------------------------------------------------- events */

    /* The simulation: the page walks the states the engine can reach, one
     * every 1.6 s, so a reviewer can watch the rail fill. It is a script and
     * a clock; nothing is read and nothing is written. */
    var WF_ORDER = ['staged', 'in_progress', 'waiting', 'failed', 'retrying',
        'completed', 'warnings', 'compensated'];
    var playTimer = null;

    function stopPlay() {
        if (playTimer) { clearInterval(playTimer); playTimer = null; }
    }

    function startPlay() {
        if (playTimer) return;
        playTimer = setInterval(function () {
            var i = WF_ORDER.indexOf(wfKey());
            S.wf = WF_ORDER[(i + 1) % WF_ORDER.length];
            render();
        }, 1600);
    }

    document.addEventListener('click', function (e) {
        var t = e.target.closest('[data-act]');
        if (!t || t.disabled) return;
        var act = t.dataset.act;
        if (act === 'case') go({ 'case': t.dataset.case, state: CASES[t.dataset.case].state, fail: defaultFail(t.dataset.case), attempt: 1, target: CASE_TARGET[t.dataset.case] });
        else if (act === 'state') go({ state: t.dataset.state });
        else if (act === 'mode') go({ mode: t.dataset.mode });
        else if (act === 'attempt') {
            var n = Number(t.dataset.attempt);
            go({ attempt: n, wf: n === 2 ? 'completed' : 'failed' });
        }
        else if (act === 'wf') { S.play = false; stopPlay(); go({ wf: t.dataset.wf }); }
        else if (act === 'play') {
            if (S.play) { S.play = false; stopPlay(); go({}); }
            else { S.play = true; go({ wf: 'staged' }); }
        }
    });

    render();
}());
