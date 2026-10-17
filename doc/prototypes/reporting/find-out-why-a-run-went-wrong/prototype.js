/* Reporting: find out why a run went wrong. Throwaway prototype:
 * self-contained, plain JavaScript, mock data, no framework, no build step,
 * and nothing that outlives the page.
 *
 * See doc/knowledge/journeys/reporting/journey_find_out_why_a_run_went_wrong.org.
 *
 * The question this page exists for: are a run's errors a place you go, a
 * property of the run you are already looking at, or a ledger across the
 * party's runs? Three answers, switched by ?variant=A|B|C:
 *
 *   A  one tab of the run, beside the outputs and the timeline. The count is
 *      on the tab. You go there deliberately.
 *   B  a panel beside the run. The run stays visible, the rows are filterable
 *      by scope and severity, and a failure is read against its run.
 *   C  a ledger across runs. The screen is the rows themselves, grouped by
 *      scope and then by subject, with the run as a column.
 *
 * Every row is a fixture. The twenty-one rows of the first run are copied from
 * a real run of SA-CCR Exposure whose market data had been removed; the other
 * two runs of the same report are fixtures too. Nothing here reads a server:
 * no operation and no BFF route serves a run's errors to a browser today, and
 * no reporting screen exists in the application.
 *
 * The error model is the spine of the page and every variant obeys it: a row
 * carries severity, scope_code, message and a subject with a subject_id, or it
 * names no subject at all; the subject is a discriminator plus a value and is
 * treated as a value, never as a link; a row carries event_time and no actor;
 * and no row is ever merged with another. Two rows of one trade with one
 * message are told apart by detail alone, and that difference is drawn. */

(function () {
    'use strict';

    /* The scope vocabulary, in the order the engine's words read best: the
       curve a trade needed, the trade, the analytic, the input, the run. The
       membership is exactly these five and a row never invents a sixth. */
    var ORDER = ['Curve', 'Trade', 'Analytics', 'Configuration', 'Report'];

    var SCOPE_NOTE = {
        Curve: 'the curve the engine named',
        Trade: 'the trade the engine named',
        Analytics: 'an analytic, or the run\u2019s summary of the analytics',
        Configuration: 'an input the run used defaults for',
        Report: 'the run itself, when no engine saw it'
    };

    var MSG_CURVE = 'yield curve building failed for curve(s) Yield/GBP/GBP1D on date 2026-10-15 while processing Yield/GBP/GBP1D';
    var MSG_TRADE = 'Cannot build all required curves! Building failed for: ([IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)])';
    var MSG_CONFIG = 'Input configuration was not provided. The default values will be used for all netting sets in the portfolio';
    var MSG_REPORT = 'Report run failed before the engine started; the run wrote no step output';

    /* A curve id is free text the engine writes. A trade id is a UUID. The two
       kinds are why the subject is a discriminator and a value, not a key. */
    var CURVE_ID = '[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]';

    var TRADE_IDS = [
        '6f0f2b91-3c47-4a2e-9b90-1d5e7a4c8f21',
        '8a3d51c0-7e12-4f8b-a1c4-2b6e9d0f3a75',
        '0c94be27-5d31-4e6a-8f92-3a7b1c6d5e48',
        '42e1a7f3-9b58-4c0d-b673-8e2f5a9c1d04',
        'b7d30c18-2a94-4e57-9c0b-6f1d8a2e4c93',
        '1e5b8d40-6c73-4a29-b5f8-0d4c7e1a9b62',
        'd0a6f492-8e15-4b3c-a804-5c9d2e7f1b38',
        '3f8c1a06-4d92-4e7b-86f1-9b2c5d0e4a17'
    ];

    /* ------------------------------------------------------------------ data */

    /* One error row. A row names a subject in full or names none: a subject
       without an id, or an id without a subject, is not a row this model
       admits, so the builder marks it and the screen would say so out loud. */
    function row(severity, scope, subject, subjectId, message, detail) {
        var named = subject !== undefined && subject !== null;
        var identified = subjectId !== undefined && subjectId !== null;
        var error = null;
        if (ORDER.indexOf(scope) < 0) error = 'unknown scope ' + scope;
        else if (severity !== 'error' && severity !== 'warning') error = 'unknown severity ' + severity;
        else if (named !== identified) error = 'half a subject';
        else if (named && identified && (subject === '' || subjectId === '')) error = 'empty subject';
        return {
            severity: severity,
            scope_code: scope,
            subject: named ? subject : undefined,
            subject_id: identified ? subjectId : undefined,
            message: message,
            detail: detail === undefined ? undefined : detail,
            violation: error
        };
    }

    function tradeDetail(context) {
        return '{"exceptionType": "Error building trade for context \'' + context + '\'"}';
    }

    /* The run whose market data had been removed: one curve, sixteen trades
       across eight trades and two analytics, two analytic errors, one analytic
       warning, one configuration warning. The two rows per trade carry the
       same scope, subject, id and message, and differ only in detail. */
    function dirtyRows() {
        var rows = [row('error', 'Curve', 'curve', CURVE_ID, MSG_CURVE)];

        TRADE_IDS.forEach(function (id) {
            rows.push(row('error', 'Trade', 'trade', id, MSG_TRADE, tradeDetail('analytic/PRICING')));
            rows.push(row('error', 'Trade', 'trade', id, MSG_TRADE, tradeDetail('analytic/SA_CCR')));
        });

        rows.push(row('error', 'Analytics', 'analytic', 'PRICING',
            'Portfolio does not contain any built trades, context is \'analytic/PRICING\''));
        rows.push(row('error', 'Analytics', 'analytic', 'SA_CCR',
            'Portfolio does not contain any built trades, context is \'analytic/SA_CCR\''));
        rows.push(row('warning', 'Analytics', 'analytic', 'OREApp::run()',
            'Error in ORE analytics: Failed to run analytics PRICING,SA_CCR'));
        rows.push(row('warning', 'Configuration', 'configuration', 'Counterparty', MSG_CONFIG));

        return rows;
    }

    /* The clean shape: eleven rows, eight analytic errors and three
       configuration warnings. No row names a trade or a curve, and the run
       finished. "Carrying rows" is not "failed", which is the point of drawing
       the two runs in the same surface. */
    function cleanRows() {
        var rows = ['SA_CVA', 'SA_FVA', 'XVA', 'DIM', 'SENSITIVITY', 'STRESS', 'VAR', 'PFE']
            .map(function (analytic) {
                return row('error', 'Analytics', 'analytic', analytic,
                    'Error in ORE analytics: Failed to run analytics ' + analytic);
            });

        ['Counterparty', 'NettingSet', 'Simulation'].forEach(function (input) {
            rows.push(row('warning', 'Configuration', 'configuration', input, MSG_CONFIG));
        });

        return rows;
    }

    /* A run that failed before any engine saw it: one Report row and no
       subject. The row names the run itself. */
    function failedRows() {
        return [row('error', 'Report', undefined, undefined, MSG_REPORT)];
    }

    var RUNS = [
        {
            id: 'r1',
            definition: 'SA-CCR Exposure',
            definitionVersion: 'definition v7',
            party: 'ACME',
            label: '2026-10-15 18:00',
            state: 'completed',
            requestedBy: 'Priya Raman',
            requestedAt: '2026-10-15 18:00:02',
            completedAt: '2026-10-15 18:04:41',
            collectedAt: '2026-10-15 18:04:44',
            runId: '9a6b0f27-93d4-4d2a-b08e-5c1f7e3a6d21',
            shapeNote: 'the report ran, and its analytics built nothing',
            rows: dirtyRows()
        },
        {
            id: 'r2',
            definition: 'SA-CCR Exposure',
            definitionVersion: 'definition v7',
            party: 'ACME',
            label: '2026-10-14 18:00',
            state: 'completed',
            requestedBy: 'Priya Raman',
            requestedAt: '2026-10-14 18:00:03',
            completedAt: '2026-10-14 18:03:58',
            collectedAt: '2026-10-14 18:04:01',
            runId: '1c4d77e2-5b90-4a13-8f26-d0e83a51b7c4',
            shapeNote: 'eleven rows, and not one of them names a trade or a curve',
            rows: cleanRows()
        },
        {
            id: 'r3',
            definition: 'SA-CCR Exposure',
            definitionVersion: 'definition v7',
            party: 'ACME',
            label: '2026-10-16 18:00',
            state: 'failed',
            requestedBy: 'Daniel Okafor',
            requestedAt: '2026-10-16 18:00:01',
            completedAt: '2026-10-16 18:00:26',
            collectedAt: '2026-10-16 18:00:29',
            runId: 'b52e0d18-7c34-4f9a-a6e1-3d8f2c47b9e0',
            shapeNote: 'one row, and no engine saw the run',
            rows: failedRows()
        }
    ];

    RUNS.forEach(function (run) {
        run.rows.forEach(function (r) { r.run = run; });
        annotateDuplicates(run.rows);
    });

    /* A run's rows are never de-duplicated. Where the run wrote the same scope,
       subject, id and message twice, each copy keeps its place and carries
       1/2 and 2/2 so the reader can see that the run recorded two rows. */
    function duplicateKey(r) {
        return [
            r.scope_code,
            r.subject === undefined ? '\u2205' : r.subject,
            r.subject_id === undefined ? '\u2205' : r.subject_id,
            r.message
        ].join('\u0001');
    }

    function annotateDuplicates(rows) {
        var total = {};
        var seen = {};
        rows.forEach(function (r) {
            var key = duplicateKey(r);
            total[key] = (total[key] || 0) + 1;
        });
        rows.forEach(function (r) {
            var key = duplicateKey(r);
            seen[key] = (seen[key] || 0) + 1;
            r.groupSize = total[key];
            r.groupIndex = seen[key];
        });
    }

    /* ----------------------------------------------------------------- state */

    var S = { variant: 'A', run: 'r1', scope: 'all', sev: 'all', subject: '', tab: 'errors' };

    var VARIANTS = [
        { id: 'A', name: 'A \u00b7 Tab on the run', gist: 'The errors are one tab of the run, beside the outputs and the timeline. The count is on the tab.' },
        { id: 'B', name: 'B \u00b7 Panel beside the run', gist: 'The run stays visible; the rows sit in a panel beside it, filtered by scope and severity.' },
        { id: 'C', name: 'C \u00b7 Ledger across runs', gist: 'The screen is the rows, all runs together, grouped by scope then subject, the run a column.' }
    ];

    function variantByName(id) {
        var found = VARIANTS.filter(function (v) { return v.id === id; })[0];
        return found || VARIANTS[0];
    }

    function runById(id) {
        var found = RUNS.filter(function (r) { return r.id === id; })[0];
        return found || RUNS[0];
    }

    /* --------------------------------------------------------------- helpers */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function shortId(value) {
        if (value === undefined) return '';
        if (/^[0-9a-f]{8}-[0-9a-f]{4}-/.test(value)) return value.slice(0, 8) + '\u2026' + value.slice(-4);
        return value;
    }

    function sevTag(severity) {
        return '<span class="tag ' + (severity === 'error' ? 'bad' : 'warn') + '">' + esc(severity) + '</span>';
    }

    function stateTag(state) {
        return '<span class="tag ' + (state === 'failed' ? 'bad' : 'ok') + '">' + esc(state) + '</span>';
    }

    /* The detail is the only field that separates two otherwise identical
       rows, so it is drawn, not folded away: the analytic context first, then
       the engine's own detail string verbatim. */
    function contextOf(detail) {
        if (!detail) return undefined;
        try {
            var parsed = JSON.parse(detail);
            if (!parsed || typeof parsed.exceptionType !== 'string') return undefined;
            var found = /context '([^']*)'/.exec(parsed.exceptionType);
            return found ? found[1] : parsed.exceptionType;
        } catch (err) {
            return undefined;
        }
    }

    function contextTag(r) {
        var context = contextOf(r.detail);
        return context ? '<span class="tag context">' + esc(context) + '</span>' : '';
    }

    function detailBlock(r) {
        if (!r.detail) return '<span class="faint mono">\u2014</span>';
        return '<span class="raw mono">' + esc(r.detail) + '</span>';
    }

    function violationTag(r) {
        return r.violation ? ' <span class="tag bad">row violates the model: ' + esc(r.violation) + '</span>' : '';
    }

    /* The subject is a label and a value to copy and filter by. It is not a
       link, because a trade id from the engine resolves to no row the platform
       holds. Pressing it filters the rows that name the same value. */
    function subjectCell(r) {
        if (r.subject === undefined) {
            return '<span class="tag muted">no subject</span><span class="subnote">the run itself</span>';
        }
        return '<span class="dsc">' + esc(r.subject) + '</span>' +
            '<button class="subjlink" data-act="subject" data-subject="' + esc(r.subject_id) + '" ' +
            'title="' + esc(r.subject_id) + ' \u2014 show the rows that name it">' +
            esc(shortId(r.subject_id)) + '</button>';
    }

    function pairMark(r) {
        if (r.groupSize < 2) return '<span class="faint">\u2014</span>';
        return '<span class="pair">' + r.groupIndex + '/' + r.groupSize + '</span>';
    }

    function rowsLabel(n) {
        return n + (n === 1 ? ' row' : ' rows');
    }

    function countBy(items, predicate) {
        return items.filter(predicate).length;
    }

    function scopeCount(rows, scope) {
        return countBy(rows, function (r) { return r.scope_code === scope; });
    }

    function subjectKey(r) {
        return (r.subject === undefined ? '\u2205' : r.subject) + '\u0001' +
            (r.subject_id === undefined ? '\u2205' : r.subject_id);
    }

    function matches(row, filters) {
        if (filters.scope && filters.scope !== 'all' && row.scope_code !== filters.scope) return false;
        if (filters.sev && filters.sev !== 'all' && row.severity !== filters.sev) return false;
        if (filters.subject) {
            var bySubject = row.subject_id === filters.subject;
            var byMessage = row.message && row.message.indexOf(filters.subject) >= 0;
            if (!bySubject && !byMessage) return false;
        }
        return true;
    }

    function filters() {
        return { scope: S.scope, sev: S.sev, subject: S.subject };
    }

    function visibleRows(rows) {
        return rows.filter(function (r) { return matches(r, filters()); });
    }

    function severityCounts(rows) {
        return {
            error: countBy(rows, function (r) { return r.severity === 'error'; }),
            warning: countBy(rows, function (r) { return r.severity === 'warning'; })
        };
    }

    function shapeLine(rows) {
        var parts = ORDER.map(function (scope) {
            var n = scopeCount(rows, scope);
            return n === 0 ? '' : n + ' ' + scope.toLowerCase();
        }).filter(function (part) { return part !== ''; });
        return rows.length + (rows.length === 1 ? ' row' : ' rows') + ' \u00b7 ' + parts.join(' \u00b7 ');
    }

    /* ------------------------------------------------------------ shared page */

    function appHeader() {
        return '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="../index.org">Reporting prototypes</a>' +
            '<span class="here" style="padding:6px 10px;border-radius:0.5rem;background:var(--panel-2);color:var(--ink);font-size:13px">Runs</span></nav>' +
            '<span class="modechip">Party ACME \u00b7 market risk</span>' +
            '</div></header>';
    }

    function fixtureNote() {
        return '<div class="notice warn">PROTOTYPE \u00b7 EVERY ROW HERE IS A FIXTURE. The 2026-10-15 rows ' +
            'are copied from a real run of SA-CCR Exposure whose market data had been removed; the ' +
            '2026-10-14 and 2026-10-16 runs are fixtures too. Nothing reads a server: no operation and ' +
            'no route serves a run\u2019s errors to a browser, and the application has no reporting screen ' +
            'today. Errors are immutable and read as a group: nothing here is created, edited or ' +
            'acknowledged.</div>';
    }

    function runStrip(activeId, stacked) {
        var chips = RUNS.map(function (run) {
            return '<button class="runchip' + (run.id === activeId ? ' on' : '') + '" ' +
                'data-act="run" data-run="' + run.id + '">' +
                '<span class="rlbl">' + esc(run.definition) + '</span>' +
                '<span class="rdate">' + esc(run.label) + '</span>' +
                '<span class="rshape">' + esc(shapeLine(run.rows)) + '</span>' +
                '</button>';
        }).join('');
        return '<div class="runstrip' + (stacked ? ' runstrip-col' : '') + '">' + chips + '</div>';
    }

    function runFacts(run) {
        return '<div class="facts">' +
            fact('Definition', '<span>' + esc(run.definition) + '</span><span class="faint">' + esc(run.definitionVersion) + '</span>') +
            fact('Party', esc(run.party)) +
            fact('Requested by', esc(run.requestedBy) + '<span class="faint">' + esc(run.requestedAt) + '</span>') +
            fact('Finished', stateTag(run.state)) +
            fact('Completed', '<span class="mono">' + esc(run.completedAt) + '</span>') +
            fact('Errors collected', '<span class="mono">' + esc(run.collectedAt) + '</span>') +
            fact('Run id', '<span class="mono" title="' + esc(run.runId) + '">' + esc(shortId(run.runId)) + '</span>') +
            '</div>';
    }

    function fact(label, valueHtml) {
        return '<div class="fact"><span class="lbl">' + esc(label) + '</span>' +
            '<span class="val">' + valueHtml + '</span></div>';
    }

    function summaryStrip(rows) {
        var counts = severityCounts(rows);
        return '<div class="summary">' +
            '<span class="countchip">' + sevTag('error') + '<b>' + counts.error + '</b></span>' +
            '<span class="countchip">' + sevTag('warning') + '<b>' + counts.warning + '</b></span>' +
            ORDER.map(function (scope) {
                return '<span class="countchip"><span class="scope">' + esc(scope) + '</span><b>' +
                    scopeCount(rows, scope) + '</b></span>';
            }).join('') +
            '</div>';
    }

    function scopeFilter(rows) {
        return '<div class="fgroup"><span class="flbl">Scope</span>' +
            '<button class="chipbtn' + (S.scope === 'all' ? ' on' : '') + '" data-act="scope" data-scope="all">' +
            'All<span class="n">' + rows.length + '</span></button>' +
            ORDER.map(function (scope) {
                return '<button class="chipbtn' + (S.scope === scope ? ' on' : '') + '" data-act="scope" data-scope="' +
                    esc(scope) + '">' + esc(scope) + '<span class="n">' + scopeCount(rows, scope) + '</span></button>';
            }).join('') +
            '</div>';
    }

    function severityFilter(rows) {
        var counts = severityCounts(rows);
        return '<div class="fgroup"><span class="flbl">Severity</span>' +
            '<button class="chipbtn' + (S.sev === 'all' ? ' on' : '') + '" data-act="sev" data-sev="all">' +
            'All<span class="n">' + rows.length + '</span></button>' +
            '<button class="chipbtn' + (S.sev === 'error' ? ' on' : '') + '" data-act="sev" data-sev="error">' +
            'error<span class="n">' + counts.error + '</span></button>' +
            '<button class="chipbtn' + (S.sev === 'warning' ? ' on' : '') + '" data-act="sev" data-sev="warning">' +
            'warning<span class="n">' + counts.warning + '</span></button>' +
            '</div>';
    }

    function subjectFilter() {
        if (!S.subject) return '';
        return '<div class="fgroup"><span class="flbl">Subject</span>' +
            '<button class="chipbtn on subject" data-act="clear-subject">naming ' +
            '<span class="mono">' + esc(shortId(S.subject)) + '</span> \u00d7</button></div>';
    }

    function noRowNotice() {
        return '<div class="notice">No row matches this filter. The vocabulary is still five scopes ' +
            'and two severities; this run names none of them under the filter you chose.</div>';
    }

    function shapeBreakdown(rows) {
        return rowsLabel(rows.length) + ': ' +
            scopeCount(rows, 'Curve') + ' Curve, ' + scopeCount(rows, 'Trade') + ' Trade, ' +
            scopeCount(rows, 'Analytics') + ' Analytics, ' + scopeCount(rows, 'Configuration') +
            ' Configuration, ' + scopeCount(rows, 'Report') + ' Report';
    }

    function accountingNote(rows) {
        return '<p class="rowfoot">This run holds ' + shapeBreakdown(rows) +
            '. The journey counts twenty for it (one Curve, sixteen Trade, two Analytics errors, ' +
            'one Analytics warning); this fixture also carries the Configuration warning the brief lists, ' +
            'so the index holds one more. No row was merged, collapsed or dropped.</p>';
    }

    function accountingNoteLedger(runs, rows) {
        var perRun = runs.map(function (run) {
            return run.label + ' holds ' + shapeBreakdown(run.rows);
        }).join('; ');
        return '<p class="rowfoot">The ledger holds ' + shapeBreakdown(rows) +
            (runs.length > 1
                ? ' across ' + runs.length + ' runs: '
                : ' from one run: ') + perRun + '. The journey counts twenty for the 2026-10-15 run ' +
            '(one Curve, sixteen Trade, two Analytics errors, one Analytics warning); this fixture also ' +
            'carries the Configuration warning the brief lists, so that run holds one more. No row was ' +
            'merged, collapsed or dropped.</p>';
    }

    /* -------------------------------------------------------- variant A: tab */

    function renderA() {
        var run = runById(S.run);
        var rows = run.rows;
        var shown = visibleRows(rows);

        var body = pageHead(run) + fixtureNote() +
            '<section class="card">' +
            '<header class="sectionhead"><h2>The run</h2>' +
            '<div class="meta"><span>' + esc(run.party) + '</span><span>' + esc(run.label) + '</span>' +
            stateTag(run.state) + '</div></header>' +
            runStrip(S.run, false) +
            factsGrid(run) +
            tabBar(run, rows) +
            (S.tab === 'errors' ? errorsTab(run, rows, shown) : tabStub(S.tab)) +
            '</section>';

        mount(body, '');
        setNote('A');
        setBar(lineA(run, rows, shown));
    }

    /* The run states who and when. An error row carries event_time and no
       actor, so the collection instant is stated once, here, and the rows
       share it. */
    function factsGrid(run) {
        return '<div class="statgrid statgrid-facts">' +
            fact('Definition', esc(run.definition) +
                ' <span class="faint">' + esc(run.definitionVersion) + '</span>') +
            fact('Requested by', esc(run.requestedBy) +
                ' <span class="faint">' + esc(run.requestedAt) + '</span>') +
            fact('Completed', '<span class="mono">' + esc(run.completedAt) + '</span>') +
            fact('Errors collected', '<span class="mono">' + esc(run.collectedAt) + '</span>') +
            fact('Run id', '<span class="mono" title="' + esc(run.runId) + '">' +
                esc(shortId(run.runId)) + '</span>') +
            '</div>';
    }

    function pageHead(run) {
        return '<header class="pagehead"><div>' +
            '<p class="crumbs"><a href="../index.org">Reporting prototypes</a><span>\u203a</span>' +
            '<span>ACME</span><span>\u203a</span><span>Runs</span></p>' +
            '<h1>' + esc(run.definition) + '</h1>' +
            '<p class="lede">Run of ' + esc(run.label) + ' \u00b7 ' + esc(run.state) + ' \u00b7 ' +
            run.rows.length + ' error rows. The rows carry no actor; the run names who asked for it.</p>' +
            '</div><div class="actions">' +
            '<span class="updated">' + esc(run.collectedAt) + '</span>' +
            '<a class="btn linklike" href="../index.org">../index.org</a>' +
            '</div></header>';
    }

    function tabBar(run, rows) {
        var tabs = [
            { id: 'outputs', name: 'Outputs', count: '' },
            { id: 'timeline', name: 'Timeline', count: '' },
            { id: 'errors', name: 'Errors', count: String(rows.length) }
        ];
        return '<div class="tabbar">' + tabs.map(function (tab) {
            return '<button data-act="tab" data-tab="' + tab.id + '"' +
                (S.tab === tab.id ? ' class="on"' : '') + '>' + esc(tab.name) +
                (tab.count === '' ? '' : '<span class="tabcount">' + esc(tab.count) + '</span>') +
                '</button>';
        }).join('') + '</div>';
    }

    function tabStub(tab) {
        if (tab === 'outputs') {
            return '<div class="stub"><h3>Outputs</h3>' +
                '<p>This prototype draws the Errors tab only. The output panel is the prototype for ' +
                '\u201cRead what a run produced\u201d, which sits beside this tab and shows what the ' +
                'archive does contain.</p></div>';
        }
        return '<div class="stub"><h3>Timeline</h3>' +
            '<p>This prototype draws the Errors tab only. The timeline is the prototype for ' +
            '\u201cRun a report and watch it finish\u201d, which shows the run\u2019s six phases and ' +
            'seven workflow steps.</p></div>';
    }

    function errorsTab(run, rows, shown) {
        return summaryStrip(rows) +
            '<div class="filterbar">' + scopeFilter(rows) + subjectFilter() + '</div>' +
            (shown.length === 0 ? noRowNotice() : aTable(shown)) +
            '<p class="rowfoot">Read-only. Every row is an event the run wrote, and the rows are read as ' +
            'a group. Two rows carry <span class="pair">1/2</span> and <span class="pair">2/2</span> when ' +
            'the run recorded the same scope, subject, id and message twice: the detail beside the message ' +
            'says which analytic failed. Nothing here merges them, and the subject is a value to filter by, ' +
            'not a link.</p>' +
            accountingNote(rows);
    }

    function aTable(rows) {
        var groups = ORDER.map(function (scope) {
            var inScope = rows.filter(function (r) { return r.scope_code === scope; });
            if (inScope.length === 0) return '';
            var subjects = [];
            inScope.forEach(function (r) {
                var key = subjectKey(r);
                if (subjects.indexOf(key) < 0) subjects.push(key);
            });
            return '<tr class="scopehead"><td colspan="5">' +
                '<span class="scopename">' + esc(scope) + '</span>' +
                '<span class="scopecount">' + rowsLabel(inScope.length) + ' \u00b7 ' +
                subjects.length + (subjects.length === 1 ? ' subject' : ' subjects') + '</span>' +
                '<span class="scopenote">' + esc(SCOPE_NOTE[scope]) + '</span>' +
                '</td></tr>' +
                inScope.map(aRow).join('');
        }).join('');

        return '<div class="table-wrap"><table class="errors"><thead><tr>' +
            '<th>Severity</th><th>Subject</th><th>Message</th><th>Detail</th><th>Pair</th>' +
            '</tr></thead><tbody>' + groups + '</tbody></table></div>';
    }

    function aRow(r) {
        return '<tr' + (r.groupSize > 1 ? ' class="paired"' : '') + '>' +
            '<td class="cellsev">' + sevTag(r.severity) + violationTag(r) + '</td>' +
            '<td class="cellsubj">' + subjectCell(r) + '</td>' +
            '<td class="cellmsg"><span class="mono">' + esc(r.message) + '</span>' +
            (r.groupIndex > 1
                ? '<span class="sameas">same scope, subject and message as the row above; only detail differs</span>'
                : '') +
            '</td>' +
            '<td class="celldetail">' + contextTag(r) + detailBlock(r) + '</td>' +
            '<td class="cellpair">' + pairMark(r) + '</td>' +
            '</tr>';
    }

    /* ------------------------------------------------------ variant B: panel */

    function renderB() {
        var run = runById(S.run);
        var rows = run.rows;
        var shown = visibleRows(rows);

        var body = pageHead(run) + fixtureNote() +
            '<div class="split">' +
            '<section class="card">' +
            '<header class="sectionhead"><h2>The run</h2>' + stateTag(run.state) + '</header>' +
            runStrip(S.run, true) +
            runFacts(run) +
            '<p class="sidebar-note">The panel beside reads the rows of this run: ' +
            esc(run.shapeNote) + '.</p>' +
            '</section>' +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Errors</h2>' +
            '<div class="meta"><span>showing ' + shown.length + ' of ' + rows.length + ' rows</span>' +
            '<span class="faint">collected ' + esc(run.collectedAt) + '</span></div></header>' +
            summaryStrip(rows) +
            '<div class="filterbar">' + scopeFilter(rows) + severityFilter(rows) + subjectFilter() + '</div>' +
            (shown.length === 0 ? noRowNotice() : bItems(shown)) +
            '<p class="rowfoot">The run stays on screen so a failure is read against the run it belongs to. ' +
            'A pair of rows under one subject is one failure of one subject in two analytics; the detail ' +
            'beside each message says which. Errors are immutable: there is nothing to edit, delete or ' +
            'acknowledge here.</p>' +
            accountingNote(rows) +
            '</section></div>';

        mount(body, '');
        setNote('B');
        setBar(lineB(run, rows, shown));
    }

    function bItems(rows) {
        var html = '';
        var i = 0;
        while (i < rows.length) {
            var first = rows[i];
            if (first.groupSize > 1) {
                var key = duplicateKey(first);
                var group = [];
                while (i < rows.length && duplicateKey(rows[i]) === key) {
                    group.push(rows[i]);
                    i += 1;
                }
                html += '<div class="dupgroup">' +
                    '<div class="duphead">' + sevTag(first.severity) +
                    '<span class="tag muted">' + esc(first.scope_code) + '</span>' +
                    '<span class="dupsubj">' + (first.subject === undefined ? '' : esc(first.subject) + ' ') +
                    esc(first.subject_id) + '</span>' +
                    '<span class="dupnote">the run recorded this once per analytic: ' + group.length +
                    ' rows, one subject, one message, different detail</span></div>' +
                    group.map(bItem).join('') +
                    '</div>';
            } else {
                html += bItem(first);
                i += 1;
            }
        }
        return html;
    }

    function bItem(r) {
        return '<article class="erritem">' +
            '<div class="itemhead">' + sevTag(r.severity) +
            '<span class="tag muted">' + esc(r.scope_code) + '</span>' +
            (r.groupSize > 1 ? '<span class="pair">' + r.groupIndex + '/' + r.groupSize + '</span>' : '') +
            contextTag(r) + violationTag(r) +
            '</div>' +
            '<p class="msg mono">' + esc(r.message) + '</p>' +
            '<p class="subjline">' + subjectCell(r) + '</p>' +
            (r.detail ? '<span class="raw mono">' + esc(r.detail) + '</span>' : '<span class="raw faint">no detail</span>') +
            '</article>';
    }

    /* ----------------------------------------------------- variant C: ledger */

    function renderC() {
        var runs = S.run === 'all' ? RUNS : [runById(S.run)];
        var rows = [];
        runs.forEach(function (run) {
            run.rows.forEach(function (r) { rows.push(r); });
        });
        var shown = visibleRows(rows);

        var body = '<header class="pagehead"><div>' +
            '<p class="crumbs"><a href="../index.org">Reporting prototypes</a><span>\u203a</span>' +
            '<span>ACME</span><span>\u203a</span><span>Errors</span></p>' +
            '<h1>Errors</h1>' +
            '<p class="lede">Every error row this party\u2019s runs recorded, grouped by scope and then by ' +
            'subject, with the run as a column. This is the shape for one question: what is broken across ' +
            'my party right now, rather than what is broken in one run.</p>' +
            '</div><div class="actions">' +
            '<span class="updated">' + shown.length + ' of ' + rows.length + ' rows</span>' +
            '<a class="btn linklike" href="../index.org">../index.org</a>' +
            '</div></header>' +
            fixtureNote() +
            '<section class="card">' +
            '<header class="sectionhead"><h2>The rows</h2>' +
            '<div class="meta"><span>' + rowsLabel(rows.length) + ' from ' + runs.length +
            (runs.length === 1 ? ' run' : ' runs') + '</span>' +
            '<span class="faint">grouped by scope, then by subject</span></div></header>' +
            summaryStrip(rows) +
            '<div class="filterbar">' + runFilter() + severityFilter(rows) + subjectFilter() + '</div>' +
            (shown.length === 0 ? noRowNotice() : cTable(shown)) +
            '<p class="rowfoot">A subject is a discriminator and a value: a trade id is a UUID, a curve id is ' +
            'free text the engine writes. The subject groups the rows; it is not a key, and nothing here ' +
            'links to a trade or a curve. Two rows under one subject with one message are one failure in ' +
            'two analytics, told apart by detail, and never merged. Errors are immutable: no row offers an ' +
            'edit, a delete or an acknowledge.</p>' +
            accountingNoteLedger(runs, rows) +
            '</section>';

        mount(body, 'wide');
        setNote('C');
        setBar(lineC(runs, rows, shown));
    }

    function runFilter() {
        return '<div class="fgroup"><span class="flbl">Run</span>' +
            '<button class="chipbtn' + (S.run === 'all' ? ' on' : '') + '" data-act="run" data-run="all">' +
            'All runs<span class="n">' + RUNS.reduce(function (sum, run) { return sum + run.rows.length; }, 0) +
            '</span></button>' +
            RUNS.map(function (run) {
                return '<button class="chipbtn' + (S.run === run.id ? ' on' : '') + '" data-act="run" data-run="' +
                    run.id + '">' + esc(run.label) + '<span class="n">' + run.rows.length + '</span></button>';
            }).join('') + '</div>';
    }

    function cTable(rows) {
        var html = ORDER.map(function (scope) {
            var inScope = rows.filter(function (r) { return r.scope_code === scope; });
            if (inScope.length === 0) return '';

            var subjects = [];
            var byKey = {};
            inScope.forEach(function (r) {
                var key = subjectKey(r);
                if (!byKey[key]) {
                    byKey[key] = { subject: r.subject, subject_id: r.subject_id, rows: [] };
                    subjects.push(byKey[key]);
                }
                byKey[key].rows.push(r);
            });

            var head = '<tr class="scopehead"><td colspan="5">' +
                '<span class="scopename">' + esc(scope) + '</span>' +
                '<span class="scopecount">' + rowsLabel(inScope.length) + ' \u00b7 ' + subjects.length +
                (subjects.length === 1 ? ' subject' : ' subjects') + '</span>' +
                '<span class="scopenote">' + esc(SCOPE_NOTE[scope]) + '</span>' +
                '</td></tr>';

            var body = subjects.map(function (group) {
                var runs = [];
                group.rows.forEach(function (r) {
                    if (runs.indexOf(r.run.label) < 0) runs.push(r.run.label);
                });
                var pairs = group.rows.some(function (r) { return r.groupSize > 1; });
                return '<tr class="subjhead"><td colspan="5">' +
                    (group.subject === undefined
                        ? '<span class="tag muted">no subject</span><span class="subnote">the run itself</span>'
                        : '<span class="dsc">' + esc(group.subject) + '</span>' +
                          '<span class="mono fullid">' + esc(group.subject_id) + '</span>') +
                    '<span class="scopecount">' + group.rows.length +
                    (group.rows.length === 1 ? ' row' : ' rows') +
                    (runs.length > 1 ? ' across ' + runs.length + ' runs' : '') + '</span>' +
                    (pairs ? '<span class="pair">one failure, two analytics</span>' : '') +
                    '</td></tr>' +
                    group.rows.map(cRow).join('');
            }).join('');

            return head + body;
        }).join('');

        return '<div class="table-wrap"><table class="ledger"><thead><tr>' +
            '<th>Severity</th><th>Subject</th><th>Message</th><th>Detail</th><th>Run</th>' +
            '</tr></thead><tbody>' + html + '</tbody></table></div>';
    }

    function cRow(r) {
        return '<tr' + (r.groupSize > 1 ? ' class="paired"' : '') + '>' +
            '<td class="cellsev">' + sevTag(r.severity) + violationTag(r) + '</td>' +
            '<td class="cellsubj">' + subjectCell(r) +
            (r.groupSize > 1 ? ' <span class="pair">' + r.groupIndex + '/' + r.groupSize + '</span>' : '') +
            '</td>' +
            '<td class="cellmsg"><span class="mono">' + esc(r.message) + '</span></td>' +
            '<td class="celldetail">' + contextTag(r) + detailBlock(r) + '</td>' +
            '<td class="cellrun">' + '<span class="mono">' + esc(r.run.label) + '</span>' +
            '<span class="runstate">' + esc(r.run.state) + '</span></td>' +
            '</tr>';
    }

    /* --------------------------------------------------------------- chrome */

    function mount(body, shellClass) {
        document.getElementById('app').innerHTML =
            '<div class="shell' + (shellClass ? ' ' + shellClass : '') + '">' +
            appHeader() + '<main>' + body + '</main></div>';
    }

    function setNote(variantId) {
        var active = variantByName(variantId);
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, nothing reads a server \u00b7 variant ' +
            active.id + ' live: ' + active.name.replace(/^[A-C] \u00b7 /, '');
    }

    function lineA(run, rows, shown) {
        return '<b>A</b> <span class="k">\u00b7 tab on the run \u00b7</span> run ' +
            esc(run.label) + ' (' + rows.length + ' rows, ' + esc(run.state) + ') ' +
            '<span class="k">\u00b7</span> tab <span class="mono">' + esc(S.tab) + '</span>' +
            '<span class="k">\u00b7</span> scope <span class="mono">' + esc(S.scope) + '</span>' +
            '<span class="k">\u00b7</span> severity <span class="mono">' + esc(S.sev) + '</span>' +
            '<span class="k">\u00b7</span> showing <b>' + shown.length + '</b> of ' + rows.length + ' rows' +
            (S.subject ? ' <span class="k">\u00b7</span> subject <span class="mono">' + esc(shortId(S.subject)) + '</span>' : '');
    }

    function lineB(run, rows, shown) {
        return '<b>B</b> <span class="k">\u00b7 panel beside the run \u00b7</span> run ' +
            esc(run.label) + ' (' + rows.length + ' rows, ' + esc(run.state) + ') ' +
            '<span class="k">\u00b7</span> scope <span class="mono">' + esc(S.scope) + '</span>' +
            '<span class="k">\u00b7</span> severity <span class="mono">' + esc(S.sev) + '</span>' +
            '<span class="k">\u00b7</span> showing <b>' + shown.length + '</b> of ' + rows.length + ' rows' +
            (S.subject ? ' <span class="k">\u00b7</span> subject <span class="mono">' + esc(shortId(S.subject)) + '</span>' : '');
    }

    function lineC(runs, rows, shown) {
        var ids = runs.map(function (run) { return run.label; }).join(', ');
        return '<b>C</b> <span class="k">\u00b7 ledger across runs \u00b7</span> ' +
            (S.run === 'all' ? 'all ' + runs.length + ' runs' : 'run ' + esc(ids)) +
            '<span class="k">\u00b7</span> severity <span class="mono">' + esc(S.sev) + '</span>' +
            '<span class="k">\u00b7</span> showing <b>' + shown.length + '</b> of ' + rows.length + ' rows' +
            (S.subject ? ' <span class="k">\u00b7</span> subject <span class="mono">' + esc(shortId(S.subject)) + '</span>' : '') +
            ' <span class="k">\u00b7</span> grouped by scope, then subject';
    }

    function setBar(line) {
        var buttons = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + v.id + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.name) + '</button>';
        }).join('');

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + buttons +
            '<a class="label" href="../index.org" style="color:var(--ink-faint)">../index.org</a>' +
            '</div>' +
            '<div class="stateline">' + line + '</div>';
    }

    /* -------------------------------------------------------------- actions */

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');

        if (act === 'variant') {
            S.variant = el.getAttribute('data-variant');
            /* C is a surface for the whole party, so it opens across runs; A and
               B need one run and fall back to the run whose market data was
               removed. */
            if (S.variant === 'C') {
                S.run = 'all';
                S.scope = 'all';
            } else if (S.run === 'all') {
                S.run = 'r1';
            }
            S.tab = 'errors';
        } else if (act === 'run') {
            S.run = el.getAttribute('data-run');
        } else if (act === 'scope') {
            S.scope = el.getAttribute('data-scope');
        } else if (act === 'sev') {
            S.sev = el.getAttribute('data-sev');
        } else if (act === 'tab') {
            S.tab = el.getAttribute('data-tab');
        } else if (act === 'subject') {
            var value = el.getAttribute('data-subject');
            S.subject = S.subject === value ? '' : value;
        } else if (act === 'clear-subject') {
            S.subject = '';
        }

        render();
        writeParams();
    });

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        if (variantByName(params.get('variant')) && params.get('variant')) S.variant = params.get('variant');
        if (S.variant === 'C') S.run = 'all';
        if (params.get('run') && (params.get('run') === 'all' || runById(params.get('run')).id === params.get('run'))) {
            S.run = params.get('run');
        }
        if (params.get('scope') && (params.get('scope') === 'all' || ORDER.indexOf(params.get('scope')) >= 0)) {
            S.scope = params.get('scope');
        }
        if (params.get('sev') && ['all', 'error', 'warning'].indexOf(params.get('sev')) >= 0) {
            S.sev = params.get('sev');
        }
        if (params.get('subject')) S.subject = params.get('subject');
        var tab = params.get('tab');
        if (tab === 'outputs' || tab === 'timeline' || tab === 'errors') S.tab = tab;
    }

    function writeParams() {
        var params = new URLSearchParams();
        params.set('variant', S.variant);
        params.set('run', S.run);
        params.set('scope', S.scope);
        params.set('sev', S.sev);
        if (S.subject) params.set('subject', S.subject);
        if (S.variant === 'A') params.set('tab', S.tab);
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    function render() {
        if (S.variant === 'A') renderA();
        else if (S.variant === 'B') renderB();
        else renderC();
    }

    readParams();
    render();
})();
