/* Reporting prototype: see the reports a party runs. Self-contained: plain
 * JavaScript, mock data, no framework, no build step, and nothing that
 * outlives the page.
 *
 * See the reports a party runs, from
 * doc/knowledge/journeys/reporting/journey_see_the_reports_a_party_runs.org.
 *
 * The screen answers "which reports does my party run, and how did each one
 * last go?" Every row is a fixture: the definitions, the runs and the errors
 * are mock, no operation serves them, and nothing here reads the server.
 *
 * Three variants disagree about the front door of reporting:
 *   A index-first     -- the catalogue of definitions is the page.
 *   B ledger-first    -- one dense table of runs is the page; the catalogue is
 *                        a filter.
 *   C attention-first -- only failed runs and runs carrying errors are shown;
 *                        clean runs sit behind a filter that is off by default. */

(function () {
    'use strict';

    /* ---------------------------------------------------------- fixtures */

    var NOW = '2026-10-17 08:43:12';

    var PARTIES = [
        { code: 'ACMCOR', name: 'ACCO Capital Markets' },
        { code: 'ACCOUK', name: 'ACCO UK' },
        { code: 'ACCOUS', name: 'ACCO US' },
        { code: 'ACCOHK', name: 'ACCO Hong Kong' }
    ];

    var SCOPES = ['Trade', 'Analytics', 'Curve', 'Configuration', 'Report'];

    var HEALTH = {
        'never run': { tag: 'dashed', cls: 'never' },
        'running': { tag: 'accent', cls: 'run' },
        'completed clean': { tag: 'ok', cls: 'clean' },
        'completed with errors': { tag: 'warn', cls: 'attn' },
        'failed': { tag: 'bad', cls: 'fail' }
    };

    var HEALTH_ORDER = ['failed', 'completed with errors', 'running', 'never run', 'completed clean'];

    /* The catalogue the party owns. A definition with a schedule expression is
       scheduled; one with none is run by hand, so "not run" means something
       different for each. */
    var REPORTS = [
        {
            id: 'r-group-consolidated-exposure',
            name: 'Group Consolidated Exposure',
            type: 'risk',
            party: 'ACMCOR',
            schedule: '0 0 6 * * ?',
            scheduleLabel: 'Daily 06:00',
            owner: 'Priya Raman',
            cycle: 'eod',
            audience: 'market-risk'
        },
        {
            id: 'r-liquidity-buffer-bonds',
            name: 'Bucketed DV01',
            type: 'risk',
            party: 'ACMCOR',
            schedule: '0 30 5 * * ?',
            scheduleLabel: 'Daily 05:30',
            owner: 'Priya Raman',
            cycle: 'eod',
            audience: 'market-risk'
        },
        {
            id: 'r-large-exposures',
            name: 'Large Exposures',
            type: 'risk',
            party: 'ACCOUK',
            schedule: '0 15 6 * * ?',
            scheduleLabel: 'Daily 06:15',
            owner: 'Tom Aldridge',
            cycle: 'eod',
            audience: 'market-risk'
        },
        {
            id: 'r-leverage-ratio',
            name: 'Leverage Ratio',
            type: 'risk',
            party: 'ACCOUK',
            schedule: '0 45 6 * * ?',
            scheduleLabel: 'Daily 06:45',
            owner: 'Tom Aldridge',
            cycle: 'eod',
            audience: 'market-risk'
        },
        {
            id: 'r-liquidity-coverage',
            name: 'Liquidity Coverage',
            type: 'risk',
            party: 'ACCOUS',
            schedule: '0 0 7 * * ?',
            scheduleLabel: 'Daily 07:00',
            owner: 'Sofia Marques',
            cycle: 'eod',
            audience: 'market-risk'
        },
        {
            id: 'r-board-risk-dashboard',
            name: 'Board Risk Dashboard',
            type: 'grid',
            party: 'ACCOHK',
            schedule: '0 15 7 ? * MON-FRI',
            scheduleLabel: 'Weekdays 07:15',
            owner: 'Wei Chen',
            cycle: 'eod',
            audience: 'market-risk'
        },
        {
            id: 'r-data-quality-completeness',
            name: 'Data Quality Completeness',
            type: 'risk',
            party: 'ACMCOR',
            schedule: null,
            scheduleLabel: 'On demand',
            owner: 'Dan Okoro',
            cycle: 'intraday',
            audience: 'engineer'
        },
        {
            id: 'r-system-health',
            name: 'System Health',
            type: 'risk',
            party: 'ACCOHK',
            schedule: '0 0/15 * * * ?',
            scheduleLabel: 'Every 15 minutes',
            owner: 'Wei Chen',
            cycle: 'intraday',
            audience: 'engineer'
        },
        {
            id: 'r-headline-position',
            name: 'Headline Position',
            type: 'risk',
            party: 'ACMCOR',
            schedule: '0 0 8 * * ?',
            scheduleLabel: 'Daily 08:00',
            owner: 'Tom Aldridge',
            cycle: 'intraday',
            audience: 'trading-desk'
        },
        {
            id: 'r-sample-bundle-smoke-test',
            name: 'Intraday Risk Monitor',
            type: 'risk',
            party: 'ACCOUK',
            schedule: null,
            scheduleLabel: 'On demand',
            owner: 'Dan Okoro',
            cycle: 'intraday',
            audience: 'release check'
        }
    ];

    function E(severity, scope, subject, message) {
        return { severity: severity, scope: scope, subject: subject, message: message };
    }

    /* The runs, newest first by start. The latest run of each definition is
       the run with the greatest started_at, as the journey defines it. */
    var RUNS = [
        {
            id: 'RUN-20261017-0630-01',
            reportId: 'r-liquidity-coverage',
            kind: 'partial',
            state: 'running',
            started: '2026-10-17 08:31:10',
            ended: null,
            errors: []
        },
        {
            id: 'RUN-20261017-0800-01',
            reportId: 'r-system-health',
            kind: 'full',
            state: 'completed',
            started: '2026-10-17 08:30:00',
            ended: '2026-10-17 08:30:41',
            errors: []
        },
        {
            id: 'RUN-20261017-0800-02',
            reportId: 'r-system-health',
            kind: 'full',
            state: 'completed',
            started: '2026-10-17 08:15:00',
            ended: '2026-10-17 08:15:39',
            errors: []
        },
        {
            id: 'RUN-20261017-0800-03',
            reportId: 'r-headline-position',
            kind: 'full',
            state: 'completed',
            started: '2026-10-17 08:00:02',
            ended: '2026-10-17 08:02:11',
            errors: []
        },
        {
            id: 'RUN-20261017-0800-04',
            reportId: 'r-system-health',
            kind: 'full',
            state: 'completed',
            started: '2026-10-17 08:00:00',
            ended: '2026-10-17 08:00:44',
            errors: [
                E('warning', 'Analytics', 'Service latency',
                    'The market data service answered in 2.4 s, above the 2.0 s panel threshold; the panel shows the previous sample.')
            ]
        },
        {
            id: 'RUN-20261017-0715-01',
            reportId: 'r-board-risk-dashboard',
            kind: 'full',
            state: 'completed',
            started: '2026-10-17 07:15:02',
            ended: '2026-10-17 07:22:48',
            errors: [
                E('error', 'Analytics', 'VaR 1-day 99%',
                    'The VaR series is missing for ACCOHK equities; the panel shows the previous day and says so.'),
                E('warning', 'Configuration', 'Dashboard layout',
                    'Panel "Liquidity" falls back to the tenant default layout.')
            ]
        },
        {
            id: 'RUN-20261017-0645-01',
            reportId: 'r-leverage-ratio',
            kind: 'full',
            state: 'completed',
            started: '2026-10-17 06:45:01',
            ended: '2026-10-17 06:51:20',
            errors: []
        },
        {
            id: 'RUN-20261017-0615-01',
            reportId: 'r-large-exposures',
            kind: 'full',
            state: 'failed',
            started: '2026-10-17 06:15:00',
            ended: '2026-10-17 06:15:38',
            errors: [
                E('error', 'Configuration', 'Run configuration',
                    'The run configuration names the storage bucket "report-output-eu", which party ACCOUK cannot write.'),
                E('error', 'Report', 'Large Exposures',
                    'The engine stopped before the report was written; no output exists for this run.')
            ]
        },
        {
            id: 'RUN-20261017-0600-01',
            reportId: 'r-group-consolidated-exposure',
            kind: 'full',
            state: 'completed',
            started: '2026-10-17 06:00:04',
            ended: '2026-10-17 06:12:41',
            errors: [
                E('error', 'Trade', 'TRD-8841204 (IRS 10Y EUR)',
                    'Trade would not build: the curve "EUR-OIS-10Y" is not published for 2026-10-16.'),
                E('error', 'Trade', 'TRD-8841277 (IRS 5Y GBP)',
                    'No market data for GBP-OIS-5Y at 2026-10-16; the trade was dropped from the exposure.'),
                E('error', 'Trade', 'TRD-8841301 (Basis 3M EUR)',
                    'Leg 2 references curve "EUR-BASIS-3M", which has no fixings after 2026-10-10.'),
                E('error', 'Trade', 'TRD-8841350 (FX Forward 6M)',
                    'Trade would not build: counterparty "NWCAP" is missing a legal entity id in reference data.'),
                E('error', 'Curve', 'EUR-OIS-10Y',
                    'Curve has 3 missing fixings; the 10Y bucket was extrapolated.'),
                E('error', 'Curve', 'GBP-OIS-5Y',
                    'Curve failed the monotonicity check between 4Y and 6Y.'),
                E('error', 'Analytics', 'Portfolio NPV',
                    'The analytic returned no result for 41 of 1,208 trades.'),
                E('error', 'Report', 'Counterparty concentration',
                    'The section holds 0 rows where 27 were expected.'),
                E('warning', 'Trade', 'TRD-8811090 (FX Forward)',
                    'Trade settled on 2026-10-15 and is included as at the previous close.'),
                E('warning', 'Configuration', 'Report parameters',
                    'Configuration falls back to the tenant default for "base_currency".'),
                E('warning', 'Report', 'Report title',
                    'The header uses the definition\'s current name; the run carries the name from 2026-10-12.')
            ]
        },
        {
            id: 'RUN-20261017-0530-01',
            reportId: 'r-liquidity-buffer-bonds',
            kind: 'full',
            state: 'completed',
            started: '2026-10-17 05:30:01',
            ended: '2026-10-17 05:36:02',
            errors: []
        },
        {
            id: 'RUN-20261016-1722-01',
            reportId: 'r-sample-bundle-smoke-test',
            kind: 'validation',
            state: 'completed',
            started: '2026-10-16 17:22:10',
            ended: '2026-10-16 17:23:05',
            errors: []
        },
        {
            id: 'RUN-20261016-1720-01',
            reportId: 'r-sample-bundle-smoke-test',
            kind: 'validation',
            state: 'failed',
            started: '2026-10-16 17:20:02',
            ended: '2026-10-16 17:20:46',
            errors: [
                E('error', 'Report', 'Intraday Risk Monitor',
                    'The sample bundle did not load: the fixture names book "ACME-NEW" and no such book exists.')
            ]
        },
        {
            id: 'RUN-20261016-0715-01',
            reportId: 'r-board-risk-dashboard',
            kind: 'full',
            state: 'completed',
            started: '2026-10-16 07:15:01',
            ended: '2026-10-16 07:21:36',
            errors: []
        },
        {
            id: 'RUN-20261016-0645-01',
            reportId: 'r-leverage-ratio',
            kind: 'full',
            state: 'completed',
            started: '2026-10-16 06:45:03',
            ended: '2026-10-16 06:51:08',
            errors: []
        },
        {
            id: 'RUN-20261016-0615-01',
            reportId: 'r-large-exposures',
            kind: 'full',
            state: 'completed',
            started: '2026-10-16 06:15:01',
            ended: '2026-10-16 06:20:52',
            errors: []
        },
        {
            id: 'RUN-20261016-0600-01',
            reportId: 'r-group-consolidated-exposure',
            kind: 'full',
            state: 'completed',
            started: '2026-10-16 06:00:02',
            ended: '2026-10-16 06:10:18',
            errors: []
        },
        {
            id: 'RUN-20261016-0530-01',
            reportId: 'r-liquidity-buffer-bonds',
            kind: 'full',
            state: 'completed',
            started: '2026-10-16 05:30:00',
            ended: '2026-10-16 05:35:44',
            errors: []
        },
        {
            id: 'RUN-20261016-0800-01',
            reportId: 'r-headline-position',
            kind: 'full',
            state: 'completed',
            started: '2026-10-16 08:00:01',
            ended: '2026-10-16 08:02:04',
            errors: []
        },
        {
            id: 'RUN-20261015-0600-01',
            reportId: 'r-group-consolidated-exposure',
            kind: 'full',
            state: 'completed',
            started: '2026-10-15 06:00:05',
            ended: '2026-10-15 06:11:02',
            errors: [
                E('error', 'Trade', 'TRD-8839102 (IRS 7Y USD)',
                    'Trade would not build: the curve "USD-OIS-7Y" is not published for 2026-10-14.'),
                E('error', 'Curve', 'USD-OIS-7Y',
                    'Curve has 2 missing fixings and was extrapolated.'),
                E('error', 'Analytics', 'Portfolio NPV',
                    'The analytic returned no result for 12 of 1,196 trades.'),
                E('warning', 'Report', 'Report title',
                    'The run carries the definition\'s name from 2026-10-01.')
            ]
        }
    ];

    var VARIANTS = [
        {
            id: 'A',
            name: 'A \u00b7 index-first',
            gist: 'The front door is the catalogue of report definitions: one row each, its schedule, its owner, and the health of its latest run. Open a report to reach its runs.',
            lede: 'The reports this tenant runs, with the health of each one\u2019s latest run. Open a report to read its runs.'
        },
        {
            id: 'B',
            name: 'B \u00b7 ledger-first',
            gist: 'The front door is one dense ledger of runs across the tenant, newest first, with the report as a column. The catalogue is a filter, not a page.',
            lede: 'Every run across the tenant, newest first. Narrow by party, report or health; the catalogue is a filter.'
        },
        {
            id: 'C',
            name: 'C \u00b7 attention-first',
            gist: 'The front door shows only what needs a human: failed runs and runs that completed while carrying errors. Clean runs exist, behind a filter that is off by default.',
            lede: 'Only the runs that need a human: failed, and completed while carrying errors. Clean runs are behind a filter.'
        }
    ];

    /* ----------------------------------------------------------- helpers */

    function $(id) { return document.getElementById(id); }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function reportById(id) {
        return REPORTS.filter(function (r) { return r.id === id; })[0];
    }

    function partyName(code) {
        var party = PARTIES.filter(function (p) { return p.code === code; })[0];
        return party ? party.name : code;
    }

    function byStartedDesc(left, right) {
        if (left.started === right.started) return left.id < right.id ? 1 : -1;
        return left.started < right.started ? 1 : -1;
    }

    function runsOf(reportId) {
        return RUNS.filter(function (run) { return run.reportId === reportId; }).sort(byStartedDesc);
    }

    function latestRun(reportId) {
        return runsOf(reportId)[0] || null;
    }

    function runHealth(run) {
        if (!run) return 'never run';
        if (run.state === 'running') return 'running';
        if (run.state === 'failed') return 'failed';
        return run.errors.length > 0 ? 'completed with errors' : 'completed clean';
    }

    function healthOfReport(reportId) {
        return runHealth(latestRun(reportId));
    }

    function severityCounts(run) {
        var out = { error: 0, warning: 0, total: 0 };
        if (!run) return out;
        run.errors.forEach(function (error) {
            out[error.severity] += 1;
            out.total += 1;
        });
        return out;
    }

    function scopeCounts(run) {
        var made = {};
        if (run) {
            run.errors.forEach(function (error) {
                if (!made[error.scope]) made[error.scope] = { scope: error.scope, error: 0, warning: 0 };
                made[error.scope][error.severity] += 1;
            });
        }
        return SCOPES.filter(function (scope) { return made[scope]; })
            .map(function (scope) { return made[scope]; });
    }

    function plural(count, word) {
        return count + ' ' + word + (count === 1 ? '' : 's');
    }

    function secondsBetween(start, end) {
        var from = Date.parse(start.replace(' ', 'T'));
        var to = Date.parse(end.replace(' ', 'T'));
        return Math.max(0, Math.round((to - from) / 1000));
    }

    function elapsed(run) {
        if (run.ended) return secondsBetween(run.started, run.ended);
        return secondsBetween(run.started, NOW);
    }

    function duration(run) {
        if (!run.ended) return fmtDuration(secondsBetween(run.started, NOW)) + ' so far';
        return fmtDuration(secondsBetween(run.started, run.ended));
    }

    function fmtDuration(seconds) {
        if (seconds < 60) return seconds + 's';
        var minutes = Math.floor(seconds / 60);
        var rest = seconds % 60;
        if (minutes < 60) return minutes + 'm ' + (rest < 10 ? '0' + rest : rest) + 's';
        var hours = Math.floor(minutes / 60);
        return hours + 'h ' + (minutes % 60 < 10 ? '0' + (minutes % 60) : minutes % 60) + 'm';
    }

    function dayOf(stamp) {
        if (!stamp) return '\u2014';
        var parts = stamp.split(' ')[0].split('-');
        var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        return Number(parts[2]) + ' ' + months[Number(parts[1]) - 1];
    }

    function clockOf(stamp) {
        if (!stamp) return '\u2014';
        return stamp.split(' ')[1];
    }

    function healthTag(word) {
        var meta = HEALTH[word];
        return '<span class="tag ' + meta.tag + '">' + esc(word) + '</span>';
    }

    function reportLink(report) {
        return '<span class="reportname"><span class="nm">' + esc(report.name) + '</span>' +
            '<span class="rf mono">' + esc(report.type) + ' \u00b7 ' + esc(report.cycle) + '</span></span>';
    }

    function errorCell(run) {
        if (!run) return '<span class="errcell"><span class="none">\u2014</span></span>';
        var counts = severityCounts(run);
        if (run.state === 'running') {
            return '<span class="errcell"><span class="none">not finished</span></span>';
        }
        if (counts.total === 0) {
            return '<span class="errcell"><span class="none">no errors</span></span>';
        }
        var parts = [];
        if (counts.error > 0) {
            parts.push('<span class="n-err">' + counts.error + '</span><span class="unit">' +
                (counts.error === 1 ? 'error' : 'errors') + '</span>');
        }
        if (counts.warning > 0) {
            parts.push('<span class="n-warn">' + counts.warning + '</span><span class="unit">' +
                (counts.warning === 1 ? 'warning' : 'warnings') + '</span>');
        }
        return '<span class="errcell">' + parts.join('<span class="sep">\u00b7</span>') + '</span>';
    }

    function scopeChips(run) {
        var counts = scopeCounts(run);
        if (counts.length === 0) return '';
        return '<span class="scopes">' + counts.map(function (entry) {
            var bits = [];
            if (entry.error > 0) bits.push('<span class="se">' + entry.error + 'e</span>');
            if (entry.warning > 0) bits.push('<span class="sw">' + entry.warning + 'w</span>');
            return '<span class="s">' + esc(entry.scope) + ' <b>' + bits.join(' ') + '</b></span>';
        }).join('') + '</span>';
    }

    function rowClass(word) {
        if (word === 'completed with errors') return 'row-attn';
        if (word === 'failed') return 'row-fail';
        return '';
    }

    function reportHealthCounts(rows) {
        var counts = {};
        HEALTH_ORDER.forEach(function (word) { counts[word] = 0; });
        rows.forEach(function (report) { counts[healthOfReport(report.id)] += 1; });
        return counts;
    }

    function runHealthCounts(rows) {
        var counts = {};
        HEALTH_ORDER.forEach(function (word) { counts[word] = 0; });
        rows.forEach(function (run) { counts[runHealth(run)] += 1; });
        return counts;
    }

    /* ------------------------------------------------------------ state */

    var S = {
        variant: 'A',
        party: 'all',
        health: 'all',
        report: 'all',
        drill: null,
        drillRun: null,
        run: null,
        showClean: false
    };

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var variant = params.get('variant');
        if (VARIANTS.some(function (v) { return v.id === variant; })) S.variant = variant;
        var report = params.get('report');
        if (report && reportById(report)) {
            S.drill = report;
            var runs = runsOf(report);
            S.drillRun = runs.length > 0 ? runs[0].id : null;
        }
        var run = params.get('run');
        if (run && RUNS.some(function (candidate) { return candidate.id === run; })) S.run = run;
        if (params.get('clean') === '1') S.showClean = true;
    }

    function writeParams() {
        var params = new URLSearchParams();
        params.set('variant', S.variant);
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    function activeVariant() {
        return VARIANTS.filter(function (v) { return v.id === S.variant; })[0];
    }

    function partyReports() {
        return REPORTS.filter(function (report) {
            return S.party === 'all' || report.party === S.party;
        });
    }

    function partyRuns() {
        return RUNS.filter(function (run) {
            if (S.party === 'all') return true;
            return reportById(run.reportId).party === S.party;
        }).sort(byStartedDesc);
    }

    function setVariant(id) {
        if (S.variant === id) return;
        S.variant = id;
        S.drill = null;
        S.drillRun = null;
        S.run = null;
        S.health = 'all';
        S.report = 'all';
        S.showClean = false;
    }

    function openReport(id) {
        S.drill = id;
        var runs = runsOf(id);
        S.drillRun = runs.length > 0 ? runs[0].id : null;
        S.run = null;
    }

    function openRun(id) {
        S.run = id;
    }

    function clearDrill() {
        S.drill = null;
        S.drillRun = null;
        S.run = null;
    }

    /* ------------------------------------------------------------ parts */

    function partyChips(countFor) {
        var buttons = [{ code: 'all', label: 'All parties' }].concat(PARTIES.map(function (party) {
            return { code: party.code, label: party.code };
        }));
        return '<div class="grp"><span class="glbl">Party</span>' + buttons.map(function (party) {
            return '<button class="chip' + (S.party === party.code ? ' on' : '') +
                '" data-act="party" data-party="' + esc(party.code) + '">' +
                esc(party.label) + '<span class="n">' + countFor(party.code) + '</span></button>';
        }).join('') + '</div>';
    }

    function reportCountFor(code) {
        if (code === 'all') return REPORTS.length;
        return REPORTS.filter(function (report) { return report.party === code; }).length;
    }

    function runCountFor(code) {
        if (code === 'all') return RUNS.length;
        return RUNS.filter(function (run) { return reportById(run.reportId).party === code; }).length;
    }

    function healthChips(counts, total) {
        return '<div class="grp"><span class="glbl">Health</span>' +
            '<button class="chip' + (S.health === 'all' ? ' on' : '') + '" data-act="health" data-health="all">all' +
            '<span class="n">' + total + '</span></button>' +
            HEALTH_ORDER.filter(function (word) {
                return word !== 'never run' || counts[word] > 0;
            }).map(function (word) {
                return '<button class="chip' + (S.health === word ? ' on' : '') +
                    '" data-act="health" data-health="' + word + '">' + esc(word) +
                    '<span class="n">' + counts[word] + '</span></button>';
            }).join('') + '</div>';
    }

    function healthSummary(rows) {
        var counts = reportHealthCounts(rows);
        return '<div class="healthsum">' + HEALTH_ORDER.map(function (word) {
            return '<button class="hcard ' + HEALTH[word].cls + (S.health === word ? ' on' : '') +
                '" data-act="health" data-health="' + esc(word) + '">' +
                '<span><span class="hl">' + esc(word) + '</span></span>' +
                '<span class="hn">' + counts[word] + '</span></button>';
        }).join('') + '</div>';
    }

    function catalogueRows(rows, owned) {
        if (rows.length === 0) {
            if (owned === 0) {
                return '<tr><td colspan="8" class="wrap dim">This party owns no report definition. ' +
                    'A definition is created by the party, so this is a statement about the party, not an empty table.</td></tr>';
            }
            return '<tr><td colspan="8" class="wrap dim">No definition matches the party and health filters.</td></tr>';
        }
        return rows.map(function (report) {
            var run = latestRun(report.id);
            var word = healthOfReport(report.id);
            return '<tr class="clickable ' + rowClass(word) + (S.drill === report.id ? ' row-open' : '') +
                '" data-act="open-report" data-report="' + esc(report.id) + '">' +
                '<td>' + reportLink(report) + '</td>' +
                '<td class="mono">' + esc(report.party) + '</td>' +
                '<td><span class="cellgroup"><span>' + esc(report.scheduleLabel) + '</span>' +
                (report.schedule ? '<span class="tag muted mono">' + esc(report.schedule) + '</span>'
                    : '<span class="tag dashed">on demand</span>') + '</span></td>' +
                '<td class="nowrap">' + esc(report.owner) + '</td>' +
                '<td class="nowrap">' + (run ? '<span class="mono">' + esc(clockOf(run.started)) + '</span> <span class="faint">' + esc(dayOf(run.started)) + '</span>'
                    : '<span class="faint">\u2014</span>') + '</td>' +
                '<td class="mono nowrap">' + (run ? esc(duration(run)) : '\u2014') + '</td>' +
                '<td>' + healthTag(word) + '</td>' +
                '<td>' + errorCell(run) + '</td>' +
                '</tr>';
        }).join('');
    }

    function cataloguePanel() {
        var rows = partyReports();
        var filtered = S.health === 'all' ? rows : rows.filter(function (report) {
            return healthOfReport(report.id) === S.health;
        });
        if (S.health === 'all') {
            filtered = filtered.slice().sort(function (left, right) {
                return left.name.localeCompare(right.name);
            });
        } else {
            filtered = filtered.slice().sort(function (left, right) {
                return left.name.localeCompare(right.name);
            });
        }

        return '<section class="card">' +
            '<header class="sectionhead"><h2>Report definitions</h2>' +
            '<div class="meta"><span>' + rows.length + ' definitions \u00b7 ' +
            (S.party === 'all' ? PARTIES.length + ' parties' : 'party ' + esc(S.party)) +
            (S.health === 'all' ? '' : ' \u00b7 filtered to ' + esc(S.health)) + '</span></div></header>' +
            '<div class="filters" style="margin-bottom:14px">' + partyChips(reportCountFor) +
            healthChips(reportHealthCounts(rows), rows.length) + '</div>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Report</th><th>Party</th><th>Schedule</th><th>Owner</th>' +
            '<th>Latest run started</th><th>Duration</th><th>Health</th><th>Errors</th>' +
            '</tr></thead><tbody>' + catalogueRows(filtered, rows.length) + '</tbody></table></div>' +
            '<p class="rowfoot">One row per definition. Health is derived from the latest run, never from ' +
            'the definition: a definition can be healthy while its last run failed. Open a report to read its runs. ' +
            'The error count is split into errors and warnings so a completed row carrying errors is not read as clean.</p>' +
            '</section>';
    }

    function errorList(errors) {
        if (errors.length === 0) {
            return '<p class="dim">This run recorded no errors.</p>';
        }
        return '<ul class="errlist">' + errors.map(function (error) {
            return '<li class="sev-' + esc(error.severity) + '">' +
                '<div class="eline"><span class="tag ' + (error.severity === 'error' ? 'bad' : 'warn') + '">' +
                esc(error.severity) + '</span>' +
                '<span class="tag scope">' + esc(error.scope) + '</span>' +
                '<span class="subj">' + esc(error.subject) + '</span></div>' +
                '<p class="msg">' + esc(error.message) + '</p></li>';
        }).join('') + '</ul>';
    }

    function reportDetail() {
        var report = reportById(S.drill);
        if (!report) { clearDrill(); return cataloguePanel(); }
        var runs = runsOf(report.id);
        var shown = runs.filter(function (run) { return run.id === S.drillRun; })[0] || runs[0] || null;
        var word = healthOfReport(report.id);

        var runRows = runs.length === 0
            ? '<tr><td colspan="6" class="wrap dim">This definition has never run. Nothing was checked, ' +
              'so it does not read as clean.</td></tr>'
            : runs.map(function (run) {
                var runWord = runHealth(run);
                return '<tr class="clickable ' + rowClass(runWord) + (shown && run.id === shown.id ? ' row-open' : '') +
                    '" data-act="pick-run" data-run="' + esc(run.id) + '">' +
                    '<td><span class="mono">' + esc(clockOf(run.started)) + '</span> <span class="faint">' +
                    esc(dayOf(run.started)) + '</span></td>' +
                    '<td class="mono">' + (run.ended ? esc(clockOf(run.ended)) : '\u2014') + '</td>' +
                    '<td>' + esc(run.kind) + '</td>' +
                    '<td class="mono">' + esc(run.id) + '</td>' +
                    '<td class="mono">' + esc(duration(run)) + '</td>' +
                    '<td>' + healthTag(runWord) + '</td>' +
                    '<td>' + errorCell(run) + '</td>' +
                    '</tr>';
            }).join('');

        return '<section class="card">' +
            '<div class="detailhead"><div>' +
            '<a class="back" href="#" data-act="back-to-catalogue">\u2190 All report definitions</a>' +
            '<h2 style="font-size:20px;margin-top:6px">' + esc(report.name) + '</h2>' +
            '<p class="dim" style="margin-top:4px">' + esc(report.id) + ' \u00b7 ' + esc(report.type) +
            ' report \u00b7 ' + esc(report.cycle) + ' cycle \u00b7 audience ' + esc(report.audience) + '</p>' +
            '</div><div>' + healthTag(word) + '</div></div>' +
            '<div class="defbar" style="margin-bottom:18px">' +
            '<div class="item"><span class="k">Party</span><span class="mono">' + esc(report.party) +
            '</span> <span class="faint">' + esc(partyName(report.party)) + '</span></div>' +
            '<div class="item"><span class="k">Schedule</span>' + esc(report.scheduleLabel) +
            (report.schedule ? ' <span class="mono faint">' + esc(report.schedule) + '</span>'
                : ' <span class="tag dashed">on demand</span>') + '</div>' +
            '<div class="item"><span class="k">Owner</span>' + esc(report.owner) + '</div>' +
            '<div class="item"><span class="k">Latest run</span>' +
            (runs.length ? errorCell(shown) + ' <span class="faint">\u00b7 ' + esc(shown.id) + '</span>'
                : '<span class="faint">no run</span>') + '</div>' +
            '</div>' +
            '<header class="sectionhead"><h3 style="font-size:15px">Runs</h3>' +
            '<div class="meta"><span>' + runs.length + ' runs stored \u00b7 newest first</span></div></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Started</th><th>Ended</th><th>Kind</th><th>Run</th><th>Duration</th><th>Health</th><th>Errors</th>' +
            '</tr></thead><tbody>' + runRows + '</tbody></table></div>' +
            (shown ? '<header class="sectionhead" style="margin-top:22px"><h3 style="font-size:15px">Errors of ' +
                esc(clockOf(shown.started)) + ' run</h3>' +
                '<div class="meta">' + scopeChips(shown) + '</div></header>' +
                errorList(shown.errors)
                : '') +
            '<p class="rowfoot">The health word is a door, not a verdict. A completed run that carries errors ' +
            'still wrote its report; the errors name the trade, the curve, the analytic or the configuration ' +
            'that did not resolve.</p>' +
            '</section>';
    }

    function pageHead() {
        var variant = activeVariant();
        return '<header class="pagehead"><div>' +
            '<h1>Reporting \u00b7 ' + (S.variant === 'A' ? 'reports this tenant runs' : S.variant === 'B' ? 'run ledger' : 'what needs a human') + '</h1>' +
            '<p class="lede">' + esc(variant.lede) + '</p>' +
            '</div><div class="actions">' +
            '<span class="updated mono">Now 17 Oct 08:43:12</span>' +
            '<a class="btn linklike" href="../index.org">Prototype index</a>' +
            '</div></header>';
    }

    function partyHeader() {
        return '<div class="contextbar">' +
            '<span class="who">Tenant ACCO \u00b7 ' +
            (S.party === 'all' ? 'all four parties' : 'party ' + esc(S.party)) + '</span>' +
            '<span class="hint">Signed in as tenant administrator. A party user reads their own party by ' +
            'row-level security, not a different screen.</span>' +
            '<span class="tag muted">read-only</span>' +
            '</div>';
    }

    /* ---------------------------------------------------------- variant A */

    function viewA() {
        if (S.drill) return reportDetail();
        var rows = partyReports();
        return partyHeader() +
            '<section class="card"><header class="sectionhead"><h2>Health of the latest run</h2>' +
            '<div class="meta"><span>five words, derived from the latest run of each definition</span></div>' +
            '</header>' + healthSummary(rows) +
            '<p class="rowfoot">A definition that has never run is not clean: nothing ran and nothing was ' +
            'checked. A run in flight outranks an earlier attempt that finished.</p></section>' +
            cataloguePanel();
    }

    /* ---------------------------------------------------------- variant B */

    function ledgerRows() {
        var rows = partyRuns();
        if (S.report !== 'all') {
            rows = rows.filter(function (run) { return run.reportId === S.report; });
        }
        if (S.health !== 'all') {
            rows = rows.filter(function (run) { return runHealth(run) === S.health; });
        }
        return rows;
    }

    function reportFilter() {
        var options = ['<option value="all">All reports (' + REPORTS.length + ')</option>'].concat(
            REPORTS.map(function (report) {
                return '<option value="' + esc(report.id) + '"' +
                    (S.report === report.id ? ' selected' : '') + '>' +
                    esc(report.name) + ' \u00b7 ' + esc(report.party) + '</option>';
            })
        );
        return '<div class="grp"><span class="glbl">Report</span>' +
            '<select data-act="report">' + options.join('') + '</select></div>';
    }

    function runDetailPanel() {
        var run = RUNS.filter(function (candidate) { return candidate.id === S.run; })[0];
        if (!run) return '';
        var report = reportById(run.reportId);
        var word = runHealth(run);
        return '<section class="card">' +
            '<div class="detailhead"><div>' +
            '<a class="back" href="#" data-act="close-run">\u2190 Back to the ledger</a>' +
            '<h2 style="font-size:18px;margin-top:6px">' + esc(report.name) + '</h2>' +
            '<p class="mono dim" style="margin-top:3px">' + esc(run.id) + ' \u00b7 ' + esc(run.kind) +
            ' \u00b7 ' + esc(report.party) + '</p>' +
            '</div><div>' + healthTag(word) + '</div></div>' +
            '<div class="defbar" style="margin-bottom:16px">' +
            '<div class="item"><span class="k">Started</span><span class="mono">' + esc(run.started) + '</span></div>' +
            '<div class="item"><span class="k">Ended</span><span class="mono">' + esc(run.ended || 'still running') + '</span></div>' +
            '<div class="item"><span class="k">Duration</span><span class="mono">' + esc(duration(run)) + '</span></div>' +
            '<div class="item"><span class="k">Errors</span>' + errorCell(run) + '</div>' +
            '</div>' + errorList(run.errors) +
            '<p class="rowfoot">The run wrote its output and still recorded these. They name the scope, ' +
            'the subject and the engine\u2019s message, which is the read the error screen serves.</p>' +
            '</section>';
    }

    function viewB() {
        var rows = ledgerRows();
        var body = rows.length === 0
            ? '<tr><td colspan="8" class="wrap dim">No run matches these filters.</td></tr>'
            : rows.map(function (run) {
                var report = reportById(run.reportId);
                var word = runHealth(run);
                return '<tr class="clickable ' + rowClass(word) + (S.run === run.id ? ' row-open' : '') +
                    '" data-act="open-run" data-run="' + esc(run.id) + '">' +
                    '<td><span class="mono">' + esc(clockOf(run.started)) + '</span> <span class="faint">' +
                    esc(dayOf(run.started)) + '</span></td>' +
                    '<td class="mono">' + esc(report.party) + '</td>' +
                    '<td>' + reportLink(report) + '</td>' +
                    '<td>' + esc(run.kind) + '</td>' +
                    '<td>' + healthTag(word) + '</td>' +
                    '<td class="mono">' + (run.ended ? esc(clockOf(run.ended)) : '\u2014') + '</td>' +
                    '<td class="mono">' + esc(duration(run)) + '</td>' +
                    '<td>' + errorCell(run) + '</td>' +
                    '</tr>';
            }).join('');

        var all = partyRuns();
        var base = S.report === 'all' ? all : all.filter(function (run) {
            return run.reportId === S.report;
        });
        var counts = {'completed clean': 0, 'completed with errors': 0, 'failed': 0, 'running': 0};
        all.forEach(function (run) { counts[runHealth(run)] += 1; });

        return partyHeader() +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Runs</h2>' +
            '<div class="meta"><span>' + rows.length + ' of ' + all.length +
            ' runs \u00b7 newest first \u00b7 report, kind and health as columns</span></div></header>' +
            '<div class="filters" style="margin-bottom:14px">' + partyChips(runCountFor) + reportFilter() +
            healthChips(runHealthCounts(base), base.length) + '</div>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Started</th><th>Party</th><th>Report</th><th>Kind</th><th>Health</th>' +
            '<th>Ended</th><th>Duration</th><th>Errors</th>' +
            '</tr></thead><tbody>' + body + '</tbody></table></div>' +
            '<div class="legend" style="margin-top:12px">' +
            '<span><span class="sw ok"></span>' + counts['completed clean'] + ' completed clean</span>' +
            '<span><span class="sw warn"></span>' + counts['completed with errors'] + ' completed with errors</span>' +
            '<span><span class="sw bad"></span>' + counts.failed + ' failed</span>' +
            '<span>' + counts.running + ' running</span></div>' +
            '<p class="rowfoot">One row per run, newest first. The catalogue is the report filter above, ' +
            'not a page: a reader who knows the report narrows to it, and a reader who does not reads the ' +
            'freshness of the whole tenant. A definition that has never run has no run, so it cannot appear ' +
            'in a ledger; the index-first variant is where that row lives.</p>' +
            '</section>' + runDetailPanel();
    }

    /* ---------------------------------------------------------- variant C */

    function attentionRuns() {
        return REPORTS.map(function (report) {
            return { report: report, run: latestRun(report.id) };
        }).filter(function (entry) {
            var word = runHealth(entry.run);
            return word === 'failed' || word === 'completed with errors';
        }).filter(function (entry) {
            return S.party === 'all' || entry.report.party === S.party;
        }).sort(function (left, right) {
            var leftWord = runHealth(left.run);
            var rightWord = runHealth(right.run);
            var rank = { 'failed': 0, 'completed with errors': 1 };
            if (rank[leftWord] !== rank[rightWord]) return rank[leftWord] - rank[rightWord];
            return left.run.started < right.run.started ? 1 : -1;
        });
    }

    function quietRuns() {
        return REPORTS.map(function (report) {
            return { report: report, run: latestRun(report.id) };
        }).filter(function (entry) {
            var word = runHealth(entry.run);
            return word !== 'failed' && word !== 'completed with errors';
        }).filter(function (entry) {
            return S.party === 'all' || entry.report.party === S.party;
        }).sort(function (left, right) {
            if (!left.run) return 1;
            if (!right.run) return -1;
            return left.run.started < right.run.started ? 1 : -1;
        });
    }

    function attentionRow(entry) {
        var report = entry.report;
        var run = entry.run;
        var word = runHealth(run);
        var first = run.errors[0];
        var why = first
            ? '<p class="why"><span class="lbl">' + (first.severity === 'error' ? 'error' : 'warning') +
              ' \u00b7 ' + esc(first.scope) + '</span> ' + esc(first.message) + '</p>'
            : '<p class="why faint">The run recorded no error row; read the run to see how it ended.</p>';
        return '<tr class="clickable ' + rowClass(word) + '" data-act="inspect" data-report="' + esc(report.id) + '">' +
            '<td><span class="reportname"><span class="nm">' + esc(report.name) +
            '</span><span class="rf mono">' + esc(report.type) + ' \u00b7 ' + esc(report.scheduleLabel) + '</span></span></td>' +
            '<td class="mono">' + esc(report.party) + '</td>' +
            '<td>' + healthTag(word) + '</td>' +
            '<td>' + errorCell(run) + '</td>' +
            '<td>' + (scopeChips(run) || '<span class="faint">\u2014</span>') + '</td>' +
            '<td><span class="mono">' + esc(clockOf(run.started)) + '</span> <span class="faint">' +
            esc(dayOf(run.started)) + '</span></td>' +
            '<td>' + why + '</td>' +
            '</tr>';
    }

    function quietTable() {
        var rows = quietRuns();
        return '<section class="card">' +
            '<header class="sectionhead"><h2>The rest</h2>' +
            '<div class="meta"><span>' + rows.length + ' definitions need nothing from you right now</span></div></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Report</th><th>Party</th><th>Schedule</th><th>Health</th><th>Latest run</th><th>Errors</th>' +
            '</tr></thead><tbody>' + rows.map(function (entry) {
                var word = runHealth(entry.run);
                return '<tr class="' + rowClass(word) + '">' +
                    '<td>' + reportLink(entry.report) + '</td>' +
                    '<td class="mono">' + esc(entry.report.party) + '</td>' +
                    '<td>' + esc(entry.report.scheduleLabel) + '</td>' +
                    '<td>' + healthTag(word) + '</td>' +
                    '<td class="mono">' + (entry.run ? esc(clockOf(entry.run.started)) + ' ' +
                        esc(dayOf(entry.run.started)) : '<span class="faint">\u2014</span>') + '</td>' +
                    '<td>' + errorCell(entry.run) + '</td>' +
                    '</tr>';
            }).join('') + '</tbody></table></div>' +
            '<p class="rowfoot">Shown only because you asked for it. <b>completed clean</b> is the ' +
            'quiet state; <b>never run</b> is not clean, but nothing failed and no errors are waiting.</p>' +
            '</section>';
    }

    function inspectPanel() {
        if (!S.drill) return '';
        var report = reportById(S.drill);
        var run = latestRun(report.id);
        if (!run) return '';
        return '<section class="card">' +
            '<div class="detailhead"><div>' +
            '<a class="back" href="#" data-act="back-to-attention">\u2190 Back to what needs a human</a>' +
            '<h2 style="font-size:18px;margin-top:6px">' + esc(report.name) + '</h2>' +
            '<p class="dim" style="margin-top:3px">' + esc(report.party) + ' \u00b7 latest run ' +
            esc(run.started) + ' \u00b7 ' + esc(run.kind) + '</p>' +
            '</div><div>' + healthTag(runHealth(run)) + '</div></div>' +
            '<header class="sectionhead"><h3 style="font-size:15px">What the run recorded</h3>' +
            '<div class="meta">' + scopeChips(run) + '</div></header>' +
            errorList(run.errors) + '</section>';
    }

    function viewC() {
        var attention = attentionRuns();
        var quiet = quietRuns();
        var failed = attention.filter(function (entry) { return runHealth(entry.run) === 'failed'; }).length;
        var carrying = attention.length - failed;

        return partyHeader() +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Needs a human</h2>' +
            '<div class="meta"><span class="tag bad">' + failed + ' failed</span>' +
            '<span class="tag warn">' + carrying + ' completed with errors</span>' +
            '<span class="faint">latest run of each definition</span></div></header>' +
            '<div class="filters" style="margin-bottom:14px">' + partyChips(reportCountFor) + '</div>' +
            (attention.length === 0
                ? '<p class="dim">Nothing needs a human: no latest run failed and none completed carrying errors.</p>'
                : '<div class="table-wrap"><table><thead><tr>' +
                  '<th>Report</th><th>Party</th><th>Health</th><th>Errors</th><th>Scopes</th>' +
                  '<th>Latest run</th><th>Why, from the run</th>' +
                  '</tr></thead><tbody>' + attention.map(attentionRow).join('') + '</tbody></table></div>') +
            '<p class="rowfoot">A completed run that carries errors is here, beside the failed one, because ' +
            'the report still went out and somebody should know what is missing from it.</p>' +
            '</section>' +
            '<div class="hiddenrow"><span><b>' + quiet.length + ' definitions</b> are quiet: ' +
            quiet.filter(function (e) { return runHealth(e.run) === 'completed clean'; }).length +
            ' completed clean, ' +
            quiet.filter(function (e) { return runHealth(e.run) === 'running'; }).length + ' running, ' +
            quiet.filter(function (e) { return runHealth(e.run) === 'never run'; }).length +
            ' never run. They are hidden because this screen is the short list, not the catalogue.</span>' +
            '<button class="btn" data-act="toggle-clean">' + (S.showClean ? 'Hide them' : 'Show them') + '</button></div>' +
            (S.showClean ? quietTable() : '') +
            inspectPanel();
    }

    /* ---------------------------------------------------------- chrome */

    function amberNote() {
        return '<div class="notice warn"><b>What is fixture, and one thing to unlearn:</b> every number, ' +
            'name and time on this page is mock. Nothing reads a server, and the application has no reporting ' +
            'UI today: the definitions, runs and errors below are invented to show the shape of one. ' +
            '<b>A completed run is not necessarily clean.</b> The engine can finish, write its report, and still ' +
            'record errors \u2014 the health word reads <b>completed with errors</b> and the row counts them, ' +
            'split into errors and warnings, so it never reads as clean.</div>';
    }

    function shell(body) {
        return '<div class="shell">' +
            '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="../index.org">Reporting</a>' +
            '<a href="../index.org" class="here">Reports</a></nav>' +
            '<span class="modechip">Tenant administration</span>' +
            '</div></header>' +
            '<main>' + body + '</main></div>';
    }

    function stateLine() {
        var variant = activeVariant();
        var shown;
        if (S.variant === 'A') {
            if (S.drill) {
                var report = reportById(S.drill);
                shown = 'drilled into ' + report.name + ' \u00b7 ' + runsOf(report.id).length +
                    ' runs \u00b7 latest ' + healthOfReport(report.id);
            } else {
                var rows = partyReports();
                var counts = reportHealthCounts(rows);
                shown = 'the catalogue \u00b7 ' + rows.length + ' definitions \u00b7 ' +
                    (S.party === 'all' ? '4 parties' : S.party) + ' \u00b7 ' +
                    counts['completed with errors'] + ' carrying errors \u00b7 ' +
                    counts.failed + ' failed \u00b7 ' + counts['never run'] + ' never run' +
                    (S.health === 'all' ? '' : ' \u00b7 filtered to ' + S.health);
            }
        } else if (S.variant === 'B') {
            var runs = ledgerRows();
            shown = runs.length + ' runs \u00b7 newest first \u00b7 ' +
                (S.party === 'all' ? 'all parties' : S.party) +
                (S.report === 'all' ? '' : ' \u00b7 report narrowed') +
                (S.health === 'all' ? '' : ' \u00b7 health ' + S.health) +
                (S.run ? ' \u00b7 one run open' : '');
        } else {
            shown = attentionRuns().length + ' runs need a human \u00b7 ' +
                (S.party === 'all' ? 'all parties' : S.party) + ' \u00b7 ' +
                (S.showClean ? 'clean runs shown' : quietRuns().length + ' quiet definitions hidden') +
                (S.drill ? ' \u00b7 one report open' : '');
        }
        return '<div class="bar-line"><span class="label">variant <b>' + esc(variant.name) +
            '</b><span class="sep">\u00b7</span> showing ' + esc(shown) + '</span></div>';
    }

    function renderBar() {
        var variant = activeVariant();
        var buttons = VARIANTS.map(function (candidate) {
            return '<button data-act="variant" data-variant="' + candidate.id + '"' +
                (S.variant === candidate.id ? ' class="on"' : '') + '>' + esc(candidate.name) + '</button>';
        }).join('');
        var bar = '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + buttons +
            '<span class="gist">' + esc(variant.gist) + '</span>' +
            '</div>' + stateLine();
        $('proto-bar').innerHTML = bar;

        $('proto-note').textContent = 'PROTOTYPE \u00b7 mock data, nothing reads a server \u00b7 ' +
            'reporting: see the reports a party runs \u00b7 variant ' + S.variant + ' \u2014 ' +
            (S.variant === 'A' ? 'index-first' : S.variant === 'B' ? 'ledger-first' : 'attention-first') +
            ' \u00b7 ' + (S.variant === 'A'
                ? (S.drill ? 'one report open' : 'the catalogue')
                : S.variant === 'B' ? 'the run ledger' : 'the short list');
    }

    function render() {
        var body;
        if (S.variant === 'B') body = viewB();
        else if (S.variant === 'C') body = viewC();
        else body = viewA();
        $('app').innerHTML = shell(pageHead() + amberNote() + body);
        renderBar();
    }

    /* ---------------------------------------------------------- events */

    document.addEventListener('click', function (event) {
        var target = event.target.closest('[data-act]');
        if (!target) return;
        var act = target.getAttribute('data-act');
        if (act === 'report' || target.tagName === 'OPTION') return;
        event.preventDefault();
        if (act === 'variant') {
            setVariant(target.getAttribute('data-variant'));
        } else if (act === 'party') {
            S.party = target.getAttribute('data-party');
            if (S.drill && reportById(S.drill).party !== S.party && S.party !== 'all') clearDrill();
        } else if (act === 'health') {
            S.health = target.getAttribute('data-health');
        } else if (act === 'open-report') {
            openReport(target.getAttribute('data-report'));
        } else if (act === 'pick-run') {
            S.drillRun = target.getAttribute('data-run');
        } else if (act === 'back-to-catalogue') {
            clearDrill();
        } else if (act === 'open-run') {
            openRun(target.getAttribute('data-run'));
        } else if (act === 'close-run') {
            S.run = null;
        } else if (act === 'toggle-clean') {
            S.showClean = !S.showClean;
        } else if (act === 'inspect') {
            S.drill = target.getAttribute('data-report');
        } else if (act === 'back-to-attention') {
            S.drill = null;
        } else {
            return;
        }
        render();
        writeParams();
    });

    document.addEventListener('change', function (event) {
        var target = event.target.closest('[data-act="report"]');
        if (!target) return;
        S.report = target.value;
        S.run = null;
        render();
        writeParams();
    });

    readParams();
    render();
})();
