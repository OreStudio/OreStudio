/* Reporting prototype: look back over a report's history. Self-contained:
 * plain JavaScript, mock data, no framework, no build step, and nothing that
 * outlives the page.
 *
 * Look back over a report's history, from
 * doc/knowledge/journeys/reporting/journey_look_back_over_a_reports_history.org.
 *
 * The screen answers "what did this report produce over the last few weeks, and
 * is what I am looking at the newest one?" It is the history of one report
 * definition. Every run states when it started and when it completed, the newest
 * run is marked latest, and the last five fire times are shown with the sentence
 * that they are derived from runs because no trigger-event read exists.
 *
 * Three variants disagree about the structure of a history:
 *   A run list   -- a table of runs newest first, the open run read below it.
 *   B timeline   -- the runs down a vertical timeline, the open run expanded in
 *                   place, so the rhythm of the schedule shows.
 *   C comparison -- the latest run beside one earlier run, with what differs
 *                   between them stated.
 *
 * A static page cannot receive an event, so the arrival of a new run is
 * simulated from the bar. The real signal is
 * reporting.v1.report_instances_events.created. */

(function () {
    'use strict';

    /* ---------------------------------------------------------- fixtures */

    var CLOCK = '2026-10-17 08:43:12';
    var CLOCK_AFTER_ARRIVAL = '2026-10-17 09:06:30';

    var REPORT = {
        id: 'rd-saccr-exposure',
        name: 'SA-CCR Exposure',
        party: 'ACMCOR',
        partyName: 'ACCO Capital Markets',
        type: 'risk',
        schedule: '0 6 * * * ?',
        scheduleLabel: 'Hourly at :06',
        owner: 'Tom Aldridge'
    };

    function E(severity, scope, subject, message) {
        return { severity: severity, scope: scope, subject: subject, message: message };
    }

    /* The runs of this one definition, newest first. Fourteen hours of them, so
       the rhythm of an hourly schedule and one window with no run are both
       visible. Between 06:06 and 04:06 there is no run at all. */
    var RUNS = [
        {
            id: 'RUN-20261017-0806',
            started: '2026-10-17 08:06:02',
            ended: '2026-10-17 08:09:41',
            state: 'completed',
            kind: 'full',
            priced: 248,
            errors: []
        },
        {
            id: 'RUN-20261017-0706',
            started: '2026-10-17 07:06:02',
            ended: '2026-10-17 07:11:12',
            state: 'completed',
            kind: 'full',
            priced: 244,
            errors: [
                E('error', 'Trade', 'TRD-0001482', 'The trade has no netting set, so SA-CCR cannot place it.'),
                E('error', 'Trade', 'TRD-0001497', 'The trade has no netting set, so SA-CCR cannot place it.'),
                E('warning', 'Trade', 'TRD-0001503', 'The maturity is beyond the curve, so the trade was priced at the last pillar.'),
                E('error', 'Curve', 'USD-OIS-3M', 'The curve has no pillar at 12 months; the trade was skipped.')
            ]
        },
        {
            id: 'RUN-20261017-0606',
            started: '2026-10-17 06:06:01',
            ended: '2026-10-17 06:08:55',
            state: 'completed',
            kind: 'full',
            priced: 248,
            errors: []
        },
        {
            id: 'RUN-20261017-0406',
            started: '2026-10-17 04:06:02',
            ended: '2026-10-17 04:11:30',
            state: 'failed',
            kind: 'partial',
            priced: 61,
            errors: [
                E('error', 'Report', 'SA-CCR Exposure', 'The engine stopped with status 1 before it wrote the output archive.'),
                E('error', 'Curve', 'EUR-ESTR-6M', 'The curve has a negative pillar and the SA-CCR grid refuses it.')
            ]
        },
        {
            id: 'RUN-20261017-0306',
            started: '2026-10-17 03:06:03',
            ended: '2026-10-17 03:09:12',
            state: 'completed',
            kind: 'full',
            priced: 247,
            errors: []
        },
        {
            id: 'RUN-20261017-0206',
            started: '2026-10-17 02:06:02',
            ended: '2026-10-17 02:08:44',
            state: 'completed',
            kind: 'full',
            priced: 246,
            errors: []
        },
        {
            id: 'RUN-20261017-0106',
            started: '2026-10-17 01:06:02',
            ended: '2026-10-17 01:12:02',
            state: 'completed',
            kind: 'full',
            priced: 240,
            errors: [
                E('error', 'Trade', 'TRD-0001411', 'The trade has no netting set, so SA-CCR cannot place it.'),
                E('error', 'Trade', 'TRD-0001418', 'The trade has no netting set, so SA-CCR cannot place it.'),
                E('error', 'Trade', 'TRD-0001423', 'The trade has no netting set, so SA-CCR cannot place it.'),
                E('warning', 'Analytics', 'saccr', 'The analytic ran without a collateral balance for one netting set.'),
                E('warning', 'Analytics', 'saccr', 'The add-on for one netting set is above the reporting threshold.'),
                E('error', 'Configuration', 'sa-ccr-grid', 'The grid has no row for commodity swaps.')
            ]
        },
        {
            id: 'RUN-20261017-0006',
            started: '2026-10-17 00:06:01',
            ended: '2026-10-17 00:09:31',
            state: 'completed',
            kind: 'full',
            priced: 245,
            errors: []
        },
        {
            id: 'RUN-20261016-2306',
            started: '2026-10-16 23:06:02',
            ended: '2026-10-16 23:08:55',
            state: 'completed',
            kind: 'full',
            priced: 243,
            errors: []
        },
        {
            id: 'RUN-20261016-2206',
            started: '2026-10-16 22:06:02',
            ended: '2026-10-16 22:09:10',
            state: 'failed',
            kind: 'full',
            priced: 0,
            errors: [
                E('error', 'Report', 'SA-CCR Exposure', 'The engine could not read the prepared input and stopped.')
            ]
        },
        {
            id: 'RUN-20261016-2106',
            started: '2026-10-16 21:06:02',
            ended: '2026-10-16 21:08:40',
            state: 'completed',
            kind: 'full',
            priced: 242,
            errors: []
        },
        {
            id: 'RUN-20261016-2006',
            started: '2026-10-16 20:06:03',
            ended: '2026-10-16 20:11:22',
            state: 'completed',
            kind: 'full',
            priced: 238,
            errors: [
                E('error', 'Trade', 'TRD-0001388', 'The trade has no netting set, so SA-CCR cannot place it.'),
                E('warning', 'Curve', 'GBP-SONIA-3M', 'The curve has no pillar at 12 months; the trade was priced at the last one.'),
                E('warning', 'Trade', 'TRD-0001391', 'The maturity is beyond the curve, so the trade was priced at the last pillar.')
            ]
        }
    ];

    /* The arrival the bar simulates: the next hourly window, at 09:06. One only,
       because a static page can simulate one event and the following window
       would meet a run still in flight. */
    var ARRIVAL = {
        id: 'RUN-20261017-0906',
        started: '2026-10-17 09:06:02',
        ended: null,
        state: 'running',
        kind: 'full',
        priced: null,
        errors: []
    };

    var VARIANTS = [
        {
            id: 'A',
            name: 'A \u00b7 run list',
            gist: 'The history is a table of runs, newest first. The latest is pinned and marked, every row states its started and completed time, and the open run is read below the table.',
            lede: 'Every run of this report, newest first. Open any row to read the run below the list.'
        },
        {
            id: 'B',
            name: 'B \u00b7 timeline',
            gist: 'The history is a vertical timeline. The timestamps run down the left, the open run expands in place, and the rhythm of the schedule shows at a glance.',
            lede: 'The same runs down a timeline, so the rhythm of the schedule shows. Open a node to expand the run where it sits.'
        },
        {
            id: 'C',
            name: 'C \u00b7 comparison',
            gist: 'The latest run and one earlier run sit side by side, with the differences between them stated, for the person asking what changed.',
            lede: 'The latest run beside one earlier run, with what differs between them stated.'
        }
    ];

    var HEALTH = {
        'never run': { tag: 'dashed', cls: 'h-never' },
        'running': { tag: 'accent', cls: 'h-run' },
        'completed clean': { tag: 'ok', cls: 'h-ok' },
        'completed with errors': { tag: 'warn', cls: 'h-attn' },
        'failed': { tag: 'bad', cls: 'h-bad' }
    };

    /* ----------------------------------------------------------- helpers */

    function $(id) { return document.getElementById(id); }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    var DAY_NAMES = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
    var MONTH_NAMES = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

    function parts(stamp) {
        var match = /^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})$/.exec(stamp);
        if (!match) return null;
        var year = Number(match[1]);
        var month = Number(match[2]) - 1;
        var day = Number(match[3]);
        return {
            year: year,
            month: month,
            day: day,
            dow: new Date(Date.UTC(year, month, day)).getUTCDay(),
            clock: match[4] + ':' + match[5] + ':' + match[6],
            hours: Number(match[4]),
            minutes: Number(match[5]),
            seconds: Number(match[6])
        };
    }

    function stampMs(stamp) {
        var p = parts(stamp);
        if (!p) return 0;
        return Date.UTC(p.year, p.month, p.day, p.hours, p.minutes, p.seconds);
    }

    function secondsBetween(from, to) {
        return Math.max(0, Math.round((stampMs(to) - stampMs(from)) / 1000));
    }

    function fmtClock(stamp) { return parts(stamp).clock; }

    function fmtDay(stamp) {
        var p = parts(stamp);
        return DAY_NAMES[p.dow] + ' ' + p.day + ' ' + MONTH_NAMES[p.month];
    }

    function fmtDayTime(stamp) { return fmtDay(stamp) + ' \u00b7 ' + fmtClock(stamp); }

    function fmtFull(stamp) {
        var p = parts(stamp);
        return fmtDay(stamp) + ' ' + p.year + ' \u00b7 ' + p.clock;
    }

    function fmtDuration(seconds) {
        if (seconds < 60) return seconds + 's';
        var minutes = Math.floor(seconds / 60);
        var rest = seconds % 60;
        if (minutes < 60) return minutes + 'm ' + (rest < 10 ? '0' + rest : rest) + 's';
        var hours = Math.floor(minutes / 60);
        return hours + 'h ' + (minutes % 60 < 10 ? '0' + (minutes % 60) : minutes % 60) + 'm';
    }

    function describeSpace(seconds) {
        if (seconds >= 86400 && seconds % 86400 === 0) {
            return (seconds / 86400) + (seconds === 86400 ? ' day' : ' days');
        }
        if (seconds >= 3600 && seconds % 3600 === 0) {
            return (seconds / 3600) + (seconds === 3600 ? ' hour' : ' hours');
        }
        return fmtDuration(seconds);
    }

    function clock() { return S.arrived ? CLOCK_AFTER_ARRIVAL : CLOCK; }

    function newestFirst(left, right) {
        if (left.started === right.started) return left.id < right.id ? 1 : -1;
        return left.started < right.started ? 1 : -1;
    }

    function allRuns() {
        var list = RUNS.slice();
        if (S.committed) list.push(ARRIVAL);
        return list.sort(newestFirst);
    }

    function runById(id) {
        return allRuns().filter(function (run) { return run.id === id; })[0] || null;
    }

    function latestRun() { return allRuns()[0]; }

    function pendingArrival() {
        return S.arrived && !S.committed ? ARRIVAL : null;
    }

    function health(run) {
        if (!run) return 'never run';
        if (run.state === 'running') return 'running';
        if (run.state === 'failed') return 'failed';
        return run.errors.length > 0 ? 'completed with errors' : 'completed clean';
    }

    function healthTag(run) {
        var word = health(run);
        return '<span class="tag ' + HEALTH[word].tag + '">' + esc(word) + '</span>';
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

    function elapsedSeconds(run) {
        if (run.ended) return secondsBetween(run.started, run.ended);
        return secondsBetween(run.started, clock());
    }

    function duration(run) {
        if (!run.ended) return fmtDuration(elapsedSeconds(run)) + ' so far';
        return fmtDuration(elapsedSeconds(run));
    }

    function outputMessage(run) {
        if (run.state === 'failed') return 'The run failed before it wrote an output archive. There is nothing to read.';
        if (run.state === 'running') return 'The run is still going. It has not written its output yet.';
        var skipped = run.errors.length;
        return 'SA-CCR exposure written. ' + run.priced + ' trades priced, ' + skipped +
            (skipped === 1 ? ' trade skipped.' : ' trades skipped.');
    }

    function isNew(run) { return S.newIds.indexOf(run.id) >= 0; }

    function newTag(run) {
        return isNew(run) ? ' <span class="tag accent">new</span>' : '';
    }

    function tightestSpacing(runs) {
        var min = Infinity;
        for (var index = 1; index < runs.length; index += 1) {
            var seconds = secondsBetween(runs[index].started, runs[index - 1].started);
            if (seconds < min) min = seconds;
        }
        return min === Infinity ? 0 : min;
    }

    /* A window with no run: two runs further apart than the tightest spacing of
       the set. The screen cannot say why the window is empty. */
    function spacingGaps(runs) {
        var min = tightestSpacing(runs);
        var found = [];
        for (var index = 1; index < runs.length; index += 1) {
            var seconds = secondsBetween(runs[index].started, runs[index - 1].started);
            if (min > 0 && seconds > min * 1.5) found.push({ index: index, seconds: seconds });
        }
        return found;
    }

    function errorCell(run) {
        var counts = severityCounts(run);
        if (!counts.total) return '<span class="faint">\u2014</span>';
        var bits = [];
        if (counts.error) {
            bits.push('<span class="n-err">' + counts.error + '</span> error' + (counts.error === 1 ? '' : 's'));
        }
        if (counts.warning) {
            bits.push('<span class="n-warn">' + counts.warning + '</span> warning' + (counts.warning === 1 ? '' : 's'));
        }
        return '<span class="errcell">' + bits.join(' \u00b7 ') + '</span>';
    }

    function fact(label, value) {
        return '<div><span class="lbl">' + esc(label) + '</span><span class="val">' + value + '</span></div>';
    }

    /* ------------------------------------------------------------- state */

    var S = {
        variant: 'A',
        open: null,
        arrived: false,
        committed: false,
        dismissed: false,
        newIds: [],
        markOn: true,
        markToken: 0
    };

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var variant = (params.get('variant') || 'A').toUpperCase();
        S.variant = variant === 'B' || variant === 'C' ? variant : 'A';
        if (params.get('arrived') === '1') S.arrived = true;
        if (params.get('mark') === '0') S.markOn = false;
        var open = params.get('open');
        S.open = open && RUNS.some(function (run) { return run.id === open; })
            ? open
            : RUNS[0].id;
    }

    function writeParams() {
        var params = new URLSearchParams(window.location.search);
        params.set('variant', S.variant);
        params.set('open', S.open);
        if (S.committed) params.delete('arrived');
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    function activeVariant() {
        return VARIANTS.filter(function (candidate) { return candidate.id === S.variant; })[0];
    }

    function setVariant(id) {
        if (S.variant === id) return;
        S.variant = id;
    }

    /* The person reloads the history. Rows that were not in the previous load are
       marked for three seconds, unless the person turned the marking off. */
    function reloadHistory() {
        var before = {};
        allRuns().forEach(function (run) { before[run.id] = true; });
        S.committed = true;
        S.dismissed = false;
        var after = allRuns();
        S.newIds = S.markOn
            ? after.filter(function (run) { return !before[run.id]; })
                .map(function (run) { return run.id; })
            : [];
        if (!S.newIds.length) return;
        S.markToken += 1;
        var token = S.markToken;
        window.setTimeout(function () {
            if (S.markToken !== token) return;
            S.newIds = [];
            render();
        }, 3000);
    }

    function openLatestRun() {
        if (!S.committed) reloadHistory();
        S.open = latestRun().id;
    }

    /* -------------------------------------------------------------- page */

    function pageHead() {
        return '<div class="pagehead"><div>' +
            '<div class="crumbs"><a href="../index.org">Reporting</a><span>\u203a</span>' +
            '<span>' + esc(REPORT.name) + '</span><span>\u203a</span><span>History</span></div>' +
            '<h1>' + esc(REPORT.name) + '</h1>' +
            '<p class="lede">' + esc(activeVariant().lede) + '</p>' +
            '</div>' +
            '<div class="updated">' + esc(fmtFull(clock())) +
            '<br>the fixture clock</div></div>';
    }

    function definitionBar() {
        return '<div class="contextbar">' +
            '<span class="who">' + esc(REPORT.party) + ' \u00b7 ' + esc(REPORT.partyName) + '</span>' +
            '<span class="tag">' + esc(REPORT.type) + '</span>' +
            '<span class="tag mono">' + esc(REPORT.schedule) + '</span>' +
            '<span class="tag">' + esc(REPORT.scheduleLabel) + '</span>' +
            '<span class="tag muted">owner ' + esc(REPORT.owner) + '</span>' +
            '<span class="hint">This is the history of one definition. There is no list of runs across the tenant, and this screen does not link to one.</span>' +
            '</div>';
    }

    function amberNote() {
        return '<div class="notice warn"><b>What is fixture, and what is simulated:</b> ' +
            'the report, its runs, their times and their errors are all mock. Nothing reads a server, and ' +
            'the application has no reporting UI today. <b>The arrival of a new run is simulated</b> by the ' +
            'button in the bar, because a static page cannot receive an event. The real signal is ' +
            '<span class="mono">reporting.v1.report_instances_events.created</span>, which the screen ' +
            'subscribes to and does not poll. <b>The five fire times are derived from runs</b> because no ' +
            'read of <span class="mono">report_trigger_event</span> exists.</div>';
    }

    function fireStrip() {
        var runs = allRuns();
        var five = runs.slice(0, 5);
        var gaps = spacingGaps(five);
        var chips = [];

        five.forEach(function (run, index) {
            var gap = gaps.filter(function (candidate) { return candidate.index === index; })[0];
            if (gap) {
                chips.push('<span class="gapmark">no run \u00b7 ' + esc(describeSpace(gap.seconds)) +
                    '<br>' + esc(fmtClock(runs[index].started)) + ' to ' +
                    esc(fmtClock(runs[index - 1].started)) + '</span>');
            }
            chips.push('<div class="fire' + (index === 0 ? ' first' : '') + '">' +
                '<span class="fd">' + esc(fmtDay(run.started)) + '</span>' +
                '<span class="ft">' + esc(fmtClock(run.started)) + '</span></div>');
        });

        var gapNote = '';
        if (gaps.length) {
            var first = gaps[0];
            gapNote = '<p class="firenote">The runs are ' + esc(describeSpace(tightestSpacing(five))) +
                ' apart, except one window: between ' +
                esc(fmtDayTime(five[first.index].started)) + ' and ' +
                esc(fmtDayTime(five[first.index - 1].started)) + '. No run started in it.</p>';
        }

        return '<section class="card">' +
            '<div class="sectionhead"><h2>Previous fire times</h2>' +
            '<div class="meta"><span>the last five \u00b7 derived from runs</span>' +
            '<span class="tag mono">' + esc(REPORT.scheduleLabel) + '</span></div></div>' +
            '<div class="fires">' + chips.join('') + '</div>' +
            gapNote +
            '<p class="rule"><b>These five are the runs that exist, not the firings.</b> ' +
            'There is no read of <span class="mono">report_trigger_event</span> over NATS, so a firing ' +
            'that produced no run, because the concurrency policy skipped it or the trigger was refused, ' +
            'is invisible here. The screen cannot tell a quiet window from a firing that produced nothing.</p>' +
            '</section>';
    }

    function arrivalNotice() {
        var pending = pendingArrival();
        if (!pending || S.dismissed) return '';
        var open = runById(S.open);
        var openWasLatest = open && open.id === latestRun().id;
        var lead = openWasLatest
            ? '<b>The run you have open is no longer the latest.</b> '
            : '<b>A new run of this report started.</b> ';
        return '<div class="arrival" role="status" aria-live="polite">' +
            '<div class="what">' + lead +
            'A new run of ' + esc(REPORT.name) + ' started at ' +
            '<span class="when">' + esc(pending.started) + '</span>. ' +
            'This page has not moved, and nothing was opened for you.</div>' +
            '<div class="acts">' +
            '<button class="btn small" data-act="open-latest">Open the latest run</button>' +
            '<button class="btn ghost small" data-act="dismiss">Dismiss</button>' +
            '</div></div>';
    }

    function historyPanel() {
        var runs = allRuns();
        var pending = pendingArrival();
        return '<section class="card">' +
            '<div class="sectionhead"><h2>Runs</h2><div class="meta">' +
            '<span class="runmeta">' + runs.length + ' runs \u00b7 newest first</span>' +
            '<button class="btn refresh' + (pending ? ' stale' : '') + '" data-act="refresh">' +
            (pending ? '<span class="dot"></span>Refresh \u00b7 1 new' : 'Refresh') + '</button>' +
            '<label class="mele"><input type="checkbox" data-act="mark"' +
            (S.markOn ? ' checked' : '') + '> Mark new rows for 3 s</label>' +
            '<span class="tag">watching run events</span>' +
            '</div></div>' +
            '<p class="rule tight">The newest run is the run that started last. Every row states when ' +
            'the run started and when it completed.</p>' +
            (pending ? '<div class="notice spaced">A new run arrived at ' +
                '<span class="mono">' + esc(fmtDayTime(pending.started)) + '</span>. ' +
                'Refresh brings it into the list.</div>' : '') +
            variantBody() +
            '</section>';
    }

    /* ----------------------------------------------------------- run body */

    function runDetail(run) {
        var latest = latestRun();
        var counts = severityCounts(run);
        var notLatest = run.id === latest.id ? '' :
            '<div class="notlatest">This is not the latest run. The latest run started at ' +
            '<span class="mono">' + esc(latest.started) + '</span>.' +
            '<button class="btn small" data-act="open-latest">Open the latest run</button></div>';

        var errors = counts.total
            ? '<ul class="errlist">' + run.errors.map(function (error) {
                return '<li class="sev-' + esc(error.severity) + '">' +
                    '<div class="eline">' +
                    '<span class="tag ' + (error.severity === 'error' ? 'bad' : 'warn') + '">' +
                    esc(error.severity) + '</span>' +
                    '<span class="tag muted">' + esc(error.scope) + '</span>' +
                    '<span class="subj">' + esc(error.subject) + '</span></div>' +
                    '<div class="msg">' + esc(error.message) + '</div></li>';
            }).join('') + '</ul>'
            : '<p class="empty">This run recorded no error.</p>';

        return '<div class="runbody">' +
            '<div class="runhead">' +
            '<div><span class="lbl">Run</span><span class="rid">' + esc(run.id) + '</span></div>' +
            healthTag(run) + newTag(run) +
            '</div>' +
            notLatest +
            '<div class="facts">' +
            fact('Started', esc(fmtFull(run.started))) +
            fact('Completed', run.ended ? esc(fmtFull(run.ended)) : '<span class="faint">\u2014</span>') +
            fact('Duration', esc(duration(run))) +
            fact('Kind', esc(run.kind + ' run')) +
            fact('Trades priced', typeof run.priced === 'number' ? esc(String(run.priced)) : '<span class="faint">\u2014</span>') +
            fact('Errors', counts.total ? esc(String(counts.total)) : 'none') +
            '</div>' +
            '<p class="output">' + esc(outputMessage(run)) + '</p>' +
            errors +
            '<div class="rowacts">' +
            '<span class="elsewhere" title="a separate journey, not drawn in this prototype">Open what the run produced</span>' +
            '<span class="elsewhere" title="a separate journey, not drawn in this prototype">Open the run\u2019s errors</span>' +
            '</div></div>';
    }

    function runPanel() {
        var run = runById(S.open);
        if (!run) return '';
        return '<div class="runpanel">' + runDetail(run) + '</div>';
    }

    /* ---------------------------------------------------------- variant A */

    function viewA() {
        var latest = latestRun();
        var rows = allRuns().map(function (run) {
            var word = health(run);
            var tone = word === 'failed' ? ' row-fail'
                : word === 'completed with errors' ? ' row-attn'
                    : word === 'running' ? ' row-running' : '';
            return '<tr class="clickable' + (S.open === run.id ? ' row-open' : '') + tone +
                (isNew(run) ? ' row-new' : '') + '" data-act="open" data-run="' + esc(run.id) + '"' +
                ' aria-current="' + (S.open === run.id ? 'true' : 'false') + '">' +
                '<td>' + (run.id === latest.id ? '<span class="badge-latest">latest</span>' : '') + '</td>' +
                '<td class="nowrap">' + esc(fmtDayTime(run.started)) + newTag(run) + '</td>' +
                '<td class="nowrap">' + (run.ended ? esc(fmtDayTime(run.ended)) : '<span class="faint">\u2014</span>') + '</td>' +
                '<td><span class="tag ' + HEALTH[word].tag + '">' + esc(word) + '</span></td>' +
                '<td>' + errorCell(run) + '</td>' +
                '<td class="num">' + esc(duration(run)) + '</td>' +
                '<td class="code">' + esc(run.id) + '</td>' +
                '</tr>';
        }).join('');

        return '<div class="table-wrap"><table><thead><tr>' +
            '<th>Newest</th><th>Started</th><th>Completed</th><th>Health</th>' +
            '<th>Errors</th><th class="num">Duration</th><th>Run</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            runPanel();
    }

    /* ---------------------------------------------------------- variant B */

    function viewB() {
        var runs = allRuns();
        var latest = latestRun();
        var min = tightestSpacing(runs);
        var gaps = spacingGaps(runs);
        var pieces = [];
        var lastDay = null;

        runs.forEach(function (run, index) {
            var word = health(run);
            var day = fmtDay(run.started);
            if (day !== lastDay) {
                pieces.push('<li class="tl-day">' + esc(day) + '</li>');
                lastDay = day;
            }
            var gap = gaps.filter(function (candidate) { return candidate.index === index; })[0];
            if (gap) {
                pieces.push('<li class="tl-gap">No run between ' +
                    esc(fmtDayTime(runs[index].started)) + ' and ' +
                    esc(fmtDayTime(runs[index - 1].started)) + '. The runs are ' +
                    esc(describeSpace(min)) + ' apart; this window has none.</li>');
            }
            pieces.push('<li class="tl-item ' + HEALTH[word].cls +
                (S.open === run.id ? ' open' : '') + (isNew(run) ? ' is-new' : '') + '">' +
                '<button class="tl-node" data-act="open" data-run="' + esc(run.id) + '">' +
                '<span class="tl-time">' + esc(fmtDayTime(run.started)) +
                '<br><span class="arr">\u2192</span> <span class="end">' +
                (run.ended ? esc(fmtClock(run.ended)) : 'not completed') + '</span></span>' +
                '<span class="tl-body"><span class="tl-line">' +
                '<span class="tag ' + HEALTH[word].tag + '">' + esc(word) + '</span>' +
                (run.id === latest.id ? '<span class="badge-latest">latest</span>' : '') +
                newTag(run) +
                '</span><span class="tl-meta">' + esc(duration(run)) + ' \u00b7 ' +
                (typeof run.priced === 'number' ? esc(String(run.priced)) : '\u2014') +
                ' trades priced \u00b7 ' +
                (run.errors.length
                    ? esc(String(run.errors.length)) + (run.errors.length === 1 ? ' error' : ' errors')
                    : 'no errors') +
                '</span></span></button>' +
                (S.open === run.id ? '<div class="tl-detail">' + runDetail(run) + '</div>' : '') +
                '</li>');
        });

        return '<ol class="tl">' + pieces.join('') + '</ol>';
    }

    /* ---------------------------------------------------------- variant C */

    function comparedRun() {
        var runs = allRuns();
        var open = runById(S.open);
        if (!open) return runs[1] || runs[0];
        if (open.id !== runs[0].id) return open;
        return runs[1] || runs[0];
    }

    function differences(earlier, latest) {
        var rows = [];
        var earlierWord = health(earlier);
        var latestWord = health(latest);
        rows.push(['Health', earlierWord === latestWord
            ? 'the same both times: ' + earlierWord
            : earlierWord + ' \u2192 ' + latestWord]);

        var earlierErrors = severityCounts(earlier).total;
        var latestErrors = severityCounts(latest).total;
        rows.push(['Errors', earlierErrors === latestErrors
            ? 'the same both times: ' + earlierErrors
            : earlierErrors + ' \u2192 ' + latestErrors]);

        var earlierSeconds = elapsedSeconds(earlier);
        var latestSeconds = elapsedSeconds(latest);
        rows.push(['Duration', duration(earlier) + ' \u2192 ' + duration(latest) + ' (' +
            (earlierSeconds === latestSeconds
                ? 'no change'
                : earlierSeconds > latestSeconds
                    ? fmtDuration(earlierSeconds - latestSeconds) + ' faster'
                    : fmtDuration(latestSeconds - earlierSeconds) + ' slower') + ')']);

        rows.push(['Trades priced',
            (typeof earlier.priced === 'number' ? earlier.priced : '\u2014') + ' \u2192 ' +
            (typeof latest.priced === 'number' ? latest.priced : '\u2014')]);

        rows.push(['Run kind', earlier.kind === latest.kind
            ? 'the same both times: ' + earlier.kind
            : earlier.kind + ' \u2192 ' + latest.kind]);

        rows.push(['Started', fmtDayTime(earlier.started) + ' \u2192 ' + fmtDayTime(latest.started) +
            ' (' + describeSpace(secondsBetween(earlier.started, latest.started)) + ' apart)']);

        rows.push(['Output', outputMessage(earlier) === outputMessage(latest)
            ? 'the same message both times'
            : '\u201c' + outputMessage(earlier) + '\u201d \u2192 \u201c' + outputMessage(latest) + '\u201d']);

        return rows;
    }

    function comparisonCard(run, kind) {
        var latest = latestRun();
        return '<article class="cmp-card' + (kind === 'latest' ? ' latest' : '') +
            (isNew(run) ? ' is-new' : '') + '">' +
            '<header><span class="headlbl">' + esc(kind === 'latest'
                ? 'The latest run' : 'The earlier run') + '</span>' +
            (run.id === latest.id ? '<span class="badge-latest">latest</span>' : '') +
            (isNew(run) ? '<span class="tag accent">new</span>' : '') + '</header>' +
            '<div class="when">' + esc(run.id) + '</div>' +
            '<div class="kv">' +
            '<span class="k">Started</span><span class="v">' + esc(fmtFull(run.started)) + '</span>' +
            '<span class="k">Completed</span><span class="v">' +
            (run.ended ? esc(fmtFull(run.ended)) : '\u2014') + '</span>' +
            '<span class="k">Health</span><span class="v">' + healthTag(run) + '</span>' +
            '<span class="k">Errors</span><span class="v">' + errorCell(run) + '</span>' +
            '<span class="k">Duration</span><span class="v">' + esc(duration(run)) + '</span>' +
            '<span class="k">Trades priced</span><span class="v">' +
            (typeof run.priced === 'number' ? esc(String(run.priced)) : '\u2014') + '</span>' +
            '<span class="k">Kind</span><span class="v">' + esc(run.kind + ' run') + '</span>' +
            '</div>' +
            '<p class="output">' + esc(outputMessage(run)) + '</p>' +
            '</article>';
    }

    function viewC() {
        var runs = allRuns();
        var latest = runs[0];
        var chosen = comparedRun();
        var fallback = chosen.id !== S.open;

        var picks = runs.map(function (run) {
            return '<li><button class="pick' + (S.open === run.id ? ' on' : '') +
                (isNew(run) ? ' is-new' : '') + '" data-act="open" data-run="' + esc(run.id) + '">' +
                '<span class="when">' + esc(fmtDayTime(run.started)) + '</span>' +
                '<span class="tag ' + HEALTH[health(run)].tag + '">' + esc(health(run)) + '</span>' +
                (run.id === latest.id ? '<span class="badge-latest">latest</span>' : '') +
                '</button></li>';
        }).join('');

        var hint = fallback
            ? '<p class="rule">You opened the latest run, which is the left-hand card. The right-hand ' +
            'card shows the run before it. Press any run in the list to compare that one instead.</p>'
            : '';

        var diffRows = differences(chosen, latest).map(function (pair) {
            return '<li><b>' + esc(pair[0]) + '</b><span>' + pair[1] + '</span></li>';
        }).join('');

        return '<div class="cmp">' +
            '<aside class="cmp-picker"><div class="lbl">Choose a run</div>' +
            '<ul class="picker">' + picks + '</ul></aside>' +
            '<div><div class="cmp-cards">' +
            comparisonCard(latest, 'latest') +
            comparisonCard(chosen, 'earlier') +
            '</div>' + hint +
            '<div class="diffs"><h3>What differs, from the earlier run to the latest</h3>' +
            '<ul>' + diffRows + '</ul></div></div>' +
            '</div>';
    }

    function variantBody() {
        if (S.variant === 'B') return viewB();
        if (S.variant === 'C') return viewC();
        return viewA();
    }

    /* ------------------------------------------------------------- chrome */

    function shell(body) {
        return '<div class="shell">' +
            '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="../index.org">Reporting</a>' +
            '<a href="../index.org">Reports</a>' +
            '<a href="../index.org" class="here">History</a></nav>' +
            '<span class="modechip">Trading desk</span>' +
            '</div></header>' +
            '<main>' + body + '</main></div>';
    }

    function stateLine() {
        var open = runById(S.open);
        var latest = latestRun();
        var bits = [
            open
                ? 'open ' + fmtDayTime(open.started) + (open.id === latest.id ? ' (the latest)' : ' (not the latest)')
                : 'no run open',
            allRuns().length + ' runs',
            pendingArrival() ? 'a new run has arrived and is not shown until refresh' : 'no new run',
            S.markOn ? 'new rows marked for 3 s' : 'new rows not marked'
        ];
        return '<div class="bar-line"><span class="label">variant <b>' +
            esc(activeVariant().name) + '</b><span class="sep">\u00b7</span> ' +
            esc(bits.join(' \u00b7 ')) + '</span></div>';
    }

    function renderBar() {
        var variant = activeVariant();
        var buttons = VARIANTS.map(function (candidate) {
            return '<button data-act="variant" data-variant="' + candidate.id + '"' +
                (S.variant === candidate.id ? ' class="on"' : '') + '>' +
                esc(candidate.name) + '</button>';
        }).join('');
        var simulate = S.arrived
            ? '<button class="sim" disabled>Arrival already simulated</button>'
            : '<button class="sim" data-act="arrive">Simulate: a new run arrived</button>';
        $('proto-bar').innerHTML = '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + buttons + simulate +
            '<span class="gist">' + esc(variant.gist) + '</span>' +
            '</div>' + stateLine();

        $('proto-note').textContent = 'PROTOTYPE \u00b7 mock data, nothing reads a server \u00b7 ' +
            'reporting: look back over a report\u2019s history \u00b7 variant ' + S.variant + ' \u2014 ' +
            (S.variant === 'A' ? 'run list' : S.variant === 'B' ? 'timeline' : 'comparison') +
            ' \u00b7 the arrival is simulated';
    }

    function render() {
        $('app').innerHTML = shell(
            pageHead() +
            definitionBar() +
            amberNote() +
            fireStrip() +
            arrivalNotice() +
            historyPanel()
        );
        renderBar();
    }

    /* ------------------------------------------------------------- events */

    document.addEventListener('click', function (event) {
        var target = event.target.closest('[data-act]');
        if (!target) return;

        var act = target.getAttribute('data-act');
        if (act === 'mark') return;

        event.preventDefault();

        if (act === 'variant') {
            setVariant(target.getAttribute('data-variant'));
        } else if (act === 'open') {
            S.open = target.getAttribute('data-run');
        } else if (act === 'refresh') {
            reloadHistory();
            render();
            writeParams();
            return;
        } else if (act === 'arrive') {
            S.arrived = true;
            S.dismissed = false;
        } else if (act === 'dismiss') {
            S.dismissed = true;
            render();
            return;
        } else if (act === 'open-latest') {
            openLatestRun();
            render();
            writeParams();
            return;
        } else {
            return;
        }

        render();
        writeParams();
    });

    document.addEventListener('change', function (event) {
        var target = event.target.closest('[data-act="mark"]');
        if (!target) return;
        S.markOn = target.checked;
        render();
    });

    /* --------------------------------------------------------------- boot */

    readParams();
    render();
}());
