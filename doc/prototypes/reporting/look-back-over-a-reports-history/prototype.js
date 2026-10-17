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
       visible. Between 06:06 and 04:06 there is no run at all.

       `exposure` is the run's SA-CCR EAD in USD: the one number the report exists
       to produce, and the series the trend plot draws. It is a fixture like the
       rest of the page, and it is what the archive says once something parses it.
       A run that failed wrote no archive, so it carries no exposure at all rather
       than a zero, and a run that completed carrying errors wrote an exposure over
       the trades it placed, so its point is partial. */
    var RUNS = [
        {
            id: 'RUN-20261017-0806',
            exposure: 2884204118,
            started: '2026-10-17 08:06:02',
            ended: '2026-10-17 08:09:41',
            state: 'completed',
            kind: 'full',
            priced: 248,
            errors: []
        },
        {
            id: 'RUN-20261017-0706',
            exposure: 2861118402,
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
            exposure: 2879402118,
            started: '2026-10-17 06:06:01',
            ended: '2026-10-17 06:08:55',
            state: 'completed',
            kind: 'full',
            priced: 248,
            errors: []
        },
        {
            id: 'RUN-20261017-0406',
            exposure: null,
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
            exposure: 2852884204,
            started: '2026-10-17 03:06:03',
            ended: '2026-10-17 03:09:12',
            state: 'completed',
            kind: 'full',
            priced: 247,
            errors: []
        },
        {
            id: 'RUN-20261017-0206',
            exposure: 2848204118,
            started: '2026-10-17 02:06:02',
            ended: '2026-10-17 02:08:44',
            state: 'completed',
            kind: 'full',
            priced: 246,
            errors: []
        },
        {
            id: 'RUN-20261017-0106',
            exposure: 2831118904,
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
            exposure: 2844882118,
            started: '2026-10-17 00:06:01',
            ended: '2026-10-17 00:09:31',
            state: 'completed',
            kind: 'full',
            priced: 245,
            errors: []
        },
        {
            id: 'RUN-20261016-2306',
            exposure: 2838402884,
            started: '2026-10-16 23:06:02',
            ended: '2026-10-16 23:08:55',
            state: 'completed',
            kind: 'full',
            priced: 243,
            errors: []
        },
        {
            id: 'RUN-20261016-2206',
            exposure: null,
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
            exposure: 2840118204,
            started: '2026-10-16 21:06:02',
            ended: '2026-10-16 21:08:40',
            state: 'completed',
            kind: 'full',
            priced: 242,
            errors: []
        },
        {
            id: 'RUN-20261016-2006',
            exposure: 2824204118,
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
        exposure: null,
        started: '2026-10-17 09:06:02',
        ended: null,
        state: 'running',
        kind: 'full',
        priced: null,
        errors: []
    };

    /* One scenario sits beside the runs, reachable with
       ?scenario=results-with-errors: the latest run that completed carrying
       errors is drawn with what its errors left out, so a reader meets a
       partial figure rather than only reading about one. It arranges the run's
       own fixtures and changes no run: the error rows are the run's own shapes,
       and the positions it did not place are what its errors say were skipped. */
    var SCENARIO = {
        id: 'results-with-errors',
        unplaced: 4,
        errors: [
            E('error', 'Trade', 'TRD-0001512', 'The trade has no netting set, so SA-CCR cannot place it.'),
            E('error', 'Trade', 'TRD-0001526', 'The maturity is beyond the curve, so SA-CCR cannot place it.'),
            E('error', 'Curve', 'USD-OIS-6M', 'The curve has no pillar at 18 months; the trade was skipped.')
        ]
    };

    function scenarioFor(run) {
        return S.scenario && run && run.id === RUNS[0].id ? SCENARIO : null;
    }

    function runErrors(run) {
        var sc = scenarioFor(run);
        return sc ? run.errors.concat(sc.errors) : run.errors;
    }

    function runPriced(run) {
        var sc = scenarioFor(run);
        if (typeof run.priced !== 'number') return run.priced;
        return sc ? Math.max(0, run.priced - sc.unplaced) : run.priced;
    }

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

    /* -------------------------------------------------------------- plots */
    /* The trend plot is inline SVG written here, because a prototype opens from
       file:// with no build, no CDN and no network: a chart library cannot be
       loaded, and none is copied in either. A series across the runs of one
       report is a change over time, so it is a line: the latest run is marked and
       named, the value axis states the range it spans instead of starting
       silently somewhere above zero, and a run that produced no exposure breaks
       the line rather than drawing a zero. A run that completed carrying errors
       wrote an exposure over the trades it placed, so its point is drawn hollow
       with its segment dashed, and the caption says what those errors exclude.
       The exact numbers sit in the table under the plot, so the plot never
       replaces them. */

    function round(n) {
        return Math.round(n * 10) / 10;
    }

    function msStamp(ms) {
        return new Date(ms).toISOString().slice(0, 19).replace('T', ' ');
    }

    function shortMoney(n) {
        var sign = n < 0 ? '-' : '';
        var a = Math.abs(n);
        if (a >= 1e9) return sign + 'USD ' + (a / 1e9).toFixed(2) + ' bn';
        if (a >= 1e6) return sign + 'USD ' + (a / 1e6).toFixed(1) + ' m';
        if (a >= 1e3) return sign + 'USD ' + Math.round(a / 1e3) + ' k';
        return sign + 'USD ' + Math.round(a);
    }

    function fullMoney(n) {
        return (n < 0 ? '-' : '') + 'USD ' + String(Math.abs(n)).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    }

    function lineDomain(points) {
        var values = points.filter(function (point) {
            return typeof point.value === 'number';
        }).map(function (point) { return point.value; });
        var lo = Math.min.apply(null, values);
        var hi = Math.max.apply(null, values);
        var pad = (hi - lo) * 0.22;
        if (!pad) pad = Math.abs(hi || 1) * 0.05;
        return { lo: lo - pad, hi: hi + pad };
    }

    function exposurePoints() {
        var runs = allRuns();
        /* The mark belongs on the newest run that produced a figure. When the
           newest run is in flight it has none, so the newest marked run is the
           one before it and the caption says so rather than naming a point that
           is not on the line. */
        var newestWithFigure = runs.filter(function (run) {
            return typeof run.exposure === 'number';
        })[0] || null;
        return runs.slice().reverse().map(function (run) {
            var partial = run.state === 'completed' && runErrors(run).length > 0;
            var value = typeof run.exposure === 'number' ? run.exposure : null;
            var isLatest = !!newestWithFigure && run.id === newestWithFigure.id;
            var isNewest = run.id === runs[0].id;
            var title;
            if (value === null) {
                title = run.id + ', started ' + run.started + ': no exposure' +
                    (run.state === 'failed' ? ' \u2014 the run failed before it wrote an archive'
                        : ' \u2014 the run has not finished');
            } else {
                title = run.id + ', started ' + run.started + ': ' + fullMoney(value) +
                    (partial ? ' (partial: ' + runErrors(run).length + ' errors)' : '');
            }
            return {
                label: run.id,
                at: stampMs(run.started),
                value: value,
                partial: partial,
                mark: isLatest ? 'latest' : null,
                markLabel: run.id + (isNewest ? ' \u00b7 latest' : ' \u00b7 newest with a figure'),
                title: title
            };
        });
    }

    function exposureChart(points) {
        var W = 1080, left = 110, right = 28, top = 34, plotH = 190, bottom = 36;
        var plotW = W - left - right;
        var H = top + plotH + bottom;
        var axisY = top + plotH;
        var domain = lineDomain(points);
        var lo = domain.lo, hi = domain.hi;
        var times = points.map(function (point) { return point.at; });
        var t0 = Math.min.apply(null, times);
        var t1 = Math.max.apply(null, times);

        function xOf(t) { return left + (t - t0) / (t1 - t0) * plotW; }
        function yOf(v) { return top + (hi - v) / (hi - lo) * plotH; }

        var aria = 'Line: SA-CCR exposure across the runs of this report, in USD. ' +
            points.map(function (point) { return point.title; }).join('; ') +
            '. The value axis runs from ' + shortMoney(lo) + ' to ' + shortMoney(hi) +
            ' and does not start at zero.';
        var out = '<svg class="plot-svg" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(aria) + '">';

        [lo, (lo + hi) / 2, hi].forEach(function (v) {
            out += '<line class="grid" x1="' + left + '" y1="' + round(yOf(v)) + '" x2="' +
                (left + plotW) + '" y2="' + round(yOf(v)) + '"></line>';
            out += '<text class="pl-tick" x="' + (left - 10) + '" y="' + (round(yOf(v)) + 4) +
                '" text-anchor="end">' + esc(shortMoney(v)) + '</text>';
        });
        out += '<line class="axis" x1="' + left + '" y1="' + axisY + '" x2="' + (left + plotW) +
            '" y2="' + axisY + '"></line>';

        points.forEach(function (point) {
            if (typeof point.value === 'number') return;
            out += '<line class="gap" x1="' + round(xOf(point.at)) + '" y1="' + top + '" x2="' +
                round(xOf(point.at)) + '" y2="' + axisY + '"><title>' + esc(point.title) +
                '</title></line>';
        });

        for (var i = 1; i < points.length; i += 1) {
            var a = points[i - 1], b = points[i];
            if (typeof a.value !== 'number' || typeof b.value !== 'number') continue;
            out += '<line class="seg' + (a.partial || b.partial ? ' partial' : '') +
                '" x1="' + round(xOf(a.at)) + '" y1="' + round(yOf(a.value)) + '" x2="' +
                round(xOf(b.at)) + '" y2="' + round(yOf(b.value)) + '"></line>';
        }

        points.forEach(function (point) {
            if (typeof point.value !== 'number') return;
            var cx = round(xOf(point.at));
            var cy = round(yOf(point.value));
            var cls = 'pt ' + (point.partial ? 'partial' : 'pos') + (point.mark ? ' ' + point.mark : '');
            out += '<circle class="' + cls + '" cx="' + cx + '" cy="' + cy + '" r="' +
                (point.partial ? 4.5 : (point.mark ? 5 : 3.5)) + '"><title>' +
                esc(point.title) + '</title></circle>';
            if (!point.mark) return;
            var anchor = cx > left + plotW - 120 ? 'end' : 'middle';
            var tx = anchor === 'end' ? cx + 4 : cx;
            out += '<text class="pl-mark" x="' + round(tx) + '" y="' + round(cy - 12) +
                '" text-anchor="' + anchor + '">' + esc(point.markLabel) + '</text>';
        });

        /* The axis labels the clock, and the day is drawn as a boundary of its own,
           so an hourly run keeps a readable axis instead of a row of colliding
           date stamps. */
        var stride = Math.max(1, Math.ceil(points.length / 12));
        points.forEach(function (point, index) {
            if (index % stride !== 0 && index !== points.length - 1) return;
            var anchor = index === 0 ? 'start'
                : (index === points.length - 1 ? 'end' : 'middle');
            out += '<text class="pl-tick" x="' + round(xOf(point.at)) + '" y="' + (axisY + 16) +
                '" text-anchor="' + anchor + '">' + esc(fmtClock(msStamp(point.at)).slice(0, 5)) + '</text>';
        });

        var daySeen = null;
        points.forEach(function (point) {
            var day = fmtDay(msStamp(point.at));
            if (day === daySeen) return;
            if (daySeen !== null) {
                out += '<line class="daymark" x1="' + round(xOf(point.at)) + '" y1="' + (top - 6) +
                    '" x2="' + round(xOf(point.at)) + '" y2="' + axisY + '"></line>';
            }
            out += '<text class="pl-mark daylbl" x="' + round(xOf(point.at) + (daySeen === null ? 0 : 6)) +
                '" y="' + (top - 14) + '">' + esc(day) + '</text>';
            daySeen = day;
        });

        return out + '</svg>';
    }

    function exposureTable(runs) {
        var rows = runs.map(function (run) {
            var partial = run.state === 'completed' && runErrors(run).length > 0;
            return '<tr>' +
                '<td class="code">' + esc(run.id) + '</td>' +
                '<td class="nowrap">' + esc(fmtDayTime(run.started)) + '</td>' +
                '<td class="num">' + (typeof run.exposure === 'number'
                    ? esc(fullMoney(run.exposure)) +
                      (partial ? ' <span class="tag bad">partial</span>' : '')
                    : '<span class="faint">no figure</span>') + '</td>' +
                '<td><span class="tag ' + HEALTH[health(run)].tag + '">' + esc(health(run)) + '</span></td>' +
                '</tr>';
        }).join('');
        return '<div class="table-wrap"><table class="exposure-table"><thead><tr>' +
            '<th>Run</th><th>Started</th><th class="num">SA-CCR exposure (USD)</th><th>State</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>';
    }

    /* The trend, and the numbers it draws. The card appears in all three variants,
       because a series across runs is what this screen is for, and the variants
       disagree about the list below it and not about the series. */
    function trendCard() {
        var points = exposurePoints();
        var runs = allRuns().slice().reverse();
        var domain = lineDomain(points);
        var partialRuns = runs.filter(function (run) {
            return run.state === 'completed' && runErrors(run).length > 0;
        });
        var missing = runs.filter(function (run) {
            return typeof run.exposure !== 'number';
        });
        var latest = latestRun();
        var marked = points.filter(function (point) { return point.mark === 'latest'; })[0] || null;
        var first = points[0];
        var last = points[points.length - 1];
        var latestHasFigure = typeof latest.exposure === 'number';

        var caption = 'A line, because the runs of one report in order are a change over time: each point is a ' +
            'run, ' + (marked ? marked.label + ' is marked and named' : 'no run has a figure to mark') +
            ', and the value axis runs from ' + shortMoney(domain.lo) +
            ' to ' + shortMoney(domain.hi) + ' and does not start at zero, which the axis states rather than ' +
            'leaving it to the eye. The points run from ' + fmtDayTime(msStamp(first.at)) + ' to ' +
            fmtDayTime(msStamp(last.at)) + '.' +
            (latestHasFigure ? '' : ' The newest run, ' + latest.id + ', has no exposure yet, so it is drawn ' +
                'as a break in the line and not as a point.')

        var excluded = '';
        if (partialRuns.length) {
            excluded = 'The dashed points and their segments are the ' + partialRuns.length + ' run' +
                (partialRuns.length === 1 ? '' : 's') + ' that completed carrying errors: ' +
                partialRuns.map(function (run) { return run.id; }).join(', ') +
                '. Each exposure covers the trades that run placed, and the errors it recorded name what is ' +
                'missing, so each of those points is partial.' +
                (missing.length
                    ? ' ' + missing.length + ' run' + (missing.length === 1 ? '' : 's') +
                      ' produced no exposure at all, and the line breaks at ' +
                      (missing.length === 1 ? 'it' : 'them') +
                      ': a failure is not a value of zero.'
                    : '');
        } else if (missing.length) {
            excluded = 'The line breaks at the ' + missing.length + ' run' +
                (missing.length === 1 ? '' : 's') + ' that produced no exposure: a failure is not a value of zero.';
        }

        return '<section class="card" id="trend">' +
            '<div class="sectionhead"><h2>Exposure across the runs</h2>' +
            '<div class="meta"><span class="tag warn">fixture</span>' +
            '<span>SA-CCR exposure, USD \u00b7 one point per run \u00b7 inline SVG, openable from ' +
            '<span class="mono">file://</span></span></div></div>' +
            '<figure class="plot">' +
            '<div class="plot-head"><span class="plot-title">SA-CCR exposure by run</span>' +
            '<span class="plot-unit">USD, EAD</span>' +
            (partialRuns.length ? '<span class="tag bad">partial</span>' : '') +
            '</div>' + exposureChart(points) +
            '<figcaption class="plot-cap"><span>' + esc(caption) + '</span>' +
            (excluded
                ? '<span class="plot-excl"><span class="tag bad">partial</span><span>' +
                  esc(excluded) + '</span></span>'
                : '') +
            '</figcaption></figure>' +
            exposureTable(runs) +
            '<p class="rule">The numbers stay in text: the table above carries every point\u2019s exact exposure ' +
            'and the state of the run that produced it, and the plot is drawn from the same fixture rows. ' +
            'Nothing reads a server, and no chart library is loaded or copied in: the line is inline SVG ' +
            'written by this page.</p>' +
            '</section>';
    }

    /* ------------------------------------------------ the form of a plot */
    /* The vocabulary is the ordinary one, and the rule that picks a form is
       written down here so the page states it rather than a reader guessing.
       One whole in few parts is a donut; one whole in many parts, or parts
       that must be compared precisely, is horizontal bars; many wholes each
       split the same way are stacked bars; two series over the same
       categories are grouped bars; two measures against each other are a
       scatter; a change over time is a line; a contribution that sums to a
       total is a waterfall; and a handful of exact numbers stays a table. */

    var FORMS = [
        {
            id: 'donut',
            name: 'Donut',
            shape: 'One whole, few parts',
            rule: 'One whole in not more than about six parts becomes a donut. A seventh part and the ones ' +
                'after it join one \'other\' slice, and the hole carries the total. Each part states its ' +
                'percentage and its value in the legend beside the slice, never on it.'
        },
        {
            id: 'bars',
            name: 'Horizontal bars',
            shape: 'One whole, many parts, or exact comparison',
            rule: 'One whole in more than about six parts, or parts that must be compared precisely, is ' +
                'horizontal bars: a pie of fifteen parts is unreadable and its angles cannot be compared.'
        },
        {
            id: 'stacked',
            name: 'Stacked bar',
            shape: 'Many wholes, each split the same way',
            rule: 'More than one whole, each decomposing by the same categories, is a stacked bar: one bar ' +
                'per whole and one segment per part.'
        },
        {
            id: 'grouped',
            name: 'Grouped bar',
            shape: 'Two series over the same categories',
            rule: 'Two series that share the same categories and the same unit are grouped bars, side by ' +
                'side, so two things are compared rather than one stacked on the other.'
        },
        {
            id: 'scatter',
            name: 'Scatter',
            shape: 'Two measures against each other',
            rule: 'Two measures plotted against each other are a scatter, so an outlier is visible as a ' +
                'point away from the rest.'
        },
        {
            id: 'line',
            name: 'Line',
            shape: 'A change over time',
            rule: 'A change over time is a line, and a single point is not a trend.'
        },
        {
            id: 'waterfall',
            name: 'Waterfall',
            shape: 'A contribution that sums to a total',
            rule: 'A contribution that adds up to a total is a waterfall. No figure on this page is a ' +
                'contribution to a known total, so none is drawn as one.'
        },
        {
            id: 'table',
            name: 'Table',
            shape: 'Exact and few',
            rule: 'A handful of exact numbers stays a table, and nothing on this page replaces it.'
        }
    ];

    function formRule() {
        var rows = FORMS.map(function (form) {
            return '<tr><td class="fshape">' + esc(form.shape) + '</td>' +
                '<td class="fname"><span class="fchip f-' + form.id + '"></span>' + esc(form.name) + '</td>' +
                '<td class="frule">' + esc(form.rule) + '</td></tr>';
        }).join('');
        return '<details class="formrule" open>' +
            '<summary>How the form of a plot is chosen</summary>' +
            '<p>Every plot on this page is chosen by the shape of its numbers, and the rule is the same on ' +
            'every screen. A pie is never used where its angles cannot be compared, and no chart replaces a ' +
            'number a reader has come to check.</p>' +
            '<div class="table-wrap"><table class="formtable"><thead><tr>' +
            '<th>The numbers are</th><th>Form</th><th>The rule that chooses it</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="formfoot">Every donut carries its percentages in the legend beside each slice. A plot ' +
            'whose figure the errors left partial keeps the dashed or hollow treatment, and a donut draws the ' +
            'excluded part as a hatched slice of its own so the whole of the pie is not silently smaller than ' +
            'the run.</p>' +
            '</details>';
    }

    /* One colour per scope, the same on every plot of this page, and no token
       spent on anything that is not data. */
    function scopeColour(scope) {
        /* One scope, one colour, on every plot of this page and on the next
           screen that draws the same scope: the map is the scope's own name, not
           its position in a count, so a scope cannot change colour because
           another scope gained a row. */
        var map = {
            Analytics: 'var(--accent-bright)',
            Configuration: 'var(--warn)',
            Curve: 'var(--ink-dim)',
            Report: 'var(--bad)',
            Trade: 'color-mix(in srgb, var(--ok) 78%, var(--bg))'
        };
        return map[scope] || 'var(--ink-dim)';
    }

    /* The population of priced positions: the most any run in the window
       placed. A run that priced fewer left the rest unplaced, and that is the
       part the donut must hatch rather than drop. */
    function population() {
        return allRuns().reduce(function (top, run) {
            return Math.max(top, typeof runPriced(run) === 'number' ? runPriced(run) : 0);
        }, 0);
    }

    function skippedOf(run) {
        if (typeof runPriced(run) !== 'number') return 0;
        return Math.max(0, population() - runPriced(run));
    }

    function runLabel(run) {
        return fmtDay(run.started).replace(/^\w+ /, '') + ' ' + fmtClock(run.started).slice(0, 5);
    }

    function capitalise(text) {
        return text ? text.charAt(0).toUpperCase() + text.slice(1) : text;
    }

    function r1(n) {
        return Math.round(n * 10) / 10;
    }

    function shortCount(n) {
        return String(n);
    }

    function prettyPct(part, whole) {
        if (!whole) return '0.0%';
        return (part / whole * 100).toFixed(1) + '%';
    }

    function hatching() {
        return '<defs><pattern id="hatch" width="6" height="6" patternUnits="userSpaceOnUse" ' +
            'patternTransform="rotate(45)">' +
            '<rect width="6" height="6" fill="var(--bad)" fill-opacity="0.12"></rect>' +
            '<line x1="0" y1="0" x2="0" y2="6" stroke="var(--bad)" stroke-width="1.6" ' +
            'stroke-opacity="0.7"></line></pattern></defs>';
    }

    function arcPath(cx, cy, rOuter, rInner, from, to) {
        var large = (to - from) > Math.PI ? 1 : 0;
        var x0 = cx + rOuter * Math.cos(from), y0 = cy + rOuter * Math.sin(from);
        var x1 = cx + rOuter * Math.cos(to), y1 = cy + rOuter * Math.sin(to);
        var x2 = cx + rInner * Math.cos(to), y2 = cy + rInner * Math.sin(to);
        var x3 = cx + rInner * Math.cos(from), y3 = cy + rInner * Math.sin(from);
        return 'M' + r1(x0) + ' ' + r1(y0) +
            ' A' + rOuter + ' ' + rOuter + ' 0 ' + large + ' 1 ' + r1(x1) + ' ' + r1(y1) +
            ' L' + r1(x2) + ' ' + r1(y2) +
            ' A' + rInner + ' ' + rInner + ' 0 ' + large + ' 0 ' + r1(x3) + ' ' + r1(y3) + ' Z';
    }

    function legend(rows, head) {
        return '<div class="legend">' + (head
            ? '<div class="legendhead"><span>Part</span><span>Value</span><span>Share</span></div>'
            : '') + rows.map(function (row) {
                return '<div class="lrow' + (row.hatched ? ' excl' : '') + '" title="' +
                    esc(row.title || row.label) + '">' +
                    (row.hatched ? '<span class="swatch hatched"></span>'
                        : '<span class="swatch" style="background:' + row.colour + '"></span>') +
                    '<span class="lname">' + esc(row.label) + '</span>' +
                    '<span class="lval">' + esc(row.valueText) + '</span>' +
                    '<span class="lpct">' + esc(row.pctText) + '</span></div>';
            }).join('') + '</div>';
    }

    /* A donut: one whole in a few parts, with the total in the hole and every
       percentage and value in the legend beside the slice. An excluded part is
       a hatched slice of its own, never a pie that is quietly smaller than the
       whole it claims to divide. */
    function donutChart(spec) {
        var slices = spec.slices;
        var W = 340, H = 300, cx = 170, cy = 150, rOuter = 112, rInner = 66;
        var positive = slices.filter(function (slice) { return slice.value > 0; });
        var whole = positive.reduce(function (sum, slice) { return sum + slice.value; }, 0);
        var MIN_EXCLUDED = 4 / 360;
        var sweepTotal = positive.reduce(function (sum, slice) {
            return sum + slice.value / whole;
        }, 0) + slices.filter(function (slice) {
            return slice.excluded && !(slice.value > 0);
        }).length * MIN_EXCLUDED;

        var out = '<svg class="plot-svg plot-donut" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(spec.aria) + '">' + hatching();
        var angle = -Math.PI / 2;
        var rows = [];
        slices.forEach(function (slice) {
            var text = slice.valueText ||
                (spec.valueText ? spec.valueText(slice.value)
                    : (slice.value + (slice.value === 1 ? ' run' : ' runs')));
            var pct = whole > 0 && slice.value > 0 ? prettyPct(slice.value, whole) : null;
            rows.push({
                label: slice.label,
                colour: slice.colour,
                hatched: !!slice.excluded,
                valueText: text,
                pctText: slice.excluded ? 'excluded' : (pct === null ? '0.0%' : pct),
                title: slice.label + ': ' + text + (pct ? ' (' + pct + ')' : '')
            });
            var share = slice.value > 0 ? slice.value / whole : (slice.excluded ? MIN_EXCLUDED : 0);
            if (!(share > 0)) return;
            var to = angle + share / sweepTotal * Math.PI * 2;
            out += '<path class="slice' + (slice.excluded ? ' excluded' : '') + '" d="' +
                arcPath(cx, cy, rOuter, rInner, angle, to) + '" fill="' +
                (slice.excluded ? 'url(#hatch)' : slice.colour) + '" stroke="' +
                (slice.excluded ? 'var(--bad)' : 'var(--bg)') + '" stroke-width="1"><title>' +
                esc(slice.label + ': ' + text + (pct ? ' (' + pct + ')' : '')) + '</title></path>';
            angle = to;
        });
        if (!positive.length) {
            out += '<circle cx="' + cx + '" cy="' + cy + '" r="' + (rOuter + rInner) / 2 +
                '" fill="none" stroke="var(--line-strong)" stroke-dasharray="4 4"></circle>';
        }
        out += '<text class="hole-val" x="' + cx + '" y="' + (cy - 2) + '" text-anchor="middle">' +
            esc(spec.holeValue) + '</text>' +
            '<text class="hole-lbl" x="' + cx + '" y="' + (cy + 17) + '" text-anchor="middle">' +
            esc(spec.holeLabel) + '</text></svg>';
        return '<div class="donut-wrap">' + out + legend(rows, true) + '</div>';
    }

    /* Stacked bars: several wholes, each split by the same categories. */
    function stackedChart(spec) {
        var bars = spec.bars;
        var W = 700, left = 130, right = 26, top = 24, barH = 26, gap = 18, axisH = 30;
        var H = top + bars.length * (barH + gap) + axisH;
        var plotW = W - left - right;
        var highest = Math.max.apply(null, bars.map(function (bar) {
            return bar.segments.reduce(function (sum, segment) { return sum + Math.max(0, segment.value); }, 0);
        }).concat([0]));
        if (highest <= 0) highest = 1;

        function xOf(v) { return left + v / highest * plotW; }

        var out = '<svg class="plot-svg plot-stacked" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(spec.aria) + '">' + hatching();
        out += '<line class="axis" x1="' + left + '" y1="' + (H - axisH + 2) + '" x2="' + (left + plotW) +
            '" y2="' + (H - axisH + 2) + '"></line>';
        [0, highest].forEach(function (v, i) {
            out += '<text class="pl-tick" x="' + r1(xOf(v)) + '" y="' + (H - axisH + 17) +
                '" text-anchor="' + (i === 0 ? 'start' : 'end') + '">' + esc(String(v)) + '</text>';
        });
        bars.forEach(function (bar, i) {
            var y = top + i * (barH + gap);
            var x = left;
            out += '<text class="pl-lbl" x="' + (left - 10) + '" y="' + (y + barH / 2 + 4) +
                '" text-anchor="end">' + esc(bar.label) + '</text>';
            bar.segments.forEach(function (segment) {
                var width = Math.min(plotW - (x - left), segment.value / highest * plotW);
                if (width <= 0.6) return;
                out += '<rect class="sbar" x="' + r1(x) + '" y="' + y + '" width="' + r1(width) +
                    '" height="' + barH + '" fill="' + segment.colour + '" stroke="var(--bg)" ' +
                    'stroke-width="1"><title>' + esc(segment.label + ': ' + segment.value) + '</title></rect>';
                x += width;
            });
        });
        return out + '</svg>';
    }

    function stackedLegend(parts) {
        return '<div class="legend titled"><span class="lhead">The parts</span>' +
            '<div class="legendrow">' + parts.map(function (part) {
                return '<div class="lrow"><span class="swatch" style="background:' + part.colour +
                    '"></span><span class="lname">' + esc(part.label) + '</span></div>';
            }).join('') + '</div></div>';
    }

    /* Grouped bars: two series over the same runs and the same unit. */
    function groupedChart(spec) {
        var rows = spec.categories;
        var W = 820, left = 150, right = 96, top = 20, rowH = 40, axisH = 30;
        var H = top + rows.length * rowH + axisH;
        var plotW = W - left - right;
        var highest = 0;
        rows.forEach(function (row) {
            row.values.forEach(function (value) { highest = Math.max(highest, value); });
        });
        if (highest <= 0) highest = 1;

        function xOf(v) { return left + v / highest * plotW; }

        var out = '<svg class="plot-svg plot-grouped" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(spec.aria) + '">';
        var axisY = H - axisH + 2;
        out += '<line class="axis" x1="' + left + '" y1="' + axisY + '" x2="' + (left + plotW) +
            '" y2="' + axisY + '"></line>';
        [0, highest].forEach(function (v, i) {
            out += '<text class="pl-tick" x="' + r1(xOf(v)) + '" y="' + (H - axisH + 17) +
                '" text-anchor="' + (i === 0 ? 'start' : 'end') + '">' + esc(String(v)) + '</text>';
        });
        rows.forEach(function (row, i) {
            var y = top + i * rowH;
            out += '<text class="pl-lbl" x="' + (left - 10) + '" y="' + (y + rowH / 2 + 8) +
                '" text-anchor="end">' + esc(row.label) + '</text>';
            row.values.forEach(function (value, j) {
                var barH = 15;
                var by = y + 3 + j * (barH + 2);
                out += '<rect class="gbar ' + spec.series[j].id + (spec.series[j].partial ? ' partial' : '') +
                    '" x="' + left + '" y="' + by + '" width="' + r1(Math.max(2, xOf(value) - left)) +
                    '" height="' + barH + '" fill="' + spec.series[j].colour + '" rx="2"><title>' +
                    esc(row.label + ' \u00b7 ' + spec.series[j].label + ': ' + value) + '</title></rect>';
                out += '<text class="pl-val" x="' + r1(Math.min(xOf(value) + 6, W - right + 4)) + '" y="' +
                    (by + barH - 3) + '">' + esc(String(value)) + '</text>';
            });
        });
        out += '</svg>';
        return out + '<div class="legend titled"><span class="lhead">The two series</span>' +
            '<div class="legendrow">' + spec.series.map(function (s) {
                return '<div class="lrow' + (s.partial ? ' excl' : '') + '">' +
                    '<span class="swatch" style="background:' + s.colour + '"></span>' +
                    '<span class="lname">' + esc(s.label) + (s.partial ? ' (partial)' : '') + '</span></div>';
            }).join('') + '</div></div>';
    }

    /* A scatter: two measures against each other, on log axes because trades
       and money span orders of magnitude, with a label placed where it collides
       with nothing. */
    function scatterChart(spec) {
        var points = spec.points;
        var W = 820, left = 104, right = 40, top = 24, plotH = 250, bottom = 58;
        var H = top + plotH + bottom;
        var plotW = W - left - right;
        var xs = points.map(function (p) { return p.x; });
        var ys = points.map(function (p) { return p.y; });
        /* Both axes are plain magnitudes, so both start at zero and each states
           its own range: a truncated axis would make a small difference look
           large, which is the one thing a scatter must not do. */
        function domain(values) {
            var hi = Math.max.apply(null, values.concat([0]));
            var pad = hi * 0.08;
            return { lo: 0, hi: hi ? hi + pad : 1 };
        }
        var xd = domain(xs);
        var yd = domain(ys);
        function xOf(v) { return left + (v - xd.lo) / (xd.hi - xd.lo) * plotW; }
        function yOf(v) { return top + (yd.hi - v) / (yd.hi - yd.lo) * plotH; }

        var out = '<svg class="plot-svg plot-scatter" viewBox="0 0 ' + W + ' ' + H + '" role="img" ' +
            'aria-label="' + esc(spec.aria) + '">';
        [0, yd.hi / 2, yd.hi].forEach(function (v, i) {
            out += '<line class="grid" x1="' + left + '" y1="' + r1(yOf(v)) + '" x2="' + (left + plotW) +
                '" y2="' + r1(yOf(v)) + '"></line>';
            out += '<text class="pl-tick" x="' + (left - 10) + '" y="' + (r1(yOf(v)) + 4) +
                '" text-anchor="end">' + esc(spec.tickY(v)) + '</text>';
        });
        [0, xd.hi / 2, xd.hi].forEach(function (v) {
            out += '<line class="grid" x1="' + r1(xOf(v)) + '" y1="' + top + '" x2="' + r1(xOf(v)) +
                '" y2="' + (top + plotH) + '"></line>';
            out += '<text class="pl-tick" x="' + r1(xOf(v)) + '" y="' + (top + plotH + 18) +
                '" text-anchor="middle">' + esc(spec.tickX(v)) + '</text>';
        });
        out += '<line class="axis" x1="' + left + '" y1="' + (top + plotH) + '" x2="' + (left + plotW) +
            '" y2="' + (top + plotH) + '"></line>';
        out += '<text class="pl-axis" x="' + (left + plotW / 2) + '" y="' + (H - 12) +
            '" text-anchor="middle">' + esc(spec.xLabel) + '</text>';
        out += '<text class="pl-axis" transform="translate(20,' + (top + plotH / 2) +
            ') rotate(-90)" text-anchor="middle">' + esc(spec.yLabel) + '</text>';

        var placed = points.map(function (point) {
            return { point: point, cx: xOf(point.x), cy: yOf(point.y) };
        }).sort(function (a, b) { return a.cy - b.cy; });
        var taken = placed.map(function (item) {
            return { x: item.cx - 7, y: item.cy - 7, w: 14, h: 14 };
        });
        placed.forEach(function (item) {
            var point = item.point;
            var cx = r1(item.cx);
            var cy = r1(item.cy);
            out += '<circle class="sp' + (point.failed ? ' failed' : '') +
                (point.partial ? ' partial' : '') + '" cx="' + cx + '" cy="' + cy + '" r="6" fill="' +
                (point.partial ? 'none' : point.colour) + '" stroke="' +
                (point.partial ? 'var(--bad)' : point.colour) + '" stroke-width="1.6"' +
                (point.partial ? ' stroke-dasharray="3 2"' : '') + '><title>' + esc(point.title) +
                '</title></circle>';
            var options = [
                { dx: 9, dy: 4, anchor: 'start' },
                { dx: -9, dy: 4, anchor: 'end' },
                { dx: 9, dy: -8, anchor: 'start' },
                { dx: -9, dy: -8, anchor: 'end' },
                { dx: 9, dy: 16, anchor: 'start' },
                { dx: -9, dy: 16, anchor: 'end' },
                { dx: -9, dy: -20, anchor: 'end' },
                { dx: 9, dy: -20, anchor: 'start' }
            ];
            var width = Math.max(40, point.label.length * 5.8);
            var box = null;
            for (var i = 0; i < options.length && !box; i += 1) {
                var option = options[i];
                var boxLeft = option.anchor === 'start' ? cx + option.dx : cx + option.dx - width;
                var candidate = {
                    x: boxLeft, y: cy + option.dy - 9, w: width, h: 13,
                    dx: option.dx, dy: option.dy, anchor: option.anchor
                };
                if (candidate.x < left + 2 || candidate.x + width > left + plotW + 34) continue;
                var clear = taken.every(function (other) {
                    return candidate.x + candidate.w < other.x - 2 || other.x + other.w < candidate.x - 2 ||
                        candidate.y + candidate.h < other.y - 1 || other.y + other.h < candidate.y - 1;
                });
                if (clear) box = candidate;
            }
            if (!box) {
                box = { x: cx + 9, y: cy - 4, w: width, h: 13, dx: 9, dy: 4, anchor: 'start' };
            }
            taken.push({ x: box.x, y: box.y, w: box.w, h: box.h });
            if (Math.abs(box.dy) > 14) {
                out += '<line class="leader" x1="' + cx + '" y1="' + cy + '" x2="' + r1(cx + box.dx * 0.4) +
                    '" y2="' + r1(cy + box.dy * 0.8) + '"></line>';
            }
            out += '<text class="pl-lbl" x="' + r1(cx + box.dx) + '" y="' + r1(cy + box.dy) +
                '" text-anchor="' + box.anchor + '">' + esc(point.label) + '</text>';
        });
        return out + '</svg>';
    }

    function plotFigure(spec, svg) {
        return '<figure class="plot">' +
            '<div class="plot-head">' +
            '<span class="plot-title">' + esc(spec.title) + '</span>' +
            '<span class="plot-unit">' + esc(spec.unit) + '</span>' +
            (spec.partial ? '<span class="tag bad">partial</span>' : '') +
            '</div>' + svg +
            '<figcaption class="plot-cap"><span>' + esc(spec.caption) + '</span>' +
            (spec.excluded
                ? '<span class="plot-excl"><span class="tag bad">partial</span><span>' +
                  esc(spec.excluded) + '</span></span>'
                : '') +
            '</figcaption></figure>';
    }

    /* -------------------------------------------------- problems, plotted */
    /* The latest run's problems are one whole in a few parts, so they are a
       donut. A run that completed carrying errors placed fewer positions than
       the window's population, and that missing part is a hatched slice named
       excluded, so the whole of the pie is the whole population and not a
       quietly smaller set. */
    /* When the latest run carries nothing, the whole to divide is the window
       itself: the runs, by how each one ended. One whole, at most five parts,
       so the donut is still the form and its legend still carries the share. */
    function healthDonut() {
        var runs = allRuns();
        var order = ['completed clean', 'completed with errors', 'failed', 'running'];
        var parts = order.map(function (word) {
            return {
                label: word,
                value: runs.filter(function (run) { return health(run) === word; }).length
            };
        }).filter(function (part) { return part.value > 0; });
        var colour = {
            'completed clean': 'var(--ok)',
            'completed with errors': 'var(--warn)',
            'failed': 'var(--bad)',
            'running': 'var(--accent)'
        };
        parts.forEach(function (part) { part.colour = colour[part.label]; });
        var summary = parts.map(function (part) {
            return part.value + ' ' + part.label;
        }).join(', ');
        return plotFigure({
            title: 'The runs in the window, by how each ended',
            unit: 'count of runs',
            partial: false,
            caption: 'One whole, ' + parts.length + ' part' + (parts.length === 1 ? '' : 's') + ': the ' +
                runs.length + ' runs of this report in the window, divided by how each ended, so the donut ' +
                'is the form and the hole carries the count. The latest run carries no problem of its own, so ' +
                'this is the whole the window offers; its own problems by scope replace it on a run that ' +
                'recorded any. ' + summary + '.',
            excluded: ''
        }, donutChart({
            slices: parts,
            holeValue: String(runs.length),
            holeLabel: runs.length === 1 ? 'run in the window' : 'runs in the window',
            aria: 'Donut: the runs in the window by how each ended. ' + summary +
                '. Every percentage is in the legend.'
        }));
    }

    function problemsDonut() {
        var run = latestRun();
        var skipped = skippedOf(run);
        var counts = {};
        runErrors(run).forEach(function (error) {
            counts[error.scope] = (counts[error.scope] || 0) + 1;
        });
        var slices = Object.keys(counts).sort(function (a, b) {
            return (counts[b] - counts[a]) || (a < b ? -1 : 1);
        }).map(function (scope) {
            return { label: scope, value: counts[scope], colour: scopeColour(scope) };
        });
        if (skipped) {
            slices.push({
                label: 'excluded: ' + skipped + ' position' + (skipped === 1 ? '' : 's') + ' not placed',
                value: skipped,
                valueText: skipped + (skipped === 1 ? ' position' : ' positions'),
                excluded: true
            });
        }
        if (!slices.length) return healthDonut();
        var partial = runErrors(run).length > 0 || skipped > 0;
        var hole = slices.filter(function (slice) { return !slice.excluded; })
            .reduce(function (sum, slice) { return sum + slice.value; }, 0) +
            slices.filter(function (slice) { return slice.excluded; })
                .reduce(function (sum, slice) { return sum + slice.value; }, 0);
        var summary = slices.filter(function (s) { return !s.excluded; }).map(function (s) {
            return s.value + ' ' + s.label;
        }).join(', ');
        return plotFigure({
            title: 'The latest run\u2019s problems, by scope',
            unit: 'count of error rows and unplaced positions',
            partial: partial,
            caption: 'One whole, ' + slices.length + ' part' + (slices.length === 1 ? '' : 's') +
                ': the latest run, ' + run.id + ', divided by the scope of what went wrong, so the donut is ' +
                'the form and the hole carries the total. A bar chart would compare the counts and lose the ' +
                'whole they are parts of. Each scope keeps one colour here and in the stacked bar below. ' + (summary ? summary + '.' : ''),
            excluded: skipped
                ? 'The hatched slice is excluded: ' + skipped + ' position' + (skipped === 1 ? '' : 's') +
                  ' of the ' + population() + ' the window priced are not in this run, so they are drawn ' +
                  'as a slice of their own rather than dropped. A pie without them would be smaller than the ' +
                  'population it claims to divide.'
                : ''
        }, donutChart({
            slices: slices,
            valueText: function (value) {
                return value + (value === 1 ? ' row' : ' rows');
            },
            holeValue: String(hole),
            holeLabel: skipped ? 'rows and positions' : 'error rows recorded',
            aria: 'Donut: the latest run\u2019s problems by scope, counted in error rows. ' + summary +
                (skipped ? ', and ' + skipped + ' positions excluded as a hatched slice' : '') +
                '. Every percentage is in the legend.'
        }));
    }

    /* Several runs each split the same way: stacked bars. */
    function problemsStacked() {
        var runs = allRuns().filter(function (run) {
            return runErrors(run).length > 0;
        }).slice(0, 6).reverse();
        if (runs.length < 2) {
            return '<div class="plot-absent"><b>No stacked bar for these runs.</b> One run at most in the ' +
                'window carries problems, so there is one whole and no set of wholes to stack. The donut ' +
                'above is the form for one.</div>';
        }
        var bars = runs.map(function (run) {
            var counts = {};
            runErrors(run).forEach(function (error) {
                counts[error.scope] = (counts[error.scope] || 0) + 1;
            });
            return {
                label: runLabel(run) + (run.id === latestRun().id ? ' \u00b7 latest' : ''),
                segments: Object.keys(counts).sort(function (a, b) {
                    return (counts[b] - counts[a]) || (a < b ? -1 : 1);
                }).map(function (scope) {
                    return { label: scope, value: counts[scope], colour: scopeColour(scope) };
                })
            };
        });
        var scopes = [];
        bars.forEach(function (bar) {
            bar.segments.forEach(function (segment) {
                if (scopes.indexOf(segment.label) === -1) scopes.push(segment.label);
            });
        });
        var summary = runs.map(function (run) {
            return run.id + ': ' + runErrors(run).length + ' problem' + (runErrors(run).length === 1 ? '' : 's') +
                ' (' + runErrors(run).map(function (error) { return error.scope; })
                    .filter(function (value, index, all) { return all.indexOf(value) === index; })
                    .join(', ') + ')';
        }).join('; ');
        return plotFigure({
            title: 'Problems by scope, run by run',
            unit: 'count of error rows',
            partial: runs.some(function (run) { return runErrors(run).length > 0; }),
            caption: 'Stacked bars, because there is more than one whole and each is split the same way: ' +
                'one bar per run with problems in the window, one segment per scope, and the same colour for ' +
                'the same scope as the donut. A donut would show one run and hide the three places this ' +
                'report goes wrong. ' + summary + '.',
            excluded: 'No segment here is hatched: an error row is either recorded or it is not, so every ' +
                'count is whole. The unplaced positions are a different kind of part, and the donut above ' +
                'draws them where a whole population is divided.'
        }, stackedChart({
            bars: bars,
            aria: 'Stacked bars: problems by scope, run by run. ' + summary + '.'
        }) + stackedLegend(scopes.map(function (scope) {
            return { label: scope, colour: scopeColour(scope) };
        })));
    }

    /* Two series over the same runs and the same unit: the errors and the
       warnings each run recorded. Both are counts of error rows, so they sit on
       one axis honestly, and the pair says whether a run merely warned or
       actually failed to price something. */
    function severityGrouped() {
        var runs = allRuns().filter(function (run) {
            return runErrors(run).length > 0;
        }).slice(0, 6).reverse();
        if (runs.length < 2) {
            return '<div class="plot-absent"><b>No grouped bar for these runs.</b> One run at most in the ' +
                'window recorded a problem, so there is one category and no pair to compare. The donut ' +
                'above states the one run\u2019s split.</div>';
        }
        var rows = runs.map(function (run) {
            var errors = runErrors(run);
            return {
                label: runLabel(run),
                values: [
                    errors.filter(function (e) { return e.severity === 'error'; }).length,
                    errors.filter(function (e) { return e.severity === 'warning'; }).length
                ]
            };
        });
        var summary = runs.map(function (run) {
            var errors = runErrors(run);
            return run.id + ': ' +
                errors.filter(function (e) { return e.severity === 'error'; }).length + ' errors, ' +
                errors.filter(function (e) { return e.severity === 'warning'; }).length + ' warnings';
        }).join('; ');
        return plotFigure({
            title: 'Errors against warnings, run by run',
            unit: 'count of error rows, both series',
            partial: true,
            caption: 'Grouped bars, because the two series share the same runs and the same unit and the ' +
                'question is the two side by side: a stack would make the warnings start where the errors ' +
                'end, and a reader could not see which run only warned. ' + summary + '.',
            excluded: 'A run that completed clean is not drawn, because zero errors and zero warnings is the ' +
                'absence of a part and not a part. No bar here is hatched: an error row is either recorded ' +
                'or it is not, so both counts are whole.'
        }, groupedChart({
            categories: rows,
            series: [
                { id: 'errors', label: 'Errors', colour: 'var(--bad)', partial: false },
                { id: 'warnings', label: 'Warnings', colour: 'var(--warn)', partial: false }
            ],
            aria: 'Grouped bars: errors and warnings recorded, run by run. ' + summary + '.'
        }));
    }

    /* Two measures against each other: how long a run took against how many
       positions it placed. A run that took long and placed few is the outlier a
       reader wants, and it is a distance on this plot rather than a row to
       find. Both axes are ordinary counts of real magnitude, so both are
       linear and both start at zero. */
    function durationScatter() {
        var runs = allRuns().filter(function (run) {
            return typeof run.priced === 'number';
        });
        if (runs.length < 3) {
            return '<div class="plot-absent"><b>No scatter for this window.</b> Fewer than three runs ' +
                'placed positions, and three points on two axes state nothing the table does not.</div>';
        }
        var points = runs.map(function (run) {
            var partial = run.state === 'completed' && runErrors(run).length > 0;
            var failed = run.state === 'failed';
            return {
                label: runLabel(run),
                x: elapsedSeconds(run),
                y: runPriced(run),
                partial: partial || failed,
                failed: failed,
                colour: partial || failed ? 'var(--bad)' : (run.id === latestRun().id
                    ? 'var(--accent-bright)' : 'var(--ok)'),
                title: run.id + ', started ' + run.started + ': ' + duration(run) + ', ' +
                    runPriced(run) + ' positions placed' +
                    (failed ? ' (the run failed)' : (partial ? ' (partial)' : ''))
            };
        });
        var summary = points.map(function (point) {
            return point.label + ' placed ' + point.y + ' in ' + fmtDuration(point.x);
        }).join('; ');
        var slowest = points.slice().sort(function (a, b) { return b.x - a.x; })[0];
        var partials = points.filter(function (point) { return point.partial; });
        return plotFigure({
            title: 'How long a run took against how many positions it placed',
            unit: 'duration (across) \u00b7 positions placed (up)',
            partial: partials.length > 0,
            caption: 'A scatter, because two measures are plotted against each other and the question is ' +
                'shape: a run that took long and placed few is away from the rest, and so is a run that ' +
                'placed few in a short time. ' + slowest.label + ' is the slowest run at ' +
                fmtDuration(slowest.x) + ', and the run at the left placed fewest. Both axes start at zero ' +
                'and both are plain counts of seconds and positions. ' + summary + '.',
            excluded: partials.length
                ? 'The hollow points are the runs whose positions are not the whole answer: ' +
                  partials.map(function (point) { return point.label; }).join(', ') +
                  '. A run that failed stopped placing positions part-way, and a run that completed carrying ' +
                  'errors placed fewer than the window\u2019s population, so each of those points is a floor ' +
                  'and is drawn hollow for that reason.'
                : ''
        }, scatterChart({
            points: points,
            xLabel: 'Duration, seconds',
            yLabel: 'Positions placed',
            tickX: function (v) { return String(Math.round(v)); },
            tickY: function (v) { return String(Math.round(v)); },
            aria: 'Scatter: how long a run took against how many positions it placed, one point per run. ' +
                summary + '.'
        }));
    }

    function plotsCard() {
        return '<section class="card" id="plots">' +
            '<div class="sectionhead"><h2>The problems and the shape of the window</h2>' +
            '<div class="meta"><span class="tag warn">fixture</span>' +
            '<span>inline SVG \u00b7 the same runs as the table above \u00b7 a plot never replaces the numbers</span>' +
            '</div></div>' + formRule() +
            problemsDonut() + problemsStacked() + severityGrouped() + durationScatter() +
            '<p class="rule">Every mark here is drawn from the same fixture runs the table above carries, and ' +
            'nothing reads a server. No chart library is loaded or copied in \u2014 a page opened from ' +
            '<span class="mono">file://</span> has no build, no CDN and no network \u2014 so each plot is inline ' +
            'SVG written by this page.</p>' +
            '</section>';
    }

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
        return runErrors(run).length > 0 ? 'completed with errors' : 'completed clean';
    }

    function healthTag(run) {
        var word = health(run);
        return '<span class="tag ' + HEALTH[word].tag + '">' + esc(word) + '</span>';
    }

    function severityCounts(run) {
        var out = { error: 0, warning: 0, total: 0 };
        if (!run) return out;
        runErrors(run).forEach(function (error) {
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
        var skipped = runErrors(run).length;
        return 'SA-CCR exposure written. ' + runPriced(run) + ' trades priced, ' + skipped +
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
        scenario: null,
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
        S.scenario = params.get('scenario') === 'results-with-errors' ? 'results-with-errors' : null;
    }

    function writeParams() {
        var params = new URLSearchParams(window.location.search);
        params.set('variant', S.variant);
        params.set('open', S.open);
        if (S.committed) params.delete('arrived');
        if (S.scenario) params.set('scenario', S.scenario);
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
            'read of <span class="mono">report_trigger_event</span> exists. <b>The exposure series is a ' +
            'fixture too:</b> the trend plot draws the SA-CCR exposure of each run, which nothing parses out ' +
            'of a run\u2019s archive today, and a run that completed carrying errors is drawn as a partial ' +
            'point because its exposure covers only the trades it placed.</div>';
    }

    /* The scenario banner. It is not dismissable: a partial figure a reader may
       put away is a figure a reader will take as whole. */
    function scenarioNote() {
        if (!S.scenario) return '';
        var sc = SCENARIO;
        var run = latestRun();
        var errors = runErrors(run);
        return '<div class="notice partial" role="status">' +
            '<strong>The scenario results-with-errors is on.</strong> ' +
            'The latest run, ' + esc(run.id) + ', is drawn as a run that completed with what its errors ' +
            'left out: ' + sc.unplaced + ' positions it did not place and ' + sc.errors.length + ' more error ' +
            'rows, ' + esc(sc.errors.map(function (error) { return error.scope; })
                .filter(function (value, index, all) { return all.indexOf(value) === index; }).join(' and ')) +
            '. The exposure above is a floor, its point is hollow and dashed, and every donut carries the ' +
            'excluded part as a hatched slice. The run recorded ' + errors.length + ' problems in all. ' +
            '<span class="mono">?scenario=results-with-errors</span> puts it back.</div>';
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
            ? '<ul class="errlist">' + runErrors(run).map(function (error) {
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
            fact('Trades priced', typeof runPriced(run) === 'number' ? esc(String(runPriced(run))) : '<span class="faint">\u2014</span>') +
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
                (typeof runPriced(run) === 'number' ? esc(String(runPriced(run))) : '\u2014') +
                ' trades priced \u00b7 ' +
                (runErrors(run).length
                    ? esc(String(runErrors(run).length)) + (runErrors(run).length === 1 ? ' error' : ' errors')
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
            (typeof runPriced(earlier) === 'number' ? runPriced(earlier) : '\u2014') + ' \u2192 ' +
            (typeof runPriced(latest) === 'number' ? runPriced(latest) : '\u2014')]);

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
            (typeof runPriced(run) === 'number' ? esc(String(runPriced(run))) : '\u2014') + '</span>' +
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
        if (S.scenario) bits.push('scenario ' + S.scenario);
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
            scenarioNote() +
            trendCard() +
            plotsCard() +
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
