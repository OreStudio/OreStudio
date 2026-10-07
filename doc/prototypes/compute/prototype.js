/* Compute: the five grid screens prototype. Self-contained: plain JavaScript,
 * mock data, no framework, no build step, and nothing that outlives the page.
 *
 * The use cases and the chart list come from
 * doc/knowledge/journeys/compute/compute.org. The accepted fleet screen this
 * builds on is doc/prototypes/compute-grid/, whose idiom this copies: plain
 * HTML strings, a variant bar that prints the state, and no server read.
 *
 * Every series here is a fixture from a seeded generator, so the walk repeats
 * exactly. Every chart is hand-rolled inline SVG. Nothing here reads a server,
 * and nothing outlives the page. */

(function () {
    'use strict';

    // ------------------------------------------------------------- the seed
    /* mulberry32, a small seeded PRNG. Not Math.random, so the walk repeats. */
    function mulberry32(a) {
        return function () {
            a |= 0; a = (a + 0x6D2B79F5) | 0;
            var t = Math.imul(a ^ (a >>> 15), 1 | a);
            t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
            return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
        };
    }
    var rand = mulberry32(20261007);

    // ------------------------------------------------------------ the clock
    /* 72 intervals at five minutes is six hours, 09:00 to 15:00. */
    var INTERVALS = 72;
    var SPACING_MIN = 5;
    var START_MIN = 9 * 60;
    var NOW_MIN = START_MIN + INTERVALS * SPACING_MIN;

    function pad2(n) { return n < 10 ? '0' + n : String(n); }
    function clock(min) {
        var m = ((min % 1440) + 1440) % 1440;
        return pad2(Math.floor(m / 60)) + ':' + pad2(m % 60);
    }
    function intervalTime(i) { return clock(START_MIN + i * SPACING_MIN); }

    // ----------------------------------------------------------- the palette
    /* Okabe-Ito, colour-blind safe, plus the house's own semantic colours. */
    var C = {
        blue: '#0072B2',
        sky: '#56B4E9',
        teal: '#009E73',
        green: '#009E73',
        amber: '#E69F00',
        yellow: '#F0E442',
        vermillion: '#D55E00',
        violet: '#CC79A7',
        grey: '#9aa4b2',
        greyDark: '#4b5563'
    };

    /* Viridis, dark to light: a perceptually ordered ramp, never red to green. */
    var RAMP = [
        [0.00, [68, 1, 84]],
        [0.25, [59, 82, 139]],
        [0.50, [33, 145, 140]],
        [0.75, [94, 201, 98]],
        [1.00, [253, 231, 37]]
    ];
    function rampRgb(t) {
        if (!(t > 0)) t = 0;
        if (t > 1) t = 1;
        for (var s = 1; s < RAMP.length; s++) {
            if (t <= RAMP[s][0]) {
                var a = RAMP[s - 1], b = RAMP[s];
                var f = (t - a[0]) / (b[0] - a[0]);
                return [
                    Math.round(a[1][0] + (b[1][0] - a[1][0]) * f),
                    Math.round(a[1][1] + (b[1][1] - a[1][1]) * f),
                    Math.round(a[1][2] + (b[1][2] - a[1][2]) * f)
                ];
            }
        }
        return RAMP[RAMP.length - 1][1];
    }
    function rampCss(t) { return 'rgb(' + rampRgb(t).join(',') + ')'; }
    /* A number sitting on a ramp cell must stay readable at both ends. */
    function inkOn(t) {
        var c = rampRgb(t);
        var lum = 0.299 * c[0] + 0.587 * c[1] + 0.114 * c[2];
        return lum > 145 ? '#0b0e13' : '#f0f0f2';
    }

    // ------------------------------------------------------------- the tenancy
    var TENANTS = [
        { id: 'northwind', name: 'Northwind Capital', short: 'Northwind', color: C.blue, share: 0.40 },
        { id: 'helios', name: 'Helios Asset Management', short: 'Helios', color: C.teal, share: 0.28 },
        { id: 'meridian', name: 'Meridian Bank', short: 'Meridian', color: C.violet, share: 0.20 },
        { id: 'system', name: 'System (installation)', short: 'System', color: C.greyDark, share: 0.12 }
    ];
    /* The tenant the tenant variant narrows to. */
    var TENANT_VARIANT = 'northwind';

    function tenantById(id) {
        for (var i = 0; i < TENANTS.length; i++) if (TENANTS[i].id === id) return TENANTS[i];
        return TENANTS[0];
    }

    // ------------------------------------------------------------ the nodes
    var NODE_CORES = [16, 32, 16, 64, 32, 16, 32, 16, 64, 32, 16, 32, 16, 32, 16, 64];
    var WRAPPER_VERSION_BY_NODE = [
        'v0.0.25', 'v0.0.25', 'v0.0.25', 'v0.0.25', 'v0.0.25', 'v0.0.24', 'v0.0.25', 'v0.0.25',
        'v0.0.25', 'v0.0.24', 'v0.0.25', 'v0.0.25', 'v0.0.19', 'v0.0.25', 'v0.0.25', 'v0.0.25'
    ];

    /* The story: one hot node, one node that went quiet, one draining, one
       slow, one bad release, one load spike. */
    var NODES = [];
    (function buildNodes() {
        for (var i = 0; i < 16; i++) {
            var cores = NODE_CORES[i];
            NODES.push({
                index: i,
                hostId: (0x10000000 + i * 7919).toString(16),
                host: 'grid-' + pad2(i + 1) + '.example.com',
                short: 'grid-' + pad2(i + 1),
                cores: cores,
                memGiB: cores * 4,
                gpu: (i % 4 === 0) ? (cores === 64 ? 2 : 1) : 0,
                wrapperVersion: WRAPPER_VERSION_BY_NODE[i],
                hot: i === 3,
                quiet: i === 11,
                draining: i === 6,
                slow: i === 8,
                baseWork: 6 + Math.round(rand() * 10),
                work: [], fails: [], state: []
            });
        }
    })();

    function shapeAt(i) {
        var wave = 0.55 + 0.45 * Math.sin((i / INTERVALS) * Math.PI * 1.5 - 0.4);
        var spike = (i >= 46 && i <= 52) ? 0.85 * (1 - Math.abs(i - 49) / 4) : 0;
        return Math.max(0.08, wave + spike);
    }

    function stateOfNode(node, i) {
        /* grid-12 is lost from 12:20 and grid-07 drains from 11:30. */
        if (node.quiet) return i >= 40 ? 'lost' : 'running';
        if (node.draining) return i >= 30 ? 'draining' : 'running';
        return 'running';
    }

    (function buildNodeSeries() {
        NODES.forEach(function (node) {
            for (var i = 0; i < INTERVALS; i++) {
                var f = shapeAt(i);
                var st = stateOfNode(node, i);
                var w = 0;
                if (st === 'running' || st === 'draining') {
                    w = Math.round(node.baseWork * f * (node.hot ? 1.9 : 1) + (rand() * 3 - 1.5));
                    if (st === 'draining') w = Math.round(w * 0.45);
                    if (w < 0) w = 0;
                }
                node.work[i] = w;
                var p = node.hot ? 0.004 : (node.slow ? 0.006 : 0.002);
                var fails = 0;
                for (var k = 0; k < w; k++) if (rand() < p) fails++;
                node.fails[i] = fails;
                node.state[i] = st;
            }
        });
    })();

    // ------------------------------------------------------------- the apps
    var APP_VERSIONS = [
        { app: 'ores.pricer', version: 'v2.4.1', released: '2026-10-01', status: 'current', jobs: 8, bad: true, failRate: 0.375 },
        { app: 'ores.pricer', version: 'v2.4.0', released: '2026-09-12', status: 'previous', jobs: 3, failRate: 0 },
        { app: 'ores.curve-boot', version: 'v1.9.3', released: '2026-09-28', status: 'current', jobs: 4, failRate: 0 },
        { app: 'ores.curve-boot', version: 'v1.9.2', released: '2026-09-02', status: 'previous', jobs: 2, failRate: 0 },
        { app: 'ores.risk', version: 'v3.1.0', released: '2026-10-03', status: 'current', jobs: 4, failRate: 0 },
        { app: 'ores.report', version: 'v1.2.7', released: '2026-08-19', status: 'current', jobs: 2, failRate: 0 },
        { app: 'ores.report', version: 'v1.2.6', released: '2026-07-22', status: 'previous', jobs: 1, failRate: 0 },
        { app: 'ores.scenario', version: 'v0.8.4', released: '2026-09-30', status: 'current', jobs: 3, failRate: 0 },
        { app: 'ores.scenario', version: 'v0.8.3', released: '2026-09-05', status: 'previous', jobs: 1, failRate: 0 },
        { app: 'ores.vol-surface', version: 'v0.5.1', released: '2026-10-02', status: 'current', jobs: 3, failRate: 0 }
    ];
    var WRAPPER_VERSIONS = ['v0.0.25', 'v0.0.24', 'v0.0.19'];

    var APP_BASE_SECONDS = {
        'ores.pricer': 240, 'ores.curve-boot': 95, 'ores.risk': 520,
        'ores.report': 900, 'ores.scenario': 610, 'ores.vol-surface': 300
    };

    // ------------------------------------------------------------- the work
    var BATCHES = ['BATCH-2418', 'BATCH-2419', 'BATCH-2421', 'BATCH-2424', 'BATCH-2427', 'BATCH-2431'];

    function pickTenantId() {
        var r = rand();
        if (r < 0.40) return 'northwind';
        if (r < 0.68) return 'helios';
        if (r < 0.88) return 'meridian';
        return 'system';
    }

    function pickNode(submit) {
        var ok = NODES.filter(function (n) {
            if (n.quiet && submit >= 38) return false;
            if (n.draining && submit >= 30 && rand() < 0.7) return false;
            return true;
        });
        if (ok.length === 0) ok = NODES;
        return ok[Math.floor(rand() * ok.length)];
    }

    var STORY = {
        hot: 'grid-04',
        quiet: 'grid-12 lost from ' + intervalTime(40),
        draining: 'grid-07 draining from ' + intervalTime(30),
        badVersion: 'ores.pricer v2.4.1',
        spike: 'load spike ' + intervalTime(46) + ' to ' + intervalTime(52),
        slow: 'grid-09 slow (mean task time 1.6x the fleet)'
    };

    var JOBS = [];
    (function buildJobs() {
        var counter = 40000;
        APP_VERSIONS.forEach(function (av, avIndex) {
            for (var n = 0; n < av.jobs; n++) {
                counter += 1 + Math.floor(rand() * 7);
                var tenantId = pickTenantId();
                var submit = av.bad
                    ? 34 + Math.floor(rand() * (INTERVALS - 38))
                    : 4 + Math.floor(rand() * 56);
                var duration = Math.round((APP_BASE_SECONDS[av.app] || 300) * (0.6 + rand() * 0.8));
                var node = pickNode(submit);
                if (node.slow) duration = Math.round(duration * 1.6);
                var span = Math.max(1, Math.round(duration / (SPACING_MIN * 60)));
                var end = submit + span;
                var failThis = av.bad && (n === 1 || n === 4 || n === 6);
                var state;
                if (failThis) state = 'failed';
                else if (submit >= INTERVALS - 1) state = 'queued';
                else if (end >= INTERVALS) state = 'running';
                else state = 'done';

                var attempts = [{
                    node: node.short,
                    start: submit,
                    end: Math.min(INTERVALS, end),
                    state: state
                }];
                if (!failThis && rand() < 0.15) {
                    var first = pickNode(submit).short;
                    attempts.unshift({ node: first, start: submit, end: Math.min(submit + 1, INTERVALS), state: 'aborted' });
                }

                var phases = buildPhases(duration, state);
                JOBS.push({
                    id: 'J-' + counter,
                    batch: BATCHES[(avIndex + n) % BATCHES.length],
                    app: av.app,
                    version: av.version,
                    tenant: tenantId,
                    node: node.short,
                    submit: submit,
                    durationSec: duration,
                    state: state,
                    attempts: attempts,
                    phases: phases,
                    exitCode: failThis ? 1 : 0,
                    stderr: failThis
                        ? 'Traceback (most recent call last):\n  File "pricer.py", line 214, in run\n    curve = market.spot(ccy, date)\nValueError: no market data for 2026-10-06 EUR spot curve\nworker exited 1'
                        : '',
                    requirements: {
                        cores: [1, 1, 2, 2, 4, 4, 8, 8, 16][Math.floor(rand() * 9)],
                        memGiB: 0,
                        gpu: rand() < 0.22 ? 1 : 0,
                        wallclockMin: Math.max(2, Math.round(duration / 60 * (1.1 + rand() * 0.5))),
                        inputMiB: 40 + Math.round(rand() * 900)
                    }
                });
                JOBS[JOBS.length - 1].requirements.memGiB = JOBS[JOBS.length - 1].requirements.cores * (2 + Math.floor(rand() * 4));
            }
        });
    })();

    function buildPhases(duration, state) {
        var names = ['queued', 'dispatched', 'downloaded', 'running', 'uploaded', 'validated'];
        var weights = [0.04, 0.03, 0.14, 0.72, 0.05, 0.02];
        var total = 0;
        var raw = weights.map(function (w) { var v = w * (0.7 + rand() * 0.6); total += v; return v; });
        var secs = raw.map(function (v) { return v / total * duration; });
        return names.map(function (name, i) { return { name: name, sec: secs[i] }; });
    }

    // --------------------------------------------------------- the fleet series
    var FLEET = (function buildFleet() {
        var f = {
            throughput: [], failures: [], success: [], clientError: [], noReply: [],
            done: [], inProgress: [], unsent: [], inactive: [],
            online: [], idle: [], total: [],
            coresUsed: [], memUsed: [], gpuUsed: [],
            coresTotal: 0, memTotal: 0, gpuTotal: 0,
            tenantCores: {}
        };
        NODES.forEach(function (n) {
            f.coresTotal += n.cores;
            f.memTotal += n.memGiB;
            f.gpuTotal += n.gpu;
        });
        TENANTS.forEach(function (t) { f.tenantCores[t.id] = []; });

        var jit = [];
        for (var j = 0; j < INTERVALS; j++) jit[j] = rand();

        /* The bad release fails every task it runs, so its failures arrive as a
           burst over the intervals its jobs ran. The background rate is low, so
           the failure line is flat and then it is not. */
        var burst = zeros(INTERVALS);
        JOBS.forEach(function (job) {
            if (job.state !== 'failed') return;
            var run = job.attempts[0];
            var start = run.start;
            var end = Math.max(start + 1, run.end);
            for (var k = start; k < end && k < INTERVALS; k++) burst[k] += 9;
        });

        for (var i = 0; i < INTERVALS; i++) {
            var done = 0, fails = burst[i], online = 0, idle = 0, used = 0;
            NODES.forEach(function (node) {
                done += node.work[i];
                fails += node.fails[i];
                if (node.state[i] !== 'lost') online++;
                if (node.state[i] !== 'lost' && node.work[i] < node.baseWork * 0.25) idle++;
                if (node.state[i] !== 'lost') {
                    used += Math.min(node.cores, Math.round(node.cores * node.work[i] / (node.baseWork * 2.1)));
                }
            });
            var shape = shapeAt(i);
            var clientError = Math.round(fails * 0.62);
            f.total[i] = NODES.length;
            f.online[i] = online;
            f.idle[i] = idle;
            f.throughput[i] = done;
            f.failures[i] = fails;
            f.success[i] = done - fails;
            f.clientError[i] = clientError;
            f.noReply[i] = fails - clientError;
            f.done[i] = done;
            f.inProgress[i] = Math.round(done * 0.22) + Math.round(shape * 14);
            f.unsent[i] = Math.round(done * 0.12) + Math.round(shape * 9) + (i > 52 ? 20 : 0);
            f.inactive[i] = Math.round(done * 0.05) + 3;
            f.coresUsed[i] = Math.min(f.coresTotal, used);
            f.memUsed[i] = Math.min(f.memTotal, Math.round((f.coresUsed[i] / f.coresTotal) * f.memTotal * (0.80 + 0.18 * jit[i])));
            f.gpuUsed[i] = Math.min(f.gpuTotal, Math.round(f.gpuTotal * (0.35 + 0.55 * shape)));

            var runningShare = 0;
            TENANTS.forEach(function (t, ti) {
                var share = t.share * (0.94 + 0.12 * jit[(i + ti * 7) % INTERVALS]);
                runningShare += share;
            });
            TENANTS.forEach(function (t, ti) {
                var share = t.share * (0.94 + 0.12 * jit[(i + ti * 7) % INTERVALS]) / runningShare;
                f.tenantCores[t.id][i] = Math.round(f.coresUsed[i] * share);
            });
        }
        return f;
    })();

    // ------------------------------------------------ the duration fixtures
    var NODE_DURATIONS = NODES.map(function (node) {
        var samples = JOBS.filter(function (j) { return j.node === node.short; })
            .map(function (j) { return j.durationSec; });
        var mean = 250 * (node.slow ? 1.65 : 1) * (node.hot ? 0.92 : 1);
        var guard = 0;
        while (samples.length < 14 && guard < 200) {
            samples.push(Math.max(20, Math.round(mean * (0.55 + rand() * 1.0))));
            guard++;
        }
        samples.sort(function (a, b) { return a - b; });
        return samples;
    });

    var JOB_DURATIONS = JOBS.map(function (j) { return j.durationSec; }).sort(function (a, b) { return a - b; });

    function percentile(sorted, q) {
        if (sorted.length === 0) return 0;
        var pos = (sorted.length - 1) * q;
        var lo = Math.floor(pos), hi = Math.ceil(pos);
        if (lo === hi) return sorted[lo];
        return sorted[lo] + (sorted[hi] - sorted[lo]) * (pos - lo);
    }
    var P50 = percentile(JOB_DURATIONS, 0.50);
    var P90 = percentile(JOB_DURATIONS, 0.90);
    var P99 = percentile(JOB_DURATIONS, 0.99);

    // --------------------------------------------------------- the arrivals
    var ARRIVALS = [
        { app: 'ores.risk', version: 'v3.1.0', tenant: 'northwind', inMinutes: 25, cores: 16, memGiB: 64, gpu: 0, wallclockMin: 35, jobs: 4 },
        { app: 'ores.pricer', version: 'v2.4.1', tenant: 'helios', inMinutes: 55, cores: 8, memGiB: 16, gpu: 0, wallclockMin: 20, jobs: 6 },
        { app: 'ores.vol-surface', version: 'v0.5.1', tenant: 'meridian', inMinutes: 100, cores: 4, memGiB: 32, gpu: 1, wallclockMin: 45, jobs: 2 },
        { app: 'ores.report', version: 'v1.2.7', tenant: 'northwind', inMinutes: 160, cores: 2, memGiB: 8, gpu: 0, wallclockMin: 90, jobs: 1 },
        { app: 'ores.scenario', version: 'v0.8.4', tenant: 'helios', inMinutes: 240, cores: 8, memGiB: 24, gpu: 0, wallclockMin: 60, jobs: 3 }
    ];

    // ------------------------------------------------------- the concurrency
    // The policy is a behaviour, skip or queue or fail, and not a number, so
    // there is no cap column to carry.
    var POLICIES = [
        { app: 'ores.pricer', inFlight: 11, behaviour: 'queue' },
        { app: 'ores.curve-boot', inFlight: 6, behaviour: 'skip' },
        { app: 'ores.risk', inFlight: 3, behaviour: 'fail' },
        { app: 'ores.report', inFlight: 1, behaviour: 'queue' },
        { app: 'ores.scenario', inFlight: 7, behaviour: 'skip' },
        { app: 'ores.vol-surface', inFlight: 2, behaviour: 'fail' }
    ];

    // --------------------------------------------------------- the failures
    var FAILED_JOBS = JOBS.filter(function (j) { return j.state === 'failed'; });
    var BAD_VERSION_FIRST_SUBMIT = (function () {
        var first = INTERVALS;
        FAILED_JOBS.forEach(function (j) { if (j.submit < first) first = j.submit; });
        return first === INTERVALS ? 34 : first;
    })();

    /* Task failures per node: the low background plus the burst the bad release
       leaves on whichever node a failed job touched. */
    function nodeFailureTotals() {
        var totals = {};
        NODES.forEach(function (n) {
            totals[n.short] = n.fails.reduce(function (a, b) { return a + b; }, 0);
        });
        FAILED_JOBS.forEach(function (job) {
            var run = job.attempts[0];
            var span = Math.max(1, run.end - run.start);
            totals[job.node] = (totals[job.node] || 0) + 9 * span;
        });
        return NODES.map(function (n) { return totals[n.short]; });
    }

    // ------------------------------------------------------------- variants
    var SCREENS = [
        { id: 'watch', name: '1 Watch the grid', short: 'Watch the grid', question: 'Is the grid healthy right now?' },
        { id: 'job', name: '2 Follow a job', short: 'Follow a job', question: 'Where is one job, and where did its time go?' },
        { id: 'failure', name: '3 Diagnose a failure', short: 'Diagnose a failure', question: 'Why did it fail, and can I run it again?' },
        { id: 'capacity', name: '4 Plan the capacity', short: 'Plan the capacity', question: 'What is coming, and can the grid take it?' },
        { id: 'versions', name: '5 Keep what it runs', short: 'Keep what it runs', question: 'What may the grid run, and how much at once?' }
    ];

    var VARIANTS = [
        { id: 'fleet', name: 'Whole fleet', gist: 'The super administrator reads the whole installation: every node, every tenant\u2019s work, and the node logs.' },
        { id: 'tenant', name: 'One tenant', gist: 'A tenant administrator reads Northwind Capital\u2019s rows; the node logs are withheld, because a node\u2019s logs are the installation\u2019s.' }
    ];

    var S = {
        screen: 'watch',
        variant: 'fleet',
        heatMetric: 'work',
        job: null,
        showState: true,
        watchTab: 'fleet',
        updatedAt: clock(NOW_MIN),
        logsDownloaded: 0
    };
    var log = [];

    function screenById(id) { for (var i = 0; i < SCREENS.length; i++) if (SCREENS[i].id === id) return SCREENS[i]; return SCREENS[0]; }
    function variantById(id) { for (var i = 0; i < VARIANTS.length; i++) if (VARIANTS[i].id === id) return VARIANTS[i]; return VARIANTS[0]; }
    function activeScreen() { return screenById(S.screen); }
    function isTenant() { return S.variant === 'tenant'; }

    function filteredJobs() {
        if (!isTenant()) return JOBS;
        return JOBS.filter(function (j) { return j.tenant === TENANT_VARIANT; });
    }

    function selectedJob() {
        var jobs = filteredJobs();
        for (var i = 0; i < jobs.length; i++) if (jobs[i].id === S.job) return jobs[i];
        var failed = jobs.filter(function (j) { return j.state === 'failed'; })[0];
        return failed || jobs[0] || JOBS[0];
    }

    // ------------------------------------------------------------- helpers
    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }
    function fmtInt(n) { return String(Math.round(n)); }
    function num(n) { return String(Math.round(n)).replace(/\B(?=(\d{3})+(?!\d))/g, ','); }
    function fmtAxis(v) {
        if (v === 0) return '0';
        if (v >= 10000) return Math.round(v / 1000) + 'k';
        if (v >= 1000) return (v / 1000).toFixed(1) + 'k';
        if (v >= 10) return String(Math.round(v));
        if (v >= 1) return v.toFixed(v % 1 === 0 ? 0 : 1);
        return v.toFixed(2);
    }
    function asDuration(sec) {
        if (sec < 60) return Math.round(sec) + ' s';
        var m = Math.floor(sec / 60), s = Math.round(sec % 60);
        if (m < 60) return m + ' m ' + pad2(s) + ' s';
        return Math.floor(m / 60) + ' h ' + pad2(m % 60) + ' m';
    }
    function pct(x) { return (x * 100).toFixed(1) + '%'; }
    function niceMax(v) {
        if (!(v > 0)) return 1;
        var mag = Math.pow(10, Math.floor(Math.log(v) / Math.LN10));
        var n = v / mag;
        var step = n <= 1 ? 1 : n <= 2 ? 2 : n <= 2.5 ? 2.5 : n <= 5 ? 5 : 10;
        return step * mag;
    }
    function zeros(n) { var a = []; for (var i = 0; i < n; i++) a[i] = 0; return a; }
    function maxOf(series) {
        var m = 0;
        series.forEach(function (v) { if (v > m) m = v; });
        return m;
    }
    function maxOfMatrix(rows) {
        var m = 0;
        rows.forEach(function (row) { row.forEach(function (v) { if (v > m) m = v; }); });
        return m;
    }

    // ------------------------------------------------------------- svg kit
    function svgEl(w, h, inner, cls, title) {
        return '<svg viewBox="0 0 ' + w + ' ' + h + '" class="chart ' + (cls || '') + '" role="img" preserveAspectRatio="xMidYMid meet">' +
            '<title>' + esc(title) + '</title>' + inner + '</svg>';
    }
    function fullPlot(w, h, left, right, top, bottom) {
        return { x: left, y: top, w: w - left - right, h: h - top - bottom };
    }
    function xAt(p, i) { return p.x + (i / (INTERVALS - 1)) * p.w; }
    function yAt(p, v, max) { return p.y + p.h - (v / max) * p.h; }
    function pt(x, y) { return x.toFixed(2) + ' ' + y.toFixed(2); }
    function path(points) {
        var out = '';
        for (var i = 0; i < points.length; i++) out += (i === 0 ? 'M' : 'L') + points[i][0].toFixed(2) + ' ' + points[i][1].toFixed(2) + ' ';
        return out;
    }

    function yAxis(p, max, unit, ticks, fmt) {
        var f = fmt || fmtAxis;
        var out = '';
        for (var t = 0; t <= ticks; t++) {
            var v = max * t / ticks;
            var y = p.y + p.h - (v / max) * p.h;
            out += '<line class="grid" x1="' + p.x + '" y1="' + y.toFixed(1) + '" x2="' + (p.x + p.w) + '" y2="' + y.toFixed(1) + '"/>';
            out += '<text class="tick" x="' + (p.x - 6) + '" y="' + (y + 3.5).toFixed(1) + '">' + esc(f(v)) + '</text>';
        }
        if (unit) out += '<text class="axis-label" x="' + p.x + '" y="' + (p.y - 5) + '">' + esc(unit) + '</text>';
        return out;
    }

    function timeAxis(p, unitLabel) {
        var out = '';
        for (var i = 0; i < INTERVALS; i += 12) {
            var x = xAt(p, i);
            out += '<line class="grid" x1="' + x.toFixed(1) + '" y1="' + p.y + '" x2="' + x.toFixed(1) + '" y2="' + (p.y + p.h) + '"/>';
            out += '<text class="tick mid" x="' + x.toFixed(1) + '" y="' + (p.y + p.h + 14) + '">' + esc(intervalTime(i)) + '</text>';
        }
        out += '<text class="axis-label" x="' + (p.x + p.w) + '" y="' + (p.y + p.h + 28) + '" text-anchor="end">' +
            esc(unitLabel || 'time (5-minute intervals, 09:00 to 15:00)') + '</text>';
        return out;
    }

    function linePath(values, p, max) {
        var pts = [];
        for (var i = 0; i < values.length; i++) pts.push([xAt(p, i), yAt(p, values[i], max)]);
        return path(pts);
    }
    function areaPath(values, p, max) {
        var pts = [];
        for (var i = 0; i < values.length; i++) pts.push([xAt(p, i), yAt(p, values[i], max)]);
        pts.push([xAt(p, values.length - 1), p.y + p.h]);
        pts.unshift([xAt(p, 0), p.y + p.h]);
        return path(pts) + 'Z';
    }

    function lineSeries(values, p, max, color, width) {
        return '<path d="' + linePath(values, p, max) + '" fill="none" stroke="' + color + '" stroke-width="' + (width || 2) + '" stroke-linejoin="round"/>';
    }
    function areaSeries(values, p, max, color, opacity) {
        return '<path d="' + areaPath(values, p, max) + '" fill="' + color + '" fill-opacity="' + (opacity === undefined ? 0.5 : opacity) + '" stroke="none"/>';
    }

    function stackedBands(seriesList, colors, p, max, opacity) {
        var cum = zeros(INTERVALS);
        var out = '';
        seriesList.forEach(function (series, si) {
            var top = zeros(INTERVALS);
            var i;
            for (i = 0; i < INTERVALS; i++) top[i] = cum[i] + series[i];
            var pts = [];
            for (i = 0; i < INTERVALS; i++) pts.push([xAt(p, i), yAt(p, top[i], max)]);
            for (i = INTERVALS - 1; i >= 0; i--) pts.push([xAt(p, i), yAt(p, cum[i], max)]);
            out += '<path d="' + path(pts) + 'Z" fill="' + colors[si] + '" fill-opacity="' + (opacity === undefined ? 0.78 : opacity) + '" stroke="none"/>';
            var topPts = [];
            for (i = 0; i < INTERVALS; i++) topPts.push([xAt(p, i), yAt(p, top[i], max)]);
            out += '<path d="' + path(topPts) + '" fill="none" stroke="' + colors[si] + '" stroke-width="1.2"/>';
            cum = top;
        });
        return out;
    }

    function barSeries(values, p, max, color) {
        var n = values.length;
        var slot = p.w / n;
        var bw = Math.max(1, slot - 1.5);
        var out = '';
        for (var i = 0; i < n; i++) {
            var h = (values[i] / max) * p.h;
            var x = p.x + i * slot + (slot - bw) / 2;
            out += '<rect x="' + x.toFixed(2) + '" y="' + (p.y + p.h - h).toFixed(2) + '" width="' + bw.toFixed(2) +
                '" height="' + h.toFixed(2) + '" fill="' + color + '"/>';
        }
        return out;
    }

    function stackedBars(seriesList, colors, p, max) {
        var slot = p.w / INTERVALS;
        var bw = Math.max(1, slot - 1.5);
        var out = '';
        for (var i = 0; i < INTERVALS; i++) {
            var acc = 0;
            for (var s = 0; s < seriesList.length; s++) {
                var v = seriesList[s][i];
                if (!v) continue;
                var yTop = yAt(p, acc + v, max);
                var yBot = yAt(p, acc, max);
                out += '<rect x="' + (p.x + i * slot + (slot - bw) / 2).toFixed(2) + '" y="' + yTop.toFixed(2) +
                    '" width="' + bw.toFixed(2) + '" height="' + Math.max(0, yBot - yTop).toFixed(2) + '" fill="' + colors[s] + '"/>';
                acc += v;
            }
        }
        return out;
    }

    function chartCard(id, question, meta, svg, foot, controls, wide) {
        return '<section class="card chart-card' + (wide ? ' wide' : '') + '" id="' + id + '">' +
            '<header class="charthead"><div><h2>' + esc(question) + '</h2><span class="meta">' + esc(meta) + '</span></div>' +
            (controls ? '<div class="controls">' + controls + '</div>' : '') + '</header>' +
            svg + (foot || '') + '</section>';
    }

    function miniLegend(items) {
        return '<div class="legend">' + items.map(function (it) {
            return '<span class="legend-item"><span class="swatch" style="background:' + it.color + '"></span>' + esc(it.label) + '</span>';
        }).join('') + '</div>';
    }

    // ============================================================ the charts

    /* 1. Node load heatmap: one row per node, one column per interval. */
    function heatmapChart(metric, wide) {
        var work = metric !== 'failures';
        var values = NODES.map(function (n) { return work ? n.work : n.fails; });
        var max = maxOfMatrix(values) || 1;
        var w = 1000, rowH = 15, left = 74, right = 96, top = 14, bottom = 36;
        var p = { x: left, y: top, w: w - left - right, h: NODES.length * rowH };
        var h = p.y + p.h + bottom;
        var cellW = p.w / INTERVALS;
        var unit = work ? 'tasks' : 'failed tasks';
        var out = '';

        NODES.forEach(function (node, r) {
            out += '<text class="tick rowlabel" x="' + (p.x - 6) + '" y="' + (p.y + r * rowH + rowH * 0.74).toFixed(1) + '">' + esc(node.short) + '</text>';
            for (var i = 0; i < INTERVALS; i++) {
                var v = values[r][i];
                var t = work ? Math.pow(v / max, 0.75) : Math.pow(v / max, 0.6);
                out += '<rect class="cell" x="' + (p.x + i * cellW).toFixed(2) + '" y="' + (p.y + r * rowH).toFixed(2) +
                    '" width="' + (cellW - 0.5).toFixed(2) + '" height="' + (rowH - 1).toFixed(2) + '" fill="' + rampCss(t) + '">' +
                    '<title>' + esc(node.short + ' \u00b7 ' + intervalTime(i) + ' \u00b7 ' + v + ' ' + unit) + '</title></rect>';
            }
        });
        out += timeAxis(p, 'time (5-minute intervals, 09:00 to 15:00)');

        var lx = p.x + p.w + 16;
        var steps = 28;
        for (var s = 0; s < steps; s++) {
            out += '<rect x="' + lx + '" y="' + (p.y + (s / steps) * p.h).toFixed(1) + '" width="11" height="' + (p.h / steps + 1).toFixed(1) +
                '" fill="' + rampCss(1 - s / steps) + '"/>';
        }
        out += '<text class="tick start" x="' + (lx + 15) + '" y="' + (p.y + 8) + '">' + fmtInt(max) + '</text>';
        out += '<text class="tick start" x="' + (lx + 15) + '" y="' + (p.y + p.h / 2 + 4) + '">' + fmtInt(max / 2) + '</text>';
        out += '<text class="tick start" x="' + (lx + 15) + '" y="' + (p.y + p.h) + '">0</text>';
        out += '<text class="axis-label" x="' + lx + '" y="' + (p.y - 5) + '">' + esc(unit) + '</text>';

        var question = work ? 'Which node was hot, and when?' : 'Where is it failing?';
        var meta = work
            ? 'Work done per node per interval, tasks per 5 minutes'
            : 'Failed tasks per node per interval, failures per 5 minutes';
        var controls = '<button class="btn small' + (work ? ' on' : '') + '" data-act="metric" data-metric="work">Work</button>' +
            '<button class="btn small' + (!work ? ' on' : '') + '" data-act="metric" data-metric="failures">Failures</button>';
        var foot = '<p class="rowfoot">One row per node, one column per 5-minute interval, colour from the viridis ramp: ' +
            'dark is no work, light is the busiest interval on any node. The same chart carries a different metric, ' +
            'which is how compute.org asks for it: the chart type does not change, only the number it colours.</p>';
        return chartCard('heatmap', question, meta, svgEl(w, h, out, 'heat', question + ', ' + meta), foot, controls, wide);
    }

    /* 2. Node state ribbon: one row per node, bands over time. */
    function ribbonChart() {
        var colors = { running: C.teal, draining: C.yellow, lost: C.vermillion };
        var w = 1000, rowH = 15, left = 74, right = 20, top = 14, bottom = 36;
        var p = { x: left, y: top, w: w - left - right, h: NODES.length * rowH };
        var h = p.y + p.h + bottom;
        var cellW = p.w / INTERVALS;
        var out = '';
        NODES.forEach(function (node, r) {
            out += '<text class="tick rowlabel" x="' + (p.x - 6) + '" y="' + (p.y + r * rowH + rowH * 0.74).toFixed(1) + '">' + esc(node.short) + '</text>';
            var start = 0;
            for (var i = 1; i <= INTERVALS; i++) {
                var changed = (i === INTERVALS) || node.state[i] !== node.state[start];
                if (changed) {
                    out += '<rect class="cell" x="' + (p.x + start * cellW).toFixed(2) + '" y="' + (p.y + r * rowH).toFixed(2) +
                        '" width="' + ((i - start) * cellW - 0.5).toFixed(2) + '" height="' + (rowH - 1).toFixed(2) +
                        '" fill="' + colors[node.state[start]] + '" fill-opacity="0.85"><title>' +
                        esc(node.short + ' \u00b7 ' + node.state[start] + ' \u00b7 ' + intervalTime(start) + ' to ' + intervalTime(Math.min(i, INTERVALS - 1))) +
                        '</title></rect>';
                    start = i;
                }
            }
        });
        out += timeAxis(p, 'time (5-minute intervals, 09:00 to 15:00)');
        var question = 'Which node was quiet, lost or draining?';
        var meta = 'Node state per interval, bands over time';
        var foot = '<p class="rowfoot">A node keeps its row while it is quiet: grid-12 is lost from ' + esc(intervalTime(40)) +
            ' and grid-07 drains from ' + esc(intervalTime(30)) + '. The band is the state, so a gap in the work still shows as a node.</p>' +
            miniLegend([
                { label: 'running', color: C.teal },
                { label: 'draining', color: C.yellow },
                { label: 'lost', color: C.vermillion }
            ]);
        return chartCard('ribbon', question, meta, svgEl(w, h, out, 'ribbon', question + ', ' + meta), foot, '', true);
    }

    /* 3. Load over time: stacked area across the result states. */
    function loadChart() {
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 40);
        var series = [FLEET.done, FLEET.inProgress, FLEET.unsent, FLEET.inactive];
        var colors = [C.blue, C.sky, C.grey, C.greyDark];
        var totals = zeros(INTERVALS);
        series.forEach(function (s) { for (var i = 0; i < INTERVALS; i++) totals[i] += s[i]; });
        var max = niceMax(maxOf(totals));
        var out = yAxis(p, max, 'workunits', 4) + timeAxis(p) + stackedBands(series, colors, p, max);
        var question = 'How much work is in flight, and of what sort?';
        var meta = 'Workunits by result state per interval, stacked, count';
        var foot = '<p class="rowfoot">The result states stack from done at the bottom to inactive at the top. ' +
            'The widest band is work already done that the grid still counts, not a queue.</p>' +
            miniLegend([
                { label: 'done', color: C.blue },
                { label: 'in progress', color: C.sky },
                { label: 'unsent', color: C.grey },
                { label: 'inactive', color: C.greyDark }
            ]);
        return chartCard('load', question, meta, svgEl(w, h, out, 'load', question + ', ' + meta), foot);
    }

    /* 4. Fleet size: area over total, online and idle. */
    function fleetSizeChart() {
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 40);
        var max = niceMax(NODES.length);
        var out = yAxis(p, max, 'nodes', 4) + timeAxis(p);
        out += areaSeries(FLEET.total, p, max, C.blue, 0.06);
        out += areaSeries(FLEET.online, p, max, C.teal, 0.22);
        out += areaSeries(FLEET.idle, p, max, C.grey, 0.30);
        out += lineSeries(FLEET.total, p, max, C.blue, 1.6);
        out += lineSeries(FLEET.online, p, max, C.teal, 2);
        out += lineSeries(FLEET.idle, p, max, C.grey, 2);
        var question = 'How many nodes do I have, and how many are up?';
        var meta = 'Nodes per interval, count, total against online and idle';
        var foot = '<p class="rowfoot">Total is the registered fleet, online is every node still heartbeating, ' +
            'and idle is a node reporting with almost no work. The three answer different questions: a node may be up and idle.</p>' +
            miniLegend([
                { label: 'total registered', color: C.blue },
                { label: 'online', color: C.teal },
                { label: 'idle', color: C.grey }
            ]);
        return chartCard('fleet', question, meta, svgEl(w, h, out, 'fleet', question + ', ' + meta), foot);
    }

    /* 5. Throughput: bars per interval. */
    function throughputChart() {
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 40);
        var max = niceMax(maxOf(FLEET.throughput));
        var out = yAxis(p, max, 'tasks', 4) + timeAxis(p) + barSeries(FLEET.throughput, p, max, C.blue);
        var question = 'How much is finishing?';
        var meta = 'Tasks completed per interval, tasks per 5 minutes';
        var foot = '<p class="rowfoot">The bar at ' + esc(intervalTime(49)) + ' is the spike: a batch of ' +
            fmtInt(FLEET.throughput[49] - FLEET.throughput[10]) + ' more tasks than the same shape an hour earlier would carry.</p>';
        return chartCard('throughput', question, meta, svgEl(w, h, out, 'throughput', question + ', ' + meta), foot);
    }

    /* 6. Outcomes: stacked bars over success, client error, no reply. */
    function outcomesChart() {
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 40);
        var series = [FLEET.success, FLEET.clientError, FLEET.noReply];
        var colors = [C.green, C.vermillion, C.amber];
        var totals = zeros(INTERVALS);
        series.forEach(function (s) { for (var i = 0; i < INTERVALS; i++) totals[i] += s[i]; });
        var max = niceMax(maxOf(totals));
        var out = yAxis(p, max, 'results', 4) + timeAxis(p) + stackedBars(series, colors, p, max);
        var question = 'Is it succeeding?';
        var meta = 'Results per interval by outcome, stacked, count';
        var foot = '<p class="rowfoot">Success is green, a client error is red, and a job that got no reply is amber. ' +
            'No reply is not a failure of the work: the wrapper stopped reporting and the result was never seen. ' +
            'The failure bars sit late because the bad release was submitted late (see Diagnose a failure).</p>' +
            miniLegend([
                { label: 'success', color: C.green },
                { label: 'client error', color: C.vermillion },
                { label: 'no reply', color: C.amber }
            ]);
        return chartCard('outcomes', question, meta, svgEl(w, h, out, 'outcomes', question + ', ' + meta), foot);
    }

    /* 7. Queue depth: line over unsent and in-progress. */
    function queueChart() {
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 40);
        var max = niceMax(Math.max(maxOf(FLEET.unsent), maxOf(FLEET.inProgress)));
        var out = yAxis(p, max, 'workunits', 4) + timeAxis(p);
        out += areaSeries(FLEET.unsent, p, max, C.amber, 0.16);
        out += areaSeries(FLEET.inProgress, p, max, C.sky, 0.16);
        out += lineSeries(FLEET.unsent, p, max, C.amber, 2);
        out += lineSeries(FLEET.inProgress, p, max, C.sky, 2);
        var question = 'Is the grid keeping up?';
        var meta = 'Workunits waiting and in progress per interval, count';
        var foot = '<p class="rowfoot">Unsent work keeps rising after the spike at ' + esc(intervalTime(49)) +
            ' while in-progress work falls: the grid is taking work faster than it hands it back. ' +
            'A rising unsent line with a flat in-progress line is the signature of a full queue.</p>' +
            miniLegend([
                { label: 'unsent', color: C.amber },
                { label: 'in progress', color: C.sky }
            ]);
        return chartCard('queue', question, meta, svgEl(w, h, out, 'queue', question + ', ' + meta), foot);
    }

    /* 8. Per-node sparkline, drawn inside each node table row. */
    function sparkline(values, width, height) {
        var lowest = Math.min.apply(null, values);
        var highest = Math.max.apply(null, values);
        var span = highest - lowest === 0 ? 1 : highest - lowest;
        var pts = values.map(function (value, index) {
            var x = (index / (values.length - 1)) * width;
            var y = height - ((value - lowest) / span) * (height - 6) - 3;
            return pt(x, y);
        }).join(' ');
        return '<svg viewBox="0 0 ' + width + ' ' + height + '" class="chart" role="img" style="margin:0;width:' + width + 'px;max-width:100%">' +
            '<title>Work over the last six hours.</title>' +
            '<polyline points="' + pts + '" fill="none" stroke="' + C.sky + '" stroke-width="1.4"></polyline>' +
            '</svg>';
    }

    function nodeTablePanel() {
        var failTotals = nodeFailureTotals();
        var rows = NODES.map(function (node, ni) {
            var done = node.work.reduce(function (a, b) { return a + b; }, 0);
            var fails = failTotals[ni];
            var lost = node.state[INTERVALS - 1] === 'lost';
            var draining = node.state[INTERVALS - 1] === 'draining';
            var stateTag = lost ? '<span class="tag bad">lost</span>'
                : draining ? '<span class="tag warn">draining</span>'
                    : '<span class="tag ok">running</span>';
            return '<tr>' +
                '<td><span class="cellgroup"><span class="mono">' + esc(node.host) + '</span>' +
                (node.hot ? '<span class="tag warn">hot</span>' : '') +
                (node.slow ? '<span class="tag warn">slow</span>' : '') +
                (node.wrapperVersion !== 'v0.0.25' ? '<span class="tag muted">old build</span>' : '') +
                '</span></td>' +
                '<td class="code">' + node.cores + '</td>' +
                '<td>' + sparkline(node.work, 120, 22) + '</td>' +
                '<td class="code">' + num(done) + '</td>' +
                '<td class="code">' + (fails > 0 ? '<span class="tag bad">' + fails + '</span>' : '0') + '</td>' +
                '<td class="code">' + fmtInt(250 * (node.slow ? 1.65 : 1)) + ' s</td>' +
                '<td>' + stateTag + '</td>' +
                '<td class="code">' + esc(node.wrapperVersion) + '</td>' +
                '</tr>';
        }).join('');
        return '<section class="card wide">' +
            '<header class="sectionhead"><h2>Which row of this table is the problem?</h2>' +
            '<span class="faint" style="font-size:12px">' + NODES.length + ' nodes, one row each, with a work trend in the row</span></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Node</th><th>Cores</th><th>Work, 09:00 to 15:00</th><th>Tasks done</th><th>Failed</th>' +
            '<th>Mean task time</th><th>State</th><th>Wrapper release</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="rowfoot">The sparkline answers the table\u2019s own question: a row whose trend falls away is the node to open. ' +
            'The fleet is the installation\u2019s, so these rows do not narrow with the tenant variant. ' +
            'The wrapper release comes from the heartbeat, which carries no host, so the join is a fixture here and a gap in the read (see Keep what it runs).</p>' +
            '</section>';
    }

    /* 9. Job timeline: one row per job, a bar per run, the cluster Gantt. */
    function ganttPanel(jobs) {
        var rows = jobs.slice().sort(function (a, b) { return a.submit - b.submit || (a.id < b.id ? -1 : 1); });
        var w = 1000, rowH = 18, left = 92, right = 92, top = 16, bottom = 40;
        var p = { x: left, y: top, w: w - left - right, h: rows.length * rowH };
        var h = p.y + p.h + bottom;
        var out = '';
        var colors = { done: C.teal, failed: C.vermillion, running: C.sky, queued: C.grey, aborted: C.amber };
        rows.forEach(function (job, r) {
            out += '<text class="tick rowlabel" x="' + (p.x - 6) + '" y="' + (p.y + r * rowH + rowH * 0.72).toFixed(1) + '">' + esc(job.id) + '</text>';
            job.attempts.forEach(function (run) {
                var x0 = p.x + (run.start / (INTERVALS - 1)) * p.w;
                var x1 = p.x + (run.end / (INTERVALS - 1)) * p.w;
                var bw = Math.max(2, x1 - x0);
                out += '<rect x="' + x0.toFixed(2) + '" y="' + (p.y + r * rowH + 3).toFixed(2) + '" width="' + bw.toFixed(2) +
                    '" height="' + (rowH - 7) + '" rx="2" fill="' + colors[run.state] + '" fill-opacity="0.9"><title>' +
                    esc(job.id + ' \u00b7 ' + job.app + ' ' + job.version + ' \u00b7 ' + run.node + ' \u00b7 ' + intervalTime(run.start) + ' to ' + intervalTime(Math.min(run.end, INTERVALS - 1))) +
                    '</title></rect>';
            });
        });
        out += timeAxis(p, 'time (5-minute intervals, 09:00 to 15:00)');
        var question = 'What ran when, and what overlapped?';
        var meta = rows.length + ' jobs, one row per job, one bar per run, the cluster Gantt';
        var foot = '<p class="rowfoot">A row with two bars is a job that ran twice: the amber bar is the abandoned attempt. ' +
            'The failed runs cluster after ' + esc(intervalTime(BAD_VERSION_FIRST_SUBMIT)) + ', where the bad release was submitted. ' +
            'Rows are ordered by submit time, so overlap reads as rows sharing a column.</p>' +
            miniLegend([
                { label: 'done', color: C.teal },
                { label: 'running', color: C.sky },
                { label: 'failed', color: C.vermillion },
                { label: 'queued', color: C.grey },
                { label: 'abandoned', color: C.amber }
            ]);
        return chartCard('gantt', question, meta, svgEl(w, h, out, 'gantt', question + ', ' + meta), foot, '', true);
    }

    /* 10. Job waterfall: where one job's time went. */
    function waterfallChart(job) {
        var w = 1000, h = 260, left = 96, right = 90, top = 30, bottom = 44;
        var p = { x: left, y: top, w: w - left - right, h: h - top - bottom };
        var total = job.phases.reduce(function (a, ph) { return a + ph.sec; }, 0) || 1;
        var max = niceMax(total);
        var out = '';
        for (var t = 0; t <= 4; t++) {
            var v = max * t / 4;
            var gx = p.x + (v / max) * p.w;
            out += '<line class="grid" x1="' + gx.toFixed(1) + '" y1="' + p.y + '" x2="' + gx.toFixed(1) + '" y2="' + (p.y + p.h) + '"/>';
            out += '<text class="tick mid" x="' + gx.toFixed(1) + '" y="' + (p.y + p.h + 14) + '">' + fmtAxis(v) + '</text>';
        }
        out += '<text class="axis-label" x="' + (p.x + p.w) + '" y="' + (p.y + p.h + 28) + '" text-anchor="end">phase duration (seconds), the bars run left to right</text>';
        var x = p.x;
        job.phases.forEach(function (ph, i) {
            var bw = ph.sec / max * p.w;
            var y = p.y + 26 + i * ((p.h - 40) / job.phases.length);
            var bh = (p.h - 40) / job.phases.length - 6;
            out += '<rect x="' + x.toFixed(2) + '" y="' + y.toFixed(2) + '" width="' + Math.max(0.6, bw).toFixed(2) + '" height="' + bh.toFixed(2) +
                '" rx="2" fill="' + [C.grey, C.sky, C.blue, C.teal, C.violet, C.amber][i] + '" fill-opacity="0.9"><title>' +
                esc(ph.name + ' \u00b7 ' + ph.sec.toFixed(1) + ' s') + '</title></rect>';
            out += '<text class="tick rowlabel" x="' + (left - 10) + '" y="' + (y + bh / 2 + 4).toFixed(1) + '">' + esc(ph.name) + '</text>';
            out += '<text class="start" x="' + (x + bw + 6).toFixed(1) + '" y="' + (y + bh / 2 + 4).toFixed(1) + '">' + ph.sec.toFixed(1) + ' s</text>';
            x += bw;
        });
        var totalX = p.x + (total / max) * p.w;
        out += '<line class="marker" x1="' + totalX.toFixed(1) + '" y1="' + (p.y + 14) + '" x2="' + totalX.toFixed(1) + '" y2="' + (p.y + p.h) + '"/>';
        out += '<text class="tick start strong" x="' + (totalX + 6).toFixed(1) + '" y="' + (p.y + 12) + '">total ' + asDuration(total) + '</text>';

        var question = 'Where did this job\u2019s time go?';
        var meta = job.id + ' \u00b7 ' + job.app + ' ' + job.version + ' on ' + job.node + ', seconds per phase';
        var controls = filteredJobs().filter(function (j) { return j.state === 'failed' || j.state === 'done'; }).slice(0, 5).map(function (j) {
            return '<button class="btn small' + (j.id === job.id ? ' on' : '') + '" data-act="job" data-job="' + esc(j.id) + '">' + esc(j.id) + '</button>';
        }).join('');
        var foot = '<p class="rowfoot">The phases run left to right in the order the lifecycle records them: queued, dispatched, downloaded, ' +
            'running, uploaded, validated. Running is the work; the bar beside it is the time the grid spent moving data and waiting. ' +
            'A job with a long downloaded bar waited on its inputs, not on a node.</p>';
        return chartCard('waterfall', question, meta, svgEl(w, h, out, 'waterfall', question + ', ' + meta), foot, controls, true);
    }

    /* 11. Duration distribution: histogram with p50/p90/p99 marked. */
    function histogramChart(jobs) {
        var durations = jobs.map(function (j) { return j.durationSec; }).sort(function (a, b) { return a - b; });
        var p50 = percentile(durations, 0.50), p90 = percentile(durations, 0.90), p99 = percentile(durations, 0.99);
        var bucket = 100;
        var maxSec = Math.max(p99 * 1.2, 600);
        var count = Math.ceil(maxSec / bucket);
        var buckets = zeros(count);
        durations.forEach(function (d) {
            var b = Math.min(count - 1, Math.floor(d / bucket));
            buckets[b]++;
        });
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 40);
        var max = niceMax(maxOf(buckets));
        var out = yAxis(p, max, 'jobs', 4);
        var slot = p.w / count;
        var bw = Math.max(2, slot - 2);
        for (var i = 0; i < count; i++) {
            var bh = (buckets[i] / max) * p.h;
            out += '<rect x="' + (p.x + i * slot + (slot - bw) / 2).toFixed(2) + '" y="' + (p.y + p.h - bh).toFixed(2) +
                '" width="' + bw.toFixed(2) + '" height="' + bh.toFixed(2) + '" fill="' + C.blue + '"><title>' +
                esc((i * bucket) + ' to ' + ((i + 1) * bucket) + ' s \u00b7 ' + buckets[i] + ' jobs') + '</title></rect>';
        }
        [['p50', p50, 0], ['p90', p90, 1], ['p99', p99, 2]].forEach(function (m) {
            var x = p.x + (m[1] / maxSec) * p.w;
            if (x > p.x + p.w) x = p.x + p.w;
            out += '<line class="marker" x1="' + x.toFixed(1) + '" y1="' + p.y + '" x2="' + x.toFixed(1) + '" y2="' + (p.y + p.h) + '"/>';
            out += '<text class="tick mid strong" x="' + x.toFixed(1) + '" y="' + (p.y + 12 + m[2] * 14) + '">' + m[0] + ' ' + asDuration(m[1]) + '</text>';
        });
        for (var t = 0; t <= count; t += 3) {
            var tx = p.x + (t / count) * p.w;
            out += '<text class="tick mid" x="' + tx.toFixed(1) + '" y="' + (p.y + p.h + 14) + '">' + (t * bucket) + '</text>';
        }
        out += '<text class="axis-label" x="' + (p.x + p.w) + '" y="' + (p.y + p.h + 28) + '" text-anchor="end">job duration (seconds)</text>';

        var question = 'Is this job normal?';
        var meta = durations.length + ' jobs, 100-second buckets, with p50, p90 and p99 marked';
        var foot = '<p class="rowfoot">Half the jobs finish in ' + asDuration(p50) + ', nine in ten in ' + asDuration(p90) +
            ', and the tail at ' + asDuration(p99) + ' is the report runs. A job at p99 is normal for its app and not for the fleet.</p>';
        return chartCard('hist', question, meta, svgEl(w, h, out, 'hist', question + ', ' + meta), foot);
    }

    /* 12. Duration by node: a box per node. */
    function durationByNodeChart() {
        var w = 1000, h = 280;
        var left = 58, right = 16, top = 30, bottom = 52;
        var p = { x: left, y: top, w: w - left - right, h: h - top - bottom };
        var all = [];
        NODE_DURATIONS.forEach(function (s) { all = all.concat(s); });
        var max = niceMax(Math.max.apply(null, all));
        var out = yAxis(p, max, 'task duration (seconds)', 4);
        var slot = p.w / NODES.length;
        var bw = Math.min(34, slot - 10);
        NODES.forEach(function (node, i) {
            var s = NODE_DURATIONS[i];
            var lo = s[0], hi = s[s.length - 1];
            var q1 = percentile(s, 0.25), med = percentile(s, 0.5), q3 = percentile(s, 0.75);
            var cx = p.x + i * slot + slot / 2;
            var yLo = yAt(p, lo, max), yHi = yAt(p, hi, max);
            var yQ1 = yAt(p, q1, max), yQ3 = yAt(p, q3, max), yMed = yAt(p, med, max);
            var color = node.slow ? C.amber : C.blue;
            out += '<line class="grid" x1="' + cx.toFixed(1) + '" y1="' + yHi.toFixed(1) + '" x2="' + cx.toFixed(1) + '" y2="' + yLo.toFixed(1) + '" stroke="' + color + '" stroke-width="1.2"/>';
            out += '<rect x="' + (cx - bw / 2).toFixed(1) + '" y="' + yQ3.toFixed(1) + '" width="' + bw.toFixed(1) + '" height="' + (yQ1 - yQ3).toFixed(1) +
                '" fill="' + color + '" fill-opacity="0.28" stroke="' + color + '"><title>' +
                esc(node.short + ' \u00b7 p25 ' + asDuration(q1) + ' \u00b7 p50 ' + asDuration(med) + ' \u00b7 p75 ' + asDuration(q3)) + '</title></rect>';
            out += '<line x1="' + (cx - bw / 2).toFixed(1) + '" y1="' + yMed.toFixed(1) + '" x2="' + (cx + bw / 2).toFixed(1) + '" y2="' + yMed.toFixed(1) + '" stroke="' + color + '" stroke-width="2.4"/>';
            out += '<text class="tick mid" x="' + cx.toFixed(1) + '" y="' + (p.y + p.h + 13) + '" transform="rotate(-45 ' + cx.toFixed(1) + ' ' + (p.y + p.h + 13) + ')">' + esc(node.short) + '</text>';
        });
        out += '<text class="axis-label" x="' + (p.x + p.w) + '" y="' + (p.y + p.h + 48) + '" text-anchor="end">node (one box per node, 14 to 31 task samples)</text>';
        var question = 'Is one node slow?';
        var meta = 'Task duration per node, box p25 to p75, line median, whisker min to max, seconds';
        var foot = '<p class="rowfoot">' + esc(STORY.slow) + '. Its box sits above the fleet, which is the answer: the work is the same ' +
            'and the node is the difference. The nodes are the installation\u2019s, so this panel does not narrow with the tenant variant.</p>' +
            miniLegend([
                { label: 'node within the fleet', color: C.blue },
                { label: 'node flagged slow', color: C.amber }
            ]);
        return chartCard('bynode', question, meta, svgEl(w, h, out, 'bynode', question + ', ' + meta), foot, '', true);
    }

    /* 13. Failure rate over time. */
    function failureRateChart() {
        var values = zeros(INTERVALS);
        for (var i = 0; i < INTERVALS; i++) {
            values[i] = FLEET.throughput[i] > 0 ? (FLEET.failures[i] / FLEET.throughput[i]) * 100 : 0;
        }
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 40);
        var max = niceMax(Math.max(5, maxOf(values)));
        var out = yAxis(p, max, 'failure rate (%)', 4, function (v) { return v.toFixed(0) + '%'; }) + timeAxis(p);
        out += areaSeries(values, p, max, C.vermillion, 0.18);
        out += lineSeries(values, p, max, C.vermillion, 2);
        var mx = xAt(p, BAD_VERSION_FIRST_SUBMIT);
        out += '<line class="marker" x1="' + mx.toFixed(1) + '" y1="' + p.y + '" x2="' + mx.toFixed(1) + '" y2="' + (p.y + p.h) + '"/>';
        out += '<text class="tick start strong" x="' + (mx + 4).toFixed(1) + '" y="' + (p.y + 12) + '">' + esc('v2.4.1 first submitted ' + intervalTime(BAD_VERSION_FIRST_SUBMIT)) + '</text>';
        var question = 'When did it start?';
        var meta = 'Failed tasks as a share of completed tasks, per interval, percent';
        var foot = '<p class="rowfoot">The line runs near zero until the bad release was submitted, then the bursts arrive with it. ' +
            'That shape is the finding: the failures arrived with a release, not with a node. The line is the fleet\u2019s own task failures, ' +
            'so it stays whole in the tenant variant while the job rows below it narrow.</p>';
        return chartCard('failtime', question, meta, svgEl(w, h, out, 'failtime', question + ', ' + meta), foot, '', true);
    }

    /* 14. Failures by node. */
    function failuresByNodeChart() {
        var values = nodeFailureTotals();
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 44);
        var max = niceMax(Math.max(1, maxOf(values)));
        var out = yAxis(p, max, 'failed tasks', 4);
        var slot = p.w / NODES.length;
        var bw = Math.min(40, slot - 14);
        var top3 = values.slice().sort(function (a, b) { return b - a; }).slice(0, 3);
        NODES.forEach(function (node, i) {
            var v = values[i];
            var color = top3.indexOf(v) >= 0 && v > 0 ? C.vermillion : C.grey;
            var bh = (v / max) * p.h;
            var cx = p.x + i * slot + slot / 2;
            out += '<rect x="' + (cx - bw / 2).toFixed(1) + '" y="' + (p.y + p.h - bh).toFixed(1) + '" width="' + bw.toFixed(1) + '" height="' + bh.toFixed(1) +
                '" fill="' + color + '"><title>' + esc(node.short + ' \u00b7 ' + v + ' failed tasks') + '</title></rect>';
            if (v > 0) out += '<text class="tick mid strong" x="' + cx.toFixed(1) + '" y="' + (p.y + p.h - bh - 5).toFixed(1) + '">' + v + '</text>';
            out += '<text class="tick mid" x="' + cx.toFixed(1) + '" y="' + (p.y + p.h + 13) + '" transform="rotate(-45 ' + cx.toFixed(1) + ' ' + (p.y + p.h + 13) + ')">' + esc(node.short) + '</text>';
        });
        out += '<text class="axis-label" x="' + (p.x + p.w) + '" y="' + (p.y + p.h + 42) + '" text-anchor="end">node (failed tasks over the range)</text>';
        var question = 'Is one node bad?';
        var meta = 'Failed tasks per node over the range, count';
        var foot = '<p class="rowfoot">The bars are task failures. The three failed jobs each fail every task they touch for a few intervals, ' +
            'so their nodes carry a burst, and the rest is the low background that follows the work. ' +
            'A bar per node looks like a bad node; the release panel below says which it is.</p>' +
            miniLegend([
                { label: 'node with failures', color: C.vermillion },
                { label: 'node with none', color: C.grey }
            ]);
        return chartCard('failnode', question, meta, svgEl(w, h, out, 'failnode', question + ', ' + meta), foot);
    }

    /* 15. Failures by app version. */
    function failuresByVersionChart() {
        var visible = filteredJobs();
        var rows = APP_VERSIONS.map(function (av) {
            var jobs = visible.filter(function (j) { return j.app === av.app && j.version === av.version; });
            var failed = jobs.filter(function (j) { return j.state === 'failed'; }).length;
            return { label: av.app.replace('ores.', '') + ' ' + av.version, failed: failed, total: jobs.length, rate: jobs.length ? failed / jobs.length : 0, bad: av.bad };
        }).filter(function (r) { return r.total > 0; })
            .sort(function (a, b) { return b.rate - a.rate; });
        var w = 1000, h = 320, left = 190, right = 90, top = 16, bottom = 34;
        var p = { x: left, y: top, w: w - left - right, h: rows.length * 26 };
        var h2 = p.y + p.h + bottom;
        var maxRate = Math.max(0.5, Math.max.apply(null, rows.map(function (r) { return r.rate; })));
        var out = '';
        rows.forEach(function (row, i) {
            var y = p.y + i * 26;
            var bw = (row.rate / maxRate) * p.w;
            out += '<text class="tick rowlabel" x="' + (left - 8) + '" y="' + (y + 15) + '">' + esc(row.label) + '</text>';
            out += '<rect x="' + p.x + '" y="' + (y + 4) + '" width="' + p.w + '" height="18" fill="' + C.greyDark + '" fill-opacity="0.18"/>';
            out += '<rect x="' + p.x + '" y="' + (y + 4) + '" width="' + Math.max(1, bw).toFixed(1) + '" height="18" fill="' + (row.bad ? C.vermillion : C.grey) + '"><title>' +
                esc(row.label + ' \u00b7 ' + row.failed + ' of ' + row.total + ' jobs failed') + '</title></rect>';
            out += '<text class="tick start' + (row.bad ? ' strong' : '') + '" x="' + (p.x + p.w + 8) + '" y="' + (y + 17) + '">' +
                pct(row.rate) + ' (' + row.failed + ' of ' + row.total + ')</text>';
        });
        out += '<text class="axis-label" x="' + p.x + '" y="' + (p.y - 4) + '">failure rate, jobs failed as a share of jobs run</text>';
        var badRow = rows.filter(function (r) { return r.bad; })[0];
        var question = 'Is one release bad?';
        var meta = visible.length + ' jobs across ' + rows.length + ' app versions, failure rate and job count';
        var foot = '<p class="rowfoot">' +
            (badRow
                ? 'The bad release is ' + esc(badRow.label) + ' at ' + pct(badRow.rate) + ' (' + badRow.failed + ' of ' + badRow.total + ' jobs). ' +
                  'Every other release ran at zero failures over the range, so the release is the cause and the fleet is not.'
                : 'No release in this view failed a job over the range.') +
            ' The count is printed beside the rate, because a rate over one job says nothing.</p>' +
            miniLegend([
                { label: 'release with failures', color: C.vermillion },
                { label: 'release with none', color: C.grey }
            ]);
        return chartCard('failversion', question, meta, svgEl(w, h2, out, 'failversion', question + ', ' + meta), foot, '', true);
    }

    /* 16. Capacity headroom: used against total, per resource. */
    function capacityChart() {
        var minis = [
            { name: 'cores', used: FLEET.coresUsed, total: FLEET.coresTotal, unit: 'cores' },
            { name: 'memory', used: FLEET.memUsed, total: FLEET.memTotal, unit: 'GiB' },
            { name: 'GPU', used: FLEET.gpuUsed, total: FLEET.gpuTotal, unit: 'GPUs' }
        ];
        var w = 1000, miniH = 150, out = '';
        var last = INTERVALS - 1;
        minis.forEach(function (mini, idx) {
            var top = idx * miniH + 14;
            var p = { x: 78, y: top, w: w - 78 - 90, h: miniH - 54 };
            var max = niceMax(mini.total * 1.15);
            out += '<text class="start strong" x="' + p.x + '" y="' + (top - 4) + '">' + esc(mini.name) +
                ' \u00b7 ' + num(mini.used[last]) + ' of ' + num(mini.total) + ' ' + esc(mini.unit) +
                ' used (' + pct(mini.used[last] / (mini.total || 1)) + ')</text>';
            out += yAxis(p, max, mini.unit, 2);
            out += areaSeries(mini.used, p, max, mini.name === 'GPU' ? C.violet : C.blue, 0.35);
            out += lineSeries(mini.used, p, max, mini.name === 'GPU' ? C.violet : C.blue, 1.8);
            out += '<line class="marker" x1="' + p.x + '" y1="' + yAt(p, mini.total, max).toFixed(1) + '" x2="' + (p.x + p.w) + '" y2="' + yAt(p, mini.total, max).toFixed(1) + '"/>';
            out += '<text class="tick start" x="' + (p.x + p.w + 6) + '" y="' + (yAt(p, mini.total, max) + 4).toFixed(1) + '">total ' + num(mini.total) + '</text>';
            for (var i = 0; i < INTERVALS; i += 24) {
                out += '<text class="tick mid" x="' + xAt(p, i).toFixed(1) + '" y="' + (p.y + p.h + 13) + '">' + esc(intervalTime(i)) + '</text>';
            }
        });
        var question = 'Can the grid take more?';
        var meta = 'Used against total per interval, cores, memory and GPU';
        var foot = '<p class="rowfoot">The dashed line is what the hosts have; the fill is what the work holds. ' +
            esc('GPU is at ' + pct(FLEET.gpuUsed[last] / (FLEET.gpuTotal || 1)) + ' and cores at ' + pct(FLEET.coresUsed[last] / FLEET.coresTotal) +
                ', so a GPU job waits and a CPU job does not. That is the headroom the arrival panel then spends.') + '</p>' +
            miniLegend([
                { label: 'used', color: C.blue },
                { label: 'GPU used', color: C.violet }
            ]);
        return chartCard('headroom', question, meta, svgEl(w, miniH * 3 + 10, out, 'headroom', question + ', ' + meta), foot, '', true);
    }

    /* 17. Upcoming arrivals: a forward timeline of scheduled runs. */
    function arrivalsChart(arrivals) {
        var horizonMin = 360;
        var w = 1000, rowH = 34, left = 120, right = 60, top = 16, bottom = 40;
        var p = { x: left, y: top, w: w - left - right, h: arrivals.length * rowH };
        var h = p.y + p.h + bottom;
        var out = '';
        var maxJobs = Math.max.apply(null, arrivals.map(function (a) { return a.cores * a.jobs; })) || 1;
        arrivals.forEach(function (a, r) {
            var t = tenantById(a.tenant);
            var x0 = p.x + (a.inMinutes / horizonMin) * p.w;
            var bw = Math.max(4, (a.wallclockMin / horizonMin) * p.w);
            var y = p.y + r * rowH;
            out += '<text class="tick rowlabel" x="' + (left - 8) + '" y="' + (y + 15) + '">' + esc(intervalTime(NOW_MIN + a.inMinutes)) + ' ' + esc(t.short) + '</text>';
            out += '<rect x="' + x0.toFixed(1) + '" y="' + (y + 3) + '" width="' + bw.toFixed(1) + '" height="18" rx="3" fill="' + t.color + '" fill-opacity="0.85"><title>' +
                esc(a.app + ' ' + a.version + ' \u00b7 ' + a.jobs + ' jobs \u00b7 ' + a.cores + ' cores, ' + a.memGiB + ' GiB, ' + a.gpu + ' GPU \u00b7 ' + a.wallclockMin + ' min wallclock') +
                '</title></rect>';
            out += '<text class="start" x="' + (x0 + bw + 6).toFixed(1) + '" y="' + (y + 16) + '">' +
                esc(a.app.replace('ores.', '') + ' ' + a.version + ' \u00b7 ' + a.jobs + ' jobs \u00b7 ' + a.cores + ' cores' + (a.gpu ? ', ' + a.gpu + ' GPU' : '')) + '</text>';
        });
        for (var m = 0; m <= horizonMin; m += 60) {
            var x = p.x + (m / horizonMin) * p.w;
            out += '<line class="grid" x1="' + x.toFixed(1) + '" y1="' + p.y + '" x2="' + x.toFixed(1) + '" y2="' + (p.y + p.h) + '"/>';
            out += '<text class="tick mid" x="' + x.toFixed(1) + '" y="' + (p.y + p.h + 15) + '">' + esc(intervalTime(NOW_MIN + m)) + '</text>';
        }
        out += '<text class="axis-label" x="' + (p.x + p.w) + '" y="' + (p.y + p.h + 30) + '" text-anchor="end">time ahead of now (15:00), one bar per scheduled run, width is its wallclock</text>';
        var question = 'What kicks in soon?';
        var meta = arrivals.length + ' scheduled runs in the next six hours, with their requirements';
        var foot = '<p class="rowfoot">Each bar starts when the run is due and is as wide as its wallclock. ' +
            'The 16-core risk run at ' + esc(intervalTime(NOW_MIN + 25)) + ' arrives while cores are at ' +
            esc(pct(FLEET.coresUsed[INTERVALS - 1] / FLEET.coresTotal)) + ', so it fits; a GPU run would queue.</p>' +
            miniLegend(TENANTS.map(function (t) { return { label: t.short, color: t.color }; }));
        return chartCard('arrivals', question, meta, svgEl(w, h, out, 'arrivals', question + ', ' + meta), foot, '', true);
    }

    /* 18. Requirements matrix: jobs against resources. */
    function requirementsMatrix(jobs) {
        var cols = [
            { key: 'cores', label: 'cores' },
            { key: 'memGiB', label: 'memory GiB' },
            { key: 'gpu', label: 'GPU' },
            { key: 'wallclockMin', label: 'wallclock min' },
            { key: 'inputMiB', label: 'input MiB' }
        ];
        var rows = jobs.slice().sort(function (a, b) { return a.submit - b.submit; }).slice(-18);
        var maxByCol = cols.map(function (c) {
            var m = 0;
            rows.forEach(function (j) { if (j.requirements[c.key] > m) m = j.requirements[c.key]; });
            return m || 1;
        });
        var cellW = 108, cellH = 22, left = 104, top = 40;
        var w = left + cols.length * cellW + 10;
        var h = top + rows.length * cellH + 16;
        var out = '';
        cols.forEach(function (c, ci) {
            out += '<text class="tick mid strong" x="' + (left + ci * cellW + cellW / 2).toFixed(1) + '" y="' + (top - 12) + '">' + esc(c.label) + '</text>';
        });
        rows.forEach(function (job, ri) {
            var y = top + ri * cellH;
            out += '<text class="tick rowlabel" x="' + (left - 8) + '" y="' + (y + 15) + '">' + esc(job.id + ' ' + job.app.replace('ores.', '')) + '</text>';
            cols.forEach(function (c, ci) {
                var v = job.requirements[c.key];
                var t = v / maxByCol[ci];
                var x = left + ci * cellW;
                out += '<rect class="cell" x="' + x + '" y="' + y + '" width="' + (cellW - 3) + '" height="' + (cellH - 3) + '" rx="2" fill="' + rampCss(t) + '"><title>' +
                    esc(job.id + ' \u00b7 ' + c.label + ' ' + v) + '</title></rect>';
                out += '<text class="tick mid" x="' + (x + (cellW - 3) / 2).toFixed(1) + '" y="' + (y + 14) + '" style="fill:' + inkOn(t) + '">' + num(v) + '</text>';
            });
        });
        var question = 'What does each job need?';
        var meta = rows.length + ' jobs against five resources, colour from the viridis ramp, the number is the requirement';
        var foot = '<p class="rowfoot">Rows are the jobs, columns the resources, and the number in each cell is the requirement. ' +
            'The bright column is the one that binds: read down it and the arrivals panel says whether the grid has it. ' +
            'A GPU job is a single bright cell, because GPU is scarce, not because the job is large.</p>';
        return chartCard('requirements', question, meta, svgEl(w, h, out, 'requirements', question + ', ' + meta), foot, '', true);
    }

    /* 19. Allocation by tenant. */
    function allocationChart() {
        var tenants = isTenant() ? [tenantById(TENANT_VARIANT)] : TENANTS;
        var series = tenants.map(function (t) { return FLEET.tenantCores[t.id]; });
        var colors = tenants.map(function (t) { return t.color; });
        var w = 1000, h = 250;
        var p = fullPlot(w, h, 58, 16, 22, 40);
        var max = niceMax(FLEET.coresTotal);
        var out = yAxis(p, max, 'cores', 4) + timeAxis(p) + stackedBands(series, colors, p, max);
        var question = 'Who is using the grid?';
        var meta = 'Cores allocated per tenant per interval, stacked area, cores';
        var foot = (isTenant()
            ? '<p class="rowfoot">The tenant variant shows Northwind Capital\u2019s own share. The other tenants\u2019 bands are the installation\u2019s ' +
              'cross-tenant aggregate, so they are withheld here and stay whole for the super administrator. This is the one chart on these ' +
              'screens with a permission question, and it is a gate on the panel, not a second screen.</p>'
            : '<p class="rowfoot">Every tenant\u2019s work runs on the same hosts, so the bands stack to the fleet\u2019s used cores. ' +
              'This is a cross-tenant aggregate: the super administrator reads it whole, and a tenant administrator sees only their own band.</p>') +
            miniLegend(tenants.map(function (t) { return { label: t.short, color: t.color }; }));
        return chartCard('allocation', question, meta, svgEl(w, h, out, 'allocation', question + ', ' + meta), foot, '', !isTenant());
    }

    /* 20. Versions in flight: a node by version matrix. */
    function versionsInFlightChart() {
        var cellW = 150, cellH = 22, left = 90, top = 44;
        var w = left + WRAPPER_VERSIONS.length * cellW + 20;
        var h = top + NODES.length * cellH + 16;
        var versionColor = { 'v0.0.25': C.teal, 'v0.0.24': C.amber, 'v0.0.19': C.vermillion };
        var versionInk = { 'v0.0.25': '#f0f0f2', 'v0.0.24': '#0b0e13', 'v0.0.19': '#f0f0f2' };
        var counts = {};
        WRAPPER_VERSIONS.forEach(function (v) { counts[v] = 0; });
        NODES.forEach(function (n) { counts[n.wrapperVersion]++; });
        var out = '';
        WRAPPER_VERSIONS.forEach(function (v, ci) {
            out += '<text class="tick mid strong" x="' + (left + ci * cellW + cellW / 2).toFixed(1) + '" y="' + (top - 24) + '">' + esc(v) + '</text>';
            out += '<text class="tick mid" x="' + (left + ci * cellW + cellW / 2).toFixed(1) + '" y="' + (top - 10) + '">' +
                counts[v] + ' of ' + NODES.length + ' nodes</text>';
        });
        NODES.forEach(function (node, ri) {
            var y = top + ri * cellH;
            out += '<text class="tick rowlabel" x="' + (left - 8) + '" y="' + (y + 15) + '">' + esc(node.short) + '</text>';
            WRAPPER_VERSIONS.forEach(function (v, ci) {
                var x = left + ci * cellW;
                var here = node.wrapperVersion === v;
                out += '<rect class="cell" x="' + x + '" y="' + y + '" width="' + (cellW - 3) + '" height="' + (cellH - 3) + '" rx="2" fill="' +
                    (here ? versionColor[v] : C.greyDark) + '" fill-opacity="' + (here ? 0.85 : 0.18) + '"><title>' +
                    esc(node.short + ' \u00b7 ' + v + (here ? '' : ' \u00b7 not this release')) + '</title></rect>';
                if (here) out += '<text class="tick mid" x="' + (x + (cellW - 3) / 2).toFixed(1) + '" y="' + (y + 14) + '" style="fill:' + versionInk[v] + '">running</text>';
            });
        });
        var question = 'Which release is running where?';
        var meta = NODES.length + ' nodes against ' + WRAPPER_VERSIONS.length + ' wrapper releases';
        var foot = '<p class="rowfoot">grid-13 still runs v0.0.19, the oldest build, and five nodes run v0.0.24. ' +
            'The wrapper heartbeat carries no host, so in the real read nothing joins a release to a node: this matrix is the screen the join would earn. ' +
            'The catalogue below is the app the work runs, which is a different release from the wrapper that carries it.</p>' +
            miniLegend([
                { label: 'v0.0.25, current', color: C.teal },
                { label: 'v0.0.24, one behind', color: C.amber },
                { label: 'v0.0.19, oldest build', color: C.vermillion }
            ]);
        return chartCard('inflight', question, meta, svgEl(w, h, out, 'inflight', question + ', ' + meta), foot, '', true);
    }

    /* 21. Work in flight under each concurrency policy. The policy is a
       behaviour, not a number, so there is no cap line to draw. */
    function concurrencyChart() {
        var w = 1000, h = 300, left = 170, right = 210, top = 16, bottom = 34;
        var p = { x: left, y: top, w: w - left - right, h: POLICIES.length * 36 };
        var h2 = p.y + p.h + bottom;
        var max = niceMax(Math.max.apply(null, POLICIES.map(function (r) { return r.inFlight; })) * 1.15);
        var out = '';
        POLICIES.forEach(function (row, i) {
            var y = p.y + i * 36;
            var inW = (row.inFlight / max) * p.w;
            out += '<text class="tick rowlabel" x="' + (left - 8) + '" y="' + (y + 22) + '">' + esc(row.app.replace('ores.', '')) + '</text>';
            out += '<rect x="' + p.x + '" y="' + (y + 4) + '" width="' + inW.toFixed(1) + '" height="22" fill="' + C.blue + '" fill-opacity="0.85"><title>' +
                esc(row.app + ' \u00b7 ' + row.inFlight + ' in flight, policy ' + row.behaviour) + '</title></rect>';
            out += '<text class="tick start" x="' + (p.x + inW + 8).toFixed(1) + '" y="' + (y + 20) + '">' +
                esc(row.inFlight + ' in flight \u00b7 policy ' + row.behaviour) + '</text>';
        });
        out += '<text class="axis-label" x="' + p.x + '" y="' + (p.y - 4) + '">jobs in flight under each policy, jobs</text>';
        var question = 'Is work piling up behind a policy?';
        var meta = POLICIES.length + ' concurrency policies, in flight and behaviour';
        var foot = '<p class="rowfoot">The policy is a behaviour rather than a number: when it is full the work is skipped, queued or failed. ' +
            'So the panel shows how much is in flight and what happens next, and draws no cap. ' +
            'The grid can have cores free while a policy is full, which is why this panel and the load panel disagree on purpose.</p>' +
            miniLegend([
                { label: 'jobs in flight', color: C.blue }
            ]);
        return chartCard('concurrency', question, meta, svgEl(w, h2, out, 'concurrency', question + ', ' + meta), foot, '', true);
    }

    // ============================================================= the panels

    function summaryPanel() {
        var last = INTERVALS - 1;
        var inflight = FLEET.unsent[last] + FLEET.inProgress[last];
        var failRate = FLEET.throughput[last] > 0 ? FLEET.failures[last] / FLEET.throughput[last] : 0;
        return '<section class="card">' +
            '<header class="sectionhead"><h2>Grid summary</h2>' +
            '<span class="faint" style="font-size:12px">sampled ' + esc(intervalTime(last)) + ' \u00b7 5-minute interval</span></header>' +
            '<div class="statgrid">' +
            '<div><span class="lbl">Nodes</span><span class="val"><span class="big">' + FLEET.online[last] + '</span>' +
            '<span class="tag">of ' + NODES.length + ' online</span><span class="tag">' + FLEET.idle[last] + ' idle</span>' +
            (NODES.length - FLEET.online[last] > 0 ? '<span class="tag bad">' + (NODES.length - FLEET.online[last]) + ' lost</span>' : '') +
            '</span></div>' +
            '<div><span class="lbl">Work in flight</span><span class="val"><span class="big">' + num(inflight) + '</span>' +
            '<span class="tag accent">' + FLEET.unsent[last] + ' unsent</span><span class="tag">' + FLEET.inProgress[last] + ' in progress</span></span></div>' +
            '<div><span class="lbl">Throughput, this interval</span><span class="val"><span class="big">' + num(FLEET.throughput[last]) + '</span>' +
            '<span class="tag">tasks done</span></span></div>' +
            '<div><span class="lbl">Failures, this interval</span><span class="val"><span class="big">' + FLEET.failures[last] + '</span>' +
            '<span class="tag ' + (failRate > 0.02 ? 'bad' : 'ok') + '">' + pct(failRate) + '</span></span></div>' +
            '<div><span class="lbl">Capacity, cores</span><span class="val"><span class="big">' + num(FLEET.coresUsed[last]) + '</span>' +
            '<span class="tag">of ' + num(FLEET.coresTotal) + ' used</span></span></div>' +
            '<div><span class="lbl">Work over the range</span><span class="val"><span class="big">' + num(FLEET.throughput.reduce(function (a, b) { return a + b; }, 0)) + '</span>' +
            '<span class="tag">tasks completed</span></span></div>' +
            '</div>' +
            '<p class="rowfoot">The grid is the installation\u2019s: every tenant\u2019s work runs on the same hosts. ' +
            'The fleet counts are whole for every role the permission admits; the work counts are cross-tenant aggregates and ' +
            'the tenant variant narrows them to one tenant\u2019s slice, as compute.org records.</p>' +
            '</section>';
    }

    function jobsTablePanel(jobs) {
        var rows = jobs.slice().sort(function (a, b) { return a.submit - b.submit; }).map(function (job) {
            var stateTag = job.state === 'failed' ? '<span class="tag bad">failed</span>'
                : job.state === 'running' ? '<span class="tag accent">running</span>'
                    : job.state === 'queued' ? '<span class="tag muted">queued</span>'
                        : '<span class="tag ok">done</span>';
            return '<tr>' +
                '<td class="code">' + esc(job.id) + '</td>' +
                '<td class="code">' + esc(job.batch) + '</td>' +
                '<td>' + esc(job.app.replace('ores.', '')) + ' <span class="mono faint">' + esc(job.version) + '</span></td>' +
                '<td>' + esc(tenantById(job.tenant).short) + '</td>' +
                '<td class="code">' + esc(job.node) + '</td>' +
                '<td>' + stateTag + '</td>' +
                '<td class="code">' + esc(asDuration(job.durationSec)) + '</td>' +
                '<td class="code">' + esc(intervalTime(job.submit)) + '</td>' +
                '</tr>';
        }).join('');
        return '<section class="card wide">' +
            '<header class="sectionhead"><h2>Where is one job?</h2>' +
            '<span class="faint" style="font-size:12px">' + jobs.length + ' jobs, one row each</span></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Job</th><th>Batch</th><th>App</th><th>Tenant</th><th>Node</th><th>State</th><th>Duration</th><th>Submitted</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="rowfoot">A member sees their own jobs, a tenant administrator their tenant\u2019s, and the super administrator all of them. ' +
            'That is one journey over three visible sets, which is why there is one screen and not two.</p>' +
            '</section>';
    }

    function failureDetailPanel() {
        var failed = filteredJobs().filter(function (j) { return j.state === 'failed'; });
        var rows = failed.map(function (job) {
            return '<tr>' +
                '<td class="code">' + esc(job.id) + '</td>' +
                '<td>' + esc(job.app.replace('ores.', '')) + ' <span class="mono faint">' + esc(job.version) + '</span></td>' +
                '<td class="code">' + esc(job.node) + '</td>' +
                '<td class="code">' + esc(intervalTime(job.submit)) + '</td>' +
                '<td class="code">' + job.exitCode + '</td>' +
                '<td>' + esc(job.stderr.split('\n').slice(-1)[0]) + '</td>' +
                '</tr>';
        }).join('');
        var first = failed[0];
        if (!first) {
            rows = '<tr><td colspan="6" class="faint">No failed job in this view. The fleet\u2019s own failure line above stays whole.</td></tr>';
        }
        return '<section class="card wide">' +
            '<header class="sectionhead"><h2>Why did it fail?</h2>' +
            '<span class="faint" style="font-size:12px">' + failed.length + ' failed jobs in this view</span></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Job</th><th>App</th><th>Node</th><th>Submitted</th><th>Exit code</th><th>Last stderr line</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            (first ? '<h3 style="margin-top:18px;font-size:14px">stderr, ' + esc(first.id) + '</h3><pre class="stderr">' + esc(first.stderr) + '</pre>' : '') +
            '<p class="rowfoot">The job\u2019s own detail narrows like any row. The node\u2019s logs are different: they are the installation\u2019s and may carry ' +
            'another tenant\u2019s inputs, so pulling them is a capability the super administrator holds and a tenant administrator does not. ' +
            'One screen, one control withheld.</p>' +
            '</section>';
    }

    function nodeLogsPanel() {
        var job = selectedJob();
        var disabled = isTenant();
        var button = '<button class="btn" ' + (disabled ? 'disabled' : 'data-act="download-logs"') +
            ' title="' + (disabled
                ? 'A node\u2019s logs are the installation\u2019s and may carry another tenant\u2019s inputs.'
                : 'Download the wrapper and worker logs for this node.') + '">Download this node\u2019s logs</button>';
        return '<section class="card">' +
            '<header class="sectionhead"><h2>Can I run it again?</h2>' +
            '<span class="faint" style="font-size:12px">the node\u2019s own logs</span></header>' +
            '<div class="cellgroup">' + button +
            '<span class="tag">' + esc(job.node) + '</span>' +
            '<span class="tag">' + esc(job.app + ' ' + job.version) + '</span>' +
            (disabled ? '<span class="tag warn">withheld for a tenant administrator</span>' : '<span class="tag ok">super administrator</span>') +
            '</div>' +
            (disabled
                ? '<div class="notice warn" style="margin-top:12px">A node\u2019s logs are the installation\u2019s and may carry another tenant\u2019s inputs. ' +
                  'The super administrator holds this capability; a tenant administrator does not. The job\u2019s own detail above still narrows to this tenant.</div>'
                : '<div class="notice" style="margin-top:12px">' + (S.logsDownloaded > 0
                    ? 'Downloaded ' + esc(job.node) + ' logs ' + S.logsDownloaded + ' time' + (S.logsDownloaded === 1 ? '' : 's') + ' in this session. A fixture: no file leaves the page.'
                    : 'The wrapper and worker lines for ' + esc(job.node) + '. In the real read this is a capability held by the super administrator.') + '</div>') +
            '</section>';
    }

    function cataloguePanel() {
        var rows = APP_VERSIONS.map(function (av) {
            var jobs = JOBS.filter(function (j) { return j.app === av.app && j.version === av.version; });
            var failed = jobs.filter(function (j) { return j.state === 'failed'; }).length;
            var nodes = NODES.filter(function (n) { return n.wrapperVersion === av.version; }).length;
            return '<tr>' +
                '<td class="code">' + esc(av.app) + '</td>' +
                '<td class="code">' + esc(av.version) + '</td>' +
                '<td class="code">' + esc(av.released) + '</td>' +
                '<td>' + (av.status === 'current' ? '<span class="tag ok">current</span>' : '<span class="tag muted">previous</span>') +
                (av.bad ? '<span class="tag bad">bad failure rate</span>' : '') + '</td>' +
                '<td class="code">' + jobs.length + '</td>' +
                '<td class="code">' + (failed > 0 ? '<span class="tag bad">' + failed + ' of ' + jobs.length + ' \u00b7 ' + pct(jobs.length ? failed / jobs.length : 0) + '</span>' : '0 of ' + jobs.length) + '</td>' +
                '<td class="code">' + nodes + '</td>' +
                '</tr>';
        }).join('');
        return '<section class="card wide">' +
            '<header class="sectionhead"><h2>What may the grid run?</h2>' +
            '<span class="faint" style="font-size:12px">' + APP_VERSIONS.length + ' app versions</span></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>App</th><th>Version</th><th>Released</th><th>Status</th><th>Jobs run</th><th>Failed</th><th>Nodes on this release</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="rowfoot">Reading the catalogue is everyone\u2019s, because a tenant needs to know what it may submit. ' +
            'Writing it is the super administrator\u2019s. That is a permission on the write, not a second journey, so this screen carries one catalogue ' +
            'and the edit control is absent rather than disabled.</p>' +
            '</section>';
    }

    // -------------------------------------------------------------- screens
    /* One screen's panels, split so the dashboard is a page and not a scroll.
       The hero stays above the tabs; the tabs divide what supports it. */
    function subTabs(tabs, current, note) {
        return '<div class="subtabs">' + tabs.map(function (t) {
            return '<button data-act="tab" data-tab="' + t.id + '"' +
                (current === t.id ? ' class="on"' : '') + '>' + esc(t.name) + '</button>';
        }).join('') + (note ? '<span class="count">' + esc(note) + '</span>' : '') + '</div>';
    }

    var WATCH_TABS = [
        { id: 'fleet', name: 'Fleet' },
        { id: 'work', name: 'Work' }
    ];

    function watchScreen() {
        var tab = S.watchTab;
        var support = tab === 'work'
            ? loadChart() + throughputChart() + outcomesChart() + queueChart()
            : ribbonChart() + fleetSizeChart() + nodeTablePanel();
        return summaryPanel() +
            heatmapChart(S.heatMetric, true) +
            subTabs(WATCH_TABS, tab, tab === 'work'
                ? 'the work the grid is doing'
                : 'the nodes it is doing it on') +
            support;
    }

    function jobScreen() {
        var jobs = filteredJobs();
        var job = selectedJob();
        return ganttPanel(jobs) +
            waterfallChart(job) +
            histogramChart(jobs) +
            durationByNodeChart() +
            jobsTablePanel(jobs);
    }

    function failureScreen() {
        return failureRateChart() +
            failuresByNodeChart() +
            failuresByVersionChart() +
            heatmapChart('failures', false) +
            failureDetailPanel() +
            nodeLogsPanel();
    }

    function capacityScreen() {
        var arrivals = isTenant()
            ? ARRIVALS.filter(function (a) { return a.tenant === TENANT_VARIANT; })
            : ARRIVALS;
        return capacityChart() +
            arrivalsChart(arrivals) +
            requirementsMatrix(filteredJobs()) +
            allocationChart();
    }

    function versionsScreen() {
        return versionsInFlightChart() +
            concurrencyChart() +
            cataloguePanel();
    }

    function screenBody() {
        if (S.screen === 'job') return jobScreen();
        if (S.screen === 'failure') return failureScreen();
        if (S.screen === 'capacity') return capacityScreen();
        if (S.screen === 'versions') return versionsScreen();
        return watchScreen();
    }

    // ---------------------------------------------------------------- chrome
    function pageHead() {
        var screen = activeScreen();
        return '<header class="pagehead"><div>' +
            '<div class="crumbs">Operations \u203a Compute grid \u203a ' + esc(screen.short) + '</div>' +
            '<h1>' + esc(screen.question) + '</h1>' +
            '<p class="lede">' + esc(screenLede(screen.id)) + '</p>' +
            '</div><div class="actions">' +
            '<span class="updated">Sampled ' + esc(S.updatedAt) + '</span>' +
            '<button class="btn" data-act="refresh">Refresh</button>' +
            '<a class="btn linklike" href="../index.html">Back to prototypes</a>' +
            '</div></header>';
    }

    function screenLede(id) {
        if (id === 'job') return 'One job end to end: what ran when, where its time went, and whether its duration is normal.';
        if (id === 'failure') return 'A job\u2019s full detail, the node\u2019s own logs, and the two charts that separate a bad release from a bad node.';
        if (id === 'capacity') return 'What the nodes have, what arriving work needs, and whether the grid can absorb it.';
        if (id === 'versions') return 'The apps, their versions, and the concurrency policy that caps how many of a thing run together.';
        return 'The fleet, the load, and how it moved over the last six hours. Every chart in compute.org\u2019s grid list is drawn from a mock series.';
    }

    function notice() {
        var text = isTenant()
            ? 'PROTOTYPE. ' + filteredJobs().length + ' of the range\u2019s ' + JOBS.length + ' jobs (this tenant\u2019s work) are ' +
              'fixtures from a seeded generator; the fleet rows stay whole. ' +
              'The node log download is withheld: a node\u2019s logs are the installation\u2019s and may carry another tenant\u2019s inputs.'
            : 'PROTOTYPE. Every row and every series is a fixture from a seeded generator, so the walk repeats exactly. ' +
              'Nothing on this page reads the server.';
        return '<div class="notice warn">' + esc(text) + '</div>';
    }

    function render() {
        var body = pageHead() + notice() + '<div class="chartgrid">' + screenBody() + '</div>';
        document.getElementById('app').innerHTML =
            '<div class="shell">' +
            '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav">' + SCREENS.map(function (s) {
                return '<button data-act="screen" data-screen="' + s.id + '"' + (S.screen === s.id ? ' class="here"' : '') + '>' + esc(s.name) + '</button>';
            }).join('') + '</nav>' +
            '<span class="modechip">' + (isTenant() ? 'Tenant administration' : 'System administration') + '</span>' +
            '</div></header>' +
            '<main>' + body + '</main></div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 signed in as ' +
            (isTenant() ? 'a tenant administrator on Northwind Capital' : 'the system administrator on the system tenant') +
            ' \u00b7 screen ' + S.screen + ' \u00b7 variant ' + S.variant;

        renderBar();
    }

    function renderBar() {
        var screen = activeScreen();
        var variant = variantById(S.variant);
        var jobs = filteredJobs();
        var failed = jobs.filter(function (j) { return j.state === 'failed'; });
        var screenButtons = SCREENS.map(function (s) {
            return '<button data-act="screen" data-screen="' + s.id + '"' + (S.screen === s.id ? ' class="on"' : '') + '>' + esc(s.short) + '</button>';
        }).join('');
        var variantButtons = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + v.id + '"' + (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.name) + '</button>';
        }).join('');
        var state = '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>screen: <b>' + esc(S.screen) + '</b> (' + esc(screen.question) + ')</span>' +
            '<span>variant: <b>' + esc(S.variant) + '</b></span>' +
            '<span>tenant: <b>' + (isTenant() ? esc(TENANT_VARIANT) : 'all') + '</b></span>' +
            '<span>heat metric: <b>' + esc(S.heatMetric) + '</b></span>' +
            '<span>job: <b>' + esc(selectedJob().id) + '</b></span>' +
            '<span>nodes: <b>' + NODES.length + '</b></span>' +
            '<span>jobs: <b>' + JOBS.length + '</b> (shown ' + jobs.length + ', failed ' + failed.length + ')</span>' +
            '<span>app versions: <b>' + APP_VERSIONS.length + '</b></span>' +
            '<span>tenants: <b>' + TENANTS.length + '</b></span>' +
            '<span>arrivals: <b>' + ARRIVALS.length + '</b></span>' +
            '<span>range: <b>' + INTERVALS + ' \u00d7 5 min, 09:00 to 15:00</b></span>' +
            '<span>P50 / P90 / P99: <b>' + asDuration(P50) + ' / ' + asDuration(P90) + ' / ' + asDuration(P99) + '</b></span>' +
            '<span>withheld: <b>' + (isTenant() ? 'node logs (Download this node\u2019s logs)' : 'none') + '</b></span>' +
            '<span>seed: <b>20261007 (mulberry32)</b></span>' +
            '</div>' +
            '<p class="state-note">Story: hot ' + esc(STORY.hot) + ' \u00b7 ' + esc(STORY.quiet) + ' \u00b7 ' + esc(STORY.draining) +
            ' \u00b7 bad release ' + esc(STORY.badVersion) + ' \u00b7 ' + esc(STORY.spike) + ' \u00b7 ' + esc(STORY.slow) + '</p>' +
            '<p class="state-note">' + esc(variant.gist) + '</p>' +
            (log.length === 0
                ? '<p class="state-note">No action yet.</p>'
                : '<ol>' + log.map(function (entry, index) {
                    return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
                }).join('') + '</ol>') +
            '</div>';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' +
            '<span class="screen-buttons">' + screenButtons + '</span>' +
            '<span class="sep">|</span>' +
            '<span class="variant-buttons">' + variantButtons + '</span>' +
            '<span class="gist">' + esc(screen.short + ': ' + variant.gist) + '</span>' +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') + '</button>' +
            '</div>' + (S.showState ? state : '');
    }

    // ---------------------------------------------------------------- events
    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (screenById(p.get('screen')).id === p.get('screen')) S.screen = p.get('screen');
        if (variantById(p.get('variant')).id === p.get('variant')) S.variant = p.get('variant');
        if (p.get('metric') === 'failures' || p.get('metric') === 'work') S.heatMetric = p.get('metric');
        if (p.get('tab') === 'fleet' || p.get('tab') === 'work') S.watchTab = p.get('tab');
        if (p.get('job')) S.job = p.get('job');
    }

    function writeParams() {
        var p = new URLSearchParams();
        p.set('screen', S.screen);
        p.set('variant', S.variant);
        p.set('metric', S.heatMetric);
        p.set('tab', S.watchTab);
        p.set('job', selectedJob().id);
        window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
    }

    function refresh() {
        S.updatedAt = clock(NOW_MIN + 1);
        log.push('refresh \u00b7 re-read the range and the fleet at ' + S.updatedAt);
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'screen') {
            S.screen = el.getAttribute('data-screen');
            S.job = null;
            log.push('screen \u00b7 ' + S.screen);
        } else if (act === 'variant') {
            S.variant = el.getAttribute('data-variant');
            log.push('variant \u00b7 ' + S.variant + (isTenant() ? ' \u00b7 rows narrowed to ' + TENANT_VARIANT + ', node logs withheld' : ' \u00b7 all rows, node logs offered'));
        } else if (act === 'metric') {
            S.heatMetric = el.getAttribute('data-metric');
            log.push('heatmap metric \u00b7 ' + S.heatMetric);
        } else if (act === 'tab') {
            S.watchTab = el.getAttribute('data-tab');
            log.push('tab \u00b7 ' + S.watchTab);
        } else if (act === 'job') {
            S.job = el.getAttribute('data-job');
            log.push('job \u00b7 ' + S.job);
        } else if (act === 'download-logs') {
            S.logsDownloaded++;
            log.push('download node logs \u00b7 ' + selectedJob().node + ' \u00b7 fixture, no file leaves the page');
        } else if (act === 'refresh') {
            refresh();
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
        }
        render();
        writeParams();
    });

    readParams();
    render();
})();
