/* Housekeeping prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * PROTOTYPE. Throwaway. Mock data, no backend.
 *
 * The screen answers three questions for the system administrator acting from
 * the system tenant:
 *
 *   1. every store the installation keeps, what bounds it, how large it is and
 *      whether it is growing;
 *   2. what a policy change will do, stated before it is made;
 *   3. what housekeeping has actually done, including what it skipped and why.
 *
 * Variants: A one table, B cards by where the store lives, C two panes with the
 * policy editor. States: fresh, skipped, failed, disabled, empty. The runs log
 * is a tab in every variant and opens directly with ?view=runs.
 *
 * There is no write path. The save bars state the change and the audit line the
 * platform would record, and a save reports that it is a mock. */

(function () {
    'use strict';

    const READ_AT = '24 Nov 2026, 17:32:10';
    const MAX_MB = 1900;

    const VARIANTS = [
        {
            id: 'A',
            name: 'One table',
            gist: 'Every store in one table. Choose a row and the policy and its runs open below it.'
        },
        {
            id: 'B',
            name: 'Cards by where it lives',
            gist: 'Four groups of cards, each store sized against the largest, its policy in one line and its last outcome on a badge.'
        },
        {
            id: 'C',
            name: 'Two panes',
            gist: 'The stores list on the left, the policy editor on the right, and a save bar that states the change and the audit line first.'
        }
    ];

    const STATES = [
        {
            id: 'fresh',
            name: 'Fresh',
            gist: 'Every enabled policy applied at 17:15 and freed 0 B because no chunk is older than its period yet. The zero is expected, not broken.'
        },
        {
            id: 'skipped',
            name: 'Skipped',
            gist: 'An enabled delete-rows policy on the ordinary table is recorded as skipped: the sweep applies only a chunk drop today, so the store is not bounded.'
        },
        {
            id: 'failed',
            name: 'Failed',
            gist: 'The 17:15 chunk drop on report_cashflows_90d failed, with a detail line; the policy stays enabled and retries.'
        },
        {
            id: 'disabled',
            name: 'Disabled',
            gist: 'report_cashflows_90d is switched off, so its store is kept for ever and grows past 90 days.'
        },
        {
            id: 'empty',
            name: 'Empty',
            gist: 'No retention policy exists at all. Nothing bounds any store but the digest-keyed package cache, and the screen offers the way to add one.'
        }
    ];

    const GROUPS = [
        { id: 'db', name: 'Database tables', note: 'Ordinary tables. A row-level delete is the only action that bounds one.' },
        { id: 'timescale', name: 'Timescale chunked tables', note: 'Chunked on write time. A chunk drop is the only action that bounds one.' },
        { id: 'object', name: 'Object store', note: 'Key prefixes. A lifecycle rule is the only bound, and none exists.' },
        { id: 'scratch', name: 'Compute host scratch', note: 'Directories and caches on the compute host. A release rule is the only bound.' }
    ];

    const STORES = [
        {
            id: 'calendar_dates',
            name: 'ores_refdata_calendar_dates_tbl',
            label: 'Calendar dates',
            code: 'refdata.calendar_dates',
            owner: 'Reference data (refdata)',
            timeColumn: 'valid_from',
            measureKind: 'relation rows',
            targetKind: 'chunked_table',
            description: 'The business days of every calendar, one row per calendar per day, derived from each calendar\'s rules and its exceptions. The installation publishes them once and every tenant reads the same copy.',
            group: 'timescale',
            kind: 'Timescale chunked table',
            where: 'System tenant · 1 copy',
            sizeMB: 283,
            series: [276, 278, 279, 280, 280, 281, 282, 282, 283, 283, 283, 283],
            size: '283 MB',
            measure: '486,540 rows',
            growth: 'Growing · +18 MB over 30 days',
            defaultAction: 'drop_chunks',
            note: 'Chunks are cut on write time. Nothing drops them, so the table is bounded by nothing yet.'
        },
        {
            id: 'report_cashflows',
            name: 'ores_reporting_report_cashflows_tbl',
            label: 'Report run cashflows',
            limitMB: 512,
            limitBytes: 536870912,
            code: 'reporting.report_cashflows',
            owner: 'Reporting (reporting)',
            timeColumn: 'written_at',
            measureKind: 'relation rows',
            targetKind: 'chunked_table',
            description: 'The cashflow rows each report run writes: the projected flows of every trade the run priced, one row per flow per measure. Kept so a run can be read again without pricing it again.',
            group: 'timescale',
            kind: 'Timescale chunked table',
            where: 'System tenant · 1 copy',
            sizeMB: 186,
            series: [0, 0, 12, 40, 88, 130, 160, 175, 182, 185, 186, 186],
            size: '186 MB',
            measure: '186 MB after 39 runs',
            growth: 'Flat · 186 MB after 39 runs',
            defaultAction: 'drop_chunks',
            note: 'Run output, kept for a quarter. The policy drops a chunk only once it is older than its period.'
        },
        {
            id: 'report_metrics',
            name: 'ores_reporting_report_metrics_tbl',
            label: 'Report run metrics',
            code: 'reporting.report_metrics',
            owner: 'Reporting (reporting)',
            timeColumn: 'written_at',
            measureKind: 'relation rows',
            targetKind: 'chunked_table',
            description: 'The summary figures each report run writes beside its cashflows, one row per metric per run. Kept under the same policy as the cashflows, because the two are read together.',
            group: 'timescale',
            kind: 'Timescale chunked table',
            where: 'System tenant · 1 copy',
            sizeMB: 4,
            series: [0, 0, 0.4, 1.2, 2.1, 2.8, 3.3, 3.6, 3.8, 3.9, 4, 4],
            size: '4 MB',
            measure: '4 MB after 39 runs',
            growth: 'Flat · 4 MB after 39 runs',
            defaultAction: 'drop_chunks',
            note: 'Run output, kept for a quarter under the same shape of policy as the cashflows table.'
        },
        {
            id: 'delivery_counts',
            name: 'ores_marketdata_delivery_counts_tbl',
            label: 'Market data delivery counts',
            code: 'marketdata.delivery_counts',
            owner: 'Market data (marketdata)',
            timeColumn: 'period_start',
            measureKind: 'relation rows',
            targetKind: 'ordinary_table',
            description: 'How many items each feed delivered for each series in each period: one row per series per period, carried forward on every flush rather than appended.',
            group: 'db',
            kind: 'Ordinary table',
            where: 'System tenant · grows by insert',
            sizeMB: 29,
            series: [1, 2, 4, 6, 8, 10, 12, 15, 18, 21, 25, 29],
            size: '29 MB',
            measure: '~27,000 rows/hour',
            growth: 'Growing · ~27,000 rows/hour',
            defaultAction: 'delete_rows',
            note: 'An ordinary table: there are no chunks to drop, so a delete-rows policy is the only action that could bound it.'
        },
        {
            id: 'ticks',
            name: 'ores_marketdata_ticks_tbl',
            label: 'Market data ticks',
            code: 'marketdata.ticks',
            owner: 'Market data (marketdata)',
            timeColumn: 'ts',
            measureKind: 'relation rows',
            targetKind: 'chunked_table',
            description: 'The quotes every feed delivered, as they arrived, compressed by the store. The raw record a curve is built from, kept longer than anything derived from it.',
            group: 'timescale',
            kind: 'Timescale chunked table',
            where: 'System tenant · compressed',
            sizeMB: 0.016,
            series: [0.014, 0.014, 0.015, 0.015, 0.015, 0.016, 0.016, 0.016, 0.016, 0.016, 0.016, 0.016],
            size: '16 kB compressed',
            measure: '60,223 rows',
            growth: 'Growing · 60,223 rows so far',
            defaultAction: 'drop_chunks',
            note: 'Compressed chunks. No retention policy targets this table.'
        },
        {
            id: 'compute_input',
            name: 'compute/input/',
            label: 'Compute job inputs',
            code: 'compute.job_inputs',
            owner: 'Compute (compute)',
            timeColumn: 'created_at',
            measureKind: 'objects and bytes',
            targetKind: 'object_prefix',
            description: 'The input archives a report run hands to the grid, one per work unit: the trade set, the configuration and the market data the engine reads.',
            group: 'object',
            kind: 'Object store prefix',
            where: 'Object store · size not listed',
            sizeMB: null,
            series: null,
            size: 'Not measured',
            measure: 'Object count not listed',
            growth: 'Not measured · no byte total',
            defaultAction: 'expire_objects',
            note: 'A key prefix with no lifecycle rule: nothing expires what it holds.'
        },
        {
            id: 'compute_output',
            name: 'compute/output/',
            label: 'Compute job outputs',
            code: 'compute.job_outputs',
            owner: 'Compute (compute)',
            timeColumn: 'created_at',
            measureKind: 'objects and bytes',
            targetKind: 'object_prefix',
            description: 'The result archives the grid writes back, one per work unit: the engine\'s own output files and its log, packed together.',
            group: 'object',
            kind: 'Object store prefix',
            where: 'Object store · size not listed',
            sizeMB: null,
            series: null,
            size: 'Not measured',
            measure: 'Object count not listed',
            growth: 'Not measured · no byte total',
            defaultAction: 'expire_objects',
            note: 'A key prefix with no lifecycle rule: run output accumulates with nothing to expire it.'
        },
        {
            id: 'wrappers',
            name: 'publish/run/wrappers',
            label: 'Compute node scratch',
            limitMB: 2048,
            limitBytes: 2147483648,
            code: 'compute.node_scratch',
            owner: 'Compute (compute)',
            timeColumn: 'folder mtime',
            measureKind: 'files and bytes',
            targetKind: 'directory',
            description: 'The working folder each grid node keeps while it runs a job: the unpacked input, the engine\'s log and the output, until the job\'s result is uploaded.',
            group: 'scratch',
            kind: 'Host scratch directory',
            where: 'Compute host · 565 job folders',
            sizeMB: 1900,
            series: [300, 520, 760, 980, 1180, 1360, 1520, 1650, 1750, 1820, 1870, 1903],
            size: '1.9 GB',
            measure: '565 job folders · 3.4 MB per completed job',
            growth: 'Growing · +3.4 MB per completed job',
            defaultAction: 'release_directory',
            note: 'A working directory is released only once its output is stored elsewhere; no release rule exists today.'
        },
        {
            id: 'packages',
            name: 'publish/run/packages',
            label: 'Engine package cache',
            code: 'compute.package_cache',
            owner: 'Compute (compute)',
            timeColumn: 'n/a — bounded by digest',
            measureKind: 'files and bytes',
            targetKind: 'directory',
            description: 'The unpacked engine packages a host shares between its nodes, named by the package\'s digest so two versions holding the same bytes are one copy. Bounded by that keying rather than by a policy.',
            group: 'scratch',
            kind: 'Package cache',
            where: 'Compute host · one copy per host',
            sizeMB: 209,
            series: [0, 209, 209, 209, 209, 209, 209, 209, 209, 209, 209, 209],
            size: '209 MB',
            measure: 'One copy per host',
            growth: 'Flat · one copy per host',
            keyed: true,
            defaultAction: 'release_directory',
            note: 'Content-addressed: the digest names the copy, so repeating a transport does not repeat the bytes. The store is bounded by its keying and needs no retention policy.'
        }
    ];

    const ACTIONS = [
        { id: 'drop_chunks', name: 'Drop chunks' },
        { id: 'delete_rows', name: 'Delete rows' },
        { id: 'expire_objects', name: 'Expire objects' },
        { id: 'release_directory', name: 'Release directory' }
    ];

    const S = {
        variant: 'A',
        state: 'fresh',
        view: 'stores',
        selected: null,
        runFilter: null,
        onlyUnbounded: false,
        panel: false,
        overrides: {},
        added: [],
        draftKey: null,
        draft: null,
        notice: null
    };

    /* ------------------------------------------------------------ data */

    function actionLabel(action) {
        const found = ACTIONS.filter(function (a) { return a.id === action; })[0];
        return found ? found.name : action;
    }

    function basePolicies() {
        const list = [
            {
                id: 'report_cashflows_90d',
                storeId: 'report_cashflows',
                target: 'ores_reporting_report_cashflows_tbl',
                action: 'drop_chunks',
                periodDays: 90,
                cadenceHours: 24,
                enabled: true,
                reason: 'Cashflow runs are read for a quarter; an older chunk can be recomputed from the run ledger.',
                lastOutcome: 'applied',
                lastSweep: '24 Nov 2026, 17:15',
                freed: '0 B',
                detail: 'No chunk was older than 90 days; the oldest chunk was written 28 Aug 2026.'
            },
            {
                id: 'report_metrics_90d',
                storeId: 'report_metrics',
                target: 'ores_reporting_report_metrics_tbl',
                action: 'drop_chunks',
                periodDays: 90,
                cadenceHours: 24,
                enabled: true,
                reason: 'Metrics are small and read beside the cashflows of the same run, so the same quarter applies.',
                lastOutcome: 'applied',
                lastSweep: '24 Nov 2026, 17:15',
                freed: '0 B',
                detail: 'No chunk was older than 90 days; the oldest chunk was written 2 Sep 2026.'
            }
        ];

        if (S.state === 'skipped') {
            list.push({
                id: 'delivery_counts_7d',
                storeId: 'delivery_counts',
                target: 'ores_marketdata_delivery_counts_tbl',
                action: 'delete_rows',
                periodDays: 7,
                cadenceHours: 24,
                enabled: true,
                reason: 'Delivery counters are useful for a week; an older row is read only during an incident.',
                lastOutcome: 'skipped',
                lastSweep: '24 Nov 2026, 17:15',
                freed: '0 B',
                detail: 'The sweep applies only a chunk drop today; the delete-rows action was not attempted.'
            });
        }

        const cashflows = list[0];

        if (S.state === 'failed') {
            cashflows.lastOutcome = 'failed';
            cashflows.freed = '0 B';
            cashflows.detail = 'drop_chunks failed: connection reset by peer while listing chunks (SQLSTATE 08006). The policy remains enabled and retries at the next sweep.';
        }

        if (S.state === 'disabled') {
            cashflows.enabled = false;
            cashflows.lastSweep = '24 Nov 2026, 16:15';
            cashflows.freed = '0 B';
            cashflows.detail = 'Applied at 16:15, before the policy was switched off at 17:20; no chunk was older than 90 days.';
        }

        if (S.state === 'empty') {
            list.length = 0;
        }

        return list;
    }

    function policies() {
        return basePolicies().concat(S.added).map(function (policy) {
            const override = S.overrides[policy.id];
            return override ? Object.assign({}, policy, override) : Object.assign({}, policy);
        });
    }

    function storeById(id) {
        return STORES.filter(function (store) { return store.id === id; })[0] || null;
    }

    function policyFor(store) {
        if (!store) return null;
        return policies().filter(function (policy) { return policy.storeId === store.id; })[0] || null;
    }

    function isBounded(store) {
        if (store.keyed) return true;
        const policy = policyFor(store);
        return !!policy && policy.enabled && policy.lastOutcome === 'applied';
    }

    function unboundedCount() {
        return STORES.filter(function (store) { return !isBounded(store); }).length;
    }

    /* What "no bound" costs, in bytes. A store with nothing removing it is kept
       for ever, so the measured rate of the last twelve weeks is the only
       honest way to say how bad that is. An unmeasured store says so. */

    function unboundedConsequence(store) {
        if (store.keyed) return 'It needs no policy: the digest keying bounds it.';
        if (store.sizeMB === null) return 'Its size is not measured, so its growth cannot be seen either.';
        const rate = ratePerWeek(store);
        if (rate === null) return 'Nothing removes what it holds.';
        if (Math.abs(rate) < 0.05) return 'Nothing removes what it holds. It is not growing at the moment.';
        return 'Nothing removes it: ' + (rate > 0 ? '+' : '') + fmtMB(rate) + ' a week, about ' +
            (rate > 0 ? '+' : '') + fmtMB(rate * 52) + ' a year at the last twelve weeks.';
    }

    /* The column answers one question: does anything remove what this store
       holds? A policy that exists is not the same answer: a policy switched off,
       skipped or failed leaves the store exactly as unbounded as no policy at
       all. The reason for the answer lives beside it, in the Bounded by cell. */

    /* The list wants the verdict alone: the reason is in the cell beside it, and
       a column of long labels is not scannable. The store's page and the cards
       want the reason as well, so they ask for the longer form. */

    function verdictOf(store) {
        if (store.keyed) return { label: 'Bounded by keying', tone: 'ok' };
        if (isBounded(store)) return { label: 'Bounded', tone: 'ok' };
        const policy = policyFor(store);
        if (policy && policy.lastOutcome === 'failed') return { label: 'Not bounded', tone: 'bad' };
        if (policy && policy.lastOutcome === 'pending') return { label: 'Not bounded', tone: 'muted' };
        return { label: 'Not bounded', tone: 'warn' };
    }

    function verdictTag(store) {
        const verdict = verdictOf(store);
        return '<span class="tag ' + verdict.tone + '">' + esc(verdict.label) + '</span>';
    }

    function statusOf(store) {
        if (store.keyed) return { label: 'Bounded by keying', tone: 'ok' };
        const policy = policyFor(store);
        if (!policy) return { label: 'Not bounded', tone: 'warn' };
        if (!policy.enabled) return { label: 'Not bounded — policy off', tone: 'warn' };
        if (policy.lastOutcome === 'skipped') return { label: 'Not bounded — sweep skips it', tone: 'warn' };
        if (policy.lastOutcome === 'failed') return { label: 'Not bounded — last sweep failed', tone: 'bad' };
        if (policy.lastOutcome === 'pending') return { label: 'Not bounded yet — first sweep pending', tone: 'muted' };
        return { label: 'Bounded', tone: 'ok' };
    }

    function statusTag(store) {
        const status = statusOf(store);
        return '<span class="tag ' + status.tone + '">' + esc(status.label) + '</span>';
    }

    function nextSweep(cadenceHours) {
        let hour = 17 + Number(cadenceHours);
        const day = hour >= 24 ? ' tomorrow' : ' today';
        hour = hour % 24;
        return (hour < 10 ? '0' : '') + hour + ':15' + day;
    }

    function nextSweepFor(policy) {
        if (!policy.enabled) return 'None — the policy is off';
        if (policy.lastOutcome === 'failed') return '18:15 today (retry)';
        if (policy.lastOutcome === 'skipped') return nextSweep(policy.cadenceHours) + ' (will skip again)';
        return nextSweep(policy.cadenceHours);
    }

    function fmtMB(mb) {
        if (mb === null || mb === undefined) return 'Not measured';
        if (mb >= 1000) return (mb / 1000).toFixed(1) + ' GB';
        if (mb >= 1) return (Math.round(mb * 10) / 10) + ' MB';
        return Math.round(mb * 1024) + ' kB';
    }

    function knownTotalMB() {
        return STORES.reduce(function (sum, store) {
            return store.sizeMB === null ? sum : sum + store.sizeMB;
        }, 0);
    }

    function knownCount() {
        return STORES.filter(function (store) { return store.sizeMB !== null; }).length;
    }

    /* ----------------------------------------------------------- runs */

    function tickDetail() {
        if (S.state === 'skipped') return 'Sweep completed: 3 policies due, 2 applied, 1 skipped, 0 B freed, 1.6 s.';
        if (S.state === 'failed') return 'Sweep completed: 2 policies due, 1 applied, 1 failed, 0 B freed, 3.1 s.';
        if (S.state === 'disabled') return 'Sweep completed: 1 policy due, 1 applied, 0 B freed, 1.1 s.';
        return 'Sweep completed: 2 policies due, 2 applied, 0 B freed, 1.4 s.';
    }

    function runRows() {
        if (S.state === 'empty') return [];

        const rows = [
            {
                at: '17:15:05',
                sort: 5,
                storeId: null,
                store: 'All stores',
                policyId: null,
                policy: '(sweep tick)',
                action: '—',
                outcome: 'applied',
                freed: '0 B',
                detail: tickDetail()
            },
            {
                at: '17:15:04',
                sort: 4,
                storeId: 'report_cashflows',
                store: 'ores_reporting_report_cashflows_tbl',
                policyId: 'report_cashflows_90d',
                policy: 'report_cashflows_90d',
                action: 'Drop chunks',
                outcome: 'applied',
                freed: '0 B',
                detail: 'No chunk was older than 90 days; the oldest chunk was written 28 Aug 2026.'
            },
            {
                at: '17:15:03',
                sort: 3,
                storeId: 'report_metrics',
                store: 'ores_reporting_report_metrics_tbl',
                policyId: 'report_metrics_90d',
                policy: 'report_metrics_90d',
                action: 'Drop chunks',
                outcome: 'applied',
                freed: '0 B',
                detail: 'No chunk was older than 90 days; the oldest chunk was written 2 Sep 2026.'
            }
        ];

        if (S.state === 'skipped') {
            rows.push({
                at: '17:15:02',
                sort: 2,
                storeId: 'delivery_counts',
                store: 'ores_marketdata_delivery_counts_tbl',
                policyId: 'delivery_counts_7d',
                policy: 'delivery_counts_7d',
                action: 'Delete rows',
                outcome: 'skipped',
                freed: '0 B',
                detail: 'The sweep applies only a chunk drop today; the delete-rows action was not attempted. The store is not bounded.'
            });
        }

        if (S.state === 'failed') {
            const cashflows = rows.filter(function (row) { return row.policyId === 'report_cashflows_90d'; })[0];
            cashflows.outcome = 'failed';
            cashflows.detail = 'drop_chunks failed: connection reset by peer while listing chunks (SQLSTATE 08006). The policy remains enabled and retries at the next sweep.';
        }

        if (S.state === 'disabled') {
            const cashflows = rows.filter(function (row) { return row.policyId === 'report_cashflows_90d'; })[0];
            cashflows.at = '16:15:04';
            cashflows.sort = 1;
            cashflows.detail = 'Applied before the policy was switched off at 17:20; no chunk was older than 90 days.';
        }

        return rows.sort(function (a, b) { return b.sort - a.sort; });
    }

    function outcomeTag(outcome) {
        const tone = outcome === 'failed' ? 'bad' : outcome === 'skipped' ? 'warn' : 'ok';
        const label = outcome === 'failed' ? 'Failed' : outcome === 'skipped' ? 'Skipped' : 'Applied';
        return '<span class="tag ' + tone + '">' + label + '</span>';
    }

    /* --------------------------------------------------------- pieces */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function variantById(id) {
        return VARIANTS.filter(function (v) { return v.id === id; })[0];
    }

    function stateById(id) {
        return STATES.filter(function (s) { return s.id === id; })[0];
    }

    function activeVariant() { return variantById(S.variant) || VARIANTS[0]; }
    function activeState() { return stateById(S.state) || STATES[0]; }

    function notice(tone, title, body, actions) {
        return '<div class="notice ' + tone + '">' +
            '<p class="title">' + esc(title) + '</p>' +
            '<p>' + body + '</p>' +
            (actions ? '<div class="actions">' + actions + '</div>' : '') +
            '</div>';
    }

    function inlineNotice(tone, title, body) {
        return '<div class="notice ' + tone + ' compact">' +
            '<p class="title">' + esc(title) + '</p>' +
            '<p>' + body + '</p>' +
            '</div>';
    }

    function detail(k, v, cls) {
        return '<div class="detail"><span class="k">' + esc(k) + '</span>' +
            '<span class="v' + (cls ? ' ' + cls : '') + '">' + v + '</span></div>';
    }

    /* -------------------------------------------------------- chrome */

    function header() {
        return '<header class="pageheader"><div>' +
            '<h1>Housekeeping</h1>' +
            '<p class="sub">Every store the installation keeps, what bounds it, how large it is, and what the sweep has actually done.</p>' +
            '</div><div class="head-actions">' +
            '<span class="meta">Signed in as system administrator · tenant system<br>Read at ' + esc(READ_AT) + '</span>' +
            '<a class="btn secondary" href="../../index.html">Back to prototypes</a>' +
            '</div></header>';
    }

    function statsLine() {
        const policiesHeld = policies().length;
        return '<div class="statline">' +
            '<span><b>' + STORES.length + '</b> stores</span>' +
            '<span><b>' + policiesHeld + '</b> policies</span>' +
            '<span><b class="warn">' + unboundedCount() + '</b> not bounded</span>' +
            '<span><b>' + fmtMB(knownTotalMB()) + '</b> measured across ' + knownCount() + ' stores</span>' +
            '<span>' + (STORES.length - knownCount()) + ' stores report no size</span>' +
            '</div>';
    }

    function tabs() {
        return '<div class="tabs" role="tablist" aria-label="Housekeeping view">' +
            '<button type="button" role="tab" aria-selected="' + (S.view === 'stores') + '" aria-controls="view-panel" data-act="view" data-view="stores">Stores <span class="count">' + STORES.length + '</span></button>' +
            '<button type="button" role="tab" aria-selected="' + (S.view === 'runs') + '" aria-controls="view-panel" data-act="view" data-view="runs">Runs <span class="count">' + runRows().length + '</span></button>' +
            '</div>';
    }

    function stateBanner() {
        const held = policies().length;
        const unbounded = unboundedCount();

        if (S.state === 'empty' && held === 0) {
            return notice('warn', 'No retention policy exists.',
                'The installation keeps ' + STORES.length + ' stores and holds no retention policy. ' + unbounded +
                ' have nothing bounding them; only <span class="mono">publish/run/packages</span> is bounded, by its content digest keying. Nothing removes old data or artefacts today.',
                '<button type="button" class="btn primary small" data-act="first-policy">Add the first policy</button>');
        }

        if (S.state === 'empty') {
            return notice('ok', 'A policy now exists (mock).',
                'The page holds ' + held + ' policy for this fixture. The empty state is still selected; the new policy lives only in the page and no sweep will apply it.',
                '');
        }

        if (S.state === 'skipped') {
            return notice('warn', 'A policy is enabled, but it does not bound its store.',
                '<span class="mono">delivery_counts_7d</span> asks to delete rows older than 7 days from ' +
                '<span class="mono">ores_marketdata_delivery_counts_tbl</span>. The sweep applies only a chunk drop today, ' +
                'so the 17:15 run is recorded as skipped and the table keeps every row. It holds 29 MB and grows by about ' +
                '27,000 rows an hour. This store is not bounded.',
                '<button type="button" class="btn secondary small" data-act="runs-store" data-store="delivery_counts">See the skipped run</button>');
        }

        if (S.state === 'failed') {
            return notice('bad', 'The 17:15 sweep failed for report_cashflows_90d.',
                '<span class="mono">drop_chunks</span> could not list the chunks: connection reset by peer while listing chunks (SQLSTATE 08006). ' +
                'The table is unchanged at 186 MB and 0 B was freed. The policy stays enabled and retries at the next sweep, 18:15.',
                '<button type="button" class="btn secondary small" data-act="runs-store" data-store="report_cashflows">See the failed run</button>');
        }

        if (S.state === 'disabled') {
            const cashflows = policyFor(storeById('report_cashflows'));
            if (cashflows && cashflows.enabled) {
                return notice('ok', 'The policy is enabled again (mock).',
                    'The page now holds <span class="mono">report_cashflows_90d</span> enabled with a ' + cashflows.periodDays +
                    '-day period. No sweep applies it until the mock save is replaced by a real write.',
                    '');
            }
            return notice('warn', 'A policy is switched off, so its store is kept for ever.',
                '<span class="mono">report_cashflows_90d</span> was switched off at 17:20. It last applied at 16:15 and freed 0 B. ' +
                'The table holds 186 MB and keeps every chunk past 90 days until the policy is enabled again. ' +
                'That leaves ' + unbounded + ' of the ' + STORES.length + ' stores with nothing bounding them.',
                '<button type="button" class="btn secondary small" data-act="edit-store" data-store="report_cashflows">Open the policy editor</button>');
        }

        return notice('warn', unbounded + ' of the ' + STORES.length + ' stores have nothing that bounds them.',
            'A store with no policy that applies is never deleted from. It grows until the database or the disk is full, ' +
            'which is the one failure this screen exists to prevent. The policies held today freed 0 B because nothing ' +
            'is older than its period yet: a zero is the expected answer for a store bounded this week, not a failure. ' +
            'It does not bound the ' + unbounded + ' stores below.',
            '<button type="button" class="btn primary small" data-act="filter-unbounded">' +
            (S.onlyUnbounded ? 'Show every store' : 'Show the ' + unbounded + ' stores nothing bounds') + '</button>' +
            (S.view === 'runs' ? '' : '<button type="button" class="btn secondary small" data-act="view" data-view="runs">Open the runs log</button>'));
    }

    /* ------------------------------------------------------ variant A */

    function boundedBy(store) {
        const policy = policyFor(store);

        if (store.keyed) {
            return '<span class="boundkey">Content digest keying — one copy per host</span>' +
                '<span class="subline">Not age-bounded; no policy is needed</span>';
        }

        if (!policy) {
            const rate = ratePerWeek(store);
            let cost = 'Nothing removes what it holds';
            if (store.sizeMB === null) cost = 'Size not measured, so growth is invisible';
            else if (rate !== null && Math.abs(rate) >= 0.05) cost = 'Nothing removes it · about ' + fmtMB(rate * 52) + ' a year';
            return '<span class="none">None — no retention policy</span>' +
                '<span class="subline warnline">' + esc(cost) + '</span>';
        }

        let html = esc(actionLabel(policy.action)) + ' · ' + policy.periodDays + ' days' +
            '<span class="subline">every ' + policy.cadenceHours + ' h · <span class="mono">' + esc(policy.id) + '</span></span>';

        if (!policy.enabled) {
            html += '<span class="subline warnline">Switched off: the action is not applied.</span>';
        } else if (policy.lastOutcome === 'skipped') {
            html += '<span class="subline warnline">Not applied: the sweep applies only a chunk drop today.</span>';
        } else if (policy.lastOutcome === 'failed') {
            html += '<span class="subline badline">Last sweep failed; nothing was removed.</span>';
        } else if (policy.lastOutcome === 'pending') {
            html += '<span class="subline">Waiting for its first sweep.</span>';
        }

        return html;
    }

    function sweepCell(store) {
        const policy = policyFor(store);
        if (store.keyed) return '<span class="subline">Not applicable</span>';
        if (!policy) return 'Never<span class="subline">no policy</span>';
        if (policy.lastOutcome === 'pending') return 'Never<span class="subline">not yet swept</span>';

        const when = policy.lastSweep.replace('24 Nov 2026, ', '');
        let outcome = policy.lastOutcome;
        if (!policy.enabled && policy.lastOutcome === 'applied') outcome = 'applied, before switch-off';

        return esc(when) + '<span class="subline">' + esc(outcome) + '</span>';
    }


    /* The activity around one store: what the sampler measured, what the sweep
       did, and what a person changed. It is one log because the question "what
       has happened to this store" has one answer. */

    const ACTIVITY = {
        calendar_dates: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'info', label: 'Measured', what: '283 MB · 486,540 rows', detail: 'hourly sample, 12 points kept' },
            { at: '24 Nov 17:15', kind: 'Swept', outcome: 'skipped', label: 'Skipped', what: 'no policy names this store', detail: 'nothing bounds it, so nothing was applied' }
        ],
        report_cashflows: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'info', label: 'Measured', what: '186 MB · +0.0 MB since 17:00', detail: 'hourly sample, 12 points kept' },
            { at: '24 Nov 17:15', kind: 'Swept', outcome: 'applied', label: 'Applied', what: 'removed 0 chunks', detail: 'nothing older than 90 days' },
            { at: '24 Nov 16:58', kind: 'Changed', outcome: 'info', label: 'Recorded', what: 'period 120 -> 90 days', detail: 'super_admin: a quarter is the longest the audit allows' }
        ],
        report_metrics: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'info', label: 'Measured', what: '4 MB · +0.0 MB since 17:00', detail: 'hourly sample' },
            { at: '24 Nov 17:15', kind: 'Swept', outcome: 'applied', label: 'Applied', what: 'removed 0 chunks', detail: 'nothing older than 90 days' }
        ],
        delivery_counts: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'info', label: 'Measured', what: '29 MB · +0.6 MB since 17:00', detail: 'hourly sample; about 27,000 rows an hour' },
            { at: '24 Nov 18:00', kind: 'Refused', outcome: 'failed', label: 'Refused', what: 'policy delivery_counts_30d could not be written', detail: 'the sweep performs a chunk drop; this store is an ordinary table' },
            { at: '24 Nov 17:15', kind: 'Swept', outcome: 'skipped', label: 'Skipped', what: 'no policy applies', detail: 'nothing bounds it, so nothing was applied' }
        ],
        ticks: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'info', label: 'Measured', what: '16 kB · +0.0 kB since 17:00', detail: 'hourly sample; compressed by the store' }
        ],
        compute_input: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'skipped', label: 'Not measured', what: 'size not measured', detail: 'the storage service reports bytes per prefix only when asked, and no probe reads it yet' }
        ],
        compute_output: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'skipped', label: 'Not measured', what: 'size not measured', detail: 'the storage service reports bytes per prefix only when asked, and no probe reads it yet' }
        ],
        wrappers: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'info', label: 'Measured', what: '1.9 GB · 675 folders', detail: 'hourly sample; 3.4 MB per completed job' },
            { at: '24 Nov 17:40', kind: 'Released', outcome: 'info', label: 'Released', what: 'job 95f40ef1 · 3,842,446 bytes', detail: 'the node released a completed job\'s folder' }
        ],
        packages: [
            { at: '24 Nov 18:00', kind: 'Sampled', outcome: 'info', label: 'Measured', what: '209 MB · 1 copy', detail: 'keyed by package digest, shared by five nodes' }
        ]
    };

    /* Growth: the size the installation measured, one point a week. The
       prototype draws twelve of them; in the product they come from the store
       samples, not from a fixture. */

    function pointsFor(series, width, height) {
        const max = Math.max.apply(null, series);
        const min = Math.min.apply(null, series);
        const span = max - min || 1;
        return series.map(function (v, i) {
            const x = (i / (series.length - 1)) * (width - 2) + 1;
            const y = height - 2 - ((v - min) / span) * (height - 4);
            return x.toFixed(1) + ',' + y.toFixed(1);
        }).join(' ');
    }

    function sparkline(store, width, height) {
        if (!store.series) {
            return '<span class="spark unknown" aria-hidden="true"></span>' +
                '<span class="meta">not measured</span>';
        }
        const id = 'spark-' + store.id;
        return '<svg class="spark" width="' + width + '" height="' + height + '" viewBox="0 0 ' +
            width + ' ' + height + '" role="img" aria-labelledby="' + id + '">' +
            '<title id="' + id + '">' + esc(store.label) + ' size, weekly for twelve weeks: from ' +
            esc(fmtMB(Math.min.apply(null, store.series))) + ' to ' + esc(store.size) + '</title>' +
            '<polyline fill="none" stroke="currentColor" stroke-width="1.25" points="' +
            pointsFor(store.series, width, height) + '"/></svg>';
    }

    /* A plot, not a line drawn on nothing: size against time, with both axes
       labelled and ticked. The numbers are the store's samples, weekly. */

    const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

    function weekLabels(count) {
        const end = new Date(Date.UTC(2026, 10, 24));
        const labels = [];
        for (let i = count - 1; i >= 0; i--) {
            const d = new Date(end.getTime() - i * 7 * 24 * 3600 * 1000);
            labels.push(d.getUTCDate() + ' ' + MONTHS[d.getUTCMonth()]);
        }
        return labels;
    }

    function growthChart(store) {
        if (!store.series) {
            return '<p class="note">The size of this store is not measured, so it has no history to plot. ' +
                'The object store reports bytes per prefix only when asked, and no sampler reads it yet.</p>';
        }

        const series = store.series;
        const labels = weekLabels(series.length);
        const w = 560;
        const h = 240;
        const left = 64;
        const right = 16;
        const top = 12;
        const bottom = 40;
        const plotW = w - left - right;
        const plotH = h - top - bottom;

        const rawMax = Math.max.apply(null, series);
        const rawMin = Math.min.apply(null, series);
        // A flat store still needs a range to draw against.
        const span = rawMax - rawMin || rawMax * 0.2 || 1;
        const yMax = rawMax + span * 0.08;
        const yMin = Math.max(0, rawMin - span * 0.08);
        const ySpan = yMax - yMin || 1;

        const xOf = (i) => left + (i / (series.length - 1)) * plotW;
        const yOf = (v) => top + plotH - ((v - yMin) / ySpan) * plotH;

        // Four horizontal gridlines, each with the size it stands for.
        const ticks = 4;
        let grid = '';
        for (let i = 0; i <= ticks; i++) {
            const value = yMin + (ySpan * i) / ticks;
            const y = yOf(value).toFixed(1);
            grid += '<line class="gridline" x1="' + left + '" y1="' + y + '" x2="' + (w - right) +
                '" y2="' + y + '"/>' +
                '<text class="ytick" x="' + (left - 8) + '" y="' + (Number(y) + 3.5).toFixed(1) +
                '" text-anchor="end">' + esc(fmtMB(Math.round(value))) + '</text>';
        }

        // One tick a week, labelled every third week so the axis stays readable.
        let xaxis = '';
        for (let i = 0; i < series.length; i++) {
            const x = xOf(i).toFixed(1);
            if (i % 3 === 0 || i === series.length - 1) {
                xaxis += '<text class="xtick" x="' + x + '" y="' + (h - bottom + 16) +
                    '" text-anchor="middle">' + esc(labels[i]) + '</text>';
            }
            xaxis += '<line class="tick" x1="' + x + '" y1="' + (h - bottom) + '" x2="' + x +
                '" y2="' + (h - bottom + 4) + '"/>';
        }

        const points = series.map((v, i) => xOf(i).toFixed(1) + ',' + yOf(v).toFixed(1)).join(' ');
        const dots = series.map((v, i) => '<circle class="dot" cx="' + xOf(i).toFixed(1) + '" cy="' +
            yOf(v).toFixed(1) + '" r="2.2"><title>' + esc(labels[i]) + ': ' + esc(fmtMB(v)) +
            '</title></circle>').join('');

        const first = series[0];
        const last = series[series.length - 1];
        const delta = last - first;
        const weeks = series.length - 1;
        const perWeek = weeks ? delta / weeks : 0;

        return '<figure class="chart">' +
            '<svg class="growthchart" viewBox="0 0 ' + w + ' ' + h + '" role="img" aria-labelledby="chart-' +
            store.id + '">' +
            '<title id="chart-' + store.id + '">' + esc(store.label) + ' size over time: ' + esc(labels[0]) +
            ' to ' + esc(labels[labels.length - 1]) + ', from ' + esc(fmtMB(first)) + ' to ' +
            esc(store.size) + '</title>' +
            grid +
            '<line class="axis" x1="' + left + '" y1="' + top + '" x2="' + left + '" y2="' + (h - bottom) + '"/>' +
            '<line class="axis" x1="' + left + '" y1="' + (h - bottom) + '" x2="' + (w - right) + '" y2="' +
            (h - bottom) + '"/>' +
            '<text class="axtitle" x="' + left + '" y="' + (top - 2) + '" text-anchor="start">Size</text>' +
            '<text class="axtitle" x="' + (w - right) + '" y="' + (h - 6) +
            '" text-anchor="end">Time, one point a week</text>' +
            xaxis +
            '<polyline class="series" fill="none" points="' + points + '"/>' +
            dots +
            '</svg>' +
            '<figcaption class="chartmeta">' + esc(fmtMB(first)) + ' on ' + esc(labels[0]) + ', ' +
            esc(store.size) + ' today: ' + (delta >= 0 ? '+' : '') + esc(fmtMB(delta)) + ' over ' + weeks +
            ' weeks, about ' + (perWeek >= 0 ? '+' : '') + esc(fmtMB(perWeek)) + ' a week.</figcaption>' +
            '</figure>';
    }

    function fmtMB(v) {
        if (v === 0) return '0 B';
        if (Math.abs(v) < 1) return Math.round(v * 1024) + ' kB';
        if (Math.abs(v) >= 1024) return (v / 1024).toFixed(1) + ' GB';
        return (v % 1 === 0 ? v : v.toFixed(1)) + ' MB';
    }

    /* The list says what a store holds and which way it is going. The plot is
       on the store's page: twelve points in a row would be read as a shape at a
       glance, which is exactly what a list of nine stores cannot afford. */

    function ratePerWeek(store) {
        if (!store.series) return null;
        const weeks = store.series.length - 1;
        if (!weeks) return null;
        return (store.series[store.series.length - 1] - store.series[0]) / weeks;
    }

    function sizeCell(store) {
        let out = '<span class="v">' + esc(store.size) + '</span>';
        if (store.limitBytes) {
            const pct = Math.min(100, (store.sizeMB / store.limitMB) * 100);
            const tone = pct >= 90 ? 'bad' : pct >= 75 ? 'warn' : 'ok';
            out += '<span class="limitbar" role="img" aria-label="' +
                esc(Math.round(pct) + ' percent of the ' + fmtMB(store.limitMB) + ' budget') + '">' +
                '<span class="fill ' + tone + '" style="width:' + pct.toFixed(1) + '%"></span></span>' +
                '<span class="subline">of ' + esc(fmtMB(store.limitMB)) + ' · ' + Math.round(pct) + '%</span>';
        } else {
            out += '<span class="subline">no size budget</span>';
        }
        return out;
    }

    function growthCell(store) {
        const rate = ratePerWeek(store);
        if (rate === null) return '<span class="subline">not measured</span>';
        const flat = Math.abs(rate) < 0.05;
        const arrow = flat ? '→' : rate > 0 ? '↑' : '↓';
        const tone = flat ? 'flat' : rate > 0 ? 'up' : 'down';
        return '<span class="trend ' + tone + '">' + arrow + ' ' +
            (flat ? 'flat' : (rate > 0 ? '+' : '') + esc(fmtMB(rate)) + '/wk') + '</span>';
    }

    function storeRow(store) {
        const selected = S.selected === store.id;
        return '<tr class="selectable' + (selected ? ' selected' : '') + '" data-act="select" data-store="' + store.id + '">' +
            '<td class="storecell"><button type="button" class="linkish sname" data-act="select" data-store="' + store.id + '"' +
            ' aria-expanded="' + selected + '">' + esc(store.label) + '</button>' +
            '<span class="storetarget" title="' + esc(store.name) + '">' + esc(store.name) + '</span>' +
            '<span class="skind">' + esc(store.kind) + ' · ' + esc(store.where) + '</span></td>' +
            '<td>' + verdictTag(store) + '</td>' +
            '<td>' + boundedBy(store) + '</td>' +
            '<td class="sizecell">' + sizeCell(store) + '</td>' +
            '<td class="growthcell">' + growthCell(store) + '</td>' +
            '</tr>';
    }

    function viewA() {
        // A list of stores, and one page per store: the way the rest of the
        // application works. The list is the page until a store is opened.
        if (S.selected) return storePage();

        const shown = S.onlyUnbounded
            ? STORES.filter(function (store) { return !isBounded(store); })
            : STORES;

        let body;
        if (shown.length === 0) {
            body = '<p class="empty">Every store is bounded. Nothing here needs a decision.</p>';
        } else {
            const caption = S.onlyUnbounded
                ? 'The stores nothing bounds, what bounds each, its size and its growth'
                : 'Every store, whether anything bounds it, what bounds it, its size and its growth';
            body = '<div class="table-wrap"><table class="data">' +
                '<caption class="visually-hidden">' + caption + '</caption>' +
                '<thead><tr><th>Store / Target</th><th>Bounded?</th><th>Bounded by</th><th>Size</th><th>Growth</th></tr></thead>' +
                '<tbody>' + shown.map(storeRow).join('') + '</tbody></table></div>';
        }

        return '<section class="card master">' +
            '<header><div><h2>' + (S.onlyUnbounded ? 'The stores nothing bounds' : 'Every store') + '</h2>' +
            '<p class="meta">' + (S.onlyUnbounded
                ? 'This is the work list. Give each store a policy, or record why it needs none. It should end empty.'
                : 'A store with no policy that applies is kept for ever. The Bounded by column says what would remove it.') +
            '</p></div>' +
            '<div class="filters">' +
            '<button type="button" class="chip" aria-pressed="' + S.onlyUnbounded + '" data-act="filter-unbounded">' +
            'Not bounded <span class="count">' + unboundedCount() + '</span></button>' +
            '<span class="meta">' + policies().length + ' policies · ' + STORES.length + ' stores</span>' +
            '</div></header>' +
            body +
            '<p class="note">Sizes come from the database catalogues, the object-store listing and a directory walk. ' +
            'A policy that frees 0 B has found nothing older than its period; it is not a failure. ' +
            'A store bounded by its content digest needs no policy.</p>' +
            '</section>';
    }

    function policyBlock(store, policy) {
        if (store.keyed) {
            return '<div class="section"><h3>Bound</h3>' + inlineNotice('info', 'Bounded without a policy', esc(store.note)) + '</div>';
        }

        if (!policy) {
            return '<div class="section"><h3>Policy</h3>' +
                inlineNotice('warn', 'No retention policy covers this store',
                    esc(unboundedConsequence(store)) + ' ' + esc(store.note)) +
                '<div class="section-actions">' +
                '<button type="button" class="btn primary small" data-act="edit-store" data-store="' + store.id + '">Create a policy</button>' +
                '<button type="button" class="btn secondary small" data-act="runs-store" data-store="' + store.id + '">See its runs</button>' +
                '</div></div>';
        }

        let warning = '';
        if (!policy.enabled) {
            warning = inlineNotice('warn', 'The policy is switched off',
                'The store is kept for ever until the policy is enabled again. Nothing is deleted.');
        } else if (policy.lastOutcome === 'skipped') {
            warning = inlineNotice('warn', 'This policy does not bound the store',
                'The sweep applies only a chunk drop today, so this ' + esc(actionLabel(policy.action).toLowerCase()) +
                ' policy is recorded as skipped. Nothing removes what it holds until the sweep can apply that action.');
        } else if (policy.lastOutcome === 'failed') {
            warning = inlineNotice('bad', 'The last sweep failed', esc(policy.detail));
        } else if (policy.lastOutcome === 'pending') {
            warning = inlineNotice('info', 'Waiting for its first sweep',
                'The policy has not been applied yet. The first due sweep is at ' + esc(nextSweep(policy.cadenceHours)) + '.');
        }

        return '<div class="section"><h3>Policy</h3>' +
            '<div class="details c2">' +
            detail('Policy', esc(policy.id), 'mono') +
            detail('Target', esc(policy.target), 'mono') +
            detail('Action', esc(actionLabel(policy.action))) +
            detail('Enabled', policy.enabled ? 'Yes' : 'No — switched off') +
            detail('Period', policy.periodDays + ' days') +
            detail('Cadence', 'every ' + policy.cadenceHours + ' h') +
            detail('Last outcome', esc(policy.lastOutcome)) +
            detail('Last sweep', esc(policy.lastSweep)) +
            detail('Freed by the last sweep', esc(policy.freed)) +
            detail('Next sweep', esc(nextSweepFor(policy))) +
            '<p class="span2"><span class="k">Reason the store is kept</span>' + esc(policy.reason) + '</p>' +
            '</div>' + warning +
            '<div class="section-actions">' +
            '<button type="button" class="btn secondary small" data-act="edit-store" data-store="' + store.id + '">Open the policy editor</button>' +
            '<button type="button" class="btn secondary small" data-act="runs-store" data-store="' + store.id + '">See its runs</button>' +
            '</div></div>';
    }

    function recentRuns(store) {
        const list = runRows().filter(function (row) { return row.storeId === store.id; });
        let body;

        if (list.length === 0) {
            body = '<p class="empty">No run is recorded for this store. ' +
                (store.keyed ? 'It needs none: the digest keying bounds it.' : 'The first sweep under a policy will appear here.') +
                '</p>';
        } else {
            body = '<div class="table-wrap"><table class="data">' +
                '<caption class="visually-hidden">Recent runs for ' + esc(store.label) + '</caption>' +
                '<thead><tr><th>Time</th><th>Policy</th><th>Outcome</th><th>Freed</th><th>Detail</th></tr></thead><tbody>' +
                list.map(function (row) {
                    return '<tr>' +
                        '<td class="mono">24 Nov, ' + esc(row.at) + '</td>' +
                        '<td class="mono">' + esc(row.policy) + '</td>' +
                        '<td>' + outcomeTag(row.outcome) + '</td>' +
                        '<td class="num">' + esc(row.freed) + '</td>' +
                        '<td>' + esc(row.detail) + '</td>' +
                        '</tr>';
                }).join('') +
                '</tbody></table></div>';
        }

        return '<div class="section"><h3>Recent runs</h3>' + body +
            '<div class="section-actions"><button type="button" class="btn secondary small" data-act="runs-store" data-store="' +
            store.id + '">Open the runs log</button></div></div>';
    }

    function activityTag(row) {
        const tone = row.outcome === 'failed' ? 'bad' : row.outcome === 'skipped' ? 'warn' : 'ok';
        return '<span class="tag ' + tone + '">' + esc(row.label || row.kind) + '</span>';
    }

    function activityLog(store) {
        const list = ACTIVITY[store.id] || [];
        let body;

        if (list.length === 0) {
            body = '<p class="empty">Nothing has happened to this store yet.</p>';
        } else {
            body = '<div class="table-wrap"><table class="data">' +
                '<caption class="visually-hidden">Latest activity around ' + esc(store.label) + '</caption>' +
                '<thead><tr><th>When</th><th>What</th><th>Outcome</th><th>Activity</th><th>Detail</th></tr></thead><tbody>' +
                list.map(function (row) {
                    return '<tr>' +
                        '<td class="mono">' + esc(row.at) + '</td>' +
                        '<td>' + esc(row.kind) + '</td>' +
                        '<td>' + activityTag(row) + '</td>' +
                        '<td>' + esc(row.what) + '</td>' +
                        '<td class="meta">' + esc(row.detail) + '</td>' +
                        '</tr>';
                }).join('') +
                '</tbody></table></div>';
        }

        return '<div class="section"><h3>Latest activity around the store</h3>' + body +
            '<div class="section-actions">' +
            '<button type="button" class="btn secondary small" data-act="runs-store" data-store="' +
            store.id + '">Open the full log</button>' +
            '</div></div>';
    }

    function storeRecord(store, policy) {
        return '<div class="record">' +
            detail('Code', '<span class="mono">' + esc(store.code) + '</span>') +
            detail('Name', esc(store.label)) +
            detail('Owner', esc(store.owner)) +
            detail('Target kind', '<span class="mono">' + esc(store.targetKind) + '</span>') +
            detail('Target', '<span class="mono">' + esc(store.name) + '</span>') +
            detail('Time column', store.timeColumn === 'n/a — bounded by digest'
                ? esc(store.timeColumn)
                : '<span class="mono">' + esc(store.timeColumn) + '</span>') +
            detail('Measured by', esc(store.measureKind)) +
            detail('Bounded by', policy
                ? esc(actionLabel(policy.action) + ' · ' + policy.periodDays + ' days')
                : store.keyed ? 'Content digest keying' : 'Nothing — no retention policy') +
            '</div>' +
            '<p class="description recorddesc">' + esc(store.description) + '</p>';
    }

    function storePage() {
        const store = storeById(S.selected) || STORES[0];
        const policy = policyFor(store);

        return '<nav class="crumbs" aria-label="Breadcrumb">' +
            '<button type="button" class="linkish" data-act="back">Housekeeping</button>' +
            '<span class="sep">/</span><span aria-current="page">' + esc(store.label) + '</span>' +
            '</nav>' +
            '<section class="card storepage" id="store-detail" aria-labelledby="detail-heading">' +
            '<div class="panelnav">' +
            '<button type="button" class="btn secondary small" data-act="back">Back to the stores</button>' +
            '<button type="button" class="btn secondary small" data-act="runs-store" data-store="' +
            store.id + '">Log for this store</button>' +
            '</div>' +
            '<header><div><h2 id="detail-heading">' + esc(store.label) + '</h2>' +
            '<p class="meta">A store of the installation · <span class="mono">' + esc(store.code) + '</span></p>' +
            '</div>' + statusTag(store) + '</header>' +
            '<section class="recordsection"><h3>The store</h3>' +
            storeRecord(store, policy) + '</section>' +
            '<div class="details c3">' +
            detail('Size now', esc(store.size)) +
            detail('Measured', esc(store.measure)) +
            detail('Where it lives', esc(store.kind + ' · ' + store.where)) +
            '</div>' +
            '<section class="growthsection"><h3>Growth over twelve weeks</h3>' +
            growthChart(store) +
            '<p class="note">A point is one row of the store sampling table: the size measured at an ' +
            'instant, keyed to this store. The latest point is the size now; the points before it are the ' +
            'plot. Without the series the screen can only say what the store holds now, which is the one ' +
            'question nobody asks twice.</p></section>' +
            policyBlock(store, policy) +
            (S.editing === store.id ? '<section class="paneleditor" aria-labelledby="editor-heading">' +
                '<div class="panelnav"><button type="button" class="btn secondary small" ' +
                'data-act="cancel-edit">Close the editor</button></div>' + editor(store) + '</section>' : '') +
            activityLog(store) +
            recentRuns(store) +
            '</section>';
    }

    /* ------------------------------------------------------ variant B */

    function policyLine(store) {
        const policy = policyFor(store);
        if (store.keyed) return 'Bounded by content digest keying — one copy per host';
        if (!policy) return '<b>No policy</b> — this store is not bounded';

        let line = '<b>' + esc(actionLabel(policy.action)) + ' · ' + policy.periodDays + ' days</b>';
        if (!policy.enabled) line += ' — switched off, nothing removes it';
        else if (policy.lastOutcome === 'skipped') line += ' — skipped, not bounded';
        else if (policy.lastOutcome === 'failed') line += ' — last sweep failed';
        else if (policy.lastOutcome === 'pending') line += ' — waiting for the first sweep';
        return line;
    }

    function storeCard(store) {
        const status = statusOf(store);
        let bar;
        let sizeMeta;

        if (store.sizeMB === null) {
            bar = '<div class="sbar" role="img" aria-label="Size not measured"><span class="unknown"></span></div>';
            sizeMeta = 'size not listed';
        } else {
            const raw = (store.sizeMB / MAX_MB) * 100;
            const width = Math.max(0.6, raw);
            bar = '<div class="sbar" role="img" aria-label="' + esc(store.size) + ', ' + raw.toFixed(1) +
                ' percent of the largest store"><span style="width:' + width.toFixed(2) + '%"></span></div>';
            sizeMeta = raw < 1 ? 'under 1% of the largest store' : raw.toFixed(0) + '% of the largest store';
        }

        return '<article class="storecard">' +
            '<header><h3>' + esc(store.label) + '</h3>' + statusTag(store) + '</header>' +
            '<p class="storetarget">' + esc(store.name) + '</p>' +
            '<p class="where">' + esc(store.kind) + ' · ' + esc(store.where) + '</p>' +
            '<div class="sizeline"><span class="bigsize">' + esc(store.size) + '</span><span class="meta">' + esc(sizeMeta) + '</span></div>' +
            bar +
            '<p class="policyline">' + policyLine(store) + '</p>' +
            '<p class="growthline">' + esc(store.growth) + '</p>' +
            '<p class="cardfoot"><button type="button" class="btn secondary small" data-act="runs-store" data-store="' +
            store.id + '">Runs for this store</button></p>' +
            '</article>';
    }

    function viewB() {
        return GROUPS.map(function (group) {
            const list = STORES.filter(function (store) { return store.group === group.id; });
            const measured = list.filter(function (store) { return store.sizeMB !== null; });
            const total = list.reduce(function (sum, store) {
                return store.sizeMB === null ? sum : sum + store.sizeMB;
            }, 0);

            return '<section class="group" aria-labelledby="group-' + group.id + '">' +
                '<div class="grouphead"><div><h2 id="group-' + group.id + '">' + esc(group.name) + '</h2>' +
                '<p class="note">' + esc(group.note) + '</p></div>' +
                '<span class="meta">' + (list.length === 1 ? '1 store' : list.length + ' stores') +
                (measured.length ? ' · ' + fmtMB(total) + ' measured' : ' · no size reported') + '</span></div>' +
                '<div class="cardgrid">' + list.map(storeCard).join('') + '</div></section>';
        }).join('');
    }

    /* ------------------------------------------------------ variant C */

    function selectStore(id) {
        if (S.selected !== id) {
            S.selected = id;
            S.draftKey = null;
            S.notice = null;
        }
    }

    function draftBaseline(store) {
        const policy = policyFor(store);
        if (policy) {
            return {
                kind: 'policy',
                policyId: policy.id,
                periodDays: policy.periodDays,
                cadenceHours: policy.cadenceHours,
                enabled: policy.enabled,
                reason: ''
            };
        }
        if (store.keyed) return null;
        return {
            kind: 'new',
            storeId: store.id,
            action: store.defaultAction,
            periodDays: 30,
            cadenceHours: 24,
            enabled: true,
            reason: ''
        };
    }

    function draftKeyFor(store) {
        const policy = policyFor(store);
        return S.state + '|' + store.id + '|' + (policy ? policy.id : 'new');
    }

    function ensureDraft() {
        const store = storeById(S.selected) || STORES[0];
        const key = draftKeyFor(store);
        if (S.draftKey !== key) {
            S.draftKey = key;
            S.draft = draftBaseline(store);
        }
        if (S.draft && S.draft.reason === undefined) S.draft.reason = '';
    }

    function actionOptions(selected) {
        return ACTIONS.map(function (action) {
            return '<option value="' + action.id + '"' + (action.id === selected ? ' selected' : '') + '>' + esc(action.name) + '</option>';
        }).join('');
    }

    function periodOptions(selected) {
        return [7, 14, 30, 60, 90, 180, 365].map(function (days) {
            return '<option value="' + days + '"' + (days === selected ? ' selected' : '') + '>' + days + ' days</option>';
        }).join('');
    }

    function cadenceOptions(selected) {
        return [1, 6, 12, 24].map(function (hours) {
            return '<option value="' + hours + '"' + (hours === selected ? ' selected' : '') + '>' + hours + ' hours</option>';
        }).join('');
    }

    function diffChanges(policy, draft) {
        const changes = [];
        if (draft.periodDays !== policy.periodDays) {
            changes.push('Period ' + policy.periodDays + ' → ' + draft.periodDays + ' days');
        }
        if (draft.cadenceHours !== policy.cadenceHours) {
            changes.push('Cadence ' + policy.cadenceHours + ' → ' + draft.cadenceHours + ' hours');
        }
        if (draft.enabled !== policy.enabled) {
            changes.push(draft.enabled ? 'Enable the policy' : 'Disable the policy');
        }
        return changes;
    }

    function nextLine(draft) {
        return draft.enabled ? 'next sweep ' + nextSweep(draft.cadenceHours) : 'no sweep will apply it';
    }

    function auditUpdate(policy, draft) {
        const parts = [];
        if (draft.periodDays !== policy.periodDays) {
            parts.push('period_days=' + policy.periodDays + '→' + draft.periodDays);
        }
        if (draft.cadenceHours !== policy.cadenceHours) {
            parts.push('cadence_hours=' + policy.cadenceHours + '→' + draft.cadenceHours);
        }
        if (draft.enabled !== policy.enabled) {
            parts.push('enabled=' + policy.enabled + '→' + draft.enabled);
        }
        return 'housekeeping.policy.update · target=' + policy.target + ' · policy=' + policy.id +
            (parts.length ? ' · ' + parts.join(' · ') : '') +
            ' · commentary=' + (draft.reason.trim() === '' ? '<required>' : JSON.stringify(draft.reason.trim())) +
            ' · actor=system administrator · tenant=system · at=' + READ_AT;
    }

    function policyName(store, draft) {
        return store.id + '_' + draft.periodDays + 'd';
    }

    function auditCreate(store, draft) {
        return 'housekeeping.policy.create · target=' + store.name + ' · policy=' + policyName(store, draft) +
            ' · action=' + draft.action + ' · period_days=' + draft.periodDays +
            ' · cadence_hours=' + draft.cadenceHours + ' · enabled=' + draft.enabled +
            ' · commentary=' + (draft.reason.trim() === '' ? '<required>' : JSON.stringify(draft.reason.trim())) +
            ' · actor=system administrator · tenant=system · at=' + READ_AT;
    }

    function effectText(store, policy, draft) {
        if (!draft.enabled) return 'The store is kept for ever while the policy is off.';
        if (policy && policy.lastOutcome === 'skipped') {
            return 'Nothing removes what it holds: the sweep applies only a chunk drop today.';
        }
        if (policy && policy.lastOutcome === 'failed') {
            return 'The store holds ' + store.size + '. The last sweep failed; the next retry is at 18:15.';
        }
        return 'The store holds ' + store.size + ' today. No dry run exists, so the screen cannot state what the next sweep would free.';
    }

    function addEffect(store, draft) {
        if (!draft.enabled) return 'The policy is created switched off; nothing removes what the store holds.';
        if (draft.action !== 'drop_chunks') {
            return 'The first sweep records this policy as skipped: the sweep applies only a chunk drop today. Nothing removes what the store holds.';
        }
        return 'The first sweep drops chunks older than ' + draft.periodDays + ' days. The store holds ' +
            store.size + ' today; no dry run exists, so the screen cannot state what it would free.';
    }

    function savedNotice() {
        if (!S.notice) return '';
        return '<div class="notice ok compact" role="status">' +
            '<p class="title">' + esc(S.notice.title) + '</p>' +
            '<p>' + esc(S.notice.body) + '</p>' +
            '<p class="audit">' + esc(S.notice.audit) + '</p>' +
            '</div>';
    }

    function reasonField(draft) {
        return '<div class="control full"><label for="reason">Reason for the change</label>' +
            '<textarea id="reason" data-act="draft-reason" placeholder="Why this period, for the person who reads the record later">' +
            esc(draft.reason) + '</textarea>' +
            '<span class="hint">Required. The audit line carries it, and the server refuses a change that states none.</span></div>';
    }

    function enabledField(draft) {
        return '<div class="control"><span>Enabled</span><label class="check" for="enabled">' +
            '<input id="enabled" type="checkbox" data-act="draft-enabled"' + (draft.enabled ? ' checked' : '') + '>' +
            '<span>Apply this policy when it is due<span class="hint">Off keeps the store for ever; nothing is deleted by turning it off.</span></span>' +
            '</label></div>';
    }

    function policyEditor(store, policy) {
        const draft = S.draft;
        const changes = diffChanges(policy, draft);
        const hasChange = changes.length > 0;
        const reasonOk = draft.reason.trim() !== '';
        const canSave = hasChange && reasonOk;

        let warning = '';
        if (!policy.enabled) {
            warning = inlineNotice('warn', 'The policy is switched off',
                'The store is kept for ever while the policy is off. Enabling it restores the ' + policy.periodDays +
                '-day period; this screen deletes nothing.');
        } else if (policy.lastOutcome === 'skipped') {
            warning = inlineNotice('warn', 'This policy cannot bound its store',
                'The sweep applies only a chunk drop today, so a delete-rows policy is recorded as skipped. ' +
                'Changing its period does not make it effective: nothing removes what the store holds until the sweep can apply that action.');
        } else if (policy.lastOutcome === 'failed') {
            warning = inlineNotice('bad', 'The last sweep failed', esc(policy.detail));
        }

        const diffText = hasChange
            ? changes.join(' · ') + ' · ' + nextLine(draft)
            : 'No change pending.';
        const effect = hasChange ? effectText(store, policy, draft) : 'The policy reads as saved.';
        const audit = hasChange ? auditUpdate(policy, draft) : 'No audit line: the policy is unchanged.';

        return '<div class="details c3 editorfacts">' +
            detail('Policy', esc(policy.id), 'mono') +
            detail('Target', esc(policy.target), 'mono') +
            detail('Action', esc(actionLabel(policy.action))) +
            detail('Enabled', policy.enabled ? 'Yes' : 'No — switched off') +
            detail('Period', policy.periodDays + ' days') +
            detail('Cadence', 'every ' + policy.cadenceHours + ' h') +
            detail('Last outcome', esc(policy.lastOutcome)) +
            detail('Last sweep', esc(policy.lastSweep)) +
            detail('Next due sweep', esc(nextSweepFor(policy))) +
            detail('Store size', esc(store.size)) +
            detail('Freed by the last sweep', esc(policy.freed)) +
            detail('Bounded by', esc(actionLabel(policy.action) + ' · ' + policy.periodDays + ' days')) +
            '</div>' +
            warning +
            '<form class="editorform" data-act="editor" aria-label="Policy fields">' +
            '<div class="formgrid">' +
            '<div class="control"><label for="period">Period</label><select id="period" data-act="draft-period">' +
            periodOptions(draft.periodDays) + '</select><span class="hint">The age past which the action applies.</span></div>' +
            '<div class="control"><label for="cadence">Cadence</label><select id="cadence" data-act="draft-cadence">' +
            cadenceOptions(draft.cadenceHours) + '</select><span class="hint">How often the sweep may act; it ticks hourly at :15.</span></div>' +
            enabledField(draft) +
            reasonField(draft) +
            '</div>' +
            '<div class="savebar">' +
            '<p class="diff">' + (hasChange ? '<b>Will change:</b> ' : '') + esc(diffText) + '</p>' +
            '<p class="effect">' + esc(effect) + '</p>' +
            '<p class="audit-label">Audit line to be recorded</p>' +
            '<p class="audit">' + esc(audit) + '</p>' +
            '<div class="saveactions">' +
            '<button type="button" class="btn primary" data-act="save"' + (canSave ? '' : ' disabled') + '>Save policy</button>' +
            (hasChange && !reasonOk ? '<span class="tag warn">Reason required</span>' : '') +
            '<span class="note">Saves are mock; nothing is written.</span>' +
            '</div></div></form>';
    }

    function addEditor(store) {
        const draft = S.draft;
        const name = policyName(store, draft);
        const reasonOk = draft.reason.trim() !== '';
        const unsupported = draft.action !== 'drop_chunks';
        const canSave = reasonOk;

        const diffText = name + ' · ' + actionLabel(draft.action) + ' · ' + draft.periodDays +
            ' days · every ' + draft.cadenceHours + ' h · ' + (draft.enabled ? 'enabled' : 'disabled') +
            ' · ' + nextLine(draft);
        const effect = addEffect(store, draft);
        const audit = auditCreate(store, draft);

        return '<div class="details c3 editorfacts">' +
            detail('Target', esc(store.name), 'mono') +
            detail('Action the store requires', esc(actionLabel(store.defaultAction))) +
            detail('Store size', esc(store.size)) +
            detail('Measured', esc(store.measure)) +
            detail('Bounded by', 'Nothing') +
            detail('Freed by the last sweep', '—') +
            '</div>' +
            inlineNotice('info', 'No policy covers this store', esc(store.note)) +
            '<form class="editorform" data-act="editor" aria-label="New policy fields">' +
            '<div class="formgrid">' +
            '<div class="control"><label for="action">Action</label><select id="action" data-act="draft-action">' +
            actionOptions(draft.action) + '</select><span class="hint">Only a chunk drop is applied by the sweep today.</span></div>' +
            '<div class="control"><label for="period">Period</label><select id="period" data-act="draft-period">' +
            periodOptions(draft.periodDays) + '</select><span class="hint">The age past which the action applies.</span></div>' +
            '<div class="control"><label for="cadence">Cadence</label><select id="cadence" data-act="draft-cadence">' +
            cadenceOptions(draft.cadenceHours) + '</select><span class="hint">How often the sweep may act; it ticks hourly at :15.</span></div>' +
            enabledField(draft) +
            reasonField(draft) +
            '</div>' +
            (unsupported ? '<div class="notice warn compact"><p class="title">This action will not bound the store</p><p>' +
                'The sweep applies only a chunk drop today. A ' + esc(actionLabel(draft.action).toLowerCase()) +
                ' policy is recorded as skipped, so nothing removes what the store holds.</p></div>' : '') +
            '<div class="savebar">' +
            '<p class="diff"><b>Will add:</b> ' + esc(diffText) + '</p>' +
            '<p class="effect">' + esc(effect) + '</p>' +
            '<p class="audit-label">Audit line to be recorded</p>' +
            '<p class="audit">' + esc(audit) + '</p>' +
            '<div class="saveactions">' +
            '<button type="button" class="btn primary" data-act="save"' + (canSave ? '' : ' disabled') + '>Add policy</button>' +
            (!reasonOk ? '<span class="tag warn">Reason required</span>' : '') +
            '<span class="note">Saves are mock; nothing is written.</span>' +
            '</div></div></form>';
    }

    function keyedEditor(store) {
        return inlineNotice('info', 'Bounded without a policy',
            esc(store.note) + ' The editor offers no change: there is nothing age-based to set.') +
            '<div class="details c3 editorfacts" style="margin-top:16px">' +
            detail('Store', esc(store.name), 'mono') +
            detail('Kind', esc(store.kind)) +
            detail('Size', esc(store.size)) +
            detail('Bounded by', 'Content digest keying') +
            '</div>' +
            '<div class="section-actions"><button type="button" class="btn secondary small" data-act="runs-store" data-store="' +
            store.id + '">See its runs</button></div>';
    }

    function editor(store) {
        const policy = policyFor(store);
        const head = '<header class="editorhead"><div><h2 id="editor-heading">' + esc(store.label) + '</h2>' +
            '<p class="storetarget">' + esc(store.name) + '</p>' +
            '<p class="sub">' + esc(store.kind) + ' · ' + esc(store.where) + ' · ' + esc(store.size) + '</p></div>' +
            statusTag(store) + '</header>';

        if (store.keyed) return head + keyedEditor(store);
        if (policy) return head + savedNotice() + policyEditor(store, policy);
        return head + savedNotice() + addEditor(store);
    }

    function viewC() {
        const store = storeById(S.selected) || STORES[0];

        return '<div class="panes">' +
            '<aside class="stores-pane" aria-labelledby="stores-heading">' +
            '<h2 class="panehead" id="stores-heading">Stores <span class="meta">' + STORES.length + '</span></h2>' +
            '<ul class="storelist">' +
            STORES.map(function (item) {
                const status = statusOf(item);
                const selected = S.selected === item.id;
                return '<li><button type="button" data-act="select" data-store="' + item.id + '"' +
                    (selected ? ' aria-current="true"' : '') + '>' +
                    '<span class="lname">' + esc(item.name) + '</span>' +
                    '<span class="lmeta">' + esc(item.size) + ' · ' + esc(item.kind) + '</span>' +
                    '<span class="tag ' + status.tone + '">' + esc(status.label) + '</span>' +
                    '</button></li>';
            }).join('') +
            '</ul></aside>' +
            '<section class="editor" aria-labelledby="editor-heading">' + editor(store) + '</section>' +
            '</div>';
    }

    /* --------------------------------------------------------- runs view */

    function runsView() {
        const all = runRows();
        const filtered = S.runFilter
            ? all.filter(function (row) { return row.storeId === S.runFilter; })
            : all;
        const filterStore = S.runFilter ? storeById(S.runFilter) : null;

        const applied = filtered.filter(function (row) { return row.outcome === 'applied'; }).length;
        const skipped = filtered.filter(function (row) { return row.outcome === 'skipped'; }).length;
        const failed = filtered.filter(function (row) { return row.outcome === 'failed'; }).length;

        let body;
        if (all.length === 0) {
            body = '<p class="empty">No run has been recorded. The sweep has no retention policy to apply.</p>' +
                '<div class="section-actions"><button type="button" class="btn primary small" data-act="first-policy">Add the first policy</button></div>';
        } else if (filtered.length === 0) {
            body = '<p class="empty">No run is recorded for this store. ' +
                (filterStore && filterStore.keyed ? 'It needs none: the digest keying bounds it.' : 'The first sweep under a policy will appear here.') + '</p>';
        } else {
            body = '<div class="table-wrap"><table class="data">' +
                '<caption class="visually-hidden">Housekeeping runs, newest first</caption>' +
                '<thead><tr><th>Time</th><th>Store</th><th>Policy</th><th>Action</th><th>Outcome</th><th>Freed</th><th>Detail</th></tr></thead><tbody>' +
                filtered.map(function (row) {
                    return '<tr>' +
                        '<td class="mono">24 Nov, ' + esc(row.at) + '</td>' +
                        '<td class="mono">' + esc(row.store) + '</td>' +
                        '<td class="mono">' + esc(row.policy) + '</td>' +
                        '<td>' + esc(row.action) + '</td>' +
                        '<td>' + outcomeTag(row.outcome) + '</td>' +
                        '<td class="num">' + esc(row.freed) + '</td>' +
                        '<td class="detailcell">' + esc(row.detail) + '</td>' +
                        '</tr>';
                }).join('') +
                '</tbody></table></div>';
        }

        return '<section class="card runview">' +
            '<header><div><h2>Runs</h2><p class="meta">Each sweep, the policy it applied, what it freed, and why it was skipped or failed.</p></div>' +
            '<span class="meta">' + all.length + ' runs in the window</span></header>' +
            (filterStore
                ? '<p class="runfilter">Showing one store: <span class="mono">' + esc(filterStore.name) + '</span>' +
                '<button type="button" class="btn secondary small" data-act="clear-runfilter">Show all runs</button></p>'
                : '') +
            '<div class="runsummary">' +
            (applied ? '<span class="tag ok">' + applied + ' applied</span>' : '') +
            (skipped ? '<span class="tag warn">' + skipped + ' skipped</span>' : '') +
            (failed ? '<span class="tag bad">' + failed + ' failed</span>' : '') +
            '</div>' +
            body +
            '<p class="note">The sweep ticks hourly at :15. A run states the policy, the action, the bytes freed and a detail ' +
            'line. A skipped run is not an error: the policy is enabled but the sweep cannot perform its action today.' +
            (S.state === 'fresh' ? ' The fresh fixture records no skipped and no failed run; choose the Skipped or Failed state for those rows.' : '') +
            '</p>' +
            '</section>';
    }

    /* --------------------------------------------------------- render */

    function buildView() {
        if (S.view === 'runs') return runsView();
        if (S.variant === 'B') return viewB();
        if (S.variant === 'C') return viewC();
        return viewA();
    }

    function render() {
        const active = document.activeElement;
        const focusId = active && active.id ? active.id : null;
        let caret = null;
        if (active && active.tagName === 'TEXTAREA' && typeof active.selectionStart === 'number') {
            caret = active.selectionStart;
        }

        ensureDraft();

        document.getElementById('app').innerHTML =
            '<div class="page">' + header() + statsLine() + tabs() + stateBanner() +
            '<div id="view-panel" role="tabpanel">' + buildView() + '</div></div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE · mock data, no service · variant ' + S.variant +
            ' · state ' + S.state + ' · signed in as system administrator, tenant system';

        renderState();
        renderBar();

        if (focusId) {
            const el = document.getElementById(focusId);
            if (el) {
                el.focus();
                if (caret !== null && el.tagName === 'TEXTAREA') {
                    el.setSelectionRange(caret, caret);
                }
            }
        }
    }

    function renderState() {
        const variant = activeVariant();
        const state = activeState();
        const selected = storeById(S.selected);

        document.getElementById('proto-state').hidden = !S.panel;
        document.getElementById('proto-state').innerHTML =
            '<p class="gist">Variant <b>' + esc(variant.id + ' — ' + variant.name) + '</b>. ' + esc(variant.gist) + '</p>' +
            '<p class="gist">State <b>' + esc(state.id) + '</b>. ' + esc(state.gist) + '</p>' +
            '<div class="grid">' +
            '<span>view: <span class="v">' + esc(S.view) + '</span></span>' +
            '<span>selected: <span class="v">' + esc(selected ? selected.label : '—') + '</span></span>' +
            '<span>policies: <span class="v">' + policies().length + '</span></span>' +
            '<span>not bounded: <span class="v">' + unboundedCount() + ' of ' + STORES.length + '</span></span>' +
            '<span>runs: <span class="v">' + runRows().length + '</span></span>' +
            '<span>run filter: <span class="v">' + esc(S.runFilter || 'all stores') + '</span></span>' +
            '</div>' +
            '<ul>' +
            '<li>The fixture holds nine stores: four chunked tables, one ordinary table, two object prefixes and two scratch stores. Every size and growth figure is mock.</li>' +
            '<li><span class="mono">publish/run/packages</span> is bounded by its content digest keying, not by a policy. It is the one store marked bounded without one.</li>' +
            '<li>The sweep ticks hourly at :15, so a 24 h policy applied at 17:15 is next due at 17:15 tomorrow. Saves state the next sweep and the audit line but write nothing.</li>' +
            '<li>The fresh fixture holds three applied runs at 17:15: the sweep tick and the two drop-chunks policies. Each freed 0 B because no chunk is older than 90 days.</li>' +
            '<li>Skipped, failed, disabled and empty are separate fixtures, not a sequence. Switching state resets mock edits made in variant C.</li>' +
            '<li>No size read and no dry run exist in the platform, so the screen states what it knows and says when it cannot state an effect.</li>' +
            '</ul>';
    }

    function renderBar() {
        const variantButtons = VARIANTS.map(function (v) {
            return '<button type="button" data-act="variant" data-variant="' + v.id + '" title="' + esc(v.name) + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.id) + '</button>';
        }).join('');
        const stateButtons = STATES.map(function (s) {
            return '<button type="button" data-act="state" data-state="' + s.id + '"' +
                (S.state === s.id ? ' class="on"' : '') + '>' + esc(s.name) + '</button>';
        }).join('');
        const viewButtons = ['stores', 'runs'].map(function (view) {
            return '<button type="button" data-act="view" data-view="' + view + '"' +
                (S.view === view ? ' class="on"' : '') + '>' + (view === 'stores' ? 'Stores' : 'Runs') + '</button>';
        }).join('');

        document.getElementById('proto-bar').innerHTML =
            '<span class="label">variant <b>' + S.variant + '</b></span>' + variantButtons +
            '<span class="sep">|</span><span class="label">state</span>' + stateButtons +
            '<span class="sep">|</span><span class="label">view</span>' + viewButtons +
            '<span class="sep">|</span>' +
            '<button type="button" data-act="panel">' + (S.panel ? 'Hide state' : 'Show state') + '</button>';
    }

    /* ---------------------------------------------------------- save */

    function saveDraft() {
        const store = storeById(S.selected);
        if (!store || !S.draft) return;

        const policy = policyFor(store);
        const reason = S.draft.reason.trim();
        if (reason === '') return;

        if (policy) {
            const changes = diffChanges(policy, S.draft);
            if (changes.length === 0) return;
            S.overrides[policy.id] = {
                periodDays: S.draft.periodDays,
                cadenceHours: S.draft.cadenceHours,
                enabled: S.draft.enabled
            };
            S.notice = {
                title: 'Saved (mock): ' + policy.id,
                body: changes.join(' · ') + '. ' + effectText(store, policy, S.draft),
                audit: auditUpdate(policy, S.draft)
            };
        } else if (!store.keyed) {
            const name = policyName(store, S.draft);
            S.added.push({
                id: name,
                storeId: store.id,
                target: store.name,
                action: S.draft.action,
                periodDays: S.draft.periodDays,
                cadenceHours: S.draft.cadenceHours,
                enabled: S.draft.enabled,
                reason: reason,
                lastOutcome: 'pending',
                lastSweep: 'Never swept',
                freed: '—',
                detail: 'The policy has not been applied yet.'
            });
            S.notice = {
                title: 'Policy added (mock): ' + name,
                body: addEffect(store, S.draft),
                audit: auditCreate(store, S.draft)
            };
        }

        S.draftKey = null;
        render();
    }

    /* --------------------------------------------------------- wiring */

    function writeParams() {
        try {
            const params = new URLSearchParams();
            params.set('variant', S.variant);
            params.set('state', S.state);
            params.set('view', S.view);
            if (S.selected) params.set('store', S.selected);
            if (S.runFilter) params.set('runsfor', S.runFilter);
            if (S.onlyUnbounded) params.set('filter', 'unbounded');
            window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
        } catch (err) {
            /* A page opened from the file system may refuse to rewrite its
               address; the screen still works without it. */
        }
    }

    function readParams() {
        const params = new URLSearchParams(window.location.search);
        if (variantById(params.get('variant'))) S.variant = params.get('variant');
        if (stateById(params.get('state'))) S.state = params.get('state');
        if (params.get('view') === 'runs') S.view = 'runs';
        if (params.get('view') === 'stores') S.view = 'stores';
        if (storeById(params.get('store'))) S.selected = params.get('store');
        if (storeById(params.get('runsfor'))) S.runFilter = params.get('runsfor');
        if (params.get('filter') === 'unbounded') S.onlyUnbounded = true;
        if (!storeById(params.get('store'))) {
            if (S.state === 'empty') S.selected = 'calendar_dates';
            else if (S.state === 'skipped') S.selected = 'delivery_counts';
            else if (S.state === 'failed' || S.state === 'disabled') S.selected = 'report_cashflows';
        }
    }

    document.addEventListener('click', function (ev) {
        const el = ev.target.closest('[data-act]');
        if (!el) return;
        const act = el.getAttribute('data-act');

        if (act === 'panel') {
            ev.preventDefault();
            S.panel = !S.panel;
            renderState();
            renderBar();
            return;
        }

        if (['variant', 'state', 'view', 'select', 'back', 'edit', 'cancel-edit', 'runs-store', 'clear-runfilter', 'first-policy', 'edit-store', 'save', 'filter-unbounded'].indexOf(act) < 0) {
            return;
        }

        ev.preventDefault();

        if (act === 'save') {
            // Closed before the draft is written, because the save renders.
            S.editing = null;
            saveDraft();
            writeParams();
            return;
        }

        if (act === 'variant') {
            S.variant = el.getAttribute('data-variant');
            S.notice = null;
        } else if (act === 'state') {
            S.state = el.getAttribute('data-state');
            S.overrides = {};
            S.added = [];
            S.draftKey = null;
            S.notice = null;
            S.runFilter = null;
            S.selected = null;
            S.editing = null;
            S.onlyUnbounded = false;
        } else if (act === 'view') {
            S.view = el.getAttribute('data-view');
            if (S.view === 'runs') S.runFilter = null;
        } else if (act === 'select') {
            selectStore(el.getAttribute('data-store'));
        } else if (act === 'edit') {
            S.editing = el.getAttribute('data-store');
            S.draftKey = null;
        } else if (act === 'cancel-edit') {
            S.editing = null;
            S.draftKey = null;
            S.notice = null;
        } else if (act === 'back') {
            S.selected = null;
            if (S.view === 'runs' && S.runFilter) S.runFilter = null;
        } else if (act === 'runs-store') {
            S.view = 'runs';
            S.runFilter = el.getAttribute('data-store');
        } else if (act === 'clear-runfilter') {
            S.runFilter = null;
        } else if (act === 'filter-unbounded') {
            S.onlyUnbounded = !S.onlyUnbounded;
            S.view = 'stores';
            S.selected = null;
        } else if (act === 'first-policy' || act === 'edit-store') {
            const target = act === 'first-policy' ? 'calendar_dates' : el.getAttribute('data-store');
            if (S.variant === 'A') {
                S.editing = target;
                selectStore(target);
            } else {
                S.variant = 'C';
                S.view = 'stores';
                selectStore(target);
            }
            S.draftKey = null;
            S.notice = null;
        }

        writeParams();
        render();
    });

    document.addEventListener('change', function (ev) {
        const el = ev.target.closest('[data-act]');
        if (!el || !S.draft) return;
        const act = el.getAttribute('data-act');

        if (act === 'draft-period') S.draft.periodDays = Number(el.value);
        else if (act === 'draft-cadence') S.draft.cadenceHours = Number(el.value);
        else if (act === 'draft-enabled') S.draft.enabled = el.checked;
        else if (act === 'draft-action') {
            S.draft.action = el.value;
            if (!S.draft.kind) S.draft.kind = 'new';
        } else {
            return;
        }

        S.notice = null;
        writeParams();
        render();
    });

    document.addEventListener('input', function (ev) {
        const el = ev.target.closest('[data-act="draft-reason"]');
        if (!el || !S.draft) return;
        S.draft.reason = el.value;
        S.notice = null;
        render();
    });

    document.addEventListener('submit', function (ev) {
        if (ev.target.matches('[data-act="editor"]')) ev.preventDefault();
    });

    readParams();
    render();
})();
