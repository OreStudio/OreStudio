/* Operations: compute grid prototype. Self-contained: plain JavaScript, mock
 * data, no framework, no build step, and nothing that outlives the page.
 *
 * Watch the compute grid, from
 * doc/knowledge/journeys/operations/journey_watch_the_compute_grid.org.
 *
 * The summary and the node rows are fixtures shaped by the
 * compute.v1.telemetry.get_grid_stats reply: the newest stored grid sample and
 * the latest sample of each node. The wrappers are fixtures shaped by
 * telemetry.v1.services.list; they belong here rather than on the services
 * screen, because a wrapper runs on a node. Nothing here reads the server. */

(function () {
    'use strict';

    var VARIANTS = [
        {
            id: 'sampled',
            name: 'Sampled',
            gist: 'Four nodes, one of them quiet for three hours, one with no host record.'
        },
        {
            id: 'nosample',
            name: 'No sample yet',
            gist: 'The deployment stores no grid summary; the node table stands alone.'
        }
    ];

    /* compute.v1.telemetry.get_grid_stats: the summary and one row per node. */
    var gridStats = {
        sampledAt: '14:31:02',
        totalHosts: 6,
        onlineHosts: 5,
        idleHosts: 2,
        resultsInactive: 4,
        resultsUnsent: 9,
        resultsInProgress: 3,
        resultsDone: 112,
        totalWorkunits: 128,
        totalBatches: 9,
        activeBatches: 3,
        outcomesSuccess: 412,
        outcomesClientError: 3,
        outcomesNoReply: 1,
        nodes: [
            {
                hostId: '9e0f33aa',
                host: 'grid-01.example.com',
                tasksCompleted: 1284,
                tasksSinceLast: 12,
                avgTaskDurationMs: 42000,
                inputBytesFetched: 1288490188,
                outputBytesUploaded: 230686720,
                secondsSinceHeartbeat: 8
            },
            {
                hostId: '41c87b02',
                host: 'grid-02.example.com',
                tasksCompleted: 903,
                tasksSinceLast: 4,
                avgTaskDurationMs: 38000,
                inputBytesFetched: 838860800,
                outputBytesUploaded: 146800640,
                secondsSinceHeartbeat: 12
            },
            {
                hostId: 'ad55e110',
                host: 'grid-05.example.com',
                tasksCompleted: 101,
                tasksSinceLast: 0,
                avgTaskDurationMs: undefined,
                inputBytesFetched: 41943040,
                outputBytesUploaded: 2097152,
                secondsSinceHeartbeat: 11520
            },
            {
                hostId: 'c30b9d47',
                host: undefined,
                tasksCompleted: 57,
                tasksSinceLast: 2,
                avgTaskDurationMs: 51000,
                inputBytesFetched: 18874368,
                outputBytesUploaded: 3145728,
                secondsSinceHeartbeat: 31
            }
        ]
    };

    /* The compute wrapper rows, as telemetry.v1.services.list reports them. */
    var serviceInstances = [
        { serviceName: 'ores.compute.wrapper', instanceId: '1a90fe12-5b3c-4d6e-8f70-91a2b3c4d5e6', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 5 },
        { serviceName: 'ores.compute.wrapper', instanceId: '84b36cd1-7c2e-4a09-93b1-2c3d4e5f6a7b', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 7 },
        { serviceName: 'ores.compute.wrapper', instanceId: 'f27a03be-9d4f-4b21-84c3-5e6f7a8b9c0d', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 6 },
        { serviceName: 'ores.compute.wrapper', instanceId: '5c18e4a9-1e0f-4c32-95d5-8a9b0c1d2e3f', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 8 },
        { serviceName: 'ores.compute.wrapper', instanceId: undefined, state: 'missing', version: undefined, lastHeartbeatSeconds: undefined }
    ];

    var computeWrapperServiceName = 'ores.compute.wrapper';

    var GAPS = [
        {
            title: 'The read narrows a shared grid to one tenant',
            body: 'The grid belongs to the installation: every tenant\u2019s work runs on the same hosts. The read filters by the tenant of the session anyway, so an administrator sees one tenant\u2019s slice of it. The read must serve the whole grid.'
        },
        {
            title: 'The failures are stored and then dropped',
            body: 'The ingest carries the failed task count and the longest task, and the node summary drops both. A node failing every task reads as a node doing nothing.'
        },
        {
            title: 'No history behind the sample',
            body: 'The read serves the newest stored sample and no series, so a trend cannot be drawn from these operations. The poller keeps writing samples that no read returns.'
        },
        {
            title: 'A node with no host record shows its identifier only',
            body: 'The host names come from compute.v1.hosts.list joined by host id; a node whose host row is missing keeps its row with no name.'
        },
        {
            title: 'A wrapper cannot be placed on its node',
            body: 'The wrapper heartbeat carries the service name, the instance id and the release, and no host. The node sample carries the host and no release. Nothing joins the two, so the wrappers are listed beside the nodes rather than on them. A host id on the heartbeat would join them, and each node row could then state the release it runs.'
        },
        {
            title: 'No permission gates the read',
            body: 'The handler authenticates the caller and checks nothing else.'
        }
    ];

    /* `state` is the point in the read: rest is the screen as first drawn,
       refreshed is after the refresh control was pressed. */
    var S = { variant: 'sampled', state: 'rest', updatedAt: '14:31:12', showState: true };
    var log = [];

    function variantById(id) {
        return VARIANTS.filter(function (v) { return v.id === id; })[0];
    }

    function refresh() {
        S.updatedAt = '14:32:10';
        S.state = 'refreshed';
        log.push('refresh \u00b7 re-read the summary and the nodes at 14:32:10');
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (variantById(p.get('variant'))) S.variant = p.get('variant');
        if (p.get('state') === 'refreshed') refresh();
        else if (p.get('state') === 'rest') S.state = 'rest';
    }

    function writeParams() {
        var p = new URLSearchParams();
        p.set('variant', S.variant);
        p.set('state', S.state);
        window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
    }

    function variant() {
        return VARIANTS.filter(function (v) { return v.id === S.variant; })[0] || VARIANTS[0];
    }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function asGiB(bytes) {
        return (bytes / 1024 / 1024 / 1024).toFixed(2) + ' GiB';
    }

    function asMiB(bytes) {
        return Math.round(bytes / 1024 / 1024) + ' MB';
    }

    function asMinutes(seconds) {
        if (seconds < 60) return String(seconds) + ' s';
        var minutes = Math.floor(seconds / 60);
        if (minutes < 60) return String(minutes) + ' m ' + String(seconds % 60) + ' s';
        return String(Math.floor(minutes / 60)) + ' h ' + String(minutes % 60) + ' m';
    }

    function newestVersionOf(running) {
        var newest;
        running.forEach(function (instance) {
            if (instance.version === undefined) return;
            if (newest === undefined || instance.version > newest) newest = instance.version;
        });
        return newest;
    }

    // --------------------------------------------------------------- parts

    function instanceStateTag(state) {
        if (state === 'running') return '<span class="tag accent">running</span>';
        if (state === 'stopped') return '<span class="tag muted">stopped</span>';
        return '<span class="tag warn">missing</span>';
    }

    function instanceVersion(instance, newestVersion) {
        if (instance.version === undefined) return '<span class="mono faint">\u2014</span>';
        return '<span class="cellgroup"><span class="mono">' + esc(instance.version) + '</span>' +
            (instance.state === 'running' && instance.version !== newestVersion
                ? '<span class="tag warn">older build</span>' : '') + '</span>';
    }

    function summaryPanel() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>Grid summary</h2>' +
            '<span class="faint" style="font-size:12px">sampled ' + esc(gridStats.sampledAt) + '</span></header>' +
            '<div class="statgrid">' +
            '<div><span class="lbl">Hosts</span><span class="val">' +
            '<span class="mono">' + gridStats.totalHosts + '</span>' +
            '<span class="tag">Online ' + gridStats.onlineHosts + '</span>' +
            '<span class="tag">Idle ' + gridStats.idleHosts + '</span></span></div>' +
            '<div><span class="lbl">Work</span><span class="val">' +
            '<span class="mono">' + gridStats.totalWorkunits + ' workunits \u00b7 ' +
            gridStats.totalBatches + ' batches</span>' +
            '<span class="tag accent">Active ' + gridStats.activeBatches + '</span></span></div>' +
            '<div><span class="lbl">Outcomes</span><span class="val">' +
            '<span class="tag">' + gridStats.outcomesSuccess + ' success</span>' +
            '<span class="tag warn">' + gridStats.outcomesClientError + ' client error</span>' +
            '<span class="tag warn">' + gridStats.outcomesNoReply + ' no reply</span></span></div>' +
            '</div>' +
            '<p class="rowfoot">The grid is the installation\u2019s: every tenant\u2019s work runs on the same hosts. ' +
            'The failure counts are the outcomes the server sends; the per-node failures are dropped ' +
            'before they arrive (see below).</p>' +
            '</section>';
    }

    function noSamplePanel() {
        return '<section class="card">' +
            '<h2 style="font-size:17px;margin-bottom:10px">Grid summary</h2>' +
            '<div class="notice warn">No sample yet. The deployment stores no grid summary, so the summary ' +
            'cannot be drawn; the node table below stands alone.</div>' +
            '</section>';
    }

    function nodeRow(node) {
        var quiet = node.secondsSinceHeartbeat > 300;
        var name = node.host === undefined
            ? '<span class="cellgroup"><span class="mono">' + esc(node.hostId) + '</span>' +
              '<span class="tag muted">no host record</span></span>'
            : '<span class="mono">' + esc(node.host) + '</span>';
        return '<tr>' +
            '<td>' + name + '</td>' +
            '<td class="mono">' + node.tasksCompleted + '</td>' +
            '<td class="mono">' + node.tasksSinceLast + '</td>' +
            '<td class="mono">' +
                (node.avgTaskDurationMs === undefined
                    ? '-' : String(Math.round(node.avgTaskDurationMs / 1000)) + ' s') + '</td>' +
            '<td class="mono">' + esc(asGiB(node.inputBytesFetched)) + '</td>' +
            '<td class="mono">' + esc(asMiB(node.outputBytesUploaded)) + '</td>' +
            '<td>' + (quiet
                ? '<span class="tag warn">' + esc(asMinutes(node.secondsSinceHeartbeat)) + '</span>'
                : '<span class="mono">' + esc(asMinutes(node.secondsSinceHeartbeat)) + '</span>') + '</td>' +
            '</tr>';
    }

    function nodesPanel() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>Nodes</h2>' +
            '<span class="faint" style="font-size:12px">' + gridStats.nodes.length + ' rows</span></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Node</th><th>Tasks done</th><th>Since last</th><th>Mean time</th>' +
            '<th>Fetched</th><th>Uploaded</th><th>Since heartbeat</th>' +
            '</tr></thead><tbody>' + gridStats.nodes.map(nodeRow).join('') + '</tbody></table></div>' +
            '<p class="rowfoot">Read-only. A node whose last column grows is the one to look at; ' +
            'the node keeps its row while it is quiet.</p>' +
            '<div class="actions" style="display:flex;flex-wrap:wrap;align-items:center;gap:12px;margin-top:14px">' +
            '<button class="btn" disabled title="Opening a node has no journey yet: it waits for the compute journeys.">' +
            'Open the node</button>' +
            '<span class="faint" style="font-size:12px">Waits for the compute journeys, which own the host and workunit screens.</span>' +
            '</div></section>';
    }

    function wrappersPanel() {
        var wrappers = serviceInstances.filter(function (instance) {
            return instance.serviceName === computeWrapperServiceName;
        });
        var running = wrappers.filter(function (instance) { return instance.state === 'running'; });
        var stopped = wrappers.filter(function (instance) { return instance.state === 'stopped'; });
        var missing = wrappers.filter(function (instance) { return instance.state === 'missing'; });
        var newestVersion = newestVersionOf(running);

        var rows = wrappers.map(function (instance) {
            return '<tr>' +
                '<td class="mono" title="' + esc(instance.instanceId) + '">' +
                    (instance.instanceId === undefined ? '\u2014' : esc(instance.instanceId.slice(0, 8))) + '</td>' +
                '<td>' + instanceStateTag(instance.state) + '</td>' +
                '<td>' + instanceVersion(instance, newestVersion) + '</td>' +
                '<td class="mono">' +
                    (instance.lastHeartbeatSeconds === undefined ? '\u2014'
                        : esc(asMinutes(instance.lastHeartbeatSeconds)) + ' ago') + '</td>' +
                '</tr>';
        }).join('');

        return '<section class="card">' +
            '<header class="sectionhead"><h2>Compute wrappers</h2>' +
            '<div class="meta"><span>' + running.length + ' of ' + wrappers.length +
            ' reported in the last five minutes</span>' +
            (stopped.length > 0 ? '<span class="tag muted">' + stopped.length + ' stopped</span>' : '') +
            (missing.length > 0 ? '<span class="tag warn">' + missing.length + ' missing</span>' : '') +
            '</div></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Instance</th><th>Status</th><th>Version</th><th>Last heartbeat</th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="rowfoot">One wrapper runs on each node and takes the work that node runs. ' +
            'The roster is the registry\u2019s five replicas; the rows are telemetry.v1.services.list. ' +
            'The node rows above are the samples these wrappers publish, and nothing joins the two yet (see below).</p>' +
            '</section>';
    }

    function gapPanel() {
        return '<section class="card gaps">' +
            '<header class="sectionhead"><h2>Not on this screen yet</h2>' +
            '<span class="faint" style="font-size:12px">each gap names the journey that records it</span></header>' +
            '<dl>' + GAPS.map(function (gap) {
                return '<div><dt>' + esc(gap.title) + '</dt><dd>' + esc(gap.body) + '</dd></div>';
            }).join('') + '</dl></section>';
    }

    function pageHead() {
        return '<header class="pagehead"><div>' +
            '<h1>Operations: compute grid</h1>' +
            '<p class="lede">The installation\u2019s host and work summary, one row per node, ' +
            'and the compute wrappers that report for those nodes.</p>' +
            '</div><div class="actions">' +
            '<span class="updated">Updated ' + esc(S.updatedAt) + '</span>' +
            '<button class="btn" data-act="refresh">Refresh</button>' +
            '<a class="btn linklike" href="../index.html">Back to prototypes</a>' +
            '</div></header>';
    }

    function render() {
        var wrappers = serviceInstances.filter(function (instance) {
            return instance.serviceName === computeWrapperServiceName;
        });
        var wrappersRunning = wrappers.filter(function (instance) { return instance.state === 'running'; });

        var body = pageHead() +
            '<div class="notice warn">PROTOTYPE. Every row below is a fixture shaped by the ' +
            'compute.v1.telemetry.get_grid_stats and the telemetry.v1.services.list replies. ' +
            'Nothing on this page reads the server.</div>' +
            (S.variant === 'sampled' ? summaryPanel() : noSamplePanel()) +
            nodesPanel() +
            wrappersPanel() +
            gapPanel();

        document.getElementById('app').innerHTML =
            '<div class="shell">' +
            '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="../index.html">Operations</a></nav>' +
            '<span class="modechip">System administration</span>' +
            '</div></header>' +
            '<main>' + body + '</main></div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 signed in as system administrator, on the system tenant' +
            ' \u00b7 variant ' + S.variant + ' \u00b7 state ' + S.state;

        renderBar(wrappers, wrappersRunning);
    }

    function renderBar(wrappers, wrappersRunning) {
        var variantButtons = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + v.id + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.name) + '</button>';
        }).join('');
        var active = variant();
        var state = '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>fixture: <b>' + esc(active.id) + '</b></span>' +
            '<span>nodes: <b>' + String(gridStats.nodes.length) + '</b></span>' +
            '<span>wrappers reporting: <b>' + wrappersRunning.length + ' of ' + wrappers.length + '</b></span>' +
            '<span>updated: <b>' + esc(S.updatedAt) + '</b></span>' +
            '</div>' +
            '<p class="state-note">Signed in as system administrator, on the system tenant.</p>' +
            (log.length === 0
                ? '<p class="state-note">No action yet.</p>'
                : '<ol>' + log.map(function (entry, index) {
                    return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
                }).join('') + '</ol>') +
            '</div>';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + variantButtons +
            '<span class="gist">' + esc(active.gist) + '</span>' +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') + '</button>' +
            '</div>' + (S.showState ? state : '');
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'variant') {
            S.variant = el.getAttribute('data-variant');
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
