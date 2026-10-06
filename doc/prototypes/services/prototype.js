/* Operations: services prototype. Self-contained: plain JavaScript, mock data,
 * no framework, no build step, and nothing that outlives the page.
 *
 * See the running services, from
 * doc/knowledge/journeys/operations/journey_see_the_running_services.org.
 *
 * The screen answers the roster the registry expects with the samples the
 * instances send, so a service that stopped keeps its row and a version that
 * lags behind is visible. Every row is a fixture: the roster comes from the
 * service registry and the state from the installation's service manager, and
 * no operation serves either yet. Nothing here reads the server.
 *
 * The compute wrappers belong to the grid screen, which shows them against the
 * nodes they run on, so this screen filters them out. */

(function () {
    'use strict';

    var VARIANTS = [
        {
            id: 'reporting',
            name: 'Reporting',
            gist: 'The roster the registry expects, met by the samples: one instance is stopped and one build is older.'
        },
        {
            id: 'nothing',
            name: 'Nothing reported',
            gist: 'No instance reported in five minutes; every row says so, and the screen cannot say why.'
        }
    ];

    /* The state of one expected instance, as the services screen needs it. The
       state comes from the installation's own service manager, the one
       `compass services status` reports with the same words: running, stopped,
       failed, missing. The fixture carries the states the design needs. */
    var serviceInstances = [
        { serviceName: 'ores.analytics.service', instanceId: 'a3f81c02-6d44-4b0e-9c21-7f5e0d8a1b34', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 6 },
        { serviceName: 'ores.assets.service', instanceId: '5d21b7e4-90c1-4a37-b8f4-2e6d9c05a7b1', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 9 },
        { serviceName: 'ores.compute.service', instanceId: '6f2c19a1-3e7d-4c58-a1b2-0d4f8e6c9a23', state: 'running', version: 'v0.0.24', lastHeartbeatSeconds: 4 },
        { serviceName: 'ores.compute.wrapper', instanceId: '1a90fe12-5b3c-4d6e-8f70-91a2b3c4d5e6', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 5 },
        { serviceName: 'ores.compute.wrapper', instanceId: '84b36cd1-7c2e-4a09-93b1-2c3d4e5f6a7b', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 7 },
        { serviceName: 'ores.compute.wrapper', instanceId: 'f27a03be-9d4f-4b21-84c3-5e6f7a8b9c0d', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 6 },
        { serviceName: 'ores.compute.wrapper', instanceId: '5c18e4a9-1e0f-4c32-95d5-8a9b0c1d2e3f', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 8 },
        { serviceName: 'ores.compute.wrapper', instanceId: undefined, state: 'missing', version: undefined, lastHeartbeatSeconds: undefined },
        { serviceName: 'ores.dq.service', instanceId: '77c0e5a3-2f10-4d43-a6e7-0b1c2d3e4f50', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 11 },
        { serviceName: 'ores.http.server', instanceId: '0be2a911-3a21-4e54-b7f8-1c2d3e4f5061', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 3 },
        { serviceName: 'ores.iam.service', instanceId: '91b0f33d-4b32-4f65-8809-2d3e4f506172', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 12 },
        { serviceName: 'ores.marketdata.service', instanceId: 'e14d9077-5c43-4a76-991a-3e4f50617283', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 8 },
        { serviceName: 'ores.ore.service', instanceId: '3c7b12d5-6d54-4b87-8a2b-4f5061728394', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 14 },
        { serviceName: 'ores.refdata.service', instanceId: 'b6e0a4c8-7e65-4c98-9b3c-5061728394a5', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 7 },
        { serviceName: 'ores.reporting.service', instanceId: 'c9a3f10b-8f76-4da9-8c4d-61728394a5b6', state: 'stopped', version: undefined, lastHeartbeatSeconds: undefined },
        { serviceName: 'ores.scheduler.service', instanceId: '8f4a1139-9087-4eba-9d5e-728394a5b6c7', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 10 },
        { serviceName: 'ores.storage.service', instanceId: '2d9710fe-a198-4fcb-8e6f-8394a5b6c7d8', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 13 },
        { serviceName: 'ores.synthetic.service', instanceId: '51ba6cc3-b2a9-40dc-9f70-94a5b6c7d8e9', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 16 },
        { serviceName: 'ores.telemetry.service', instanceId: '2a9977f6-c3ba-41ed-8071-a5b6c7d8e9f0', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 2 },
        { serviceName: 'ores.trading.service', instanceId: 'd4c8b210-d4cb-42fe-9182-b6c7d8e9f0a1', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 15 },
        { serviceName: 'ores.variability.service', instanceId: '9e07ab45-e5dc-430f-8293-c7d8e9f0a1b2', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 6 },
        { serviceName: 'ores.web.service', instanceId: '4021d7aa-f6ed-4410-93a4-d8e9f0a1b2c3', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 1 },
        { serviceName: 'ores.workflow.service', instanceId: '66d5e2f1-a7fe-4521-84b5-e9f0a1b2c3d4', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 12 },
        { serviceName: 'ores.workspace.service', instanceId: 'c13f8b96-b80f-4632-95c6-f0a1b2c3d4e5', state: 'running', version: 'v0.0.25', lastHeartbeatSeconds: 9 }
    ];

    var computeWrapperServiceName = 'ores.compute.wrapper';

    var GAPS = [
        {
            title: 'The expected services are not a read',
            body: 'The registry that states which services exist and how many replicas each expects is a codegen model, projects/modeling/service_registry.org; no operation serves it. Without it the screen can only be drawn from the samples, and a service that stops leaves the list when five minutes pass.'
        },
        {
            title: 'The state comes from absence, not from a read',
            body: 'Running here means "reported in the last five minutes"; stopped and missing come from the installation\'s service manager, the one compass services status asks. Nothing serves that state to a screen, so a reader cannot tell a service somebody stopped from one that fell over.'
        },
        {
            title: 'The version is the release only',
            body: 'The heartbeat carries the release string, not the full build string, so two builds of one release read the same and the older-build label can only compare releases.'
        },
        {
            title: 'The reply is unordered',
            body: 'telemetry.v1.services.list orders nothing; this screen sorts by service name, then instance. A stable order from the read \u2014 service, then instance \u2014 would settle it once.'
        },
        {
            title: 'No uptime',
            body: 'Nothing says when an instance started, so an instance that just restarted reads like one that has run for weeks.'
        },
        {
            title: 'No permission gates the read',
            body: 'The handler authenticates the caller and checks nothing else; any signed-in person can read every instance.'
        }
    ];

    /* `state` is the point in the read: rest is the screen as first drawn,
       refreshed is after the refresh control was pressed. */
    var S = { variant: 'reporting', state: 'rest', readAt: '14:32:05', showState: true };
    var log = [];

    function variantById(id) {
        return VARIANTS.filter(function (v) { return v.id === id; })[0];
    }

    function refresh() {
        S.readAt = '14:33:02';
        S.state = 'refreshed';
        log.push('refresh \u00b7 re-read every instance at 14:33:02');
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

    /* The fixture the variant reads: the roster as sampled, or every expected
       instance reported missing because no sample arrived. */
    function instances() {
        var reported = serviceInstances.filter(function (instance) {
            return instance.serviceName !== computeWrapperServiceName;
        });
        if (S.variant === 'reporting') return reported;
        return reported.map(function (instance) {
            return {
                serviceName: instance.serviceName,
                instanceId: undefined,
                state: 'missing',
                version: undefined,
                lastHeartbeatSeconds: undefined
            };
        });
    }

    function groupByService(rows) {
        var made = {};
        var order = [];
        rows.forEach(function (instance) {
            if (!made[instance.serviceName]) {
                made[instance.serviceName] = [];
                order.push(instance.serviceName);
            }
            made[instance.serviceName].push(instance);
        });
        order.sort(function (left, right) { return left.localeCompare(right); });
        return order.map(function (serviceName) {
            return { serviceName: serviceName, instances: made[serviceName] };
        });
    }

    function newestVersionOf(running) {
        var newest;
        running.forEach(function (instance) {
            if (instance.version === undefined) return;
            if (newest === undefined || instance.version > newest) newest = instance.version;
        });
        return newest;
    }

    function asMinutes(seconds) {
        if (seconds < 60) return String(seconds) + ' s';
        var minutes = Math.floor(seconds / 60);
        if (minutes < 60) return String(minutes) + ' m ' + String(seconds % 60) + ' s';
        return String(Math.floor(minutes / 60)) + ' h ' + String(minutes % 60) + ' m';
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

    function instanceCount(reported, expected) {
        if (reported < expected) {
            return '<span class="tag warn">' + reported + ' of ' + expected + '</span>';
        }
        return '<span class="mono dim">' + reported + ' of ' + expected + '</span>';
    }

    function hasSkew(running, newestVersion) {
        return running.some(function (instance) { return instance.version !== newestVersion; });
    }

    function skewSummary(running, newestVersion) {
        var behind = [];
        var versions = [];
        running.forEach(function (instance) {
            if (instance.version === newestVersion) return;
            if (behind.indexOf(instance.serviceName) < 0) behind.push(instance.serviceName);
            var version = instance.version === undefined ? '' : instance.version;
            if (versions.indexOf(version) < 0) versions.push(version);
        });
        versions.sort();
        return behind.join(', ') + ' runs ' + versions.join(', ') + ' while the rest run ' + newestVersion;
    }

    function instancesPanel() {
        var rows = instances();
        var groups = groupByService(rows);
        var running = rows.filter(function (instance) { return instance.state === 'running'; });
        var stopped = rows.filter(function (instance) { return instance.state === 'stopped'; });
        var missing = rows.filter(function (instance) { return instance.state === 'missing'; });
        var newestVersion = newestVersionOf(running);

        var body = groups.map(function (group) {
            var reported = group.instances.filter(function (row) { return row.state === 'running'; }).length;
            return group.instances.map(function (instance, index) {
                return '<tr class="groupline">' +
                    (index === 0
                        ? '<td rowspan="' + group.instances.length + '"><span class="mono">' + esc(group.serviceName) + '</span></td>' +
                          '<td rowspan="' + group.instances.length + '">' + instanceCount(reported, group.instances.length) + '</td>'
                        : '') +
                    '<td class="mono" title="' + esc(instance.instanceId) + '">' +
                        (instance.instanceId === undefined ? '\u2014' : esc(instance.instanceId.slice(0, 8))) + '</td>' +
                    '<td>' + instanceStateTag(instance.state) + '</td>' +
                    '<td>' + instanceVersion(instance, newestVersion) + '</td>' +
                    '<td class="mono">' +
                        (instance.lastHeartbeatSeconds === undefined ? '\u2014'
                            : esc(asMinutes(instance.lastHeartbeatSeconds)) + ' ago') + '</td>' +
                    '</tr>';
            }).join('');
        }).join('');

        return '<section class="card">' +
            '<header class="sectionhead"><h2>Instances</h2>' +
            '<div class="meta"><span>' + running.length + ' of ' + rows.length +
            ' instances reported in the last five minutes</span>' +
            (stopped.length > 0 ? '<span class="tag muted">' + stopped.length + ' stopped</span>' : '') +
            (missing.length > 0 ? '<span class="tag warn">' + missing.length + ' missing</span>' : '') +
            '</div></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Service</th><th>Instances</th><th>Instance</th><th>Status</th><th>Version</th><th>Last heartbeat</th>' +
            '</tr></thead><tbody>' + body + '</tbody></table></div>' +
            '<p class="rowfoot">Read-only. One row per expected instance, whether it reports or not. ' +
            'The instance id is a UUID the heartbeat publisher generates at startup; the column shows its first eight characters.</p>' +
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
            '<h1>Operations: services</h1>' +
            '<p class="lede">Every service the registry expects, met by the instances that report. ' +
            'The compute wrappers are the grid screen\u2019s.</p>' +
            '</div><div class="actions">' +
            '<span class="updated">Updated ' + esc(S.readAt) + '</span>' +
            '<button class="btn" data-act="refresh">Refresh</button>' +
            '<a class="btn linklike" href="../index.html">Back to prototypes</a>' +
            '</div></header>';
    }

    function render() {
        var rows = instances();
        var running = rows.filter(function (instance) { return instance.state === 'running'; });
        var newestVersion = newestVersionOf(running);
        var skew = S.variant === 'reporting' && newestVersion !== undefined &&
            hasSkew(running, newestVersion);

        var body = pageHead() +
            '<div class="notice warn">PROTOTYPE. The counts, states and versions below are fixtures: ' +
            'the roster comes from the service registry and the state from the installation\u2019s service manager, ' +
            'and no operation serves either yet. Nothing on this page reads the server.</div>' +
            (skew ? '<div class="notice warn">Version skew: ' + esc(skewSummary(running, newestVersion)) +
                '. After a rollout, an instance that did not take the build is what this line is for.</div>' : '') +
            instancesPanel() +
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

        renderBar(running, rows, newestVersion);
    }

    function renderBar(running, rows, newestVersion) {
        var variantButtons = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + v.id + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.name) + '</button>';
        }).join('');
        var active = variant();
        var state = '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>fixture: <b>' + esc(active.id) + '</b></span>' +
            '<span>updated: <b>' + esc(S.readAt) + '</b></span>' +
            '<span>instances reporting: <b>' + running.length + ' of ' + rows.length + '</b></span>' +
            '<span>newest release: <b>' + esc(newestVersion === undefined ? '\u2014' : newestVersion) + '</b></span>' +
            '</div>' +
            '<p class="state-note">Signed in as system administrator, on the system tenant. The roster is ' +
            'the registry\u2019s, less the compute wrappers the grid screen owns; the samples are ' +
            'telemetry.v1.services.list.</p>' +
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
