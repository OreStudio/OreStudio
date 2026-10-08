/* Operations: telemetry logs prototype. Self-contained: plain JavaScript, mock
 * data, no framework, no build step, and nothing that outlives the page.
 *
 * Read the telemetry logs, from
 * doc/knowledge/journeys/operations/journey_read_the_telemetry_logs.org.
 *
 * Two variants and two states. The rows are fixtures shaped by the
 * telemetry.v1.logs.list reply; the filters combine the way the query combines
 * them, with AND. Nothing here reads the server. */

(function () {
    'use strict';

    var VARIANTS = [
        {
            id: 'matches',
            name: 'Matches',
            gist: 'The last hour of server lines, filtered by the bar above the table.'
        },
        {
            id: 'nothing',
            name: 'Nothing matches',
            gist: 'The filter in the range returns no entry; the screen says which filter to drop.'
        }
    ];

    var STATES = [
        ['rest', 'At rest'],
        ['searched', 'Search run']
    ];

    var TOTAL_COUNT = 2431;

    var LEVELS = ['Any level', 'ERROR', 'WARN', 'INFO', 'DEBUG'];
    var SOURCES = ['Any source', 'server', 'client'];

    var GAPS = [
        {
            title: 'The store holds no client lines',
            body: 'The entry model has a client source and the ingest stamps every stored entry source=server; nothing publishes a client line. Choosing the client source returns nothing, forever.'
        },
        {
            title: 'The filters combine with AND only',
            body: 'Every filter narrows the same set; there is no way to ask for one component or another, and nothing suggests the values \u2014 component and tag are typed blind.'
        },
        {
            title: 'The message filter reaches the database as text',
            body: 'The match runs as SQL text behind a hand-written escape; the journey records the defect on capture BBA0A093.'
        },
        {
            title: 'The statistics have no subject',
            body: 'The hourly, daily and per-session aggregates are stored and no subject reads them, so the screen cannot draw a count over time.'
        },
        {
            title: 'No permission gates the read',
            body: 'The handler authenticates the caller and checks nothing else.'
        }
    ];

    /* telemetry.v1.logs.list: every stored entry is source server today. */
    var logEntries = [
        { id: 2431, time: '14:31:02.114', level: 'ERROR', source: 'server', sourceName: 'ores.compute.service', component: 'ores.compute.poller', message: 'fetch failed, retrying', tag: 'compute.fetch', sessionId: undefined },
        { id: 2430, time: '14:30:58.902', level: 'WARN', source: 'server', sourceName: 'ores.compute.service', component: 'ores.compute.poller', message: 'retrying fetch after timeout', tag: 'compute.fetch', sessionId: undefined },
        { id: 2429, time: '14:30:44.201', level: 'INFO', source: 'server', sourceName: 'ores.iam.service', component: 'ores.iam.auth', message: 'session opened', tag: 'iam.session', sessionId: 'e8f1a7c2' },
        { id: 2428, time: '14:30:41.550', level: 'DEBUG', source: 'server', sourceName: 'ores.telemetry.service', component: 'ores.telemetry.ingest', message: 'stored 18 service samples', tag: 'telemetry.ingest', sessionId: undefined },
        { id: 2427, time: '14:30:30.008', level: 'INFO', source: 'server', sourceName: 'ores.telemetry.service', component: 'ores.telemetry.service.app.nats_poller', message: 'sampled the NATS server', tag: 'nats.sample', sessionId: undefined },
        { id: 2426, time: '14:30:12.731', level: 'WARN', source: 'server', sourceName: 'ores.iam.service', component: 'ores.iam.auth', message: 'sign-in rejected: unknown account', tag: 'iam.signin', sessionId: undefined },
        { id: 2425, time: '14:29:59.440', level: 'INFO', source: 'server', sourceName: 'ores.reporting.service', component: 'ores.reporting.queue', message: 'batch queued for the grid', tag: 'reporting.queue', sessionId: 'c02d55b9' },
        { id: 2424, time: '14:29:47.020', level: 'ERROR', source: 'server', sourceName: 'ores.workflow.service', component: 'ores.workflow.engine', message: 'step timed out, instance paused', tag: 'workflow.step', sessionId: undefined }
    ];

    var S = {
        variant: 'matches',
        state: 'rest',
        readAt: '14:32:12',
        level: 'Any level',
        source: 'Any source',
        component: '',
        tag: '',
        message: '',
        applied: { component: '', tag: '', message: '' },
        log: [],
        panel: true
    };

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function variantById(id) {
        return VARIANTS.filter(function (v) { return v.id === id; })[0];
    }

    function activeVariant() { return variantById(S.variant) || VARIANTS[0]; }

    function search() {
        S.applied = { component: S.component, tag: S.tag, message: S.message };
        S.readAt = '14:33:08';
        S.state = 'searched';
        S.log.push('search \u00b7 level "' + S.level + '", source "' + S.source +
            '", component "' + S.component + '", tag "' + S.tag + '", message "' + S.message + '"');
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (variantById(p.get('variant'))) S.variant = p.get('variant');
        if (LEVELS.indexOf(p.get('level')) >= 0) S.level = p.get('level');
        if (SOURCES.indexOf(p.get('source')) >= 0) S.source = p.get('source');
        if (p.get('component') !== null) { S.component = p.get('component'); S.applied.component = p.get('component'); }
        if (p.get('tag') !== null) { S.tag = p.get('tag'); S.applied.tag = p.get('tag'); }
        if (p.get('message') !== null) { S.message = p.get('message'); S.applied.message = p.get('message'); }
        if (p.get('state') === 'searched') search();
        else if (p.get('state') === 'rest') S.state = 'rest';
    }

    function writeParams() {
        try {
            var p = new URLSearchParams();
            p.set('variant', S.variant);
            p.set('state', S.state);
            p.set('level', S.level);
            p.set('source', S.source);
            if (S.applied.component !== '') p.set('component', S.applied.component);
            if (S.applied.tag !== '') p.set('tag', S.applied.tag);
            if (S.applied.message !== '') p.set('message', S.applied.message);
            window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
        } catch (err) {
            /* A page opened from the file system may refuse to rewrite its
               address; the screen still works without it. */
        }
    }

    // ------------------------------------------------------------- parts

    function matchesLevel(entry) {
        return S.level === 'Any level' || entry.level === S.level;
    }

    function matchesSource(entry) {
        return S.source === 'Any source' || entry.source === S.source;
    }

    function includes(value, filter) {
        return filter === '' || value.indexOf(filter) >= 0;
    }

    function rows() {
        if (S.variant === 'nothing') return [];
        return logEntries.filter(function (entry) {
            return matchesLevel(entry) && matchesSource(entry) &&
                includes(entry.component, S.applied.component) &&
                includes(entry.tag, S.applied.tag) &&
                includes(entry.message, S.applied.message);
        });
    }

    function header() {
        return '<header class="pageheader"><div>' +
            '<h1>Operations: telemetry logs</h1>' +
            '<p class="sub">The lines behind a symptom, found by time, level, source, component, tag or session.</p>' +
            '</div><div class="head-actions">' +
            '<span class="meta">Read at ' + esc(S.readAt) + '</span>' +
            '<a class="btn secondary" href="../index.html">Back to prototypes</a>' +
            '</div></header>';
    }

    function filterBar() {
        function select(label, options, value, act, grow) {
            return '<label class="field' + (grow ? ' grow' : '') + '"><span class="flabel">' + esc(label) + '</span>' +
                '<select data-act="' + act + '">' + options.map(function (option) {
                    return '<option value="' + esc(option) + '"' + (option === value ? ' selected' : '') + '>' +
                        esc(option) + '</option>';
                }).join('') + '</select></label>';
        }
        function input(label, placeholder, value, act, grow) {
            return '<label class="field' + (grow ? ' grow' : '') + '"><span class="flabel">' + esc(label) + '</span>' +
                '<input data-act="' + act + '" value="' + esc(value) + '" placeholder="' + esc(placeholder) + '"></label>';
        }
        return '<section class="card filters">' +
            select('Range', ['Last hour'], 'Last hour', 'range', false) +
            select('Level', LEVELS, S.level, 'level', false) +
            select('Source', SOURCES, S.source, 'source', false) +
            input('Component', 'ores.compute.poller', S.component, 'component', false) +
            input('Tag', 'compute.fetch', S.tag, 'tag', false) +
            input('Message', 'Search the message', S.message, 'message', true) +
            '<button class="btn secondary" data-act="search">Search</button>' +
            '</section>';
    }

    function prototypeNotice() {
        return '<div class="notice warn">PROTOTYPE. Every row below is a fixture shaped by the ' +
            'telemetry.v1.logs.list reply. Nothing on this page reads the server. The filters ' +
            'combine with AND, as the query does.</div>';
    }

    function levelTag(level) {
        var tone = level === 'ERROR' || level === 'WARN' ? 'warn'
            : level === 'DEBUG' ? 'muted' : '';
        return '<span class="tag ' + tone + '">' + esc(level) + '</span>';
    }

    function entriesPanel(list) {
        return '<section class="card">' +
            '<header><h2>Entries</h2><span class="meta">' + list.length + ' of ' + TOTAL_COUNT + '</span></header>' +
            '<div class="table-wrap"><table class="data"><thead><tr>' +
            '<th>Time</th><th>Level</th><th>Source</th><th>Name</th><th>Component</th><th>Message</th>' +
            '</tr></thead><tbody>' +
            list.map(function (entry) {
                return '<tr>' +
                    '<td class="mono">' + esc(entry.time) + '</td>' +
                    '<td>' + levelTag(entry.level) + '</td>' +
                    '<td class="mono">' + esc(entry.source) + '</td>' +
                    '<td class="mono">' + esc(entry.sourceName) + '</td>' +
                    '<td class="mono">' + esc(entry.component) + '</td>' +
                    '<td>' + esc(entry.message) + '</td>' +
                    '</tr>';
            }).join('') +
            '</tbody></table></div>' +
            '<div class="paging">' +
            '<span class="note">Showing 1\u2013' + list.length + ' of ' + TOTAL_COUNT + ' entries</span>' +
            '<button class="btn secondary small" disabled>Previous</button>' +
            '<button class="btn secondary small" disabled>Next</button>' +
            '<span class="note">The reply carries limit and offset; the fixture holds one page.</span>' +
            '</div></section>';
    }

    function emptyPanel() {
        return '<section class="card">' +
            '<header><h2>Entries</h2><span class="tag warn">Nothing matches</span></header>' +
            '<p class="empty">No entry matches the filter in this range. Widen the range or drop a filter.</p>' +
            '<p class="note">' + (S.source === 'client'
                ? 'No entry has the source client: the store holds server lines today.'
                : 'Filters combine with AND; the store holds server lines today, so no entry has the source client.') +
            '</p></section>';
    }

    function gapPanel() {
        return '<section class="card">' +
            '<header><h2>Not on this screen yet</h2>' +
            '<span class="meta">each gap names the journey that records it</span></header>' +
            '<dl class="gaps">' + GAPS.map(function (gap) {
                return '<div class="gap"><dt>' + esc(gap.title) + '</dt><dd>' + esc(gap.body) + '</dd></div>';
            }).join('') + '</dl></section>';
    }

    function body() {
        var list = rows();
        return (list.length === 0 ? emptyPanel() : entriesPanel(list)) + gapPanel();
    }

    // -------------------------------------------------------------- page

    function render() {
        document.getElementById('app').innerHTML =
            '<div class="page">' + header() + filterBar() + prototypeNotice() + body() + '</div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 variant ' + S.variant +
            ' \u00b7 state ' + S.state + ' \u00b7 signed in as system administrator, tenant Acme Corporation';

        renderState();
        renderBar();
    }

    function renderState() {
        var active = activeVariant();
        var log = S.log.length === 0
            ? '<p class="note">No action yet.</p>'
            : '<ol>' + S.log.map(function (entry, index) {
                return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
            }).join('') + '</ol>';
        document.getElementById('proto-state').hidden = !S.panel;
        document.getElementById('proto-state').innerHTML =
            '<p class="gist">Variant <b>' + esc(active.name) + '</b> \u2014 ' + esc(active.gist) + '</p>' +
            '<div class="grid">' +
            '<span>fixture: <span class="v">' + esc(S.variant) + '</span></span>' +
            '<span>level: <span class="v">' + esc(S.level) + '</span></span>' +
            '<span>source: <span class="v">' + esc(S.source) + '</span></span>' +
            '<span>component: <span class="v">' + (S.applied.component === '' ? '\u2014' : esc(S.applied.component)) + '</span></span>' +
            '<span>tag: <span class="v">' + (S.applied.tag === '' ? '\u2014' : esc(S.applied.tag)) + '</span></span>' +
            '<span>message: <span class="v">' + (S.applied.message === '' ? '\u2014' : esc(S.applied.message)) + '</span></span>' +
            '</div>' +
            '<p class="note">Signed in as system administrator, tenant Acme Corporation. The read ' +
            'takes limit and offset; the fixture holds one page.</p>' +
            log;
    }

    function renderBar() {
        var variantButtons = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + v.id + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.name) + '</button>';
        }).join('');
        var stateButtons = STATES.map(function (s) {
            return '<button data-act="state" data-state="' + s[0] + '"' +
                (S.state === s[0] ? ' class="on"' : '') + '>' + esc(s[1]) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">variant <b>' + S.variant + '</b></span>' + variantButtons +
            '<span class="sep">|</span><span class="label">state</span>' + stateButtons +
            '<span class="sep">|</span>' +
            '<button data-act="panel">' + (S.panel ? 'Hide state' : 'Show state') + '</button>';
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'range') { ev.preventDefault(); return; }
        ev.preventDefault();
        if (act === 'variant') S.variant = el.getAttribute('data-variant');
        else if (act === 'state') {
            if (el.getAttribute('data-state') === 'searched') search();
            else {
                S.state = 'rest';
                S.readAt = '14:32:12';
                S.applied = { component: '', tag: '', message: '' };
                S.component = '';
                S.tag = '';
                S.message = '';
                S.log = [];
            }
        } else if (act === 'search') search();
        else if (act === 'panel') S.panel = !S.panel;
        writeParams();
        render();
    });

    document.addEventListener('change', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'level') S.level = el.value;
        else if (act === 'source') S.source = el.value;
        else if (act === 'range') return;
        else return;
        writeParams();
        render();
    });

    document.addEventListener('input', function (ev) {
        var el = ev.target.closest('[data-act="component"], [data-act="tag"], [data-act="message"]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'component') S.component = el.value;
        else if (act === 'tag') S.tag = el.value;
        else if (act === 'message') S.message = el.value;
    });

    readParams();
    render();
})();
