/* Operations: message bus prototype. Self-contained: plain JavaScript, mock
 * data, no framework, no build step, and nothing that outlives the page.
 *
 * Watch the message bus, from
 * doc/knowledge/journeys/operations/journey_watch_the_message_bus.org.
 *
 * Two variants and two states. The rows are fixtures shaped by the NATS server
 * and stream sample replies: server counters that run since the server started,
 * and one row per stream per sample. Nothing here reads the server. */

(function () {
    'use strict';

    var VARIANTS = [
        {
            id: 'sampled',
            name: 'Sampled',
            gist: 'The server sample every 30 seconds gives the vitals, the streams and one hour of counters for the trend.'
        },
        {
            id: 'empty',
            name: 'Empty range',
            gist: 'The range holds no samples; the screen points at the poller before anything else.'
        }
    ];

    var STATES = [
        ['rest', 'At rest'],
        ['applied', 'Range applied']
    ];

    var RANGES = [
        ['15m', 'Last 15 minutes'],
        ['1h', 'Last hour'],
        ['6h', 'Last 6 hours']
    ];

    var GAPS = [
        {
            title: 'Nothing lists the streams',
            body: 'A stream appears in the table only once a sample carries its name; a stream with no traffic is invisible.'
        },
        {
            title: 'A range longer than the limit truncates in silence',
            body: 'The read takes at most 1000 samples and the reply carries no total, so a busy range quietly loses its oldest samples.'
        },
        {
            title: 'The counters run since the server started',
            body: 'Messages and bytes are running totals; the change over the range is computed on the screen because no operation sends a rate.'
        },
        {
            title: 'A slow consumer cannot be named',
            body: 'The slow-consumer count is a number; no operation says which consumer fell behind.'
        },
        {
            title: 'No permission gates the read',
            body: 'The handler authenticates the caller and checks nothing else.'
        }
    ];

    /* telemetry.v1.nats_server_samples.list: the counters run since the server
       started. */
    var natsServerSamples = [
        { sampledAt: '14:01:00', inMsgs: 1228110, outMsgs: 3392011, inBytes: 208666624, outBytes: 1135515648, connections: 21, memBytes: 84934656, slowConsumers: 0 },
        { sampledAt: '14:11:00', inMsgs: 1234002, outMsgs: 3398144, inBytes: 209715200, outBytes: 1140228096, connections: 22, memBytes: 85983232, slowConsumers: 0 },
        { sampledAt: '14:21:00', inMsgs: 1239884, outMsgs: 3404447, inBytes: 210763776, outBytes: 1145290752, connections: 22, memBytes: 85983232, slowConsumers: 0 },
        { sampledAt: '14:31:45', inMsgs: 1240512, outMsgs: 3410882, inBytes: 220200960, outBytes: 1181167616, connections: 23, memBytes: 88080384, slowConsumers: 0 }
    ];

    /* telemetry.v1.nats_stream_samples.list: one row per stream per sample. */
    var natsStreamSamples = [
        { streamName: 'ORES_TRADES', messages: 12004, bytes: 88080384, consumerCount: 2 },
        { streamName: 'ORES_RESULTS', messages: 3201, bytes: 20971520, consumerCount: 1 }
    ];

    var S = { variant: 'sampled', state: 'rest', range: '1h', appliedRange: 'Last hour', log: [], panel: true };

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function num(n) {
        return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    }

    function asMiB(bytes) {
        return Math.round(bytes / 1024 / 1024) + ' MB';
    }

    function variantById(id) {
        return VARIANTS.filter(function (v) { return v.id === id; })[0];
    }

    function rangeLabel(id) {
        var found = RANGES.filter(function (r) { return r[0] === id; })[0];
        return found ? found[1] : null;
    }

    function activeVariant() { return variantById(S.variant) || VARIANTS[0]; }

    function applyRange() {
        S.appliedRange = rangeLabel(S.range) || 'Last hour';
        S.state = 'applied';
        S.log.push('apply \u00b7 re-read the range "' + S.appliedRange + '" at 14:32:11');
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (variantById(p.get('variant'))) S.variant = p.get('variant');
        if (p.get('range') && rangeLabel(p.get('range'))) S.range = p.get('range');
        S.appliedRange = rangeLabel(S.range) || 'Last hour';
        if (p.get('state') === 'applied') applyRange();
        else if (p.get('state') === 'rest') S.state = 'rest';
    }

    function writeParams() {
        try {
            var p = new URLSearchParams();
            p.set('variant', S.variant);
            p.set('state', S.state);
            p.set('range', S.range);
            window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
        } catch (err) {
            /* A page opened from the file system may refuse to rewrite its
               address; the screen still works without it. */
        }
    }

    // ------------------------------------------------------------- parts

    function detail(label, value, mono) {
        return '<div class="detail"><span class="k">' + esc(label) + '</span>' +
            '<span class="v' + (mono ? ' mono' : '') + '">' + value + '</span></div>';
    }

    function tag(text, tone) {
        return '<span class="tag ' + tone + '">' + esc(text) + '</span>';
    }

    function header() {
        var options = RANGES.map(function (r) {
            return '<option value="' + r[0] + '"' + (S.range === r[0] ? ' selected' : '') + '>' + esc(r[1]) + '</option>';
        }).join('');
        return '<header class="pageheader"><div>' +
            '<h1>Operations: message bus</h1>' +
            '<p class="sub">The NATS server\u2019s vitals and one row per stream.</p>' +
            '</div><div class="head-actions">' +
            '<label class="field"><span class="flabel">Range</span>' +
            '<select data-act="range">' + options + '</select></label>' +
            '<button class="btn secondary" data-act="apply">Apply</button>' +
            '<a class="btn secondary" href="../index.html">Back to prototypes</a>' +
            '</div></header>';
    }

    function prototypeNotice() {
        return '<div class="notice warn">PROTOTYPE. Every row below is a fixture shaped by the NATS ' +
            'server and stream sample replies. Nothing on this page reads the server.</div>';
    }

    function vitalsPanel() {
        var newest = natsServerSamples[natsServerSamples.length - 1];
        var oldest = natsServerSamples[0];
        if (newest === undefined || oldest === undefined) return '';
        return '<section class="card">' +
            '<header><h2>NATS server</h2><span class="meta">newest sample ' + esc(newest.sampledAt) + '</span></header>' +
            '<div class="details c4">' +
            detail('Connections', esc(String(newest.connections)), true) +
            detail('Memory', esc(asMiB(newest.memBytes)), true) +
            '<div class="detail"><span class="k">Slow consumers</span><span class="v">' +
            tag(String(newest.slowConsumers), newest.slowConsumers > 0 ? 'warn' : '') + '</span></div>' +
            detail('Totals since start', esc(String(num(newest.inMsgs))) + ' in \u00b7 ' +
                esc(String(num(newest.outMsgs))) + ' out', true) +
            '</div>' +
            '<p class="note">Over ' + esc(oldest.sampledAt) + '\u2013' + esc(newest.sampledAt) + ': +' +
            num(newest.inMsgs - oldest.inMsgs) + ' messages in, +' +
            num(newest.outMsgs - oldest.outMsgs) + ' out, ' +
            asMiB(newest.inBytes - oldest.inBytes) + ' in and ' +
            asMiB(newest.outBytes - oldest.outBytes) + ' out. The totals themselves are running ' +
            'totals since the NATS server started.</p>' +
            '</section>';
    }

    function streamsPanel(empty) {
        var body;
        if (empty) {
            body = '<p class="empty">No stream sample in the range. The table draws a stream once a ' +
                'sample names it, so nothing is listed here either.</p>';
        } else {
            body = '<div class="table-wrap"><table class="data"><thead><tr>' +
                '<th>Stream</th><th>Messages stored</th><th>Bytes stored</th><th>Consumers</th>' +
                '</tr></thead><tbody>' +
                natsStreamSamples.map(function (row) {
                    return '<tr>' +
                        '<td class="mono">' + esc(row.streamName) + '</td>' +
                        '<td class="mono">' + num(row.messages) + '</td>' +
                        '<td class="mono">' + esc(asMiB(row.bytes)) + '</td>' +
                        '<td class="mono">' + row.consumerCount + '</td>' +
                        '</tr>';
                }).join('') +
                '</tbody></table></div>';
        }
        return '<section class="card">' +
            '<header><h2>Streams</h2><span class="meta">' +
            (empty ? 'no samples in the range' : natsStreamSamples.length + ' streams') +
            '</span></header>' + body + '</section>';
    }

    function trendPanel() {
        var points = sparkPoints(natsServerSamples.map(function (s) { return s.inMsgs; }), 640, 80);
        return '<section class="card">' +
            '<header><h2>Trend</h2><span class="meta">messages in, over the range</span></header>' +
            '<svg viewBox="0 0 640 80" class="spark" role="img">' +
            '<title>Messages in over the range, from the sample series.</title>' +
            '<polyline points="' + points + '" fill="none" stroke="currentColor" stroke-width="2"></polyline>' +
            '</svg>' +
            '<p class="note">Drawn from the samples the range returns. The screen computes the movement ' +
            'itself: the counters are running totals, and no operation sends a rate.</p>' +
            '</section>';
    }

    function gapPanel() {
        return '<section class="card">' +
            '<header><h2>Not on this screen yet</h2>' +
            '<span class="meta">each gap names the journey that records it</span></header>' +
            '<dl class="gaps">' + GAPS.map(function (gap) {
                return '<div class="gap"><dt>' + esc(gap.title) + '</dt><dd>' + esc(gap.body) + '</dd></div>';
            }).join('') + '</dl></section>';
    }

    function sparkPoints(values, width, height) {
        if (values.length === 0) return '';
        var lowest = Math.min.apply(null, values);
        var highest = Math.max.apply(null, values);
        var span = highest - lowest === 0 ? 1 : highest - lowest;
        return values.map(function (value, index) {
            var x = values.length === 1 ? width / 2 : (index / (values.length - 1)) * width;
            var y = height - ((value - lowest) / span) * (height - 8) - 4;
            return x.toFixed(1) + ',' + y.toFixed(1);
        }).join(' ');
    }

    function body() {
        if (S.variant === 'empty') {
            return '<section class="card">' +
                '<h2>NATS server</h2>' +
                '<div class="notice warn">The range holds no samples. The telemetry service takes one ' +
                'sample every 30 seconds; an empty range points at its poller first.</div>' +
                '</section>' +
                streamsPanel(true) +
                gapPanel();
        }
        return vitalsPanel() + streamsPanel(false) + trendPanel() + gapPanel();
    }

    // -------------------------------------------------------------- page

    function render() {
        document.getElementById('app').innerHTML =
            '<div class="page">' + header() + prototypeNotice() + body() + '</div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 variant ' + S.variant +
            ' \u00b7 state ' + S.state + ' \u00b7 signed in as system administrator, on the system tenant';

        renderState();
        renderBar();
    }

    function renderState() {
        var samples = S.variant === 'sampled' ? String(natsServerSamples.length) : '0';
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
            '<span>range: <span class="v">' + esc(S.appliedRange) + '</span></span>' +
            '<span>samples asked for: <span class="v">' + samples + '</span></span>' +
            '</div>' +
            '<p class="note">Signed in as system administrator, on the system tenant.</p>' +
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
            if (el.getAttribute('data-state') === 'applied') applyRange();
            else { S.state = 'rest'; S.appliedRange = rangeLabel(S.range) || 'Last hour'; S.log = []; }
        } else if (act === 'apply') applyRange();
        else if (act === 'panel') S.panel = !S.panel;
        writeParams();
        render();
    });

    document.addEventListener('change', function (ev) {
        var el = ev.target.closest('[data-act="range"]');
        if (!el) return;
        S.range = el.value;
        if (S.state === 'rest') S.appliedRange = rangeLabel(S.range) || 'Last hour';
        writeParams();
        render();
    });

    readParams();
    render();
})();
