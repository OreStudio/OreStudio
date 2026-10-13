/* Compute: browse the grid's jobs. Self-contained: plain JavaScript, mock
 * data, no framework, no build step.
 *
 * From doc/knowledge/journeys/compute/journey_browse_the_grids_jobs.org.
 * The fixtures are shaped by the compute.v1 batch, workunit and result
 * reads. Nothing here reads the server. */

(function () {
    'use strict';

    var ROLES = {
        operator: { name: 'Operator', gist: 'Sees every tenant and party, and the node of each run.', parties: true, nodes: true },
        tenant: { name: 'Tenant admin', gist: 'Sees every party of the tenant. No node names.', parties: true, nodes: false },
        member: { name: 'Party member', gist: 'Sees only the party they belong to. No node names.', parties: false, nodes: false }
    };
    var STATES = ['loaded', 'loading', 'empty', 'error', 'refused'];
    var PAGE_SIZES = [10, 25, 50];
    var PARTIES = ['ACCOUK', 'ACCOUS', 'ACCOHK', 'ACMCOR'];
    var NODES = ['grid-01.example.com', 'grid-02.example.com', 'grid-04.example.com'];

    function pad(n) { return n < 10 ? '0' + n : '' + n; }

    var batches = [];
    for (var i = 0; i < 37; i++) {
        var party = PARTIES[i % 4 === 3 ? (i % 8 === 3 ? 3 : 1) : i % 3 === 0 ? 0 : i % 3];
        var failed = i % 5 === 0 ? 1 + (i % 3) : 0;
        batches.push({
            id: 'b' + i,
            ref: 'batch_2026_10_' + pad(12 - Math.floor(i / 6)) + '_' + String.fromCharCode(97 + (i % 6)),
            party: party,
            state: i < 3 ? 'open' : 'closed',
            jobs: 8 + (i * 7) % 33,
            failed: i < 3 ? 0 : failed
        });
    }

    function workunitsOf(b) {
        var out = [];
        for (var w = 0; w < b.jobs; w++) {
            out.push({
                id: b.id + 'w' + w,
                label: 'wu ' + pad(w + 1).replace(/^/, '00'),
                priority: 5,
                redundancy: w % 4 === 0 ? 2 : 1,
                input: 's3://ores-inputs/' + b.ref + '/job_' + pad(w + 1) + '.tar.zst',
                config: 'inline',
                bad: w < b.failed
            });
        }
        return out;
    }

    function resultsOf(b, wu) {
        var out = [];
        for (var r = 0; r < wu.redundancy + (wu.bad ? 1 : 0); r++) {
            var failedRun = wu.bad && r === wu.redundancy;
            var inFlight = b.state === 'open' && r === 0;
            out.push({
                id: wu.id + 'r' + r,
                label: 'result ' + (r + 1),
                state: inFlight ? 'running' : failedRun ? 'failed' : 'succeeded',
                outcome: inFlight ? '' : failedRun ? 'client error' : 'success',
                error: failedRun ? 'engine exited with code 3: market data snapshot not found' : '',
                node: inFlight ? '' : NODES[(r + 1) % 3],
                received: inFlight ? '' : '14:' + pad(2 + r) + ':' + pad(10 + r),
                output: failedRun ? '' : 's3://ores-outputs/' + b.ref + '/' + wu.id + '_' + (r + 1) + '.parquet'
            });
        }
        return out;
    }

    var q = new URLSearchParams(location.search);
    var role = ROLES[q.get('role')] ? q.get('role') : 'operator';
    var screen = STATES.indexOf(q.get('state')) >= 0 ? q.get('state') : 'loaded';
    var ui = { text: '', state: '', party: '', page: 0, size: 10, open: {}, selected: null, wuPage: {}, readOnly: true };

    function el(tag, cls, text) {
        var e = document.createElement(tag);
        if (cls) { e.className = cls; }
        if (text !== undefined) { e.textContent = text; }
        return e;
    }

    function badge(text) {
        var cls = { succeeded: 'ok', closed: 'ok', failed: 'bad', running: 'run', open: 'queue' }[text] || 'queue';
        return el('span', 'badge b-' + cls, text);
    }

    function filtered() {
        return batches.filter(function (b) {
            return (!ui.text || b.ref.indexOf(ui.text) >= 0 || b.id === ui.text) &&
                (!ui.state || b.state === ui.state) &&
                (role === 'member' ? b.party === 'ACCOUK' : !ui.party || b.party === ui.party);
        });
    }

    function pager(total, page, size, onPage, onSize, inner) {
        var bar = el('div', 'pager' + (inner ? ' inner' : ''));
        var last = Math.max(0, Math.ceil(total / size) - 1);
        [['First', 0], ['Previous', page - 1], ['Next', page + 1], ['Last', last]].forEach(function (p) {
            var b = el('button', 'btn', p[0]);
            b.disabled = p[1] < 0 || p[1] > last || p[1] === page;
            b.onclick = function () { onPage(p[1]); };
            bar.appendChild(b);
        });
        bar.appendChild(el('span', '', 'Page ' + (page + 1) + ' of ' + (last + 1) + ', ' + total + ' in all'));
        var sel = el('select');
        PAGE_SIZES.forEach(function (n) {
            var o = el('option', '', String(n));
            o.selected = n === size;
            sel.appendChild(o);
        });
        sel.onchange = function () { onSize(parseInt(sel.value, 10)); };
        bar.appendChild(sel);
        if (total <= 1000) {
            var all = el('button', 'btn', 'Load all');
            all.onclick = function () { onSize(total || 1); };
            bar.appendChild(all);
        }
        return bar;
    }

    function row(cls, label, state, count, id, twist) {
        var r = el('li', 'row' + (ui.selected === id ? ' sel' : ''));
        var n = el('span', 'name');
        n.appendChild(el('span', 'twist', twist || ''));
        n.appendChild(document.createTextNode(label));
        r.appendChild(n);
        r.appendChild(el('span', 'count', count || ''));
        r.appendChild(state ? badge(state) : el('span'));
        r.dataset.id = id;
        return r;
    }

    function tree() {
        var list = filtered();
        var panel = el('div', 'panel');
        panel.appendChild(el('h2', '', 'Batches, workunits and results'));
        var ul = el('ul', 'tree');
        var start = ui.page * ui.size;
        list.slice(start, start + ui.size).forEach(function (b) {
            var open = !!ui.open[b.id];
            var r = row('', b.ref + (role === 'member' ? '' : '  · ' + b.party), b.state,
                b.jobs + ' jobs, ' + b.failed + ' failed', b.id, open ? '▾' : '▸');
            r.onclick = function () { ui.open[b.id] = !open; ui.selected = b.id; render(); };
            ul.appendChild(r);
            if (!open) { return; }
            var wus = workunitsOf(b);
            var wp = ui.wuPage[b.id] || 0;
            var wsize = 5;
            var l1 = el('ul', 'tree lvl1');
            wus.slice(wp * wsize, wp * wsize + wsize).forEach(function (wu) {
                var wopen = !!ui.open[wu.id];
                var wr = row('', wu.label, wu.bad ? 'failed' : 'succeeded',
                    wu.redundancy + (wu.redundancy > 1 ? ' runs' : ' run'), wu.id, wopen ? '▾' : '▸');
                wr.onclick = function (e) { e.stopPropagation(); ui.open[wu.id] = !wopen; ui.selected = wu.id; render(); };
                l1.appendChild(wr);
                if (wopen) {
                    var l2 = el('ul', 'tree lvl2');
                    resultsOf(b, wu).forEach(function (res) {
                        var rr = row('', res.label, res.state, '', res.id);
                        rr.onclick = function (e) { e.stopPropagation(); ui.selected = res.id; render(); };
                        l2.appendChild(rr);
                    });
                    l1.appendChild(l2);
                }
            });
            ul.appendChild(l1);
            ul.appendChild(pager(wus.length, wp, wsize, function (p) { ui.wuPage[b.id] = p; render(); }, function () {}, true));
        });
        panel.appendChild(ul);
        panel.appendChild(pager(list.length, ui.page, ui.size,
            function (p) { ui.page = p; render(); },
            function (s) { ui.size = s; ui.page = 0; render(); }));
        return panel;
    }

    function find(id) {
        for (var i = 0; i < batches.length; i++) {
            var b = batches[i];
            if (b.id === id) { return { kind: 'batch', b: b }; }
            var wus = workunitsOf(b);
            for (var j = 0; j < wus.length; j++) {
                if (wus[j].id === id) { return { kind: 'workunit', b: b, wu: wus[j] }; }
                var rs = resultsOf(b, wus[j]);
                for (var k = 0; k < rs.length; k++) {
                    if (rs[k].id === id) { return { kind: 'result', b: b, wu: wus[j], res: rs[k] }; }
                }
            }
        }
        return null;
    }

    function details() {
        var panel = el('div', 'panel');
        panel.appendChild(el('h2', '', 'Details'));
        var d = el('div', 'details');
        var hit = ui.selected && find(ui.selected);
        if (!hit) {
            d.appendChild(el('p', 'dim', 'Select a batch, a workunit or a result to read its details.'));
            panel.appendChild(d);
            return panel;
        }
        var head = el('div');
        head.appendChild(el('strong', '', hit.kind === 'batch' ? hit.b.ref : hit.kind === 'workunit' ? hit.wu.label : hit.res.label));
        head.appendChild(document.createTextNode(' '));
        head.appendChild(el('span', 'access', 'Read only'));
        d.appendChild(head);
        var dl = el('dl');
        function add(k, v, link) {
            dl.appendChild(el('dt', '', k));
            var dd = el('dd');
            if (link) { var a = el('a', '', v); a.href = '#'; a.onclick = function (e) { e.preventDefault(); }; dd.appendChild(a); }
            else { dd.textContent = v === '' ? '—' : v; }
            dl.appendChild(dd);
        }
        var dl2 = [];
        if (hit.kind === 'batch') {
            add('State', hit.b.state);
            if (ROLES[role].parties) { add('Party', hit.b.party, true); }
            add('Jobs', String(hit.b.jobs));
            add('Failed', String(hit.b.failed));
            add('Closed', hit.b.state === 'closed' ? 'closed does not mean every run succeeded' : 'still open');
        } else if (hit.kind === 'workunit') {
            add('Priority', String(hit.wu.priority));
            add('Target redundancy', String(hit.wu.redundancy));
            add('Input', hit.wu.input);
            add('Config', hit.wu.config);
            add('Batch', hit.b.ref, true);
        } else {
            add('State', hit.res.state);
            add('Outcome', hit.res.outcome);
            add('Node', ROLES[role].nodes ? (hit.res.node || 'not yet known') : 'hidden for your role', ROLES[role].nodes && !!hit.res.node);
            add('Received', hit.res.received);
            add('Error', hit.res.error);
            add('Output', hit.res.output || 'none');
        }
        d.appendChild(dl);
        var act = el('div', 'actions');
        var dlIn = el('button', 'btn', 'Download input');
        dlIn.disabled = hit.kind === 'batch';
        var dlOut = el('button', 'btn', 'Download output');
        dlOut.disabled = hit.kind !== 'result' || !hit.res.output;
        var job = el('button', 'btn', 'Open job');
        job.disabled = hit.kind === 'batch';
        [dlIn, dlOut, job].forEach(function (b) { act.appendChild(b); });
        d.appendChild(act);
        panel.appendChild(d);
        return panel;
    }

    function finder() {
        var f = el('div', 'finder');
        function field(label, node) { var l = el('label', '', label); l.appendChild(node); f.appendChild(l); }
        var t = el('input'); t.type = 'search'; t.placeholder = 'batch reference or id'; t.value = ui.text;
        t.onkeydown = function (e) { if (e.key === 'Enter') { ui.text = t.value.trim(); ui.page = 0; render(); } };
        field('Find', t);
        var s = el('select');
        ['', 'open', 'closed'].forEach(function (v) { var o = el('option', '', v || 'Any'); o.value = v; o.selected = v === ui.state; s.appendChild(o); });
        s.onchange = function () { ui.state = s.value; ui.page = 0; render(); };
        field('State', s);
        if (ROLES[role].parties) {
            var p = el('select');
            [''].concat(PARTIES).forEach(function (v) { var o = el('option', '', v || 'Any'); o.value = v; o.selected = v === ui.party; p.appendChild(o); });
            p.onchange = function () { ui.party = p.value; ui.page = 0; render(); };
            field('Party', p);
        }
        var go = el('button', 'btn', 'Search');
        go.onclick = function () { ui.text = t.value.trim(); ui.page = 0; render(); };
        var clr = el('button', 'btn', 'Clear');
        clr.onclick = function () { ui.text = ''; ui.state = ''; ui.party = ''; ui.page = 0; render(); };
        f.appendChild(go); f.appendChild(clr);
        return f;
    }

    function stateNote() {
        var p = el('div', 'panel');
        var map = {
            loading: ['note', 'Reading the batches...'],
            empty: ['note', 'No jobs match. Clear the filters or widen the dates.'],
            error: ['note err', 'The batches could not be read: the compute service did not answer.'],
            refused: ['note', 'Your role cannot read jobs.']
        }[screen];
        var n = el('div', map[0], map[1]);
        if (screen === 'error') { var r = el('button', 'btn', 'Retry'); r.style.marginLeft = '12px'; r.onclick = function () { setScreen('loaded'); }; n.appendChild(r); }
        p.appendChild(n);
        return p;
    }

    function setScreen(s) { screen = s; render(); }
    function setRole(r) { role = r; ui.page = 0; ui.party = ''; ui.selected = null; render(); }

    function bar() {
        var b = document.getElementById('proto-bar');
        b.textContent = '';
        var line = el('div', 'bar-line');
        line.appendChild(el('span', 'tag-prototype', 'Prototype'));
        line.appendChild(el('span', 'label', 'Role'));
        Object.keys(ROLES).forEach(function (k) {
            var x = el('button', k === role ? 'on' : '', ROLES[k].name);
            x.onclick = function () { setRole(k); };
            line.appendChild(x);
        });
        line.appendChild(el('span', 'sep', '|'));
        line.appendChild(el('span', 'label', 'State'));
        STATES.forEach(function (k) {
            var x = el('button', k === screen ? 'on' : '', k);
            x.onclick = function () { setScreen(k); };
            line.appendChild(x);
        });
        b.appendChild(line);
        var g = el('div', 'bar-line');
        g.appendChild(el('span', 'gist dim', ROLES[role].gist));
        b.appendChild(g);
    }

    function render() {
        document.getElementById('proto-note').textContent = 'Prototype: mock data, nothing is read from the server';
        var app = document.getElementById('app');
        app.textContent = '';
        app.appendChild(el('h1', 'screen-title', 'Compute grid'));
        var tabs = el('div', 'tabs');
        ['Overview', 'Machines', 'Usage', 'Jobs'].forEach(function (t) { tabs.appendChild(el('span', t === 'Jobs' ? 'on' : '', t)); });
        app.appendChild(tabs);
        if (screen === 'refused') {
            app.appendChild(stateNote());
        } else {
            app.appendChild(finder());
            var shown = screen === 'loaded' && filtered().length === 0 ? 'empty' : screen;
            if (shown === 'loaded') {
                var split = el('div', 'split');
                split.appendChild(tree());
                split.appendChild(details());
                app.appendChild(split);
            } else {
                var saved = screen;
                screen = shown;
                app.appendChild(stateNote());
                screen = saved;
            }
        }
        bar();
    }

    render();
}());
