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
    /* Shapes taken from the development database on 2026-10-13: the soak's
     * batch references and party codes, the storage URIs, the host names, and
     * the server_state and outcome of the stored results (all done and
     * success). The failed and in-flight rows are invented, because the stored
     * data holds none. */
    var PARTY_OF = { p1: 'ACCOHK', p2: 'ACCOUS', p3: 'ACCOUK', p4: 'ACMCOR' };
    var PARTIES = ['ACCOUK', 'ACCOUS', 'ACCOHK', 'ACMCOR'];
    var NODES = [
        'timid-uakari-3eaa5870', 'quiet-hamster-ed10585e', 'idle-xerus-ed485b3f',
        'timid-hamster-593e583f', 'quiet-yak-bc485725', 'bumbling-tapir-b16f5933',
        'witty-tapir-d8115deb', 'plucky-pangolin-6d6f5712', 'graceful-xerus-22625e7e',
        'lively-walrus-b698576f', 'rusty-uakari-eb685c79', 'brave-yak-d562501d'
    ];
    var SIZES = { p1: [15, 6, 9, 12], p2: [20, 10, 14, 8], p3: [35, 15, 31, 24], p4: [3, 2, 3, 2] };
    var STORAGE = '/api/v1/storage/ores/compute/';

    function pad(n) { return n < 10 ? '0' + n : '' + n; }

    /* A stable pseudo-uuid, so the same record shows the same id each render. */
    function uuid(seed) {
        var x = 0x9e3779b1 ^ seed, out = '';
        for (var i = 0; i < 32; i++) {
            x = (Math.imul(x, 1103515245) + 12345) >>> 0;
            out += ((x >>> 16) & 15).toString(16);
            if (i === 7 || i === 11 || i === 15 || i === 19) { out += '-'; }
        }
        return out;
    }

    var batches = [];
    var seq = 0;
    [9, 8, 7, 6, 5, 4].forEach(function (wave) {
        ['p4', 'p3', 'p2', 'p1'].forEach(function (pk, pi) {
            var jobs = SIZES[pk][(wave + pi) % 4];
            var newest = wave === 9;
            batches.push({
                id: uuid(++seq),
                ref: 'soak-1013031448-' + wave + '-' + pk,
                party: PARTY_OF[pk],
                state: newest && pk === 'p3' ? 'open' : 'closed',
                jobs: jobs,
                failed: wave === 7 && pk === 'p3' ? 2 : wave === 5 && pk === 'p2' ? 1 : 0
            });
        });
    });

    function workunitsOf(b) {
        var out = [];
        for (var w = 0; w < b.jobs; w++) {
            var id = uuid(b.jobs * 977 + w * 31 + b.ref.length + b.ref.charCodeAt(b.ref.length - 3));
            out.push({
                id: id,
                label: id.slice(0, 8),
                priority: 1,
                redundancy: 1,
                input: STORAGE + 'input/' + b.id + '.tar.gz',
                config: '',
                bad: w < b.failed
            });
        }
        return out;
    }

    function resultsOf(b, wu) {
        var out = [];
        var n = wu.redundancy + (wu.bad ? 1 : 0);
        for (var r = 0; r < n; r++) {
            var failedRun = wu.bad && r === wu.redundancy;
            var inFlight = b.state === 'open' && r === 0 && wu.id.charCodeAt(0) % 3 === 0;
            var rid = uuid(wu.id.charCodeAt(2) * 131 + wu.id.charCodeAt(5) * 17 + r + wu.id.length * 7 + wu.id.charCodeAt(9));
            out.push({
                id: rid,
                label: rid.slice(0, 8),
                state: inFlight ? 'running' : failedRun ? 'failed' : 'succeeded',
                serverState: inFlight ? 'in progress (4)' : 'done (5)',
                outcome: inFlight ? '' : failedRun ? 'client error (2)' : 'success (1)',
                error: failedRun ? 'engine exited with code 3: market data snapshot not found' : '',
                node: inFlight ? '' : NODES[(wu.id.charCodeAt(1) + r) % NODES.length],
                received: inFlight ? '' : '2026-10-13 02:' + pad(27 + (r % 3)) + ':' + pad(10 + (wu.id.charCodeAt(3) % 49)),
                output: failedRun || inFlight ? '' : STORAGE + 'output/' + rid + '.tar.gz'
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
            add('Config', hit.wu.config || 'none');
            add('Batch', hit.b.ref, true);
        } else {
            add('State', hit.res.state + ' · server state ' + hit.res.serverState);
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
