/* Compute: browse the grid's jobs. Self-contained: plain JavaScript, mock
 * data, no framework, no build step.
 *
 * From doc/knowledge/journeys/compute/journey_browse_the_grids_jobs.org.
 *
 * Every field the screen shows is a field of the entity model it comes from
 * (ores.compute.batch, .workunit, .result and .host), under the model's own
 * table-display header. The screen computes nothing: no count, no total and
 * no state that the models do not hold. A list's own paging total is the one
 * figure the server returns beside the rows. Nothing here reads the server. */

(function () {
    'use strict';

    var ROLES = {
        operator: { name: 'Operator', gist: 'Sees every tenant and party, and the host of each result.', parties: true, hosts: true },
        tenant: { name: 'Tenant admin', gist: 'Sees every party of the tenant. No host names.', parties: true, hosts: false },
        member: { name: 'Party member', gist: 'Sees only the party they belong to. No host names.', parties: false, hosts: false }
    };
    var STATES = ['loaded', 'loading', 'empty', 'error', 'refused'];
    var PAGE_SIZES = [10, 25, 50];

    /* batch.status: open, processing, assimilating, closed. */
    var BATCH_STATUSES = ['open', 'processing', 'assimilating', 'closed'];
    /* result.server_state: 1=Inactive, 2=Unsent, 4=InProgress, 5=Done. */
    var SERVER_STATE = {
        1: ['Inactive', 'gray'], 2: ['Unsent', 'queue'], 4: ['InProgress', 'run'], 5: ['Done', 'ok']
    };
    /* result.outcome: 1=Success, 3=ClientError, 4=NoReply; 0 is not set. */
    var OUTCOME = { 1: ['Success', 'ok'], 3: ['ClientError', 'bad'], 4: ['NoReply', 'warn'] };

    /* Shapes taken from the development database on 2026-10-13: the soak's
     * external_ref values, the party codes, the storage URIs, the host display
     * names and the stored server_state and outcome (all 5 and 1). The failed
     * and in-flight results, the app_version_id and the ids are invented. */
    var PARTY_OF = { p1: 'ACCOHK', p2: 'ACCOUS', p3: 'ACCOUK', p4: 'ACMCOR' };
    var PARTIES = ['ACCOUK', 'ACCOUS', 'ACCOHK', 'ACMCOR'];
    var HOSTS = [
        'timid-uakari-3eaa5870', 'quiet-hamster-ed10585e', 'idle-xerus-ed485b3f',
        'timid-hamster-593e583f', 'quiet-yak-bc485725', 'bumbling-tapir-b16f5933',
        'witty-tapir-d8115deb', 'plucky-pangolin-6d6f5712', 'graceful-xerus-22625e7e',
        'lively-walrus-b698576f', 'rusty-uakari-eb685c79', 'brave-yak-d562501d'
    ];
    var SIZES = { p1: [15, 6, 9, 12], p2: [20, 10, 14, 8], p3: [35, 15, 31, 24], p4: [3, 2, 3, 2] };
    var STORAGE = '/api/v1/storage/ores/compute/';
    var SERVICE = 'ores_solid_dirac_compute_service';
    var APP_VERSION = '01a132ed-e945-76fb-b727-b9daee60a0ed';

    function pad(n) { return n < 10 ? '0' + n : '' + n; }

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
            batches.push({
                id: uuid(++seq),
                external_ref: 'soak-1013031448-' + wave + '-' + pk,
                party: PARTY_OF[pk],
                status: wave === 9 && pk === 'p3' ? 'processing' : 'closed',
                modified_by: SERVICE,
                recorded_at: '2026-10-13 02:' + pad(27 + (wave % 3)) + ':27',
                jobs: SIZES[pk][(wave + pi) % 4],
                failed: wave === 7 && pk === 'p3' ? 2 : wave === 5 && pk === 'p2' ? 1 : 0
            });
        });
    });

    function workunitsOf(b) {
        var out = [];
        for (var w = 0; w < b.jobs; w++) {
            var id = uuid(b.jobs * 977 + w * 31 + b.external_ref.length + b.external_ref.charCodeAt(b.external_ref.length - 3));
            out.push({
                id: id,
                batch_id: b.id,
                app_version_id: APP_VERSION,
                input_uri: STORAGE + 'input/' + b.id + '.tar.gz',
                config_uri: null,
                priority: 1,
                target_redundancy: 1,
                canonical_result_id: null,
                modified_by: SERVICE,
                recorded_at: b.recorded_at,
                failing: w < b.failed
            });
            resultsOf(b, out[out.length - 1]);
        }
        return out;
    }

    function resultsOf(b, wu) {
        var inFlight = b.status === 'processing' && wu.id.charCodeAt(0) % 3 === 0;
        var rid = uuid(wu.id.charCodeAt(2) * 131 + wu.id.charCodeAt(5) * 17 + wu.id.length * 7 + wu.id.charCodeAt(9));
        var r = {
            id: rid,
            workunit_id: wu.id,
            host: inFlight ? null : HOSTS[(wu.id.charCodeAt(1)) % HOSTS.length],
            server_state: inFlight ? 4 : 5,
            outcome: inFlight ? 0 : wu.failing ? 3 : 1,
            output_uri: inFlight || wu.failing ? null : STORAGE + 'output/' + rid + '.tar.gz',
            error_message: wu.failing ? 'engine exited with code 3: market data snapshot not found' : null,
            received_at: inFlight ? null : '2026-10-13 02:' + pad(27 + (wu.id.charCodeAt(4) % 3)) + ':' + pad(10 + (wu.id.charCodeAt(3) % 49)),
            modified_by: SERVICE,
            recorded_at: b.recorded_at
        };
        wu.canonical_result_id = inFlight || wu.failing ? null : rid;
        return [r];
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

    function badge(text, tone) {
        return el('span', 'badge b-' + tone, text);
    }

    function statusTone(st) {
        return { open: 'queue', processing: 'run', assimilating: 'run', closed: 'ok' }[st] || 'queue';
    }

    function filtered() {
        return batches.filter(function (b) {
            return (!ui.text || b.external_ref.indexOf(ui.text) >= 0 || b.id === ui.text) &&
                (!ui.state || b.status === ui.state) &&
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
            var o = el('option', '', n + ' per page');
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

    function row(label, id, twist, badges, note) {
        var r = el('li', 'row' + (ui.selected === id ? ' sel' : ''));
        var n = el('span', 'name');
        n.appendChild(el('span', 'twist', twist || ''));
        n.appendChild(document.createTextNode(label));
        r.appendChild(n);
        r.appendChild(el('span', 'count', note || ''));
        var bs = el('span', 'badges');
        (badges || []).forEach(function (x) { bs.appendChild(x); });
        r.appendChild(bs);
        r.dataset.id = id;
        return r;
    }

    function resultBadges(res) {
        var out = [badge(SERVER_STATE[res.server_state][0], SERVER_STATE[res.server_state][1])];
        if (OUTCOME[res.outcome]) { out.push(badge(OUTCOME[res.outcome][0], OUTCOME[res.outcome][1])); }
        return out;
    }

    function tree() {
        var list = filtered();
        var panel = el('div', 'panel');
        var head = el('h2', 'with-count', 'Batches, workunits and results');
        head.appendChild(el('span', 'faint', list.length + ' batch' + (list.length === 1 ? '' : 'es') + ', page ' + (ui.page + 1) + ' of ' + Math.max(1, Math.ceil(list.length / ui.size))));
        panel.appendChild(head);
        var ul = el('ul', 'tree');
        var start = ui.page * ui.size;
        list.slice(start, start + ui.size).forEach(function (b) {
            var open = !!ui.open[b.id];
            var r = row(b.external_ref + (ROLES[role].parties ? '  \u00b7 ' + b.party : ''), b.id, open ? '\u25be' : '\u25b8', [badge(b.status, statusTone(b.status))]);
            r.onclick = function () { ui.open[b.id] = !open; ui.selected = b.id; render(); };
            ul.appendChild(r);
            if (!open) { return; }
            var wus = workunitsOf(b);
            var wp = ui.wuPage[b.id] || 0;
            var wsize = 5;
            var l1 = el('ul', 'tree lvl1');
            wus.slice(wp * wsize, wp * wsize + wsize).forEach(function (wu) {
                var wopen = !!ui.open[wu.id];
                var wr = row(wu.id.slice(0, 8), wu.id, wopen ? '\u25be' : '\u25b8', [], 'priority ' + wu.priority + ' \u00b7 target redundancy ' + wu.target_redundancy);
                wr.onclick = function (e) { e.stopPropagation(); ui.open[wu.id] = !wopen; ui.selected = wu.id; render(); };
                l1.appendChild(wr);
                if (wopen) {
                    var l2 = el('ul', 'tree lvl2');
                    resultsOf(b, wu).forEach(function (res) {
                        var rr = row(res.id.slice(0, 8), res.id, '', resultBadges(res));
                        rr.onclick = function (e) { e.stopPropagation(); ui.selected = res.id; render(); };
                        l2.appendChild(rr);
                    });
                    l1.appendChild(l2);
                }
            });
            ul.appendChild(l1);
            if (wus.length > wsize) { ul.appendChild(pager(wus.length, wp, wsize, function (p) { ui.wuPage[b.id] = p; render(); }, function () {}, true)); }
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
                if (rs[0].id === id) { return { kind: 'result', b: b, wu: wus[j], res: rs[0] }; }
            }
        }
        return null;
    }

    function details() {
        var panel = el('div', 'panel detail-panel');
        var hit = ui.selected && find(ui.selected);
        var title = !hit ? 'Details' : hit.kind === 'batch' ? 'Batch' : hit.kind === 'workunit' ? 'Workunit' : 'Result';
        var head = el('h2', 'with-count', title);
        if (hit) { head.appendChild(el('span', 'access', 'Read only')); }
        panel.appendChild(head);
        var d = el('div', 'details');
        if (!hit) {
            d.appendChild(el('p', 'dim', 'Select a batch, a workunit or a result to read its details.'));
            panel.appendChild(d);
            return panel;
        }
        var rec = hit.kind === 'batch' ? hit.b : hit.kind === 'workunit' ? hit.wu : hit.res;
        d.appendChild(el('div', 'kicker', 'ID'));
        d.appendChild(el('div', 'big mono', rec.id));
        var dl = el('dl');
        function put(k, node) {
            dl.appendChild(el('dt', '', k));
            var dd = el('dd');
            dd.appendChild(node);
            dl.appendChild(dd);
        }
        function text(k, v) { put(k, document.createTextNode(v === null || v === '' ? 'none' : String(v))); }
        function link(k, v, target) {
            var a = el('a', v.length === 36 ? 'mono' : '', v);
            a.href = '#';
            a.title = 'Open ' + target;
            a.onclick = function (e) { e.preventDefault(); toast('Would open ' + target + ' ' + v); };
            put(k, a);
        }
        function file(k, uri) {
            if (!uri) { text(k, null); return; }
            var a = el('a', 'mono path', uri);
            a.href = uri;
            a.download = uri.split('/').pop();
            a.onclick = function (e) { e.preventDefault(); toast('Would download ' + uri); };
            put(k, a);
        }
        function audit() { text('Modified By', rec.modified_by); text('Recorded At', rec.recorded_at); }
        if (hit.kind === 'batch') {
            text('External Ref', hit.b.external_ref);
            if (ROLES[role].parties) { link('Party', hit.b.party, 'party'); }
            put('Status', badge(hit.b.status, statusTone(hit.b.status)));
            audit();
        } else if (hit.kind === 'workunit') {
            link('Batch ID', hit.b.id, 'batch');
            link('App Version ID', hit.wu.app_version_id, 'app version');
            file('Input URI', hit.wu.input_uri);
            text('Config URI', hit.wu.config_uri);
            text('Priority', hit.wu.priority);
            text('Target Redundancy', hit.wu.target_redundancy);
            if (hit.wu.canonical_result_id) { link('Canonical Result ID', hit.wu.canonical_result_id, 'result'); }
            else { text('Canonical Result ID', null); }
            audit();
        } else {
            link('Workunit ID', hit.wu.id, 'workunit');
            if (!ROLES[role].hosts) { text('Host ID', 'hidden for your role'); }
            else if (hit.res.host) { link('Host ID', hit.res.host, 'host'); }
            else { text('Host ID', null); }
            var st = SERVER_STATE[hit.res.server_state];
            put('Server State', badge(st[0], st[1]));
            if (OUTCOME[hit.res.outcome]) { put('Outcome', badge(OUTCOME[hit.res.outcome][0], OUTCOME[hit.res.outcome][1])); }
            else { text('Outcome', 'not set (0)'); }
            file('Output URI', hit.res.output_uri);
            text('Error Message', hit.res.error_message);
            text('Received At', hit.res.received_at);
            audit();
        }
        d.appendChild(dl);
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
        [''].concat(BATCH_STATUSES).forEach(function (v) { var o = el('option', '', v || 'Any'); o.value = v; o.selected = v === ui.state; s.appendChild(o); });
        s.onchange = function () { ui.state = s.value; ui.page = 0; render(); };
        field('Status', s);
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

    var toastTimer = null;
    function toast(text) {
        var t = document.getElementById('toast');
        if (!t) { t = el('div', 'toast'); t.id = 'toast'; document.body.appendChild(t); }
        t.textContent = text + ' (prototype: nothing is served)';
        t.style.display = 'block';
        clearTimeout(toastTimer);
        toastTimer = setTimeout(function () { t.style.display = 'none'; }, 3500);
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
