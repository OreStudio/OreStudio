/* Compute: one app version. Self-contained: plain JavaScript, mock data, no
 * framework, no build step.
 *
 * From doc/knowledge/journeys/compute/journey_keep_what_the_grid_runs.org.
 *
 * Every field the screen shows is a field of the entity model it comes from
 * (ores.compute.app_version, .app_version_platform and .platform), under the
 * model's own table-display header. The one exception is the release note.
 * The app version has no such column today, and the story that adds it names
 * the field release_note. Nothing here reads the server. */

(function () {
    'use strict';

    var ROLES = {
        operator: { name: 'Operator', gist: 'A super administrator. Reads the catalogue and writes the release note.', writes: true },
        tenant: { name: 'Tenant admin', gist: 'Reads the catalogue. The write controls are withheld.', writes: false },
        member: { name: 'Party member', gist: 'Reads the catalogue. The write controls are withheld.', writes: false }
    };
    var STATES = ['loaded', 'loading', 'empty', 'error', 'refused'];
    var VERSIONS = ['newest', 'older', 'nopackage'];
    var PAGE_SIZES = [5, 10];
    var SERVICE = 'ores_solid_dirac_compute_service';
    var STORAGE = '/api/v1/storage/ores/compute/packages/ore/';

    /* The app, the version ids and the platform codes come from the
     * development database. The engine versions, the digests, the package
     * names and the release notes are invented. */
    var APP = { id: '5b0c6d2e-1f3a-4e8b-9c47-2a6d0e91f3b8', name: 'ORE', description: 'Open Source Risk Engine' };

    var NOTES = {
        newest: [
            '## Summary',
            'This release moves the engine to ORE 1.8.12 and fixes the memory growth seen in long Monte Carlo runs.',
            '## Changes',
            '- The engine is built from ORE 1.8.12.0.',
            '- The wrapper reports peak memory with each result.',
            '- The wrapper checks the package digest before it starts a job.',
            '## Known issues',
            '- A market data snapshot older than 90 days stops the run with exit code 3.'
        ],
        older: [
            '## Summary',
            'The last release before the wrapper reported peak memory.',
            '## Changes',
            '- The engine is built from ORE 1.8.11.0.',
            '- The wrapper retries a failed package download three times.'
        ],
        nopackage: null
    };

    var PLATFORMS = [
        { code: 'linux-x86_64-gnu', display_name: 'Linux x86-64', os_family: 'linux', cpu_arch: 'x86_64', active: true },
        { code: 'linux-aarch64-gnu', display_name: 'Linux ARM64', os_family: 'linux', cpu_arch: 'aarch64', active: true },
        { code: 'macos-arm64', display_name: 'macOS Apple silicon', os_family: 'macos', cpu_arch: 'arm64', active: true },
        { code: 'windows-x86_64-msvc', display_name: 'Windows x86-64', os_family: 'windows', cpu_arch: 'x86_64', active: false }
    ];

    var ALL = [
        { key: 'newest', engine_version: '1.8.12.0', wrapper_version: '0.14.2', min_ram_mb: 2048, recorded_at: '2026-10-09 14:02:11' },
        { key: 'older', engine_version: '1.8.11.0', wrapper_version: '0.14.0', min_ram_mb: 2048, recorded_at: '2026-09-18 09:41:50' },
        { key: 'nopackage', engine_version: '1.8.13.0', wrapper_version: '0.14.3', min_ram_mb: 4096, recorded_at: '2026-10-21 16:20:03' }
    ];

    function hex(seed, n) {
        var x = 0x9e3779b1 ^ seed, out = '';
        for (var i = 0; i < n; i++) {
            x = (Math.imul(x, 1103515245) + 12345) >>> 0;
            out += ((x >>> 16) & 15).toString(16);
        }
        return out;
    }

    function uuid(seed) {
        var h = hex(seed, 32);
        return h.slice(0, 8) + '-' + h.slice(8, 12) + '-' + h.slice(12, 16) + '-' + h.slice(16, 20) + '-' + h.slice(20);
    }

    var q = new URLSearchParams(location.search);
    var role = ROLES[q.get('role')] ? q.get('role') : 'operator';
    var screen = STATES.indexOf(q.get('state')) >= 0 ? q.get('state') : 'loaded';
    var which = VERSIONS.indexOf(q.get('version')) >= 0 ? q.get('version') : 'newest';
    var ui = { page: 0, size: 5, editing: false };

    function current() {
        var v = ALL.filter(function (x) { return x.key === which; })[0];
        var seed = ALL.indexOf(v) + 1;
        return {
            id: uuid(seed * 31),
            app_id: APP.id,
            wrapper_version: v.wrapper_version,
            engine_version: v.engine_version,
            min_ram_mb: v.min_ram_mb,
            release_note: NOTES[which],
            modified_by: SERVICE,
            recorded_at: v.recorded_at,
            packages: which === 'nopackage' ? [] : PLATFORMS.map(function (p, i) {
                return {
                    platform: p,
                    package_uri: STORAGE + v.engine_version + '/ore-' + v.engine_version + '-' + p.code + '.tar.gz',
                    sha256: hex(seed * 1000 + i, 64)
                };
            })
        };
    }

    function el(tag, cls, text) {
        var e = document.createElement(tag);
        if (cls) { e.className = cls; }
        if (text !== undefined) { e.textContent = text; }
        return e;
    }

    function badge(text, tone) { return el('span', 'badge b-' + tone, text); }

    var toastTimer = null;
    function toast(text) {
        var t = document.getElementById('toast');
        if (!t) { t = el('div', 'toast'); t.id = 'toast'; document.body.appendChild(t); }
        t.textContent = text + ' (prototype: nothing is served)';
        t.style.display = 'block';
        clearTimeout(toastTimer);
        toastTimer = setTimeout(function () { t.style.display = 'none'; }, 3500);
    }

    function panel(title, extra) {
        var p = el('div', 'panel');
        var h = el('h2', 'with-count', title);
        if (extra) { h.appendChild(typeof extra === 'string' ? el('span', 'faint', extra) : extra); }
        p.appendChild(h);
        return p;
    }

    function facts(rows) {
        var dl = el('dl', 'facts');
        rows.forEach(function (r) {
            dl.appendChild(el('dt', '', r[0]));
            var dd = el('dd');
            if (typeof r[1] === 'string') { dd.textContent = r[1]; } else { dd.appendChild(r[1]); }
            dl.appendChild(dd);
        });
        return dl;
    }

    function link(text, target, mono) {
        var a = el('a', mono ? 'mono' : '', text);
        a.href = '#';
        a.title = 'Open ' + target;
        a.onclick = function (e) { e.preventDefault(); toast('Would open ' + target + ' ' + text); };
        return a;
    }

    function header(v) {
        var p = panel('App version', 'Read only');
        var top = el('div', 'head-id');
        top.appendChild(el('div', 'kicker', 'ID'));
        top.appendChild(el('div', 'big mono', v.id));
        p.appendChild(top);
        p.appendChild(facts([
            ['App ID', link(APP.name + ' (' + v.app_id.slice(0, 8) + ')', 'app')],
            ['Wrapper Version', v.wrapper_version],
            ['Engine Version', v.engine_version],
            ['Min RAM (MB)', String(v.min_ram_mb)],
            ['Modified By', v.modified_by],
            ['Recorded At', v.recorded_at]
        ]));
        return p;
    }

    /* The note is stored text. The page reads two marks: a line that starts
     * with "## " is a heading and a line that starts with "- " is an item. */
    function noteBody(lines) {
        var body = el('div', 'note-body');
        var list = null;
        lines.forEach(function (line) {
            if (line.indexOf('- ') === 0) {
                if (!list) { list = el('ul'); body.appendChild(list); }
                list.appendChild(el('li', '', line.slice(2)));
                return;
            }
            list = null;
            if (line.indexOf('## ') === 0) { body.appendChild(el('h3', '', line.slice(3))); } else { body.appendChild(el('p', '', line)); }
        });
        return body;
    }

    function releaseNote(v) {
        var actions = el('span', 'panel-actions');
        if (ROLES[role].writes) {
            var edit = el('button', 'btn', v.release_note ? 'Edit release note' : 'Write release note');
            edit.onclick = function () { toast('Would open the release note for editing'); };
            actions.appendChild(edit);
        } else {
            actions.appendChild(el('span', 'access', 'Read only'));
        }
        var p = panel('Release Note', actions);
        if (v.release_note) {
            p.appendChild(noteBody(v.release_note));
        } else {
            var n = el('div', 'note');
            n.appendChild(el('div', '', 'This release has no note.'));
            if (!ROLES[role].writes) { n.appendChild(el('p', 'hint', 'A super administrator writes the note when the version is published.')); }
            p.appendChild(n);
        }
        return p;
    }

    function packages(v) {
        var p = panel('Platforms and packages', v.packages.length + (v.packages.length === 1 ? ' package' : ' packages'));
        if (!v.packages.length) {
            p.appendChild(el('p', 'warn-note', 'This version has no package. A job that pins it is stored and never runs. The wrapper has nothing to download.'));
            return p;
        }
        var wrap = el('div', 'scroll');
        var t = el('table', 'table');
        var head = el('tr');
        ['Code', 'Display Name', 'OS Family', 'CPU Arch', 'Active', 'Package URI', 'SHA256'].forEach(function (h) { head.appendChild(el('th', '', h)); });
        t.appendChild(head);
        v.packages.forEach(function (x) {
            var tr = el('tr');
            tr.appendChild(el('td', 'mono', x.platform.code));
            tr.appendChild(el('td', '', x.platform.display_name));
            tr.appendChild(el('td', '', x.platform.os_family));
            tr.appendChild(el('td', '', x.platform.cpu_arch));
            var a = el('td');
            a.appendChild(x.platform.active ? badge('true', 'ok') : badge('false', 'gray'));
            tr.appendChild(a);
            var u = el('td');
            var dl = el('a', 'dl mono', x.package_uri.split('/').pop());
            dl.href = x.package_uri;
            dl.title = x.package_uri;
            dl.onclick = function (e) { e.preventDefault(); toast('Would download ' + x.package_uri); };
            u.appendChild(dl);
            tr.appendChild(u);
            var s = el('td', 'sha', x.sha256.slice(0, 16) + '…');
            s.title = x.sha256;
            tr.appendChild(s);
            t.appendChild(tr);
        });
        wrap.appendChild(t);
        p.appendChild(wrap);
        return p;
    }

    function pager(total) {
        var bar = el('div', 'pager');
        var last = Math.max(0, Math.ceil(total / ui.size) - 1);
        [['First', 0], ['Previous', ui.page - 1], ['Next', ui.page + 1], ['Last', last]].forEach(function (x) {
            var b = el('button', 'btn', x[0]);
            b.disabled = x[1] < 0 || x[1] > last || x[1] === ui.page;
            b.onclick = function () { ui.page = x[1]; render(); };
            bar.appendChild(b);
        });
        bar.appendChild(el('span', '', 'Page ' + (ui.page + 1) + ' of ' + (last + 1) + ', ' + total + ' in all'));
        var sel = el('select');
        PAGE_SIZES.forEach(function (n) {
            var o = el('option', '', n + ' per page');
            o.value = n;
            o.selected = n === ui.size;
            sel.appendChild(o);
        });
        sel.onchange = function () { ui.size = parseInt(sel.value, 10); ui.page = 0; render(); };
        bar.appendChild(sel);
        var all = el('button', 'btn', 'Load all');
        all.onclick = function () { ui.size = total; ui.page = 0; render(); };
        bar.appendChild(all);
        return bar;
    }

    function siblings() {
        var p = panel('Versions of ' + APP.name, ALL.length + ' versions');
        var wrap = el('div', 'scroll');
        var t = el('table', 'table');
        var head = el('tr');
        ['Engine Version', 'Wrapper Version', 'Min RAM (MB)', 'Recorded At'].forEach(function (h) { head.appendChild(el('th', '', h)); });
        t.appendChild(head);
        ALL.slice(ui.page * ui.size, ui.page * ui.size + ui.size).forEach(function (v) {
            var tr = el('tr', v.key === which ? 'sel' : '');
            var c = el('td');
            if (v.key === which) { c.textContent = v.engine_version + ' (this version)'; } else {
                var a = el('a', '', v.engine_version);
                a.href = '#';
                a.onclick = function (e) { e.preventDefault(); which = v.key; render(); };
                c.appendChild(a);
            }
            tr.appendChild(c);
            tr.appendChild(el('td', '', v.wrapper_version));
            tr.appendChild(el('td', '', String(v.min_ram_mb)));
            tr.appendChild(el('td', '', v.recorded_at));
            t.appendChild(tr);
        });
        wrap.appendChild(t);
        p.appendChild(wrap);
        p.appendChild(pager(ALL.length));
        return p;
    }

    function finder() {
        var f = el('div', 'finder');
        var l = el('label', '', 'Find');
        var t = el('input'); t.type = 'search'; t.placeholder = 'app, engine version or id';
        t.onkeydown = function (e) { if (e.key === 'Enter') { toast('Would find ' + t.value.trim()); } };
        l.appendChild(t);
        f.appendChild(l);
        var go = el('button', 'btn', 'Find');
        go.onclick = function () { toast('Would find ' + t.value.trim()); };
        f.appendChild(go);
        return f;
    }

    function stateNote() {
        var p = el('div', 'panel');
        var map = {
            loading: ['note', 'Reading the app version...'],
            empty: ['note', 'No such app version. It matches nothing.'],
            error: ['note err', 'The app version could not be read: the compute service did not answer.'],
            refused: ['note', 'Your role cannot read the catalogue.']
        }[screen];
        var n = el('div', map[0], map[1]);
        if (screen === 'error') {
            var r = el('button', 'btn', 'Retry');
            r.style.marginLeft = '12px';
            r.onclick = function () { setScreen('loaded'); };
            n.appendChild(r);
        }
        if (screen === 'empty') {
            var back = el('a', '', 'Back to the catalogue');
            back.href = '#';
            back.style.marginLeft = '12px';
            back.onclick = function (e) { e.preventDefault(); toast('Would open the catalogue'); };
            n.appendChild(back);
        }
        p.appendChild(n);
        return p;
    }

    function setScreen(s) { screen = s; render(); }
    function setRole(r) { role = r; render(); }
    function setVersion(k) { which = k; render(); }

    function group(line, label, names, currentName, onPick) {
        line.appendChild(el('span', 'label', label));
        names.forEach(function (k) {
            var x = el('button', k === currentName ? 'on' : '', k);
            x.onclick = function () { onPick(k); };
            line.appendChild(x);
        });
    }

    function bar() {
        var b = document.getElementById('proto-bar');
        b.textContent = '';
        var line = el('div', 'bar-line');
        line.appendChild(el('span', 'tag-prototype', 'Prototype'));
        group(line, 'Role', Object.keys(ROLES), role, setRole);
        line.appendChild(el('span', 'sep', '|'));
        group(line, 'State', STATES, screen, setScreen);
        b.appendChild(line);
        var l2 = el('div', 'bar-line');
        group(l2, 'Version', VERSIONS, which, setVersion);
        b.appendChild(l2);
        var g = el('div', 'bar-line');
        g.appendChild(el('span', 'gist dim', ROLES[role].gist));
        b.appendChild(g);
    }

    function render() {
        document.getElementById('proto-note').textContent = 'Prototype: mock data, nothing is read from the server';
        var app = document.getElementById('app');
        app.textContent = '';
        app.appendChild(el('h1', 'screen-title', 'Compute catalogue'));
        if (screen === 'refused') {
            app.appendChild(stateNote());
        } else {
            app.appendChild(finder());
            if (screen !== 'loaded') {
                app.appendChild(stateNote());
            } else {
                var v = current();
                var crumbs = el('div', 'crumbs');
                var back = el('a', '', '← Catalogue, ' + APP.name);
                back.href = '#';
                back.onclick = function (e) { e.preventDefault(); toast('Would open the catalogue'); };
                crumbs.appendChild(back);
                app.appendChild(crumbs);
                var left = el('div', 'stack');
                left.appendChild(header(v));
                left.appendChild(releaseNote(v));
                var right = el('div', 'stack');
                right.appendChild(siblings());
                var cols = el('div', 'cols');
                cols.appendChild(left);
                cols.appendChild(right);
                app.appendChild(cols);
                var wide = el('div', 'stack');
                wide.style.marginTop = '16px';
                wide.appendChild(packages(v));
                app.appendChild(wide);
            }
        }
        bar();
    }

    render();
}());
