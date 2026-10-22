/* Compute: follow a job. Self-contained: plain JavaScript, mock data, no
 * framework, no build step.
 *
 * From doc/knowledge/journeys/compute/journey_follow_a_job.org.
 *
 * Every field the screen shows is a field of the entity model it comes from
 * (ores.compute.batch, .workunit, .result, .host and .app_version), under the
 * model's own table-display header. The workunit has no status: a job with a
 * canonical_result_id is done, and the outcome of its run is read beside it.
 * Nothing here reads the server. */

(function () {
    'use strict';

    var ROLES = {
        operator: { name: 'Operator', gist: 'Sees the node of each run and the whole lineage.', hosts: true, parties: true },
        tenant: { name: 'Tenant admin', gist: 'Sees every party of the tenant. No node names.', hosts: false, parties: true },
        member: { name: 'Party member', gist: 'Sees only a job of their own party. No node names.', hosts: false, parties: false }
    };
    var STATES = ['loaded', 'loading', 'empty', 'error', 'refused'];
    var JOBS = ['done', 'failed', 'running'];
    var ORIGINS = ['report', 'shell'];
    var PAGE_SIZES = [5, 10];

    /* result.server_state: 1=Inactive, 2=Unsent, 4=InProgress, 5=Done. */
    var SERVER_STATE = { 1: ['Inactive', 'gray'], 2: ['Unsent', 'queue'], 4: ['InProgress', 'run'], 5: ['Done', 'ok'] };
    /* result.outcome: 1=Success, 3=ClientError, 4=NoReply; 0 is not set. */
    var OUTCOME = { 1: ['Success', 'ok'], 3: ['ClientError', 'bad'], 4: ['NoReply', 'warn'] };

    var STORAGE = '/api/v1/storage/ores/compute/';
    var SERVICE = 'ores_solid_dirac_compute_service';
    var MEMBER_PARTY = 'ACCOUK';
    var DAY = '2026-10-13 ';
    /* The batch was written at this second of the day. Every run is placed
     * from it, in seconds. */
    var BATCH_AT = 2 * 3600 + 27 * 60 + 27;

    /* Shapes taken from the development database on 2026-10-13: the soak's
     * external_ref, the party codes, the storage URIs, the host display names
     * and the app version id. The failed and in-flight runs, the report run,
     * the scheduler job and every other id are invented. */
    var HOSTS = ['timid-uakari-3eaa5870', 'quiet-hamster-ed10585e', 'idle-xerus-ed485b3f', 'brave-yak-d562501d'];
    var APP_VERSION = {
        id: '01a132ed-e945-76fb-b727-b9daee60a0ed',
        engine_version: '1.8.12.0',
        wrapper_version: '0.14.2'
    };
    var REPORT = {
        scheduler_job: { job_name: 'report.daily_risk', schedule_expression: '0 2 * * *' },
        definition: { id: '7a1c0e3b-5d22-4f0e-9a41-3c8e61d2b0a7', name: 'Daily risk report' },
        instance_id: '3e91b7c4-0a5f-4d1e-8c23-61f0aa95d7e2'
    };

    function pad(n) { return n < 10 ? '0' + n : '' + n; }

    function clock(sec) {
        return DAY + pad(Math.floor(sec / 3600)) + ':' + pad(Math.floor(sec % 3600 / 60)) + ':' + pad(sec % 60);
    }

    function uuid(seed) {
        var x = 0x9e3779b1 ^ seed, out = '';
        for (var i = 0; i < 32; i++) {
            x = (Math.imul(x, 1103515245) + 12345) >>> 0;
            out += ((x >>> 16) & 15).toString(16);
            if (i === 7 || i === 11 || i === 15 || i === 19) { out += '-'; }
        }
        return out;
    }

    var q = new URLSearchParams(location.search);
    var role = ROLES[q.get('role')] ? q.get('role') : 'operator';
    var screen = STATES.indexOf(q.get('state')) >= 0 ? q.get('state') : 'loaded';
    var jobKind = JOBS.indexOf(q.get('job')) >= 0 ? q.get('job') : 'done';
    var origin = ORIGINS.indexOf(q.get('origin')) >= 0 ? q.get('origin') : 'report';
    var ui = { page: 0, size: 5, text: '' };

    function batch() {
        return {
            id: uuid(11),
            external_ref: origin === 'report' ? REPORT.instance_id : 'soak-1013031448-9-p3',
            party: MEMBER_PARTY,
            status: jobKind === 'running' ? 'processing' : 'closed',
            modified_by: SERVICE,
            recorded_at: clock(BATCH_AT)
        };
    }

    /* The batch's jobs. The one the page is about is the first, and the
     * others are its siblings. offset and span are in seconds from the batch's
     * write; a span of null is a run still going. */
    function jobsOfBatch() {
        var rows = [
            { n: 0, offset: 1, span: jobKind === 'running' ? null : 26, bad: jobKind === 'failed' },
            { n: 1, offset: 1, span: 31, bad: false },
            { n: 2, offset: 2, span: 24, bad: false },
            { n: 3, offset: 2, span: 58, bad: false },
            { n: 4, offset: 3, span: 19, bad: true },
            { n: 5, offset: 5, span: null, bad: false }
        ];
        return rows.map(function (r) {
            return { id: uuid(100 + r.n * 17), offset: r.offset, span: r.span, bad: r.bad, here: r.n === 0 };
        });
    }

    /* The runs of the selected job. The done job asks for two runs, and the
     * failed job's canonical run is the failed one: acceptance does not
     * filter on the outcome. */
    function job() {
        var b = batch();
        var wid = jobsOfBatch()[0].id;
        var runs = [];
        function run(n, state, outcome, host, offset, span, err) {
            var rid = uuid(500 + n * 13 + jobKind.length);
            runs.push({
                id: rid,
                workunit_id: wid,
                host: host,
                server_state: state,
                outcome: outcome,
                output_uri: outcome === 1 ? STORAGE + 'output/' + rid + '.tar.gz' : null,
                error_message: err || null,
                recorded_at: clock(BATCH_AT + offset),
                received_at: span === null ? null : clock(BATCH_AT + offset + span),
                start: offset,
                end: span === null ? null : offset + span,
                stats: span === null ? null : { started_at: clock(BATCH_AT + offset + 3), finished_at: clock(BATCH_AT + offset + span - 1), input_bytes: 1048576 * 19, output_bytes: outcome === 1 ? 1048576 * 6 : 0, peak_memory_bytes: 1048576 * 612 },
                modified_by: SERVICE
            });
        }
        if (jobKind === 'done') {
            run(1, 5, 1, HOSTS[0], 1, 26);
            run(2, 5, 1, HOSTS[1], 1, 27);
        } else if (jobKind === 'failed') {
            run(1, 5, 3, HOSTS[3], 1, 14, 'engine exited with code 3: market data snapshot not found');
        } else {
            run(1, 4, 0, null, 1, null);
        }
        return {
            b: b,
            wu: {
                id: wid,
                batch_id: b.id,
                app_version_id: APP_VERSION.id,
                input_uri: STORAGE + 'input/' + b.id + '.tar.gz',
                config_uri: null,
                priority: 1,
                target_redundancy: jobKind === 'done' ? 2 : 1,
                canonical_result_id: jobKind === 'running' ? null : runs[0].id,
                modified_by: SERVICE,
                recorded_at: b.recorded_at
            },
            runs: runs
        };
    }

    function el(tag, cls, text) {
        var e = document.createElement(tag);
        if (cls) { e.className = cls; }
        if (text !== undefined) { e.textContent = text; }
        return e;
    }

    function badge(text, tone) { return el('span', 'badge b-' + tone, text); }

    function statusTone(st) {
        return { open: 'queue', processing: 'run', assimilating: 'run', closed: 'ok' }[st] || 'queue';
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

    function link(text, target, mono) {
        var a = el('a', mono ? 'mono' : '', text);
        a.href = '#';
        a.title = 'Open ' + target;
        a.onclick = function (e) { e.preventDefault(); toast('Would open ' + target + ' ' + text); };
        return a;
    }

    function file(uri) {
        if (!uri) { return el('span', 'unknown', 'none'); }
        var a = el('a', 'mono path', uri);
        a.href = uri;
        a.onclick = function (e) { e.preventDefault(); toast('Would download ' + uri); };
        return a;
    }

    function panel(title, extra) {
        var p = el('div', 'panel');
        var h = el('h2', 'with-count', title);
        if (extra) { h.appendChild(el('span', 'faint', extra)); }
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

    function nodeCell(res) {
        if (!ROLES[role].hosts) { return el('span', 'unknown', 'hidden for your role'); }
        if (res.host) { return link(res.host, 'host'); }
        return el('span', 'unknown', 'not yet known');
    }

    /* The workunit has no status. A job with a canonical result is done, and
     * a job that ran and failed is done too, so the outcome is read beside it. */
    function stateBadges(d) {
        var out = el('span', 'badges');
        var canon = d.runs.filter(function (r) { return r.id === d.wu.canonical_result_id; })[0];
        if (!canon) {
            out.appendChild(badge('Not done', 'run'));
            return out;
        }
        out.appendChild(badge('Done', 'ok'));
        if (OUTCOME[canon.outcome]) { out.appendChild(badge(OUTCOME[canon.outcome][0], OUTCOME[canon.outcome][1])); }
        return out;
    }

    function header(d) {
        var p = panel('Job', 'Read only');
        var top = el('div', 'head-id');
        top.appendChild(el('div', 'kicker', 'Workunit ID'));
        top.appendChild(el('div', 'big mono', d.wu.id));
        top.appendChild(stateBadges(d));
        p.appendChild(top);
        var first = d.runs[0];
        p.appendChild(facts([
            ['External Ref', link(d.b.external_ref, 'batch', d.b.external_ref.length === 36)],
            ['Batch Status', badge(d.b.status, statusTone(d.b.status))],
            ['Party', ROLES[role].parties ? link(d.b.party, 'party') : d.b.party],
            ['Host ID', nodeCell(first)],
            ['Batch Recorded At', d.b.recorded_at],
            ['Received At', first.received_at || el('span', 'unknown', 'not yet received')],
            ['Priority', String(d.wu.priority)],
            ['Target Redundancy', String(d.wu.target_redundancy)],
            ['Canonical Result ID', d.wu.canonical_result_id ? link(d.wu.canonical_result_id, 'result', true) : el('span', 'unknown', 'none')]
        ]));
        if (d.runs.some(function (r) { return r.outcome === 3; })) {
            p.appendChild(el('p', 'warn-note', 'A closed batch is not a successful batch. This job ran and failed, and the grid counts it as done. Read the outcome of the run.'));
        }
        return p;
    }

    function runsTable(d) {
        var p = panel('Runs of the job', d.runs.length + (d.runs.length === 1 ? ' run' : ' runs'));
        var wrap = el('div', 'scroll');
        var t = el('table', 'table');
        var head = el('tr');
        ['Run', 'Server State', 'Outcome', 'Host ID', 'Recorded At', 'Received At'].forEach(function (h) { head.appendChild(el('th', '', h)); });
        t.appendChild(head);
        d.runs.forEach(function (r, i) {
            var tr = el('tr', r.id === d.wu.canonical_result_id ? 'sel' : '');
            tr.appendChild(el('td', '', String(i + 1) + (r.id === d.wu.canonical_result_id ? ' (canonical)' : '')));
            var s = el('td'); s.appendChild(badge(SERVER_STATE[r.server_state][0], SERVER_STATE[r.server_state][1])); tr.appendChild(s);
            var o = el('td');
            if (OUTCOME[r.outcome]) { o.appendChild(badge(OUTCOME[r.outcome][0], OUTCOME[r.outcome][1])); } else { o.appendChild(el('span', 'unknown', 'not set (0)')); }
            tr.appendChild(o);
            var n = el('td'); n.appendChild(nodeCell(r)); tr.appendChild(n);
            tr.appendChild(el('td', '', r.recorded_at));
            var rc = el('td'); if (r.received_at) { rc.textContent = r.received_at; } else { rc.appendChild(el('span', 'unknown', 'not yet received')); } tr.appendChild(rc);
            t.appendChild(tr);
            if (r.error_message) {
                var er = el('tr');
                var ec = el('td', '', 'Error Message: ' + r.error_message);
                ec.colSpan = 6;
                ec.style.color = 'var(--bad)';
                er.appendChild(ec);
                t.appendChild(er);
            }
        });
        wrap.appendChild(t);
        p.appendChild(wrap);
        return p;
    }

    /* One bar for the run, from its row being written to its output arriving.
     * The stored times give two ends and nothing between them, so the bar is
     * one block and the page says why. */
    function waterfall(d) {
        var p = panel('Where the time went', 'two stored times');
        var r = d.runs[0];
        var span = r.end === null ? null : r.end - r.start;
        var body = el('div', 'body-pad');
        var track = el('div', 'bar-track');
        var fill = el('div', 'bar-fill ' + (r.end === null ? 'run' : r.outcome === 3 ? 'bad' : ''));
        fill.style.left = '0%';
        fill.style.width = r.end === null ? '55%' : '100%';
        track.appendChild(fill);
        body.appendChild(track);
        var axis = el('div', 'axis');
        axis.style.padding = '6px 0 0';
        axis.appendChild(el('span', '', 'Recorded ' + r.recorded_at.slice(11)));
        axis.appendChild(el('span', '', r.received_at ? 'Received ' + r.received_at.slice(11) + ' (' + span + ' s)' : 'still running'));
        body.appendChild(axis);
        body.appendChild(el('p', 'hint', 'The service stores when the run was written and when its output arrived. It stores nothing between, so the bar is not split into phases.'));
        p.appendChild(body);
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

    /* The batch's jobs, one row each, a bar per job from its dispatch to its
     * output. The page's own job is highlighted. */
    function timeline() {
        var rows = jobsOfBatch();
        var p = panel('Jobs of the batch', rows.length + ' jobs');
        var horizon = 64;
        var wrap = el('div', 'scroll');
        var t = el('table', 'table');
        var head = el('tr');
        ['Workunit ID', 'Timeline'].forEach(function (h) { head.appendChild(el('th', '', h)); });
        t.appendChild(head);
        rows.slice(ui.page * ui.size, ui.page * ui.size + ui.size).forEach(function (r) {
            var tr = el('tr', r.here ? 'sel' : '');
            var c = el('td');
            c.appendChild(r.here ? el('span', 'mono', r.id.slice(0, 8) + ' (this job)') : link(r.id.slice(0, 8), 'job', true));
            tr.appendChild(c);
            var b = el('td');
            var track = el('div', 'bar-track');
            var fill = el('div', 'bar-fill ' + (r.span === null ? 'run' : r.bad ? 'bad' : '') + (r.here ? ' sel' : ''));
            fill.style.left = (r.offset / horizon * 100) + '%';
            fill.style.width = ((r.span === null ? horizon - r.offset - 8 : r.span) / horizon * 100) + '%';
            track.appendChild(fill);
            b.appendChild(track);
            tr.appendChild(b);
            t.appendChild(tr);
        });
        wrap.appendChild(t);
        p.appendChild(wrap);
        var axis = el('div', 'axis');
        axis.appendChild(el('span', '', clock(BATCH_AT).slice(11)));
        axis.appendChild(el('span', '', clock(BATCH_AT + horizon).slice(11)));
        p.appendChild(axis);
        p.appendChild(pager(rows.length));
        return p;
    }

    function filesPanel(d) {
        var p = panel('Files');
        var f = el('div', 'files');
        [['Input URI', d.wu.input_uri], ['Config URI', d.wu.config_uri]].forEach(function (x) {
            var w = el('div'); w.appendChild(el('span', 'kicker', x[0])); w.appendChild(file(x[1])); f.appendChild(w);
        });
        d.runs.forEach(function (r, i) {
            var w = el('div');
            w.appendChild(el('span', 'kicker', 'Output URI, run ' + (i + 1)));
            w.appendChild(file(r.output_uri));
            f.appendChild(w);
        });
        p.appendChild(f);
        return p;
    }

    function versionLink(id) {
        var a = el('a', 'mono', id);
        a.href = '../app-version/index.html?role=' + role;
        a.title = 'Open app version';
        return a;
    }

    function releasePanel(d) {
        var p = panel('Release that ran it');
        p.appendChild(facts([
            ['App Version ID', versionLink(d.wu.app_version_id)],
            ['Engine Version', APP_VERSION.engine_version],
            ['Wrapper Version', APP_VERSION.wrapper_version]
        ]));
        return p;
    }

    function mib(n) { return (n / 1048576).toFixed(n >= 1073741824 ? 0 : 1) + ' MiB'; }

    /* One row per run, one column per field the run statistics add to the
     * result: started_at, finished_at, input_bytes, output_bytes and
     * peak_memory_bytes. A run still going has none of them yet. */
    function statsPanel(d) {
        var p = panel('Run statistics', d.runs.length + (d.runs.length === 1 ? ' run' : ' runs'));
        var wrap = el('div', 'scroll');
        var t = el('table', 'table');
        var head = el('tr');
        ['Run', 'Started At', 'Finished At', 'Input Bytes', 'Output Bytes', 'Peak Memory'].forEach(function (h) { head.appendChild(el('th', '', h)); });
        t.appendChild(head);
        d.runs.forEach(function (r, i) {
            var tr = el('tr');
            tr.appendChild(el('td', '', String(i + 1)));
            if (!r.stats) {
                var c = el('td');
                c.colSpan = 5;
                c.appendChild(el('span', 'unknown', 'not yet known: the run has not finished'));
                tr.appendChild(c);
            } else {
                tr.appendChild(el('td', '', r.stats.started_at.slice(11)));
                tr.appendChild(el('td', '', r.stats.finished_at.slice(11)));
                tr.appendChild(el('td', '', mib(r.stats.input_bytes)));
                tr.appendChild(el('td', '', r.stats.output_bytes ? mib(r.stats.output_bytes) : 'none'));
                tr.appendChild(el('td', '', mib(r.stats.peak_memory_bytes)));
            }
            t.appendChild(tr);
        });
        wrap.appendChild(t);
        p.appendChild(wrap);
        return p;
    }

    /* The chain that put the job on the grid. The hop from the batch to the
     * report run is its external_ref and not a checked relationship, so the
     * page says so. A batch the shell dispatched has no report behind it. */
    function lineage(d) {
        var p = panel('Lineage');
        var ol = el('ol', 'chain');
        function item(cls, what, via, node) {
            var li = el('li', cls);
            var w = el('div', 'what');
            if (node) { w.appendChild(node); } else { w.textContent = what; }
            li.appendChild(w);
            if (via) { li.appendChild(el('div', 'via', via)); }
            ol.appendChild(li);
            return li;
        }
        if (origin === 'report') {
            item('', '', 'Scheduled job ' + REPORT.scheduler_job.job_name + ', ' + REPORT.scheduler_job.schedule_expression, link(REPORT.scheduler_job.job_name, 'scheduled job'));
            item('', '', 'Report definition, by scheduler_job_id', link(REPORT.definition.name, 'report definition'));
            item('', '', 'Report run, by definition_id and trigger_run_id', link(REPORT.instance_id.slice(0, 8), 'report run', true));
            item('gap', '', 'The batch names the run in its external_ref. The model does not state this as a relationship.', link(d.b.external_ref.slice(0, 8), 'batch', true));
        } else {
            item('gap', 'No report behind this batch', 'The batch carries an operator string in its external_ref: ' + d.b.external_ref + '. The shell dispatched it.');
            item('', '', 'Batch', link(d.b.external_ref, 'batch'));
        }
        var here = item('here', 'Workunit ' + d.wu.id.slice(0, 8) + ' (this job)', 'by batch_id');
        var sub = el('ul');
        d.runs.forEach(function (r, i) {
            var li = el('li');
            li.appendChild(el('span', 'what', 'Result ' + r.id.slice(0, 8) + ', run ' + (i + 1) + ', '));
            li.appendChild(OUTCOME[r.outcome] ? el('span', 'what', OUTCOME[r.outcome][0]) : el('span', 'unknown', 'no outcome yet'));
            sub.appendChild(li);
        });
        here.appendChild(sub);
        var sib = el('li', '');
        sib.appendChild(el('span', 'what', (jobsOfBatch().length - 1) + ' sibling workunits in the same batch, listed above'));
        ol.appendChild(sib);
        p.appendChild(ol);
        return p;
    }

    function finder() {
        var f = el('div', 'finder');
        function field(label, node) { var l = el('label', '', label); l.appendChild(node); f.appendChild(l); }
        var t = el('input'); t.type = 'search'; t.placeholder = 'batch reference, job id or result id'; t.value = ui.text;
        t.onkeydown = function (e) { if (e.key === 'Enter') { toast('Would find ' + t.value.trim()); } };
        field('Find', t);
        var go = el('button', 'btn', 'Find');
        go.onclick = function () { toast('Would find ' + t.value.trim()); };
        f.appendChild(go);
        return f;
    }

    function stateNote() {
        var p = el('div', 'panel');
        var map = {
            loading: ['note', 'Reading the job...'],
            empty: ['note', 'No such job. It matches nothing, or your rows do not include it.'],
            error: ['note err', 'The job could not be read: the compute service did not answer.'],
            refused: ['note', 'Your role cannot read jobs.']
        }[screen];
        var n = el('div', map[0], map[1]);
        if (screen === 'error') {
            var r = el('button', 'btn', 'Retry');
            r.style.marginLeft = '12px';
            r.onclick = function () { setScreen('loaded'); };
            n.appendChild(r);
        }
        if (screen === 'empty') {
            var back = el('a', '', 'Back to the batch list');
            back.href = '#';
            back.style.marginLeft = '12px';
            back.onclick = function (e) { e.preventDefault(); toast('Would open the Jobs tab'); };
            n.appendChild(back);
        }
        p.appendChild(n);
        return p;
    }

    function setScreen(s) { screen = s; render(); }
    function setRole(r) { role = r; render(); }
    function setJob(j) { jobKind = j; ui.page = 0; render(); }
    function setOrigin(o) { origin = o; render(); }

    function group(line, label, names, current, onPick) {
        line.appendChild(el('span', 'label', label));
        names.forEach(function (k) {
            var x = el('button', k === current ? 'on' : '', k);
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
        group(l2, 'Job', JOBS, jobKind, setJob);
        l2.appendChild(el('span', 'sep', '|'));
        group(l2, 'Batch origin', ORIGINS, origin, setOrigin);
        b.appendChild(l2);
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
        ['Overview', 'Usage', 'Machines', 'Nodes', 'Heatmap', 'Jobs'].forEach(function (t) { tabs.appendChild(el('span', t === 'Jobs' ? 'on' : '', t)); });
        app.appendChild(tabs);
        if (screen === 'refused') {
            app.appendChild(stateNote());
        } else {
            app.appendChild(finder());
            if (screen !== 'loaded') {
                app.appendChild(stateNote());
            } else {
                var d = job();
                var crumbs = el('div', 'crumbs');
                var back = el('a', '', '← Jobs tab, batch ' + d.b.external_ref.slice(0, 20));
                back.href = '#';
                back.onclick = function (e) { e.preventDefault(); toast('Would open the Jobs tab'); };
                crumbs.appendChild(back);
                app.appendChild(crumbs);
                var left = el('div', 'stack');
                left.appendChild(header(d));
                left.appendChild(runsTable(d));
                left.appendChild(statsPanel(d));
                left.appendChild(waterfall(d));
                left.appendChild(timeline());
                var right = el('div', 'stack');
                right.appendChild(filesPanel(d));
                right.appendChild(releasePanel(d));
                right.appendChild(lineage(d));
                var cols = el('div', 'cols');
                cols.appendChild(left);
                cols.appendChild(right);
                app.appendChild(cols);
            }
        }
        bar();
    }

    render();
}());
