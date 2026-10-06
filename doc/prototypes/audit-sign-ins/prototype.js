/* Audit sign-ins prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * Ported from
 * projects/ores.web/packages/web/src/prototype/AuditSignInsPrototype.tsx. The
 * screen is an event log: no versions, no diff, no revert, and Refresh is its
 * only action. Three structural variants, a tab, three filters, and the
 * expanded session, all of them reachable from the query string.
 *
 * The rows are fixtures, because the browser has no read path for the sessions,
 * the login records or the auth events, and nothing serves the session samples
 * on either transport. Every panel says so on the screen. */

(function () {
    'use strict';

    var VARIANTS = {
        a: { id: 'a', name: 'A \u00b7 One log, sections', gist: 'One filter bar, then the active sessions, the activity and the failures as sections of one page.' },
        b: { id: 'b', name: 'B \u00b7 Tabs over one filter bar', gist: 'The three readings share one filter bar and take turns in the body.' },
        c: { id: 'c', name: 'C \u00b7 Session-first timeline', gist: 'One row per session, expanded to its activity, with the failures in a rail beside it.' }
    };

    var TABS = ['sessions', 'activity', 'failures'];
    var PERIODS = ['last hour', 'last 24 hours', 'last 7 days'];
    var EVENTS = ['any event', 'login', 'login failed', 'logout', 'token refresh'];
    var REFRESHED_AT = '2026-09-30 12:04 UTC';

    var ACCOUNT = {
        username: 'amara.okafor',
        fullName: 'Amara Okafor',
        email: 'amara.okafor@acme.example',
        accountType: 'user'
    };

    var SESSIONS = [
        { id: '5F1B0A2C-9C34-4C7E-9A11-8E4B7D2F6A01', client: 'ores.web', address: '203.0.113.44',
          country: 'United Kingdom', startedAt: '2026-09-30 08:12 UTC', duration: '3h 41m',
          bytesIn: '18.4 MB', bytesOut: '2.1 MB', thisDevice: true },
        { id: 'A7C4E1D8-2B69-4F03-8D52-1C9A6E3B7F42', client: 'ores.shell', address: '198.51.100.7',
          country: 'Germany', startedAt: '2026-09-29 21:03 UTC', duration: '14h 50m',
          bytesIn: '1.2 MB', bytesOut: '340 KB', thisDevice: false },
        { id: 'D2E8B5A9-4C17-4A6B-B3F8-7E5D0C2A9B63', client: 'ores.web', address: '192.0.2.19',
          country: 'Netherlands', startedAt: '2026-09-28 06:40 UTC', duration: '2d 5h',
          bytesIn: '44.9 MB', bytesOut: '6.8 MB', thisDevice: false }
    ];

    var FAILURES = [
        { account: 'jonas.lindqvist', failedAttempts: 7, lastAddress: '198.51.100.7', locked: true },
        { account: 'amara.okafor', failedAttempts: 2, lastAddress: '203.0.113.44', locked: false },
        { account: 'tomas.novak', failedAttempts: 1, lastAddress: '192.0.2.90', locked: false }
    ];

    var S = {
        variant: 'b',
        selectedTab: 'sessions',
        refreshedAt: REFRESHED_AT,
        period: 'last 24 hours',
        event: 'any event',
        openSession: SESSIONS[0].id,
        showState: true,
        log: []
    };

    // ------------------------------------------------------------- state

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function tag(tone, text) {
        return '<span class="tag ' + tone + '">' + esc(text) + '</span>';
    }

    function detail(label, value, mono) {
        return '<div class="detail"><dt>' + esc(label) + '</dt><dd' +
            (mono ? ' class="mono"' : '') + '>' + esc(value) + '</dd></div>';
    }

    function rowById(id) {
        return SESSIONS.filter(function (row) { return row.id === id; })[0];
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var v = (p.get('variant') || '').toLowerCase();
        if (VARIANTS[v]) S.variant = v;
        if (TABS.indexOf(p.get('tab')) >= 0) S.selectedTab = p.get('tab');
        if (PERIODS.indexOf(p.get('period')) >= 0) S.period = p.get('period');
        if (EVENTS.indexOf(p.get('event')) >= 0) S.event = p.get('event');
        if (rowById(p.get('session'))) S.openSession = p.get('session');
        if (p.get('refreshed')) S.refreshedAt = p.get('refreshed');
    }

    function syncUrl() {
        var p = new URLSearchParams();
        p.set('variant', S.variant);
        p.set('tab', S.selectedTab);
        p.set('period', S.period);
        p.set('event', S.event);
        p.set('session', S.openSession);
        if (S.refreshedAt !== REFRESHED_AT) p.set('refreshed', S.refreshedAt);
        try {
            window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
        } catch (err) {
            /* A file:// page may refuse history; the state still holds. */
        }
    }

    // -------------------------------------------------------------- parts

    function header() {
        return '<header class="phead"><div><h1>Audit: sign-ins</h1>' +
            '<p class="desc">Who is signed in, what they are doing, and who is failing to get in.</p></div>' +
            '<div class="actions"><span class="readat">Read at ' + esc(S.refreshedAt) + '</span>' +
            '<button class="btn secondary" data-act="refresh">Refresh</button></div></header>';
    }

    function filters() {
        var periods = PERIODS.map(function (option) {
            return '<option' + (S.period === option ? ' selected' : '') + '>' + esc(option) + '</option>';
        }).join('');
        var events = EVENTS.map(function (option) {
            return '<option' + (S.event === option ? ' selected' : '') + '>' + esc(option) + '</option>';
        }).join('');
        return '<section class="card p4 filters">' +
            '<label>Account<select class="control" disabled>' +
            '<option>Every account (the server cannot filter by account yet)</option></select></label>' +
            '<label>Period<select class="control" id="f-period">' + periods + '</select></label>' +
            '<label>Event<select class="control" id="f-event">' + events + '</select></label>' +
            '<p class="why">No version, diff or revert control appears on this screen: it is an event ' +
            'log, not a versioned entity.</p></section>';
    }

    function sessionsPanel() {
        var rows = SESSIONS.map(function (row) {
            return '<li class="srow"><span class="client w7">' + esc(row.client) + '</span>' +
                '<span class="grow"><span class="addr">' + esc(row.address) + '</span>' +
                '<span class="sub">' + esc(row.country) + ' \u00b7 started ' + esc(row.startedAt) +
                ' \u00b7 ' + esc(row.duration) + '</span></span>' +
                '<span class="bytes">' + esc(row.bytesIn) + ' in / ' + esc(row.bytesOut) + ' out</span>' +
                '<button class="btn secondary sm act" disabled title="Ending another account\u2019s session ' +
                'has no subject yet: iam.v1.sessions.end does not exist.">End session</button></li>';
        }).join('');
        return '<section class="card">' +
            '<header class="chead" style="display:flex;flex-wrap:wrap;align-items:baseline;' +
            'justify-content:space-between;gap:0.5rem"><h2>Active sessions</h2>' +
            '<span style="font-size:0.75rem;color:var(--ink-faint)">' + SESSIONS.length + ' open</span></header>' +
            '<ul class="srows">' + rows + '</ul>' +
            '<p class="faint" style="font-size:0.75rem">End session is drawn unavailable: the repository ' +
            'writes an end time for the caller\u2019s own logout only, and iam.v1.sessions.delete needs ' +
            'the iam::* wildcard.</p></section>';
    }

    function activityPanel() {
        var row = rowById(S.openSession) || SESSIONS[0];
        var body = row === undefined
            ? '<div class="notice warn">No session is selected.</div>'
            : '<div class="details three">' +
              detail('Session', row.client, true) +
              detail('Address', row.address, true) +
              detail('Started', row.startedAt) + '</div>' +
              '<div class="notice warn">Not available. Nothing serves the samples: ' +
              'iam.v1.sessions.samples replies with success and no rows, and no route serves them in ' +
              'the browser either. The totals above are the session row\u2019s own counters.</div>';
        return '<section class="card">' +
            '<header class="chead"><h2>Session activity</h2>' +
            '<p>How the byte totals moved while the session was open.</p></header>' + body + '</section>';
    }

    function failuresPanel() {
        var rows = FAILURES.map(function (row) {
            return '<tr><td>' + esc(row.account) + '</td><td>' + esc(row.failedAttempts) + '</td>' +
                '<td>' + esc(row.lastAddress) + '</td>' +
                '<td class="tagname">' + tag(row.locked ? 'warn' : 'muted', row.locked ? 'Locked' : 'Not locked') +
                '</td></tr>';
        }).join('');
        return '<section class="card">' +
            '<header class="chead"><h2>Failed attempts</h2>' +
            '<p>A locked account explains a support call; a burst explains an incident.</p></header>' +
            '<table class="grid"><thead><tr><th>Account</th><th>Failed</th><th>Last address</th>' +
            '<th>State</th></tr></thead><tbody>' + rows + '</tbody></table>' +
            '<p class="faint" style="font-size:0.75rem">iam.v1.login_info.list exists and no route ' +
            'serves it, so this panel cannot read the table today.</p></section>';
    }

    function gapsPanel() {
        var rows = [
            ['missing', 'The authentication events \u2014 ', 'ores_iam_auth_events_tbl',
             ' holds them; candidate iam.v1.auth_events.list'],
            ['missing', 'The session statistics \u2014 three continuous aggregates exist; candidate ',
             'iam.v1.sessions.statistics', ''],
            ['missing', 'Ending another account\u2019s session \u2014 candidate ',
             'iam.v1.sessions.end', ''],
            ['missing', 'Every read on this screen \u2014 no route serves the sessions, the samples or ' +
             'the login records', '', '']
        ].map(function (row) {
            return '<li>' + tag('warn', row[0]) + '<span>' + esc(row[1]) +
                (row[2] ? '<span class="mono">' + esc(row[2]) + '</span>' + esc(row[3]) : '') + '</span></li>';
        }).join('');
        return '<section class="card g3">' +
            '<header class="chead"><h2>Not available in this build</h2>' +
            '<p>The readings this journey asks for and the server does not serve.</p></header>' +
            '<ul class="gaps">' + rows + '</ul></section>';
    }

    function sessionTimeline() {
        var rows = SESSIONS.map(function (row) {
            var expanded = row.id === S.openSession;
            return '<div class="tlitem"><button class="tlhead" data-act="open-session" data-session="' +
                esc(row.id) + '"><span class="client">' + esc(row.client) + '</span>' +
                '<span class="grow"><span class="addr">' + esc(row.address) + '</span>' +
                '<span class="sub">' + esc(row.country) + ' \u00b7 ' + esc(row.startedAt) + ' \u00b7 ' +
                esc(row.duration) + '</span></span>' +
                '<span class="tlopen">' + (expanded ? 'Hide' : 'Activity') + '</span></button>' +
                (expanded ? '<div class="tldetail">' + esc(row.bytesIn) + ' in and ' + esc(row.bytesOut) +
                    ' out over ' + esc(row.duration) + '. The samples that moved those totals have no ' +
                    'read path: nothing serves iam.v1.sessions.samples.</div>' : '') + '</div>';
        }).join('');
        return '<section class="card tl">' +
            '<h2 style="font-size:1.125rem;font-weight:500">Sessions and their activity</h2>' +
            rows + '</section>';
    }

    function tabs() {
        return '<div class="tabs">' + TABS.map(function (tab) {
            return '<button data-act="tab" data-tab="' + tab + '"' +
                (S.selectedTab === tab ? ' class="on"' : '') + '>' + esc(tab) + '</button>';
        }).join('') + '</div>';
    }

    // ----------------------------------------------------------- variants

    function variantA() {
        return '<div class="stack">' + filters() + sessionsPanel() + activityPanel() +
            failuresPanel() + gapsPanel() + '</div>';
    }

    function variantB() {
        var panel = S.selectedTab === 'sessions' ? sessionsPanel()
            : (S.selectedTab === 'activity' ? activityPanel() : failuresPanel());
        return '<div class="stack">' + filters() + tabs() + panel + gapsPanel() + '</div>';
    }

    function variantC() {
        return '<div class="stack">' + filters() +
            '<div class="cols right-320">' + sessionTimeline() +
            '<div class="stack">' + failuresPanel() + gapsPanel() + '</div></div></div>';
    }

    // --------------------------------------------------------------- page

    function render() {
        var body = S.variant === 'a' ? variantA() : (S.variant === 'b' ? variantB() : variantC());
        var notice = '<div class="notice warn">PROTOTYPE. Every row below is a fixture. The browser has ' +
            'no read path for the sessions, the login records or the auth events, and nothing serves the ' +
            'session samples, so no panel here reads the server.</div>';

        document.getElementById('app').innerHTML = '<div class="page wide">' + header() +
            '<div class="stack">' + notice + body + '</div></div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 variant ' + S.variant +
            ' \u00b7 tenant administrator \u00b7 tab ' + S.selectedTab +
            ' \u00b7 period ' + S.period + ' \u00b7 event ' + S.event;

        renderState();
        renderBar();
    }

    function renderState() {
        var panel = document.getElementById('proto-state');
        panel.hidden = !S.showState;
        if (!S.showState) return;
        var log = S.log.length === 0
            ? '<p class="none">No action yet.</p>'
            : '<ol class="log">' + S.log.map(function (entry, index) {
                return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
            }).join('') + '</ol>';
        panel.innerHTML = '<div class="facts">' +
            '<span>variant: <span class="mono">' + esc(S.variant) + '</span></span>' +
            '<span>tab: <span class="mono">' + esc(S.selectedTab) + '</span></span>' +
            '<span>period: <span class="mono">' + esc(S.period) + '</span></span>' +
            '<span>event: <span class="mono">' + esc(S.event) + '</span></span>' +
            '<span>read at: <span class="mono">' + esc(S.refreshedAt) + '</span></span>' +
            '</div>' +
            '<p class="faint" style="margin-top:0.5rem">Signed in as <span class="mono">' +
            esc(ACCOUNT.username) + '</span>, tenant administrator, tenant Acme Corporation.</p>' + log;
    }

    function renderBar() {
        var variantButtons = Object.keys(VARIANTS).map(function (k) {
            return '<button data-act="variant" data-variant="' + k + '"' +
                (S.variant === k ? ' class="on"' : '') + '>' + k.toUpperCase() + '</button>';
        }).join('');
        var tabButtons = TABS.map(function (tab) {
            return '<button data-act="tab" data-tab="' + tab + '"' +
                (S.selectedTab === tab ? ' class="on"' : '') + '>' +
                tab.charAt(0).toUpperCase() + tab.slice(1) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">variant <b>' + S.variant.toUpperCase() + '</b> \u2014 ' +
            esc(VARIANTS[S.variant].name.replace(/^[A-Za-z] \u00b7 /, '')) + '</span>' +
            variantButtons +
            '<span class="gist">' + esc(VARIANTS[S.variant].gist) + '</span>' +
            '<span class="sep">|</span><span class="label">tab</span>' + tabButtons +
            '<span class="sep">|</span>' +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') + '</button>';
    }

    // --------------------------------------------------------------- events

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'variant') { S.variant = el.getAttribute('data-variant'); syncUrl(); render(); }
        else if (act === 'tab') { S.selectedTab = el.getAttribute('data-tab'); syncUrl(); render(); }
        else if (act === 'toggle-state') { S.showState = !S.showState; render(); }
        else if (act === 'open-session') { S.openSession = el.getAttribute('data-session'); syncUrl(); render(); }
        else if (act === 'refresh') {
            var stamp = '2026-09-30 12:' + String(4 + S.log.length).padStart(2, '0') + ' UTC';
            S.refreshedAt = stamp;
            S.log = S.log.concat(['refresh \u00b7 re-read every subject at ' + stamp]);
            syncUrl();
            render();
        }
    });

    document.addEventListener('change', function (ev) {
        var el = ev.target;
        if (el.id === 'f-period') S.period = el.value;
        else if (el.id === 'f-event') S.event = el.value;
        else return;
        syncUrl();
        renderState();
    });

    readParams();
    render();
})();
