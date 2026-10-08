/* Operations: versions and the database prototype. Self-contained: plain
 * JavaScript, mock data, no framework, no build step, and nothing that outlives
 * the page.
 *
 * Check the versions and the database, from
 * doc/knowledge/journeys/operations/journey_check_the_versions_and_the_database.org.
 *
 * The client comes from the bundle stamp; the server and the database come from
 * the login answer, which must learn to carry the database row. The screen
 * stands for the read that does not exist yet. Nothing here reads the server. */

(function () {
    'use strict';

    var VARIANTS = [
        {
            id: 'carried',
            name: 'Login answer carries it',
            gist: 'The answer that opened the session states the server build and the database row; the screen reads both from it.'
        },
        {
            id: 'unreachable',
            name: 'Deployment silent',
            gist: 'The deployment has not answered, so the server and the database read unknown rather than inventing values.'
        }
    ];

    var GAPS = [
        {
            title: 'The login answer does not carry the database row yet',
            body: 'The row is recorded and one reader touches it: every service compares the fingerprint at its own startup and refuses to start on a mismatch. No operation serves it to a person. The design puts it in the login answer, beside the server build \u2014 the same answer that states what the session is talking to. Until the answer carries it, a real session shows unknown here.'
        },
        {
            title: 'The client and the server strings do not share a shape',
            body: 'The client states a release and a commit; the server adds the platform and the build information in a different composition. The screen shows them side by side; only an eye can compare them.'
        },
        {
            title: 'The per-instance versions are releases only',
            body: 'Each service reports its release string with its heartbeat, so two builds of one release cannot be told apart on the services screen either.'
        }
    ];

    /* The client build stamp is written into the bundle at build time. */
    var clientVersion = { version: 'v0.0.25', commit: 'a1e507d', dirty: false };

    /* The server build string is the login answer's version field. */
    var serverVersion = { version: 'v0.0.25 [x64-linux] (local a1e507d, 2026-10-04)', address: 'https://ores.example.com' };

    /* The database row, as the login answer should carry it. */
    var databaseState = { fingerprint: '1109eccab21e8fe8', environment: 'development', commit: 'a1e507d', created: '2026-10-04 14:02' };

    var S = { variant: 'carried', state: 'rest', panel: true };

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function variantById(id) {
        return VARIANTS.filter(function (v) { return v.id === id; })[0];
    }

    function activeVariant() { return variantById(S.variant) || VARIANTS[0]; }

    function serverKnown() { return S.variant === 'carried'; }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (variantById(p.get('variant'))) S.variant = p.get('variant');
        if (variantById(p.get('state'))) S.variant = p.get('state');
    }

    function writeParams() {
        try {
            var p = new URLSearchParams();
            p.set('variant', S.variant);
            p.set('state', S.variant);
            window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
        } catch (err) {
            /* A page opened from the file system may refuse to rewrite its
               address; the screen still works without it. */
        }
    }

    // ------------------------------------------------------------- parts

    function detail(label, value, mono) {
        return '<div class="detail"><span class="k">' + esc(label) + '</span>' +
            '<span class="v' + (mono ? ' mono' : '') + '">' + esc(value) + '</span></div>';
    }

    function header() {
        return '<header class="pageheader"><div>' +
            '<h1>Operations: versions and the database</h1>' +
            '<p class="sub">What this browser runs, what the deployment runs, and what the deployment stores.</p>' +
            '</div><div class="head-actions">' +
            '<a class="btn secondary" href="../index.html">Back to prototypes</a>' +
            '</div></header>';
    }

    function prototypeNotice() {
        return '<div class="notice warn">PROTOTYPE. The client and the server panels carry the real ' +
            'build shapes; the database panel shows the row the login answer must learn to carry. ' +
            'Nothing on this page reads the server.</div>';
    }

    function clientPanel() {
        return '<section class="card">' +
            '<header><h2>Client</h2><span class="meta">what this browser runs</span></header>' +
            '<div class="details c3">' +
            detail('Version', clientVersion.version, true) +
            detail('Commit', clientVersion.commit, true) +
            detail('Checkout', clientVersion.dirty ? 'Uncommitted changes' : 'Clean', false) +
            '</div>' +
            '<p class="note">Stamped into the bundle when it was built; it cannot change while the ' +
            'tab is open.</p></section>';
    }

    function serverPanel() {
        var known = serverKnown();
        var body = known
            ? '<div class="details c2">' +
              detail('Version', serverVersion.version, true) +
              detail('Address', serverVersion.address, true) +
              '</div>'
            : '<div class="details c2">' +
              detail('Version', 'unknown', false) +
              detail('Address', serverVersion.address, true) +
              '<p class="span2">The deployment has not answered, so its build is unknown. The ' +
              'screen says unknown rather than inventing a version.</p>' +
              '</div>';
        return '<section class="card">' +
            '<header><h2>Server</h2><span class="meta">what the deployment runs</span></header>' +
            body +
            '<p class="note">In a real session the login answer states this build in full, and the ' +
            'session keeps it; the footer reads the same value. The prototype shell has no session, ' +
            'so its footer shows a placeholder.</p></section>';
    }

    function databasePanel() {
        var known = serverKnown();
        return '<section class="card">' +
            '<header><h2>Database</h2><span class="meta">what the deployment stores</span></header>' +
            '<div class="details c4">' +
            detail('Fingerprint', known ? databaseState.fingerprint : 'unknown', known) +
            detail('Environment', known ? databaseState.environment : 'unknown', false) +
            detail('Commit', known ? databaseState.commit : 'unknown', known) +
            detail('Created', known ? databaseState.created : 'unknown', false) +
            '</div>' +
            '<div class="tagrow">' +
            '<span class="tag warn">not carried yet</span>' +
            '<span class="note">The login answer must state these four fields beside the server ' +
            'build; it does not today, so a real session reads unknown here.</span>' +
            '</div></section>';
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
        return clientPanel() + serverPanel() + databasePanel() + gapPanel();
    }

    // -------------------------------------------------------------- page

    function render() {
        document.getElementById('app').innerHTML =
            '<div class="page">' + header() + prototypeNotice() + body() + '</div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 variant ' + S.variant +
            ' \u00b7 signed in as any person; on every screen the footer states the client and the server';

        renderState();
        renderBar();
    }

    function renderState() {
        var active = activeVariant();
        var known = serverKnown();
        document.getElementById('proto-state').hidden = !S.panel;
        document.getElementById('proto-state').innerHTML =
            '<p class="gist">Variant <b>' + esc(active.name) + '</b> \u2014 ' + esc(active.gist) + '</p>' +
            '<div class="grid">' +
            '<span>fixture: <span class="v">' + esc(S.variant) + '</span></span>' +
            '<span>client from: <span class="v">the bundle stamp</span></span>' +
            '<span>server from: <span class="v">' + (known ? 'the login answer' : 'no answer') + '</span></span>' +
            '<span>database from: <span class="v">' + (known ? 'the login answer (target)' : 'no answer') + '</span></span>' +
            '</div>' +
            '<p class="note">Signed in as any person; on every screen the footer states the client ' +
            'and the server.</p>';
    }

    function renderBar() {
        var variantButtons = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + v.id + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.name) + '</button>';
        }).join('');
        var stateButtons = VARIANTS.map(function (v) {
            return '<button data-act="state" data-state="' + v.id + '"' +
                (S.variant === v.id ? ' class="on"' : '') + '>' + esc(v.id) + '</button>';
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
        ev.preventDefault();
        if (act === 'variant') S.variant = el.getAttribute('data-variant');
        else if (act === 'state') S.variant = el.getAttribute('data-state');
        else if (act === 'panel') S.panel = !S.panel;
        writeParams();
        render();
    });

    readParams();
    render();
})();
