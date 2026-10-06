/* Rescue access prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * Ported from
 * projects/ores.web/packages/web/src/prototype/RescueAccessPrototype.tsx.
 * Three structural variants, and the live state the shared VariantBar prints:
 * which account the roster selected, whether the recovery link was sent, and
 * how the lock control is drawn.
 *
 * The rows are fixtures, because the browser has no read path for the account
 * or its login record, and no route reaches the reset, lock or unlock
 * subjects. Every panel says so on the screen. */

(function () {
    'use strict';

    var VARIANTS = {
        a: { id: 'a', name: 'A \u00b7 One decision screen', gist: 'The account, its state and every action on one page, stacked in the order the call goes.' },
        b: { id: 'b', name: 'B \u00b7 Diagnose, then act', gist: 'The state leads and names the action it suggests; the actions sit under it.' },
        c: { id: 'c', name: 'C \u00b7 Roster and panel', gist: 'The account list stays in view on the left, because the administrator arrives from it.' }
    };

    var STATES = [
        ['rest', 'At rest'],
        ['sent', 'Link sent']
    ];

    var ACCOUNT = {
        username: 'amara.okafor',
        fullName: 'Amara Okafor',
        email: 'amara.okafor@acme.example',
        accountType: 'user'
    };

    var RESCUED_ACCOUNT = {
        username: 'jonas.lindqvist',
        fullName: 'Jonas Lindqvist',
        email: 'jonas.lindqvist@acme.example',
        accountType: 'user'
    };

    var RESCUED_LOGIN_STATE = {
        lastSignInAt: '2026-09-29 21:03 UTC',
        lastSignInFrom: '198.51.100.7 \u00b7 Germany',
        failedAttempts: 7,
        locked: true,
        online: true,
        passwordResetRequired: false
    };

    var ROSTER = [
        { username: 'amara.okafor', fullName: 'Amara Okafor', state: 'active' },
        { username: 'jonas.lindqvist', fullName: 'Jonas Lindqvist', state: 'locked' },
        { username: 'priya.raman', fullName: 'Priya Raman', state: 'active' },
        { username: 'tomas.novak', fullName: 'Tomas Novak', state: 'reset required' }
    ];

    var SENT_AT = '2026-09-30 12:04 UTC';

    var S = {
        variant: 'b',
        sentAt: null,
        nextState: 'locked',
        selected: RESCUED_ACCOUNT.username,
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

    function applyPreset(name) {
        if (name === 'sent') {
            S.sentAt = SENT_AT;
            S.log = S.log.concat(['send recovery link \u00b7 ' + RESCUED_ACCOUNT.email]);
        } else {
            S.sentAt = null;
            S.log = [];
        }
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var v = (p.get('variant') || '').toLowerCase();
        if (VARIANTS[v]) S.variant = v;
        var account = p.get('account');
        if (ROSTER.some(function (row) { return row.username === account; })) S.selected = account;
        var st = p.get('state');
        for (var i = 0; i < STATES.length; i++) if (STATES[i][0] === st) applyPreset(st);
        if (p.get('lock') === 'locked' || p.get('lock') === 'unlocked') S.nextState = p.get('lock');
    }

    function syncUrl() {
        var p = new URLSearchParams();
        p.set('variant', S.variant);
        p.set('account', S.selected);
        if (S.sentAt !== null) p.set('sent', '1');
        p.set('lock', S.nextState);
        try {
            window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
        } catch (err) {
            /* A file:// page may refuse history; the state still holds. */
        }
    }

    // -------------------------------------------------------------- parts

    function header() {
        var a = RESCUED_ACCOUNT;
        var st = RESCUED_LOGIN_STATE;
        return '<section class="card">' +
            '<header class="chead" style="display:flex;flex-wrap:wrap;align-items:flex-start;' +
            'justify-content:space-between;gap:0.75rem">' +
            '<div><h2>' + esc(a.fullName) + '</h2>' +
            '<p class="mono" style="font-size:0.875rem;color:var(--ink-dim)">' + esc(a.username) +
            ' \u00b7 ' + esc(a.email) + ' \u00b7 ' + esc(a.accountType) + '</p></div>' +
            tag('warn', 'Locked') + '</header>' +
            '<div class="details three">' +
            detail('Last sign-in', st.lastSignInAt) +
            detail('From', st.lastSignInFrom) +
            detail('Failed attempts', String(st.failedAttempts), true) +
            '</div></section>';
    }

    function recoveryPanel() {
        return '<section class="card">' +
            '<header class="chead"><h2>Send a recovery link</h2>' +
            '<p>The administrator does not choose the colleague\u2019s password. The system emails a ' +
            'single-use link to the address on the account, and the colleague sets a password that ' +
            'only they know.</p></header>' +
            '<div class="field"><label class="flabel" for="recovery-to">Send it to</label>' +
            '<input class="control" id="recovery-to" readonly value="' + esc(RESCUED_ACCOUNT.email) + '">' +
            '<span class="hint">The address on the account. It is not edited here.</span></div>' +
            '<div class="footrow"><span class="tiny">Nothing sends mail in this tree, no subject ' +
            'requests a reset, and no table holds a token. This screen sends nothing.</span>' +
            '<span class="end"><button class="btn primary" data-act="send-link">Send the recovery link</button></span></div>' +
            (S.sentAt !== null
                ? '<div class="notice success">The prototype recorded a link sent at ' + esc(S.sentAt) +
                  '. Nothing left the browser.</div>'
                : '') +
            '<details class="fallback"><summary>Set a password here instead (fallback)</summary>' +
            '<p>Kept for an account whose mailbox cannot receive. The administrator then knows the ' +
            'password, so the record has to say so. <span class="mono">iam.v1.accounts.reset-password</span> ' +
            'exists and no route reaches it. Whether this fallback survives is the open question this ' +
            'prototype raises.</p></details>' +
            '</section>';
    }

    function statePanel() {
        var seg = ['unlocked', 'locked'].map(function (option) {
            return '<button data-act="lock" data-lock="' + option + '"' +
                (S.nextState === option ? ' class="on"' : '') + '>' + option + '</button>';
        }).join('');
        return '<section class="card">' +
            '<header class="chead"><h2>Lock the account</h2>' +
            '<p>A locked account cannot sign in. Unlocking clears the failed attempt count.</p></header>' +
            '<div style="display:flex;flex-wrap:wrap;align-items:center;gap:0.5rem">' +
            '<div class="seg">' + seg + '</div>' +
            '<span style="font-size:0.75rem;color:var(--ink-faint)">The subject exists and needs ' +
            '<span class="mono">iam::accounts:lock</span>; no BFF route reaches it.</span></div>' +
            '<div class="notice info">A lock leaves open sessions open. Nothing ends them today, so ' +
            'the colleague\u2019s existing session keeps working until it expires.</div>' +
            '</section>';
    }

    function gapsPanel() {
        var rows = [
            ['missing', 'Send a recovery link \u2014 no subject requests a reset, no table holds a ' +
                'token, and nothing in the tree sends mail', ''],
            ['missing', 'Self-service recovery \u2014 the same machinery, started by the member who ' +
                'cannot sign in. Candidates ', 'iam.v1.accounts.request-password-reset', ' and complete-password-reset'],
            ['missing', 'Activate or deactivate an account \u2014 no active flag exists', ''],
            ['missing', 'End the sessions a lock leaves open \u2014 candidate ', 'iam.v1.sessions.end', '']
        ].map(function (row) {
            return '<li>' + tag('warn', row[0]) + '<span>' + esc(row[1]) +
                (row[2] ? '<span class="mono">' + esc(row[2]) + '</span>' + esc(row[3]) : '') + '</span></li>';
        }).join('');
        return '<section class="card g3">' +
            '<header class="chead"><h2>Not available in this build</h2>' +
            '<p>What this journey asks for and the server does not have.</p></header>' +
            '<ul class="gaps">' + rows + '</ul></section>';
    }

    function recommendation() {
        var st = RESCUED_LOGIN_STATE;
        var text = st.locked
            ? st.failedAttempts + ' failed attempts locked this account. Unlock it if the colleague ' +
              'simply forgot the password, or send a recovery link if the attempts were not theirs.'
            : 'The account is not locked. Send a recovery link if the colleague has forgotten the password.';
        return '<section class="card g2">' +
            '<h2 style="font-size:1.125rem;font-weight:500">What the state suggests</h2>' +
            '<p style="font-size:0.875rem;color:var(--ink-dim)">' + esc(text) + '</p>' +
            '<div style="display:flex;flex-wrap:wrap;gap:0.5rem;padding-top:0.25rem">' +
            tag('warn', st.failedAttempts + ' failed attempts') +
            tag('neutral', st.locked ? 'Locked' : 'Not locked') +
            tag(st.online ? 'accent' : 'muted', st.online ? 'Session open' : 'No session') +
            '</div></section>';
    }

    function roster() {
        var items = ROSTER.map(function (row) {
            return '<li><button data-act="pick" data-account="' + esc(row.username) + '"' +
                (row.username === S.selected ? ' class="on"' : '') + '>' +
                '<span class="nm">' + esc(row.fullName) + '</span>' +
                '<span class="un">' + esc(row.username) + '</span>' +
                '<span class="st">' + esc(row.state) + '</span></button></li>';
        }).join('');
        return '<section class="card p4 hfit g2">' +
            '<h2 style="padding:0 0.5rem;font-size:0.875rem;font-weight:500;color:var(--ink-dim)">Accounts</h2>' +
            '<ul class="roster">' + items + '</ul>' +
            '<p style="padding:0 0.5rem;font-size:0.75rem;color:var(--ink-faint)">The account list has no ' +
            'read path in the browser today: no route serves <span class="mono">iam.v1.accounts.list</span>.</p>' +
            '</section>';
    }

    // ----------------------------------------------------------- variants

    function variantA() {
        return '<div class="stack">' + header() + recoveryPanel() + statePanel() + gapsPanel() + '</div>';
    }

    function variantB() {
        return '<div class="stack">' + header() + recommendation() +
            '<div class="cols two">' + recoveryPanel() + statePanel() + '</div>' +
            gapsPanel() + '</div>';
    }

    function variantC() {
        return '<div class="cols left-280">' + roster() +
            '<div class="stack">' + header() + recoveryPanel() + statePanel() + gapsPanel() + '</div></div>';
    }

    // --------------------------------------------------------------- page

    function render() {
        var body = S.variant === 'a' ? variantA() : (S.variant === 'b' ? variantB() : variantC());
        var head = '<header class="phead"><div><h1>Rescue access</h1>' +
            '<p class="desc">Get one colleague back into the system, or shut the account down.</p></div></header>';
        var notice = '<div class="notice warn">PROTOTYPE. Every row below is a fixture. The browser has ' +
            'no read path for the account or its login record, and no route reaches the reset, lock or ' +
            'unlock subjects, so no panel here reads or writes the server.</div>';

        document.getElementById('app').innerHTML = '<div class="page wide">' + head +
            '<div class="stack">' + notice + body + '</div></div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 variant ' + S.variant +
            ' \u00b7 tenant administrator on a colleague \u00b7 state ' +
            (S.sentAt !== null ? 'link sent' : 'rest') + ' \u00b7 lock drawn as ' + S.nextState;

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
            '<span>selected account: <span class="mono">' + esc(S.selected) + '</span></span>' +
            '<span>recovery link sent: <span class="mono">' + esc(S.sentAt === null ? 'no' : S.sentAt) + '</span></span>' +
            '<span>lock control drawn as: <span class="mono">' + esc(S.nextState) + '</span></span>' +
            '</div>' +
            '<p class="faint" style="margin-top:0.5rem">Signed in as <span class="mono">' +
            esc(ACCOUNT.username) + '</span>, tenant administrator, tenant Acme Corporation.</p>' + log;
    }

    function renderBar() {
        var variantButtons = Object.keys(VARIANTS).map(function (k) {
            return '<button data-act="variant" data-variant="' + k + '"' +
                (S.variant === k ? ' class="on"' : '') + '>' + k.toUpperCase() + '</button>';
        }).join('');
        var stateButtons = STATES.map(function (s) {
            var on = s[0] === 'sent' ? S.sentAt !== null : S.sentAt === null;
            return '<button data-act="set-state" data-state="' + s[0] + '"' +
                (on ? ' class="on"' : '') + '>' + esc(s[1]) + '</button>';
        }).join('');
        var lockButtons = ['locked', 'unlocked'].map(function (option) {
            return '<button data-act="set-lock" data-lock="' + option + '"' +
                (S.nextState === option ? ' class="on"' : '') + '>' + option + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">variant <b>' + S.variant.toUpperCase() + '</b> \u2014 ' +
            esc(VARIANTS[S.variant].name.replace(/^[A-Za-z] \u00b7 /, '')) + '</span>' +
            variantButtons +
            '<span class="gist">' + esc(VARIANTS[S.variant].gist) + '</span>' +
            '<span class="sep">|</span><span class="label">state</span>' + stateButtons +
            '<span class="sep">|</span><span class="label">lock</span>' + lockButtons +
            '<span class="sep">|</span>' +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') + '</button>';
    }

    // --------------------------------------------------------------- events

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'variant') { S.variant = el.getAttribute('data-variant'); syncUrl(); render(); }
        else if (act === 'set-state') { applyPreset(el.getAttribute('data-state')); syncUrl(); render(); }
        else if (act === 'toggle-state') { S.showState = !S.showState; render(); }
        else if (act === 'set-lock' || act === 'lock') {
            S.nextState = el.getAttribute('data-lock');
            S.log = S.log.concat(['lock state drawn as ' + S.nextState + ' \u00b7 nothing sent']);
            syncUrl();
            render();
        }
        else if (act === 'pick') { S.selected = el.getAttribute('data-account'); syncUrl(); render(); }
        else if (act === 'send-link') {
            S.sentAt = SENT_AT;
            S.log = S.log.concat(['send recovery link \u00b7 ' + RESCUED_ACCOUNT.email]);
            syncUrl();
            render();
        }
    });

    readParams();
    render();
})();
