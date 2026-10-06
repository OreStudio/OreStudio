/* Protect my account prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * Ported from
 * projects/ores.web/packages/web/src/prototype/ProtectMyAccountPrototype.tsx.
 * Three structural variants, and the live state the shared VariantBar prints:
 * how many characters the two password fields hold, whether the chosen
 * password meets the policy, and the log of what the prototype recorded.
 *
 * The rows are fixtures, because the browser has no read path for the account,
 * the login record or the sessions today. Every panel says so on the screen. */

(function () {
    'use strict';

    var VARIANTS = {
        a: { id: 'a', name: 'A \u00b7 Password first', gist: 'One column: the password form leads, the sign-in state and the sessions follow.' },
        b: { id: 'b', name: 'B \u00b7 Sessions first', gist: 'The places you are signed in lead, because that is what the member came to check.' },
        c: { id: 'c', name: 'C \u00b7 Two columns', gist: 'Password on the left, state and sessions on the right, with the gaps stated below.' }
    };

    var STATES = [
        ['rest', 'At rest'],
        ['policy', 'Policy met'],
        ['changed', 'Changed']
    ];

    var PASSWORD_POLICY = {
        minLength: 12,
        requireUppercase: true,
        requireLowercase: true,
        requireDigit: true,
        requireSpecial: true,
        specialChars: '!@#$%^&*-_=+'
    };

    var RULE_LABEL = {
        length: 'At least ' + PASSWORD_POLICY.minLength + ' characters',
        upper: 'An uppercase letter (A-Z)',
        lower: 'A lowercase letter (a-z)',
        digit: 'A digit (0-9)',
        special: 'A special character (' + PASSWORD_POLICY.specialChars + ')'
    };

    var ACCOUNT = {
        username: 'amara.okafor',
        fullName: 'Amara Okafor',
        email: 'amara.okafor@acme.example',
        accountType: 'user'
    };

    var LOGIN_STATE = {
        lastSignInAt: '2026-09-30 08:12 UTC',
        lastSignInFrom: '203.0.113.44 \u00b7 United Kingdom',
        failedAttempts: 2,
        locked: false,
        online: true,
        passwordResetRequired: false
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

    var POLICY_SEED = {
        current: 'hunter2',
        chosen: 'Correct-Horse-Battery-9!'
    };

    var S = {
        variant: 'b',
        current: '',
        chosen: '',
        confirm: '',
        changed: false,
        reveal: { 'pw-current': false, 'pw-new': false, 'pw-confirm': false },
        showState: true,
        log: []
    };

    // ------------------------------------------------------------- state

    function passwordRules() {
        var rules = ['length'];
        if (PASSWORD_POLICY.requireUppercase) rules.push('upper');
        if (PASSWORD_POLICY.requireLowercase) rules.push('lower');
        if (PASSWORD_POLICY.requireDigit) rules.push('digit');
        if (PASSWORD_POLICY.requireSpecial) rules.push('special');
        return rules;
    }

    /* The same assessment ui/passwordPolicy.ts makes against the server's record. */
    function assess(password) {
        var met = {};
        if (password.length >= PASSWORD_POLICY.minLength) met.length = true;
        if (PASSWORD_POLICY.requireUppercase && /[A-Z]/.test(password)) met.upper = true;
        if (PASSWORD_POLICY.requireLowercase && /[a-z]/.test(password)) met.lower = true;
        if (PASSWORD_POLICY.requireDigit && /[0-9]/.test(password)) met.digit = true;
        if (PASSWORD_POLICY.requireSpecial && PASSWORD_POLICY.specialChars !== '' &&
            Array.from(password).some(function (ch) { return PASSWORD_POLICY.specialChars.indexOf(ch) >= 0; })) {
            met.special = true;
        }
        var rules = passwordRules();
        var valid = rules.every(function (rule) { return met[rule]; });
        var strength;
        if (password.length === 0) strength = 0;
        else if (valid) strength = password.length >= PASSWORD_POLICY.minLength + 4 ? 4 : 3;
        else strength = rules.filter(function (rule) { return met[rule]; }).length >= Math.ceil(rules.length / 2) ? 2 : 1;
        return { met: met, valid: valid, strength: strength };
    }

    function acceptable() {
        return assess(S.chosen).valid && S.chosen === S.confirm;
    }

    function stateName() {
        if (S.changed) return 'changed';
        if (acceptable()) return 'policy met';
        return 'rest';
    }

    /* The states the source exposes. `changed` is the screen after a change:
       the success notice, the log entry and the panel cleared. The source
       leaves the confirmation field holding its old text after it clears the
       two passwords, so it draws a stale mismatch; the reset clears it. */
    function applyPreset(name) {
        if (name === 'policy') {
            S.current = POLICY_SEED.current;
            S.chosen = POLICY_SEED.chosen;
            S.confirm = POLICY_SEED.chosen;
            S.changed = false;
            S.log = [];
        } else if (name === 'changed') {
            S.current = '';
            S.chosen = '';
            S.confirm = '';
            S.changed = true;
            S.log = ['change password \u00b7 current ' + POLICY_SEED.current.length +
                ' chars \u00b7 new ' + POLICY_SEED.chosen.length + ' chars'];
        } else {
            S.current = '';
            S.chosen = '';
            S.confirm = '';
            S.changed = false;
            S.log = [];
        }
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var v = (p.get('variant') || '').toLowerCase();
        if (VARIANTS[v]) S.variant = v;
        var st = p.get('state');
        for (var i = 0; i < STATES.length; i++) if (STATES[i][0] === st) applyPreset(st);
        if (p.get('current') !== null) S.current = p.get('current');
        if (p.get('new') !== null) S.chosen = p.get('new');
        if (p.get('confirm') !== null) S.confirm = p.get('confirm');
        if (p.get('changed') === '1') S.changed = true;
    }

    function syncUrl() {
        var p = new URLSearchParams();
        p.set('variant', S.variant);
        if (S.changed) p.set('changed', '1');
        try {
            window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
        } catch (err) {
            /* A file:// page may refuse history; the state still holds. */
        }
    }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    // -------------------------------------------------------------- parts

    function passwordInput(id, value) {
        var shown = S.reveal[id] === true;
        return '<div class="pw"><input class="control" id="' + id + '" type="' + (shown ? 'text' : 'password') + '" value="' + esc(value) + '"' +
            (id === 'pw-current' ? ' autocomplete="current-password"' : ' autocomplete="new-password"') +
            '><button type="button" class="toggle" data-act="reveal" data-target="' + id + '" aria-pressed="' + shown + '">' +
            (shown ? 'Hide' : 'Show') + '</button></div>';
    }

    function newPasswordField() {
        var a = assess(S.chosen);
        var mismatch = S.confirm.length > 0 && S.confirm !== S.chosen;
        var bars = [1, 2, 3, 4].map(function (level) {
            var cls = a.strength >= level ? ' f' + (a.strength >= 3 ? 3 : a.strength) : '';
            return '<span class="bar' + cls + '"></span>';
        }).join('');
        var rules = passwordRules().map(function (rule) {
            var met = a.met[rule] === true;
            return '<li class="' + (met ? 'met' : '') + '"><span aria-hidden="true">' +
                (met ? '\u2713' : '\u25cb') + '</span> ' + esc(RULE_LABEL[rule]) + '</li>';
        }).join('');
        var word = ['', 'Weak', 'Fair', 'Good', 'Strong'][a.strength];
        return '<div style="display:grid;gap:0.75rem">' +
            '<div class="field"><label class="flabel" for="pw-new">New password</label>' +
            passwordInput('pw-new', S.chosen) + '</div>' +
            '<div id="pw-rules" aria-live="polite">' +
            '<div class="strength"><div class="bars" aria-hidden="true">' + bars + '</div>' +
            '<span class="word">' + esc(word) + '</span></div>' +
            '<ul class="rules">' + rules + '</ul></div>' +
            '<div class="field"><label class="flabel" for="pw-confirm">Confirm password</label>' +
            passwordInput('pw-confirm', S.confirm) +
            (mismatch ? '<span class="err">The passwords do not match.</span>' : '') + '</div></div>';
    }

    function passwordPanel() {
        var canSubmit = S.current.length > 0 && acceptable();
        return '<section class="card">' +
            '<header class="chead"><h2>Password</h2><p>Change the password you sign in with. ' +
            'The server checks the current password before it changes anything.</p></header>' +
            '<div class="field"><label class="flabel" for="pw-current">Current password</label>' +
            passwordInput('pw-current', S.current) +
            '<span class="hint">Proves the request is yours.</span></div>' +
            newPasswordField() +
            '<div class="footrow"><span class="tiny">POST /api/account/password exists and takes both ' +
            'passwords. This screen calls nothing.</span>' +
            '<span class="end"><button class="btn primary" data-act="change-password"' +
            (canSubmit ? '' : ' disabled') + '>Change password</button></span></div>' +
            (S.changed ? '<div class="notice success">The prototype recorded the change on the state ' +
                'panel below. Nothing was sent.</div>' : '') +
            '</section>';
    }

    function tag(tone, text) {
        return '<span class="tag ' + tone + '">' + esc(text) + '</span>';
    }

    function detail(label, value, mono) {
        return '<div class="detail"><dt>' + esc(label) + '</dt><dd' +
            (mono ? ' class="mono"' : '') + '>' + esc(value) + '</dd></div>';
    }

    function signInStatePanel() {
        var state = LOGIN_STATE;
        return '<section class="card">' +
            '<header class="chead"><h2>Sign-in state</h2><p>Read-only. The server writes this state ' +
            'as a side effect of signing in.</p></header>' +
            '<div style="display:flex;flex-wrap:wrap;gap:0.5rem">' +
            tag(state.locked ? 'warn' : 'neutral', state.locked ? 'Locked' : 'Not locked') +
            tag(state.online ? 'accent' : 'muted', state.online ? 'Signed in' : 'Signed out') +
            tag(state.passwordResetRequired ? 'warn' : 'muted',
                state.passwordResetRequired ? 'Password change required' : 'No change required') +
            '</div>' +
            '<div class="details two">' +
            detail('Last sign-in', state.lastSignInAt) +
            detail('From', state.lastSignInFrom) +
            detail('Failed attempts', String(state.failedAttempts), true) +
            detail('Read path', 'none in the browser') +
            '</div></section>';
    }

    function sessionsPanel() {
        var others = SESSIONS.filter(function (row) { return !row.thisDevice; });
        var rows = SESSIONS.map(function (row) {
            var action = row.thisDevice
                ? tag('accent', 'This device')
                : '<button class="btn secondary sm act" disabled title="No server operation ends one ' +
                  'other session yet: iam.v1.sessions.end does not exist.">End session</button>';
            return '<li class="srow"><span class="client">' + esc(row.client) + '</span>' +
                '<span class="grow"><span class="addr">' + esc(row.address) + '</span>' +
                '<span class="sub">' + esc(row.country) + ' \u00b7 started ' + esc(row.startedAt) +
                ' \u00b7 ' + esc(row.duration) + '</span></span>' +
                '<span class="bytes">' + esc(row.bytesIn) + ' in / ' + esc(row.bytesOut) + ' out</span>' +
                action + '</li>';
        }).join('');
        return '<section class="card">' +
            '<header class="chead"><h2>Where you are signed in</h2>' +
            '<p>One row for each session with no end time.</p></header>' +
            (others.length > 0
                ? '<div class="notice info">' + others.length + ' other sign-in' +
                  (others.length === 1 ? '' : 's') + '. If you do not recognise one, change your password. ' +
                  'This screen cannot end that session: no server operation ends one other session today.</div>'
                : '') +
            '<ul class="srows">' + rows + '</ul>' +
            '<p class="tiny faint" style="font-size:0.75rem">End session is drawn unavailable, not simulated: ' +
            'iam.v1.sessions.end does not exist, and iam.v1.auth.logout ends only this session.</p>' +
            '</section>';
    }

    function gapsPanel() {
        var rows = [
            ['missing', 'End one other session \u2014 candidate ', 'iam.v1.sessions.end', ''],
            ['missing', 'Two-factor enrolment \u2014 candidate ', 'iam.v1.accounts.enrol-totp', ''],
            ['partial', 'Active sessions \u2014 ', 'iam.v1.sessions.active',
             ' replies with success and no rows, and no route serves it in the browser'],
            ['missing', 'Session statistics \u2014 candidate ', 'iam.v1.sessions.statistics', '']
        ].map(function (row) {
            return '<li>' + tag('warn', row[0]) + '<span>' + esc(row[1]) +
                '<span class="mono">' + esc(row[2]) + '</span>' + esc(row[3]) + '</span></li>';
        }).join('');
        return '<section class="card g3">' +
            '<header class="chead"><h2>Not available in this build</h2>' +
            '<p>The operations this journey asks for and the server does not have.</p></header>' +
            '<ul class="gaps">' + rows + '</ul></section>';
    }

    // ----------------------------------------------------------- variants

    function variantA() {
        return '<div class="stack">' + passwordPanel() + signInStatePanel() + sessionsPanel() + '</div>';
    }

    function variantB() {
        return '<div class="stack">' + sessionsPanel() +
            '<div class="cols two">' + passwordPanel() + signInStatePanel() + '</div></div>';
    }

    function variantC() {
        return '<div class="cols left-420">' +
            '<div class="stack">' + passwordPanel() + '</div>' +
            '<div class="stack">' + signInStatePanel() + sessionsPanel() + gapsPanel() + '</div></div>';
    }

    // --------------------------------------------------------------- page

    function render(keepFocus) {
        var active = keepFocus ? document.activeElement : null;
        var activeId = active && active.id ? active.id : null;
        var start = null;
        var end = null;
        if (activeId && active.setSelectionRange) {
            try { start = active.selectionStart; end = active.selectionEnd; } catch (err) { start = null; }
        }

        var body = S.variant === 'a' ? variantA() : (S.variant === 'b' ? variantB() : variantC());
        var strip = '<div class="strip">' +
            '<span>account: <span class="mono">' + esc(ACCOUNT.username) + '</span></span>' +
            '<span>name: <span class="mono">' + esc(ACCOUNT.fullName) + '</span></span>' +
            '<span>email: <span class="mono">' + esc(ACCOUNT.email) + '</span></span>' +
            '<span>type: <span class="mono">' + esc(ACCOUNT.accountType) + '</span></span></div>';
        var head = '<header class="phead"><div><h1>Security</h1>' +
            '<p class="desc">Your password, and the places your account is signed in.</p></div></header>';
        var notice = '<div class="notice warn">PROTOTYPE. Every row below is a fixture. The browser has ' +
            'no read path for the account, the login record or the sessions today, so no panel here ' +
            'reads the server.</div>';

        document.getElementById('app').innerHTML = '<div class="page">' + strip + head +
            '<div class="stack">' + notice + body + '</div></div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 variant ' + S.variant +
            ' \u00b7 member on their own record \u00b7 state ' + stateName();

        renderState();
        renderBar();

        if (activeId) {
            var el = document.getElementById(activeId);
            if (el && el.focus) {
                el.focus();
                if (start !== null && el.setSelectionRange) {
                    try { el.setSelectionRange(start, end); } catch (err) { /* not a text control */ }
                }
            }
        }
    }

    /* The state panel the shared React VariantBar prints. */
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
            '<span>current password: <span class="mono">' + S.current.length + ' chars</span></span>' +
            '<span>new password: <span class="mono">' + S.chosen.length + ' chars</span></span>' +
            '<span>meets the policy: <span class="mono">' + String(acceptable()) + '</span></span>' +
            '</div>' + log;
    }

    function renderBar() {
        var variantButtons = Object.keys(VARIANTS).map(function (k) {
            return '<button data-act="variant" data-variant="' + k + '"' +
                (S.variant === k ? ' class="on"' : '') + '>' + k.toUpperCase() + '</button>';
        }).join('');
        var stateButtons = STATES.map(function (s) {
            var on = (s[0] === 'changed' && S.changed) || (s[0] === 'policy' && !S.changed && acceptable()) ||
                (s[0] === 'rest' && !S.changed && !acceptable());
            return '<button data-act="set-state" data-state="' + s[0] + '"' +
                (on ? ' class="on"' : '') + '>' + esc(s[1]) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">variant <b>' + S.variant.toUpperCase() + '</b> \u2014 ' +
            esc(VARIANTS[S.variant].name.replace(/^[A-Za-z] \u00b7 /, '')) + '</span>' +
            variantButtons +
            '<span class="gist">' + esc(VARIANTS[S.variant].gist) + '</span>' +
            '<span class="sep">|</span><span class="label">state</span>' + stateButtons +
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
        else if (act === 'reveal') {
            var id = el.getAttribute('data-target');
            S.reveal[id] = !S.reveal[id];
            render();
        }
        else if (act === 'change-password') {
            S.log = S.log.concat(['change password \u00b7 current ' + S.current.length +
                ' chars \u00b7 new ' + S.chosen.length + ' chars']);
            S.current = '';
            S.chosen = '';
            S.confirm = '';
            S.changed = true;
            syncUrl();
            render();
        }
    });

    document.addEventListener('input', function (ev) {
        var el = ev.target;
        if (el.id === 'pw-current') S.current = el.value;
        else if (el.id === 'pw-new') S.chosen = el.value;
        else if (el.id === 'pw-confirm') S.confirm = el.value;
        else return;
        render(true);
    });

    readParams();
    render();
})();
