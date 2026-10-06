/* First run journey prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * The accepted first run journey: a welcome screen, then one flat rail of nine
 * steps -- the system administrator, the new tenant steps inline, the tenant
 * administrator's first sign-in and Ready. Every step, state and piece of mock
 * data is the one the React prototype holds
 * (pages/prototype/newTenantJourney/NewTenantJourneyPrototype.tsx, journey.tsx,
 * parts.tsx, stub.ts). Nothing is invented; the provisioning run is simulated
 * exactly as useSimulatedRun simulates it, one step every 900 ms.
 *
 * States are chosen from the query string, as the navigation prototype does:
 *   ?state=welcome|system-admin|profile|details|review|provisioning|handoff|first-sign-in|ready
 *   ?profile=empty_operational|acme_demo   a starting point, already chosen
 *   ?open=1                                the details form, not the summary
 *   ?fail=1                                make the third provisioning step fail once
 *   ?run=pending|running|done|failed       a snapshot of the provisioning run
 *   ?handedoff=1                           the tenant was handed to somebody else
 * The bar mirrors the same states as buttons. */

(function () {
    'use strict';

    /* --------------------------------------------------------------- mock data */

    var PASSWORD_POLICY = {
        minLength: 12,
        requireUppercase: true,
        requireLowercase: true,
        requireDigit: true,
        requireSpecial: true,
        specialChars: '!@#$%^&*-_=+'
    };

    var PROFILES = [
        {
            code: 'empty_operational',
            name: 'Operational',
            summary: 'Production-ready setup',
            bullets: [
                'Standard reference data and counterparties',
                'Your legal entities, from their LEI',
                'No test data'
            ],
            audience: 'For real use',
            parties: ['The legal entity of the root LEI'],
            forcePasswordChange: true,
            params: [
                {
                    name: 'root_lei',
                    label: 'Root LEI',
                    type: 'text',
                    default: '',
                    required: true,
                    hint: "The LEI of the top legal entity. Its GLEIF hierarchy becomes the tenant's parties."
                },
                {
                    name: 'counterparty_size',
                    label: 'Counterparty set',
                    type: 'choice',
                    choices: ['small', 'large'],
                    default: 'small',
                    required: true,
                    hint: 'small is about 13k GLEIF counterparties; large is about 500k.'
                }
            ],
            steps: [
                'Create tenant and admin',
                'Publish base reference data',
                'Import counterparties',
                'Import parties from root LEI',
                'Provision parties (activate, onboard, essentials)',
                'Complete provisioning'
            ]
        },
        {
            code: 'acme_demo',
            name: 'ACME demo',
            summary: 'Pre-configured sandbox',
            bullets: [
                '4 legal entities, books and desks',
                '45 staff to sign in as',
                'Live synthetic market data'
            ],
            audience: 'For demos and testing',
            logo: 'acme-logo.png',
            inheritsAdminPassword: true,
            forcePasswordChange: false,
            parties: [
                'Acme Corporation Plc',
                'ACME Corporation UK plc',
                'ACME Corporation US Inc',
                'ACME Corporation HK Ltd'
            ],
            defaults: {
                code: 'acme_corporation',
                name: 'Acme Corporation',
                hostname: 'acme_corporation.localhost',
                adminUsername: 'tenant_admin',
                adminEmail: 'tenant_admin@acme.example.com'
            },
            params: [],
            steps: [
                'Create tenant and admin',
                'Publish base reference data',
                'Import counterparties',
                'Import Acme LEI hierarchy',
                'Provision Acme Corporation Plc',
                'Provision ACME UK, US and HK',
                'Load staff and photos',
                'Start market data feeds',
                'Complete provisioning'
            ]
        }
    ];

    /* The flat rail: the system administrator, the new tenant steps inline, the
       first sign-in, then Ready. The new tenant steps are the same definitions
       the New tenant prototype uses. */
    var STEPS = [
        { id: 'system-admin', title: 'Create the administrator', lead: 'This account sets up ORE Studio and creates its tenants.', final: true },
        { id: 'profile', title: 'Choose a starting point', lead: 'Choose a starting point for the new tenant.' },
        { id: 'details', title: 'Describe the tenant', lead: 'Name the tenant and create its administrator.' },
        { id: 'review', title: 'Review', lead: 'Nothing is created until you confirm.' },
        { id: 'provisioning', title: 'Provisioning', lead: 'This runs on the server. You can leave this page and come back.', final: true },
        { id: 'handoff', title: 'Hand off', lead: 'The tenant is ready. Its administrator signs in next.', final: true },
        { id: 'first-sign-in', title: 'First sign-in', lead: '', final: true },
        { id: 'ready', title: 'Ready', lead: 'ORE Studio is set up.' }
    ];

    var RUN_STATES = ['pending', 'running', 'done', 'failed'];
    var STRENGTH_TONE = ['', 's1', 's2', 's3', 's4'];
    var STRENGTH_WORD = ['', 'Weak', 'Fair', 'Good', 'Strong'];

    var WELCOME_STAGES = [
        { title: 'Create the administrator', text: 'The account that sets up ORE Studio.' },
        { title: 'Create the first tenant', text: 'Your organisation, or the ACME demo bank.' },
        { title: 'Sign in', text: "As the tenant's administrator, ready to work." }
    ];

    var HANDOFF = 5;

    /* ------------------------------------------------------------------- state */

    var S = {
        welcomed: false,
        admin: { username: 'super_admin', email: '', password: '', ok: false },
        profileCode: undefined,
        details: undefined,
        at: 0,
        open: false,
        failOnce: false,
        pw: {},
        passwordOk: false,
        started: false,
        cursor: 0,
        runFailed: false,
        failedOnce: false,
        live: false,
        handedOff: false,
        signIn: { password: '', ok: false, party: '' }
    };

    var timer = null;

    function profile() {
        return PROFILES.filter(function (p) { return p.code === S.profileCode; })[0];
    }

    function totalSteps() {
        var p = profile();
        return p ? p.steps.length : 0;
    }

    function failAt() {
        return S.failOnce ? 2 : undefined;
    }

    function emptyDetails(p) {
        var d = {
            code: '',
            name: '',
            hostname: '',
            adminUsername: 'tenant_admin',
            adminEmail: '',
            adminPassword: '',
            useMyPassword: p.inheritsAdminPassword === true,
            params: {}
        };
        if (p.defaults !== undefined) {
            Object.keys(p.defaults).forEach(function (k) { d[k] = p.defaults[k]; });
        }
        p.params.forEach(function (param) { d.params[param.name] = param.default; });
        return d;
    }

    function tenantAdmin() {
        return (S.details !== undefined ? S.details.adminUsername : 'tenant_admin') + '@' +
            (S.details !== undefined ? S.details.code : 'tenant');
    }

    function mustChange() {
        var p = profile();
        return p === undefined ? true : p.forcePasswordChange !== false;
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var code = p.get('profile');
        if (PROFILES.some(function (x) { return x.code === code; })) choose(code);
        if (p.get('open') === '1') S.open = true;
        if (p.get('fail') === '1') S.failOnce = true;
        if (p.get('handedoff') === '1') S.handedOff = true;
        var state = p.get('state') || p.get('step');
        if (state === 'welcome') {
            S.welcomed = false;
        } else {
            STEPS.forEach(function (s, i) {
                if (s.id === state) { S.welcomed = true; S.at = i; }
            });
        }
        var run = p.get('run');
        if (RUN_STATES.indexOf(run) >= 0) runSnapshot(run);
    }

    function choose(code) {
        var p = PROFILES.filter(function (x) { return x.code === code; })[0];
        if (p === undefined) return;
        S.profileCode = code;
        S.details = emptyDetails(p);
        S.open = p.defaults === undefined;
        S.passwordOk = false;
        S.pw = {};
    }

    function runSnapshot(which) {
        if (profile() === undefined) return;
        var total = totalSteps();
        S.started = true;
        S.runFailed = false;
        S.failedOnce = false;
        S.live = false;
        if (which === 'pending') { S.started = false; S.cursor = 0; }
        else if (which === 'running') { S.cursor = 1; }
        else if (which === 'done') { S.cursor = total; }
        else if (which === 'failed') { S.cursor = 2; S.runFailed = true; S.failedOnce = true; }
    }

    function passwordOk() {
        return S.details !== undefined && (S.details.useMyPassword === true || S.passwordOk);
    }

    /* ---------------------------------------------------------------- utilities */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function passwordRules() {
        var rules = ['length'];
        if (PASSWORD_POLICY.requireUppercase) rules.push('upper');
        if (PASSWORD_POLICY.requireLowercase) rules.push('lower');
        if (PASSWORD_POLICY.requireDigit) rules.push('digit');
        if (PASSWORD_POLICY.requireSpecial) rules.push('special');
        return rules;
    }

    function assess(value) {
        var met = {};
        if (value.length >= PASSWORD_POLICY.minLength) met.length = true;
        if (PASSWORD_POLICY.requireUppercase && /[A-Z]/.test(value)) met.upper = true;
        if (PASSWORD_POLICY.requireLowercase && /[a-z]/.test(value)) met.lower = true;
        if (PASSWORD_POLICY.requireDigit && /[0-9]/.test(value)) met.digit = true;
        if (PASSWORD_POLICY.requireSpecial && PASSWORD_POLICY.specialChars !== '' &&
            Array.prototype.some.call(value, function (c) {
                return PASSWORD_POLICY.specialChars.indexOf(c) >= 0;
            })) {
            met.special = true;
        }
        var rules = passwordRules();
        var valid = rules.every(function (r) { return met[r] === true; });
        var strength;
        if (value.length === 0) strength = 0;
        else if (valid) strength = value.length >= PASSWORD_POLICY.minLength + 4 ? 4 : 3;
        else strength = Object.keys(met).length >= Math.ceil(rules.length / 2) ? 2 : 1;
        return { met: met, valid: valid, strength: strength };
    }

    function ruleLabel(rule) {
        if (rule === 'length') return 'At least ' + PASSWORD_POLICY.minLength + ' characters';
        if (rule === 'upper') return 'An uppercase letter (A-Z)';
        if (rule === 'lower') return 'A lowercase letter (a-z)';
        if (rule === 'digit') return 'A digit (0-9)';
        return 'A special character (' + PASSWORD_POLICY.specialChars + ')';
    }

    /* ------------------------------------------------------------ the password */

    function pwState(key) {
        if (S.pw[key] === undefined) S.pw[key] = { value: '', confirm: '', reveal: false };
        return S.pw[key];
    }

    function newPasswordField(key, label, hint) {
        var st = pwState(key);
        var a = assess(st.value);
        var mismatch = st.confirm.length > 0 && st.confirm !== st.value;
        var bars = [1, 2, 3, 4].map(function (level) {
            return '<span class="bar ' + (a.strength >= level ? STRENGTH_TONE[a.strength] : '') + '"></span>';
        }).join('');
        var rules = passwordRules().map(function (rule) {
            var met = a.met[rule] === true;
            return '<li class="' + (met ? 'met' : '') + '"><span aria-hidden>' + (met ? '\u2713' : '\u25cb') +
                '</span> ' + esc(ruleLabel(rule)) + '</li>';
        }).join('');
        return '<div class="pwfield">' +
            '<div class="field">' +
            '<span class="lbl">' + esc(label) + '</span>' +
            '<div class="pw-wrap"><input type="' + (st.reveal ? 'text' : 'password') + '" autocomplete="new-password"' +
            ' data-focus="pw-' + key + '" data-pw="' + key + '" data-part="value" value="' + esc(st.value) + '">' +
            '<button type="button" class="pw-toggle" data-act="pw-toggle" data-pw="' + key + '">' +
            (st.reveal ? 'Hide' : 'Show') + '</button></div>' +
            (hint ? '<div class="hint">' + esc(hint) + '</div>' : '') +
            '</div>' +
            '<div><div class="pwstrength"><div class="bars">' + bars + '</div>' +
            '<span class="word">' + STRENGTH_WORD[a.strength] + '</span></div>' +
            '<ul class="pwrules">' + rules + '</ul></div>' +
            '<div class="field" style="margin-top:12px">' +
            '<span class="lbl">Confirm password</span>' +
            '<div class="pw-wrap"><input type="' + (st.reveal ? 'text' : 'password') + '" autocomplete="new-password"' +
            ' data-focus="pwc-' + key + '" data-pw="' + key + '" data-part="confirm" value="' + esc(st.confirm) + '">' +
            '<button type="button" class="pw-toggle" data-act="pw-toggle" data-pw="' + key + '">' +
            (st.reveal ? 'Hide' : 'Show') + '</button></div>' +
            (mismatch ? '<div class="pwerror">The passwords do not match.</div>' : '') +
            '</div></div>';
    }

    /* ------------------------------------------------------------- welcome */

    function welcome() {
        var cards = WELCOME_STAGES.map(function (s) {
            return '<li><div class="nm">' + esc(s.title) + '</div><p>' + esc(s.text) + '</p></li>';
        }).join('');
        return '<div class="welcome">' +
            '<img class="splash" src="ore-studio-splash.png" alt="ORE Studio">' +
            '<h1>Welcome to ORE Studio</h1>' +
            '<p class="lead">This installation is new. Set it up in three stages; it takes a few minutes.</p>' +
            '<ol>' + cards + '</ol>' +
            '<button type="button" class="btn primary" data-act="start">Get started</button>' +
            '</div>';
    }

    /* --------------------------------------------------- system administrator */

    function systemAdminStep() {
        return '<div class="grid2">' +
            '<div class="field"><span class="lbl">Username</span>' +
            '<input autocomplete="username" data-focus="su-username" data-a="username" value="' +
            esc(S.admin.username) + '"></div>' +
            '<div class="field"><span class="lbl">Email</span>' +
            '<input type="email" data-focus="su-email" data-a="email" value="' + esc(S.admin.email) + '"></div>' +
            '<div class="span2">' + newPasswordField('admin', 'Password') + '</div>' +
            '</div>';
    }

    /* ------------------------------------------------------- starting point step */

    function profileChoice() {
        var cards = PROFILES.map(function (p) {
            var bullets = p.bullets.map(function (b) {
                return '<li><span class="dot">\u2022</span>' + esc(b) + '</li>';
            }).join('');
            return '<button type="button" role="radio" aria-checked="' + (S.profileCode === p.code) + '"' +
                ' class="choicecard' + (S.profileCode === p.code ? ' on' : '') + '"' +
                ' data-act="choice" data-profile="' + p.code + '">' +
                (p.logo !== undefined ? '<img src="' + p.logo + '" alt="">' : '') +
                '<span class="top"><span class="nm">' + esc(p.name) + '</span>' +
                '<span class="aud">' + esc(p.audience) + '</span></span>' +
                '<p class="sum">' + esc(p.summary) + '</p>' +
                '<ul>' + bullets + '</ul>' +
                '<p class="counts">' + p.params.length + ' ' + (p.params.length === 1 ? 'setting' : 'settings') +
                ' \u00b7 ' + p.steps.length + ' steps</p></button>';
        }).join('');
        return '<div class="choicecards" role="radiogroup">' + cards + '</div>';
    }

    /* ----------------------------------------------------------- details step */

    function detailsForm(p) {
        var d = S.details;
        var params = '';
        if (p.params.length > 0) {
            params = '<fieldset class="grid2"><legend class="span2">' + esc(p.name) + ' settings</legend>' +
                p.params.map(function (param) {
                    var control;
                    if (param.type === 'choice') {
                        control = '<select data-p="' + esc(param.name) + '">' +
                            (param.choices || []).map(function (c) {
                                return '<option' + (d.params[param.name] === c ? ' selected' : '') + '>' + esc(c) + '</option>';
                            }).join('') + '</select>';
                    } else {
                        control = '<input data-focus="p-' + esc(param.name) + '" data-p="' + esc(param.name) + '" value="' +
                            esc(d.params[param.name]) + '">';
                    }
                    return '<div class="field"><span class="lbl">' + esc(param.label) + '</span>' + control +
                        (param.hint ? '<div class="hint">' + esc(param.hint) + '</div>' : '') + '</div>';
                }).join('') + '</fieldset>';
        }
        var admin = '<fieldset class="grid2"><legend class="span2">Tenant administrator</legend>' +
            '<div class="field"><span class="lbl">Username</span>' +
            '<input data-focus="adminUsername" data-f="adminUsername" value="' + esc(d.adminUsername) + '"></div>' +
            '<div class="field"><span class="lbl">Email</span>' +
            '<input data-focus="adminEmail" data-f="adminEmail" value="' + esc(d.adminEmail) + '"></div>' +
            (p.inheritsAdminPassword === true
                ? '<label class="checkline span2"><input type="checkbox" data-f="useMyPassword"' +
                  (d.useMyPassword ? ' checked' : '') + '> Use my password</label>'
                : '') +
            (!d.useMyPassword
                ? '<div class="span2">' + newPasswordField('tenant', 'Initial password',
                    p.forcePasswordChange ? 'They must change it at first sign-in.' : undefined) + '</div>'
                : '') +
            '</fieldset>';
        return '<div>' +
            '<fieldset class="grid2"><legend class="span2">Tenant</legend>' +
            '<div class="field"><span class="lbl">Name</span>' +
            '<input data-focus="name" data-f="name" placeholder="Northwind Capital" value="' + esc(d.name) + '"></div>' +
            '<div class="field"><span class="lbl">Code</span>' +
            '<input data-focus="code" data-f="code" placeholder="northwind" value="' + esc(d.code) + '">' +
            '<div class="hint">Short and unique. Used in usernames: admin@code.</div></div>' +
            '<div class="field span2"><span class="lbl">Hostname</span>' +
            '<input data-focus="hostname" data-f="hostname" placeholder="northwind.example.com" value="' +
            esc(d.hostname) + '"></div></fieldset>' +
            params + admin + '</div>';
    }

    function detailsStep(p) {
        var d = S.details;
        if (S.open) return detailsForm(p);
        var rows = [
            ['Tenant', d.name + ' (' + d.code + ')'],
            ['Hostname', d.hostname],
            ['Administrator', d.adminUsername + '@' + d.code],
            ['Password', d.useMyPassword ? 'Same as yours' : 'Set below']
        ].map(function (r) {
            return '<dt>' + esc(r[0]) + '</dt><dd>' + esc(r[1]) + '</dd>';
        }).join('');
        return '<div>' +
            '<div class="panel-soft">' +
            '<p style="margin:0;font-size:13px">' + esc(p.name) + ' uses its standard settings.</p>' +
            '<dl class="reviewgrid" style="margin-top:12px;grid-template-columns:8rem 1fr">' + rows + '</dl>' +
            '<button type="button" class="btn ghost small" style="margin-top:12px;margin-left:-11px"' +
            ' data-act="open-settings">Change settings</button></div>' +
            (!d.useMyPassword ? newPasswordField('admin', 'Administrator password') : '') +
            '</div>';
    }

    /* ------------------------------------------------------------ review step */

    function reviewStep(p) {
        var d = S.details;
        var rows = [
            ['Starting point', p.name],
            ['Tenant', (d.name || '-') + ' (' + (d.code || '-') + ')'],
            ['Administrator', d.adminUsername]
        ];
        p.params.forEach(function (param) {
            rows.push([param.label, d.params[param.name] || '-']);
        });
        var dl = rows.map(function (r) {
            return '<dt>' + esc(r[0]) + '</dt><dd>' + esc(r[1]) + '</dd>';
        }).join('');
        return '<dl class="reviewgrid">' + dl + '</dl>' +
            '<p style="margin:16px 0 0;font-size:13px;color:var(--ink-dim)">Creating the tenant runs ' +
            p.steps.length + ' steps.</p>' +
            '<label class="failtoggle"><input type="checkbox" data-fail="1"' + (S.failOnce ? ' checked' : '') +
            '> Prototype: make step 3 fail once</label>';
    }

    /* ------------------------------------------------------ provisioning step */

    function progressList() {
        var p = profile();
        var items = p.steps.map(function (label, i) {
            var state;
            if (i < S.cursor) state = 'done';
            else if (i === S.cursor) state = S.runFailed ? 'failed' : (S.started ? 'running' : 'pending');
            else state = 'pending';
            var mark = state === 'pending' ? '\u25cb' : state === 'running' ? '\u25d0' :
                state === 'done' ? '\u25cf' : '\u2715';
            return '<li class="' + state + '"><span class="mark" aria-hidden>' + mark + '</span>' +
                '<span class="lbl">' + esc(label) + '</span>' +
                (state === 'failed' ? '<span class="timedout">timed out</span>' : '') + '</li>';
        }).join('');
        var foot = '';
        if (S.runFailed) {
            foot = '<div class="runfoot">' +
                '<button type="button" class="btn primary small" data-act="retry">Retry from failed step</button>' +
                '<button type="button" class="btn danger small" data-act="discard">Discard tenant</button>' +
                '<span class="note">Completed steps are kept. Retrying is safe.</span></div>';
        }
        return '<ol class="runlist">' + items + '</ol>' + foot;
    }

    /* ---------------------------------------------------------- handoff step */

    function handoffStep(p) {
        var user = tenantAdmin();
        return '<div>' +
            '<p style="margin:0 0 16px;font-size:13px;color:var(--ink-dim)">Its administrator is ' +
            '<span class="mono">' + esc(user) + '</span>.</p>' +
            '<div class="handoffcards">' +
            '<button type="button" class="handoffcard" data-act="handoff-continue">' +
            '<span class="nm">Continue as tenant admin</span>' +
            '<p>Sign in as ' + esc(user) + ' now.</p></button>' +
            '<button type="button" class="handoffcard" data-act="handoff-elsewhere">' +
            '<span class="nm">Hand off to someone else</span>' +
            '<p>Give them the username' +
            (mustChange() ? '. They set their own password at first sign-in.' : ' and password.') +
            '</p></button></div></div>';
    }

    /* -------------------------------------------------------- first sign-in step */

    function partyOptions(parties, value) {
        return parties.map(function (party) {
            return '<option' + (party === value ? ' selected' : '') + '>' + esc(party) + '</option>';
        }).join('');
    }

    function firstSignInStep() {
        var p = profile();
        var parties = p !== undefined ? p.parties : [];
        var change = mustChange();
        var body = '';
        if (change) body += newPasswordField('signin', 'New password');
        if (parties.length > 1) {
            var value = S.signIn.party || parties[0];
            body += '<div class="field"><span class="lbl">Start in</span>' +
                '<select data-si="party">' + partyOptions(parties, value) + '</select>' +
                '<div class="hint">You work in more than one party. You can switch at any time.</div></div>';
        }
        return '<div>' + body + '</div>';
    }

    /* -------------------------------------------------------------- ready step */

    function readyStep() {
        var p = profile();
        var change = mustChange();
        var notice;
        if (S.handedOff) {
            notice = '<div class="notice success" role="status">Give <span class="mono">' + esc(tenantAdmin()) +
                '</span> their username.' + (change ? ' They set their own password at first sign-in.' : '') + '</div>';
        } else {
            notice = '<div class="notice success" role="status">You are signed in to ' +
                esc(S.details !== undefined ? S.details.name : 'the new tenant') + '.</div>';
        }
        return notice +
            '<div style="display:flex;flex-wrap:wrap;gap:8px">' +
            '<button type="button" class="btn primary">Go to ' + (S.handedOff ? 'Tenants' : 'home') + '</button>' +
            '<button type="button" class="btn">Create another tenant</button></div>';
    }

    /* ------------------------------------------------------------ the header */

    function tenantHeader() {
        var p = profile();
        if (p === undefined || S.details === undefined) return '';
        return '<div class="stepheader">' +
            (p.logo !== undefined ? '<img src="' + p.logo + '" alt="">' : '') +
            '<div><div class="nm">' + esc(S.details.name || 'New tenant') + '</div>' +
            '<div class="sub">' + esc(p.name) + '</div></div></div>';
    }

    /* ------------------------------------------------------------ the journey */

    function rail(at) {
        return '<nav class="railnav" aria-label="Journey steps"><ol>' +
            STEPS.map(function (s, i) {
                var cls = i === at ? 'current' : (i < at ? 'done' : 'ahead');
                return '<li class="railentry ' + cls + '"' + (i === at ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' + (i < at ? '\u2713' : String(i + 1)) + '</span>' +
                    esc(s.title) + '</li>';
            }).join('') + '</ol></nav>';
    }

    function stepBody() {
        var p = profile();
        var step = STEPS[S.at];
        if (step.id === 'system-admin') return systemAdminStep();
        if (step.id === 'profile') return profileChoice();
        if (step.id === 'first-sign-in') return firstSignInStep();
        if (step.id === 'ready') return readyStep();
        if (p === undefined || S.details === undefined) return '';
        if (step.id === 'details') return detailsStep(p);
        if (step.id === 'review') return reviewStep(p);
        if (step.id === 'provisioning') return progressList();
        return handoffStep(p);
    }

    function stepLead(step) {
        if (step.id === 'first-sign-in') {
            return mustChange() ? 'Set a password only you know, then choose where you start.' : 'Choose where you start.';
        }
        return step.lead;
    }

    function nextOf() {
        var id = STEPS[S.at].id;
        if (id === 'system-admin') {
            return { label: 'Create administrator', enabled: S.admin.ok && S.admin.username.length >= 3 };
        }
        if (id === 'profile') return { label: 'Continue', enabled: S.profileCode !== undefined };
        if (id === 'details') return { label: 'Continue', enabled: passwordOk() };
        if (id === 'review') return { label: 'Create tenant', enabled: true };
        if (id === 'first-sign-in') return { label: 'Finish', enabled: S.signIn.ok || !mustChange() };
        return null;
    }

    function tenantHeaderSteps() {
        return ['details', 'review', 'provisioning', 'handoff'];
    }

    function render() {
        if (!S.welcomed) {
            document.getElementById('app').innerHTML = welcome();
            renderNote();
            renderBar();
            return;
        }
        var at = S.at;
        var step = STEPS[at];
        var backAllowed = at > 0 && STEPS[at - 1].final !== true;
        var next = nextOf();
        var foot = next === null ? '' :
            '<div class="stepfoot">' +
            '<button type="button" class="btn ghost" data-act="back"' + (backAllowed ? '' : ' disabled') + '>Back</button>' +
            '<button type="button" class="btn primary ml-auto" data-act="next"' + (next.enabled ? '' : ' disabled') + '>' +
            esc(next.label) + '</button></div>';
        var header = tenantHeaderSteps().indexOf(step.id) >= 0 ? tenantHeader() : '';
        var signedin = at > 0 ?
            '<p class="signedin">Signed in as <span class="mono">' +
            esc(at > HANDOFF && !S.handedOff ? tenantAdmin() : S.admin.username) + '</span></p>' : '';

        document.getElementById('app').innerHTML =
            '<div class="page"><h1>Set up ORE Studio</h1>' + signedin +
            '<div class="journey">' + rail(at) +
            '<section class="card">' + header +
            '<h2>' + esc(step.title) + '</h2><p class="lead">' + esc(stepLead(step)) + '</p>' +
            stepBody() + foot + '</section></div></div>';

        renderNote();
        renderBar();
    }

    function renderNote() {
        var p = profile();
        var where = !S.welcomed ? 'welcome' : STEPS[S.at].id;
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 journey First run \u00b7 starting point ' +
            (p ? p.name : 'none') + ' \u00b7 state ' + where;
    }

    function renderBar() {
        var steps = '<button data-act="welcome"' + (!S.welcomed ? ' class="on"' : '') + '>welcome</button>' +
            STEPS.map(function (s, i) {
                return '<button data-act="state" data-state="' + s.id + '"' +
                    (S.welcomed && S.at === i ? ' class="on"' : '') + '>' + esc(s.id) + '</button>';
            }).join('');
        var profiles = '<span class="label">starting point</span>' +
            '<button data-act="profile" data-profile=""' + (S.profileCode === undefined ? ' class="on"' : '') + '>none</button>' +
            PROFILES.map(function (p) {
                return '<button data-act="profile" data-profile="' + p.code + '"' +
                    (S.profileCode === p.code ? ' class="on"' : '') + '>' + esc(p.name) + '</button>';
            }).join('');
        var runs = '<span class="label">run</span>' + RUN_STATES.map(function (r) {
            return '<button data-act="run" data-run="' + r + '">' + r + '</button>';
        }).join('');
        var extras = '<span class="sep">|</span>' +
            '<button data-act="open"' + (S.open ? ' class="on"' : '') + '>settings form</button>' +
            '<button data-act="fail"' + (S.failOnce ? ' class="on"' : '') + '>fail step 3</button>' +
            '<button data-act="handedoff"' + (S.handedOff ? ' class="on"' : '') + '>handed off</button>';
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">state</span>' + steps + '<span class="sep">|</span>' + profiles +
            '<span class="sep">|</span>' + runs + extras;
    }

    /* --------------------------------------------------------------- behaviour */

    function rerender() {
        var ae = document.activeElement;
        var key = ae && ae.getAttribute ? ae.getAttribute('data-focus') : null;
        var start = ae && typeof ae.selectionStart === 'number' ? ae.selectionStart : null;
        var end = ae && typeof ae.selectionEnd === 'number' ? ae.selectionEnd : null;
        render();
        if (key) {
            var el = document.querySelector('[data-focus="' + key + '"]');
            if (el) {
                el.focus();
                if (start !== null && el.setSelectionRange) {
                    try { el.setSelectionRange(start, end); } catch (e) { /* not a text input */ }
                }
            }
        }
    }

    function stopTimer() {
        if (timer !== null) { clearTimeout(timer); timer = null; }
    }

    function scheduleTick() {
        stopTimer();
        var total = totalSteps();
        if (!S.live || !S.started || S.runFailed || S.cursor >= total) return;
        timer = setTimeout(function () {
            timer = null;
            if (failAt() === S.cursor && !S.failedOnce) {
                S.failedOnce = true;
                S.runFailed = true;
                render();
                return;
            }
            S.cursor += 1;
            render();
            if (S.cursor >= total) {
                S.live = false;
                if (S.at === 4) S.at = 5;
                render();
            } else {
                scheduleTick();
            }
        }, 900);
    }

    function startRun() {
        S.live = true;
        S.started = true;
        S.cursor = 0;
        S.runFailed = false;
        S.failedOnce = false;
        scheduleTick();
    }

    function restart() {
        stopTimer();
        S.profileCode = undefined;
        S.details = undefined;
        S.open = false;
        S.passwordOk = false;
        S.pw = {};
        S.started = false;
        S.cursor = 0;
        S.runFailed = false;
        S.failedOnce = false;
        S.live = false;
        S.at = 1;
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'start') {
            S.welcomed = true;
        } else if (act === 'welcome') {
            S.welcomed = false;
        } else if (act === 'state') {
            STEPS.forEach(function (s, i) {
                if (s.id === el.getAttribute('data-state')) { S.at = i; S.welcomed = true; }
            });
        } else if (act === 'profile') {
            choose(el.getAttribute('data-profile'));
        } else if (act === 'choice') {
            choose(el.getAttribute('data-profile'));
        } else if (act === 'run') {
            runSnapshot(el.getAttribute('data-run'));
        } else if (act === 'open') {
            S.open = !S.open;
        } else if (act === 'open-settings') {
            S.open = true;
        } else if (act === 'fail') {
            S.failOnce = !S.failOnce;
        } else if (act === 'handedoff') {
            S.handedOff = !S.handedOff;
        } else if (act === 'pw-toggle') {
            var st = pwState(el.getAttribute('data-pw'));
            st.reveal = !st.reveal;
        } else if (act === 'back') {
            if (!el.disabled && S.at > 0 && STEPS[S.at - 1].final !== true) S.at -= 1;
        } else if (act === 'next') {
            if (!el.disabled) goNext();
        } else if (act === 'retry') {
            S.runFailed = false;
            S.live = true;
            scheduleTick();
        } else if (act === 'discard') {
            restart();
        } else if (act === 'handoff-continue') {
            S.at = 6;
        } else if (act === 'handoff-elsewhere') {
            S.handedOff = true;
            S.at = 7;
        } else {
            return;
        }
        rerender();
    });

    function goNext() {
        if (S.at === 3) {
            startRun();
            S.at = 4;
            return;
        }
        S.at += 1;
    }

    function inputChanged(el) {
        if (el.getAttribute('data-fail') !== null) { S.failOnce = el.checked; return true; }
        var a = el.getAttribute('data-a');
        if (a !== null) {
            S.admin[a] = el.value;
            return true;
        }
        var si = el.getAttribute('data-si');
        if (si !== null) { S.signIn[si] = el.value; return true; }
        var key = el.getAttribute('data-pw');
        if (key !== null) {
            var st = pwState(key);
            var part = el.getAttribute('data-part');
            if (part === 'value') st.value = el.value; else st.confirm = el.value;
            var ok = assess(st.value).valid && st.value === st.confirm;
            if (key === 'admin') S.admin.ok = ok;
            else if (key === 'signin') S.signIn.ok = ok;
            else { S.details.adminPassword = st.value; S.passwordOk = ok; }
            return true;
        }
        var f = el.getAttribute('data-f');
        if (f !== null) {
            if (el.type === 'checkbox') S.details[f] = el.checked;
            else S.details[f] = el.value;
            return true;
        }
        var p = el.getAttribute('data-p');
        if (p !== null) { S.details.params[p] = el.value; return true; }
        return false;
    }

    function onInput(ev) {
        var el = ev.target;
        if (el && el.getAttribute && inputChanged(el)) rerender();
    }

    document.addEventListener('input', onInput);
    document.addEventListener('change', onInput);

    readParams();
    render();
})();
