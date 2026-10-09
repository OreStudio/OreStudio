/* First run journey prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * The first run journey as FirstRunJourney.tsx and firstRunSteps.tsx build it
 * today. One rail opens on the welcome and carries the administrator, the
 * starting point, the tenant stages and the ready step. The starting point
 * decides the rail's shape: a profile runs the tenant stages, and Bare system
 * creates no tenant, so the rail runs from the starting point straight to
 * ready. The journey keeps its session to the end: the browser is signed in as
 * the administrator it just made, and no step signs it out. The tenant
 * administrator's own run is not a step here, so this prototype carries that
 * screen as its own state and marks it as the other person's screen.
 *
 * Every sentence is the one the product's English catalogue states
 * (projects/ores.web/packages/web/src/i18n/locales/en.ts). The profile data and
 * the run are the prototype's own mocks.
 *
 * States are chosen from the query string, as the navigation prototype does:
 *   ?state=welcome|administrator|profile|details|review|provisioning|ready|tenant-setup
 *   ?admin=resume                          the administrator step signs in
 *   ?profile=bare_system|empty_operational|acme_demo
 *   ?open=1                                the details form, not the summary
 *   ?fail=1                                make the third provisioning step fail once
 *   ?stepfail=1                            a step's action fails, above the rail
 *   ?request=1                             a request fails, above the screen
 *   ?run=pending|running|done|failed       a snapshot of the provisioning run
 * The bar at the foot mirrors the same states as buttons. */

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

    /* The starting point that creates no tenant. Its words come from the
       catalogue, because no server row describes it. It sits first among the
       cards and wears the same card. */
    var BARE_SYSTEM = {
        code: 'bare_system',
        name: 'Bare system',
        audience: 'For advanced users',
        summary: 'Keeps the system tenant alone and creates no tenant of its own.',
        bullets: [],
        parties: [],
        parameters: [],
        steps: []
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
            parameters: [
                {
                    name: 'root_lei',
                    label: 'Root LEI',
                    dataType: 'legal_entity',
                    default: '',
                    hint: "The LEI of the top legal entity. Its GLEIF hierarchy becomes the tenant's parties."
                },
                {
                    name: 'counterparty_size',
                    label: 'Counterparty set',
                    dataType: 'choice',
                    choices: ['small', 'large'],
                    default: 'small',
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
                'Acme Corporation HK Ltd'
            ],
            defaults: {
                code: 'acme_corporation',
                name: 'Acme Corporation',
                hostname: 'acme_corporation.localhost',
                adminUsername: 'tenant_admin',
                adminEmail: 'tenant_admin@acme.example.com'
            },
            parameters: [],
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

    /* The legal entities the search offers, as the mock read would answer. */
    var ENTITIES = [
        { legalName: 'Northwind Capital Plc', lei: '5493001KJTIIGC8Y1R12', country: 'GB' },
        { legalName: 'Northwind Capital US Inc', lei: '549300MLUDYVRQOOXS22', country: 'US' }
    ];

    /* The run the tenant administrator follows: their own tenant's setup, not
       the first run's provisioning. The words are the server catalogue's. */
    var TENANT_SETUP_STEPS = [
        'Publish the reference data',
        'Import the legal entities',
        'Provision the parties',
        'Hand the tenant to its administrator',
        'Finish'
    ];

    /* The welcome's three stages, from the catalogue. */
    var WELCOME_STAGES = [
        { title: 'Create the administrator', text: 'The account that owns this installation.' },
        { title: 'Create the first tenant', text: 'The first tenant, built from a starting point on the server.' },
        { title: 'Hand the tenant over', text: "The tenant's administrator signs in and finishes the tenant's own setup." }
    ];

    /* The rail, in the order the screen declares it. The administrator step
       takes its title and lead from the state, so it carries neither here. */
    var STEPS = [
        {
            id: 'welcome',
            title: 'Welcome to ORE Studio',
            lead: 'Set up a new installation: create the administrator that owns it, then choose what the installation is left with.'
        },
        { id: 'administrator' },
        {
            id: 'profile',
            title: 'Choose a starting point',
            lead: 'A starting point is a profile the server holds. It states the settings, the steps and the tenant it creates.'
        },
        {
            id: 'details',
            title: 'Describe the tenant',
            lead: 'Name the tenant and create its administrator.'
        },
        {
            id: 'review',
            title: 'Review',
            lead: 'Nothing is created until you confirm.'
        },
        {
            id: 'provisioning',
            title: 'Provisioning',
            lead: 'You can leave this page and come back.',
            final: true
        },
        {
            id: 'ready',
            title: 'Ready',
            lead: "The deployment is set up. The tenant's administrator finishes the tenant's own setup when they first sign in."
        }
    ];

    /* The stages that build a tenant. Bare system leaves them out. */
    var TENANT_STEP_IDS = ['details', 'review', 'provisioning'];

    var RUN_STATES = ['pending', 'running', 'done', 'failed'];
    var STRENGTH_TONE = ['', 's1', 's2', 's3', 's4'];
    var STRENGTH_WORD = ['', 'Weak', 'Fair', 'Good', 'Strong'];

    /* ------------------------------------------------------------------- state */

    var S = {
        step: 'welcome',
        adminMode: 'create',
        admin: { username: 'super_admin', email: 'super_admin@system.ores', password: '', ok: false },
        resume: { username: 'super_admin', password: '' },
        profileCode: undefined,
        details: undefined,
        entity: undefined,
        entityQuery: '',
        entityChanging: false,
        open: false,
        failOnce: false,
        pw: {},
        passwordOk: false,
        started: false,
        cursor: 0,
        runFailed: false,
        failedOnce: false,
        runComplete: false,
        live: false,
        retryNote: undefined,
        stepFailure: undefined,
        banner: undefined,
        profilesFailed: false,
        after: '',
        tenantStarted: true,
        tenantCursor: 3,
        tenantFailed: false
    };

    var timer = null;

    function cards() {
        return [BARE_SYSTEM].concat(PROFILES);
    }

    function profileByCode(code) {
        return cards().filter(function (p) { return p.code === code; })[0];
    }

    function profile() {
        return profileByCode(S.profileCode);
    }

    function noTenant() {
        return S.profileCode === BARE_SYSTEM.code;
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
            parameters: {}
        };
        if (p.defaults !== undefined) {
            Object.keys(p.defaults).forEach(function (k) { d[k] = p.defaults[k]; });
        }
        p.parameters.forEach(function (param) { d.parameters[param.name] = param.default; });
        return d;
    }

    function tenantAdmin() {
        return (S.details !== undefined ? S.details.adminUsername : 'tenant_admin') + '@' +
            (S.details !== undefined ? S.details.code : 'tenant');
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var code = p.get('profile');
        if (profileByCode(code) !== undefined) choose(code);
        if (p.get('admin') === 'resume') S.adminMode = 'resume';
        if (p.get('open') === '1') S.open = true;
        if (p.get('fail') === '1') S.failOnce = true;
        if (p.get('stepfail') === '1') {
            S.stepFailure = 'Provisioning request failed: the server did not answer.';
        }
        if (p.get('request') === '1') {
            S.banner = 'Request failed: 503 Service Unavailable';
            S.profilesFailed = true;
        }
        var state = p.get('state') || p.get('step');
        var known = state === 'tenant-setup' ||
            STEPS.some(function (s) { return s.id === state; });
        if (known) S.step = state;
        if (noTenant() && TENANT_STEP_IDS.indexOf(S.step) >= 0) S.step = 'profile';
        var run = p.get('run');
        if (RUN_STATES.indexOf(run) >= 0) {
            if (S.step === 'tenant-setup') tenantRunSnapshot(run);
            else runSnapshot(run);
        }
    }

    function choose(code) {
        var p = profileByCode(code);
        if (p === undefined) return;
        S.profileCode = code;
        S.details = emptyDetails(p);
        S.open = p.defaults === undefined;
        S.passwordOk = false;
        S.pw = {};
        S.entity = undefined;
        S.entityQuery = '';
        S.entityChanging = false;
        if (noTenant() && TENANT_STEP_IDS.indexOf(S.step) >= 0) S.step = 'profile';
    }

    function runSnapshot(which) {
        if (profile() === undefined || noTenant()) return;
        var total = totalSteps();
        S.step = 'provisioning';
        S.started = true;
        S.runFailed = false;
        S.failedOnce = false;
        S.live = false;
        S.runComplete = false;
        S.retryNote = undefined;
        if (which === 'pending') { S.started = false; S.cursor = 0; }
        else if (which === 'running') { S.cursor = 1; }
        else if (which === 'done') { S.cursor = total; S.runComplete = true; }
        else if (which === 'failed') { S.cursor = 2; S.runFailed = true; S.failedOnce = true; }
    }

    function tenantRunSnapshot(which) {
        S.step = 'tenant-setup';
        S.tenantStarted = true;
        S.tenantFailed = false;
        if (which === 'pending') { S.tenantStarted = false; S.tenantCursor = 0; }
        else if (which === 'running') { S.tenantCursor = 1; }
        else if (which === 'done') { S.tenantCursor = TENANT_SETUP_STEPS.length; }
        else if (which === 'failed') { S.tenantCursor = 1; S.tenantFailed = true; }
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

    function field(label, control, hint) {
        return '<div class="field"><span class="lbl">' + esc(label) + '</span>' + control +
            (hint ? '<div class="hint">' + esc(hint) + '</div>' : '') + '</div>';
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

    /* -------------------------------------------------------------- the welcome */

    function welcomeBody() {
        var cardsHtml = WELCOME_STAGES.map(function (s) {
            return '<li><p class="nm">' + esc(s.title) + '</p>' +
                '<p class="text">' + esc(s.text) + '</p></li>';
        }).join('');
        return '<div class="welcomecards"><ol>' + cardsHtml + '</ol></div>';
    }

    /* -------------------------------------------------- the administrator step */

    function administratorStep() {
        var known = S.adminMode === 'resume';
        return {
            id: 'administrator',
            title: known ? 'Sign in as the administrator' : 'Create the administrator',
            lead: known
                ? 'This installation already has its administrator. Sign in as that account to carry on. The password is also what the tenant takes when its profile shares yours.'
                : 'This installation has no administrator, and nobody can sign in yet.'
        };
    }

    function administratorBody() {
        if (S.adminMode === 'resume') {
            return '<form class="plain" onsubmit="return false">' +
                field('Administrator username',
                    '<input autocomplete="username" data-focus="re-username" data-r="username" value="' +
                    esc(S.resume.username) + '">') +
                field('Administrator password',
                    '<input type="password" autocomplete="current-password" data-focus="re-password" data-r="password" value="' +
                    esc(S.resume.password) + '">') +
                '</form>';
        }
        return '<form class="plain" onsubmit="return false">' +
            '<p class="hint">The administrator owns the installation. The deployment stops being in bootstrap mode when this account exists.</p>' +
            field('Administrator username',
                '<input autocomplete="username" data-focus="su-username" data-a="username" value="' +
                esc(S.admin.username) + '">') +
            field('Administrator email',
                '<input type="email" autocomplete="email" data-focus="su-email" data-a="email" value="' +
                esc(S.admin.email) + '">') +
            newPasswordField('admin', 'Administrator password') +
            '</form>';
    }

    /* ------------------------------------------------------- the starting point */

    function profileChoice() {
        var cardsHtml = cards().map(function (p) {
            var bullets = p.bullets.length > 0
                ? '<ul>' + p.bullets.map(function (b) {
                    return '<li><span class="dot">\u2022</span>' + esc(b) + '</li>';
                }).join('') + '</ul>'
                : '';
            var counts = p.steps.length > 0
                ? '<p class="counts">' + p.parameters.length + ' settings \u00b7 ' +
                  p.steps.length + ' steps</p>'
                : '';
            return '<button type="button" role="radio" aria-checked="' + (S.profileCode === p.code) + '"' +
                ' class="choicecard' + (S.profileCode === p.code ? ' on' : '') + '"' +
                ' data-act="choice" data-profile="' + p.code + '">' +
                (p.logo !== undefined ? '<img src="' + p.logo + '" alt="">' : '') +
                '<span class="top"><span class="nm">' + esc(p.name) + '</span>' +
                '<span class="aud">' + esc(p.audience) + '</span></span>' +
                '<p class="sum">' + esc(p.summary) + '</p>' +
                bullets + counts + '</button>';
        }).join('');
        return '<div class="choicecards" role="radiogroup">' + cardsHtml + '</div>';
    }

    /* ----------------------------------------------------------- the details step */

    function legalEntityField(param) {
        var value = S.details.parameters[param.name] || '';
        if (value !== '' && !S.entityChanging) {
            var entity = S.entity;
            var body = entity !== undefined
                ? '<p class="nm">' + esc(entity.legalName) + '</p>' +
                  '<p class="sub mono">' + esc(entity.lei) +
                  (entity.country === '' ? '' : ' \u00b7 ' + esc(entity.country)) + '</p>' +
                  '<p class="count">1 party in its hierarchy</p>'
                : '<p class="sub mono">' + esc(value) + '</p>';
            return '<div class="span2"><span class="lbl">' + esc(param.label) + '</span>' +
                '<div class="chosen">' + body +
                '<div class="right"><button type="button" class="btn ghost small"' +
                ' data-act="entity-change">Change</button></div></div></div>';
        }
        var hint = param.hint +
            ' Choosing an entity fills in the tenant below, and every field stays editable.';
        var matches = S.entityQuery.trim() === '' ? '' :
            '<ul class="entitylist">' + ENTITIES.map(function (e) {
                return '<li><button type="button" class="entityresult' + (value === e.lei ? ' on' : '') + '"' +
                    ' data-act="entity-pick" data-param="' + esc(param.name) + '" data-lei="' + esc(e.lei) + '">' +
                    '<span><span class="nm">' + esc(e.legalName) + '</span>' +
                    '<span class="parent">' + esc(e.lei) + ' \u00b7 ' + esc(e.country) + '</span></span>' +
                    '<span class="id">1 party in its hierarchy</span></button></li>';
            }).join('') + '</ul>';
        return '<div class="span2"><label class="block"><span class="lbl">' + esc(param.label) + '</span>' +
            '<input data-focus="eq-' + esc(param.name) + '" data-eq="1"' +
            ' placeholder="Search by name or LEI" value="' + esc(S.entityQuery) + '"></label>' +
            '<div class="hint">' + esc(hint) + '</div>' +
            (value !== ''
                ? '<div class="right"><button type="button" class="btn ghost small" data-act="entity-keep">Keep</button></div>'
                : '') +
            matches + '</div>';
    }

    function parameterField(param) {
        if (param.dataType === 'legal_entity') return legalEntityField(param);
        if ((param.choices || []).length > 0) {
            var options = param.choices.map(function (c) {
                return '<option' + (S.details.parameters[param.name] === c ? ' selected' : '') + '>' +
                    esc(c) + '</option>';
            }).join('');
            return field(param.label, '<select data-p="' + esc(param.name) + '">' + options + '</select>',
                param.hint);
        }
        return field(param.label,
            '<input data-focus="p-' + esc(param.name) + '" data-p="' + esc(param.name) + '" value="' +
            esc(S.details.parameters[param.name]) + '">', param.hint);
    }

    function detailsForm(p) {
        var d = S.details;
        var settings = p.parameters.length === 0 ? '' :
            '<fieldset class="grid2"><legend class="span2">' + esc(p.name) + ' settings</legend>' +
            p.parameters.map(parameterField).join('') + '</fieldset>';
        var settingsLead = p.parameters.some(function (x) { return x.dataType === 'legal_entity'; });
        var tenant = '<fieldset class="grid2"><legend class="span2">Tenant</legend>' +
            field('Name', '<input data-focus="name" data-f="name" value="' + esc(d.name) + '">') +
            field('Code',
                '<input data-focus="code" data-f="code" value="' + esc(d.code) + '">',
                'Lowercase letters, digits and underscores, starting with a letter. It names the tenant in a username.') +
            '<div class="field span2"><span class="lbl">Hostname</span>' +
            '<input data-focus="hostname" data-f="hostname" value="' + esc(d.hostname) + '"></div></fieldset>';
        var admin = '<fieldset class="grid2"><legend class="span2">Tenant administrator</legend>' +
            field('Username',
                '<input data-focus="adminUsername" data-f="adminUsername" value="' +
                esc(d.adminUsername) + '">') +
            field('Email',
                '<input type="email" data-focus="adminEmail" data-f="adminEmail" value="' +
                esc(d.adminEmail) + '">') +
            (p.inheritsAdminPassword === true
                ? '<label class="checkline span2"><input type="checkbox" data-f="useMyPassword"' +
                  (d.useMyPassword ? ' checked' : '') + '> Use my password</label>'
                : '') +
            (!d.useMyPassword
                ? '<div class="span2">' + newPasswordField('tenant', 'Administrator password',
                    p.forcePasswordChange ? 'They must change it at first sign-in.' : undefined) + '</div>'
                : '') +
            '</fieldset>';
        return '<div>' +
            (settingsLead ? settings : '') + tenant + (settingsLead ? '' : settings) + admin +
            '</div>';
    }

    function detailsStep(p) {
        var d = S.details;
        if (S.open) return detailsForm(p);
        var rows = [
            ['Tenant', d.name + ' (' + d.code + ')'],
            ['Hostname', d.hostname],
            ['Administrator', d.adminUsername],
            ['Password', d.useMyPassword ? 'Same as mine' : 'Set here']
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

    /* ------------------------------------------------------------ the review step */

    function reviewStep(p) {
        var d = S.details;
        var rows = [
            ['Starting point', p.name],
            ['Tenant', (d.name || '-') + ' (' + (d.code || '-') + ')'],
            ['Hostname', d.hostname || '-'],
            ['Administrator', d.adminUsername],
            ['Password', d.useMyPassword ? 'The same as yours' : 'The one you typed']
        ];
        p.parameters.forEach(function (param) {
            rows.push([param.label, d.parameters[param.name] || '\u2014']);
        });
        var dl = rows.map(function (r) {
            return '<dt>' + esc(r[0]) + '</dt><dd>' + esc(r[1]) + '</dd>';
        }).join('');
        return '<dl class="reviewgrid">' + dl + '</dl>' +
            '<p style="margin:16px 0 0;font-size:13px;color:var(--ink-dim)">Creating the tenant runs ' +
            p.steps.length + ' steps.</p>' +
            (p.forcePasswordChange
                ? '<p style="margin:8px 0 0;font-size:13px;color:var(--ink-dim)">' +
                  esc(d.adminUsername) + ' sets a password of their own at first sign-in.</p>'
                : '') +
            (!d.useMyPassword && (d.adminPassword || '') === '' ?
                '<div class="notice warn" style="margin-top:16px">The tenant administrator has no password yet.</div>' : '') +
            '<label class="failtoggle"><input type="checkbox" data-fail="1"' + (S.failOnce ? ' checked' : '') +
            '> Prototype: make step 3 fail once</label>';
    }

    /* ------------------------------------------------------ the provisioning step */

    function runListHtml(labels, cursor, failed, started) {
        var items = labels.map(function (label, i) {
            var state;
            if (i < cursor) state = 'done';
            else if (i === cursor) state = failed ? 'failed' : (started ? 'running' : 'pending');
            else state = 'pending';
            var mark = state === 'pending' ? '\u25cb' : state === 'running' ? '\u25d0' :
                state === 'done' ? '\u25cf' : '\u2715';
            return '<li class="' + state + '"><span class="mark" aria-hidden>' + mark + '</span>' +
                '<span class="lbl">' + esc(label) + '</span></li>';
        }).join('');
        return '<ol class="runlist">' + items + '</ol>';
    }

    function provisioningBody() {
        var p = profile();
        if (p === undefined || noTenant()) return '';
        var body = runListHtml(p.steps, S.cursor, S.runFailed, S.started);
        if (S.runFailed) {
            body += '<div class="notice error" role="alert" style="margin-top:16px">' +
                '<p class="errmsg">Provisioning request failed: the third step timed out.</p></div>' +
                '<div class="runfoot">' +
                '<button type="button" class="btn primary small" data-act="retry">Retry from the failed step</button>' +
                '<span class="note">The steps that completed are kept.</span></div>';
        }
        if (S.retryNote !== undefined) {
            body += '<div class="notice info" role="status" style="margin-top:16px">' +
                esc(S.retryNote) + '</div>';
        }
        return body;
    }

    /* ------------------------------------------------------------ the ready step */

    function readyBody() {
        if (S.after !== '') {
            return '<p class="proto-hint">PROTOTYPE: ' + esc(S.after) + '</p>';
        }
        if (noTenant()) return '';
        return '<p class="proto-hint">PROTOTYPE: ' +
            '<button type="button" class="btn ghost small" data-act="tenant-setup">' +
            "Show the tenant administrator's screen</button></p>";
    }

    /* ------------------------------------------- the tenant administrator's screen */

    function tenantSetupBody() {
        if (S.profileCode === undefined) {
            return '<div class="notice warn" role="status">' +
                "This tenant's setup has not been started. The deployment administrator has to start it." +
                '</div>';
        }
        var complete = S.tenantCursor >= TENANT_SETUP_STEPS.length && !S.tenantFailed;
        return runListHtml(TENANT_SETUP_STEPS, S.tenantCursor, S.tenantFailed, S.tenantStarted) +
            (S.tenantFailed
                ? '<div class="runfoot">' +
                  '<button type="button" class="btn primary small" data-act="retry">Retry from the failed step</button>' +
                  '<span class="note">The steps that completed are kept.</span></div>'
                : '') +
            '<div class="stepfoot">' +
            '<button type="button" class="btn primary ml-auto" data-act="enter-app"' +
            (complete ? '' : ' disabled') + '>Go to the application</button></div>';
    }

    /* ------------------------------------------------------------ the journey rail */

    function stepList() {
        var list = STEPS.map(function (s) {
            return s.id === 'administrator' ? administratorStep() : s;
        });
        if (noTenant()) {
            return list.filter(function (s) { return TENANT_STEP_IDS.indexOf(s.id) < 0; });
        }
        return list;
    }

    function here() {
        var list = stepList();
        for (var i = 0; i < list.length; i += 1) {
            if (list[i].id === S.step) return i;
        }
        return -1;
    }

    function railHtml(list, at) {
        return '<nav class="railnav" aria-label="Steps"><ol>' +
            list.map(function (s, i) {
                var cls = i === at ? 'current' : (i < at ? 'done' : 'ahead');
                return '<li class="railentry ' + cls + '"' + (i === at ? ' aria-current="step"' : '') + '>' +
                    '<span class="railmark ' + cls + '">' + (i < at ? '\u2713' : String(i + 1)) + '</span>' +
                    esc(s.title) + '</li>';
            }).join('') + '</ol></nav>';
    }

    function nextOf(step) {
        if (step.id === 'welcome') return { label: 'Get started', enabled: true };
        if (step.id === 'administrator') {
            return S.adminMode === 'resume'
                ? { label: 'Sign in and continue',
                    enabled: S.resume.username.length > 0 && S.resume.password.length > 0 }
                : { label: 'Create administrator',
                    enabled: S.admin.username.length >= 3 && S.admin.email.length > 0 && S.admin.ok };
        }
        if (step.id === 'profile') return { label: 'Continue', enabled: S.profileCode !== undefined };
        if (step.id === 'details') return { label: 'Continue', enabled: passwordOk() };
        if (step.id === 'review') return { label: 'Create tenant', enabled: true };
        if (step.id === 'provisioning') return { label: 'Continue', enabled: S.runComplete };
        if (step.id === 'ready') return { label: 'Go home', enabled: true };
        return null;
    }

    function stepBody(step) {
        var p = profile();
        if (step.id === 'welcome') return welcomeBody();
        if (step.id === 'administrator') return administratorBody();
        if (step.id === 'profile') return profileChoice();
        if (p === undefined || S.details === undefined) return '';
        if (step.id === 'details') return detailsStep(p);
        if (step.id === 'review') return reviewStep(p);
        if (step.id === 'provisioning') return provisioningBody();
        if (step.id === 'ready') return readyBody();
        return '';
    }

    function headerHtml() {
        var splash = '<img class="splash" src="ore-studio-splash.png" alt="ORE Studio">';
        var p = profile();
        if (p === undefined || noTenant()) return splash;
        var name = S.details !== undefined ? S.details.name : '';
        var heading = name !== '' ? name : p.name;
        var builtFrom = name !== '' ? p.name : '';
        return splash + '<div class="metaline"><span class="nm">' + esc(heading) + '</span>' +
            (builtFrom !== '' ? '<span class="sub">' + esc(builtFrom) + '</span>' : '') + '</div>';
    }

    function signedInHtml() {
        if (S.step === 'welcome') return '';
        var who = S.step === 'tenant-setup' ? tenantAdmin() : S.admin.username;
        return '<p class="signedin">Signed in as <span class="mono">' + esc(who) + '</span></p>';
    }

    /* The page-level alert a failed step draws above the rail. */
    function failureHtml() {
        if (S.stepFailure === undefined) return '';
        return '<div class="notice error" role="alert">' +
            '<p class="errhead">The setup stopped on an error</p>' +
            '<p class="errmsg">' + esc(S.stepFailure) + '</p></div>';
    }

    /* The banner a failed request draws above the screen. */
    function renderBanner() {
        var el = document.getElementById('banner');
        if (S.banner === undefined) { el.innerHTML = ''; return; }
        el.innerHTML = '<div class="errorbanner"><div class="notice error" role="alert">' +
            '<div class="bannerhead"><p class="errhead">A request did not finish</p></div>' +
            '<ul class="bannerlist"><li><p class="errmsg">' + esc(S.banner) + '</p></li></ul>' +
            '</div></div>';
    }

    function render() {
        if (S.step === 'tenant-setup') {
            document.getElementById('app').innerHTML =
                '<div class="page">' + signedInHtml() +
                '<section class="card tenant-setup">' +
                '<div class="stepheader"><img class="splash" src="ore-studio-splash.png" alt="ORE Studio"></div>' +
                '<h2>Finish setting up your tenant</h2>' +
                "<p class=\"lead\">Your tenant's setup runs on the server. You can leave this page and come back.</p>" +
                tenantSetupBody() + '</section></div>';
            renderBanner();
            renderNote();
            renderBar();
            return;
        }
        var list = stepList();
        var at = here();
        if (at < 0) { S.step = 'profile'; list = stepList(); at = here(); }
        var step = list[at];
        var next = nextOf(step);
        var backAllowed = at > 0 && list[at - 1].final !== true;
        var foot = '';
        if (next !== null || backAllowed) {
            foot = '<div class="stepfoot">' +
                (backAllowed ? '<button type="button" class="btn ghost" data-act="back">Back</button>' : '') +
                (next !== null
                    ? '<button type="button" class="btn primary ml-auto" data-act="next"' +
                      (next.enabled ? '' : ' disabled') + '>' + esc(next.label) + '</button>'
                    : '') + '</div>';
        }
        document.getElementById('app').innerHTML =
            '<div class="page">' +
            '<h1>Set up ORE Studio</h1>' + signedInHtml() + failureHtml() +
            '<div class="journey">' + railHtml(list, at) +
            '<section class="card">' +
            '<div class="stepheader">' + headerHtml() + '</div>' +
            (S.profilesFailed
                ? '<div class="notice error" role="alert">The starting points could not be read, so there is ' +
                  'nothing to build a tenant from. ' + esc(S.banner) + '</div>'
                : '') +
            '<h2>' + esc(step.title) + '</h2><p class="lead">' + esc(step.lead) + '</p>' +
            stepBody(step) + foot + '</section></div></div>';
        renderBanner();
        renderNote();
        renderBar();
    }

    function renderNote() {
        var p = profile();
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 journey First run \u00b7 starting point ' +
            (p ? p.name : 'none') + ' \u00b7 state ' + S.step;
    }

    function renderBar() {
        var states = STEPS.map(function (s) {
            return '<button data-act="state" data-state="' + s.id + '"' +
                (S.step === s.id ? ' class="on"' : '') + '>' + esc(s.id) + '</button>';
        }).join('') + '<button data-act="state" data-state="tenant-setup"' +
            (S.step === 'tenant-setup' ? ' class="on"' : '') + '>tenant-setup</button>';
        var profiles = '<span class="label">starting point</span>' +
            '<button data-act="profile" data-profile=""' +
            (S.profileCode === undefined ? ' class="on"' : '') + '>none</button>' +
            cards().map(function (p) {
                return '<button data-act="profile" data-profile="' + p.code + '"' +
                    (S.profileCode === p.code ? ' class="on"' : '') + '>' + esc(p.name) + '</button>';
            }).join('');
        var runs = '<span class="label">run</span>' + RUN_STATES.map(function (r) {
            return '<button data-act="run" data-run="' + r + '">' + r + '</button>';
        }).join('');
        var extras = '<span class="sep">|</span>' +
            '<button data-act="open"' + (S.open ? ' class="on"' : '') + '>settings form</button>' +
            '<button data-act="fail"' + (S.failOnce ? ' class="on"' : '') + '>fail step 3</button>' +
            '<button data-act="adminresume"' + (S.adminMode === 'resume' ? ' class="on"' : '') +
            '>administrator exists</button>' +
            '<button data-act="stepfail"' + (S.stepFailure !== undefined ? ' class="on"' : '') +
            '>step failure</button>' +
            '<button data-act="request"' + (S.banner !== undefined ? ' class="on"' : '') +
            '>request failure</button>';
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">state</span>' + states + '<span class="sep">|</span>' + profiles +
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
                S.live = false;
                render();
                return;
            }
            S.cursor += 1;
            if (S.cursor >= total) {
                S.live = false;
                S.runComplete = true;
            }
            render();
            if (S.live) scheduleTick();
        }, 900);
    }

    function startRun() {
        S.live = true;
        S.started = true;
        S.cursor = 0;
        S.runFailed = false;
        S.failedOnce = false;
        S.runComplete = false;
        S.retryNote = undefined;
        scheduleTick();
    }

    function goNext() {
        var step = stepList()[here()];
        if (step === undefined) return;
        S.stepFailure = undefined;
        if (step.id === 'review') startRun();
        if (step.id === 'ready') {
            S.after = 'The application opens here, and the browser stays signed in as the administrator.';
            rerender();
            return;
        }
        var list = stepList();
        var at = here();
        if (at + 1 < list.length) S.step = list[at + 1].id;
        rerender();
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'state') {
            S.step = el.getAttribute('data-state');
            if (S.step === 'tenant-setup') tenantRunSnapshot('running');
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
        } else if (act === 'adminresume') {
            S.adminMode = S.adminMode === 'resume' ? 'create' : 'resume';
        } else if (act === 'stepfail') {
            S.stepFailure = S.stepFailure === undefined
                ? 'Provisioning request failed: the server did not answer.'
                : undefined;
        } else if (act === 'request') {
            if (S.banner === undefined) {
                S.banner = 'Request failed: 503 Service Unavailable';
                S.profilesFailed = true;
            } else {
                S.banner = undefined;
                S.profilesFailed = false;
            }
        } else if (act === 'tenant-setup') {
            tenantRunSnapshot('running');
        } else if (act === 'enter-app') {
            S.after = 'The application opens here, and the browser stays signed in as the tenant administrator.';
            S.step = 'ready';
        } else if (act === 'entity-pick') {
            var param = el.getAttribute('data-param');
            var lei = el.getAttribute('data-lei');
            S.details.parameters[param] = lei;
            S.entity = ENTITIES.filter(function (e) { return e.lei === lei; })[0];
            S.entityChanging = false;
            S.entityQuery = '';
        } else if (act === 'entity-change') {
            S.entityChanging = true;
        } else if (act === 'entity-keep') {
            S.entityChanging = false;
            S.entityQuery = '';
        } else if (act === 'pw-toggle') {
            var st = pwState(el.getAttribute('data-pw'));
            st.reveal = !st.reveal;
        } else if (act === 'back') {
            var list = stepList();
            var at = here();
            if (at > 0 && list[at - 1].final !== true) S.step = list[at - 1].id;
        } else if (act === 'next') {
            if (!el.disabled) goNext();
        } else if (act === 'retry') {
            if (S.step === 'tenant-setup') {
                S.tenantFailed = false;
                S.tenantStarted = true;
            } else {
                S.runFailed = false;
                S.live = true;
                var p = profile();
                S.retryNote = 'The run resumed at ' +
                    (p !== undefined && p.steps[S.cursor] !== undefined ? p.steps[S.cursor] : 'the failed step') +
                    '.';
                scheduleTick();
            }
        } else {
            return;
        }
        rerender();
    });

    function inputChanged(el) {
        if (el.getAttribute('data-fail') !== null) { S.failOnce = el.checked; return true; }
        var a = el.getAttribute('data-a');
        if (a !== null) { S.admin[a] = el.value; return true; }
        var r = el.getAttribute('data-r');
        if (r !== null) { S.resume[r] = el.value; return true; }
        var eq = el.getAttribute('data-eq');
        if (eq !== null) { S.entityQuery = el.value; return true; }
        var key = el.getAttribute('data-pw');
        if (key !== null) {
            var st = pwState(key);
            var part = el.getAttribute('data-part');
            if (part === 'value') st.value = el.value; else st.confirm = el.value;
            var ok = assess(st.value).valid && st.value === st.confirm;
            if (key === 'admin') S.admin.ok = ok;
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
        if (p !== null) { S.details.parameters[p] = el.value; return true; }
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
