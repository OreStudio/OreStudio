/* Application shell prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * One design, not a comparison. A menu across the header, the cards it opens,
 * and the person's own journeys under their name.
 *
 * What you see is decided by who you signed in as. A super administrator works
 * in the system tenant's context, a tenant administrator in a tenant's, and a
 * party user in a party's; the server scopes everything by that context
 * already, so the mode is read from the sign-in and never chosen. */

(function () {
    'use strict';

    /* The four people this prototype can sign in as. Each lands in the context
       its account belongs to, which is the mode. */
    var WHO = [
        { id: 'super', name: 'Super administrator', mode: 'system', rank: 4,
          blurb: 'The deployment: its tenants, the registry they share, and standing an installation up.' },
        { id: 'tenant', name: 'Tenant administrator', mode: 'tenant', rank: 3,
          blurb: 'The tenant: its parties, its role catalogue and its own settings. Not its data.' },
        { id: 'privileged', name: 'Privileged party user', mode: 'party', rank: 2,
          blurb: 'A party\u2019s work, and the people who do it.' },
        { id: 'regular', name: 'Regular party user', mode: 'party', rank: 1,
          blurb: 'A party\u2019s work.' }
    ];

    /* Where a mode shows its menu. An area the catalogue has not reached is
       still listed, because the area exists and hiding it would pretend the
       work is smaller than it is. */
    var MENU = {
        system: ['Tenants', 'Bootstrap'],
        tenant: ['Parties', 'Access', 'Tenant'],
        party: ['Reference Data', 'Market Data', 'Trading', 'Analytics', 'Compute', 'Reporting', 'People']
    };

    var MODES = {
        system: 'System administration',
        tenant: 'Tenant administration',
        party: 'Application'
    };

    /* Every journey the catalogue holds, in the context it is done in. `min` is
       the least person who may run it: 1 a party user, 2 a privileged one,
       3 the tenant administrator, 4 the super administrator. `area` is the menu
       it sits under and `card` the heading it is gathered under. */
    var JOURNEYS = [
        // The deployment.
        { name: 'First run', path: '/setup/first-run', mode: 'system', area: 'Bootstrap', card: 'Bootstrap', min: 4, run: true, built: true },
        { name: 'New tenant', path: '/tenants/new', mode: 'system', area: 'Tenants', card: 'Tenants', min: 4, run: true },
        { name: 'Retire or reset a tenant', path: '/tenants/retire', mode: 'system', area: 'Tenants', card: 'Tenants', min: 4 },

        // The tenant.
        { name: 'New party', path: '/parties/new', mode: 'tenant', area: 'Parties', card: 'Parties', min: 3, run: true, built: true },
        { name: 'Shape the role catalogue', path: '/roles', mode: 'tenant', area: 'Access', card: 'Access', min: 3 },
        { name: 'Tune the tenant', path: '/tenant', mode: 'tenant', area: 'Tenant', card: 'Tenant', min: 3 },

        // The people of a party. A regular member sees none of these.
        { name: 'See who has access', path: '/people', mode: 'party', area: 'People', card: 'Directory', min: 2 },
        { name: 'Bring someone in', path: '/people/new', mode: 'party', area: 'People', card: 'Directory', min: 2 },
        { name: 'Register a service account', path: '/service-accounts/new', mode: 'party', area: 'People', card: 'Directory', min: 2 },
        { name: "Change someone's details", path: '/people/:id', mode: 'party', area: 'People', card: 'Profile', min: 2 },
        { name: "Change someone's access", path: '/people/:id/access', mode: 'party', area: 'People', card: 'Access', min: 2 },
        { name: 'Rescue access', path: '/people/:id/rescue', mode: 'party', area: 'People', card: 'Credentials', min: 2 },
        { name: 'Audit sign-ins', path: '/audit/sign-ins', mode: 'party', area: 'People', card: 'Credentials', min: 2 },
        { name: 'Draw the reporting line', path: '/reporting-lines', mode: 'party', area: 'People', card: 'Membership', min: 2 },

        // The person themselves. These are never a menu: they are the avatar.
        { name: 'Present myself', path: '/profile', mode: 'any', area: 'me', card: 'Profile', min: 1 },
        { name: 'Keep my details current', path: '/profile/details', mode: 'any', area: 'me', card: 'Profile', min: 1 },
        { name: 'Protect my account', path: '/profile/security', mode: 'any', area: 'me', card: 'Credentials', min: 1 },
        { name: 'Know what I may do', path: '/profile/access', mode: 'party', area: 'me', card: 'Access', min: 1 },
        { name: 'Ask for more access', path: '/profile/access/request', mode: 'party', area: 'me', card: 'Access', min: 1 },
        { name: 'Choose where I work', path: '/work', mode: 'party', area: 'me', card: 'Membership', min: 1 }
    ];

    var PERSON = {
        super: { username: 'root', name: 'A. Root', tenant: 'ORE Studio', party: '\u2014', title: 'Super Administrator', photo: 'photos/rsmith.jpeg' },
        tenant: { username: 'rsmith', name: 'R. Smith', tenant: 'Northwind Capital', party: '\u2014', title: 'Tenant Administrator', photo: 'photos/rsmith.jpeg' },
        privileged: { username: 'tokafor', name: 'Tom Okafor', tenant: 'Northwind Capital', party: 'Trading', title: 'Head of Trading', photo: 'photos/jane_doe.jpeg' },
        regular: { username: 'jdoe', name: 'Jane Doe', tenant: 'Northwind Capital', party: 'Trading', title: 'Risk Analyst', photo: 'photos/jane_doe.jpeg' }
    };

    var S = { who: 'tenant', state: 'home', area: null, at: '/profile', open: false };

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (WHO.some(function (w) { return w.id === p.get('as'); })) S.who = p.get('as');
        if (['home', 'journey', 'fullscreen'].indexOf(p.get('state')) >= 0) S.state = p.get('state');
        if (p.get('area')) S.area = p.get('area');
        if (p.get('screen')) S.at = p.get('screen');
        if (p.get('open') === '1') S.open = true;
    }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function me() { return WHO.filter(function (w) { return w.id === S.who; })[0]; }
    function person() { return PERSON[S.who]; }
    function mode() { return me().mode; }

    /* What this person may run, here. */
    function allowed(j) {
        if (j.area === 'me') return j.mode === 'any' || j.mode === mode();
        return j.mode === mode() && j.min <= me().rank;
    }

    function mine(area) {
        return JOURNEYS.filter(function (j) { return j.area === area && allowed(j); });
    }

    function meJourneys() {
        return JOURNEYS.filter(function (j) { return j.area === 'me' && allowed(j); });
    }

    function cardsFor(area) {
        var seen = [];
        mine(area).forEach(function (j) {
            if (seen.indexOf(j.card) < 0) seen.push(j.card);
        });
        return seen.map(function (name) {
            return { name: name, journeys: mine(area).filter(function (j) { return j.card === name; }) };
        });
    }

    /* The menu holds the areas that are this person's, and the areas that do
       not exist yet. An area that exists and belongs to somebody else is not
       in it: a party user is not shown the door to the people of the party. */
    function menuAreas() {
        return MENU[mode()].filter(function (a) {
            var forAnyone = JOURNEYS.filter(function (j) {
                return j.area === a && (j.mode === mode() || j.mode === 'any');
            });
            return mine(a).length > 0 || forAnyone.length === 0;
        });
    }

    function here(path) { return path === S.at; }

    // ------------------------------------------------------------- chrome

    function face() {
        return '<span class="face"><img src="' + person().photo + '" alt=""></span>';
    }

    function menu() {
        return '<nav class="appnav">' + menuAreas().map(function (a) {
            var n = mine(a).length;
            return '<a href="#" data-act="area" data-area="' + esc(a) + '"' +
                (S.area === a ? ' class="here"' : '') + (n ? '' : ' data-empty="1"') + '>' +
                esc(a) + (n ? '<span class="count">' + n + '</span>' : '') + '</a>';
        }).join('') + '</nav>';
    }

    function personChip() {
        return '<button class="trigger' + (S.open ? ' open' : '') + '" data-act="toggle">' +
            face() + '<span class="who"><span class="nm">' + esc(person().name) + '</span>' +
            '<span class="sub">' + esc(person().username) + '</span></span>' +
            '<span class="caret">' + (S.open ? '\u25b2' : '\u25bc') + '</span></button>';
    }

    function myMenu() {
        if (!S.open) return '';
        var p = person();
        var seen = [];
        meJourneys().forEach(function (j) {
            if (seen.indexOf(j.card) < 0) seen.push(j.card);
        });
        var groups = seen.map(function (cardName) {
            return '<div class="group"><div class="groupname">' + esc(cardName) + '</div>' +
                meJourneys().filter(function (j) { return j.card === cardName; }).map(function (j) {
                    return '<a href="#" data-act="go" data-screen="' + j.path + '"' + (here(j.path) ? ' class="here"' : '') + '>' +
                        esc(j.name) + '</a>';
                }).join('') + '</div>';
        }).join('');
        return '<div class="menu">' +
            '<div class="head">' + face() +
            '<div><div class="nm">' + esc(p.name) + '</div>' +
            '<div class="sub">' + esc(p.title) + ' \u00b7 ' + esc(MODES[mode()]) + '</div></div></div>' +
            (groups || '<div class="group"><div class="none">No journey about you has been built yet.</div></div>') +
            '<div class="group"><button class="item out" data-act="signout">Sign out</button></div></div>';
    }

    // --------------------------------------------------------------- body

    function card(c) {
        var navs = c.journeys.filter(function (j) { return !j.run; });
        var runs = c.journeys.filter(function (j) { return j.run; });
        var runsHtml = runs.length
            ? '<li class="runline">' + (navs.length ? 'Starts a run: ' : '') + runs.map(function (j) {
                  return '<button class="btn small" data-act="run">' + esc(j.name) + '</button>';
              }).join(' ') + '</li>'
            : '';
        return '<section class="groupcard">' +
            '<h2>' + esc(c.name) +
            (c.journeys.every(function (j) { return j.built; }) ? ''
                : '<span class="tag soon">not built yet</span>') + '</h2>' +
            '<ul>' + navs.map(function (j) {
                return '<li><a href="#" data-act="go" data-screen="' + j.path + '">' + esc(j.name) + '</a></li>';
            }).join('') + runsHtml + '</ul></section>';
    }

    function landing() {
        var p = person();
        var areas = menuAreas();
        var total = areas.reduce(function (n, a) { return n + mine(a).length; }, 0);
        var head = '<div class="crumb">' + esc(MODES[mode()]) + ' \u00b7 ' + esc(p.tenant) +
            (p.party === '\u2014' ? '' : ' \u00b7 ' + esc(p.party)) + '</div>' +
            '<h1>' + esc(MODES[mode()]) + '</h1>' +
            '<p class="lead">' + esc(me().blurb) + ' ' +
            (total ? total + ' journey' + (total === 1 ? '' : 's') + ' in this mode.'
                   : 'No journey has been built for this person in this mode yet.') + '</p>';
        var body = areas.map(function (a) {
            var cards = cardsFor(a);
            var n = mine(a).length;
            return '<section class="area"><h2 class="areaname">' + esc(a) +
                (n ? '<span class="tag">' + n + '</span>' : '') + '</h2>' +
                (cards.length
                    ? '<div class="groupgrid">' + cards.map(card).join('') + '</div>'
                    : '<p class="emptyarea">No journeys have been extracted for this area yet. It is named because ' +
                      'it exists; nothing is invented to fill it.</p>') +
                '</section>';
        }).join('');
        return '<div class="card">' + head + '</div>' + body;
    }

    function areaPage() {
        var cards = cardsFor(S.area);
        var total = mine(S.area).length;
        return '<div class="card"><div class="crumb">' + esc(MODES[mode()]) + '</div>' +
            '<h1>' + esc(S.area) + '</h1>' +
            '<p class="lead">' + total + ' journey' + (total === 1 ? '' : 's') + '.</p></div>' +
            (cards.length ? '<div class="groupgrid">' + cards.map(card).join('') + '</div>'
                          : '<p class="emptyarea">No journeys have been extracted for this area yet.</p>');
    }

    var SCREENS = {
        '/profile': ['My profile', 'Your photo, your name, your job title and how colleagues reach you.'],
        '/profile/details': ['My details', 'Your address, telephone and web page.'],
        '/profile/security': ['Security', 'Your password, your sign-in state and where you are signed in.'],
        '/profile/access': ['My access', 'The roles you hold and what they let you do.'],
        '/work': ['Where I work', 'The parties you work in, and the one you act for.'],
        '/people': ['See who has access', 'The account roster for this party.'],
        '/people/:id': ["Change someone's details", 'One colleague\u2019s record, as they see it.'],
        '/reporting-lines': ['Reporting lines', 'Who reports to whom, drawn as a tree.'],
        '/roles': ['Roles', 'The roles this tenant defines and what each bundles.'],
        '/tenants/new': ['New tenant', 'Stand a tenant up, one step at a time.'],
        '/tenant': ['Tune the tenant', 'The tenant\u2019s own settings.'],
        '/setup/first-run': ['First run', 'An empty installation, from nothing to working.']
    };

    function journeyPage() {
        var j = JOURNEYS.filter(function (x) { return x.path === S.at; })[0];
        var s = SCREENS[S.at] || [S.at, ''];
        return '<div class="card">' +
            '<div class="crumb">' + esc(MODES[mode()]) +
            (j && j.area !== 'me' ? ' \u00b7 ' + esc(j.area) : '') + '</div>' +
            '<h1>' + esc(s[0]) + '</h1><p class="lead">' + esc(s[1]) + '</p>' +
            '<div class="mini"><h2>The journey</h2><table class="kv">' +
            '<tr><td>Route</td><td>' + esc(S.at) + '</td></tr>' +
            '<tr><td>Reached from</td><td>' + (j && j.area === 'me' ? 'the avatar menu' : 'the cards') + '</td></tr>' +
            '<tr><td>Mode</td><td>' + esc(MODES[mode()]) + '</td></tr>' +
            '<tr><td>Signed in as</td><td>' + esc(me().name) + '</td></tr>' +
            '</table></div></div>';
    }

    var RUN_STEPS = ['Administrator', 'Tenant', 'Seed profiles', 'Sign in', 'Ready'];

    function runPage() {
        var at = 1;
        var rail = '<nav class="railnav"><ol>' + RUN_STEPS.map(function (s, i) {
            var cls = i === at ? 'current' : (i < at ? 'done' : 'ahead');
            return '<li class="railentry ' + cls + '"><span class="railmark ' + cls + '">' +
                (i < at ? '\u2713' : String(i + 1)) + '</span>' + esc(s) + '</li>';
        }).join('') + '</ol></nav>';
        return '<div class="journey">' + rail + '<div class="card">' +
            '<h1>New tenant</h1>' +
            '<p class="lead">This journey stands the installation up. It owns the whole screen and offers no way ' +
            'past until it is done, so no menu and no cards are drawn while it runs.</p>' +
            '<div class="mini"><h2>Step 2 of 5 \u00b7 Tenant</h2><table class="kv">' +
            '<tr><td>Menu drawn</td><td>no</td></tr>' +
            '<tr><td>Mode</td><td>' + esc(MODES[mode()]) + '</td></tr>' +
            '<tr><td>Way out</td><td>Exit, which abandons the run</td></tr>' +
            '</table></div>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="home">Exit setup</button>' +
            '<button class="btn primary mlauto" data-act="home">Continue</button></div></div></div>';
    }

    function body() {
        if (S.state === 'fullscreen') return runPage();
        if (S.state === 'journey') return journeyPage();
        return S.area ? areaPage() : landing();
    }

    // --------------------------------------------------------------- page

    function render() {
        var full = S.state === 'fullscreen';
        var head = full ? '' : '<header class="appheader"><div class="appheader-inner">' +
            '<a class="brand" href="#" data-act="home"><span class="mark">O</span>' +
            '<span class="name">ORE Studio</span></a>' +
            '<span class="modechip">' + esc(MODES[mode()]) + '</span>' + menu() +
            '<div class="accounts">' + personChip() + myMenu() + '</div></div></header>';
        document.getElementById('app').innerHTML =
            '<div class="shell">' + head + '<main>' + body() + '</main></div>';

        var p = person();
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 signed in as ' + me().name +
            ' \u00b7 ' + MODES[mode()] + ' \u00b7 ' + p.tenant +
            (p.party === '\u2014' ? '' : ' \u00b7 ' + p.party) + ' \u00b7 state ' + S.state;

        renderBar();
    }

    function renderBar() {
        var whos = WHO.map(function (w) {
            return '<button data-act="who" data-who="' + w.id + '"' + (S.who === w.id ? ' class="on"' : '') + '>' +
                esc(w.name) + '</button>';
        }).join('');
        var states = [['home', 'At rest'], ['journey', 'On a journey'], ['fullscreen', 'Full screen']]
            .map(function (s) {
                return '<button data-act="state" data-state="' + s[0] + '"' + (S.state === s[0] ? ' class="on"' : '') + '>' +
                    esc(s[1]) + '</button>';
            }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">sign in as</span>' + whos + '<span class="sep">|</span>' + states;
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'who') { S.who = el.getAttribute('data-who'); S.area = null; S.at = '/profile'; S.state = 'home'; S.open = false; }
        else if (act === 'state') { S.state = el.getAttribute('data-state'); S.open = false; }
        else if (act === 'area') { S.area = el.getAttribute('data-area'); S.state = 'home'; }
        else if (act === 'toggle') S.open = !S.open;
        else if (act === 'home') { S.state = 'home'; S.area = null; }
        else if (act === 'run') { S.state = 'fullscreen'; S.open = false; }
        else if (act === 'go') { S.at = el.getAttribute('data-screen'); S.state = 'journey'; S.open = false; }
        else if (act === 'signout') { S.open = false; S.area = null; S.state = 'home'; }
        render();
    });

    readParams();
    render();
})();
