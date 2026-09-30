/* Application navigation prototype. Self-contained: plain JavaScript, mock
 * data, no framework, no build step, and nothing that outlives the page.
 *
 * One question, five answers. Every journey in the catalogue reaches its
 * screen by the person "choosing" it, and no document owns how. The catalogue
 * holds 21 journeys in 8 groups and grows, and a person sees only the ones
 * they may run, so the answer has to survive that scale. The variants differ
 * on nothing else. */

(function () {
    'use strict';

    var VARIANTS = {
        N: {
            name: 'Navbar',
            note: 'The groups sit in the header as a horizontal bar; a group opens its index, and the index lists its journeys. Nothing is hidden, and the arithmetic is plain: eight groups across the header.'
        },
        S: {
            name: 'Sidebar',
            note: 'The same groups down the left, the current one open to its journeys. It holds eight groups without crowding and it can nest, at the cost of a column on every screen.'
        },
        I: {
            name: 'Index',
            note: 'No global navigation at all: the landing page is the index, every journey on it, grouped by topic. Nothing to hunt for, and nothing in the way while a journey runs.'
        },
        D: {
            name: 'Data only',
            note: 'No navigation either: a journey is reached from the thing it acts on. Your own record opens your screens, a roster row opens that colleague. The catalogue assumes this, and it needs the owning screens to exist.'
        },
        A: {
            name: 'Account menu',
            note: 'The header trigger, as prototyped first. Kept because it is the shape the journey documents name, and because the comparison needs it: at this scale it holds neither the administrator\u2019s work nor the groups.'
        }
    };

    var STATES = [
        ['home', 'At rest'],
        ['journey', 'On a journey'],
        ['fullscreen', 'Full screen'],
        ['today', 'Today'],
        ['narrow', 'Narrow']
    ];

    /* The catalogue as it stands: 8 groups, 21 journeys. `who` is the person a
       journey is for: me for the member's own screens, admin for the tenant's,
       all for either. `landed` is whether the group has been built, which is
       what the today state shows. */
    var GROUPS = [
        { name: 'Setup', landed: true, journeys: [
            ['First run', '/setup/first-run', 'admin'],
            ['New tenant', '/tenants/new', 'admin'],
            ['New party', '/parties/new', 'admin']] },
        { name: 'Entry', landed: false, nav: false, journeys: [
            ['Sign in', '/login', 'all'],
            ['Sign up', '/sign-up', 'all']] },
        { name: 'Profile', landed: false, journeys: [
            ['Present myself', '/profile', 'me'],
            ['Keep my details current', '/profile/details', 'me'],
            ["Change someone's details", '/people/:id', 'admin']] },
        { name: 'Credentials', landed: false, journeys: [
            ['Protect my account', '/profile/security', 'me'],
            ['Rescue access', '/people/:id/rescue', 'admin'],
            ['Audit sign-ins', '/audit/sign-ins', 'admin']] },
        { name: 'Access', landed: false, journeys: [
            ['Know what I may do', '/profile/access', 'me'],
            ['Ask for more access', '/profile/access/request', 'me'],
            ["Change someone's access", '/people/:id/access', 'admin'],
            ['Shape the role catalogue', '/roles', 'admin']] },
        { name: 'Membership', landed: false, journeys: [
            ['Choose where I work', '/work', 'me'],
            ['Draw the reporting line', '/reporting-lines', 'admin']] },
        { name: 'Directory', landed: false, journeys: [
            ['See who has access', '/people', 'admin'],
            ['Bring someone in', '/people/new', 'admin'],
            ['Register a service account', '/service-accounts/new', 'admin']] },
        { name: 'Tenancy', landed: false, journeys: [
            ['Tune the tenant', '/tenant', 'admin'],
            ['Retire or reset a tenant', '/tenant/retire', 'admin']] }
    ];

    var PERSON = {
        member: {
            username: 'jdoe', name: 'Jane Doe', tenant: 'Northwind Capital',
            party: 'Trading', title: 'Risk Analyst', photo: 'photos/jane_doe.jpeg'
        },
        admin: {
            username: 'rsmith', name: 'R. Smith', tenant: 'Northwind Capital',
            party: 'Operations', title: 'Tenant Administrator', photo: 'photos/rsmith.jpeg'
        }
    };

    var S = {
        variant: 'S',
        actor: 'admin',
        state: 'home',
        at: '/profile',
        open: false
    };

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var v = (p.get('variant') || '').toUpperCase();
        if (VARIANTS[v]) S.variant = v;
        if (p.get('actor') === 'admin' || p.get('actor') === 'member') S.actor = p.get('actor');
        var st = p.get('state');
        for (var i = 0; i < STATES.length; i++) if (STATES[i][0] === st) S.state = st;
        if (p.get('screen')) S.at = p.get('screen');
        if (p.get('open') === '1') S.open = true;
    }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function person() { return PERSON[S.actor]; }
    function isAdmin() { return S.actor === 'admin'; }
    function narrow() { return S.state === 'narrow'; }
    function here(path) { return path === S.at; }

    /* Only the journeys this person may run: a member sees their own screens,
       an administrator sees those and the tenant's. Entry is the door and is
       not a place a signed-in person navigates to, so it is not here. */
    function allowed(entry) {
        return entry[2] === 'all' || entry[2] === 'me' || isAdmin();
    }

    function groups(todayOnly) {
        return GROUPS.filter(function (g) { return g.nav !== false; })
          .map(function (g) {
              return { name: g.name, landed: g.landed, journeys: g.journeys.filter(allowed) };
          })
          .filter(function (g) { return g.journeys.length > 0; })
          .filter(function (g) { return !todayOnly || g.landed; });
    }

    function journeyCount(list) {
        return list.reduce(function (n, g) { return n + g.journeys.length; }, 0);
    }

    function currentGroup(list) {
        var found = null;
        (list || GROUPS).forEach(function (g) {
            if (g.journeys.some(function (j) { return here(j[1]); })) found = g;
        });
        return found;
    }

    // ------------------------------------------------------------- chrome

    function face(cls) {
        return '<span class="' + cls + '"><img src="' + person().photo + '" alt=""></span>';
    }

    function accountChip(withMenu) {
        var p = person();
        var body = face('face') +
            '<span class="who"><span class="nm">' + esc(p.name) + '</span>' +
            '<span class="sub">' + esc(p.username) + '</span></span>';
        if (!withMenu) {
            return '<a class="trigger" href="#" data-act="go" data-screen="/profile">' + body + '</a>';
        }
        return '<button class="trigger' + (S.open ? ' open' : '') + '" data-act="toggle">' + body +
            '<span class="caret">' + (S.open ? '\u25b2' : '\u25bc') + '</span></button>';
    }

    function accountMenu() {
        if (!S.open) return '';
        var p = person();
        var list = groups(false);
        return '<div class="menu">' +
            '<div class="head">' + face('face') +
            '<div><div class="nm">' + esc(p.name) + '</div>' +
            '<div class="sub">' + esc(p.title) + ' \u00b7 ' + esc(p.party) + '</div></div></div>' +
            list.map(function (g) {
                return '<div class="group"><div class="groupname">' + esc(g.name) + '</div>' +
                    g.journeys.map(function (j) {
                        return '<a href="#" data-act="go" data-screen="' + j[1] + '"' + (here(j[1]) ? ' class="here"' : '') + '>' +
                            esc(j[0]) + (g.landed ? '' : '<span class="soon">' + esc(g.name) + '</span>') + '</a>';
                    }).join('') + '</div>';
            }).join('') +
            '<div class="group"><button class="item out" data-act="signout">Sign out</button></div></div>';
    }

    function brand() {
        return '<a class="brand" href="#" data-act="home"><span class="mark">O</span><span class="name">ORE Studio</span></a>';
    }

    function navbar() {
        return '<nav class="appnav">' + groups(S.state === 'today').map(function (g) {
            var open = g.journeys.some(function (j) { return here(j[1]); });
            return '<a href="#" data-act="group" data-group="' + esc(g.name) + '"' + (open ? ' class="here"' : '') + '>' +
                esc(g.name) + '<span class="count">' + g.journeys.length + '</span></a>';
        }).join('') + '</nav>';
    }

    function sidebar() {
        var current = currentGroup(groups(false));
        return '<nav class="side">' + groups(S.state === 'today').map(function (g) {
            var open = current && current.name === g.name;
            return '<div class="sidegroup' + (open ? ' open' : '') + '">' +
                '<a href="#" data-act="group" data-group="' + esc(g.name) + '">' + esc(g.name) +
                '<span class="count">' + g.journeys.length + '</span></a>' +
                (open ? '<div class="sideitems">' + g.journeys.map(function (j) {
                    return '<a href="#" data-act="go" data-screen="' + j[1] + '"' + (here(j[1]) ? ' class="here"' : '') + '>' +
                        esc(j[0]) + '</a>';
                }).join('') + '</div>' : '') + '</div>';
        }).join('') + '</nav>';
    }

    // --------------------------------------------------------------- body

    var SCREENS = {
        '/profile': ['My profile', 'Your photo, your name, your job title and how colleagues reach you.'],
        '/profile/details': ['My details', 'Your address, telephone and web page.'],
        '/profile/security': ['Security', 'Your password, your sign-in state and where you are signed in.'],
        '/profile/access': ['My access', 'The roles you hold and what they let you do.'],
        '/work': ['Where I work', 'The parties you work in, and the one you act for.'],
        '/people': ['See who has access', 'The account roster for this tenant.'],
        '/people/:id': ["Change someone's details", 'One colleague\u2019s record, as they see it.'],
        '/reporting-lines': ['Reporting lines', 'Who reports to whom, drawn as a tree.'],
        '/roles': ['Roles', 'The roles this tenant defines and what each bundles.'],
        '/tenants/new': ['New tenant', 'Stand up a tenant, one step at a time.'],
        '/setup/first-run': ['First run', 'An empty installation, from nothing to working.']
    };

    function reachedFrom() {
        if (S.variant === 'A') return 'the account menu';
        if (S.variant === 'N') return 'the navbar, through the group';
        if (S.variant === 'S') return 'the sidebar, through the group';
        if (S.variant === 'I') return 'the index page';
        return 'the thing it acts on';
    }

    function journeyScreen() {
        var s = SCREENS[S.at] || SCREENS['/profile'];
        var p = person();
        var visible = groups(S.state === 'today');
        return '<div class="card">' +
            '<div class="crumb">' + esc(p.tenant) + ' \u00b7 ' + esc(p.party) + '</div>' +
            '<h1>' + esc(s[0]) + '</h1>' +
            '<p class="lead">' + esc(s[1]) + '</p>' +
            '<div class="panels">' +
            '<div class="mini"><h2>The journey</h2><table class="kv">' +
            '<tr><td>Route</td><td>' + esc(S.at) + '</td></tr>' +
            '<tr><td>Reached from</td><td>' + esc(reachedFrom()) + '</td></tr>' +
            '</table></div>' +
            '<div class="mini"><h2>The navigation</h2><table class="kv">' +
            '<tr><td>Model</td><td>' + esc(VARIANTS[S.variant].name) + '</td></tr>' +
            '<tr><td>Visible groups</td><td>' + visible.length + '</td></tr>' +
            '<tr><td>Visible journeys</td><td>' + journeyCount(visible) + '</td></tr>' +
            '</table></div></div></div>';
    }

    function indexPage(oneGroup) {
        var list = oneGroup
            ? groups(false).filter(function (g) { return g.name === oneGroup; })
            : groups(S.state === 'today');
        var p = person();
        var empty = list.length === 0;
        var head = oneGroup
            ? '<div class="crumb"><a href="#" data-act="home">Home</a> \u00b7 ' + esc(oneGroup) + '</div>' +
              '<h1>' + esc(oneGroup) + '</h1>' +
              '<p class="lead">The journeys in this group, for ' +
              (isAdmin() ? 'a tenant administrator' : 'a member') + '.</p>'
            : '<div class="crumb">' + esc(p.tenant) + ' \u00b7 ' + esc(p.party) + '</div>' +
              '<h1>' + (empty ? 'Nothing here yet' : 'What you can do here') + '</h1>' +
              (empty
                  ? '<p class="lead">No journey has been built for this person yet. The shell holds none of them ' +
                    'hostage: the groups appear here as they land.</p>'
                  : '<p class="lead">' + journeyCount(list) + ' journey' + (journeyCount(list) === 1 ? '' : 's') +
                    ' in ' + list.length + ' group' + (list.length === 1 ? '' : 's') +
                    ', for ' + (isAdmin() ? 'a tenant administrator' : 'a member') + '.' +
                    (S.state === 'today' ? ' Only the groups that have been built are listed.' : '') + '</p>');
        var body = list.map(function (g) {
            return '<section class="groupcard"><h2>' + esc(g.name) +
                (g.landed ? '' : '<span class="tag soon">not built yet</span>') + '</h2>' +
                '<ul>' + g.journeys.map(function (j) {
                    return '<li><a href="#" data-act="go" data-screen="' + j[1] + '">' + esc(j[0]) + '</a></li>';
                }).join('') + '</ul></section>';
        }).join('');
        return '<div class="card">' + head + '</div>' +
            (body ? '<div class="groupgrid">' + body + '</div>' : '');
    }

    /* The data-only model has no navigation, so its landing surface is the
       person's own record and the roster: the things a journey hangs off. */
    function dataPage() {
        var p = person();
        return '<div class="card"><div class="crumb">' + esc(p.tenant) + ' \u00b7 ' + esc(p.party) + '</div>' +
            '<h1>' + esc(p.name) + '</h1>' +
            '<p class="lead">This person\u2019s record, and the rows that lead to other records. There is no navigation: the entry point is the thing the journey acts on.</p>' +
            '<div class="panels">' +
            '<div class="mini"><h2>Your record</h2>' +
            '<a class="rowlink" href="#" data-act="go" data-screen="/profile">Your profile, photo and contact details</a>' +
            '<a class="rowlink" href="#" data-act="go" data-screen="/profile/security">Your password and sign-in state</a>' +
            '<a class="rowlink" href="#" data-act="go" data-screen="/profile/access">The roles you hold</a>' +
            '</div>' +
            '<div class="mini"><h2>The roster</h2>' +
            '<a class="rowlink" href="#" data-act="go" data-screen="/people">See who has access</a>' +
            (isAdmin()
                ? '<a class="rowlink" href="#" data-act="go" data-screen="/people/:id">A colleague\u2019s record</a>' +
                  '<a class="rowlink" href="#" data-act="go" data-screen="/reporting-lines">The reporting lines</a>'
                : '') +
            '</div></div></div>';
    }

    /* A journey that takes over the screen: the first-run journeys stand an
       installation up, so there is nothing to navigate to until they finish,
       and the shell is not drawn at all. Every model renders this the same,
       which is the point -- the shell has a boundary. */
    var SETUP_STEPS = ['Administrator', 'Tenant', 'Seed profiles', 'Sign in', 'Ready'];

    function fullscreenPage() {
        var at = 1;
        var rail = '<nav class="railnav"><ol>' + SETUP_STEPS.map(function (s, i) {
            var cls = i === at ? 'current' : (i < at ? 'done' : 'ahead');
            return '<li class="railentry ' + cls + '"><span class="railmark ' + cls + '">' +
                (i < at ? '\u2713' : String(i + 1)) + '</span>' + esc(s) + '</li>';
        }).join('') + '</ol></nav>';
        var card = '<div class="card"><h1>New tenant</h1>' +
            '<p class="lead">This journey stands up the installation, so it owns the whole screen and offers no way ' +
            'past it until it is done. The shell is not drawn: there is nothing yet to navigate to.</p>' +
            '<div class="mini"><h2>Step 2 of 5 \u00b7 Tenant</h2>' +
            '<table class="kv">' +
            '<tr><td>Shell drawn</td><td>no</td></tr>' +
            '<tr><td>Models that differ</td><td>none: this is outside all five</td></tr>' +
            '<tr><td>Way out</td><td>Exit, which abandons the run</td></tr>' +
            '</table></div>' +
            '<div class="stepfoot"><button class="btn ghost" data-act="go" data-screen="/">Exit setup</button>' +
            '<button class="btn primary mlauto" data-act="go" data-screen="/">Continue</button></div></div>';
        return '<div class="journey">' + rail + '<div>' + card + '</div></div>';
    }

    function body() {
        if (S.state === 'fullscreen') return fullscreenPage();
        /* At rest is the landing surface, and today is the same surface with
           only the groups that exist. */
        var atRest = S.state === 'home' || S.state === 'today';
        if (atRest && S.variant === 'D') return dataPage();
        if (atRest) return indexPage(null);
        return journeyScreen();
    }

    // --------------------------------------------------------------- page

    function render() {
        var full = S.state === 'fullscreen';
        var head = '';
        if (!full) {
            if (S.variant === 'N') {
                head = '<header class="appheader"><div class="appheader-inner">' + brand() + navbar() +
                    '<div class="accounts">' + accountChip(false) + '</div></div></header>';
            } else if (S.variant === 'A') {
                head = '<header class="appheader"><div class="appheader-inner">' + brand() +
                    '<div class="mlauto"></div><div class="accounts">' + accountChip(true) + accountMenu() + '</div></div></header>';
            } else {
                head = '<header class="appheader"><div class="appheader-inner">' + brand() +
                    '<div class="mlauto"></div><div class="accounts">' + accountChip(false) + '</div></div></header>';
            }
        }

        var shell = '<div class="shell' + (narrow() ? ' narrow' : '') + '">' + head;
        shell += (S.variant === 'S' && !full)
            ? '<div class="withside">' + sidebar() + '<main>' + body() + '</main></div>'
            : '<main>' + body() + '</main>';
        shell += '</div>';
        document.getElementById('app').innerHTML = shell;

        var visible = groups(S.state === 'today');
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 ' + VARIANTS[S.variant].name +
            ' \u00b7 ' + (isAdmin() ? 'tenant administrator' : 'member') +
            ' \u00b7 state ' + S.state +
            ' \u00b7 ' + visible.length + ' groups, ' + journeyCount(visible) + ' journeys';

        renderBar();
    }

    function renderBar() {
        var variants = Object.keys(VARIANTS).map(function (k) {
            return '<button data-act="variant" data-variant="' + k + '"' + (S.variant === k ? ' class="on"' : '') + '>' +
                esc(VARIANTS[k].name) + '</button>';
        }).join('');
        var actors = '<span class="label">actor</span>' +
            '<button data-act="actor" data-actor="member"' + (!isAdmin() ? ' class="on"' : '') + '>member</button>' +
            '<button data-act="actor" data-actor="admin"' + (isAdmin() ? ' class="on"' : '') + '>admin</button>';
        var states = STATES.map(function (s) {
            return '<button data-act="state" data-state="' + s[0] + '"' + (S.state === s[0] ? ' class="on"' : '') + '>' + esc(s[1]) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">model</span>' + variants +
            '<span class="sep">|</span>' + actors + '<span class="sep">|</span>' + states;
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'variant') { S.variant = el.getAttribute('data-variant'); S.open = false; }
        else if (act === 'actor') { S.actor = el.getAttribute('data-actor'); S.open = false; }
        else if (act === 'state') { S.state = el.getAttribute('data-state'); S.open = false; }
        else if (act === 'toggle') S.open = !S.open;
        else if (act === 'home') S.at = '/';
        else if (act === 'group') { S.at = '/'; S.homeGroup = el.getAttribute('data-group'); }
        else if (act === 'go') S.at = el.getAttribute('data-screen');
        else if (act === 'signout') S.open = false;
        render();
    });

    document.addEventListener('keydown', function (ev) {
        var order = Object.keys(VARIANTS);
        var at = order.indexOf(S.variant);
        if (ev.key === 'ArrowRight') S.variant = order[(at + 1) % order.length];
        else if (ev.key === 'ArrowLeft') S.variant = order[(at + order.length - 1) % order.length];
        else return;
        S.open = false;
        render();
    });

    readParams();
    render();
})();
