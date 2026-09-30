/* Account menu prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * One question, three answers. Every journey in the catalogue says a person
 * "chooses" its screen from the account menu, and no document says what the
 * menu holds, where an administrator's screens hang, or what the trigger shows
 * before the account has been read. The variants differ on exactly that. */

(function () {
    'use strict';

    var VARIANTS = {
        A: {
            name: 'Account menu only',
            note: 'The trigger holds the member\u2019s own screens and nothing else. Clean, and honest while the member is the only actor with screens \u2014 but an administrator\u2019s work then has no home at all.'
        },
        B: {
            name: 'One menu',
            note: 'The same trigger holds everything the person may do, grouped: the member\u2019s screens, then the tenant\u2019s, which only an administrator sees. One place to look, and the menu grows with every group.'
        },
        C: {
            name: 'Two navigations',
            note: 'The tenant\u2019s surfaces take the header as a top-level navigation; the trigger keeps the member\u2019s own screens. The shape most products use, and the most chrome for a product with four screens.'
        }
    };

    var STATES = [
        ['closed', 'At rest'],
        ['open', 'Menu open'],
        ['current', 'On a screen'],
        ['updated', 'After a save'],
        ['pending', 'Not read yet'],
        ['narrow', 'Narrow']
    ];

    /* Every screen the catalogue reaches from the menu, with the group that
       owns it. `landed` is which group has actually been built: nothing here
       has, so each row states what it waits for rather than promising it. */
    var MINE = [
        ['My profile', '/profile', 'Profile', 'this story'],
        ['My details', '/profile/details', 'Profile', 'this story'],
        ['Security', '/profile/security', 'Credentials', 'not landed'],
        ['My access', '/profile/access', 'Access', 'not landed'],
        ['Where I work', '/work', 'Membership', 'not landed']
    ];

    var THEIRS = [
        ['Find a colleague', '/people', 'Directory', 'not landed'],
        ['Reporting lines', '/reporting-lines', 'Membership', 'not landed'],
        ['Roles', '/roles', 'Access', 'not landed']
    ];

    var NAV = [
        ['People', '/people'],
        ['Access', '/roles'],
        ['Reporting lines', '/reporting-lines']
    ];

    var PERSON = {
        member: {
            username: 'jdoe',
            name: 'Jane Doe',
            tenant: 'Northwind Capital',
            party: 'Trading',
            photo: 'photos/jane_doe.jpeg',
            title: 'Risk Analyst'
        },
        admin: {
            username: 'rsmith',
            name: 'R. Smith',
            tenant: 'Northwind Capital',
            party: 'Operations',
            photo: 'photos/rsmith.jpeg',
            title: 'Tenant Administrator'
        }
    };

    var S = {
        variant: 'B',
        actor: 'member',
        state: 'open',
        at: '/profile'
    };

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var v = (p.get('variant') || '').toUpperCase();
        if (VARIANTS[v]) S.variant = v;
        if (p.get('actor') === 'admin' || p.get('actor') === 'member') S.actor = p.get('actor');
        var st = p.get('state');
        for (var i = 0; i < STATES.length; i++) if (STATES[i][0] === st) S.state = st;
        if (p.get('screen')) S.at = p.get('screen');
    }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function person() { return PERSON[S.actor]; }
    function isAdmin() { return S.actor === 'admin'; }
    function menuOpen() {
        return S.state !== 'closed';
    }

    /* Before the account is read the session holds a username and nothing
       else, which is the state the trigger is in today. */
    function hasName() { return S.state !== 'pending'; }

    function face(cls) {
        var p = person();
        if (!hasName()) return '<span class="' + cls + ' pending">' + esc(p.username.charAt(0).toUpperCase()) + '</span>';
        return '<span class="' + cls + '"><img src="' + p.photo + '" alt=""></span>';
    }

    function items(list) {
        return list.map(function (row) {
            var here = row[1] === S.at;
            var tag = row[3] === 'this story'
                ? (here ? '<span class="heretag">you are here</span>' : '')
                : '<span class="soon">' + esc(row[2]) + '</span>';
            return '<a href="#" data-act="go" data-screen="' + row[1] + '"' + (here ? ' class="here"' : '') + '>' +
                esc(row[0]) + tag + '</a>';
        }).join('');
    }

    function trigger() {
        var p = person();
        var who = hasName()
            ? '<span class="nm">' + esc(p.name) + '</span><span class="sub">' + esc(p.username) + ' \u00b7 ' + esc(p.tenant) + '</span>'
            : '<span class="nm">' + esc(p.username) + '</span><span class="sub">reading the account\u2026</span>';
        return '<button class="trigger' + (menuOpen() ? ' open' : '') + '" data-act="toggle">' +
            face('face') +
            '<span class="who">' + who + '</span>' +
            '<span class="caret">' + (menuOpen() ? '\u25b2' : '\u25bc') + '</span>' +
            '</button>';
    }

    function menu() {
        if (!menuOpen()) return '';
        var p = person();
        var head = '<div class="head">' + (
            hasName()
                ? face('face') +
                  '<div><div class="nm">' + esc(p.name) + '</div>' +
                  '<div class="sub">' + esc(p.title) + ' \u00b7 ' + esc(p.party) + '</div></div>'
                : '<span class="face pending">' + esc(p.username.charAt(0).toUpperCase()) + '</span>' +
                  '<div><div class="nm">' + esc(p.username) + '</div>' +
                  '<div class="sub">the account is being read</div></div>'
        ) + '</div>';

        var groups = '<div class="group">' + items(MINE) + '</div>';
        if (S.variant === 'B' && isAdmin()) {
            groups += '<div class="group"><div class="groupname">This tenant</div>' + items(THEIRS) + '</div>';
        }
        if (S.variant === 'A' && isAdmin()) {
            groups += '<div class="group"><div class="none">The administrator\u2019s screens have nowhere to go in this variant. That is the argument against it.</div></div>';
        }

        var out = '<div class="group"><button class="item out" data-act="signout">Sign out</button></div>';
        return '<div class="menu">' + head + groups + out + '</div>';
    }

    function appnav() {
        if (S.variant !== 'C' || !isAdmin()) return '';
        return '<nav class="appnav">' + NAV.map(function (n) {
            var here = S.at === n[1];
            return '<a href="#" data-act="go" data-screen="' + n[1] + '"' + (here ? ' class="here"' : '') + '>' + esc(n[0]) + '</a>';
        }).join('') + '</nav>';
    }

    var SCREENS = {
        '/profile': ['My profile', 'Your photo, your name, your job title and how colleagues reach you.'],
        '/profile/details': ['My details', 'Your address, telephone and web page.'],
        '/profile/security': ['Security', 'Your password, your sign-in state and where you are signed in.'],
        '/profile/access': ['My access', 'The roles you hold and what they let you do.'],
        '/work': ['Where I work', 'The parties you work in, and the one you act for.'],
        '/people': ['Find a colleague', 'Correct a colleague\u2019s profile and contact details.'],
        '/reporting-lines': ['Reporting lines', 'Who reports to whom, drawn as a tree.'],
        '/roles': ['Roles', 'The roles this tenant defines and the permissions each bundles.']
    };

    function body() {
        var screen = SCREENS[S.at] || SCREENS['/profile'];
        var p = person();
        return '<main><div class="card">' +
            '<div class="crumb">' + esc(p.tenant) + ' \u00b7 ' + esc(p.party) + '</div>' +
            '<h1>' + esc(screen[0]) + '</h1>' +
            '<p class="lead">' + esc(screen[1]) + '</p>' +
            '<div class="panels">' +
            '<div class="mini"><h2>Photo and identity</h2><table class="kv">' +
            '<tr><td>Full name</td><td>' + esc(hasName() ? p.name : '\u2014') + '</td></tr>' +
            '<tr><td>Job title</td><td>' + esc(hasName() ? p.title : '\u2014') + '</td></tr>' +
            '</table></div>' +
            '<div class="mini"><h2>Reached from</h2><table class="kv">' +
            '<tr><td>Trigger</td><td>' + (hasName() ? 'name and photo' : 'username only') + '</td></tr>' +
            '<tr><td>Menu</td><td>' + (menuOpen() ? 'open' : 'closed') + '</td></tr>' +
            '</table></div></div></div></main>';
    }

    function render() {
        var narrow = S.state === 'narrow' ? ' narrow' : '';
        var shell = '<div class="shell' + narrow + '">' +
            '<header class="appheader"><div class="appheader-inner">' +
            '<a class="brand" href="#"><span class="mark">O</span><span class="name">ORE Studio</span></a>' +
            appnav() +
            '<div class="accounts">' + trigger() + menu() + '</div>' +
            '</div></header>' + body() + '</div>';
        document.getElementById('app').innerHTML = shell;

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 variant ' + S.variant + ' \u00b7 ' +
            (isAdmin() ? 'tenant administrator' : 'member') + ' \u00b7 state ' + S.state;

        renderBar();
    }

    function renderBar() {
        var variants = Object.keys(VARIANTS).map(function (k) {
            return '<button data-act="variant" data-variant="' + k + '"' + (S.variant === k ? ' class="on"' : '') + '>' + k + '</button>';
        }).join('');
        var actors = '<span class="label">actor</span>' +
            '<button data-act="actor" data-actor="member"' + (!isAdmin() ? ' class="on"' : '') + '>member</button>' +
            '<button data-act="actor" data-actor="admin"' + (isAdmin() ? ' class="on"' : '') + '>admin</button>';
        var states = STATES.map(function (s) {
            return '<button data-act="state" data-state="' + s[0] + '"' + (S.state === s[0] ? ' class="on"' : '') + '>' + esc(s[1]) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">variant <b>' + S.variant + '</b> \u2014 ' + esc(VARIANTS[S.variant].name) + '</span>' +
            variants + '<span class="sep">|</span>' + actors + '<span class="sep">|</span>' + states;
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'variant') S.variant = el.getAttribute('data-variant');
        else if (act === 'actor') S.actor = el.getAttribute('data-actor');
        else if (act === 'state') S.state = el.getAttribute('data-state');
        else if (act === 'toggle') S.state = menuOpen() ? 'closed' : 'open';
        else if (act === 'go') { S.at = el.getAttribute('data-screen'); S.state = 'current'; }
        render();
    });

    document.addEventListener('keydown', function (ev) {
        var order = Object.keys(VARIANTS);
        var at = order.indexOf(S.variant);
        if (ev.key === 'ArrowRight') S.variant = order[(at + 1) % order.length];
        else if (ev.key === 'ArrowLeft') S.variant = order[(at + order.length - 1) % order.length];
        else return;
        render();
    });

    readParams();
    render();
})();
