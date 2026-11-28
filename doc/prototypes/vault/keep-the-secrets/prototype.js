/* Keep the installation's secrets prototype. Self-contained: plain JavaScript,
 * mock data, no framework, no build step, and nothing that outlives the page.
 *
 * Shaped after HashiCorp Vault: the secrets engine's list, the secret view with
 * a masked value and the version history beside it, and the audit devices' log
 * for the uses. The four screens come from
 * doc/knowledge/journeys/vault/journey_keep_the_installations_secrets.org.
 *
 * Every screen and every state has its own address. The rows are fixtures: the
 * gateway has two doors for the vault and neither lists, so the state readout
 * names the operation each screen would call and the design note beside this
 * folder says which of them exist. */

(function () {
    'use strict';

    var SCREENS = ['secrets', 'secret', 'consumers', 'uses', 'states'];
    var SCREEN_NAMES = {
        secrets: 'Secrets', secret: 'Secret', consumers: 'Consumers',
        uses: 'Uses', states: 'States'
    };
    var STATES = {
        secrets: ['empty', 'full', 'refused'],
        secret: ['masked', 'revealed', 'refused'],
        consumers: ['empty', 'full', 'refused'],
        uses: ['empty', 'full', 'refused'],
        states: ['full']
    };

    var REFRESHED_AT = '2026-11-28 14:20 UTC';
    var KEY_VERSION = 2;

    var SECRETS = [
        { name: 'db.marketdata_service.password', owner: 'marketdata_service', version: 3, read: '2 hours ago' },
        { name: 'db.inbox_service.password', owner: 'inbox_service', version: 1, read: 'yesterday' },
        { name: 'ores.marketdata.vendor.api_key', owner: 'marketdata_service', version: 2, read: 'never' },
        { name: 'db.vault_service.password', owner: 'vault_service', version: 4, read: '4 days ago' }
    ];

    var VERSIONS = [
        { version: 3, when: '2 hours ago', by: 'vault provision', reason: 'system.update' },
        { version: 2, when: 'yesterday', by: 'marco', reason: 'rotate the tick writer key' },
        { version: 1, when: '4 days ago', by: 'vault provision', reason: 'system.initial_load' }
    ];

    var CONSUMERS = [
        { name: 'marketdata_service', subject: 'marketdata_service@system.ores', key: '8f2c…a1', enabled: true },
        { name: 'inbox_service', subject: 'inbox_service@system.ores', key: '3d90…7e', enabled: true },
        { name: 'reporting_service', subject: 'reporting_service@system.ores', key: 'c41b…02', enabled: false }
    ];

    var USES = [
        { when: '2 hours ago', secret: 'db.marketdata_service.password', consumer: 'marketdata_service', actor: 'marketdata_service@system.ores', key: 2 },
        { when: 'yesterday', secret: 'db.inbox_service.password', consumer: 'inbox_service', actor: 'inbox_service@system.ores', key: 2 },
        { when: 'yesterday', secret: 'ores.marketdata.vendor.api_key', consumer: 'marketdata_service', actor: 'marco@acme.example', key: 1 }
    ];

    var FAKE_VALUE = 'f3a9-tyJ_03fl7eLhAn8_fv3fHvRFzhm';

    var OPERATIONS = {
        secrets: 'GET /api/v1/vault/secrets — missing; the web task adds it',
        secret: 'GET /api/v1/vault/secrets/{name} — missing; GET …/{name}/value exists for the reveal',
        consumers: 'GET /api/v1/vault/consumers — missing; the web task adds it',
        uses: 'none: no operation lists the uses, and no rule draws the screen',
        states: "none: this page is the prototype's own index"
    };

    var S = { screen: 'secrets', state: 'full', showState: true };

    // ------------------------------------------------------------- helpers

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function tag(tone, text) {
        return '<span class="tag ' + tone + '">' + esc(text) + '</span>';
    }

    function crumb(text) {
        return '<div class="strip"><span class="mono">' + esc(text) + '</span></div>';
    }

    function head(title, desc, actions) {
        return '<div class="phead"><div><h1>' + esc(title) + '</h1>' +
            '<div class="desc">' + esc(desc) + '</div></div>' +
            '<div class="actions">' + (actions || '') + '</div></div>';
    }

    function notice(tone, text) {
        return '<div class="notice ' + tone + '">' + esc(text) + '</div>';
    }

    function btn(label, kind, act) {
        return '<button class="btn ' + (kind || 'secondary') +
            '" data-act="' + act + '">' + esc(label) + '</button>';
    }

    function href(screen, state) {
        return '?screen=' + screen + '&state=' + state;
    }

    function go(screen, state) {
        window.location.search = href(screen, state).slice(1);
    }

    // ------------------------------------------------------------- rows

    function secretRow(s) {
        var unread = s.read === 'never';
        return '<div class="srow">' +
            '<span class="client w7"><a href="' + href('secret', 'masked') + '">' +
            esc(s.name) + '</a><span class="sub">' + esc(s.owner) + '</span></span>' +
            '<span class="grow">' + tag('neutral', 'version ' + s.version) + ' ' +
            tag('muted', 'key ' + KEY_VERSION) + '</span>' +
            '<span class="bytes">' + esc(s.read) + '</span>' +
            '<span class="act">' + tag(unread ? 'muted' : 'accent',
                unread ? 'unread' : 'read') + '</span></div>';
    }

    function consumerRow(c) {
        return '<div class="srow">' +
            '<span class="client w7">' + esc(c.name) + '<span class="sub">' +
            esc(c.subject) + '</span></span>' +
            '<span class="grow mono">' + esc(c.key) + '</span>' +
            '<span class="act">' + tag(c.enabled ? 'accent' : 'muted',
                c.enabled ? 'enabled' : 'disabled') + '</span></div>';
    }

    function useRow(u) {
        return '<div class="srow">' +
            '<span class="client"><span class="sub">' + esc(u.when) + '</span></span>' +
            '<span class="grow"><span class="mono">' + esc(u.secret) + '</span>' +
            '<span class="sub">' + esc(u.consumer) + ' · ' + esc(u.actor) + '</span></span>' +
            '<span class="act">' + tag('neutral', 'key ' + u.key) + '</span></div>';
    }

    function rows(items, render) {
        return '<div class="card p4"><div class="srows">' + items.map(render).join('') +
            '</div><div class="footrow"><span class="tiny">Showing 1-' + items.length +
            ' of ' + items.length + '</span><span class="end"><span class="tiny">Read ' +
            esc(REFRESHED_AT) + '</span></span></div></div>';
    }

    function empty(what, add) {
        return '<div class="card"><div class="chead"><h2>Nothing here yet</h2>' +
            '<p>' + esc(what) + '</p></div>' + (add || '') + '</div>';
    }

    // ------------------------------------------------------------- screens

    function screenSecrets(state) {
        var canWrite = state !== 'refused';
        var actions = btn('Refresh', 'secondary', 'refresh') +
            (canWrite ? btn('Add', 'primary', 'add') : '');
        var body = state === 'empty'
            ? empty('The installation holds no secrets yet.',
                canWrite ? btn('Add', 'primary', 'add') : '')
            : rows(SECRETS, secretRow);
        var access = canWrite ? '' : notice('warn',
            'You hold vault::secrets:read and not the write, so Add is not drawn.');
        return crumb('Home · Secrets') +
            head('Secrets', 'The values this installation keeps for its own use.', actions) +
            access + body;
    }

    function screenSecret(state) {
        var canWrite = state !== 'refused';
        var revealed = state === 'revealed';
        var s = SECRETS[0];
        var actions = (canWrite ? btn('Change', 'secondary', 'change') : '') +
            (canWrite ? btn('Delete', 'danger', 'delete') : '') +
            (canWrite ? (revealed ? btn('Hide', 'secondary', 'hide')
                : btn('Reveal', 'primary', 'reveal')) : '');
        var value = revealed
            ? '<div class="maskline"><span class="mask revealed">' + esc(FAKE_VALUE) +
              '</span>' + tag('warn', 'revealed') + '</div>' +
              notice('info', "This reveal was recorded: the vault recorded the gateway's " +
                  'resolve, and the gateway recorded you.')
            : '<div class="maskline"><span class="mask">••••••••••••</span>' +
              (canWrite ? btn('Reveal', 'primary', 'reveal') : '') + '</div>' +
              notice('info', 'The value stays masked until it is asked for, and the ask is recorded.');
        var lostRead = canWrite ? '' : notice('error',
            'You do not hold vault::secrets:read, so the reveal is not drawn.');
        var versions = VERSIONS.map(function (v) {
            return '<div class="vrow"><span class="ver">v' + v.version + '</span>' +
                '<span class="grow">' + esc(v.by) + ' · ' + esc(v.reason) + '</span>' +
                '<span class="when">' + esc(v.when) + '</span></div>';
        }).join('');
        return crumb('Home · Secrets · ' + s.name) +
            head(s.name, 'version ' + s.version + ' · owned by ' + s.owner +
                ' · key ' + KEY_VERSION, actions) +
            '<div class="stack"><div class="card"><div class="chead"><h2>Value</h2>' +
            '<p>Masked until it is asked for; the ask needs the read and is recorded twice.</p></div>' +
            lostRead + value + '</div>' +
            '<div class="card"><div class="chead"><h2>Versions</h2>' +
            '<p>Newest first, each with who wrote it and why.</p></div>' + versions + '</div></div>';
    }

    function screenConsumers(state) {
        var canWrite = state !== 'refused';
        var actions = btn('Refresh', 'secondary', 'refresh') +
            (canWrite ? btn('Add', 'primary', 'add') : '');
        var body = state === 'empty'
            ? empty('No service may receive a secret yet.',
                canWrite ? btn('Add', 'primary', 'add') : '')
            : rows(CONSUMERS, consumerRow);
        var access = canWrite ? '' : notice('warn',
            'You hold the read and not the write, so Add is not drawn.');
        return crumb('Home · Secrets · Consumers') +
            head('Consumers', 'The services that may receive a secret, and the key each one answers with.',
                actions) + access + body;
    }

    function screenUses(state) {
        var frame = crumb('Home · Secrets · Uses') +
            head('Uses', 'Every time a secret was resolved, and who asked for it.',
                btn('Refresh', 'secondary', 'refresh'));
        if (state === 'empty')
            return frame + empty('No secret has been resolved yet.');
        var refused = state === 'refused'
            ? notice('error', 'You do not hold the read this list needs, so it is not drawn.') : '';
        return frame + refused + '<div class="stack">' + rows(USES, useRow) +
            notice('warn', 'No rule in the record screen standard draws this list yet, and no ' +
                'operation lists it: the page is the proposal the capture asks for.') + '</div>';
    }

    function screenStates() {
        var list = Object.keys(STATES).map(function (screen) {
            return STATES[screen].map(function (state) {
                return '<div class="vrow"><span class="ver mono">' + esc(screen) + '</span>' +
                    '<span class="grow"><a href="' + href(screen, state) + '">?screen=' +
                    esc(screen) + '&amp;state=' + esc(state) + '</a></span>' +
                    '<span class="when">' + esc(state) + '</span></div>';
            }).join('');
        }).join('');
        return crumb('Home · States') +
            head('Every state, at its own address',
                'Each screen of the journey in its empty, full, revealed and refused states.') +
            '<div class="card p4">' + list + '</div>';
    }

    var RENDER = {
        secrets: screenSecrets, secret: screenSecret, consumers: screenConsumers,
        uses: screenUses, states: screenStates
    };

    // ------------------------------------------------------------- chrome

    function readQuery() {
        var q = new URLSearchParams(window.location.search);
        var screen = SCREENS.indexOf(q.get('screen')) >= 0 ? q.get('screen') : 'secrets';
        var allowed = STATES[screen];
        S.screen = screen;
        S.state = allowed.indexOf(q.get('state')) >= 0 ? q.get('state') : allowed[0];
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'Prototype · mock data · ' + SCREEN_NAMES[S.screen] + ' · ' + S.state;
    }

    function renderBar() {
        var screenButtons = SCREENS.map(function (screen) {
            return '<button data-screen="' + screen + '" class="' +
                (screen === S.screen ? 'on' : '') + '">' + esc(SCREEN_NAMES[screen]) +
                '</button>';
        }).join('');
        var stateButtons = STATES[S.screen].map(function (state) {
            return '<button data-state="' + state + '" class="' +
                (state === S.state ? 'on' : '') + '">' + esc(state) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">Screen</span>' + screenButtons +
            '<span class="sep">|</span><span class="label">State</span>' + stateButtons +
            '<span class="sep">|</span><button data-toggle="state">' +
            (S.showState ? 'Hide state' : 'Show state') + '</button>';
    }

    function renderState() {
        var panel = document.getElementById('proto-state');
        if (!S.showState) { panel.hidden = true; return; }
        panel.hidden = false;
        panel.innerHTML = '<div class="facts">' +
            '<div>Screen <span class="mono">' + esc(S.screen) + '</span></div>' +
            '<div>State <span class="mono">' + esc(S.state) + '</span></div>' +
            '<div class="grow">Operation <span class="mono">' + esc(OPERATIONS[S.screen]) +
            '</span></div></div><div class="none">Every row is a fixture. The mask hides a value ' +
            'that is not a password and is not the shape of one this deployment issues.</div>';
    }

    function render() {
        readQuery();
        renderNote();
        renderBar();
        renderState();
        document.getElementById('app').innerHTML =
            '<div class="page">' + RENDER[S.screen](S.state) + '</div>';
    }

    // ------------------------------------------------------------- events

    document.addEventListener('click', function (e) {
        var b = e.target.closest('button');
        if (!b) return;
        if (b.hasAttribute('data-screen'))
            return go(b.getAttribute('data-screen'), STATES[b.getAttribute('data-screen')][0]);
        if (b.hasAttribute('data-state'))
            return go(S.screen, b.getAttribute('data-state'));
        if (b.getAttribute('data-toggle') === 'state') {
            S.showState = !S.showState;
            renderBar();
            renderState();
            return;
        }
        if (b.getAttribute('data-act') === 'reveal')
            return go('secret', 'revealed');
    });

    render();
}());
