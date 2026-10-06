/* New party journey prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * The accepted new party journey, on the same journey page as the tenant
 * journeys: a flat step rail on the left, one step on the right. Every step,
 * state and piece of mock data is the one the React prototype holds
 * (pages/prototype/newTenantJourney/NewPartyJourneyPrototype.tsx, journey.tsx,
 * parts.tsx, stub.ts). Nothing is invented; the provisioning run is simulated
 * exactly as useSimulatedRun simulates it, one step every 900 ms.
 *
 * States are chosen from the query string, as the navigation prototype does:
 *   ?state=entity|describe|review|provisioning|next
 *   ?query=Northwind                       the search, as typed
 *   ?entity=549300NWCAPITAL00001           the legal entity, already chosen
 *   ?shortname=Northwind%20Capital         the party's short name
 *   ?accounts=tenant_admin,j.smith         the accounts that work in it
 *   ?fail=1                                make the third provisioning step fail once
 *   ?run=pending|running|done|failed       a snapshot of the provisioning run
 * The bar mirrors the same states as buttons. */

(function () {
    'use strict';

    /* --------------------------------------------------------------- mock data */

    var GLEIF_RESULTS = [
        { lei: '549300NWCAPITAL00001', name: 'Northwind Capital Ltd', country: 'GB' },
        { lei: '549300NWMARKETS00002', name: 'Northwind Markets LLC', country: 'US', parent: 'Northwind Capital Ltd' },
        { lei: '549300NWASIAPAC00003', name: 'Northwind Asia Pacific Pte Ltd', country: 'SG', parent: 'Northwind Capital Ltd' }
    ];

    var TENANT_ACCOUNTS = ['tenant_admin', 'j.smith', 'a.tanaka', 'm.okafor'];

    var PARTY_STEPS = [
        'Create the party',
        'Activate it',
        'Publish its essential data',
        'Link its accounts',
        'Complete'
    ];

    var STEPS = [
        { id: 'entity', title: 'Find the legal entity', lead: 'Search the GLEIF register by name or LEI.' },
        { id: 'describe', title: 'Describe the party', lead: 'Name it and choose the accounts that work in it.' },
        { id: 'review', title: 'Review', lead: 'Nothing is created until you confirm.' },
        { id: 'provisioning', title: 'Provisioning', lead: 'This runs on the server. You can leave this page and come back.', final: true },
        { id: 'next', title: 'Next steps', lead: '', final: true }
    ];

    var RUN_STATES = ['pending', 'running', 'done', 'failed'];

    /* ------------------------------------------------------------------- state */

    var S = {
        at: 0,
        query: 'Northwind',
        entity: undefined,
        shortName: '',
        accounts: ['tenant_admin'],
        failOnce: false,
        started: false,
        cursor: 0,
        runFailed: false,
        failedOnce: false,
        live: false
    };

    var timer = null;

    function failAt() {
        return S.failOnce ? 2 : undefined;
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        if (p.get('query') !== null) S.query = p.get('query');
        var lei = p.get('entity');
        if (lei !== null && lei !== '') {
            var e = GLEIF_RESULTS.filter(function (x) { return x.lei === lei; })[0];
            if (e !== undefined) pick(e);
        }
        if (p.get('shortname') !== null) S.shortName = p.get('shortname');
        var accounts = p.get('accounts');
        if (accounts !== null) {
            S.accounts = accounts.split(',').filter(function (a) {
                return TENANT_ACCOUNTS.indexOf(a) >= 0;
            });
        }
        if (p.get('fail') === '1') S.failOnce = true;
        var state = p.get('state') || p.get('step');
        STEPS.forEach(function (s, i) { if (s.id === state) S.at = i; });
        var run = p.get('run');
        if (RUN_STATES.indexOf(run) >= 0) runSnapshot(run);
    }

    function pick(e) {
        S.entity = e;
        S.shortName = e.name.replace(/ (Ltd|LLC|Pte Ltd|plc|Inc)$/u, '');
    }

    function toggle(account) {
        var i = S.accounts.indexOf(account);
        if (i >= 0) S.accounts.splice(i, 1);
        else S.accounts.push(account);
    }

    function runSnapshot(which) {
        S.started = true;
        S.runFailed = false;
        S.failedOnce = false;
        S.live = false;
        var total = PARTY_STEPS.length;
        if (which === 'pending') { S.started = false; S.cursor = 0; }
        else if (which === 'running') { S.cursor = 1; }
        else if (which === 'done') { S.cursor = total; }
        else if (which === 'failed') { S.cursor = 2; S.runFailed = true; S.failedOnce = true; }
    }

    function matches() {
        if (S.query.trim() === '') return [];
        return GLEIF_RESULTS.filter(function (e) {
            return e.name.toLowerCase().indexOf(S.query.toLowerCase()) >= 0 ||
                e.lei.indexOf(S.query.toUpperCase()) === 0;
        });
    }

    /* ---------------------------------------------------------------- utilities */

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    /* ------------------------------------------------------- find legal entity */

    function entityStep() {
        var rows = matches().map(function (e) {
            return '<li><button type="button" role="option" aria-selected="' +
                (S.entity !== undefined && S.entity.lei === e.lei) + '"' +
                ' class="entityresult' + (S.entity !== undefined && S.entity.lei === e.lei ? ' on' : '') + '"' +
                ' data-act="entity" data-lei="' + esc(e.lei) + '">' +
                '<span><span class="nm">' + esc(e.name) + '</span>' +
                (e.parent !== undefined ? '<span class="parent">Subsidiary of ' + esc(e.parent) + '</span>' : '') +
                '</span>' +
                '<span class="id">' + esc(e.country) + ' \u00b7 ' + esc(e.lei) + '</span></button></li>';
        }).join('');
        if (rows === '') rows = '<li class="none">No legal entity matches.</li>';
        return '<div>' +
            '<div class="field"><span class="lbl">Name or LEI</span>' +
            '<input data-focus="query" data-q="1" placeholder="Northwind, or 549300\u2026" value="' +
            esc(S.query) + '"></div>' +
            '<ul class="entitylist" role="listbox" aria-label="Matching legal entities">' + rows + '</ul>' +
            '<button type="button" class="btn ghost small" style="margin-left:-11px;margin-top:12px">' +
            'It has no LEI: add it by name</button></div>';
    }

    /* ----------------------------------------------------------- describe step */

    function describeStep() {
        var accounts = TENANT_ACCOUNTS.map(function (a) {
            return '<label class="checkline"><input type="checkbox" data-account="' + esc(a) + '"' +
                (S.accounts.indexOf(a) >= 0 ? ' checked' : '') + '>' +
                '<span class="mono">' + esc(a) + '</span></label>';
        }).join('');
        return '<div>' +
            '<div class="field"><span class="lbl">Short name</span>' +
            '<input data-focus="shortName" data-shortname="1" value="' + esc(S.shortName) + '">' +
            '<div class="hint">Shown in menus and the party switcher.</div></div>' +
            '<fieldset><legend style="color:var(--ink-dim);font-size:13px;font-weight:500">' +
            'Accounts that work in it</legend>' +
            '<p class="hint" style="margin:-4px 0 8px">They can sign in to this party and act for it.</p>' +
            '<div class="accountgrid">' + accounts + '</div></fieldset></div>';
    }

    /* ------------------------------------------------------------ review step */

    function reviewStep() {
        var rows = [
            ['Legal entity', S.entity !== undefined ? S.entity.name : '-'],
            ['LEI', S.entity !== undefined ? S.entity.lei : '-'],
            ['Short name', S.shortName],
            ['Accounts', S.accounts.join(', ')],
            ['Data', "The tenant's standard party data"]
        ];
        var dl = rows.map(function (r) {
            return '<dt>' + esc(r[0]) + '</dt><dd>' + esc(r[1]) + '</dd>';
        }).join('');
        return '<dl class="reviewgrid">' + dl + '</dl>' +
            '<label class="failtoggle"><input type="checkbox" data-fail="1"' + (S.failOnce ? ' checked' : '') +
            '> Prototype: make step 3 fail once</label>';
    }

    /* ------------------------------------------------------ provisioning step */

    function progressList() {
        var items = PARTY_STEPS.map(function (label, i) {
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
                '<span class="note">Completed steps are kept. Retrying is safe.</span></div>';
        }
        return '<ol class="runlist">' + items + '</ol>' + foot;
    }

    /* --------------------------------------------------------------- next step */

    function nextStep() {
        var cards = [
            ['Switch to it', 'Work in ' + S.shortName + ' now.'],
            ['Set up its books', 'Business units, portfolios and books.'],
            ['Add another party', 'Start this journey again.']
        ].map(function (c) {
            return '<button type="button" class="nextcard"><span class="nm">' + esc(c[0]) + '</span>' +
                '<p>' + esc(c[1]) + '</p></button>';
        }).join('');
        return '<div class="nextcards">' + cards + '</div>';
    }

    /* ------------------------------------------------------------ the header */

    function partyHeader() {
        if (S.entity === undefined) return '';
        return '<div class="stepheader"><div>' +
            '<div class="nm">' + esc(S.shortName || S.entity.name) + '</div>' +
            '<div class="sub mono">' + esc(S.entity.lei) + '</div></div></div>';
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

    function stepLead(step) {
        if (step.id === 'next') return S.shortName + ' is active.';
        return step.lead;
    }

    function stepBody() {
        var step = STEPS[S.at];
        if (step.id === 'entity') return entityStep();
        if (step.id === 'describe') return describeStep();
        if (step.id === 'review') return reviewStep();
        if (step.id === 'provisioning') return progressList();
        return nextStep();
    }

    function nextOf() {
        var id = STEPS[S.at].id;
        if (id === 'entity') return { label: 'Continue', enabled: S.entity !== undefined };
        if (id === 'describe') {
            return { label: 'Continue', enabled: S.shortName.trim() !== '' && S.accounts.length > 0 };
        }
        if (id === 'review') return { label: 'Add party', enabled: true };
        return null;
    }

    function render() {
        var at = S.at;
        var step = STEPS[at];
        var backAllowed = at > 0 && STEPS[at - 1].final !== true;
        var next = nextOf();
        var foot = next === null ? '' :
            '<div class="stepfoot">' +
            '<button type="button" class="btn ghost" data-act="back"' + (backAllowed ? '' : ' disabled') + '>Back</button>' +
            '<button type="button" class="btn primary ml-auto" data-act="next"' + (next.enabled ? '' : ' disabled') + '>' +
            esc(next.label) + '</button></div>';

        document.getElementById('app').innerHTML =
            '<div class="page"><h1>New party</h1>' +
            '<div class="journey">' + rail(at) +
            '<section class="card">' + partyHeader() +
            '<h2>' + esc(step.title) + '</h2><p class="lead">' + esc(stepLead(step)) + '</p>' +
            stepBody() + foot + '</section></div></div>';

        renderNote();
        renderBar();
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 journey New party \u00b7 entity ' +
            (S.entity !== undefined ? S.entity.name : 'none') + ' \u00b7 state ' + STEPS[S.at].id;
    }

    function renderBar() {
        var steps = STEPS.map(function (s, i) {
            return '<button data-act="state" data-state="' + s.id + '"' + (S.at === i ? ' class="on"' : '') + '>' +
                esc(s.id) + '</button>';
        }).join('');
        var runs = '<span class="label">run</span>' + RUN_STATES.map(function (r) {
            return '<button data-act="run" data-run="' + r + '">' + r + '</button>';
        }).join('');
        var extras = '<span class="sep">|</span>' +
            '<button data-act="fail"' + (S.failOnce ? ' class="on"' : '') + '>fail step 3</button>';
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">state</span>' + steps + '<span class="sep">|</span>' + runs + extras;
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
        var total = PARTY_STEPS.length;
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
                if (S.at === 3) S.at = 4;
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

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        ev.preventDefault();
        var act = el.getAttribute('data-act');
        if (act === 'state') {
            STEPS.forEach(function (s, i) { if (s.id === el.getAttribute('data-state')) S.at = i; });
        } else if (act === 'entity') {
            var e = GLEIF_RESULTS.filter(function (x) { return x.lei === el.getAttribute('data-lei'); })[0];
            if (e !== undefined) pick(e);
        } else if (act === 'run') {
            runSnapshot(el.getAttribute('data-run'));
        } else if (act === 'fail') {
            S.failOnce = !S.failOnce;
        } else if (act === 'back') {
            if (!el.disabled && S.at > 0 && STEPS[S.at - 1].final !== true) S.at -= 1;
        } else if (act === 'next') {
            if (!el.disabled) goNext();
        } else if (act === 'retry') {
            S.runFailed = false;
            S.live = true;
            scheduleTick();
        } else {
            return;
        }
        rerender();
    });

    function goNext() {
        if (S.at === 2) {
            startRun();
            S.at = 3;
            return;
        }
        S.at += 1;
    }

    function inputChanged(el) {
        if (el.getAttribute('data-fail') !== null) { S.failOnce = el.checked; return true; }
        if (el.getAttribute('data-q') !== null) { S.query = el.value; return true; }
        if (el.getAttribute('data-shortname') !== null) { S.shortName = el.value; return true; }
        var account = el.getAttribute('data-account');
        if (account !== null) {
            var checked = el.checked;
            var has = S.accounts.indexOf(account) >= 0;
            if (checked && !has) S.accounts.push(account);
            else if (!checked && has) S.accounts.splice(S.accounts.indexOf(account), 1);
            return true;
        }
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
