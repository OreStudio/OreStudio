/* Four-eyes controls for a journey prototype. Self-contained: plain
 * JavaScript, mock data, no framework, nothing that outlives the page. It
 * carries the three screens every gated write draws, so a prototype supplies
 * only its changes and who decides each:
 *
 *   waiting   the raised request, and who still has to answer it
 *   decide    the checker's screen: the delta, the checks, a comment, and a
 *             refusal of the person who asked
 *   declined  a declined request: the record keeps its version
 *
 * The rules are the book controls note's: a change that needs a second person
 * names the function that decides it, the person who asks cannot decide, a
 * request applies only when every required function has approved, and a decline
 * from any of them ends it. A change that names no decider is made directly.
 *
 * A prototype copies this file beside its own script, as the contract says a
 * prototype is self-contained, and calls FourEyes.create(config). */

(function (root) {
    'use strict';

    var ACTORS = [
        { id: 'h.desk', name: 'R. Alvarez', part: 'Head of Desk', acme: true },
        { id: 'c.control', name: 'P. Nwosu', part: 'Controller', acme: false },
        { id: 'f.ledger', name: 'S. Haddad', part: 'Finance', acme: false },
        { id: 'm.risk', name: 'T. Brandt', part: 'Market Risk', acme: true },
        { id: 'o.ops', name: 'K. Mori', part: 'Operations', acme: true }
    ];

    var CSS = [
        '.fe-badge{display:inline-block;border:1px solid #4a3a17;background:#241d11;color:#efd39a;border-radius:999px;padding:1px 8px;font-size:11px;margin-left:6px;font-weight:600;white-space:nowrap}',
        '.fe-badge.ok{border-color:#23543a;background:#12281d;color:#a8e6c4}',
        '.fe-badge.bad{border-color:#5c2a24;background:#2b1614;color:#f0b7ae}',
        '.fe-badge.info{border-color:#24344d;background:#12161f;color:#9ec5ff}',
        '.fe-notice{border-radius:8px;padding:12px 14px;font-size:13px;margin:0 0 14px;border:1px solid #24344d;background:#12161f;color:#e3e3e6}',
        '.fe-notice.warn{background:#2a2113;border-color:#4a3a17;color:#efd39a}',
        '.fe-notice.error{background:#2b1614;border-color:#5c2a24;color:#f0b7ae}',
        '.fe-notice.success{background:#12281d;border-color:#23543a;color:#a8e6c4}',
        '.fe-notice .fe-title{display:block;font-weight:600;margin-bottom:4px}',
        '.fe-panel{background:#16161a;border:1px solid #30363d;border-radius:8px;padding:14px;margin:0 0 14px}',
        '.fe-panel h3{margin:0 0 8px;font-size:13px;letter-spacing:.04em;text-transform:uppercase;color:#a1a1aa}',
        '.fe-changes{list-style:none;margin:0;padding:0;display:flex;flex-direction:column;gap:8px}',
        '.fe-changes li{border:1px solid #21262d;border-radius:8px;padding:10px 12px;background:#121214}',
        '.fe-what{font-size:13px;font-weight:600}',
        '.fe-who{font-size:12px;color:#8b8b95}',
        '.fe-fromto{display:grid;grid-template-columns:1fr auto 1fr;gap:8px;font-size:13px;margin-top:6px}',
        '.fe-was{color:#f85149}.fe-now{color:#3fb950}',
        '.fe-table{width:100%;border-collapse:collapse;font-size:13px;margin:10px 0}',
        '.fe-table th,.fe-table td{text-align:left;padding:7px 8px;border-bottom:1px solid #21262d}',
        '.fe-table th{font-size:11px;letter-spacing:.06em;text-transform:uppercase;color:#71717a}',
        '.fe-hint{font-size:12px;color:#8b8b95;margin:8px 0}',
        '.fe-field{display:grid;gap:4px;margin:0 0 12px}',
        '.fe-field span{font-size:12px;color:#a1a1aa}',
        '.fe-field select,.fe-field input{background:#0d1117;border:1px solid #30363d;border-radius:6px;padding:8px 10px;color:#e3e3e6;font:inherit}',
        '.fe-actions{display:flex;gap:10px;align-items:center;margin-top:10px}',
        '.fe-btn{background:#26262b;border:1px solid #30363d;border-radius:6px;padding:8px 14px;color:#e3e3e6;cursor:pointer;font:inherit}',
        '.fe-btn.primary{background:#58a6ff;border-color:#58a6ff;color:#0d1117;font-weight:600}',
        '.fe-btn[disabled]{opacity:.45;cursor:not-allowed}',
        '.fe-grow{margin-left:auto}',
        '.fe-center{text-align:center}',
        '.fe-mark{font-size:30px;margin:6px 0}',
        '.fe-cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:10px;margin-top:14px}',
        '.fe-card{background:#16161a;border:1px solid #30363d;border-radius:8px;padding:12px;text-align:left;color:#e3e3e6;cursor:pointer;font:inherit}',
        '.fe-card:hover{border-color:#58a6ff}',
        '.fe-card b{display:block;margin-bottom:4px}',
        '.fe-mono{font-family:ui-monospace,SFMono-Regular,Menlo,monospace}'
    ].join('');

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function actorOf(id) {
        return ACTORS.filter(function (a) { return a.id === id; })[0];
    }

    function actorForPart(part) {
        return ACTORS.filter(function (a) { return a.part === part; })[0];
    }

    function create(cfg) {
        var style = document.createElement('style');
        style.textContent = CSS;
        document.head.appendChild(style);

        var requestId = cfg.requestId || 'REQ-0413';
        var fe = {
            actors: ACTORS,
            states: ['waiting', 'decide', 'declined'],
            S: { answers: {}, comment: '', declinedComment: '', actor: 'o.ops', maker: cfg.maker || 'h.desk', check: '', applied: false }
        };
        var S = fe.S;

        function badge(text, kind) {
            return '<span class="fe-badge' + (kind ? ' ' + kind : '') + '">' + esc(text) + '</span>';
        }

        fe.gated = function (changes) {
            return changes.filter(function (c) { return c.decider; });
        };

        fe.parts = function (changes) {
            var seen = [];
            fe.gated(changes).forEach(function (c) {
                if (seen.indexOf(c.decider) < 0) seen.push(c.decider);
            });
            return seen;
        };

        /* The badge a review line carries. */
        fe.badge = function (change) {
            return change.decider ? badge('needs ' + change.decider, '') : badge('no approval', 'ok');
        };

        /* The notice that tops the review, and the label of its primary button. */
        fe.reviewNotice = function (changes) {
            var gated = fe.gated(changes);
            if (gated.length === 0) {
                return '<div class="fe-notice success"><span class="fe-title">The write is allowed</span>' +
                    'No change here needs a second person. Every change is recorded as a new version, with the actor and the change reason.</div>';
            }
            return '<div class="fe-notice warn"><span class="fe-title">This raises an approval request</span>' +
                gated.length + ' of ' + changes.length + ' changes need a second person: ' + esc(fe.parts(changes).join(', ')) +
                '. Nothing changes until each of them answers, and the person who asks cannot decide.</div>';
        };

        fe.primaryLabel = function (changes, writeLabel) {
            return fe.gated(changes).length > 0 ? 'Raise request' : writeLabel;
        };

        fe.raise = function (changes) {
            S.answers = {};
            S.comment = '';
            S.applied = false;
            var parts = fe.parts(changes);
            var first = parts.length ? actorForPart(parts[0]) : null;
            if (first) S.actor = first.id;
        };

        fe.reset = function () {
            S.answers = {};
            S.comment = '';
            S.applied = false;
        };

        function allApproved(parts) {
            return parts.length > 0 && parts.every(function (p) { return S.answers[p] === 'approved'; });
        }

        fe.waiting = function () {
            var changes = cfg.getChanges();
            var parts = fe.parts(changes);
            var maker = actorOf(S.maker);
            var rows = parts.map(function (part) {
                var who = actorForPart(part);
                var holds = who && !who.acme ? ' ' + badge('no account in Acme yet', 'bad') : '';
                var state = S.answers[part] === 'approved' ? badge('approved', 'ok') : badge('waiting', '');
                return '<tr><td>' + esc(part) + '</td><td>' + esc(who ? who.name : '—') + holds + '</td><td>' + state + '</td></tr>';
            }).join('');
            return '<div class="fe-center"><div class="fe-mark">⏳</div><h2>The request is raised</h2>' +
                '<p><span class="fe-mono">' + esc(requestId) + '</span> was raised by ' + esc(maker.name) + ' (' + esc(maker.part) + ') for ' +
                changes.length + ' change' + (changes.length === 1 ? '' : 's') + '. ' + esc(cfg.subject()) +
                ' stays as it is until every decider answers. The request is written down, so you can leave this screen and find it under <b>My requests</b>.</p></div>' +
                '<table class="fe-table"><thead><tr><th>Decider</th><th>Person</th><th>Answer</th></tr></thead><tbody>' + rows + '</tbody></table>' +
                '<p class="fe-hint">The person who asks cannot decide. A request applies when every decider has approved, and one decline ends it.</p>' +
                '<div class="fe-cards">' +
                '<button type="button" class="fe-card" data-fe="opendecide"><b>Open as the checker</b>See what the decider sees.</button>' +
                '<button type="button" class="fe-card" data-fe="withdraw"><b>Withdraw the request</b>Go back to the review.</button></div>';
        };

        fe.decide = function () {
            var changes = cfg.getChanges();
            var parts = fe.parts(changes);
            var actor = actorOf(S.actor) || ACTORS[0];
            var maker = actorOf(S.maker);
            var isMaker = actor.id === maker.id;
            var mine = changes.filter(function (c) { return c.decider === actor.part; });
            var options = ACTORS.map(function (a) {
                return '<option value="' + esc(a.id) + '"' + (a.id === actor.id ? ' selected' : '') + '>' +
                    esc(a.name) + ' · ' + esc(a.part) + (a.acme ? '' : ' (not in Acme yet)') + '</option>';
            }).join('');
            var delta = mine.length === 0 ?
                '<div class="fe-notice">Nothing in this request is for ' + esc(actor.part) + '. ' +
                (parts.indexOf(actor.part) < 0 ? 'This part decides none of its changes.' : '') + '</div>' :
                '<ul class="fe-changes">' + mine.map(function (c) {
                    var fromto = c.from === null || c.from === undefined ? '' :
                        '<div class="fe-fromto"><span class="fe-was">' + esc(c.from) + '</span><span>→</span><span class="fe-now">' + esc(c.to) + '</span></div>';
                    return '<li><div class="fe-what">' + esc(c.what) + '</div><div class="fe-who">' + esc(c.who) + '</div>' + fromto + '</li>';
                }).join('') + '</ul>';
            var extra = (cfg.checks ? cfg.checks(actor, changes, S.check) : []);
            var failing = extra.some(function (c) { return !c.ok; });
            var checks = '<ul class="fe-changes">' +
                '<li><div class="fe-what">' + badge(isMaker ? 'fail' : 'pass', isMaker ? 'bad' : 'ok') + ' The maker is not the decider</div><div class="fe-who">' +
                esc(maker.name) + ' asked; ' + esc(actor.name) + ' is answering.</div></li>' +
                extra.map(function (c) {
                    return '<li><div class="fe-what">' + badge(c.ok ? 'pass' : 'fail', c.ok ? 'ok' : 'bad') + ' ' + esc(c.label) + '</div><div class="fe-who">' + esc(c.detail) + '</div></li>';
                }).join('') + '</ul>';
            var already = S.answers[actor.part] || null;
            var declineBlocked = isMaker || mine.length === 0 || S.comment.trim() === '' || already !== null;
            var approveBlocked = declineBlocked || failing;
            var why = isMaker ? '<div class="fe-notice error"><span class="fe-title">You asked for this</span>A different person must decide it.</div>' :
                already ? '<div class="fe-notice">' + esc(actor.part) + ' has already ' + esc(already) + ' this request.</div>' :
                failing ? '<div class="fe-notice error"><span class="fe-title">A check failed</span>This request cannot be approved while a check fails. Decline it, or have the maker fix the cause.</div>' : '';
            return '<div class="fe-panel"><label class="fe-field"><span>Acting as</span><select data-fe-actor="1">' + options + '</select></label></div>' +
                '<div class="fe-panel"><h3>Request ' + esc(requestId) + ' · raised by ' + esc(maker.name) + '</h3>' + delta + '</div>' +
                '<div class="fe-panel"><h3>Checks</h3>' + checks + '</div>' + why +
                '<div class="fe-panel"><label class="fe-field"><span>Comment · required</span>' +
                '<input data-fe-comment="1" placeholder="Why you approve or decline" value="' + esc(S.comment) + '"></label>' +
                '<div class="fe-actions"><button type="button" class="fe-btn" data-fe="decline"' + (declineBlocked ? ' disabled' : '') + '>Decline</button>' +
                '<button type="button" class="fe-btn primary fe-grow" data-fe="approve"' + (approveBlocked ? ' disabled' : '') + '>Approve</button></div></div>';
        };

        fe.declined = function () {
            var actor = actorOf(S.actor) || ACTORS[0];
            var note = S.declinedComment ? ': “' + esc(S.declinedComment) + '”' : '';
            return '<div class="fe-center"><div class="fe-mark">✕</div><h2>The request was declined</h2>' +
                '<p>' + esc(actor.name) + ' (' + esc(actor.part) + ') declined <span class="fe-mono">' + esc(requestId) + '</span>' + note + '.</p></div>' +
                '<table class="fe-table"><tbody>' +
                '<tr><td>What changed</td><td>Nothing. ' + esc(cfg.subject()) + ' keeps its version and no new version is written.</td></tr>' +
                '<tr><td>What the maker can do</td><td>Change the request and raise it again, or leave it. The declined request stays on record.</td></tr></tbody></table>' +
                '<div class="fe-cards"><button type="button" class="fe-card" data-fe="withdraw"><b>Back to the review</b>Change the request and raise it again.</button></div>';
        };

        fe.html = function (state) {
            return state === 'waiting' ? fe.waiting() : state === 'decide' ? fe.decide() : fe.declined();
        };

        /* A click on a data-fe control. Returns the state to go to, or null
         * when the click was not one of these controls. */
        fe.click = function (el) {
            var act = el.getAttribute('data-fe');
            if (act === null) return null;
            if (act === 'opendecide') return 'decide';
            if (act === 'withdraw') { fe.reset(); return 'review'; }
            if (el.disabled) return 'stay';
            var changes = cfg.getChanges();
            if (act === 'approve') {
                var part = (actorOf(S.actor) || ACTORS[0]).part;
                S.answers[part] = 'approved';
                S.comment = '';
                if (allApproved(fe.parts(changes))) { S.applied = true; return 'outcome'; }
                return 'waiting';
            }
            if (act === 'decline') {
                S.declinedComment = S.comment;
                S.comment = '';
                return 'declined';
            }
            return null;
        };

        fe.input = function (el) {
            if (el.getAttribute('data-fe-comment') !== null) { S.comment = el.value; return true; }
            if (el.getAttribute('data-fe-actor') !== null) { S.actor = el.value; return true; }
            return false;
        };

        fe.params = function (p) {
            var actor = p.get('actor');
            if (actor !== null && actorOf(actor)) S.actor = actor;
            var maker = p.get('maker');
            if (maker !== null && actorOf(maker)) S.maker = maker;
            var check = p.get('check');
            if (check !== null) S.check = check;
        };

        return fe;
    }

    root.FourEyes = { create: create, actors: ACTORS };
})(window);
