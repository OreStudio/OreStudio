/* Profile screen prototype. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * Three structural variants of the profile screen for two actors, and six
 * states. The variants disagree about one thing: where the save boundary sits.
 * A saves the whole screen once, B saves each panel on its own, C walks the
 * panels as rail steps and defers the write to a review step.
 *
 * The states are the ones the requirement work named: the picker, the saved
 * pair, the half-written pair, a reporting line waiting for approval, and a
 * refusal of a field the caller does not own. */

(function () {
    'use strict';

    var VARIANTS = {
        A: {
            name: 'One page, one save',
            note: 'The journey document read literally: one screen, panels, one Save for everything the caller may write. The reason is asked once.'
        },
        B: {
            name: 'One page, per-panel save',
            note: 'Each writable panel carries its own action and its own reason, so a half-finished edit is never sent with the other record.'
        },
        C: {
            name: 'Panels as rail steps',
            note: 'The shape the setup journeys use: photo, identity, contact, review, one step at a time. The write waits for the review step.'
        }
    };

    var STATES = [
        ['view', 'View'],
        ['photo', 'Replace photo'],
        ['saved', 'Saved'],
        ['partial', 'Half written'],
        ['proposed', 'Line proposed'],
        ['refused', 'Refused']
    ];

    var STEPS = ['Photo', 'Identity', 'Contact details', 'Review'];

    /* Which rail step a state sits on. The identity step carries the reporting
       line, so both the proposal and the refusal belong to it. */
    var STATE_STEP = { photo: 0, view: 1, proposed: 1, refused: 1, saved: 3, partial: 3 };

    /* The images the deployment already holds, offered beside the upload.
       They are the repository's own stock faces: six of the 462 the seeder
       picks profile pictures from, copied in so the prototype is
       self-contained. See external/facestudio/. */
    var PHOTOS = {
        jane: 'photos/jane_doe.jpeg',
        tom: 'photos/tom_okafor.jpeg'
    };

    var HELD = [
        ['photos/held_01.jpeg', 'Ana Ribeiro'],
        ['photos/held_02.jpeg', 'Peter Novak'],
        ['photos/held_03.jpeg', 'Mei Lin'],
        ['photos/held_04.jpeg', 'Raj Anand']
    ];

    /* Rachel Smith is the signed-in tenant administrator, so the person whose
       record the screen shows is never the person the header names. */
    var SIGNED_IN = { member: 'jdoe', admin: 'rsmith' };

    var RECORDS = {
        jdoe: {
            username: 'jdoe',
            full_name: 'Jane Doe',
            job_title: 'Risk Analyst',
            account_type: 'User',
            sign_in_email: 'jane.doe@example.com',
            manager: { name: 'R. Smith', username: 'rsmith' },
            image: null,
            last_sign_in: '2 hours ago',
            password_age: '30 days',
            places: 3,
            roles: ['Trading', 'Viewer'],
            contact: {
                street_line_1: '1 Example Street',
                city: 'London',
                postal_code: 'EC2V 7HH',
                country_code: 'GB',
                phone: '+44 20 7000 0000',
                email: 'jane.doe@example.com',
                web_page: 'example.com'
            }
        },
        tokafor: {
            username: 'tokafor',
            full_name: 'Tom Okafor',
            job_title: 'Settlement Analyst',
            account_type: 'User',
            sign_in_email: 'tom.okafor@example.com',
            manager: { name: 'R. Smith', username: 'rsmith' },
            image: PHOTOS.tom,
            last_sign_in: 'yesterday',
            password_age: '12 days',
            places: 2,
            roles: ['Operations'],
            contact: {
                street_line_1: '9 Bridge Row',
                city: 'Manchester',
                postal_code: 'M1 4BT',
                country_code: 'GB',
                phone: '+44 161 400 0000',
                email: 'tom.okafor@example.com',
                web_page: ''
            }
        }
    };

    var S = {
        variant: 'A',
        actor: 'member',
        state: 'view',
        step: 1,
        chosen: null,
        picked: 'tokafor',
        proposal: { to: 'A. N. Other' }
    };

    /* The rail step follows the state unless the reviewer walked the rail. */
    function gotoState(next) {
        S.state = next;
        S.step = STATE_STEP[next];
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var v = (p.get('variant') || 'A').toUpperCase();
        if (VARIANTS[v]) S.variant = v;
        if (p.get('actor') === 'admin' || p.get('actor') === 'member') S.actor = p.get('actor');
        var st = p.get('state');
        for (var i = 0; i < STATES.length; i++) if (STATES[i][0] === st) gotoState(st);
        if (p.get('person') === 'jdoe' || p.get('person') === 'tokafor') S.picked = p.get('person');
    }

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function initials(name) {
        return name.split(/\s+/).map(function (w) { return w.charAt(0); }).join('').slice(0, 2).toUpperCase();
    }

    function subject() {
        return isSelf() ? RECORDS.jdoe : RECORDS[S.picked];
    }

    function signedIn() {
        return isSelf() ? SIGNED_IN.member : SIGNED_IN.admin;
    }

    /* The caller's own record, which is whose screen the member sees. */
    function isSelf() { return S.actor === 'member'; }

    function title() { return isSelf() ? 'My profile' : "Change someone's details"; }

    function lede() {
        return isSelf()
            ? 'Your photo, your name, your job title and how colleagues reach you. What you cannot change here, the panel says so.'
            : 'Correct a colleague\u2019s profile and contact details. Pick the person first; the panels are the ones they see.';
    }

    // ---------------------------------------------------------------- parts

    function avatar(rec, cls) {
        var img = rec.image
            ? '<img src="' + rec.image + '" alt="">'
            : '<span>' + esc(initials(rec.full_name)) + '</span>';
        return '<div class="' + (cls || 'avatar') + '">' + img + '</div>';
    }

    function textField(label, value, hint, readonly) {
        var input = readonly
            ? '<input value="' + esc(value) + '" disabled>'
            : '<input value="' + esc(value) + '">';
        return '<div class="field"><label>' + esc(label) + '</label>' + input +
            (hint ? '<div class="hint">' + hint + '</div>' : '') + '</div>';
    }

    function roField(label, value, hint) {
        return textField(label, value === '' ? '\u2014' : value, hint, true);
    }

    function panel(head, why, body, opts) {
        opts = opts || {};
        return '<section class="panel' + (opts.readonly ? ' readonly' : '') + '">' +
            '<h2>' + esc(head) + '</h2>' +
            (why ? '<p class="why">' + why + '</p>' : '') +
            body + '</section>';
    }

    function reasonRow(id) {
        return '<div class="field" style="margin-top:16px"><label>Why is the record changing?</label>' +
            '<input placeholder="A reason is recorded with the new version" id="' + id + '"></div>';
    }

    function saveFoot(label, action, id) {
        return '<div class="actions"><button class="btn primary" data-act="' + action + '">' + esc(label) + '</button>' +
            '<button class="btn ghost" data-act="reset">Cancel</button></div>';
    }

    // ---------------------------------------------------------- the panels

    function searchPanel() {
        if (isSelf()) return '';
        var rows = [
            ['jdoe', 'Jane Doe', 'jdoe \u00b7 Risk Analyst \u00b7 London'],
            ['tokafor', 'Tom Okafor', 'tokafor \u00b7 Settlement Analyst \u00b7 Manchester']
        ].map(function (r) {
            var on = S.picked === r[0] ? ' style="color:var(--accent-bright)"' : '';
            return '<div class="row" data-act="pick" data-person="' + r[0] + '"' + on + '>' +
                '<span>' + esc(r[1]) + '</span><span class="sub">' + esc(r[2]) + '</span></div>';
        }).join('');
        return panel('Find the person',
            'iam.v1.accounts.list, inside your tenant. The picker is the whole difference between this journey and the member\u2019s.',
            '<div class="search"><div class="field"><label>Username or name</label>' +
            '<input value="' + (S.picked === 'jdoe' ? 'jane' : 'tom') + '" placeholder="Search this tenant"></div>' +
            '<button class="btn">Find</button></div>' +
            '<div class="results">' + rows + '</div>');
    }

    function photoInto(rec, opts) {
        if (S.state === 'photo') {
            var chosen = S.chosen || rec.image || HELD[0][0];
            var thumbs = HELD.map(function (p) {
                return '<div class="thumb' + (chosen === p[0] ? ' on' : '') + '" title="' + esc(p[1]) +
                    '" data-act="photo" data-photo="' + p[0] + '"><img src="' + p[0] + '" alt=""></div>';
            }).join('');
            return '<div class="picker">' +
                '<div><div class="dropzone"><div class="big">\u2191</div>' +
                '<div>Drop an image, or <b>choose a file</b></div>' +
                '<div class="hint">PNG, JPEG or WebP \u00b7 up to 2 MB \u00b7 at least 128\u00d7128</div></div>' +
                '<div class="hint">Uploaded through ores.assets; the reply is the new image id.</div>' +
                '</div>' +
                '<div><div class="hint" style="margin:0 0 4px">Images this tenant already holds</div>' +
                '<div class="thumbstrip">' + thumbs + '</div>' +
                '<div class="hint" style="display:flex;align-items:center;gap:8px;margin-top:14px">' +
                'At the size other screens use ' + avatarSmall(chosen) + '</div>' +
                '<div class="actions" style="margin-top:14px">' +
                '<button class="btn primary" data-act="use-photo">Use this photo</button>' +
                '<button class="btn ghost" data-act="state" data-state="view">Cancel</button>' +
                '</div></div></div>';
        }
        return '<div class="identity">' +
            '<div class="avatar-wrap">' + avatar(rec) +
            (rec.image ? '' : '<div class="hint">No photo yet</div>') +
            (opts.canWrite ? '<button class="btn" data-act="state" data-state="photo">Replace photo</button>' : '') +
            '</div>' +
            '<div>' +
            textField('Username', rec.username, 'Set at creation and never changed here.', true) +
            textField('Sign-in address', rec.sign_in_email, 'The address you sign in with. <b>Change someone\u2019s access</b> and <b>Protect my account</b> own it.', true) +
            '</div></div>';
    }

    function avatarSmall(image) {
        return '<span class="avatar-small"><img src="' + image + '" alt=""></span>';
    }

    function identityFields(rec, canWrite) {
        return '<div class="field"><label>Photo, name and job title</label>' +
            '<div class="hint" style="margin:0 0 12px">A correction here changes how history reads, because the name is what provenance shows.</div></div>' +
            textField('Full name', rec.full_name, '', !canWrite) +
            textField('Job title', rec.job_title, '', !canWrite) +
            roField('Account type', rec.account_type, 'Only the deployment and the seed profile set this.');
    }

    function reportingLine(rec, canWrite) {
        var approved = S.state !== 'proposed' && S.proposal && S.proposal.approved;
        var shown = approved ? S.proposal.to : rec.manager.name;
        var tag = S.state === 'proposed'
            ? '<span class="tag proposed">proposed</span>'
            : (approved ? '<span class="tag approved">approved today</span>' : '');
        var line = '<div class="line"><span class="who">Reports to <b>' + esc(shown) + '</b></span>' +
            '<span class="tag">' + esc(rec.manager.username) + '</span>' + tag +
            (canWrite && S.state !== 'proposed' && !approved
                ? '<button class="btn ghost" data-act="propose">Propose a change</button>' : '') + '</div>';

        if (S.state === 'proposed') {
            var approver = rec.manager.name;
            line += '<div class="proposal">' +
                '<p>Proposed: reports to <b>' + esc(S.proposal ? S.proposal.to : 'A. N. Other') + '</b>. ' +
                'Waiting for approval.</p>' +
                '<p class="approvers">Who can approve: ' + esc(approver) + ' (your manager) or a tenant administrator. ' +
                'The line does not change until one of them agrees.</p>' +
                '<p class="hint" style="margin:0">Nothing in the platform holds an approval today. The workflow engine can hold the ' +
                'instance; the queue, the notification and the decision are the Membership group\u2019s, and this screen links to it.</p>' +
                '</div>';
        }
        return line;
    }

    function identityPanel(rec, opts) {
        var canWrite = opts.canWrite;
        var why = canWrite
            ? (isSelf()
                ? 'You may change your own name, job title and photo \u2014 <b>iam.v1.ops.update_self_account</b>.'
                : 'You hold <b>iam::accounts:update</b> in this tenant.')
            : 'Read-only for you. <b>iam.v1.ops.update_self_account</b> is what makes the rest of this panel yours.';
        var body = photoInto(rec, { canWrite: canWrite }) +
            '<div style="margin-top:18px">' + identityFields(rec, canWrite) + '</div>' +
            '<div class="field"><label>Reporting line</label></div>' + reportingLine(rec, canWrite);
        if (opts.perPanel) body += reasonRow('why-identity') + saveFoot('Save identity', 'save-identity');
        if (opts.singleSave) body = body;
        return panel('Photo and identity', why, body, { readonly: !canWrite });
    }

    function contactPanel(rec, opts) {
        var canWrite = opts.canWrite;
        var c = rec.contact;
        var why = canWrite
            ? (isSelf()
                ? 'You may change your own contact record \u2014 <b>iam.v1.ops.update_self_account_contact_information</b>.'
                : 'You hold <b>iam::account_contact_informations:write</b> in this tenant.')
            : 'Read-only for you.';
        var body =
            textField('Street', c.street_line_1, '', !canWrite) +
            '<div class="picker">' +
            textField('City', c.city, '', !canWrite) +
            textField('Postcode', c.postal_code, '', !canWrite) +
            '</div>' +
            '<div class="picker">' +
            textField('Country', c.country_code, 'Validated against the tenant\u2019s country list.', !canWrite) +
            textField('Telephone', c.phone, '', !canWrite) +
            '</div>' +
            textField('Contact email', c.email,
                'The address colleagues use. It is <b>not</b> the sign-in address, which is <b>' +
                esc(rec.sign_in_email) + '</b> and is changed elsewhere.', !canWrite) +
            textField('Web page', c.web_page, '', !canWrite);
        if (opts.perPanel) body += reasonRow('why-contact') + saveFoot('Save contact details', 'save-contact');
        if (opts.bare) return body;
        return panel('Contact details', why, body, { readonly: !canWrite });
    }

    function accessPanel(rec) {
        return panel('Sign-in and access',
            'Read-only here. This is <b>Protect my account</b> and <b>Know what I may do</b>; nothing on this screen writes it.',
            '<table class="kv">' +
            '<tr><td>Last sign-in</td><td>' + esc(rec.last_sign_in) + '</td></tr>' +
            '<tr><td>Password</td><td>changed ' + esc(rec.password_age) + ' ago</td></tr>' +
            '<tr><td>Signed in</td><td>' + esc(rec.places) + ' places</td></tr>' +
            '<tr><td>Roles</td><td>' + esc(rec.roles.join(', ')) + '</td></tr>' +
            '</table>', { readonly: true });
    }

    // ------------------------------------------------------------ outcomes

    function wroteTable(contactOk) {
        return '<table class="wrote">' +
            '<tr><td class="rec">Account <span class="code">v14</span></td><td class="ok">written</td></tr>' +
            '<tr><td class="rec">Contact record <span class="code">v4</span></td><td class="' + (contactOk ? 'ok">written' : 'bad">failed') + '</td></tr>' +
            '</table>';
    }

    function outcome() {
        if (S.state === 'saved') {
            return panel('Saved',
                'Two records changed, so the screen names both. Nothing makes the pair atomic, and the screen does not pretend otherwise.',
                wroteTable(true) +
                '<div class="actions"><button class="btn" data-act="state" data-state="view">Back to the profile</button></div>');
        }
        if (S.state === 'partial') {
            return panel('The contact record did not save',
                'The account write landed. The contact write was refused, and only that record is named \u2014 an account that saved and a contact that did not is a state the person must be able to see.',
                '<div class="notice bad" style="margin-bottom:12px">' +
                '<b>iam.account_contact_information.write_failed</b> \u2014 the country code <b>XX</b> is not in this tenant\u2019s list.</div>' +
                wroteTable(false) +
                '<div class="actions"><button class="btn primary" data-act="state" data-state="view">Fix the contact record</button>' +
                '<button class="btn ghost" data-act="state" data-state="view">Leave it for now</button></div>', { readonly: false });
        }
        return '';
    }

    function refusal() {
        return '<div class="notice bad"><b>field_not_self_writable</b> \u2014 ' +
            (isSelf()
                ? 'your username and your sign-in address are not yours to change here. A tenant administrator changes them.'
                : 'the sign-in address is changed by the Access journey, not from this screen.') + '</div>';
    }

    // ------------------------------------------------------------ variants

    function variantA() {
        var rec = subject();
        var out = outcome();
        if (out) return out;
        return searchPanel() +
            identityPanel(rec, { canWrite: true }) +
            contactPanel(rec, { canWrite: true }) +
            accessPanel(rec) +
            panel('Save',
                'One reason, one save, both records. The two writes are still two writes.',
                reasonRow('why-all') + saveFoot('Save changes', 'save-all'));
    }

    function variantB() {
        var rec = subject();
        var out = outcome();
        if (out) return out;
        return searchPanel() +
            identityPanel(rec, { canWrite: true, perPanel: true }) +
            contactPanel(rec, { canWrite: true, perPanel: true }) +
            accessPanel(rec);
    }

    function variantC() {
        var rec = subject();
        var step = S.step;
        var out = outcome();
        var rail = '<nav class="railnav"><ol>' + STEPS.map(function (s, i) {
            var cls = i === step ? 'current' : (i < step ? 'done' : 'ahead');
            var mark = i < step ? '\u2713' : String(i + 1);
            return '<li class="railentry ' + cls + '"><span class="railmark ' + cls + '">' + mark + '</span>' + esc(s) + '</li>';
        }).join('') + '</ol></nav>';

        var card;
        if (out) {
            card = out;
        } else if (step === 0) {
            card = '<div class="card"><h2>Photo</h2><p class="lead">A picture other people see beside your name.</p>' +
                photoInto(rec, { canWrite: true }) + stepFoot(0) + '</div>';
        } else if (step === 1) {
            card = '<div class="card"><h2>Identity</h2>' +
                '<p class="lead">The fields that appear in provenance, and the line you report on.</p>' +
                (S.state === 'refused' ? refusal() : '') +
                identityFields(rec, true) +
                '<div class="field"><label>Reporting line</label></div>' + reportingLine(rec, true) +
                stepFoot(1) + '</div>';
        } else if (step === 2) {
            card = '<div class="card"><h2>Contact details</h2><p class="lead">Where you are, and how colleagues reach you.</p>' +
                contactPanel(rec, { canWrite: true, bare: true }) + stepFoot(2) + '</div>';
        } else {
            card = '<div class="card"><h2>Review</h2><p class="lead">What will be sent, and to which record.</p>' +
                wroteTable(true).replace(/written/g, 'will be written') + reasonRow('why-rail') +
                '<div class="stepfoot"><button class="btn ghost" data-act="step" data-step="2">Back</button>' +
                '<button class="btn primary ml-auto" data-act="save-all">Save changes</button></div></div>';
        }
        return '<div class="journey">' + rail + '<div>' + searchPanel() + card + '</div></div>';
    }

    function stepFoot(i) {
        var back = i > 0 ? '<button class="btn ghost" data-act="step" data-step="' + (i - 1) + '">Back</button>' : '';
        var next = '<button class="btn primary ml-auto" data-act="step" data-step="' + (i + 1) + '">Continue</button>';
        return '<div class="stepfoot">' + back + next + '</div>';
    }

    function render() {
        var rec = subject();
        var head = '<div class="page">' +
            '<div class="brand">ORE Studio</div>' +
            '<div class="tenant">Northwind Capital \u00b7 signed in as <b>' + esc(signedIn()) + '</b>' +
            (isSelf() ? '' : ' \u00b7 editing <b>' + esc(rec.username) + '</b>') + '</div>' +
            '<h1>' + esc(title()) + '</h1>' +
            '<p class="lede">' + esc(lede()) + '</p>';

        var inner;
        if (S.variant === 'A') inner = variantA();
        else if (S.variant === 'B') inner = variantB();
        else inner = variantC();

        if (S.state === 'refused' && S.variant !== 'C') inner = refusal() + inner;

        document.getElementById('app').innerHTML = head + inner + '</div>';

        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, no service \u00b7 variant ' + S.variant + ' \u00b7 ' +
            (isSelf() ? 'member on their own record' : 'tenant administrator on a colleague') +
            ' \u00b7 state ' + S.state;

        renderBar();
    }

    function renderBar() {
        var variantButtons = Object.keys(VARIANTS).map(function (k) {
            return '<button data-act="variant" data-variant="' + k + '"' + (S.variant === k ? ' class="on"' : '') + '>' + k + '</button>';
        }).join('');
        var actorButtons = '<span class="label">actor</span>' +
            '<button data-act="actor" data-actor="member"' + (isSelf() ? ' class="on"' : '') + '>member</button>' +
            '<button data-act="actor" data-actor="admin"' + (!isSelf() ? ' class="on"' : '') + '>admin</button>';
        var stateButtons = STATES.map(function (s) {
            return '<button data-act="state" data-state="' + s[0] + '"' + (S.state === s[0] ? ' class="on"' : '') + '>' + esc(s[1]) + '</button>';
        }).join('');
        document.getElementById('proto-bar').innerHTML =
            '<span class="label">variant <b>' + S.variant + '</b> \u2014 ' + esc(VARIANTS[S.variant].name) + '</span>' +
            variantButtons + '<span class="label">|</span>' + actorButtons + '<span class="label">|</span>' + stateButtons;
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');
        if (act === 'variant') S.variant = el.getAttribute('data-variant');
        else if (act === 'actor') S.actor = el.getAttribute('data-actor');
        else if (act === 'state') gotoState(el.getAttribute('data-state'));
        else if (act === 'step') S.step = Number(el.getAttribute('data-step'));
        else if (act === 'pick') { S.actor = 'admin'; S.picked = el.getAttribute('data-person'); }
        else if (act === 'propose') gotoState('proposed');
        else if (act === 'photo') S.chosen = el.getAttribute('data-photo');
        else if (act === 'use-photo') { subject().image = S.chosen; gotoState('view'); }
        else if (act === 'save-identity' || act === 'save-contact') gotoState('saved');
        else if (act === 'save-all') gotoState(S.variant === 'C' ? 'saved' : 'partial');
        else if (act === 'reset') gotoState('view');
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
