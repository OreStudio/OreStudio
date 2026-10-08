/* The registration door.
 *
 * PROTOTYPE. Throwaway. Mock data, no backend, no build step.
 *
 * The question this answers is what the registration door should look
 * like, and the answer is argued in three structural variants:
 *
 *   A  One panel          the form and the destination in a single
 *                         column, the confirmation replacing the form.
 *   B  Form and status    the form beside a live column that states
 *                         the tenant, the party and the role, and
 *                         whether the account will work at once.
 *   C  Step rail          the journey shape the bootstrap screens use:
 *                         an ordered rail, one step on the right.
 *
 * The five door states are the ones the target state defines: closed,
 * register, refused, created (usable) and waiting (pending).
 */

const deployment = {
    appName: 'ORE Studio',
    tenant: { name: 'Acme Corporation', hostname: 'acme.example.com' },
    party: { name: 'Acme Trading', category: 'Operational', businessCentre: 'London' },
    role: { name: 'Viewer' },
    rules: [
        'At least 12 characters',
        'An upper and a lower case letter',
        'At least one digit',
        'At least one symbol',
    ],
};

const STATES = ['closed', 'register', 'refused', 'created', 'waiting'];

const VARIANT_NAMES = { A: 'One panel', B: 'Form and status', C: 'Step rail, as provisioning' };

const ui = {
    variant: 'C',
    state: 'register',
    nominated: true,
    step: 0,
    form: { username: 'jane.doe', email: 'jane.doe@acme.example.com' },
};

/** The banner artwork the real screens carry, as the site publishes it. */
const SPLASH = '/OreStudio/projects/ores.web/packages/web/src/assets/ore-studio-splash.png';

/* ---------------------------------------------------------------- */
/* Small pieces                                                     */
/* ---------------------------------------------------------------- */

const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

function header() {
    return `
        <div class="banner">
            <img src="${SPLASH}" alt="" width="964" height="323">
            <div class="banner-caption">
                <span class="name">${esc(deployment.tenant.name)}</span>
                <span class="from">${esc(deployment.tenant.hostname)}</span>
            </div>
        </div>`;
}

function rulesList() {
    return `<ul class="rules">${deployment.rules.map((r) => `<li>${esc(r)}</li>`).join('')}</ul>`;
}

function fields(which) {
    const show = (name) => (which === 'all' || which === name);
    let html = '';
    if (show('identity')) {
        html += `
        <div class="field">
            <label for="username">Username</label>
            <input id="username" name="username" autocomplete="username" value="${esc(ui.form.username)}">
        </div>
        <div class="field">
            <label for="email">Email address</label>
            <input id="email" name="email" type="email" autocomplete="email" value="${esc(ui.form.email)}">
        </div>`;
    }
    if (show('password')) {
        html += `
        <div class="field">
            <label for="password">Password</label>
            <input id="password" name="password" type="password" autocomplete="new-password">
        </div>
        <div class="field">
            <label for="confirm">Confirm password</label>
            <input id="confirm" name="confirm" type="password" autocomplete="new-password">
        </div>`;
    }
    if (show('all')) {
        html += rulesList();
    }
    return html;
}

/**
 * What the account receives, as the policy read answers it. The party
 * row is the one the whole journey turns on: with one, the account can
 * sign in; without one, it waits.
 */
function destinationRows() {
    const party = ui.nominated
        ? `<span class="given">${esc(deployment.party.name)}</span> &middot; ${esc(deployment.party.category)} &middot; ${esc(deployment.party.businessCentre)}`
        : '<span class="none">None nominated</span>';
    return `
        <table class="kv">
            <tr><td>Tenant</td><td>${esc(deployment.tenant.name)}</td></tr>
            <tr><td>Party</td><td>${party}</td></tr>
            <tr><td>Baseline role</td><td>${esc(deployment.role.name)}</td></tr>
        </table>`;
}

function statusBanner() {
    return ui.nominated
        ? `<div class="status-banner ok">
               <div class="head">Ready to use</div>
               <p>Your account joins ${esc(deployment.party.name)} and can sign in as soon as it is created.</p>
           </div>`
        : `<div class="status-banner warn">
               <div class="head">Waits for an administrator</div>
               <p>This deployment nominates no default party, so your account exists but cannot sign in until an administrator adds you to one.</p>
           </div>`;
}

function closedPanel() {
    return `
        <div class="card">
            <div class="stepheader">${header()}</div>
            <h2>Registration is closed</h2>
            <p class="lead">This deployment does not accept self-registration. Ask an administrator to create your account.</p>
            <div class="panel-soft">
                <span class="code">signup_disabled</span>
            </div>
            <div class="stepfoot">
                <button class="btn" data-act="to-door">Back to sign in</button>
            </div>
        </div>`;
}

function refusalNotice() {
    return `
        <div class="notice bad">
            That username is already taken. Choose another one.
            <span class="code">username_taken</span>
        </div>`;
}

function outcomePanel(withSummary) {
    const pending = ui.state === 'waiting';
    const summary = withSummary
        ? `<div class="panel-soft" style="margin-top:20px;text-align:left">${destinationRows()}</div>`
        : '';
    return `
        <div class="panel">
            <div class="outcome">
                <div class="mark ${pending ? 'warn' : 'ok'}">${pending ? '&#9203;' : '&#10003;'}</div>
                <h1 style="margin-top:12px">${pending ? 'Your account is waiting' : 'Your account is ready'}</h1>
                <p class="lede" style="margin:10px auto 0">
                    ${pending
                        ? 'An administrator must add you to a party before you can sign in.'
                        : 'Sign in with the username and password you just chose.'}
                </p>
                ${pending ? '<span class="code">account_pending</span>' : ''}
            </div>
            ${summary}
            <div class="actions" style="justify-content:center">
                <button class="btn primary" data-act="to-door">Go to sign in</button>
            </div>
        </div>`;
}

/* ---------------------------------------------------------------- */
/* Variant A: one panel, one conversation                           */
/* ---------------------------------------------------------------- */

function renderA() {
    if (ui.state === 'closed') {
        return `<div class="page narrow">${closedPanel()}</div>`;
    }
    if (ui.state === 'created' || ui.state === 'waiting') {
        return `<div class="page narrow">${header()}${outcomePanel(true)}</div>`;
    }
    return `
        <div class="page narrow">
            ${header()}
            <h1>Create your account</h1>
            <p class="lede">Choose how you sign in. Your account starts with the access this deployment gives every new member.</p>
            <form class="panel" data-act="form">
                ${ui.state === 'refused' ? refusalNotice() : ''}
                ${fields('all')}
                <div class="actions">
                    <button class="btn primary" type="submit">Create account</button>
                    <button class="btn ghost" type="button" data-act="to-door">Back to sign in</button>
                </div>
            </form>
            <div class="panel-soft">
                <h2>What your account receives</h2>
                ${destinationRows()}
                ${statusBanner()}
            </div>
        </div>`;
}

/* ---------------------------------------------------------------- */
/* Variant B: the form beside a live status column                  */
/* ---------------------------------------------------------------- */

function renderB() {
    if (ui.state === 'closed') {
        return `<div class="page narrow">${closedPanel()}</div>`;
    }
    const done = ui.state === 'created' || ui.state === 'waiting';
    const left = done
          ? `<div class="panel">
                 <div class="outcome">
                     <div class="mark ${ui.state === 'waiting' ? 'warn' : 'ok'}">${ui.state === 'waiting' ? '&#9203;' : '&#10003;'}</div>
                     <h2 style="margin-top:10px">${ui.state === 'waiting' ? 'Waiting for an administrator' : 'Account created'}</h2>
                     <p class="lede" style="margin:8px auto 0">
                         ${ui.state === 'waiting'
                             ? 'You cannot sign in yet. An administrator must add you to a party.'
                             : 'Return to the door and sign in.'}
                     </p>
                 </div>
                 <div class="actions" style="justify-content:center">
                     <button class="btn primary" data-act="to-door">Go to sign in</button>
                 </div>
             </div>`
          : `<form class="panel" data-act="form">
                 ${ui.state === 'refused' ? refusalNotice() : ''}
                 <h2>Your details</h2>
                 ${fields('all')}
                 <div class="actions">
                     <button class="btn primary" type="submit">Create account</button>
                     <button class="btn ghost" type="button" data-act="to-door">Back to sign in</button>
                 </div>
             </form>`;

    return `
        <div class="page wide">
            ${header()}
            <h1>Create your account</h1>
            <p class="lede">The panel on the right states what you get before you submit, so nothing about this account is a surprise afterwards.</p>
            <div class="split">
                <div>${left}</div>
                <div class="panel">
                    <h2>Where this account will land</h2>
                    ${statusBanner()}
                    ${destinationRows()}
                </div>
            </div>
        </div>`;
}

/* ---------------------------------------------------------------- */
/* Variant C: the step rail                                         */
/* ---------------------------------------------------------------- */

const RAIL = ['Your details', 'Your password', 'Review', 'Done'];

const RAIL_LEAD = [
    'Choose the name you sign in with, and the address the deployment reaches you at.',
    'These are the rules this deployment enforces, read from the server rather than kept here.',
    'This is what your account receives. Nothing is created until you confirm.',
    'Your account exists. This is what it holds, and what it still waits for.',
];

/** One rail entry, in the shape the provisioning journeys render. */
function railEntry(label, index, current) {
    const state = index < current ? 'done' : index === current ? 'current' : 'ahead';
    const mark = state === 'done' ? '&#10003;' : String(index + 1);
    return `<li class="railentry ${state}"><span class="railmark ${state}">${mark}</span>${esc(label)}</li>`;
}

function renderC() {
    if (ui.state === 'closed') {
        return `<div class="page narrow">${closedPanel()}</div>`;
    }

    const step = ui.state === 'created' || ui.state === 'waiting' ? 3 : ui.state === 'refused' ? 2 : ui.step;
    const pending = ui.state === 'waiting';
    let body = '';
    let foot = '';
    if (step === 0) {
        body = `<form data-act="form" id="step-form">${fields('identity')}</form>`;
        foot = `<button class="btn ghost" type="button" data-act="to-door">Back to sign in</button>
                <button class="btn primary ml-auto" type="button" data-act="next">Continue</button>`;
    } else if (step === 1) {
        body = `<form data-act="form" id="step-form">${fields('password')}</form>`;
        foot = `<button class="btn ghost" type="button" data-act="back">Back</button>
                <button class="btn primary ml-auto" type="button" data-act="next">Continue</button>`;
    } else if (step === 2) {
        body = `<form data-act="form" id="step-form">
                    ${ui.state === 'refused' ? refusalNotice() : ''}
                    ${destinationRows()}
                    ${statusBanner()}
                </form>`;
        foot = `<button class="btn ghost" type="button" data-act="back">Back</button>
                <button class="btn primary ml-auto" type="submit" form="step-form">Create account</button>`;
    } else {
        body = `<div class="outcome">
                    <div class="mark ${pending ? 'warn' : 'ok'}">${pending ? '&#9203;' : '&#10003;'}</div>
                    <p class="lead" style="margin:12px 0 18px">
                        ${pending
                            ? 'An administrator must add you to a party before you can sign in.'
                            : 'Sign in with the username and password you just chose.'}
                    </p>
                    ${pending ? '<span class="code">account_pending</span>' : ''}
                </div>
                ${destinationRows()}`;
        foot = `<button class="btn primary ml-auto" type="button" data-act="to-door">Go to sign in</button>`;
    }

    return `
        <div class="page">
            <div class="journey">
                <nav aria-label="Journey steps" class="railnav">
                    <ol>${RAIL.map((label, i) => railEntry(label, i, step)).join('')}</ol>
                </nav>
                <section class="card">
                    <div class="stepheader">${header()}</div>
                    <h2>${esc(RAIL[step])}</h2>
                    <p class="lead">${esc(RAIL_LEAD[step])}</p>
                    ${body}
                    <div class="stepfoot">${foot}</div>
                </section>
            </div>
        </div>`;
}

/* ---------------------------------------------------------------- */
/* Wiring                                                           */
/* ---------------------------------------------------------------- */

const RENDERERS = { A: renderA, B: renderB, C: renderC };

function stateOf(event, form) {
    const data = new FormData(form);
    ui.form.username = data.get('username') ?? ui.form.username;
    ui.form.email = data.get('email') ?? ui.form.email;
    return ui.nominated ? 'created' : 'waiting';
}

function wire() {
    document.querySelectorAll('[data-act]').forEach((node) => {
        const act = node.getAttribute('data-act');
        if (act === 'form') {
            node.addEventListener('submit', (event) => {
                event.preventDefault();
                ui.state = stateOf(event, node);
                ui.step = 3;
                commit();
            });
        }
    });
    document.querySelectorAll('button[data-act="next"]').forEach((b) =>
        b.addEventListener('click', () => {
            ui.step = Math.min(ui.step + 1, 2);
            commit();
        }),
    );
    document.querySelectorAll('button[data-act="back"]').forEach((b) =>
        b.addEventListener('click', () => {
            ui.step = Math.max(ui.step - 1, 0);
            commit();
        }),
    );
    document.querySelectorAll('button[data-act="to-door"]').forEach((b) =>
        b.addEventListener('click', () => {
            ui.state = 'register';
            ui.step = 0;
            commit();
        }),
    );
}

function chrome() {
    document.getElementById('proto-note').textContent =
        `Prototype — throwaway — variant ${ui.variant} of 3: ${VARIANT_NAMES[ui.variant]}. C is accepted. Mock data, no server.`;

    document.getElementById('proto-bar').innerHTML = `
        <button data-move="-1" title="Previous variant (left arrow)">&lsaquo;</button>
        <span class="label"><b>${ui.variant}</b> (${esc(VARIANT_NAMES[ui.variant])})${ui.variant === 'C' ? ' — accepted' : ''}</span>
        <button data-move="1" title="Next variant (right arrow)">&rsaquo;</button>`;

    document.getElementById('proto-bar').querySelectorAll('button[data-move]').forEach((b) =>
        b.addEventListener('click', () => move(Number(b.getAttribute('data-move')))),
    );

    const stateBar = document.createElement('div');
    stateBar.className = 'proto-states';
    stateBar.innerHTML =
        STATES.map((s) => `<button data-state="${s}" class="${ui.state === s ? 'on' : ''}">${s}</button>`).join('') +
        `<button data-nominate class="${ui.nominated ? 'on' : ''}">default party: ${ui.nominated ? 'yes' : 'no'}</button>`;

    const existing = document.querySelector('.proto-states');
    if (existing) {
        existing.replaceWith(stateBar);
    } else {
        document.body.appendChild(stateBar);
    }
    stateBar.querySelectorAll('button[data-state]').forEach((b) =>
        b.addEventListener('click', () => {
            ui.state = b.getAttribute('data-state');
            ui.step = ui.state === 'register' ? 0 : ui.step;
            commit();
        }),
    );
    stateBar.querySelector('button[data-nominate]').addEventListener('click', () => {
        ui.nominated = !ui.nominated;
        commit();
    });
}

function move(delta) {
    const keys = Object.keys(RENDERERS);
    const i = keys.indexOf(ui.variant);
    ui.variant = keys[(i + delta + keys.length) % keys.length];
    commit();
}

function commit() {
    const url = new URL(location.href);
    url.searchParams.set('variant', ui.variant);
    url.searchParams.set('state', ui.state);
    url.searchParams.set('nominated', ui.nominated ? '1' : '0');
    history.replaceState(null, '', url);
    draw();
}

function draw() {
    document.getElementById('app').innerHTML = RENDERERS[ui.variant]();
    wire();
    chrome();
}

document.addEventListener('keydown', (event) => {
    const tag = document.activeElement?.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA') {
        return;
    }
    if (event.key === 'ArrowLeft') move(-1);
    if (event.key === 'ArrowRight') move(1);
});

(function boot() {
    const q = new URLSearchParams(location.search);
    if (RENDERERS[q.get('variant')]) {
        ui.variant = q.get('variant');
    }
    if (STATES.includes(q.get('state'))) {
        ui.state = q.get('state');
    }
    if (q.get('nominated') === '0') {
        ui.nominated = false;
    }
    draw();
})();
