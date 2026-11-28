/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * PROTOTYPE. Mock data, no backend, no build step. The design note beside this
 * file names every value that is fake and the operation each screen would call.
 */

const SCREENS = ['secrets', 'secret', 'consumers', 'uses', 'states'];

const STATES = {
    secrets: ['empty', 'full', 'refused'],
    secret: ['masked', 'revealed', 'refused'],
    consumers: ['empty', 'full', 'refused'],
    uses: ['empty', 'full', 'refused'],
    states: ['full']
};

const SECRETS = [
    { name: 'db.marketdata_service.password', owner: 'marketdata_service', version: 3, key: 2, read: '2 hours ago' },
    { name: 'db.inbox_service.password', owner: 'inbox_service', version: 1, key: 2, read: 'yesterday' },
    { name: 'ores.marketdata.vendor.api_key', owner: 'marketdata_service', version: 2, key: 2, read: 'never' },
    { name: 'db.vault_service.password', owner: 'vault_service', version: 4, key: 2, read: '4 days ago' }
];

const VERSIONS = [
    { version: 3, when: '2 hours ago', by: 'vault provision', reason: 'system.update' },
    { version: 2, when: 'yesterday', by: 'marco', reason: 'rotate the tick writer key' },
    { version: 1, when: '4 days ago', by: 'vault provision', reason: 'system.initial_load' }
];

const CONSUMERS = [
    { name: 'marketdata_service', subject: 'marketdata_service@system.ores', key: '8f2c…a1', enabled: true },
    { name: 'inbox_service', subject: 'inbox_service@system.ores', key: '3d90…7e', enabled: true },
    { name: 'reporting_service', subject: 'reporting_service@system.ores', key: 'c41b…02', enabled: false }
];

const USES = [
    { when: '2 hours ago', secret: 'db.marketdata_service.password', consumer: 'marketdata_service', actor: 'marketdata_service@system.ores', key: 2 },
    { when: 'yesterday', secret: 'db.inbox_service.password', consumer: 'inbox_service', actor: 'inbox_service@system.ores', key: 2 },
    { when: 'yesterday', secret: 'ores.marketdata.vendor.api_key', consumer: 'marketdata_service', actor: 'marco@acme.example', key: 1 }
];

const FAKE_VALUE = 'f3a9-tyJ_03fl7eLhAn8_fv3fHvRFzhm';

function params() {
    const q = new URLSearchParams(window.location.search);
    const screen = SCREENS.includes(q.get('screen')) ? q.get('screen') : 'secrets';
    const allowed = STATES[screen];
    const state = allowed.includes(q.get('state')) ? q.get('state') : allowed[0];
    return { screen, state };
}

function href(screen, state) {
    return `?screen=${screen}&state=${state}`;
}

function el(html) {
    const t = document.createElement('template');
    t.innerHTML = html.trim();
    return t.content.firstElementChild;
}

function crumbs(parts) {
    return `<div class="crumbs">${parts.join(' › ')}</div>`;
}

function titleRow(title, lede, actions) {
    return `
        <div class="title-row">
            <div>
                <h1>${title}</h1>
                <p class="lede">${lede}</p>
            </div>
            <div class="actions">${actions || ''}</div>
        </div>`;
}

function listCard(rows) {
    return `<div class="card"><table>
        <thead><tr><th>Name</th><th>Consumer</th><th>Current</th><th>Key</th><th>Last read</th></tr></thead>
        <tbody>${rows.map(r => `
            <tr>
                <td class="name"><a href="${href('secret', 'masked')}">${r.name}</a></td>
                <td>${r.owner}</td>
                <td>version ${r.version}</td>
                <td>${r.key}</td>
                <td>${r.read}</td>
            </tr>`).join('')}
        </tbody></table>
        <div class="pager">Showing 1-${rows.length} of ${rows.length}</div></div>`;
}

function emptyCard(what, add) {
    return `<div class="card"><div class="empty">
        <strong>${what}</strong>
        Nothing is here yet.${add ? ' Add the first one to begin.' : ''}
    </div>${add ? `<div style="text-align:center"><button class="primary">Add</button></div>` : ''}</div>`;
}

function refusedCard(what, detail) {
    return `<div class="card"><div class="refused"><strong>${what}</strong><br>${detail}</div></div>`;
}

function screenSecrets(state) {
    const canWrite = state !== 'refused';
    const actions = `<button class="quiet">Refresh</button>${canWrite ? '<button class="primary">Add</button>' : ''}`;
    const head = crumbs(['Home', 'Secrets']) +
        titleRow('Secrets', 'The values this installation keeps for its own use.', actions);
    const body = state === 'empty'
        ? emptyCard('No secrets yet', canWrite)
        : listCard(SECRETS);
    const access = canWrite
        ? ''
        : '<div class="access">Read only: you hold <code>vault::secrets:read</code> and not the write, so Add is not drawn.</div>';
    return head + access + body;
}

function screenSecret(state) {
    const canWrite = state !== 'refused';
    const revealed = state === 'revealed';
    const s = SECRETS[0];
    const actions = `${canWrite ? '<button>Change</button><button class="danger">Delete</button>' : ''}`;
    const valueBlock = revealed
        ? `<div class="value"><span class="secret revealed">${FAKE_VALUE}</span><span class="badge on">revealed</span></div>
           <div class="note">This reveal was recorded: the vault recorded the gateway's resolve, and the gateway recorded you.</div>`
        : `<div class="value"><span class="secret">••••••••••••</span>${canWrite ? '<button>Reveal</button>' : ''}</div>
           <div class="note">The value stays masked until it is asked for, and the ask is recorded.</div>`;
    const body = canWrite
        ? valueBlock
        : `<div class="refused"><strong>No read</strong><br>You do not hold <code>vault::secrets:read</code>, so the reveal is not drawn.</div>`;
    return crumbs(['Home', 'Secrets', s.name]) +
        titleRow(s.name, `version ${s.version} · owned by ${s.owner}`, actions) +
        `<div class="tabs"><span class="on">Overview</span><span>Versions</span><span>Uses</span></div>
         <div class="card">${body}
            <div class="meta" style="margin-top:0.75rem">Key version ${s.key} · last rotated 27 Nov 2026</div>
         </div>
         <div class="card"><h2>History</h2><table>
            <thead><tr><th>Version</th><th>When</th><th>By</th><th>Reason</th></tr></thead>
            <tbody>${VERSIONS.map(v => `<tr><td>${v.version}</td><td>${v.when}</td><td>${v.by}</td><td>${v.reason}</td></tr>`).join('')}</tbody>
         </table></div>`;
}

function screenConsumers(state) {
    const canWrite = state !== 'refused';
    const actions = `<button class="quiet">Refresh</button>${canWrite ? '<button class="primary">Add</button>' : ''}`;
    const head = crumbs(['Home', 'Secrets', 'Consumers']) +
        titleRow('Consumers', 'The services that may receive a secret, and the key each one answers with.', actions);
    const access = canWrite ? '' : '<div class="access">Read only: Add is not drawn.</div>';
    if (state === 'empty') return head + access + emptyCard('No consumers yet', canWrite);
    const rows = CONSUMERS.map(c => `<tr>
        <td class="name"><a href="${href('consumers', 'full')}">${c.name}</a></td>
        <td>${c.subject}</td>
        <td class="name">${c.key}</td>
        <td><span class="badge ${c.enabled ? 'on' : ''}">${c.enabled ? 'enabled' : 'disabled'}</span></td>
    </tr>`).join('');
    return head + access + `<div class="card"><table>
        <thead><tr><th>Consumer</th><th>Subject</th><th>Key</th><th>Enabled</th></tr></thead>
        <tbody>${rows}</tbody></table>
        <div class="pager">Showing 1-${CONSUMERS.length} of ${CONSUMERS.length}</div></div>`;
}

function screenUses(state) {
    const head = crumbs(['Home', 'Secrets', 'Uses']) +
        titleRow('Uses', 'Every time a secret was resolved, and who asked for it.',
                 '<button class="quiet">Refresh</button>');
    if (state === 'refused') {
        return head + refusedCard('No read', 'You do not hold the read this list needs, so it is not drawn.');
    }
    if (state === 'empty') return head + emptyCard('No uses yet', false);
    const rows = USES.map(u => `<tr>
        <td>${u.when}</td>
        <td class="name">${u.secret}</td>
        <td>${u.consumer}</td>
        <td>${u.actor}</td>
        <td>${u.key}</td>
    </tr>`).join('');
    return head + `<div class="card"><table>
        <thead><tr><th>When</th><th>Secret</th><th>Consumer</th><th>Actor</th><th>Key</th></tr></thead>
        <tbody>${rows}</tbody></table>
        <div class="pager">Showing 1-${USES.length} of ${USES.length}</div></div>
        <div class="note">No rule in the record screen standard draws this list yet; this page is the proposal.</div>`;
}

function screenStates() {
    const all = Object.entries(STATES).flatMap(([screen, states]) =>
        states.map(state => `<div><a href="${href(screen, state)}">?screen=${screen}&amp;state=${state}</a></div>`));
    return crumbs(['Home', 'States']) +
        titleRow('Every state, at its own address', 'Each screen of the journey in its empty, full, revealed and refused states.') +
        `<div class="card"><div class="states-list">${all.join('')}</div></div>`;
}

const RENDERERS = {
    secrets: screenSecrets,
    secret: screenSecret,
    consumers: screenConsumers,
    uses: screenUses,
    states: screenStates
};

function renderNav(screen) {
    const labels = { secrets: 'Secrets', secret: 'Secret', consumers: 'Consumers', uses: 'Uses', states: 'States' };
    document.getElementById('nav').innerHTML = SCREENS.map(s =>
        `<a href="${href(s, STATES[s][0])}" ${s === screen ? 'aria-current="page"' : ''}>${labels[s]}</a>`).join('');
}

function renderStates(screen, state) {
    document.getElementById('states').innerHTML = STATES[screen].map(s =>
        `<a href="${href(screen, s)}" ${s === state ? 'aria-current="true"' : ''}>${s}</a>`).join('');
}

function render() {
    const { screen, state } = params();
    renderNav(screen);
    renderStates(screen, state);
    const app = document.getElementById('app');
    app.innerHTML = '';
    app.appendChild(el(`<div>${RENDERERS[screen](state)}</div>`));
}

window.addEventListener('popstate', render);
render();
