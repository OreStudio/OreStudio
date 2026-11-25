/* Approval matrix prototype.
 *
 * One page, mock data, no server. The address is the state:
 *   ?state=party-unrestricted | party-empty | party-list | party-refused |
 *          book-inherits | book-narrows | book-refused-stale
 *
 * The interactions are local to the page: Change enters edit, Add appends,
 * Remove drops a row, and Save states that nothing is written. The states the
 * rail opens are the design; the design note beside this folder names the rule
 * each part follows and what is fake.
 */

const catalogue = [
    { code: 'Swap', name: 'Interest rate swap' },
    { code: 'FxForward', name: 'FX forward' },
    { code: 'FxOption', name: 'FX option' },
    { code: 'FxCollar', name: 'FX collar' },
    { code: 'Bond', name: 'Bond' },
    { code: 'CreditDefaultSwap', name: 'Credit default swap' },
    { code: 'CommodityForward', name: 'Commodity forward' },
    { code: 'EquityOption', name: 'Equity option' },
];

const nameOf = (code) => (catalogue.find((c) => c.code === code) || {}).name || code;

const party = {
    name: 'Acme Bank Ltd',
    key: 'ACME_BANK',
    version: 3,
    books: ['Cable Options', 'GBP Rates', 'Liquidity Buffer Bonds'],
};

const book = { name: 'Cable Options', key: 'CABLE-01', version: 7, party: party.name };

const states = {
    'party-unrestricted': { screen: 'party', restricted: false, set: [] },
    'party-empty': { screen: 'party', restricted: true, set: [], editing: true },
    'party-list': { screen: 'party', restricted: true, set: ['Swap', 'FxForward', 'FxOption'] },
    'party-refused': {
        screen: 'party',
        restricted: true,
        set: ['Swap', 'FxForward'],
        editing: true,
        refusal: {
            type: 'FxOption',
            book: 'Cable Options',
            text: 'Book Cable Options carries FX Option, which this party would not permit.',
        },
    },
    'book-inherits': { screen: 'book', restricted: false, set: [] },
    'book-narrows': { screen: 'book', restricted: true, set: ['FxOption'] },
    'book-refused-stale': {
        screen: 'book',
        restricted: true,
        set: ['FxOption', 'Bond'],
        editing: true,
        refusal: {
            text: 'Book Cable Options may not carry Bond: its party permits FX Forward, FX Option, Swap.',
        },
    },
};

const stateOrder = [
    'party-unrestricted',
    'party-empty',
    'party-list',
    'party-refused',
    'book-inherits',
    'book-narrows',
    'book-refused-stale',
];

function current() {
    const requested = new URLSearchParams(window.location.search).get('state');
    const key = states[requested] ? requested : 'party-list';
    return { key, ...states[key] };
}

function partySetOf(state) {
    if (state.screen === 'party') return state.restricted ? state.set : null;
    return ['Swap', 'FxForward', 'FxOption'];
}

function rail(active) {
    return `<div class="rail">${stateOrder
        .map((k) => `<a href="?state=${k}" class="${k === active ? 'on' : ''}">${k}</a>`)
        .join('')}</div>`;
}

function crumbs(trail) {
    return `<div class="crumbs">Refdata · ${trail}</div>`;
}

function tabs(active) {
    const names = ['Details', 'Identifiers', 'Contacts', 'Market approvals'];
    return `<div class="tabs">${names
        .map((n) => `<span class="${n === active ? 'on' : ''}">${n}</span>`)
        .join('')}</div>`;
}

function partyScreen(state) {
    const set = state.restricted ? state.set : null;
    const rows = set
        ? set
              .map(
                  (code) => `<tr>
        <td class="code">${code}</td><td>${nameOf(code)}</td>
        <td style="text-align:right">${
            state.editing ? `<button class="ghost" data-remove="${code}">Remove</button>` : ''
        }</td></tr>`,
              )
              .join('')
        : '';

    const list = set
        ? set.length
            ? `<table><thead><tr><th>Product type</th><th>Name</th><th></th></tr></thead><tbody>${rows}</tbody></table>`
            : `<div class="empty">This party permits nothing. A party with no rows and the set stated allows no trade.</div>`
        : '';

    const editor = state.editing
        ? `<div class="picker">
             <select id="add-select">${catalogue
                 .filter((c) => !set || !set.includes(c.code))
                 .map((c) => `<option value="${c.code}">${c.code} — ${c.name}</option>`)
                 .join('')}</select>
             <button data-add>Add</button>
           </div>`
        : '';

    const changeLabel = state.editing ? 'Cancel' : 'Change';
    const stateLine = state.restricted
        ? `Permits <b>only these product types</b>`
        : `Permits <b>every product type the catalogue states</b>`;

    return `${crumbs('Parties · ' + party.name)}
    <h1>${party.name}</h1>
    <p class="lead">${party.key} · version ${party.version}</p>
    ${tabs('Market approvals')}
    <div class="card">
      <div class="card-head">
        <h2>Market approvals</h2>
        <button class="ghost" data-change>${changeLabel}</button>
      </div>
      <p class="state">${stateLine}</p>
      ${list}${editor}
      ${
          state.refusal
              ? `<div class="refusal"><b>The set cannot be saved.</b> ${state.refusal.text}</div>`
              : ''
      }
      <div class="hint">${
          state.restricted
              ? 'Books of this party are approved under this set: Cable Options, GBP Rates, Liquidity Buffer Bonds.'
              : 'No book of this party can exceed a set the party does not state.'
      }</div>
      <div class="foot">
        ${state.editing ? `<button data-save class="primary">Save</button>` : ''}
        <button class="ghost" data-cancel>${state.editing ? 'Discard' : 'Close'}</button>
      </div>
    </div>`;
}

function bookScreen(state) {
    const permitted = partySetOf(state);
    const carried = state.restricted ? state.set : permitted;
    const rows = permitted
        .map((code) => {
            const checked = carried.includes(code);
            return `<tr>
        <td class="code">${code}</td><td>${nameOf(code)}</td>
        <td class="yes">yes</td>
        <td class="${checked ? 'yes' : 'no'}">${checked ? 'yes' : '—'}</td>
        <td style="text-align:right">${
            state.editing
                ? `<button class="ghost" data-toggle="${code}">${checked ? 'Remove' : 'Add'}</button>`
                : ''
        }</td></tr>`;
        })
        .join('');

    const stateLine = state.restricted
        ? `This book <b>narrows</b> what its party permits`
        : `This book <b>follows its party</b> and carries all of ${party.name}'s set`;

    return `${crumbs('Books · ' + book.name)}
    <h1>${book.name}</h1>
    <p class="lead">${book.key} · version ${book.version}</p>
    <div class="tabs"><span>Overview</span><span class="on">Product types</span><span>History</span></div>
    <div class="card">
      <div class="card-head">
        <h2>Product types</h2>
        <button class="ghost" data-change>${state.editing ? 'Cancel' : 'Change'}</button>
      </div>
      <p class="state">${stateLine}</p>
      <table>
        <thead><tr><th>Product type</th><th>Name</th><th>Party permits</th><th>This book carries</th><th></th></tr></thead>
        <tbody>${rows}</tbody>
      </table>
      ${
          state.refusal
              ? `<div class="refusal"><b>The set cannot be saved.</b> ${state.refusal.text}</div>`
              : ''
      }
      <div class="hint">A type the party does not permit is not offered here: the picker reads the party's set.</div>
      <div class="foot">
        ${state.editing ? `<button data-save class="primary">Save</button>` : ''}
        <button class="ghost" data-cancel>${state.editing ? 'Discard' : 'Close'}</button>
      </div>
    </div>`;
}

function render() {
    const state = current();
    const other = state.screen === 'party' ? 'book-inherits' : 'party-list';
    document.getElementById('app').innerHTML = `
    ${rail(state.key)}
    <p class="screen-switch">The other screen of this journey: <a href="?state=${other}">${
        state.screen === 'party' ? 'the book that narrows it' : "the party's set"
    }</a></p>
    ${state.screen === 'party' ? partyScreen(state) : bookScreen(state)}`;
    wire(state);
}

/* The page writes nothing. A control changes what is drawn, and Save says so,
 * because the prototype answers the question "what does the screen look like"
 * and not "what does the server do". */
function wire(state) {
    const go = (key) => {
        window.location.search = `?state=${key}`;
    };
    document.querySelectorAll('[data-change]').forEach((el) =>
        el.addEventListener('click', () => {
            if (state.key === 'party-list' || state.key === 'party-unrestricted') go('party-empty');
            else if (state.key === 'party-empty') go('party-list');
            else if (state.key === 'book-inherits') go('book-narrows');
            else if (state.key === 'book-narrows') go('book-inherits');
        }),
    );
    document.querySelectorAll('[data-cancel]').forEach((el) =>
        el.addEventListener('click', () => {
            if (state.editing) {
                if (state.screen === 'party') go('party-list');
                else go('book-narrows');
            }
        }),
    );
    document.querySelectorAll('[data-add]').forEach((el) =>
        el.addEventListener('click', () => {
            const pick = document.getElementById('add-select');
            if (pick) {
                document.getElementById('app').insertAdjacentHTML(
                    'beforeend',
                    `<div class="hint">The prototype adds nothing: a real screen writes the set and the refusal under it.</div>`,
                );
            }
        }),
    );
    document.querySelectorAll('[data-save]').forEach((el) =>
        el.addEventListener('click', () => {
            document.getElementById('app').insertAdjacentHTML(
                'beforeend',
                `<div class="hint">Nothing is written in the prototype. The states on the rail are the design.</div>`,
            );
        }),
    );
}

render();
