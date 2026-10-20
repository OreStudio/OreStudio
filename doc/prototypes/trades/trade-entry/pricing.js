/* The pricing screen.
 *
 * It reads a market data snapshot and values the structure from it. Nothing ticks by
 * itself: a screen does not read again unless the person asks, so a newer snapshot turns
 * Refresh gold and Refresh reads it. The valuation is faked on purpose. The number moves
 * with the tweaks because a static number would hide how the screen answers a change.
 *
 * A price of a structure goes back to the editor as a change to its working copy. A new
 * trade is booked on Book a trade, the one page that books.
 */

const market = marketState();
const openingSpots = { ...market.spots };
const openingVols = { ...market.vols };
const openingRates = { ...market.rates };

let structure = null;
let priceMode = 'structure';
let viewMode = 'condensed';
let lastNpv = null;
let stale = false;

/* --- Utilities -------------------------------------------------------- */

function esc(value) {
    return String(value === undefined || value === null ? '' : value)
        .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

function money(value) {
    return Number(value).toLocaleString(undefined, { maximumFractionDigits: 0 });
}

function allLegs() {
    return structure.groups.flatMap(g => g.components);
}

function legPair(leg) {
    const f = leg.fields;
    const currencies = [f.boughtCurrency, f.soldCurrency].filter(Boolean);
    if (currencies.length === 2) {
        const pair = currencies.join('');
        if (market.spots[pair] !== undefined) return pair;
        const flipped = currencies.slice().reverse().join('');
        if (market.spots[flipped] !== undefined) return flipped;
    }
    return 'EURUSD';
}

/* --- The fake valuation ----------------------------------------------- */

/* Monotonic in vol and in the moneyness of the leg, so a tweak moves the
 * number the way a trader expects it to move. It is not a price. */
function legNpv(leg) {
    const f = leg.fields;
    const pair = legPair(leg);
    const spot = market.spots[pair];
    const vol = market.vols[pair];
    const base = openingSpots[pair];
    const notional = Number(f.notional || 0);
    if (!notional) return 0;

    const sign = f.longShort === 'Short' ? -1 : 1;
    const volTerm = (vol / 8) * 0.021;
    const isCall = f.optionType !== 'Put';
    const spotTerm = ((spot / base) - 1) * (isCall ? 1 : -1) * 0.5;
    const rateTerm = (market.rates.USD - openingRates.USD) * 0.0015;

    return sign * notional * (volTerm + spotTerm + rateTerm);
}

function structureNpv() {
    return allLegs().reduce((sum, leg) => sum + legNpv(leg), 0);
}

/* --- Rendering --------------------------------------------------------- */

function render() {
    renderHeader();
    renderMarket();
    renderTweaks();
    renderValuation();
}

function renderHeader() {
    const meta = [chip(structure.mode === 'strategy' ? 'Strategy' : 'Package'), structure.pending.length ? chip(structure.pending.length + ' unsaved', 'warn', structure.pending.join('; ')) : null];
    const toggle = (label, items, current, pick) => h('div', { class: 'mode-switch', role: 'group', 'aria-label': label },
        items.map(([id, text]) => h('button', { class: id === current ? 'on' : '', onclick: () => pick(id) }, text)));
    const actions = [
        refreshButton({ stale, onClick: refreshMarket }),
        toggle('Pricing mode', [['structure', 'Structure'], ['leg', 'Leg']], priceMode, id => { priceMode = id; render(); }),
        toggle('Presentation', [['condensed', 'Condensed'], ['expanded', 'Expanded']], viewMode, id => { viewMode = id; render(); }),
        h('a', { class: 'btn', href: 'index.html' }, 'Structure editor')
    ];
    const id = h('span', null, structure.id, ' · ', allLegs().length, allLegs().length === 1 ? ' leg' : ' legs');
    fill($('#head'), pageHead({ title: structure.name, id, meta, actions }));
    renderActions();
}

/* The price of a structure is a change to its working copy. A single priced trade can be booked. */
function renderActions() {
    const book = bookLink();
    fill($('#price-actions'),
        h('button', { class: 'btn ghost', onclick: openTicket }, 'Price a new trade'),
        book ? h('a', { class: 'btn', href: book }, 'Book a trade') : null,
        h('button', { class: 'btn primary', onclick: usePrice, title: 'Stage this price as the initial NPV of the structure' }, 'Use this price'));
}

/* A priced ticket carries its type and terms to Book a trade in the address. */
function bookLink() {
    const legs = allLegs();
    if (legs.length !== 1) return '';
    const leg = legs[0];
    const type = TRADE_TYPES.find(t => t.code === leg.tradeTypeCode && t.instrument) || TRADE_TYPES.find(t => t.product === leg.productType && t.instrument);
    if (!type) return '';
    const f = leg.fields;
    const terms = type.instrument === 'fx_vanilla_option_instrument'
        ? { option_type: f.optionType, bought_currency: f.boughtCurrency, bought_amount: f.boughtAmount, sold_currency: f.soldCurrency, sold_amount: f.soldAmount, strike: f.strike, expiry_date: f.expiry }
        : {};
    const pairs = Object.entries(terms).filter(([, v]) => v).map(([k, v]) => k + ':' + v).join(',');
    return '../book-a-trade/?type=' + type.code + (pairs ? '&terms=' + encodeURIComponent(pairs) : '');
}

function renderMarket() {
    const rows = [];
    Object.entries(market.spots).forEach(([pair, spot]) => {
        const baseline = openingSpots[pair];
        const up = spot >= baseline;
        rows.push(`<tr>
            <td>${esc(pair)} spot</td>
            <td class="num">${spot.toFixed(4)}
                <span class="${up ? 'delta-up' : 'delta-down'}">${up ? '▲' : '▼'}</span></td>
        </tr>`);
    });
    document.getElementById('market-rows').innerHTML = rows.join('');
    document.getElementById('market-note').textContent = market.snapshot;
}

function renderTweaks() {
    const controls = [];

    Object.entries(market.vols).forEach(([pair, vol]) => {
        controls.push(`
            <div class="tweak">
                <label for="t-vol-${esc(pair)}"><span>${esc(pair)} vol</span><span data-readout="vol:${esc(pair)}">${vol.toFixed(2)}%</span></label>
                <input id="t-vol-${esc(pair)}" type="range" data-tweak="vol" data-key="${esc(pair)}"
                       min="1" max="25" step="0.05" value="${vol}">
            </div>`);
    });

    Object.entries(market.rates).forEach(([ccy, rate]) => {
        controls.push(`
            <div class="tweak">
                <label for="t-rate-${esc(ccy)}"><span>${esc(ccy)} rate</span><span data-readout="rate:${esc(ccy)}">${rate.toFixed(2)}%</span></label>
                <input id="t-rate-${esc(ccy)}" type="range" data-tweak="rate" data-key="${esc(ccy)}"
                       min="-1" max="12" step="0.01" value="${rate}">
            </div>`);
    });

    document.getElementById('tweaks').innerHTML = controls.join('');

    /* A tweak updates its own readout and the valuation, and rebuilds
     * nothing. Rebuilding the slider under the pointer would end the drag
     * on the first pixel, which is the whole interaction. */
    document.querySelectorAll('[data-tweak]').forEach(input => {
        input.addEventListener('input', () => {
            const kind = input.dataset.tweak;
            const key = input.dataset.key;
            const value = Number(input.value);
            if (kind === 'vol') market.vols[key] = value;
            else market.rates[key] = value;

            const readout = document.querySelector(`[data-readout="${kind}:${key}"]`);
            if (readout) readout.textContent = `${value.toFixed(2)}%`;

            renderMarket();
            renderValuation();
        });
    });
}

function renderValuation() {
    const npv = structureNpv();
    const ccy = structure.initialNpv ? structure.initialNpv.currency : 'USD';

    document.getElementById('npv-value').textContent = money(npv);
    document.getElementById('npv-ccy').textContent = ccy;

    const deltaEl = document.getElementById('npv-delta');
    if (lastNpv === null || Math.abs(npv - lastNpv) < 0.01) {
        deltaEl.textContent = '';
        deltaEl.className = '';
    } else {
        const diff = npv - lastNpv;
        deltaEl.textContent = `${diff > 0 ? '+' : ''}${money(diff)}`;
        deltaEl.className = diff > 0 ? 'delta-up' : 'delta-down';
    }

    const rows = [];
    if (viewMode === 'condensed') {
        rows.push(`<tr>
            <td>${esc(structure.name)}</td>
            <td class="num">${money(npv)}</td>
            <td class="num">${money(allLegs().reduce((s, l) => s + Number(l.fields.notional || 0), 0))}</td>
        </tr>`);
    } else {
        rows.push(`<tr>
            <td><strong>${esc(structure.name)}</strong></td>
            <td class="num"><strong>${money(npv)}</strong></td>
            <td class="num">${money(allLegs().reduce((s, l) => s + Number(l.fields.notional || 0), 0))}</td>
        </tr>`);

        structure.groups.forEach(group => {
            rows.push(`<tr><td colspan="3" class="legend">${esc(group.name)}</td></tr>`);
            group.components.forEach(leg => {
                rows.push(`<tr class="leg-mode-row">
                    <td>${esc(leg.label)} ${esc(leg.fields.longShort || '')}</td>
                    <td class="num">${money(legNpv(leg))}</td>
                    <td class="num">${money(Number(leg.fields.notional || 0))}</td>
                </tr>`);
            });
        });
    }

    document.getElementById('price-rows').innerHTML = rows.join('');
    document.getElementById('valuation-note').textContent = market.snapshot;

    lastNpv = npv;
}

/* --- Market data -------------------------------------------------------- */

function snapshotId() {
    return `MDS-${iso(LOADED).slice(0, 10)}-${iso(LOADED).slice(11, 16).replace(':', '')}`;
}

/* Refresh reads the next snapshot. A newer one is only announced: it turns Refresh gold. */
function refreshMarket() {
    Object.keys(market.spots).forEach(pair => {
        const drift = (Math.random() - 0.5) * 0.0016;
        market.spots[pair] = Math.max(0.0001, market.spots[pair] * (1 + drift));
    });
    LOADED = new Date(NOW.getTime());
    market.snapshot = snapshotId();
    stale = false;
    render();
    toast('Read the snapshot ' + market.snapshot);
}

/* --- The pricing ticket ------------------------------------------------ */

/* The pricing screen starts a deal from the least reference data it can
 * price from. Every other field the booking needs is the booker's job, and
 * Book a trade carries this much across. */
const TICKET = {};
let ticketDirty = false;

function openTicket() {
    const leg = allLegs()[0] || { fields: {} };
    const f = leg.fields || {};
    Object.assign(TICKET, {
        product: leg.productType || 'fx', longShort: f.longShort || 'Long', optionType: f.optionType === 'Put' ? 'Put' : 'Call',
        boughtCurrency: f.boughtCurrency || 'EUR', boughtAmount: f.boughtAmount || '1000000',
        soldCurrency: f.soldCurrency || 'USD', soldAmount: f.soldAmount || '1090000',
        strike: f.strike || '1.09', expiry: (f.expiry || '2026-03-10').slice(0, 10)
    });
    ticketDirty = false;
    const mark = key => (v) => { TICKET[key] = v; ticketDirty = true; };
    const input = (label, key, fd) => field(label, valueInput(fd, TICKET[key], mark(key)));
    fill($('#ticket-fields'),
        field('Product type', select(PRODUCT_TYPES.map(p => [p.code, p.name]), TICKET.product, mark('product'))),
        input('Long or short', 'longShort', { options: ['Long', 'Short'] }),
        input('Option type', 'optionType', { options: ['Call', 'Put'] }),
        input('Bought currency', 'boughtCurrency', { type: 'ccy' }),
        input('Bought amount', 'boughtAmount', { type: 'money' }),
        input('Sold currency', 'soldCurrency', { type: 'ccy' }),
        input('Sold amount', 'soldAmount', { type: 'money' }),
        input('Strike', 'strike', {}),
        input('Expiry', 'expiry', { type: 'date' }));
    const d = $('#ticket-dialog');
    d.querySelectorAll('[data-loaded]').forEach(el => { el.textContent = 'Loaded ' + iso(LOADED); el.title = 'Loaded'; });
    d.showModal();
}

function nextComponentId() {
    const numbers = allLegs()
        .map(l => Number(String(l.id).replace(/\D/g, '')))
        .filter(n => !Number.isNaN(n));
    return `T-${numbers.length ? Math.max(...numbers) + 1 : 9001}`;
}

function confirmTicket() {
    const bought = TICKET.boughtCurrency;
    const sold = TICKET.soldCurrency;
    const amount = String(TICKET.boughtAmount).trim();
    const expiry = TICKET.expiry;
    const optionType = TICKET.optionType;
    const label = `${bought}${sold} ${optionType.toLowerCase()} ${expiry}`;

    const ticket = {
        id: nextComponentId(),
        productType: TICKET.product,
        tradeTypeCode: TICKET.product === 'fx' ? 'FxOption' : '',
        label,
        fields: {
            longShort: TICKET.longShort, optionType, style: 'European',
            settlement: 'Cash', payOffAtExpiry: 'false', expiry,
            boughtCurrency: bought, boughtAmount: amount,
            soldCurrency: sold, soldAmount: String(TICKET.soldAmount).trim(),
            strike: String(TICKET.strike).trim(), notional: amount
        },
        premium: null
    };

    /* Pricing is the first act, so there is nothing to amend: the ticket
     * becomes the whole working copy. Save in the editor writes it. */
    Object.assign(structure, {
        name: label, mode: 'package', strategy: null, authorisation: 'Draft', initialNpv: null,
        groups: [{ id: 'G-1', name: 'Priced legs', components: [ticket] }]
    });
    stage(structure, `Priced from a new ticket: ${label}`);
    ticketDirty = false;
    document.getElementById('ticket-dialog').close();
    lastNpv = null;
    render();
}

/* --- The price ---------------------------------------------------------- */

/* The price is the initial NPV of the structure, with the provenance the editor cannot
 * derive. It is a change to the working copy, and the editor saves it. */
function usePrice() {
    const npv = structureNpv();
    structure.initialNpv = {
        amount: String(Math.round(npv * 100) / 100),
        currency: structure.initialNpv ? structure.initialNpv.currency : 'USD',
        asOf: nowIso(),
        snapshot: market.snapshot,
        model: modelFor(),
        runId: nextRunId(),
        source: 'Pricing screen'
    };
    stage(structure, `Initial NPV set to ${money(npv)} from the pricing screen`);
    window.location.href = 'index.html';
}

function modelFor() {
    if (structure.mode === 'strategy' && structure.strategy === 'risk-reversal') {
        return 'FxVanillaOption / GarmanKohlhagen';
    }
    return 'Composite / sum of parts';
}

let runCounter = 2292;
function nextRunId() {
    return `RUN-${runCounter++}`;
}

/* --- Wiring ------------------------------------------------------------ */

function wire() {
    document.getElementById('reset-tweaks').addEventListener('click', () => {
        Object.assign(market.spots, openingSpots);
        Object.assign(market.vols, openingVols);
        Object.assign(market.rates, openingRates);
        render();
    });

    document.getElementById('ticket-confirm').addEventListener('click', confirmTicket);

    const dialog = $('#ticket-dialog');
    const close = guardDialog(dialog, () => ticketDirty);
    dialog.querySelectorAll('[data-close]').forEach(btn => btn.addEventListener('click', close));
}

function boot() {
    if (!CAN.structure) {
        document.querySelector('.app').replaceChildren(pageHead({ title: 'Price a structure' }),
            needsRole('the pricing screen', 'change structures', STRUCTURE_ROLES));
        return;
    }
    structure = normaliseStructure(loadStructure() || openingStructure());
    LOADED = new Date(NOW.getTime() - 4 * 60000);
    market.snapshot = snapshotId();
    stale = P.get('state') === 'stale';
    wire();
    render();
    if (!stale) setTimeout(() => { stale = true; renderHeader(); }, 8000);
}

boot();
