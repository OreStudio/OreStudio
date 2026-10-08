/* The pricing screen.
 *
 * This is the dynamic half: market data ticking, vols and rates the
 * trader can play with, and a NPV readout. It carries the Book button
 * that hands the economics to the booking screen.
 *
 * The valuation is faked on purpose. The story rules the pricing layer
 * itself out of scope, and the point here is the handoff and the two
 * views, not the number.
 */

const STORAGE_KEY = 'ores.trade-entry.structure';

const market = marketState();
const openingSpots = { ...market.spots };
const openingVols = { ...market.vols };
const openingRates = { ...market.rates };

let structure = null;
let priceMode = 'structure';
let viewMode = 'condensed';
let lastNpv = null;

/* --- Utilities -------------------------------------------------------- */

function esc(value) {
    return String(value === undefined || value === null ? '' : value)
        .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

function load() {
    try {
        const raw = localStorage.getItem(STORAGE_KEY);
        if (raw) return JSON.parse(raw);
    } catch (err) {
        console.warn('prototype: could not read stored structure', err);
    }
    return null;
}

function persist() {
    try {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(structure));
    } catch (err) {
        console.warn('prototype: could not persist structure', err);
    }
}

function money(value) {
    return Number(value).toLocaleString(undefined, { maximumFractionDigits: 0 });
}

function nowIso() {
    return new Date().toISOString().replace(/\.\d+Z$/, 'Z');
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
    document.getElementById('instrument-name').textContent = structure.name;
    document.getElementById('instrument-sub').textContent =
        `${structure.id} · ${allLegs().length} legs · ${structure.mode} mode`;

    document.querySelectorAll('#price-mode button').forEach(btn => {
        btn.classList.toggle('on', btn.dataset.pmode === priceMode);
    });
    document.querySelectorAll('#view-mode button').forEach(btn => {
        btn.classList.toggle('on', btn.dataset.view === viewMode);
    });
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

        if (priceMode === 'leg') {
            rows.push(`<tr><td colspan="3" class="legend">
                Leg mode prices each leg on its own. Switch to the expanded view to see them.
            </td></tr>`);
        }
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
    document.getElementById('mode-note').textContent = priceMode === 'structure'
        ? 'Structure mode: one model for the parts that interact, one price for the deal.'
        : 'Leg mode: what each piece is worth, side by side. This is what the trader hedges against.';

    document.getElementById('valuation-note').textContent =
        `As of ${new Date().toISOString().slice(0, 19).replace('T', ' ')}Z · ${market.snapshot}`;

    lastNpv = npv;
}

/* --- Ticking ----------------------------------------------------------- */

let ticker = null;

function startTicking() {
    if (ticker) return;
    ticker = setInterval(() => {
        Object.keys(market.spots).forEach(pair => {
            const drift = (Math.random() - 0.5) * 0.0016;
            market.spots[pair] = Math.max(0.0001, market.spots[pair] * (1 + drift));
        });
        const dot = document.getElementById('tick-dot');
        dot.classList.remove('tick');
        void dot.offsetWidth;
        dot.classList.add('tick');
        /* A tick must not rebuild the tweak sliders: that would end a drag
         * in progress every 1.2 seconds. */
        renderMarket();
        renderValuation();
    }, 1200);
}

function stopTicking() {
    clearInterval(ticker);
    ticker = null;
    document.getElementById('tick-dot').classList.add('paused');
    document.getElementById('tick-text').textContent = 'Market data paused';
    document.getElementById('tick-toggle').textContent = 'Resume';
}

function resumeTicking() {
    document.getElementById('tick-dot').classList.remove('paused');
    document.getElementById('tick-text').textContent = 'Market data live';
    document.getElementById('tick-toggle').textContent = 'Pause';
    startTicking();
}

/* --- The pricing ticket ------------------------------------------------ */

/* The pricing screen starts a deal from the least reference data it can
 * price from. Every other field the booking needs is the booker's job, and
 * Book carries this much across to them. */
function openTicket() {
    const leg = allLegs()[0] || { fields: {} };
    const f = leg.fields || {};
    const set = (id, value) => { document.getElementById(id).value = value; };

    set('t-product', leg.productType || 'fx');
    set('t-longshort', f.longShort || 'Long');
    set('t-option', f.optionType === 'Put' ? 'Put' : 'Call');
    set('t-bought-ccy', f.boughtCurrency || 'EUR');
    set('t-bought-amt', f.boughtAmount || '1000000');
    set('t-sold-ccy', f.soldCurrency || 'USD');
    set('t-sold-amt', f.soldAmount || '1090000');
    set('t-strike', f.strike || '1.09');
    set('t-expiry', (f.expiry || '2026-03-10').slice(0, 10));

    document.getElementById('ticket-dialog').showModal();
}

function nextComponentId() {
    const numbers = allLegs()
        .map(l => Number(String(l.id).replace(/\D/g, '')))
        .filter(n => !Number.isNaN(n));
    return `T-${numbers.length ? Math.max(...numbers) + 1 : 9001}`;
}

function confirmTicket() {
    const value = id => document.getElementById(id).value.trim();
    const bought = value('t-bought-ccy').toUpperCase();
    const sold = value('t-sold-ccy').toUpperCase();
    const amount = value('t-bought-amt');
    const expiry = value('t-expiry');
    const optionType = value('t-option');
    const label = `${bought}${sold} ${optionType.toLowerCase()} ${expiry}`;

    const ticket = {
        id: nextComponentId(),
        productType: value('t-product'),
        tradeTypeCode: '',
        label,
        fields: {
            longShort: value('t-longshort'), optionType, style: 'European',
            settlement: 'Cash', payOffAtExpiry: 'false', expiry,
            boughtCurrency: bought, boughtAmount: amount,
            soldCurrency: sold, soldAmount: value('t-sold-amt'),
            strike: value('t-strike'), notional: amount
        },
        premium: null
    };

    /* Pricing is the first act, so there is nothing to amend: the ticket
     * becomes the whole structure. The booking screen does the rest. */
    structure = {
        id: structure.id,
        name: label,
        counterparty: structure.counterparty,
        nettingSet: structure.nettingSet,
        book: structure.book,
        portfolio: structure.portfolio,
        tradeDate: structure.tradeDate,
        mode: 'package',
        strategy: null,
        version: structure.version + 1,
        authorisation: 'Draft',
        initialNpv: null,
        audit: structure.audit.concat([
            { at: nowIso(), actor: 'marco', text: `Priced from a new ticket: ${label}` }
        ]),
        groups: [{ id: 'G-1', name: 'Priced legs', components: [ticket] }]
    };

    persist();
    document.getElementById('ticket-dialog').close();
    lastNpv = null;
    render();
}

/* --- The handoff ------------------------------------------------------- */

/* Book carries the economics across, and stamps the initial NPV with the
 * provenance the booking screen cannot derive. */
function book() {
    const npv = structureNpv();
    structure.initialNpv = {
        amount: String(Math.round(npv * 100) / 100),
        currency: structure.initialNpv ? structure.initialNpv.currency : 'USD',
        asOf: nowIso(),
        snapshot: market.snapshot,
        model: modelFor(),
        runId: nextRunId(),
        source: 'Pricing screen handoff'
    };
    structure.version += 1;
    structure.audit.push({
        at: nowIso(),
        actor: 'marco',
        text: `Booked from pricing screen (${money(npv)})`
    });
    persist();
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
    document.getElementById('price-mode').addEventListener('click', event => {
        const btn = event.target.closest('button[data-pmode]');
        if (!btn) return;
        priceMode = btn.dataset.pmode;
        render();
    });

    document.getElementById('view-mode').addEventListener('click', event => {
        const btn = event.target.closest('button[data-view]');
        if (!btn) return;
        viewMode = btn.dataset.view;
        render();
    });

    document.getElementById('tick-toggle').addEventListener('click', () => {
        if (ticker) stopTicking();
        else resumeTicking();
    });

    document.getElementById('reset-tweaks').addEventListener('click', () => {
        Object.assign(market.spots, openingSpots);
        Object.assign(market.vols, openingVols);
        Object.assign(market.rates, openingRates);
        render();
    });

    document.getElementById('reprice').addEventListener('click', () => {
        lastNpv = null;
        render();
    });

    document.getElementById('new-trade').addEventListener('click', openTicket);
    document.getElementById('ticket-confirm').addEventListener('click', confirmTicket);

    document.querySelectorAll('dialog [data-close]').forEach(btn => {
        btn.addEventListener('click', () => btn.closest('dialog').close());
    });

    document.getElementById('book').addEventListener('click', book);
}

function fillProductTypes() {
    document.getElementById('t-product').innerHTML = PRODUCT_TYPES
        .map(p => `<option value="${esc(p.code)}">${esc(p.name)}</option>`).join('');
}

function boot() {
    structure = load() || openingStructure();
    market.snapshot = `MDS-${new Date().toISOString().slice(0, 10)}-${new Date().toISOString().slice(11, 16).replace(':', '')}`;
    fillProductTypes();
    wire();
    render();
    startTicking();
}

boot();
