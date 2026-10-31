/* Operational authorisation queue prototype. Throwaway: mock data, no backend,
 * no build step. Two variants of one screen, switchable with
 * ?variant=rail|tabs. The screen's whole state is printed in the foot bar. */

const params = new URLSearchParams(location.search);

const state = {
    variant: params.get("variant") === "tabs" ? "tabs" : "rail",
    klass: params.get("class") || "all",
    group: params.get("group") || "awaiting",
    selected: params.get("open") || "PKG-8820",
    comment: "",
    holdReason: "",
    attempt: null,
    decided: {},
};

const HOLD_REASONS = [
    ["margin_query", "Query on client margin"],
    ["sales_ticket", "Awaiting sales ticket"],
    ["amendment", "Awaiting an amendment"],
    ["asia_to_authorise", "Handover: Asia to authorise"],
    ["london_to_follow", "Handover: London to follow"],
    ["new_york_order", "Handover: New York order"],
];

const trades = [
    {
        ref: "PKG-8820", type: "Package", klass: "non-flow", state: "unseen",
        counterparty: "Barclays Bank plc", book: "GBP Vanilla Swaps",
        notional: "25,000,000 GBP", tradeDate: "2026-10-31", arrived: "12 min",
        bookedBy: "diane.kay", expiring: false,
        legs: [
            ["SWP-2201", "Interest rate swap, 5Y", "notional 15,000,000"],
            ["SWP-2202", "Interest rate swap, 7Y", "notional 10,000,000"],
        ],
    },
    {
        ref: "OPT-9931", type: "FxDigitalOption", klass: "flow", state: "unseen",
        counterparty: "HSBC Bank plc", book: "Group FX Hedges",
        notional: "4,000,000 EUR", tradeDate: "2026-10-29", arrived: "2 d 4 h",
        bookedBy: "beth.clark", expiring: true, expiry: "3 days",
    },
    {
        ref: "FWD-4412", type: "FxForward", klass: "flow", state: "unseen",
        counterparty: "BNP Paribas SA", book: "Group FX Hedges",
        notional: "1,250,000 USD", tradeDate: "2026-10-31", arrived: "2 h",
        bookedBy: "diane.kay", expiring: false,
    },
    {
        ref: "SWP-2207", type: "Swap", klass: "flow", state: "held_over",
        holdReason: "london_to_follow",
        counterparty: "Deutsche Bank AG", book: "GBP Vanilla Swaps",
        notional: "18,000,000 GBP", tradeDate: "2026-10-30", arrived: "1 d 2 h",
        bookedBy: "adrian.vance", expiring: false,
        holdComment: "London desk to confirm the fixing source before we sign it off.",
    },
    {
        ref: "CDS-7710", type: "CreditDefaultSwap", klass: "non-flow", state: "amendment",
        counterparty: "Societe Generale", book: "IG CDS EMEA",
        notional: "9,500,000 EUR", tradeDate: "2026-10-27", arrived: "4 d 1 h",
        bookedBy: "corey.gross", expiring: false,
        changed: ["notional", "counterparty"],
        previous: { notional: "8,500,000 EUR", counterparty: "Credit Agricole CIB" },
        operationalError: true,
        updated: "2 min ago by corey.gross",
    },
    {
        ref: "CMD-3301", type: "CommoditySwap", klass: "non-flow", state: "held_over",
        holdReason: "margin_query",
        counterparty: "Macquarie Bank Ltd", book: "Group Commodity Derivatives",
        notional: "3,200,000 USD", tradeDate: "2026-10-28", arrived: "3 d 6 h",
        bookedBy: "chun.kit.wong", expiring: false,
        holdComment: "Client margin call is unresolved; do not authorise until treasury answers.",
    },
    {
        ref: "EQP-5566", type: "EquityOption", klass: "non-flow", state: "denied",
        counterparty: "Jefferies LLC", book: "GBP Equity Derivatives",
        notional: "2,000,000 GBP", tradeDate: "2026-10-30", arrived: "1 d",
        bookedBy: "ashley.moore", expiring: false,
        decisionComment: "Wrong counterparty: this is Jefferies International, not Jefferies LLC.",
    },
    {
        ref: "SWP-1180", type: "Swap", klass: "flow", state: "authorised",
        counterparty: "Deutsche Bank AG", book: "GBP Vanilla Swaps",
        notional: "11,000,000 GBP", tradeDate: "2026-10-31", arrived: "5 min",
        bookedBy: "david.reeves", expiring: false,
        decisionComment: "Ticket agrees with the confirmation.",
    },
];

const GROUPS = [
    ["awaiting", "Awaiting authorisation"],
    ["held_over", "Held over"],
    ["amendment", "Amendments"],
    ["authorised", "Authorised"],
    ["denied", "Denied"],
];

const rowState = (t) => state.decided[t.ref] || t.state;

const matchesGroup = (t, group) =>
    group === "all" ? true : group === "awaiting" ? rowState(t) === "unseen" : rowState(t) === group;

const matchesClass = (t, klass) => klass === "all" || t.klass === klass;

const visible = () =>
    trades
        .filter((t) => matchesClass(t, state.klass) && matchesGroup(t, state.group))
        .sort((a, b) => (b.expiring ? 1 : 0) - (a.expiring ? 1 : 0));

const countIn = (klass, group) =>
    trades.filter((t) => matchesClass(t, klass) && matchesGroup(t, group)).length;

const open = () => trades.find((t) => t.ref === state.selected);

/* ------------------------------------------------------------------ parts */

const chipFor = (t) => {
    const s = rowState(t);
    if (s === "unseen") return '<span class="chip info">Awaiting</span>';
    if (s === "held_over") return '<span class="chip warn">Held over</span>';
    if (s === "amendment") return '<span class="chip warn">Amendment</span>';
    if (s === "authorised") return '<span class="chip ok">Authorised</span>';
    if (s === "denied") return '<span class="chip bad">Denied</span>';
    return `<span class="chip">${s}</span>`;
};

const flagsFor = (t) => {
    const out = [];
    if (t.operationalError) out.push('<span class="flag bad">operational error</span>');
    if (t.changed) out.push('<span class="flag">amended</span>');
    if (t.expiring) out.push('<span class="flag bad">expires in ' + t.expiry + "</span>");
    return out.join(" ");
};

const table = () => `
<table>
  <thead>
    <tr>
      <th>Deal</th><th>Type</th><th>Counterparty</th><th>Book</th>
      <th class="num">Notional</th><th>Trade date</th><th class="num">In queue</th>
      <th>State</th>
    </tr>
  </thead>
  <tbody>
    ${visible()
        .map(
            (t) => `
      <tr class="${t.ref === state.selected ? "on" : ""} ${t.expiring ? "expiring" : ""}"
          data-open="${t.ref}">
        <td class="tight"><span class="ref">${t.ref}</span>
          ${t.updated ? '<div class="hint">' + t.updated + "</div>" : ""}</td>
        <td>${t.type}<div class="hint">${t.klass}</div></td>
        <td>${t.counterparty}</td>
        <td class="dim">${t.book}</td>
        <td class="num">${t.notional}</td>
        <td class="tight mono">${t.tradeDate}</td>
        <td class="num dim">${t.arrived}</td>
        <td class="tight">${chipFor(t)} ${flagsFor(t)}</td>
      </tr>`
        )
        .join("")}
  </tbody>
</table>`;

/* The detail pane. The same body serves the rail's fixed pane and the tabs'
 * drawer, so the two variants can only differ in where it sits. */
const detail = () => {
    const t = open();
    if (!t) return '<div class="empty">Choose a deal to work it.</div>';

    const s = rowState(t);
    const needsComment = true; // every decision carries a comment, authorise included
    const comment = state.comment.trim().length > 0;
    const canAuthorise = s !== "denied" && comment;
    const canHold = (s === "unseen" || s === "amendment") && comment && state.holdReason;
    const canDeny = s !== "denied" && s !== "authorised" && comment;
    const canUnAuthorise = s === "authorised" && comment;

    const fact = (label, key, value) =>
        `<dt>${label}</dt><dd class="${t.changed && t.changed.includes(key) ? "changed" : ""}">${value}</dd>`;

    return `
  <div class="top">
    <h2><span class="ref">${t.ref}</span> · ${t.type}</h2>
    ${chipFor(t)}
  </div>

  ${t.expiring ? `<div class="notice bad">This option expires in ${t.expiry} and has not been exercised. It is at the top of the queue until it is decided.</div>` : ""}
  ${t.updated ? `<div class="notice info">A new version arrived ${t.updated}. The row is not locked: you may read it now or come back to it.</div>` : ""}
  ${s === "denied" ? `<div class="notice bad">Authorisation is refused: a denied deal cannot become authorised. Un-authorise it with a reason, then authorise the new version if it is right.</div>` : ""}
  ${state.attempt === "denied-authorise" ? `<div class="notice bad">Exception raised: a denied deal cannot move to authorised.</div>` : ""}
  ${state.attempt === "no-comment" ? `<div class="notice">A comment is required, including on an authorisation.</div>` : ""}
  ${state.attempt === "no-reason" ? `<div class="notice">A hold needs a reason from the list.</div>` : ""}

  <dl class="facts">
    ${fact("Counterparty", "counterparty", t.counterparty)}
    ${fact("Book", "book", t.book)}
    ${fact("Notional", "notional", t.notional)}
    ${fact("Trade date", "tradeDate", t.tradeDate)}
    ${fact("Entered by", "bookedBy", t.bookedBy)}
    ${fact("In queue", "arrived", t.arrived)}
    ${t.changed ? `<dt>Was</dt><dd class="dim">${Object.entries(t.previous).map(([k, v]) => k + ": " + v).join("<br>")}</dd>` : ""}
  </dl>

  ${t.legs ? `
  <div class="field">
    <label>The package holds ${t.legs.length} legs, and authorises as a whole</label>
    <div class="legs">
      ${t.legs.map(([ref, what, detail]) => `<div class="leg"><span class="mono">${ref}</span><span class="dim">${what}</span><span class="faint">${detail}</span></div>`).join("")}
    </div>
  </div>` : ""}

  ${s === "amendment" ? `
  <div class="row">
    <div class="field"><label>Reason code</label>
      <select><option>client_instruction</option><option>booking_correction</option><option>economic_terms</option></select></div>
    <div class="field"><label>Department</label>
      <select><option>Trading</option><option>Sales</option><option>Operations</option></select></div>
  </div>
  <label class="hint"><input type="checkbox" ${t.operationalError ? "checked" : ""}> Operational error: the deal was booked wrongly, not changed for a business reason</label>` : ""}

  ${s === "held_over" ? `<div class="notice">Held over: ${HOLD_REASONS.find(([k]) => k === t.holdReason)?.[1] || t.holdReason}.<br><span class="dim">${t.holdComment}</span></div>` : ""}
  ${s === "denied" || s === "authorised" ? `<div class="notice">${s === "denied" ? "Denied" : "Authorised"}: ${t.decisionComment}</div>` : ""}

  <div class="field">
    <label>Comment, required${s === "authorised" ? " for an un-authorisation too" : ""}</label>
    <textarea id="comment" placeholder="What did you check, and what did you find?">${state.comment}</textarea>
  </div>

  ${s === "unseen" || s === "amendment" ? `
  <div class="field">
    <label>Hold reason, required to hold over</label>
    <select id="hold-reason">
      <option value="">Choose a reason</option>
      ${HOLD_REASONS.map(([k, label]) => `<option value="${k}" ${state.holdReason === k ? "selected" : ""}>${label}</option>`).join("")}
    </select>
  </div>` : ""}

  <div class="actions">
    ${s === "authorised"
            ? `<button class="act primary" data-act="un-authorise" ${canUnAuthorise ? "" : "disabled"}>Un-authorise</button>`
            : `<button class="act primary" data-act="authorise" ${canAuthorise ? "" : "disabled"}>Authorise</button>`}
    ${s === "unseen" || s === "amendment" ? `<button class="act" data-act="hold" ${canHold ? "" : "disabled"}>Hold over</button>` : ""}
    ${s === "denied" || s === "authorised" ? "" : `<button class="act danger" data-act="deny" ${canDeny ? "" : "disabled"}>Deny</button>`}
    <button class="act quiet" data-act="clear">Clear</button>
  </div>

  <p class="hint">A decision writes the comment to the timeline and tells the person who raised the deal. Nothing here locks the row against another operator.</p>
  `;
};

/* ------------------------------------------------------------------ views */

const rail = () => `
  <aside class="rail">
    <h2>Class</h2>
    <button class="node ${state.klass === "all" ? "on" : ""}" data-class="all">
      <span>All deals</span><span class="n">${countIn("all", state.group)}</span></button>
    <button class="node ${state.klass === "flow" ? "on" : ""}" data-class="flow">
      <span>Flow</span><span class="n">${countIn("flow", state.group)}</span></button>
    <button class="node ${state.klass === "non-flow" ? "on" : ""}" data-class="non-flow">
      <span>Non-flow</span><span class="n">${countIn("non-flow", state.group)}</span></button>
    <div class="sep"></div>
    <h2>State</h2>
    ${GROUPS.map(
        ([key, label]) =>
            `<button class="node ${state.group === key ? "on" : ""}" data-group="${key}">
              <span>${label}</span><span class="n">${countIn(state.klass, key)}</span></button>`
    ).join("")}
    <button class="node ${state.group === "all" ? "on" : ""}" data-group="all">
      <span>All trades</span><span class="n">${countIn(state.klass, "all")}</span></button>
  </aside>`;

const expiringBanner = () => {
    const expiring = trades.filter((t) => t.expiring && matchesGroup(t, state.group));
    if (!expiring.length) return "";
    return `<div class="banner"><span class="chip tag-bad">expiring</span>
      ${expiring.length} deal${expiring.length > 1 ? "s" : ""} not exercised and close to expiry: ${expiring
        .map((t) => `<span class="mono">${t.ref}</span> (${t.expiry})`)
        .join(", ")}. Deciding this late is a loss the queue caused.</div>`;
};

const head = (awaiting) => `
  <div class="crumbs"><a href="#">Home</a> › <a href="#">Operations</a> › Operational authorisation queue</div>
  <header class="head">
    <div>
      <h1>Operational authorisation queue</h1>
      <div class="sub">Acme Corporation · ACME Corporation UK plc</div>
    </div>
    <div class="figures">
      <div class="figure"><div class="n">${awaiting}</div><div class="l">awaiting</div></div>
      <div class="figure"><div class="n">${countIn("all", "held_over")}</div><div class="l">held over</div></div>
      <div class="figure"><div class="n">${countIn("all", "amendment")}</div><div class="l">amendments</div></div>
      <div class="figure"><div class="n">${trades.filter((t) => t.expiring).length}</div><div class="l">expiring</div></div>
    </div>
  </header>`;

const variationRail = () => {
    const awaiting = countIn("all", "unseen");
    return `${head(awaiting)}
  ${expiringBanner()}
  <div class="body">
    ${rail()}
    <div class="list"><div class="wrap">${table()}</div></div>
    <aside class="detail"><div class="inner">${detail()}</div></aside>
  </div>`;
};

const variationTabs = () => {
    const awaiting = countIn("all", "unseen");
    const tabs = [["awaiting", "Awaiting"], ...GROUPS.slice(1)];
    return `${head(awaiting)}
  <div class="tabs">
    <div class="seg">
      ${[["all", "All classes"], ["flow", "Flow"], ["non-flow", "Non-flow"]]
        .map(([k, l]) => `<button class="${state.klass === k ? "on" : ""}" data-class="${k}">${l}</button>`)
        .join("")}
    </div>
    <span class="hint">Ordered by recency, with anything near expiry pinned first</span>
  </div>
  <div class="tabbar">
    ${tabs
        .map(
            ([k, l]) =>
                `<button class="${state.group === k ? "on" : ""}" data-group="${k}">${l}<span class="n">${countIn(state.klass, k)}</span></button>`
        )
        .join("")}
    <button class="${state.group === "all" ? "on" : ""}" data-group="all">All trades<span class="n">${countIn(
        state.klass,
        "all"
    )}</span></button>
  </div>
  ${expiringBanner()}
  <div class="body">
    <div class="list"><div class="wrap">${table()}</div></div>
  </div>
  ${open() ? '<div class="drawer-scrim" data-close="1"></div>' : ""}
  ${open() ? `<aside class="drawer"><div class="inner">${detail()}</div></aside>` : ""}`;
};

/* ------------------------------------------------------------------ render */

function render() {
    const app = document.getElementById("app");
    app.innerHTML = `<div class="shell">${state.variant === "rail" ? variationRail() : variationTabs()}</div>`;

    document.getElementById("proto-note").innerHTML =
        `<b>Prototype</b> — mock data, no service. The operational authorisation queue, two ways of dividing it. ` +
        `Variant: <a href="?variant=rail${state.group ? "&group=" + state.group : ""}">rail</a> · ` +
        `<a href="?variant=tabs${state.group ? "&group=" + state.group : ""}">tabs</a>`;

    const t = open();
    document.getElementById("proto-bar").innerHTML =
        `variant=${state.variant} class=${state.klass} group=${state.group} open=${state.selected} ` +
        `comment=${state.comment.length ? state.comment.length + " chars" : "empty"} hold=${state.holdReason || "none"} ` +
        (t ? `row-state=${rowState(t)}` : "");
}

/* ------------------------------------------------------------------ events */

document.addEventListener("click", (event) => {
    const target = event.target.closest("[data-class], [data-group], [data-open], [data-act], [data-close]");
    if (!target) return;

    if (target.dataset.class) state.klass = target.dataset.class;
    if (target.dataset.group) state.group = target.dataset.group;
    if (target.dataset.open) state.selected = target.dataset.open;
    if (target.dataset.close) state.selected = null;

    if (target.dataset.act) {
        const t = open();
        const act = target.dataset.act;
        state.attempt = null;

        if (act === "clear") {
            state.comment = "";
            state.holdReason = "";
        } else if (act === "authorise" || act === "deny" || act === "hold" || act === "un-authorise") {
            if (!state.comment.trim()) state.attempt = "no-comment";
            else if (act === "hold" && !state.holdReason) state.attempt = "no-reason";
            else if (act === "authorise" && rowState(t) === "denied") state.attempt = "denied-authorise";
            else {
                state.decided[t.ref] =
                    act === "authorise" ? "authorised" : act === "deny" ? "denied" : act === "hold" ? "held_over" : "unseen";
                if (act === "hold") t.holdReason = state.holdReason;
                t.decisionComment = state.comment;
                state.comment = "";
                state.attempt = null;
                if (act === "authorise" || act === "deny" || act === "un-authorise") state.selected = null;
            }
        }
    }

    render();
});

document.addEventListener("input", (event) => {
    if (event.target.id === "comment") {
        state.comment = event.target.value;
        // The buttons enable as the comment arrives; no full re-render, so the
        // caret stays where the operator left it.
        const buttons = document.querySelectorAll("[data-act]");
        const t = open();
        const comment = state.comment.trim().length > 0;
        buttons.forEach((b) => {
            const act = b.dataset.act;
            if (!t) return;
            if (act === "authorise") b.disabled = !comment || rowState(t) === "denied";
            if (act === "deny") b.disabled = !comment || rowState(t) === "denied" || rowState(t) === "authorised";
            if (act === "hold") b.disabled = !comment || !state.holdReason;
            if (act === "un-authorise") b.disabled = !comment;
        });
        document.getElementById("proto-bar").innerHTML =
            `variant=${state.variant} class=${state.klass} group=${state.group} open=${state.selected} ` +
            `comment=${state.comment.length} chars hold=${state.holdReason || "none"}`;
    }
    if (event.target.id === "hold-reason") {
        state.holdReason = event.target.value;
        render();
    }
});

render();
