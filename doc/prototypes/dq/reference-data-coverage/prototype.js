/*
 * PROTOTYPE. Throwaway. Mock data, no backend, no build step.
 *
 * The page answers one question: does this installation hold the reference
 * data it needs? The roster groups the bundles by the level they publish at,
 * because the level decides the tenant the rows land in and whether the
 * tenant owes a copy of its own. The member table then says, for one bundle,
 * how much of it is staged, how much is published, and — where it is not
 * ready — which of four reasons applies, because each reason has a different
 * person to fix it.
 */

const params = new URLSearchParams(location.search);

const LEVELS = ["Platform", "World", "Tenant"];

const BUNDLES = [
  {
    level: "Platform",
    code: "system_core",
    name: "System core",
    datasets: 12,
    note: "published at bring-up",
    members: [],
  },
  {
    level: "World",
    code: "world",
    name: "World",
    datasets: 41,
    note: "published at bring-up",
    members: [
      { dataset: "refdata.countries", type: "countries", subject: "refdata.v1.ops.publish_countries_from_dq", staged: 249, published: 249 },
      { dataset: "refdata.currencies", type: "currencies", subject: "refdata.v1.ops.publish_currencies_from_dq", staged: 179, published: 179 },
      { dataset: "refdata.calendars", type: "calendars", subject: "refdata.v1.ops.publish_calendars_from_dq", staged: 35, published: 33, optional: true },
    ],
  },
  {
    level: "World",
    code: "crypto",
    name: "Crypto",
    datasets: 9,
    note: "not published here",
    members: [],
  },
  {
    level: "Tenant",
    code: "tenant_defaults",
    name: "Tenant defaults",
    datasets: 3,
    note: "published when the tenant was created",
    members: [],
  },
  {
    level: "Tenant",
    code: "party_essentials",
    name: "Party essentials",
    datasets: 2,
    note: "1 of 2 ready",
    members: [
      { dataset: "refdata.conventions", type: "conventions", subject: "refdata.v1.ops.publish_conventions_from_dq", staged: 10991, published: 10991 },
      { dataset: "refdata.currency_pair_convention_calendars", type: "currency_pair_convention_calendars", subject: "refdata.v1.ops.publish_currency_pair_convention_calendars_from_dq", staged: 0, published: 0, reason: "no staged rows" },
    ],
  },
  {
    level: "Tenant",
    code: "risk_management",
    name: "Risk management",
    datasets: 4,
    note: "1 member names no subject",
    members: [
      { dataset: "testdata.business_units", type: "business_units", subject: "refdata.v1.ops.publish_business_units_from_dq", staged: 12, published: 12 },
      { dataset: "testdata.portfolios", type: "portfolios", subject: "refdata.v1.ops.publish_portfolios_from_dq", staged: 4, published: 0 },
      { dataset: "testdata.books", type: "books", subject: null, staged: 88, published: 0, reason: "no target subject" },
      { dataset: "ore.report_definitions", type: "report_definitions", subject: "reporting.v1.ops.publish_report_definitions_from_dq", staged: 28, published: 28 },
    ],
  },
];

const REASONS = [
  ["no dataset", "The bundle lists a member that no dataset row names.", "DQ"],
  ["no artefact type", "The dataset names an artefact type that does not exist.", "DQ"],
  ["no target subject", "The artefact type names no target subject, so no publish can be dispatched.", "DQ"],
  ["no staged rows", "The artefact table holds nothing, so there is nothing to publish.", "the importer"],
];

const empty = params.get("empty") === "1";
const showGaps = params.get("gaps") !== "0";
const selected = params.get("bundle") || (empty ? "world" : "party_essentials");

function el(tag, attrs, children) {
  const node = document.createElement(tag);
  Object.entries(attrs || {}).forEach(([key, value]) => {
    if (value === null || value === undefined || value === false) return;
    if (key === "class") node.className = value;
    else if (key === "text") node.textContent = value;
    else node.setAttribute(key, value);
  });
  (children || []).forEach((child) => node.append(child));
  return node;
}

function bundleState(bundle) {
  if (empty) return "none";
  if (bundle.members.some((member) => member.reason)) return "error";
  if (bundle.note.includes("not published")) return "none";
  if (bundle.members.length === 0) return "ok";
  const ready = bundle.members.filter((member) => !member.reason).length;
  return ready === bundle.members.length ? "ok" : "part";
}

const STATE_TEXT = {
  ok: "published",
  part: "part published",
  none: "not published",
  error: "not ready",
};

const STATE_DOT = { ok: "●", part: "◐", none: "○", error: "●" };

function publishedCount(member) {
  return empty ? 0 : member.published;
}

function memberState(member) {
  if (member.reason) return ["error", member.reason];
  const published = publishedCount(member);
  if (published === 0 && member.staged > 0) return ["none", "nothing published"];
  if (published < member.staged) return ["warn", `${member.staged - published} skipped`];
  return ["ok", "ok"];
}

function renderHeader() {
  document.getElementById("tenant").textContent = "ACME · tenant acme";
  document.getElementById("asof").textContent = empty
    ? "fresh database · nothing published"
    : "read 2026-11-22 20:10";
}

function renderRoster() {
  const roster = document.getElementById("roster");
  roster.replaceChildren();
  LEVELS.forEach((level) => {
    roster.append(el("h2", { text: level }));
    BUNDLES.filter((bundle) => bundle.level === level).forEach((bundle) => {
      const state = bundleState(bundle);
      const row = el(
        "div",
        {
          class: "bundle",
          role: "link",
          tabindex: "0",
          "aria-selected": bundle.code === selected ? "true" : "false",
          "data-code": bundle.code,
        },
        [
          el("span", { class: `dot ${state}`, text: STATE_DOT[state] }),
          el("span", {}, [
            el("div", { class: "name", text: bundle.code }),
            el("div", { class: "meta", text: `${bundle.datasets} datasets · ${empty ? "nothing published" : bundle.note}` }),
          ]),
          el("span", { class: `badge ${state === "ok" ? "ok" : state === "error" ? "error" : state === "part" ? "warn" : "quiet"}`, text: STATE_TEXT[state] }),
        ]
      );
      row.addEventListener("click", () => {
        const next = new URLSearchParams(location.search);
        next.set("bundle", bundle.code);
        next.delete("preview");
        next.delete("publish");
        next.delete("history");
        location.search = next.toString();
      });
      roster.append(row);
    });
  });
}

function memberRow(member) {
  const [state, text] = memberState(member);
  const badgeClass = state === "ok" ? "ok" : state === "error" ? "error" : state === "warn" ? "warn" : "quiet";
  return el("tr", { class: member.reason ? "notready" : null }, [
    el("td", { class: "mono", text: member.dataset }),
    el("td", { class: "mono", text: member.type }),
    el("td", { class: "mono", text: member.subject || "—" }),
    el("td", { class: "num", text: String(member.staged) }),
    el("td", { class: "num", text: String(publishedCount(member)) }),
    el("td", {}, [el("span", { class: `badge ${badgeClass}`, text })]),
  ]);
}

function membersPanel(bundle) {
  const rows = bundle.members.map(memberRow);
  const panel = el("div", {}, [
    el("table", {}, [
      el("thead", {}, [
        el("tr", {}, [
          el("th", { text: "Dataset" }),
          el("th", { text: "Artefact type" }),
          el("th", { text: "Target subject" }),
          el("th", { text: "Staged" }),
          el("th", { text: "Published" }),
          el("th", { text: "State" }),
        ]),
      ]),
      el("tbody", {}, rows),
    ]),
    el("p", {
      class: "sub",
      text:
        "Staged counts the artefact table. Published counts the rows whose provenance names this dataset, which is the only record a run leaves.",
    }),
  ]);
  if (bundle.members.length === 0) {
    panel.replaceChildren(
      el("p", { class: "sub", text: "Mock members are not drawn for this bundle. The roster row still states its count." })
    );
  }
  return panel;
}

function reasonsPanel() {
  const items = REASONS.map(([reason, meaning, owner]) =>
    el("tr", {}, [
      el("td", { class: "mono", text: reason }),
      el("td", { text: meaning }),
      el("td", { text: owner }),
    ])
  );
  return el("section", { class: "panel" }, [
    el("h3", { text: "Why a member is not ready" }),
    el("div", { class: "body" }, [
      el("p", { text: "One reason per member, because each one has a different person to fix it." }),
      el("table", {}, [
        el("thead", {}, [el("tr", {}, [el("th", { text: "Reason" }), el("th", { text: "What it means" }), el("th", { text: "Whose to fix" })])]),
        el("tbody", {}, items),
      ]),
    ]),
  ]);
}

function previewPanel(bundle) {
  const rows = bundle.members.map((member) => {
    const published = publishedCount(member);
    const insert = member.reason ? 0 : Math.max(0, member.staged - published);
    const update = member.reason ? 0 : Math.min(published, 0);
    const skip = member.reason ? 0 : published;
    return el("tr", {}, [
      el("td", { class: "mono", text: member.dataset }),
      el("td", { class: "num", text: String(member.staged) }),
      el("td", { class: "num", text: String(insert) }),
      el("td", { class: "num", text: String(update) }),
      el("td", { class: "num", text: String(skip) }),
    ]);
  });
  return el("section", { class: "panel" }, [
    el("h3", { text: "Preview: what a publish would write" }),
    el("div", { class: "body" }, [
      el("p", { text: "Read before the run, not after it. A member that cannot be published is listed with zero counts rather than left out." }),
      el("table", {}, [
        el("thead", {}, [
          el("tr", {}, [
            el("th", { text: "Dataset" }),
            el("th", { text: "Staged" }),
            el("th", { text: "Insert" }),
            el("th", { text: "Update" }),
            el("th", { text: "Skip" }),
          ]),
        ]),
        el("tbody", {}, rows),
      ]),
    ]),
  ]);
}

function receiptPanel(bundle) {
  const ready = bundle.members.filter((member) => !member.reason);
  return el("section", { class: "panel" }, [
    el("h3", { text: "Run: the steps as they complete" }),
    el("div", { class: "body" }, [
      el("p", { text: `Workflow instance 7f3c…a91 · bundle ${bundle.code} · mode upsert` }),
      el("table", {}, [
        el("thead", {}, [el("tr", {}, [el("th", { text: "#" }), el("th", { text: "Dataset" }), el("th", { text: "Result" })])]),
        el("tbody", {}, ready.map((member, index) =>
          el("tr", {}, [
            el("td", { text: String(index + 1) }),
            el("td", { class: "mono", text: member.dataset }),
            el("td", {}, [el("span", { class: "badge ok", text: "done · 4 counts returned" })]),
          ])
        )),
      ]),
      el("p", { class: "sub", text: "A run leaves no audit row. The counts above are the function's answer, and the provenance on each row is the record." }),
    ]),
  ]);
}

function historyPanel() {
  return el("section", { class: "panel" }, [
    el("h3", { text: "History" }),
    el("div", { class: "body" }, [
      el("div", { class: "note gap" }, [
        el("strong", { text: "This panel is empty by design today. " }),
        document.createTextNode(
          "The run-log table exists and nothing on the publish path writes it, so a history screen would show nothing however well it was built. What a run leaves is the workflow instance and the provenance on each row."
        ),
      ]),
    ]),
  ]);
}

function gapsPanel() {
  return el("section", { class: "panel" }, [
    el("h3", { text: "What this screen needs and the server does not serve" }),
    el("div", { class: "body" }, [
      el("div", { class: "note gap" }, [
        el("strong", { text: "Three answers are computed and served to no one. " }),
        document.createTextNode(
          "ores_dq_bundles_list_fn, ores_dq_bundle_datasets_list_fn and ores_dq_bundle_preview_fn exist in the database, and the publish path calls the second and third for its own purposes. The screen needs one coverage operation that answers all three."
        ),
      ]),
      el("div", { class: "note gap", style: "margin-top:10px" }, [
        el("strong", { text: "The published count has no operation. " }),
        document.createTextNode(
          "Every published row names its dataset, and no service counts by it, so the middle column is drawn from mock data in this prototype."
        ),
      ]),
    ]),
  ]);
}

function renderDetail() {
  const bundle = BUNDLES.find((item) => item.code === selected) || BUNDLES[4];
  const detail = document.getElementById("detail");
  const state = bundleState(bundle);
  const target = bundle.level === "Tenant" ? "the tenant that asked" : "the system tenant";

  const actions = el("div", { class: "actions" }, [
    el("button", { class: "primary", text: "Preview what it would write" }),
    el("button", { text: "Publish this bundle…" }),
    el("button", { class: "ghost", text: "History" }),
  ]);
  const buttons = actions.querySelectorAll("button");
  buttons[0].addEventListener("click", () => toggle("preview"));
  buttons[1].addEventListener("click", () => toggle("publish"));
  buttons[2].addEventListener("click", () => toggle("history"));

  detail.replaceChildren(
    el("h2", { text: bundle.name }),
    el("p", {
      class: "sub",
      text: `${bundle.level} level · writes into ${target} · ${bundle.datasets} datasets · ${empty ? "nothing published" : bundle.note}`,
    }),
    actions
  );

  if (empty) {
    detail.append(
      el("div", { class: "note", style: "margin-bottom:16px" }, [
        el("strong", { text: "A fresh database looks like this. " }),
        document.createTextNode(
          "Nothing has been published into the installation yet, and a recreate that fails to publish leaves exactly this screen — which the Qt client could not show."
        ),
      ])
    );
  }

  detail.append(membersPanel(bundle));
  detail.append(reasonsPanel());
  if (params.get("preview") === "1") detail.append(previewPanel(bundle));
  if (params.get("publish") === "1") detail.append(receiptPanel(bundle));
  if (params.get("history") === "1") detail.append(historyPanel());
  if (showGaps) detail.append(gapsPanel());
}

function toggle(name) {
  const next = new URLSearchParams(location.search);
  if (next.get(name) === "1") next.delete(name);
  else next.set(name, "1");
  location.search = next.toString();
}

function renderBar() {
  const bar = document.getElementById("proto-bar");
  bar.replaceChildren(el("span", { class: "label", text: "Bundles" }));
  BUNDLES.forEach((bundle) => {
    const current = bundle.code === selected;
    bar.append(
      el("a", {
        href: `?bundle=${bundle.code}`,
        text: bundle.code,
        "aria-current": current ? "true" : "false",
      })
    );
  });
  bar.append(el("span", { class: "label", text: "States" }));
  [
    ["fresh database", "?bundle=world&empty=1"],
    ["preview", `?bundle=${selected}&preview=1`],
    ["run", `?bundle=${selected}&publish=1`],
    ["history", `?bundle=${selected}&history=1`],
    ["no gap notes", `?bundle=${selected}&gaps=0`],
  ].forEach(([label, href]) => bar.append(el("a", { href, text: label })));
}

renderHeader();
renderRoster();
renderDetail();
renderBar();
