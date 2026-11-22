/*
 * PROTOTYPE. Throwaway. Mock data, no backend, no build step.
 *
 * A bundle publishes its members in the order the dependency closure gives,
 * and the person reads three things before it runs: which optional members are
 * included, which mode the run takes, and what each member would do to its
 * target table. The receipt afterwards is the functions' own counts, because a
 * run writes no row anywhere.
 */

const params = new URLSearchParams(location.search);

const STEPS = ["Contents", "Options", "Preview", "Run", "Receipt"];

const MEMBERS = [
  {
    order: 10,
    dataset: "refdata.conventions",
    type: "conventions",
    subject: "refdata.v1.ops.publish_conventions_from_dq",
    optional: false,
    staged: 10991,
    published: 10991,
  },
  {
    order: 20,
    dataset: "refdata.currency_pair_conventions",
    type: "currency_pair_conventions",
    subject: "refdata.v1.ops.publish_currency_pair_conventions_from_dq",
    optional: false,
    staged: 88,
    published: 84,
  },
  {
    order: 30,
    dataset: "refdata.currency_pair_convention_calendars",
    type: "currency_pair_convention_calendars",
    subject: "refdata.v1.ops.publish_currency_pair_convention_calendars_from_dq",
    optional: true,
    staged: 34,
    published: 0,
  },
  {
    order: 40,
    dataset: "lei.parties",
    type: "parties",
    subject: "refdata.v1.ops.publish_parties_from_dq",
    optional: true,
    staged: 1200,
    published: 0,
    params: "root_lei",
  },
];

const GAPS = params.get("gaps") !== "0";
const step = Math.min(5, Math.max(1, Number(params.get("step") || "1")));
const optedIn = params.get("optional") === "1";
const mode = params.get("mode") || "upsert";
const fails = params.get("fail") === "1";

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

function go(next) {
  const query = new URLSearchParams();
  query.set("step", String(next));
  if (optedIn) query.set("optional", "1");
  if (mode !== "upsert") query.set("mode", mode);
  if (fails) query.set("fail", "1");
  location.search = query.toString();
}

function included(member) {
  return !member.optional || optedIn;
}

function counts(member) {
  if (!included(member)) return { insert: 0, update: 0, skip: 0, reason: "not included" };
  const published = member.published;
  if (mode === "replace_all") return { insert: member.staged, update: 0, skip: 0 };
  if (mode === "insert_only") {
    return { insert: Math.max(0, member.staged - published), update: 0, skip: published };
  }
  return { insert: Math.max(0, member.staged - published), update: Math.min(published, 0), skip: published };
}

function rail() {
  return el(
    "div",
    { class: "rail", role: "list" },
    STEPS.map((label, index) =>
      el(
        "div",
        {
          class: "step",
          role: "listitem",
          "aria-current": index + 1 === step ? "step" : null,
          tabindex: "0",
        },
        [el("span", { class: "n", text: String(index + 1) }), document.createTextNode(label)]
      )
    )
  );
}

function stepContents() {
  const rows = MEMBERS.map((member) =>
    el("tr", {}, [
      el("td", { class: "num", text: String(member.order) }),
      el("td", { class: "mono", text: member.dataset }),
      el("td", { class: "mono", text: member.type }),
      el("td", {}, [el("span", { class: "badge quiet", text: member.optional ? "optional" : "always" })]),
      el("td", { class: "num", text: String(member.staged) }),
      el("td", { class: "num", text: String(member.published) }),
    ])
  );
  return el("section", {}, [
    el("table", {}, [
      el("thead", {}, [
        el("tr", {}, [
          el("th", { text: "Order" }),
          el("th", { text: "Dataset" }),
          el("th", { text: "Artefact type" }),
          el("th", { text: "Included" }),
          el("th", { text: "Staged" }),
          el("th", { text: "Published" }),
        ]),
      ]),
      el("tbody", {}, rows),
    ]),
    el("p", { class: "sub", text: "The order is the dependency closure's. The bundle's display order only breaks ties, so a member that another member needs is published first whatever the list says." }),
  ]);
}

function stepOptions() {
  const modes = [
    ["upsert", "Upsert", "Insert what is missing and update what differs."],
    ["insert_only", "Insert only", "Insert what is missing and leave the rows already there."],
    ["replace_all", "Replace all", "Clear the target table and write the staged rows again."],
  ];
  const checks = MEMBERS.filter((member) => member.optional).map((member) =>
    el("label", { class: "check" }, [
      el("input", { type: "checkbox", checked: optedIn ? "checked" : null, "data-optional": "1" }),
      el("span", {}, [
        el("span", { class: "mono", text: member.dataset }),
        document.createTextNode(member.params ? " · needs a root LEI" : " · publishes no rows by default"),
      ]),
    ])
  );
  return el("section", {}, [
    el("div", { class: "panel" }, [
      el("h3", { text: "The target" }),
      el("div", { class: "body" }, [
        el("dl", { class: "kv" }, [
          el("dt", { text: "Bundle" }),
          el("dd", { text: "party_essentials" }),
          el("dt", { text: "Level" }),
          el("dd", { text: "tenant" }),
          el("dt", { text: "Writes into" }),
          el("dd", { text: "the tenant that asked — the level decides it, the caller does not" }),
        ]),
        el("p", { class: "sub", text: "A platform or world bundle is published only by the system tenant, because a tenant reads those rows where they lie." }),
      ]),
    ]),
    el("div", { class: "panel" }, [
      el("h3", { text: "Mode" }),
      el("div", { class: "body" }, modes.map(([value, label, meaning]) =>
        el("label", { class: "check" }, [
          el("input", { type: "radio", name: "mode", checked: value === mode ? "checked" : null, "data-mode": value }),
          el("span", {}, [el("strong", { text: label }), document.createTextNode(" — " + meaning)]),
        ])
      )),
    ]),
    el("div", { class: "panel" }, [
      el("h3", { text: "Optional members" }),
      el("div", { class: "body" }, checks.concat([
        el("p", { class: "sub", text: "An optional member publishes only when it is asked for, so a large catalogue does not arrive with every small one." }),
      ])),
    ]),
  ]);
}

function stepPreview() {
  const rows = MEMBERS.map((member) => {
    const result = counts(member);
    return el("tr", { class: included(member) ? null : "notready" }, [
      el("td", { class: "mono", text: member.dataset }),
      el("td", { class: "num", text: String(member.staged) }),
      el("td", { class: "num", text: String(result.insert) }),
      el("td", { class: "num", text: String(result.update) }),
      el("td", { class: "num", text: String(result.skip) }),
      el("td", {}, [el("span", { class: `badge ${included(member) ? "quiet" : "error"}`, text: included(member) ? "will run" : "not included" })]),
    ]);
  });
  const total = MEMBERS.reduce(
    (sum, member) => sum + (included(member) ? counts(member).insert : 0),
    0
  );
  return el("section", {}, [
    el("p", { class: "sub", text: `${STEPS[2]} · mode ${mode} · ${total} rows would be inserted` }),
    el("table", {}, [
      el("thead", {}, [
        el("tr", {}, [
          el("th", { text: "Dataset" }),
          el("th", { text: "Staged" }),
          el("th", { text: "Insert" }),
          el("th", { text: "Update" }),
          el("th", { text: "Skip" }),
          el("th", { text: "This run" }),
        ]),
      ]),
      el("tbody", {}, rows),
    ]),
    el("p", { class: "sub", text: "A zero inserted count is the evidence that a catalogue agrees. A zero updated count is not, because a function that rewrites its rows returns updated instead." }),
  ]);
}

function stepRun() {
  const haltAt = fails ? 2 : -1;
  const rows = MEMBERS.filter(included).map((member, index) => {
    const halted = haltAt === index;
    const done = haltAt === -1 || index < haltAt;
    const state = halted ? ["error", "halted"] : done ? ["ok", "done"] : ["quiet", "queued"];
    return el("tr", {}, [
      el("td", { class: "num", text: String(index + 1) }),
      el("td", { class: "mono", text: member.dataset }),
      el("td", {}, [el("span", { class: `badge ${state[0]}`, text: state[1] })]),
      el("td", { class: "mono", text: halted ? "publish function raised: 4 rows unnamed by the artefact type" : done ? "4 counts returned" : "—" }),
    ]);
  });
  const progress = fails ? 40 : 100;
  return el("section", {}, [
    el("dl", { class: "kv" }, [
      el("dt", { text: "Workflow instance" }),
      el("dd", { text: "7f3c-4a91-9d20" }),
      el("dt", { text: "Started by" }),
      el("dd", { text: "m.okafor" }),
    ]),
    el("div", { class: "progress" }, [el("span", { style: `width:${progress}%` })]),
    el("table", {}, [
      el("thead", {}, [el("tr", {}, [el("th", { text: "#" }), el("th", { text: "Dataset" }), el("th", { text: "State" }), el("th", { text: "Answer" })])]),
      el("tbody", {}, rows),
    ]),
    el("p", { class: "sub", text: fails ? "A failed step halts the run and does not undo the steps before it, because each step is its own function call." : "Steps complete in order and each commits on its own. There is no compensation step: the functions are idempotent upserts, so a re-run is safe." }),
  ]);
}

function stepReceipt() {
  const rows = MEMBERS.filter(included).map((member) => {
    const result = counts(member);
    return el("tr", {}, [
      el("td", { class: "mono", text: member.dataset }),
      el("td", { class: "num", text: String(result.insert) }),
      el("td", { class: "num", text: String(result.update) }),
      el("td", { class: "num", text: String(result.skip) }),
      el("td", { class: "num", text: "0" }),
    ]);
  });
  return el("section", {}, [
    el("table", {}, [
      el("thead", {}, [
        el("tr", {}, [
          el("th", { text: "Dataset" }),
          el("th", { text: "Inserted" }),
          el("th", { text: "Updated" }),
          el("th", { text: "Skipped" }),
          el("th", { text: "Deleted" }),
        ]),
      ]),
      el("tbody", {}, rows),
    ]),
    el("div", { class: "note", style: "margin-top:14px" }, [
      el("strong", { text: "The receipt is the whole record. " }),
      document.createTextNode(
        "No history row was written, because nothing on this path writes the run log. What survives the run is the workflow instance and the provenance stamped on each published row."
      ),
    ]),
  ]);
}

function gapsPanel() {
  return el("div", { class: "note gap", style: "margin-top:18px" }, [
    el("strong", { text: "What the server does not serve. " }),
    document.createTextNode(
      "The preview counts here are computed by ores_dq_bundle_preview_fn, and no subject serves it. The receipt is the four counts each publish function returns, which are read from the workflow step result today."
    ),
  ]);
}

const BODY = {
  1: stepContents,
  2: stepOptions,
  3: stepPreview,
  4: stepRun,
  5: stepReceipt,
};

function footer() {
  const previous = el("button", { class: "ghost", text: "Back" });
  previous.disabled = step === 1;
  previous.addEventListener("click", () => go(step - 1));

  const labels = { 1: "Options", 2: "Preview", 3: "Run", 4: "Receipt", 5: "Close" };
  const next = el("button", { class: "primary", text: labels[step] });
  next.disabled = step === 5;
  next.addEventListener("click", () => go(step + 1));

  return el("div", { class: "actions", style: "margin-top:22px" }, [previous, next]);
}

function render() {
  const detail = document.getElementById("detail");
  detail.replaceChildren(rail(), BODY[step](), footer());
  if (GAPS) detail.append(gapsPanel());
  document.getElementById("instance").textContent =
    step >= 4 ? "instance 7f3c-4a91-9d20" : "";

  const bar = document.getElementById("proto-bar");
  bar.replaceChildren(el("span", { class: "label", text: "Steps" }));
  STEPS.forEach((label, index) => {
    bar.append(
      el("a", {
        href: (() => {
          const query = new URLSearchParams();
          query.set("step", String(index + 1));
          if (optedIn) query.set("optional", "1");
          if (mode !== "upsert") query.set("mode", mode);
          if (fails) query.set("fail", "1");
          return "?" + query.toString();
        })(),
        text: label,
        "aria-current": index + 1 === step ? "true" : "false",
      })
    );
  });
  bar.append(el("span", { class: "label", text: "States" }));
  [
    ["optional members in", "?step=2&optional=1"],
    ["replace all", "?step=3&mode=replace_all"],
    ["a step fails", "?step=4&fail=1"],
    ["no gap notes", `?step=${step}&gaps=0`],
  ].forEach(([label, href]) => bar.append(el("a", { href, text: label })));
}

document.addEventListener("change", (event) => {
  const target = event.target;
  if (target.matches("[data-mode]")) {
    const query = new URLSearchParams();
    query.set("step", String(step));
    query.set("mode", target.dataset.mode);
    if (optedIn) query.set("optional", "1");
    location.search = query.toString();
  }
  if (target.matches("[data-optional]")) {
    const query = new URLSearchParams();
    query.set("step", String(step));
    query.set("optional", target.checked ? "1" : "0");
    if (mode !== "upsert") query.set("mode", mode);
    location.search = query.toString();
  }
});

render();
