# The component specification

How a component is represented in TypeScript, so that migrating the screens over
is a mechanical process rather than a series of decisions.

A **component** here means an ORE Studio component in the C++ sense: `ores.iam`,
`ores.refdata`, `ores.trading`. It owns a set of entities, a set of operations,
and its own place in the navigation. This document specifies the seat a component
sits in, so that adding one is filling in a declaration rather than building a
structure.

Read `entity-specification.md` for what an entity screen does. This document is
about where those screens live and how a component is wired.

---

## 1. What the Qt client does, and what to keep

Worth stating exactly, because it is not quite "one menu per component" and the
difference matters for what we build.

The Qt menu bar is assembled in two phases.

**Shared menus are pre-created by the main window, and components fill them.**
The main window owns the tree and its order. It creates handles — File,
Reference Data, Market Data, Reporting with its three configuration submenus,
Operations with its five, Data Quality with Coding Schemes, and so on — and hands
them to every component. A component calls `addAction` on the handle it was given
and never decides where that handle sits.

**Standalone menus are returned by components and inserted by the main window.**
A component can also hand back its own top-level menu, which the main window
inserts before the Window menu, in plugin load order.

The result, for the twelve components:

| Order | Component | Where its actions appear |
|---|---|---|
| 50 | iam | Fill: File ▸ System, Operations ▸ User Accounts |
| 100 | refdata | Standalone: **Reference Data** |
| 200 | trading | Standalone: **Trading** |
| 210 | workspace | Nothing |
| 300 | mktdata | Standalone: **Market Data** |
| 305 | marketdata | Fill: Market Data |
| 310 | synthetic | Fill: Market Data |
| 350 | analytics | Standalone: **Reporting**, fills its submenus |
| 355 | compute | Nothing |
| 360 | scheduler | Fill: Operations ▸ Scheduler |
| 365 | workflow | Fill: Operations ▸ Workflows |
| 380 | dq | Standalone: **Data Quality**, fills Coding Schemes |

So there are two shapes, and the accounts screens are under the second one: IAM,
the component that owns accounts, has no top-level menu of its own. Its actions
appear under Operations ▸ User Accounts.

### What to keep from this

**The navigation tree is owned centrally, and components fill seats in it.**
That is the valuable idea: it means one place decides where "Accounts" lives, and
a component cannot make the structure inconsistent by choosing for itself.

**A component contributes actions in groups, not one at a time.** A list window, a
related view, a wizard and a report are all just entries, and grouping them is
what makes the navigation readable.

**Some actions need no session and some do.** The Qt client enables whole menus on
login, and keeps a few entries reachable before it. The same distinction exists
here and is worth making explicit per entry rather than per menu.

### What not to keep

**A menu bar.** Twenty top-level menus with nested submenus is a desktop idiom. On
the web it hides every destination behind a click, and the ones people use daily
are as far away as the ones they use yearly.

**Alphabetising a menu at startup.** The Qt client has to sort Data Quality's
entries after the fact because several components contribute to it in load order.
Ordering belongs in the declaration, not in a repair pass.

**Bold-accelerator mnemonics.** `&System`, `&User Accounts`. There is no
equivalent and no need.

---

## 2. What replaces it

### The navigation model

A persistent sidebar, grouped by component, with the current component's entities
listed beneath it.

```
┌────────────────────┬──────────────────────────────────────────────┐
│ ORE Studio         │                                              │
│                    │                                              │
│ IAM             ▾  │   Accounts                                   │
│   Accounts         │   Identities that can sign in or act as a     │
│   Roles            │   service, scoped to this tenant.            │
│   Tenants          │                                              │
│   Tenant types     │   [ Search… ]  [ Type ▾ ]        26 of 380    │
│                    │   ------------------------------------------  │
│ Reference Data  ▸  │    Username      Type    Email              │
│ Trading         ▸  │    ores_web_probe   user    …                   │
│ Market Data     ▸  │                                              │
│ Reporting       ▸  │   ------------------------------------------  │
│ Data Quality    ▸  │   Page 1 of 4              Page size [100]   │
│ Operations      ▸  │                                              │
│                    │                                              │
│ ─────────────────  │                                              │
│ System Settings    │                                              │
│ My Account         │                                              │
└────────────────────┴──────────────────────────────────────────────┘
```

The sidebar shows every component. The one you are in is expanded, showing its
entities. The rest are collapsed, one click away. This is the web equivalent of
the Qt menu bar: the same grouping, the same central ownership, but the
destinations are visible and the current one is shown by position rather than
implied.

### Why not a menu bar

Three reasons, in order of weight.

The destinations become invisible. A person who uses Accounts daily and Tenants
monthly sees both equally, or neither.

A component's entries are a list, not a menu. "Accounts, Roles, Tenants, Tenant
types, Org chart, Onboard tenant" is seven entries that should be readable at
once; a menu shows them one hover at a time.

The current location needs to be visible. A web application has a URL and a
person expects to know where they are from looking at the page, not from
remembering which menu they opened.

### Where the Qt grouping goes

Every Qt seat has a home. Nothing is lost.

| Qt seat | Here |
|---|---|
| Standalone Reference Data | Sidebar group **Reference Data** |
| Standalone Trading | Sidebar group **Trading** |
| Standalone Market Data | Sidebar group **Market Data** |
| Standalone Reporting | Sidebar group **Reporting** |
| Standalone Data Quality | Sidebar group **Data Quality** |
| Operations ▸ User Accounts | Under **IAM**, its own component |
| Operations ▸ Scheduler | Under **Scheduler** |
| Operations ▸ Workflows | Under **Workflows** |
| File ▸ System | A **System** group in the footer of the sidebar |
| File ▸ Current User | **My Account** in the footer |

The one deliberate change is IAM. In Qt, the component that owns accounts has no
menu and its screens are filed under Operations ▸ User Accounts, which is a place
a person would not look for them. Here IAM is a group like any other, and its
entities are listed under it. That is the "each component has its own top-level
menu" the Qt client intended but did not do for IAM.

---

## 3. The component declaration

A component is one declaration. Everything else is derived from it.

```ts
// packages/web/src/components/iam/component.ts

import { PersonAccounts, BuildingSkyscraper, ShieldKeyhole } from '../../ui/icons.js';

export const iamComponent: ComponentDefinition = {
  id: 'iam',
  title: 'IAM',
  description: 'Identities, roles and tenancy.',
  icon: PersonAccounts,

  entities: [
    {
      id: 'account',
      title: 'Accounts',
      collection: 'accounts',
      singular: 'account',
      path: 'accounts',
      icon: PersonAccounts,
      description: 'Identities that can sign in or act as a service.',
      // Drives which list features appear; see the entity specification.
      features: { search: ['username', 'fullName', 'email'], filter: 'accountType' },
    },
    {
      id: 'role',
      title: 'Roles',
      collection: 'roles',
      singular: 'role',
      path: 'roles',
      icon: ShieldKeyhole,
      description: 'Permission sets granted to accounts.',
      features: { search: ['name'] },
    },
    {
      id: 'tenant',
      title: 'Tenants',
      collection: 'tenants',
      singular: 'tenant',
      path: 'tenants',
      icon: BuildingSkyscraper,
      description: 'Isolated organisations, and the system tenant.',
      features: { search: ['name', 'code'] },
    },
  ],

  operations: [
    {
      id: 'org-chart',
      title: 'Org chart',
      icon: Organisation,
      kind: 'view',
      path: 'accounts/org-chart',
      requiresSession: true,
    },
    {
      id: 'onboard-tenant',
      title: 'Onboard tenant',
      icon: Wand,
      kind: 'wizard',
      path: 'tenants/onboard',
      requiresSession: true,
    },
  ],

  // Entries this component contributes outside its own group. Most components
  // have none; IAM has System Settings.
  systemEntries: [
    {
      id: 'system-settings',
      title: 'System settings',
      icon: Settings,
      kind: 'view',
      path: 'settings',
      requiresSession: true,
    },
  ],
};
```

### The type

```ts
export interface EntityDefinition {
  /** Stable key. Used in URLs, in query keys, and in the registry. */
  readonly id: string;
  /** What the sidebar says. Plural, sentence case. */
  readonly title: string;
  /** The collection name used in code and in routes. */
  readonly collection: string;
  /** The singular, for messages: "Delete account?". */
  readonly singular: string;
  /** The route segment, usually the collection. */
  readonly path: string;
  readonly icon: IconName;
  readonly description: string;
  /** Which list features this entity has, and over which fields. */
  readonly features: {
    /** Fields the search box matches. Omit for no search box. */
    readonly search?: readonly string[];
    /** The field the type filter groups on. Omit for no filter. */
    readonly filter?: string;
  };
  /** Entities that are not editable, or not deletable, or have no history. */
  readonly capabilities?: readonly ('create' | 'edit' | 'delete' | 'history')[];
}

export interface OperationDefinition {
  readonly id: string;
  readonly title: string;
  readonly icon: IconName;
  /** What opening it does, which decides how it is routed and hosted. */
  readonly kind: 'view' | 'wizard' | 'report' | 'dialog';
  readonly path: string;
  readonly description?: string;
  /**
   * Whether it needs a signed-in session. Shown disabled, or hidden, when
   * there is none; the entity specification says which.
   */
  readonly requiresSession?: boolean;
}

export interface ComponentDefinition {
  readonly id: string;
  readonly title: string;
  readonly description?: string;
  readonly icon: IconName;
  readonly entities: readonly EntityDefinition[];
  readonly operations?: readonly OperationDefinition[];
  /** Entries this component contributes to the System group. */
  readonly systemEntries?: readonly OperationDefinition[];
}
```

### The registry

One list, in the order the sidebar shows them. This is the equivalent of
`load_order`, except the order is the declaration and nothing sorts it afterwards.

```ts
// packages/web/src/components/registry.ts

export const COMPONENTS: readonly ComponentDefinition[] = [
  iamComponent,
  refdataComponent,
  tradingComponent,
  marketDataComponent,
  reportingComponent,
  dataQualityComponent,
  schedulerComponent,
  workflowComponent,
];

export function findEntity(componentId: string, entityId: string): EntityDefinition | undefined;
export function findComponent(id: string): ComponentDefinition | undefined;
export function allEntities(): readonly { component: ComponentDefinition; entity: EntityDefinition }[];
```

---

## 4. Routes and the URL

Routes are generated from the registry, not written by hand. A component that
adds an entity gets its routes for nothing.

| Route | Screen |
|---|---|
| `/<component>/<entity>` | The list screen |
| `/<component>/<entity>/new` | The detail screen, creating |
| `/<component>/<entity>/:id` | The detail screen, reading |
| `/<component>/<entity>/:id/edit` | The detail screen, editing |
| `/<component>/<entity>/:id/history` | The history screen |
| `/<component>/<operation path>` | An operation |

The component is in the URL, not just the entity. Two components may legitimately
have an entity with the same path, and a URL that does not say which component a
record belongs to is a URL that cannot be pasted into a chat and understood.

`/iam/accounts/b2cf5886-…/history` is self-describing. `/accounts/…` is not.

### The screen shell

One shell renders every list, detail and history screen. It reads the route,
finds the entity in the registry, and hands the entity's declaration to the
shared components.

```tsx
<Route path="/:componentId/:entityPath/*" element={<EntityRoutes />} />
```

`EntityRoutes` resolves `componentId` and `entityPath` against the registry and
renders the right screen. An unknown pair renders a not-found screen rather than a
blank page.

---

## 5. What a component supplies, and what is shared

This is the heart of it. The point of the registry is that the expensive parts are
written once.

### Shared, written once

| Concern | What it is |
|---|---|
| **The sidebar** | Renders the registry. Never per component. |
| **The route table** | Generated from the registry. |
| **`EntityListPage`** | Toolbar, table, search, filter, paging, states, recency |
| **`EntityDetailPage`** | Tabs, form, validation, audit prompt, actions |
| **`EntityHistoryPage`** | Timeline and diff |
| **`DataTable`** | Column declaration, rendering, sorting, selection, keyboard |
| **`FieldControl`** | The nine control types, from the field declaration |
| **`ChangeReasonDialog`** | The audit prompt, with the diff-driven reason filter |
| **`DeleteDialog`** | Confirmation |
| **`LookupSelect`** | A foreign-key select, fetching its collection |
| **The query hooks** | Generated per entity from its operation shapes |
| **The API client** | Transport, errors, session |
| **`useEntity`**, **`useEntityList`**, **`useSaveEntity`** | The generic hooks the pages call |

### Per entity, written by hand

| Concern | What it is |
|---|---|
| **The declaration** | The `EntityDefinition` above |
| **The columns** | Which fields, in what order, with what styles |
| **The fields** | Which controls, grouped into tabs |
| **The form schema** | Validation rules, which are the entity's own |
| **Nothing else** | Every other screen is the shared one |

The test of whether the design is right: **adding an entity should touch one new
file** — its declaration and its column and field lists — **plus the protocol
schemas that codegen is producing.** If it touches a shared component, the shared
component is missing an abstraction.

---

## 6. File layout

By component, then by entity. A person looking for the accounts screen looks in
one place.

```
packages/web/src/
  components/
    registry.ts                 the list, in sidebar order
    iam/
      component.ts              the declaration
      entities/
        account/
          columns.ts            the column declaration
          fields.ts             the field declaration
          form.ts               the validation schema
        role/
        tenant/
    refdata/
      component.ts
      entities/
        ...
  entity/                       the shared machinery
    EntityListPage.tsx
    EntityDetailPage.tsx
    EntityHistoryPage.tsx
    EntityRoutes.tsx
    DataTable.tsx
    FieldControl.tsx
    ChangeReasonDialog.tsx
  ui/                           the primitives
    icons.ts
    Primitives.tsx
```

The `components/` tree holds declarations and entity-specific data. The `entity/`
directory holds the machinery and never mentions an entity by name.

### Where the generated declarations live

Both the protocol schemas and the UI metadata are generated from the same org
models, so the columns and fields cannot drift from the types they describe.

```
packages/protocol/src/generated/
  iam/
    protocol/account_protocol.ts   operations, request and reply shapes
    ui/account_ui.ts               columns and fields
```

The entity declaration refers to those by name. It does not declare them, and
neither file is edited by hand. What remains hand-written per entity is the field
grouping and the entity's description; see `codegen-ts-ui-request.md`.

---

## 7. What a component declaration derives

Writing the declaration once produces:

- Sidebar entries, grouped and in order.
- Routes for every entity and operation.
- Query keys, namespaced by component so two components cannot collide.
- Navigation between a list and a detail screen, including the back link.
- Breadcrumbs: component, then entity, then record.
- The document title.
- Permissions filtering, once permissions are known.
- Which list features appear, from `features`.
- Which actions appear on a detail screen, from `capabilities`.

That list is the argument for the registry. Each of those is a thing that would
otherwise be written per component, and each is a thing that would then diverge.

---

## 8. Adding a component

1. Add its protocol schemas, generated from the C++ model.
2. Create `components/<id>/component.ts` with its entities.
3. For each entity, add `columns.ts`, `fields.ts` and `form.ts`.
4. Add the component to `COMPONENTS`, in the position the sidebar should show it.
5. Add its BFF routes and protocol operations.
6. Verify by driving the real screens: list, search, filter, open, create, edit,
   delete, history.

No shared component is edited. If one needs editing, that is the bug, and it is
worth fixing before adding the second entity rather than after the twentieth.

---

## 9. Decisions this document makes

Recorded so they can be argued with rather than rediscovered.

| Decision | Why |
|---|---|
| A sidebar, not a menu bar | Twenty top-level menus hide every destination, and a component's entries are a list to be read, not a menu to be opened |
| Component in the URL | Two components may share an entity path, and a URL should say which one it means |
| One declaration per component | The sidebar, routes, query keys and breadcrumbs all derive from it, so they cannot diverge |
| IAM gets its own group | In Qt it had no menu and its screens were filed under Operations, which is not where anyone would look |
| System and My Account in the sidebar footer | They belong to no component and are reached from anywhere |
| Order from the declaration, not from sorting | The Qt client has to alphabetise Data Quality after the fact because order was implicit |
| Routes generated, never hand-written | A hand-written route is one that will be forgotten when an entity is added |

---

## 10. Open questions

Named rather than assumed, because they affect the shape.

**Does anything still need a horizontal menu?** The Qt client has File and Window menus with actions that belong to the application rather than a component: connect, disconnect, switch party, exit. On the web some of those become settings and some become nothing, because a browser has its own window management. Worth deciding explicitly rather than by omission.

**Is the sidebar always visible, or collapsible?** With eight components and, eventually, hundreds of entities, the sidebar cannot show everything at once. Whether it scrolls, collapses groups, or is filtered by a search box is a design question this document does not settle.

**Where do reports go?** Reporting is a component in Qt, and reports are themselves something a person might run, schedule and read. They may want a different treatment from an entity list.

**How are permissions expressed?** The entity specification says an action the person may not perform is not rendered. That needs the permission for each action, which is a property of the entry, like `requiresSession`, or a separate map.
