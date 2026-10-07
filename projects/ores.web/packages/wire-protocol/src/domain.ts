/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { z } from 'zod';
import { SYSTEM_TENANT_ID, toWireTimestamp, uuid, wireTimestamp, type Uuid } from './primitives.js';

/**
 * The account classifications the server understands.
 *
 * `user` accounts authenticate with a password; the rest authenticate through
 * sessions. See `ores.iam.api/domain/account.hpp`.
 */
export const ACCOUNT_TYPES = ['user', 'service', 'algorithm', 'llm'] as const;
export type AccountType = (typeof ACCOUNT_TYPES)[number];

const accountTypeSchema = z.enum(ACCOUNT_TYPES);

/**
 * A UUID as the server writes it: canonical lowercase, hyphenated.
 *
 * `z.uuid()` alone would accept an uppercase spelling the server never
 * produces, so the pattern is narrowed explicitly, and the value is branded
 * through {@link uuid} so the compile-time type is the same {@link Uuid} every
 * other layer uses.
 */
export const uuidSchema = z
    .string()
    .regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/)
    .transform((value): Uuid => uuid(value));

/**
 * An instant as the server writes it: `YYYY-MM-DD HH:MM:SSZ`.
 *
 * Written out rather than imported so this module stays free of a cycle with
 * the wire schemas.
 */
export const wireTimestampSchema = z.string().transform((value, ctx) => {
    try {
        return wireTimestamp(value);
    } catch (cause) {
        ctx.addIssue({ code: 'custom', message: 'Not a wire timestamp', cause });
        return z.NEVER;
    }
});

/**
 * A tenant-scoped account.
 *
 * The account holds no credential field at all: the password material lives in
 * its own entity, `iam.account_credential`, which no protocol serves, so there
 * is nothing to drop at the parse boundary.
 */
export const accountSchema = z.object({
    /** Optimistic-locking version. Bumped on every accepted write. */
    version: z.int().nonnegative(),
    id: uuidSchema,
    tenantId: uuidSchema,
    username: z.string(),
    /** Present only for accounts that represent a person. */
    fullName: z.string(),
    email: z.string(),
    accountType: accountTypeSchema,
    jobTitle: z.string(),
    /** Reporting line, or `null` when the account sits at the top. */
    reportsToAccountId: uuidSchema.nullable(),
    /** Quick-login party, or `null` when the account always picks a party. */
    defaultPartyId: uuidSchema.nullable(),
    /** The person's picture, or `null` when they have none. */
    imageId: uuidSchema.nullable(),
    modifiedBy: z.string(),
    changeReasonCode: z.string(),
    changeCommentary: z.string(),
    performedBy: z.string(),
    recordedAt: wireTimestampSchema,
});

export type Account = z.infer<typeof accountSchema>;

/**
 * A person's contact record, as the browser reads it.
 *
 * The record is keyed by its contact email, and it holds the address
 * colleagues use. That address is not the account's sign-in address: the two
 * are separate fields on separate records, and a screen shows both labelled
 * apart.
 */
export const accountContactInformationSchema = z.object({
    /** Optimistic-locking version. A write may claim the version it read. */
    version: z.int().nonnegative(),
    id: uuidSchema,
    accountId: uuidSchema,
    streetLine1: z.string(),
    streetLine2: z.string(),
    city: z.string(),
    state: z.string(),
    countryCode: z.string(),
    postalCode: z.string(),
    phone: z.string(),
    /** The address colleagues use. */
    email: z.string(),
    webPage: z.string(),
    modifiedBy: z.string(),
    changeReasonCode: z.string(),
    changeCommentary: z.string(),
    performedBy: z.string(),
    recordedAt: wireTimestampSchema,
});

export type AccountContactInformation = z.infer<typeof accountContactInformationSchema>;

/** One selectable party offered at login, or switchable mid-session. */
export const partySummarySchema = z.object({
    id: uuidSchema,
    name: z.string(),
    /** `System` or `Operational`. */
    partyCategory: z.string(),
    /** FpML business-centre code, for example `GBLO`. */
    businessCenterCode: z.string(),
});

export type PartySummary = z.infer<typeof partySummarySchema>;

/**
 * One tenant, as a roster reads it.
 *
 * The registry's audit columns are absent, because a roster names each tenant
 * and says what state it is in; nothing on it is about who last edited the row.
 * The system tenant is a row like any other here, so a reader that means "the
 * tenants somebody set up" excludes the row whose id is the system id rather
 * than trusting this shape to have done it.
 */
/**
 * The provisioning run that set a tenant up, as far as a roster needs it.
 *
 * `status` is the engine's state name: `in_progress`, `completed`, `failed`,
 * `compensating` or `compensated`. The step index counts from zero, as the
 * engine counts it, and `error` is empty unless the run stopped on one.
 */
export const tenantSetupSchema = z.object({
    instanceId: z.string(),
    status: z.string(),
    currentStepIndex: z.int().nonnegative(),
    stepCount: z.int().nonnegative(),
    error: z.string(),
});

export type TenantSetup = z.infer<typeof tenantSetupSchema>;

export const tenantSummarySchema = z.object({
    id: uuidSchema,
    code: z.string(),
    name: z.string(),
    type: z.string(),
    description: z.string(),
    hostname: z.string(),
    status: z.string(),
    registrationDefault: z.boolean(),
    /**
     * The latest run that provisioned this tenant, or `null` when none is on
     * record: a tenant created before runs named their target, or one the
     * reader could not ask about.
     */
    setup: tenantSetupSchema.nullable().default(null),
});

export type TenantSummary = z.infer<typeof tenantSummarySchema>;

/** A page of tenants. `totalCount` counts every tenant the caller can see. */
export const tenantPageSchema = z.object({
    tenants: z.array(tenantSummarySchema),
    totalCount: z.int().nonnegative(),
    /**
     * Whether the provisioning runs could not be read. The roster is still
     * the registry's answer, so a failed run read leaves every `setup` empty
     * and says so here rather than failing the page.
     */
    setupUnavailable: z.boolean().default(false),
    /**
     * How many tenants of the automation type matched and were left out,
     * because the roster hides test infrastructure unless asked to show it.
     */
    hiddenTestCount: z.int().nonnegative().default(0),
});

export type TenantPage = z.infer<typeof tenantPageSchema>;

/**
 * Why a tenant is on the system administrator's attention list: its setup
 * stopped on an error, or it is suspended and nobody in it can sign in.
 */
export const attentionReasonSchema = z.enum(['setup-failed', 'suspended']);

/** One provisioning run on the activity list, named for its tenant. */
export const setupActivitySchema = z.object({
    instanceId: z.string(),
    tenantName: z.string(),
    status: z.string(),
    currentStepIndex: z.int().nonnegative(),
    stepCount: z.int().nonnegative(),
    error: z.string(),
    at: z.string(),
});

export type SetupActivity = z.infer<typeof setupActivitySchema>;

/**
 * The state of the deployment's tenants, as the system administrator's home
 * shows it.
 *
 * The counts leave out the system tenant and test tenants, as the roster does.
 * `tenants` is the roster's first page, and `activity` the newest provisioning
 * runs. A failed run read empties `activity` and the failed setups and says so,
 * because the tenants are still the registry's answer.
 */
export const deploymentOverviewSchema = z.object({
    inService: z.int().nonnegative(),
    onEvaluation: z.int().nonnegative(),
    settingUp: z.int().nonnegative(),
    attention: z.array(
        z.object({
            tenant: tenantSummarySchema,
            reason: attentionReasonSchema,
        }),
    ),
    tenants: z.array(tenantSummarySchema),
    totalCount: z.int().nonnegative(),
    activity: z.array(setupActivitySchema),
    activityUnavailable: z.boolean().default(false),
});

export type DeploymentOverview = z.infer<typeof deploymentOverviewSchema>;

/**
 * One tenant as its own screen reads it: the roster's summary and the row's
 * provenance, which the roster leaves out because it is not about who last
 * edited the row.
 */
export const tenantDetailSchema = tenantSummarySchema.extend({
    version: z.int().nonnegative(),
    modifiedBy: z.string(),
    performedBy: z.string(),
    changeReasonCode: z.string(),
    changeCommentary: z.string(),
    recordedAt: z.string(),
});

export type TenantDetail = z.infer<typeof tenantDetailSchema>;

/**
 * One party of the session's tenant, as the parties screen lists it.
 *
 * `category` is `System` for the party every tenant is given and `Operational`
 * for a business party. `parentName` is the parent's name when the parent is
 * on the same page, and `null` for a root or a parent on another page.
 */
export const tenantPartySchema = z.object({
    id: uuidSchema,
    code: z.string(),
    name: z.string(),
    category: z.string(),
    type: z.string(),
    status: z.string(),
    parentId: z.string().nullable(),
    parentName: z.string().nullable(),
    /** FpML business-centre code, for example `GBLO`, or empty when unset. */
    businessCentreCode: z.string(),
    /** The flag of the centre's country, or `null` when it has none. */
    flagImageId: z.string().nullable(),
});

export type TenantParty = z.infer<typeof tenantPartySchema>;

/** One page of the session's own parties, and how many it holds in all. */
export const partyPageSchema = z.object({
    parties: z.array(tenantPartySchema),
    totalCount: z.int().nonnegative(),
});

export type PartyPage = z.infer<typeof partyPageSchema>;

/**
 * One tenant's screen in system administration: the tenant and its setup run.
 *
 * The tenant's own data, its parties among it, is read from inside the tenant
 * and not from here.
 */
export const tenantDetailResponseSchema = z.object({
    tenant: tenantDetailSchema,
    /** Whether the runs could not be read; `tenant.setup` is then empty. */
    setupUnavailable: z.boolean().default(false),
});

export type TenantDetailResponse = z.infer<typeof tenantDetailResponseSchema>;

/**
 * How one value of a code domain is painted.
 *
 * It is a badge's visual metadata and nothing else: the label inside the pill,
 * the words behind it, and the two colours. The severity is carried because the
 * catalogue states it and a screen may one day sort or filter on it; the
 * Bootstrap class the catalogue also holds is a hint for a browser that reads
 * Bootstrap, which this one does not.
 */
export const badgePresentationSchema = z.object({
    /** The badge's own code, which a screen may key a translation on. */
    code: z.string(),
    label: z.string(),
    description: z.string(),
    backgroundColour: z.string(),
    textColour: z.string(),
    severity: z.string(),
});

export type BadgePresentation = z.infer<typeof badgePresentationSchema>;

/**
 * One tenant lifecycle status, as a screen reads it.
 *
 * The words are the status row's own — `Suspended`, `Terminated` — and the
 * colours are the badge's. Keeping the two apart is the point: the row names
 * the state in the deployment's own vocabulary, and the badge catalogue
 * supplies one visual language shared by every state in the platform.
 *
 * The badge is nullable because a status nobody has painted is written plainly
 * rather than hidden.
 */
export const tenantStatusSchema = z.object({
    code: z.string(),
    name: z.string(),
    description: z.string(),
    badge: badgePresentationSchema.nullable(),
});

export type TenantStatus = z.infer<typeof tenantStatusSchema>;

/**
 * A tenant type, painted by the badge its row names.
 *
 * The same shape as a status: the words are the type row's own and the colours
 * the badge's. A type nobody has painted has no badge and is written plainly.
 */
export const tenantTypeSchema = tenantStatusSchema;

export type TenantType = TenantStatus;

/** A page of accounts. `totalCount` counts every account the caller can see. */
export const accountPageSchema = z.object({
    accounts: z.array(accountSchema),
    totalCount: z.int().nonnegative(),
    offset: z.int().nonnegative(),
    limit: z.int().positive(),
});

export type AccountPage = z.infer<typeof accountPageSchema>;

/**
 * A login record as a screen reads it.
 *
 * `failedLogins`, `locked`, `lastLogin` and `passwordResetRequired` are what
 * Rescue access and Audit sign-ins read. The record carries no credential
 * column, so nothing secret can be forwarded by accident.
 */
export const loginInfoSchema = z.object({
    tenantId: uuidSchema,
    accountId: uuidSchema,
    lastIp: z.string(),
    lastAttemptIp: z.string(),
    failedLogins: z.int().nonnegative(),
    locked: z.boolean(),
    lastLogin: z.string(),
    online: z.boolean(),
    passwordResetRequired: z.boolean(),
});

export type LoginInfo = z.infer<typeof loginInfoSchema>;

/** One page of login records. `totalCount` counts every record the caller can see. */
export const loginInfoPageSchema = z.object({
    loginInfo: z.array(loginInfoSchema),
    totalCount: z.int().nonnegative(),
});

export type LoginInfoPage = z.infer<typeof loginInfoPageSchema>;

/**
 * A session as a screen reads it.
 *
 * `endTime` is empty while the session is open, which is what makes a row an
 * active one; the read that answers only open sessions carries the same shape.
 */
export const sessionSchema = z.object({
    tenantId: uuidSchema,
    id: uuidSchema,
    accountId: uuidSchema,
    startTime: z.string(),
    endTime: z.string(),
    clientIp: z.string(),
    clientIdentifier: z.string(),
    clientVersionMajor: z.int().nonnegative(),
    clientVersionMinor: z.int().nonnegative(),
    bytesSent: z.int().nonnegative(),
    bytesReceived: z.int().nonnegative(),
    countryCode: z.string(),
    protocol: z.string(),
});

export type Session = z.infer<typeof sessionSchema>;

/** One page of sessions. `totalCount` counts every session the caller can see. */
export const sessionPageSchema = z.object({
    sessions: z.array(sessionSchema),
    totalCount: z.int().nonnegative(),
});

export type SessionPage = z.infer<typeof sessionPageSchema>;

/**
 * The live session's selected party, as the handover carries it.
 *
 * The account and tenant identifiers travel alongside the party because the
 * browser needs them to render the shell, and re-deriving them from a token it
 * must not read would be pointless indirection.
 */
export const activePartySchema = z.object({
    accountId: uuidSchema,
    tenantId: uuidSchema,
    tenantName: z.string(),
    party: partySummarySchema,
    sessionId: z.string(),
    /** When the token was last issued, so the browser can refresh before expiry. */
    issuedAt: wireTimestampSchema,
    accessLifetimeSeconds: z.int().positive(),
});

export type ActiveParty = z.infer<typeof activePartySchema>;

/** Re-exported so consumers do not import the sentinels from two places. */
export { SYSTEM_TENANT_ID, toWireTimestamp };

/**
 * One role an account holds: what it is, what it grants, and the grant's own
 * record of who gave it, when and why.
 */
export const heldRoleSchema = z.object({
    roleId: uuidSchema,
    name: z.string(),
    description: z.string(),
    permissionCodes: z.array(z.string()),
    givenBy: z.string(),
    givenAt: z.string(),
    reasonCode: z.string(),
    commentary: z.string(),
});

export type HeldRole = z.infer<typeof heldRoleSchema>;

/** The roles one account holds. */
export const accountAccessSchema = z.object({
    roles: z.array(heldRoleSchema),
});

export type AccountAccess = z.infer<typeof accountAccessSchema>;

/**
 * One role of the tenant's catalogue.
 *
 * A service role is one the platform's own services sign in with. The seed
 * names each one for its service, so a role whose name ends in `Service` is
 * one, and its permissions are not worth reading.
 *
 * Whether a person may ask for a role is the tenant's answer, carried on the
 * row as `requestable` rather than worked out from the role's name.
 */
export const roleSummarySchema = z.object({
    id: uuidSchema,
    version: z.int().nonnegative(),
    name: z.string(),
    description: z.string(),
    service: z.boolean(),
    /** Whether a person who registers is given this role. */
    registrationDefault: z.boolean(),
    /** Whether a member may ask for this role. */
    requestable: z.boolean(),
    permissionCodes: z.array(z.string()),
});

export type RoleSummary = z.infer<typeof roleSummarySchema>;

/** One permission the platform defines: its code and what it allows. */
export const permissionEntrySchema = z.object({
    code: z.string(),
    description: z.string(),
});

export type PermissionEntry = z.infer<typeof permissionEntrySchema>;

/**
 * One account's sign-ins: the account, its sign-in state and a page of its
 * sessions, newest first.
 *
 * The state is absent for an account that has never signed in. A session's
 * end time is empty when no end was recorded, which today is every session:
 * signing out writes none.
 */
export const accountSignInsSchema = z.object({
    account: accountSchema,
    loginInfo: loginInfoSchema.nullable(),
    sessions: z.array(sessionSchema),
    totalCount: z.int().nonnegative(),
});

export type AccountSignIns = z.infer<typeof accountSignInsSchema>;

/**
 * Which columns a classification list carries.
 *
 * Every list is keyed by its code. A named list also carries a name and a
 * display order; a plain list carries only a description; an ordered list
 * carries a description and a display order, but no name.
 */
export const classificationShapeSchema = z.enum(['named', 'plain', 'ordered']);

export type ClassificationShape = z.infer<typeof classificationShapeSchema>;

/**
 * One classification list as a screen offers it.
 *
 * A list that is not editable holds spellings an ORE document must write
 * exactly, so the screen shows it and refuses to change it.
 */
export const classificationListSchema = z.object({
    key: z.string(),
    entityType: z.string(),
    topic: z.string(),
    shape: classificationShapeSchema,
    editable: z.boolean(),
    /** The permissions the server checks to write and to remove a row. */
    writePermission: z.string(),
    deletePermission: z.string(),
});

export type ClassificationList = z.infer<typeof classificationListSchema>;

/**
 * One row of a classification list.
 *
 * The name is empty and the order absent on a list whose shape lacks them.
 */
export const classificationRowSchema = z.object({
    code: z.string(),
    name: z.string(),
    description: z.string(),
    displayOrder: z.int().nullable(),
    version: z.int().nonnegative(),
    modifiedBy: z.string(),
    recordedAt: z.string(),
    reasonCode: z.string(),
    commentary: z.string(),
    /** The badge code of the row's label in the shared catalogue, or null when it has none. */
    labelCode: z.string().nullable(),
});

export type ClassificationRow = z.infer<typeof classificationRowSchema>;

/** A field's value in one version of a record, as the history renders it. */
export const historyFieldSchema = z.object({
    name: z.string(),
    value: z.string(),
});

/** One field that differs from the version before. */
export const historyChangeSchema = z.object({
    field: z.string(),
    before: z.string(),
    after: z.string(),
});

/**
 * One version of a record: who wrote it, when, every field it held, and what
 * changed from the version before. The oldest version has no changes.
 */
export const historyVersionSchema = z.object({
    version: z.int().nonnegative(),
    modifiedBy: z.string(),
    recordedAt: z.string(),
    fields: z.array(historyFieldSchema),
    changes: z.array(historyChangeSchema),
});

export type HistoryVersion = z.infer<typeof historyVersionSchema>;
