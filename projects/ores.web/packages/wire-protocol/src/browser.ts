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

/**
 * The browser-safe surface of the protocol package.
 *
 * The default entry point pulls in the NATS client and its transport, which
 * reach for Node built-ins. A browser must never load any of that, and the
 * browser also has no business opening a broker connection: the BFF owns it.
 * This entry point therefore re-exports only data, schemas, and limits, and
 * deliberately does not re-export the client, the transport, or the codec.
 */

// Data and schemas.
export {
    SYSTEM_TENANT_ID,
    fromWireTimestamp,
    isUuid,
    isWireTimestamp,
    toWireTimestamp,
    uuid,
    wireTimestamp,
} from './primitives.js';
export type { Uuid, WireTimestamp } from './primitives.js';

export {
    ACCOUNT_TYPES,
    accountAccessSchema,
    accountContactInformationSchema,
    accountPageSchema,
    accountSignInsSchema,
    accountSchema,
    activePartySchema,
    badgePresentationSchema,
    deploymentOverviewSchema,
    heldRoleSchema,
    partySummarySchema,
    permissionEntrySchema,
    roleSummarySchema,
    classificationListSchema,
    classificationRowSchema,
    classificationShapeSchema,
    historyVersionSchema,
    tenantDetailResponseSchema,
    tenantDetailSchema,
    tenantPageSchema,
    tenantPartySchema,
    partyPageSchema,
    tenantSetupSchema,
    tenantStatusSchema,
    tenantTypeSchema,
    tenantSummarySchema,
} from './domain.js';
export type {
    Account,
    AccountAccess,
    AccountContactInformation,
    AccountPage,
    AccountSignIns,
    AccountType,
    ActiveParty,
    BadgePresentation,
    DeploymentOverview,
    HeldRole,
    PartySummary,
    PermissionEntry,
    RoleSummary,
    ClassificationList,
    ClassificationRow,
    ClassificationShape,
    HistoryVersion,
    SetupActivity,
    TenantDetail,
    TenantDetailResponse,
    TenantPage,
    TenantParty,
    PartyPage,
    TenantSetup,
    TenantStatus,
    TenantType,
    TenantSummary,
} from './domain.js';

export {
    apiErrorSchema,
    tenantStatusesResponseSchema,
    tenantTypesResponseSchema,
    bootstrapStatusSchema,
    createAdministratorRequestSchema,
    databaseInfoSchema,
    initialAdministratorSchema,
    loginResultSchema,
    loginSuccessSchema,
    partyChoiceSchema,
    registrationPolicyViewSchema,
    sessionModeSchema,
    sessionViewSchema,
    signupRequestSchema,
    signupResultSchema,
    sseEnvelopeSchema,
} from './contracts.js';
export type {
    ApiError,
    TenantStatusesResponse,
    BootstrapStatus,
    CreateAdministratorRequest,
    DatabaseInfo,
    InitialAdministrator,
    LoginResult,
    LoginSuccess,
    PartyChoice,
    RegistrationPolicyView,
    SessionMode,
    SessionView,
    SignupRequest,
    SignupResult,
} from './contracts.js';

// One expected service instance and its last report, which the operations
// screens read. It is a generated protocol shape, and the browser surface
// states it rather than making a screen reach through the transport.
export type { ServiceRosterSlot } from './generated/telemetry/protocol/service_samples_protocol.js';

// Subjects, so a browser-side module can name one without importing the
// transport that would know how to reach it.
export { SUBJECTS } from './operations.js';

// The result as a form reads it: the outcome and code, and the field failures
// a profile panel branches on.
export { decidedResultSchema, fieldFailureSchema } from './operations.js';
export type { DecidedResult } from './operations.js';

// The profile screen's answers, as the BFF serves them: the writes' results
// and records, and the contact read. The request shapes ride along so the
// api client can type what it sends.
export {
    accountWriteViewSchema,
    claimedContactWriteSchema,
    contactViewSchema,
    contactWriteSchema,
    contactWriteViewSchema,
    profileWriteSchema,
} from './profile-operations.js';
export type {
    AccountWriteView,
    ClaimedContactWrite,
    ContactView,
    ContactWrite,
    ContactWriteView,
    ProfileWrite,
} from './profile-operations.js';

// The upload and the rule it must satisfy, as the picker reads them.
export { imageUploadPolicyViewSchema, imageUploadViewSchema } from './entities/image.js';
export type { ImageUploadPolicy, ImageUploadView } from './entities/image.js';

// The starting-point read, so the browser parses what the BFF served with the
// definition the server serialised it from.
export { seedProfileChoiceSchema, seedProfilesResponseSchema } from './operations.js';
export type { SeedProfileChoice, SeedProfilesResponse } from './operations.js';

// The legal entities a tenant can be started from, for the search that fills a
// form rather than making somebody type a code they would have to know.
export { leiEntityChoiceSchema, leiEntitiesResponseSchema } from './operations.js';
export type { LeiEntityChoice, LeiEntitiesResponse } from './operations.js';

// The provision request and its answer, parsed by the browser for the same
// reason: the BFF serialised them from these definitions.
export { provisionTenantRequestSchema, provisionTenantResultSchema } from './operations.js';
export type { ProvisionTenantRequest, ProvisionTenantResult } from './operations.js';

// Adding one party of the tenant the person already works in, and the run its
// data is published by. The party row itself is not here: the browser describes
// a party and the BFF places it, so no screen needs the tenant's hierarchy.
export { provisionPartyRequestSchema, provisionPartyResultSchema } from './operations.js';
export type { ProvisionPartyRequest, ProvisionPartyResult } from './operations.js';

// The run a provision starts, which its journey follows by asking again, and
// the retry that resumes it from the step that failed.
export { retryWorkflowInstanceResultSchema, workflowProgressSchema } from './operations.js';
export type {
    RetryWorkflowInstanceResult,
    WorkflowProgress,
    WorkflowStepSummary,
} from './operations.js';

// The rules a password must satisfy, read before anybody has signed in because
// the sign-in screen is where they are shown.
export { passwordPolicySchema } from './operations.js';
export type { PasswordPolicy } from './operations.js';

// The services roster the operations screen reads, as the BFF serves it: one
// row per expected instance, each with the age the BFF marked. The browser
// parses what the BFF served with the definition the BFF wrote it from.
export { serviceRosterViewSchema } from './operations.js';
export type { ServiceRosterRow, ServiceRosterView } from './operations.js';

// The compute grid the operations screen reads, as the BFF serves it: the
// stored counters with their sample time, one row per node with its hostname,
// and the wrappers placed on the nodes they run on.
export { gridViewSchema, gridNodeRowSchema, gridWrapperRowSchema } from './operations.js';
export type { GridNodeRow, GridView, GridWrapperRow } from './operations.js';

// The login record a credentials screen reads. It carries no credential
// column, so nothing secret travels with it.
export { loginInfoPageSchema, loginInfoSchema } from './domain.js';
export type { LoginInfo, LoginInfoPage } from './domain.js';

// The sessions the audit screen reads: one page of every session, and the open
// ones on their own. An empty endTime is what makes a row active.
export { sessionPageSchema, sessionSchema } from './domain.js';
export type { Session, SessionPage } from './domain.js';

// The inbox: the requests screen, the requests queue and the notification
// bell. The BFF drives the transports, so the browser receives only the
// answers these schemas describe, the operations that name them, and the
// subjects, so a browser-side module can name one without importing the
// transport that would know how to reach it.
export {
    INBOX_JOIN_SUBJECTS,
    INBOX_SUBJECTS,
    askForRoles,
    clearNotifications,
    decideRequest,
    inboxNotificationPageSchema,
    inboxNotificationViewSchema,
    inboxPageSchema,
    inboxRequestDecisionViewSchema,
    inboxRequestPageSchema,
    inboxRequestRoleViewSchema,
    inboxRequestViewSchema,
    markNotificationsRead,
    readMyNotifications,
    readMyRequests,
    readRequestQueue,
    readUnreadNotificationCount,
    withdrawRequest,
} from './inbox.js';
export type {
    InboxNotificationView,
    InboxPage,
    InboxRequestDecisionView,
    InboxRequestRoleView,
    InboxRequestView,
} from './inbox.js';
