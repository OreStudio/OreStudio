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
 * `@ores/wire-protocol` is the only place that knows how ORE Studio speaks on the
 * NATS bus: the msgpack encoding, the subject names, the snake_case field
 * names, and the `X-Error` contract.
 *
 * Everything above this package works with the camelCase domain types exported
 * here. The server's credential fields are dropped at the parse boundary and
 * are not represented at all.
 */

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
    permissionPageSchema,
    rolePageSchema,
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

export { BADGE_SUBJECTS, listBadgeDefinitionsRequestSchema, readBadgeCatalogue } from './badges.js';
export type { BadgeCatalogue } from './badges.js';

export {
    TENANT_STATUS_SUBJECTS,
    listTenantStatusesRequestSchema,
    readTenantStatuses,
} from './tenant-statuses.js';

export {
    TENANT_TYPE_SUBJECTS,
    listTenantTypesRequestSchema,
    readTenantTypes,
} from './tenant-types.js';

export {
    PROVISION_TENANT_TARGET_KIND,
    PROVISION_TENANT_WORKFLOW_TYPE,
    TENANT_SETUP_WORKFLOW_TYPE,
    TENANT_SUBJECTS,
    TENANT_SETUP_READ_LIMIT,
    listTenantsPage,
    readTenant,
    readProvisioningRuns,
    readTenantSetupRun,
    removeTenant,
    readTenantSetups,
    wireTenantPageSchema,
    wireWorkflowInstancesSchema,
} from './tenants.js';
export type {
    ProvisioningRun,
    StepsDoneReader,
    TenantListQuery,
    TenantRemovalOutcome,
    WireTenantPage,
} from './tenants.js';
export { readPartiesPage } from './party-page.js';
export {
    ACCESS_SUBJECTS,
    deleteRole,
    giveRole,
    readAccountAccess,
    readAccountPermissions,
    readRolePermissionsPage,
    readRolesPage,
    changeRolePermissions,
    type RolePermissionQuery,
    type RolesQuery,
    readMyPermissions,
    type PermissionQuery,
    readMyAccess,
    readPermissionCatalogue,
    readRoles,
    saveRole,
    saveRolePermissions,
    takeRoleAway,
} from './access.js';
export type { AccessWrite } from './access.js';
export {
    REFDATA_RECORDS,
    listRecordPage,
    listRecords,
    readRecord,
    readCalendarYear,
    rebuildCalendar,
    recordResource,
    removeRecord,
    resourceName,
    saveRecord,
} from './records.js';
export type {
    PageRequest,
    RecordPage,
    CalendarDay,
    RecordResource,
    RecordRow,
    WriteIntent,
    WriteOutcome,
} from './records.js';
export {
    CLASSIFICATION_LISTS,
    classificationCatalogue,
    classificationList,
    codeDomainOf,
    countClassificationRows,
    listClassificationRows,
    readClassificationLabels,
    readLabelCatalogue,
    setClassificationLabel,
    readEntityHistory,
    removeClassificationRow,
    saveClassificationRow,
    saveClassificationRows,
} from './classifications.js';
export type {
    ClassificationIntent,
    ClassificationRowInput,
    ClassificationWrite,
} from './classifications.js';
export type { PartyPageQuery } from './party-page.js';

export {
    ProtocolError,
    MalformedResponseError,
    NotAuthenticatedError,
    NotConnectedError,
    OperationFailedError,
    RequestTimeoutError,
    ServerError,
    ServiceUnavailableError,
    SessionExpiredError,
    TransportError,
    serverErrorFor,
    X_ERROR_HEADER,
} from './errors.js';
export type { ServerErrorCode } from './errors.js';

export { GZIP_ENCODING, CONTENT_ENCODING_HEADER, WireCodec } from './codec.js';
export type { WireFormat } from './codec.js';

export { NatsTransport } from './transport.js';
export type {
    NatsTransportOptions,
    Reply,
    RequestHeaders,
    TlsMaterial,
    Transport,
} from './transport.js';

export { resolveHeaders } from './headers.js';
export type { HeaderSource } from './headers.js';

export { DEFAULT_TIMEOUTS, OresClient } from './client.js';
export type {
    ActiveSession,
    LoginCredentials,
    LoginOutcome,
    LoginRejected,
    OresClientOptions,
    PartySelectionRequired,
    Timeouts,
} from './client.js';

export {
    SUBJECTS,
    accountIdsRequestSchema,
    accountOperationResultSchema,
    accountPageSchema as wireAccountPageSchema,
    accountReplySchema,
    accountUsernameRequestSchema,
    accountWriteReplySchema,
    activeSessionsReplySchema,
    authEventListSchema,
    changePasswordRequestSchema,
    changePasswordResultSchema,
    decidedResultSchema,
    emptyRequestSchema,
    endSessionReplySchema,
    lookupCountryReplySchema,
    httpInfoResponseSchema,
    listAccountsRequestSchema,
    listLoginInfoRequestSchema,
    listSessionsRequestSchema,
    loginInfoKeyRequestSchema,
    loginInfoPageSchema as wireLoginInfoPageSchema,
    loginInfoReplySchema,
    lockResultSchema,
    loginRequestSchema,
    loginResponseSchema,
    logoutResponseSchema,
    partyRequestSchema,
    partyResponseSchema,
    refreshResponseSchema,
    sessionPageSchema as wireSessionPageSchema,
    sessionStatisticsListSchema,
    wirePartySchema,
} from './operations.js';
export type {
    AccountIdsRequest,
    AccountOperationResult,
    AccountWriteReply,
    ChangePasswordRequest,
    ChangePasswordResult,
    DecidedResult,
    HttpInfoResponse,
    ListAccountsRequest,
    LoginRequest,
    LoginResponse,
    LockResult,
    LogoutResponse,
    PartyRequest,
    PartyResponse,
    RefreshResponse,
    WireAccountPage,
} from './operations.js';

export { changeReasonPageSchema, changeReasonSchema } from './operations.js';
export type { ChangeReason } from './operations.js';

export {
    leiEntityChoiceSchema,
    leiEntitiesResponseSchema,
    leiEntitySummaryResponseSchema,
    searchLeiEntitiesResponseSchema,
} from './operations.js';
export type {
    LeiEntityChoice,
    LeiEntitiesResponse,
    LeiEntitySummaryResponse,
    SearchLeiEntitiesResponse,
} from './operations.js';
export { seedProfileChoiceSchema, seedProfilesResponseSchema } from './operations.js';
export type { SeedProfileChoice, SeedProfilesResponse } from './operations.js';

export { provisionTenantRequestSchema, provisionTenantResultSchema } from './operations.js';
export type { ProvisionTenantRequest, ProvisionTenantResult } from './operations.js';

export {
    listPartiesReplySchema,
    partyWireRowSchema,
    provisionPartyReplySchema,
    provisionPartyRequestSchema,
    provisionPartyResultSchema,
    putPartyResultSchema,
    toPutPartyChange,
} from './operations.js';
export type {
    PartyRow,
    ProvisionPartyRequest,
    ProvisionPartyResult,
    PutPartyResult,
} from './operations.js';

export { passwordPolicySchema } from './operations.js';
export type { PasswordPolicy } from './operations.js';

// The services roster the operations screen reads: the request, the wire reply,
// and the view the BFF serves the browser with each row's age marked.
export {
    serviceRosterReplySchema,
    serviceRosterRequestSchema,
    serviceRosterRowSchema,
    serviceRosterSlotSchema,
    serviceRosterViewSchema,
} from './operations.js';
export type { ServiceRosterRow, ServiceRosterView } from './operations.js';

// The compute grid the operations screen reads: the empty request, the wire
// reply with the stored counters, the host page the node names are joined from,
// and the view the BFF serves the browser.
export {
    gridStatsReplySchema,
    gridStatsRequestSchema,
    gridViewSchema,
    listHostsReplySchema,
    listHostsRequestSchema,
    nodeSummarySchema,
} from './operations.js';
export type { GridStatsReply, GridView } from './operations.js';

// The message bus the operations screen reads: the two sample queries, their
// replies, and the view the BFF serves the browser.
export {
    busViewSchema,
    natsServerSampleSchema,
    natsServerSamplesQuerySchema,
    natsServerSamplesReplySchema,
    natsServerSamplesRequestSchema,
    natsStreamSampleSchema,
    natsStreamSamplesQuerySchema,
    natsStreamSamplesReplySchema,
    natsStreamSamplesRequestSchema,
} from './operations.js';
export type { BusView } from './operations.js';

// The telemetry logs the operations screen reads: the filter query, the entry
// rows, the reply carrying the count, and the view the BFF serves the browser.
export {
    logsListReplySchema,
    logsListRequestSchema,
    logsViewSchema,
    telemetryLogEntrySchema,
    telemetryLogQuerySchema,
} from './operations.js';
export type { LogsListReply, LogsView } from './operations.js';

// The door: what the deployment offers somebody who is not in it yet, and the
// registration that acts on that answer.
export {
    registrationPolicyReplySchema,
    registrationPolicyRequestSchema,
    signupCommandSchema,
    signupReplySchema,
    toRegistrationPolicy,
    toSignupOutcome,
} from './operations.js';
export type { RegistrationPolicy, SignupOutcome } from './operations.js';

export {
    retryWorkflowInstanceRequestSchema,
    retryWorkflowInstanceResultSchema,
    workflowProgressSchema,
    workflowStepSummarySchema,
} from './operations.js';
export type {
    RetryWorkflowInstanceRequest,
    RetryWorkflowInstanceResult,
    WorkflowProgress,
    WorkflowStepSummary,
} from './operations.js';

export {
    imageUploadPolicyViewSchema,
    imageUploadViewSchema,
    listImageSummaries,
    readImages,
    readImageUploadPolicy,
    uploadImage,
} from './entities/image.js';
export type {
    ImageContent,
    ImageSummary,
    ImageUploadPolicy,
    ImageUploadReply,
    ImageUploadView,
} from './entities/image.js';

export {
    ACCOUNT_SUBJECTS,
    changeOwnPassword,
    changeOwnPasswordRequestSchema,
    deleteAccount,
    setAccountsLocked,
} from './account-operations.js';
export type { AuthenticatedCaller, ChangeOwnPasswordRequest } from './account-operations.js';

// The profile write path: the self writes and the two administered writes,
// each over the subject the journey records, and the contact read. The view
// schemas are the same answers as the BFF serves them to the browser.
export {
    accountWriteViewSchema,
    claimedContactWriteSchema,
    contactViewSchema,
    contactWriteSchema,
    contactWriteViewSchema,
    profileWriteSchema,
    putContactInformation,
    readContactInformation,
    readMyContactInformation,
    updateAccount,
    updateSelfAccount,
    updateSelfContactInformation,
} from './profile-operations.js';
export type {
    AccountWriteView,
    ClaimedContactWrite,
    ContactView,
    ContactWrite,
    ContactWriteView,
    ProfileWrite,
} from './profile-operations.js';

export {
    defaultPartyRequestSchema,
    myPartiesSchema,
    myPartySchema,
    readMyParties,
    readReportingTree,
    reportingLineRequestSchema,
    reportingTreeNodeSchema,
    reportingTreePartySchema,
    reportingTreeSchema,
    setMyDefaultParty,
    readMyAccount,
    readMyLoginInfo,
    readMySessions,
    setReportingLine,
} from './membership.js';
export type {
    MyParties,
    MyParty,
    ReportingLineWrite,
    ReportingTree,
    ReportingTreeNode,
    ReportingTreeParty,
} from './membership.js';

export {
    CREDENTIAL_SUBJECTS,
    endSession,
    readAccount,
    readAccountSignIns,
    readAccountsPage,
    readActiveSessions,
    readAuthEvents,
    readLoginInfo,
    lookupCountry,
    readLoginInfoPage,
    readSessionStatistics,
    readSessionsPage,
    setAccountLocked,
} from './credentials.js';
export type { AuditWindow } from './credentials.js';

// The HTTP contract shared by the BFF and the browser. Both sides parse with
// these definitions, so the network boundary is checked at runtime.
export {
    apiErrorSchema,
    tenantStatusesResponseSchema,
    tenantTypesResponseSchema,
    bootstrapStatusSchema,
    createAdministratorRequestSchema,
    databaseInfoSchema,
    initialAdministratorSchema,
    loginRequestSchema as httpLoginRequestSchema,
    loginResultSchema,
    loginSuccessSchema,
    partyChoiceSchema,
    registrationPolicyViewSchema,
    selectPartyRequestSchema,
    sessionModeSchema,
    sessionViewSchema,
    signupRequestSchema,
    signupResultSchema,
    sseEnvelopeSchema,
    tenantSetupRunSchema,
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
    TenantSetupRun,
} from './contracts.js';

export { readImageMap } from './image-map.js';
export type { ImageMap } from './image-map.js';

// The inbox. The BFF calls these; the browser parses what the BFF answers with
// the schemas they are built from.
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
    inboxRequestQueueSchema,
    inboxRequestRoleViewSchema,
    inboxRequestStorySchema,
    inboxRequestViewSchema,
    inboxStoryEventSchema,
    inboxStoryFieldSchema,
    markNotificationsRead,
    readMyNotifications,
    readMyRequests,
    readRequest,
    readRequestQueue,
    readRequestStory,
    readUnreadNotificationCount,
    withdrawRequest,
} from './inbox.js';
export type {
    InboxNotificationView,
    InboxPage,
    InboxRequestDecisionView,
    InboxRequestQueue,
    InboxRequestedRole,
    InboxRequestRoleView,
    InboxRequestStory,
    InboxRequestView,
    InboxStoryEvent,
    InboxStoryField,
    RequestViewer,
} from './inbox.js';

// The timeline. One subject's story in one stream, whichever subject it is.
export {
    ACCOUNT_ENTITY,
    AUTH_EVENT_ENTITY,
    CONTACT_ENTITY,
    TIMELINE_PROVENANCE_FIELDS,
    fieldValue,
    orderTimeline,
    readPersonTimeline,
    timelineEventSchema,
    timelineFieldSchema,
    timelineGapSchema,
    timelineSchema,
} from './timeline.js';
export type { Timeline, TimelineEvent, TimelineField, TimelineGap } from './timeline.js';
