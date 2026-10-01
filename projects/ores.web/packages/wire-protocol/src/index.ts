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
    LIVE_WORKSPACE_ID,
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
    accountPageSchema,
    accountSchema,
    activePartySchema,
    badgePresentationSchema,
    partySummarySchema,
    tenantPageSchema,
    tenantStatusSchema,
    tenantSummarySchema,
} from './domain.js';
export type {
    Account,
    AccountPage,
    AccountType,
    ActiveParty,
    BadgePresentation,
    PartySummary,
    TenantPage,
    TenantStatus,
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
    TENANT_SUBJECTS,
    listTenantsRequestSchema,
    readTenantsPage,
    wireTenantPageSchema,
} from './tenants.js';
export type { ListTenantsRequest, WireTenantPage } from './tenants.js';

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
    WorkspaceContext,
} from './client.js';

export {
    SUBJECTS,
    accountIdsRequestSchema,
    accountOperationResultSchema,
    accountPageSchema as wireAccountPageSchema,
    accountReplySchema,
    accountUsernameRequestSchema,
    activeSessionsReplySchema,
    changePasswordRequestSchema,
    changePasswordResultSchema,
    emptyRequestSchema,
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
    wirePartySchema,
} from './operations.js';
export type {
    AccountIdsRequest,
    AccountOperationResult,
    ChangePasswordRequest,
    ChangePasswordResult,
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
    imageInfoSchema,
    listImagesRequestSchema,
    listImagesResponseSchema,
    getImagesRequestSchema,
    getImagesResponseSchema,
    imageBytesToBuffer,
    imageBytesToText,
    imageSchema,
} from './entities/image.js';
export type { WireImage, WireImageInfo } from './entities/image.js';

export {
    ACCOUNT_SUBJECTS,
    changeOwnPassword,
    changeOwnPasswordRequestSchema,
    deleteAccount,
    setAccountsLocked,
} from './account-operations.js';
export type { AuthenticatedCaller, ChangeOwnPasswordRequest } from './account-operations.js';

export {
    CREDENTIAL_SUBJECTS,
    readAccount,
    readAccountsPage,
    readActiveSessions,
    readLoginInfo,
    readLoginInfoPage,
    readSessionsPage,
} from './credentials.js';

// The HTTP contract shared by the BFF and the browser. Both sides parse with
// these definitions, so the network boundary is checked at runtime.
export {
    apiErrorSchema,
    tenantStatusesResponseSchema,
    bootstrapStatusSchema,
    createAdministratorRequestSchema,
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
} from './contracts.js';
export type {
    ApiError,
    TenantStatusesResponse,
    BootstrapStatus,
    CreateAdministratorRequest,
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
