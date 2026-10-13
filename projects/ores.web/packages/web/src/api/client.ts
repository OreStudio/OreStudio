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
import { siteStateSchema, type SiteState } from '@ores/contracts';
import {
    accountAccessSchema,
    permissionPageSchema,
    rolePageSchema,
    roleHoldersSchema,
    accountSignInsSchema,
    accountWriteViewSchema,
    contactViewSchema,
    contactWriteViewSchema,
    imageUploadPolicyViewSchema,
    imageUploadViewSchema,
    myPartiesSchema,
    reportingTreeSchema,
    type AccountContactInformation,
    type AccountSignIns,
    type AccountWriteView,
    type ClaimedContactWrite,
    type ContactWrite,
    type ContactWriteView,
    type ImageUploadPolicy,
    type ImageUploadView,
    type MyParties,
    type ReportingLineWrite,
    type ReportingTree,
    type ProfileWrite,
    permissionEntrySchema,
    roleSummarySchema,
    badgePresentationSchema,
    classificationListSchema,
    classificationRowSchema,
    historyVersionSchema,
    inboxNotificationPageSchema,
    bookPreviewViewSchema,
    bookRaiseViewSchema,
    inboxRequestPageSchema,
    inboxRequestQueueSchema,
    inboxRequestStorySchema,
    inboxRequestViewSchema,
    requestChangesViewSchema,
    requestChecksViewSchema,
    timelineSchema,
    type BadgePresentation,
    type ClassificationList,
    type ClassificationRow,
    type HistoryVersion,
    type Timeline,
    type InboxNotificationView,
    type InboxPage,
    type BookLineInput,
    type BookPreview,
    type BookRaise,
    type InboxRequestQueue,
    type RequestChange,
    type RequestChecks,
    type InboxRequestStory,
    type InboxRequestView,
    type AccountAccess,
    type PermissionPage,
    type RoleHolders,
    type RolePage,
    type PermissionEntry,
    type RoleSummary,
    accountSchema,
    bootstrapStatusSchema,
    initialAdministratorSchema,
    loginResultSchema,
    leiEntitiesResponseSchema,
    passwordPolicySchema,
    provisionPartyResultSchema,
    provisionTenantResultSchema,
    registrationPolicyViewSchema,
    retryWorkflowInstanceResultSchema,
    seedProfilesResponseSchema,
    sessionViewSchema,
    signupResultSchema,
    partyPageSchema,
    tenantDetailResponseSchema,
    tenantPageSchema,
    tenantSetupRunSchema,
    deploymentOverviewSchema,
    tenantStatusesResponseSchema,
    tenantTypesResponseSchema,
    serviceRosterViewSchema,
    gridSeriesViewSchema,
    gridUsageViewSchema,
    gridViewSchema,
    busViewSchema,
    logsViewSchema,
    workflowProgressSchema,
    loginInfoSchema,
    sessionSchema,
    authEventSchema,
    sessionStatisticsSchema,
    type Account,
    type AuthEvent,
    type BootstrapStatus,
    type CreateAdministratorRequest,
    type InitialAdministrator,
    type LeiEntityChoice,
    type LoginInfo,
    type LoginResult,
    type Session,
    type SessionStatisticsRow,
    type PasswordPolicy,
    type ProvisionPartyRequest,
    type ProvisionPartyResult,
    type ProvisionTenantRequest,
    type ProvisionTenantResult,
    type RegistrationPolicyView,
    type RetryWorkflowInstanceResult,
    type SeedProfileChoice,
    type ServiceRosterRow,
    type GridSeriesView,
    type GridUsageView,
    type GridView,
    type SeriesRange,
    type BusView,
    type LogsView,
    type SessionView,
    type SignupRequest,
    type SignupResult,
    type PartyPage,
    type TenantDetailResponse,
    type TenantPage,
    type TenantSetupRun,
    type DeploymentOverview,
    type TenantStatus,
    type TenantType,
    type WorkflowProgress,
} from '@ores/wire-protocol/browser';
import { ApiFailure, request } from './transport.js';

/**
 * The session and account calls.
 *
 * These run after a connection has been chosen and a credential accepted. The
 * browser holds an opaque cookie rather than a token, so it cannot decide
 * locally whether it is signed in and asks instead.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

export interface Credentials {
    readonly username: string;
    readonly password: string;
}

/**
 * The range presets the operations screens offer, which the BFF turns into a
 * window on the deployment's clock.
 */
export type BusRange = '15m' | '1h' | '6h';
export type { SeriesRange };

/**
 * The range presets the logs screen offers, which the BFF turns into a window
 * on the deployment's clock.
 */
export type LogsRange = '15m' | '1h' | '6h' | '24h';

/**
 * The period presets the audit screen offers, which the BFF turns into a
 * window on the deployment's clock. `all` names no window at all.
 */
export type AuditPeriod = 'hour' | 'day' | 'week' | 'all';

/**
 * The filters the logs screen sends, already applied.
 *
 * An empty level or source means the filter is not applied rather than a value
 * to match, because the read's fields are optional and combine with AND. The
 * component, tag and message are the text the person searched for.
 */
export interface LogsQuery {
    readonly range: LogsRange;
    readonly level: string;
    readonly source: string;
    readonly component: string;
    readonly tag: string;
    readonly message: string;
    readonly offset: number;
    readonly limit: number;
}

/** Which page of a person's permissions is asked for. An empty area is the first they hold. */
export interface PermissionPageQuery {
    readonly area: string;
    readonly search: string;
    readonly offset: number;
    readonly limit: number;
}

/** Which page of the roles is asked for. */
export interface RolesPageQuery {
    readonly roleId?: string;
    readonly search: string;
    readonly area: string;
    readonly includeService: boolean;
    readonly offset: number;
    readonly limit: number;
}

/** Which page of a role's permissions the editor asks for. */
export interface RolePermissionPageQuery extends PermissionPageQuery {
    readonly includeUnheld: boolean;
}

function permissionParams(query: PermissionPageQuery): string {
    return new URLSearchParams({
        area: query.area,
        search: query.search,
        offset: String(query.offset),
        limit: String(query.limit),
    }).toString();
}

/** A page's offset and limit as a query string. */
function pageQuery(page: { readonly offset: number; readonly limit: number }): string {
    return new URLSearchParams({
        offset: String(page.offset),
        limit: String(page.limit),
    }).toString();
}

/** A reference data resource as the BFF describes it. */
const recordResourceViewSchema = z.object({
    key: z.string(),
    entityType: z.string(),
    keyFields: z.array(z.string()),
    versioned: z.boolean(),
    writable: z.boolean(),
    search: z.boolean(),
    sortable: z.array(z.string()),
    /** Where the services announce changes to this resource: the component and the events collection. */
    events: z.object({ component: z.string(), entity: z.string() }),
    writePermission: z.string(),
    deletePermission: z.string(),
});

export type RecordResourceView = z.infer<typeof recordResourceViewSchema>;

/** A synthetic resource as the BFF describes it to this session. */
const syntheticResourceViewSchema = z.object({
    key: z.string(),
    entityType: z.string(),
    keyField: z.string(),
    writable: z.boolean(),
    readOnlyBecause: z.string().nullable(),
    search: z.boolean(),
    sortable: z.array(z.string()),
    readPermission: z.string(),
    writePermission: z.string(),
    deletePermission: z.string(),
});

export type SyntheticResourceView = z.infer<typeof syntheticResourceViewSchema>;

/** A reference data row, in the server's own field names, with its version. */
const recordRowSchema = z.looseObject({ version: z.int().nonnegative() });

export type RecordRow = z.infer<typeof recordRowSchema>;

const codeImages = z.record(z.string(), z.string());

const imageMapSchema = z.object({
    currencies: codeImages,
    countries: codeImages,
    calendars: codeImages,
    businessCentres: codeImages,
    noFlag: z.string().nullable(),
});

export type ImageMap = z.infer<typeof imageMapSchema>;

const imageSummarySchema = z.object({
    imageId: z.string(),
    code: z.string(),
    description: z.string(),
});

export type ImageSummary = z.infer<typeof imageSummarySchema>;

const calendarDaySchema = z.object({
    date: z.string(),
    businessDay: z.boolean(),
    source: z.string(),
});

export type CalendarDay = z.infer<typeof calendarDaySchema>;

/** The amend reasons that can apply to a person's record, in the order they are offered. */
const PEOPLE_REASON_CODES = [
    'system.update',
    'common.rectification',
    'common.non_material_update',
    'common.regulatory',
    'common.other',
] as const;

const feedFolderSchema = z.object({
    id: z.string(),
    parent_id: z.string(),
    name: z.string(),
    kind: z.string(),
    scope: z.string(),
    provider_code: z.string(),
    binding_mode: z.string(),
    fx_feeds: z.number(),
    ir_feeds: z.number(),
    running_feeds: z.number(),
    idle_auto_feeds: z.number(),
    has_children: z.boolean(),
});

/** A folder of the feed hierarchy with the feeds under it and how many run. */
export type FeedFolder = z.infer<typeof feedFolderSchema>;

const folderFeedSchema = z.object({
    id: z.string(),
    kind: z.string(),
    label: z.string(),
    source_name: z.string(),
    oresmd_uri: z.string(),
    folder_id: z.string(),
    enabled: z.boolean(),
    provider_code: z.string(),
    asset_class_code: z.string(),
    underlying: z.string(),
    engine: z.string(),
    ticks_per_hour: z.number(),
    auto_start: z.boolean(),
    running: z.boolean(),
});

/** A feed as the feed table lists it: what names it, what it prices and how it runs. */
export type FolderFeed = z.infer<typeof folderFeedSchema>;

/** What narrows the feeds a screen lists; every part is optional. */
export interface FeedFilter {
    /** A folder, with the folders under it. */
    readonly folder?: string;
    readonly provider?: string;
    readonly assetClass?: string;
    readonly state?: 'all' | 'started' | 'stopped';
    /** Only the feeds that are enabled, start on their own, and are not running. */
    readonly shouldRun?: boolean;
    readonly search?: string;
}

/** The feeds to start or stop: those picked one by one, or the filter's less those left out. */
export interface FeedSelection extends FeedFilter {
    readonly feedIds?: readonly string[];
    readonly excludedIds?: readonly string[];
}

function filterQuery(filter: FeedFilter): URLSearchParams {
    const query = new URLSearchParams();
    const entries: readonly (readonly [string, string | undefined])[] = [
        ['folder', filter.folder],
        ['provider', filter.provider],
        ['assetClass', filter.assetClass],
        ['state', filter.state],
        ['search', filter.search],
    ];
    for (const [key, value] of entries) {
        if (value !== undefined && value !== '') {
            query.set(key, value);
        }
    }
    if (filter.shouldRun === true) {
        query.set('shouldRun', 'true');
    }
    return query;
}

function selectionBody(selection: FeedSelection): Record<string, unknown> {
    return {
        folder: selection.folder ?? '',
        provider: selection.provider ?? '',
        assetClass: selection.assetClass ?? '',
        state: selection.state ?? '',
        shouldRun: selection.shouldRun ?? false,
        search: selection.search ?? '',
        feedIds: selection.feedIds ?? [],
        excludedIds: selection.excludedIds ?? [],
    };
}

export const api = {
    /**
     * Whether the deployment still needs its first administrator.
     *
     * Asked before a session exists, because it decides what the interface can
     * offer: while the flag is set there are no accounts, so a sign-in form
     * would only be a door with nothing behind it.
     */
    async bootstrapStatus(): Promise<BootstrapStatus> {
        return bootstrapStatusSchema.parse(await request('/api/bootstrap', { method: 'GET' }));
    },

    /**
     * The environment this deployment serves, and what it says about itself.
     *
     * Read once for the whole page, because it does not change while a person
     * is looking at it: the environment is fixed when the process starts. The
     * footer states it beside the two builds, so a person can settle which
     * checkout they are pointed at, and whether it is production, without
     * asking anybody.
     */
    async site(): Promise<SiteState> {
        return siteStateSchema.parse(await request('/api/site', { method: 'GET' }));
    },

    /**
     * Creates the first administrator, which closes bootstrap mode.
     *
     * The one write sent with no session, because the deployment has no account
     * to sign in with. What comes back is the account rather than a session:
     * the person signs in with it next.
     */
    async createAdministrator(request_: CreateAdministratorRequest): Promise<InitialAdministrator> {
        const payload = await request('/api/bootstrap/administrator', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(request_),
        });
        return initialAdministratorSchema.parse(payload);
    },

    /**
     * Records that the first-run journey finished.
     *
     * The session names the tenant the write lands in, so the request carries
     * nothing. The setup screen calls it as its last act, so the gate sees a
     * finished installation on its next read even when the installation keeps no
     * tenant of its own.
     */
    async completeSystemOnboarding(): Promise<void> {
        await request('/api/bootstrap/complete', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({}),
        });
    },

    /**
     * The signed-in tenant's own setup run.
     *
     * The session names the tenant, so the request carries nothing. An empty
     * instance id is the answer when the tenant has no run to follow, which the
     * setup screen states rather than treating as a failure.
     */
    async tenantSetupRun(): Promise<TenantSetupRun> {
        return tenantSetupRunSchema.parse(await request('/api/tenant-setup', { method: 'GET' }));
    },

    async login(credentials: Credentials): Promise<LoginResult> {
        const payload = await request('/api/session', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(credentials),
        });
        return loginResultSchema.parse(payload);
    },

    async selectParty(partyId: string): Promise<SessionView> {
        const payload = await request('/api/session/party', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ partyId }),
        });
        return sessionViewSchema.parse(payload);
    },

    /** Returns null when no session is open, which is not an error. */
    async session(): Promise<SessionView | null> {
        try {
            return sessionViewSchema.parse(await request('/api/session', { method: 'GET' }));
        } catch (error) {
            if (error instanceof ApiFailure && error.status === 401) {
                return null;
            }
            throw error;
        }
    },

    async logout(): Promise<void> {
        await request('/api/session', { method: 'DELETE' });
    },

    /**
     * The rules a password must satisfy.
     *
     * Asked for before a session exists, because the screens that show the
     * rules are the ones a person signs in on and the one that creates the
     * first administrator. The answer is the server's own policy, so a screen
     * that shows it shows the rules the server applies.
     */
    /**
     * The accounts the tenant's administrator may see.
     *
     * The administrator's screen opens on this list, and the count travels with
     * it so the screen can say how many the tenant holds without reading them.
     */
    async accounts(): Promise<{
        readonly accounts: readonly Account[];
        readonly totalCount: number;
    }> {
        return z
            .object({ accounts: z.array(accountSchema), totalCount: z.int().nonnegative() })
            .parse(await request('/api/accounts', { method: 'GET' }));
    },

    /** One page of the tenant's people, searched and ordered on the server. */
    async accountsPage(page: {
        readonly offset: number;
        readonly limit: number;
        readonly search: string;
        readonly sort: string;
        readonly descending: boolean;
    }): Promise<{ readonly rows: readonly Account[]; readonly total: number }> {
        const query = new URLSearchParams({
            offset: String(page.offset),
            limit: String(page.limit),
            search: page.search,
            sort: page.sort,
            descending: String(page.descending),
        });
        const answer = z
            .object({ accounts: z.array(accountSchema), totalCount: z.int().nonnegative() })
            .parse(await request(`/api/accounts?${query.toString()}`, { method: 'GET' }));
        return { rows: answer.accounts, total: answer.totalCount };
    },

    /** The reasons a role may be given for: the access category, in display order. */
    async accessReasons(): Promise<
        readonly { readonly code: string; readonly description: string }[]
    > {
        const answer = z
            .object({
                reasons: z.array(
                    z.object({
                        code: z.string(),
                        description: z.string(),
                        categoryCode: z.string(),
                        appliesToNew: z.boolean(),
                        displayOrder: z.number(),
                    }),
                ),
            })
            .parse(await request('/api/change-reasons', { method: 'GET' }));
        return answer.reasons
            .filter((reason) => reason.categoryCode === 'access' && reason.appliesToNew)
            .sort((a, b) => a.displayOrder - b.displayOrder)
            .map(({ code, description }) => ({ code, description }));
    },

    /**
     * The reasons a correction or a removal of reference data may carry: the
     * common category, those that apply to the kind of write, in display
     * order. A new record needs no choice; it is written as a new record.
     */
    async referenceDataReasons(kind: 'amend' | 'delete'): Promise<
        readonly {
            readonly code: string;
            readonly description: string;
            readonly requiresCommentary: boolean;
        }[]
    > {
        const answer = z
            .object({
                reasons: z.array(
                    z.object({
                        code: z.string(),
                        description: z.string(),
                        categoryCode: z.string(),
                        appliesToAmend: z.boolean(),
                        appliesToDelete: z.boolean(),
                        requiresCommentary: z.boolean(),
                        displayOrder: z.number(),
                    }),
                ),
            })
            .parse(await request('/api/change-reasons', { method: 'GET' }));
        return answer.reasons
            .filter(
                (reason) =>
                    reason.categoryCode === 'common' &&
                    (kind === 'amend' ? reason.appliesToAmend : reason.appliesToDelete),
            )
            .sort((a, b) => a.displayOrder - b.displayOrder)
            .map(({ code, description, requiresCommentary }) => ({
                code,
                description,
                requiresCommentary,
            }));
    },

    /**
     * The reasons a person's record may be amended for: the common category, in display
     * order.
     *
     * A profile write changes a record rather than creating or deleting one,
     * so the reasons offered are the ones the catalogue marks for amendments.
     * The commentary flag travels too, because a reason that requires one
     * makes the screen ask for it before the server would refuse.
     */
    async amendReasons(): Promise<
        readonly {
            readonly code: string;
            readonly description: string;
            readonly requiresCommentary: boolean;
        }[]
    > {
        const answer = z
            .object({
                reasons: z.array(
                    z.object({
                        code: z.string(),
                        description: z.string(),
                        categoryCode: z.string(),
                        appliesToAmend: z.boolean(),
                        requiresCommentary: z.boolean(),
                        displayOrder: z.number(),
                    }),
                ),
            })
            .parse(await request('/api/change-reasons', { method: 'GET' }));
        /*
         * The catalogue's amend reasons cover market data and trades: a stale
         * feed, a mapping error, a vendor outage. A person's record is changed
         * for none of those, so only the reasons that can apply to one are
         * offered, in the order a person reaches for them.
         */
        return PEOPLE_REASON_CODES.flatMap((code) => {
            const reason = answer.reasons.find(
                (candidate) => candidate.code === code && candidate.appliesToAmend,
            );
            return reason === undefined
                ? []
                : [
                      {
                          code,
                          description: reason.description,
                          requiresCommentary: reason.requiresCommentary,
                      },
                  ];
        });
    },

    /** One account of the session's own tenant and a page of its sign-ins. */
    async accountSignIns(
        username: string,
        page: { readonly offset: number; readonly limit: number },
    ): Promise<AccountSignIns> {
        return accountSignInsSchema.parse(
            await request(
                `/api/accounts/${encodeURIComponent(username)}/sign-ins?${pageQuery(page)}`,
                { method: 'GET' },
            ),
        );
    },

    /** One account of a tenant read from system administration, and a page of its sign-ins. */
    async tenantAccountSignIns(
        code: string,
        username: string,
        page: { readonly offset: number; readonly limit: number },
    ): Promise<AccountSignIns> {
        return accountSignInsSchema.parse(
            await request(
                `/api/tenants/${encodeURIComponent(code)}/accounts/${encodeURIComponent(username)}/sign-ins?${pageQuery(page)}`,
                { method: 'GET' },
            ),
        );
    },

    /** The roles the signed-in person holds, with who gave each one and why. */
    async myAccess(): Promise<AccountAccess> {
        return accountAccessSchema.parse(await request('/api/me/access', { method: 'GET' }));
    },

    /** One page of what the signed-in person's roles let them do. */
    async myPermissions(query: PermissionPageQuery): Promise<PermissionPage> {
        return permissionPageSchema.parse(
            await request(`/api/me/permissions?${permissionParams(query)}`, { method: 'GET' }),
        );
    },

    /** One page of the tenant's roles, filtered and paged by the server. */
    async rolesPage(query: RolesPageQuery): Promise<RolePage> {
        return rolePageSchema.parse(
            await request(
                `/api/roles/page?${new URLSearchParams({
                    roleId: query.roleId ?? '',
                    search: query.search,
                    area: query.area,
                    includeService: String(query.includeService),
                    offset: String(query.offset),
                    limit: String(query.limit),
                }).toString()}`,
                { method: 'GET' },
            ),
        );
    },

    /** Tells the server which entities this session listens to. */
    async watchEntities(
        watches: readonly { readonly component: string; readonly entity: string }[],
    ): Promise<void> {
        await request('/api/events/watch', {
            method: 'POST',
            headers: { 'content-type': 'application/json' },
            body: JSON.stringify({ watches }),
        });
    },

    /** One page of the people who hold a role, by name. */
    async roleHolders(
        roleId: string,
        page: { readonly offset: number; readonly limit: number },
    ): Promise<RoleHolders> {
        return roleHoldersSchema.parse(
            await request(`/api/roles/${encodeURIComponent(roleId)}/holders?${pageQuery(page)}`, {
                method: 'GET',
            }),
        );
    },

    /** One page of the catalogue against what one role grants, for the editor. */
    async rolePermissions(roleId: string, query: RolePermissionPageQuery): Promise<PermissionPage> {
        return permissionPageSchema.parse(
            await request(
                `/api/roles/${encodeURIComponent(roleId)}/permissions?${permissionParams(query)}&includeUnheld=${String(query.includeUnheld)}`,
                { method: 'GET' },
            ),
        );
    },

    /** Adds and removes permissions from a role, for a reason. */
    async changeRolePermissions(
        roleId: string,
        change: { readonly add: readonly string[]; readonly remove: readonly string[] },
        note: string,
    ): Promise<void> {
        await request(`/api/roles/${encodeURIComponent(roleId)}/permissions/changes`, {
            method: 'POST',
            body: JSON.stringify({ ...change, note }),
        });
    },

    /** One page of what one account's roles let it do. */
    async accountPermissions(
        accountId: string,
        query: PermissionPageQuery,
    ): Promise<PermissionPage> {
        return permissionPageSchema.parse(
            await request(
                `/api/accounts/${encodeURIComponent(accountId)}/permissions?${permissionParams(query)}`,
                { method: 'GET' },
            ),
        );
    },

    /** The parties the signed-in person works in, each with its name. */
    async myParties(): Promise<MyParties> {
        return myPartiesSchema.parse(await request('/api/me/parties', { method: 'GET' }));
    },

    /**
     * Sets or clears who one account reports to. The write names one field, so
     * a field changed elsewhere is not overwritten, and it states the version
     * the screen read. The server needs iam::accounts:update.
     */
    async setReportingLine(accountId: string, write: ReportingLineWrite): Promise<Account> {
        return accountSchema.parse(
            await request(`/api/accounts/${encodeURIComponent(accountId)}/reporting-line`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify(write),
            }),
        );
    },

    /**
     * The tenant's reporting shape, or one account's branch of it. The server
     * needs iam::accounts:read.
     */
    async reportingTree(rootAccountId = ''): Promise<ReportingTree> {
        const query = rootAccountId === '' ? '' : `?root=${encodeURIComponent(rootAccountId)}`;
        return reportingTreeSchema.parse(
            await request(`/api/reporting-tree${query}`, { method: 'GET' }),
        );
    },

    /**
     * Sets or clears the party quick sign-in uses. An empty id clears it, and
     * the server refuses a party the account does not work in.
     */
    async setMyDefaultParty(partyId: string): Promise<void> {
        await request('/api/me/default-party', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ partyId }),
        });
    },

    /** The roles one account holds. */
    async accountAccess(accountId: string): Promise<AccountAccess> {
        return accountAccessSchema.parse(
            await request(`/api/accounts/${encodeURIComponent(accountId)}/access`, {
                method: 'GET',
            }),
        );
    },

    /** Gives a role to an account, for a reason from the access category. */
    async giveRole(
        accountId: string,
        input: { readonly roleId: string; readonly reasonCode: string; readonly note: string },
    ): Promise<void> {
        await request(`/api/accounts/${encodeURIComponent(accountId)}/roles`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** Takes a role away from an account. */
    async takeRoleAway(accountId: string, roleId: string): Promise<void> {
        await request(
            `/api/accounts/${encodeURIComponent(accountId)}/roles/${encodeURIComponent(roleId)}`,
            { method: 'DELETE' },
        );
    },

    /**
     * The signed-in person's own approval requests, newest first.
     *
     * The roles a request asks for are joined from the account list, which a
     * member may not read: the queue resolves them, and a member's own answer
     * comes back with `roles` empty.
     */
    async myRequests(page: {
        readonly offset: number;
        readonly limit: number;
    }): Promise<InboxPage<InboxRequestView>> {
        return inboxRequestPageSchema.parse(
            await request(`/api/me/requests?${pageQuery(page)}`, { method: 'GET' }),
        );
    },

    /** Asks for roles, and answers the request raised. */
    async askForRoles(input: {
        readonly roleIds: readonly string[];
        readonly reason: string;
    }): Promise<string> {
        return z.object({ requestId: z.string() }).parse(
            await request('/api/me/requests', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(input),
            }),
        ).requestId;
    },

    /**
     * Takes back one of the signed-in person's own requests.
     *
     * The version they read travels as the claim, so a queue that moved on is
     * refused by the server rather than silently discarded.
     */
    async withdrawRequest(
        requestId: string,
        input: { readonly version: number; readonly comment: string },
    ): Promise<void> {
        await request(`/api/me/requests/${encodeURIComponent(requestId)}`, {
            method: 'DELETE',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /**
     * The requests waiting to be decided, oldest first, and the ones answered
     * within the installation's window, newest answer first.
     */
    async requestQueue(page: {
        readonly offset: number;
        readonly limit: number;
    }): Promise<InboxRequestQueue> {
        return inboxRequestQueueSchema.parse(
            await request(`/api/requests?${pageQuery(page)}`, { method: 'GET' }),
        );
    },

    /**
     * The one request an identifier names, when the signed-in person may open
     * it.
     *
     * A notice carries the request it is about, and is read after the request
     * stopped waiting, so this is a read of its own rather than a lookup in
     * the queue. Null means the request is not there, or that the server will
     * not say that it is.
     */
    async request(id: string): Promise<InboxRequestView | null> {
        const body = await request(`/api/requests/${encodeURIComponent(id)}`, {
            method: 'GET',
        });
        return body === null ? null : inboxRequestViewSchema.parse(body);
    },

    /**
     * The whole story of one request, newest first.
     *
     * Every row any component wrote for it, merged by the server: the request's
     * versions, the answers given on it, the roles it asked for and what became
     * of them, and the notices this person was given. A request they may not
     * open answers empty, which is what opening it answers too.
     */
    async requestStory(id: string): Promise<InboxRequestStory> {
        return inboxRequestStorySchema.parse(
            await request(`/api/requests/${encodeURIComponent(id)}/story`, { method: 'GET' }),
        );
    },

    /**
     * What a request proposes, for any kind: the operation, the thing it is
     * about, and each field beside the value it would replace.
     */
    async requestChanges(id: string): Promise<readonly RequestChange[]> {
        return requestChangesViewSchema.parse(
            await request(`/api/requests/${encodeURIComponent(id)}/changes`, { method: 'GET' }),
        ).changes;
    },

    /** What running the request's lines through the real write now says. */
    async requestChecks(id: string): Promise<RequestChecks> {
        return requestChecksViewSchema.parse(
            await request(`/api/requests/${encodeURIComponent(id)}/checks`, { method: 'GET' }),
        );
    },

    /**
     * Runs the proposed book writes through the real service and writes
     * nothing, so every refusal and the parts the policy needs come back before
     * anyone waits.
     */
    async previewBookChanges(lines: readonly BookLineInput[]): Promise<BookPreview> {
        return bookPreviewViewSchema.parse(
            await request('/api/book-changes/preview', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify({ lines }),
            }),
        );
    },

    /**
     * Raises one request that holds the proposed book writes. A refusal comes
     * back as an answer with its reason and no request.
     */
    async raiseBookChanges(input: {
        readonly reason: string;
        readonly lines: readonly BookLineInput[];
    }): Promise<BookRaise> {
        return bookRaiseViewSchema.parse(
            await request('/api/book-changes', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(input),
            }),
        );
    },

    /** Approves, refuses, holds or resumes a request, against the version read. */
    async decideRequest(
        requestId: string,
        input: {
            readonly version: number;
            readonly decisionCode: 'approve' | 'refuse' | 'hold' | 'resume';
            readonly comment: string;
            /** The part the decider answers for; omitted for a request that names none. */
            readonly partCode?: string;
        },
    ): Promise<void> {
        await request(`/api/requests/${encodeURIComponent(requestId)}/decision`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** The signed-in person's notifications, newest first. */
    async myNotifications(query: {
        readonly unreadOnly: boolean;
        readonly offset: number;
        readonly limit: number;
    }): Promise<InboxPage<InboxNotificationView>> {
        const params = new URLSearchParams({
            unreadOnly: String(query.unreadOnly),
            offset: String(query.offset),
            limit: String(query.limit),
        });
        return inboxNotificationPageSchema.parse(
            await request(`/api/me/notifications?${params.toString()}`, { method: 'GET' }),
        );
    },

    /** How many notifications are unread, for the bell on every screen. */
    async unreadNotificationCount(): Promise<number> {
        return z
            .object({ unread: z.int().nonnegative() })
            .parse(await request('/api/me/notifications/unread-count', { method: 'GET' })).unread;
    },

    /** Marks notifications read. An empty list marks every unread one. */
    async markNotificationsRead(ids: readonly string[]): Promise<number> {
        return z.object({ marked: z.int().nonnegative() }).parse(
            await request('/api/me/notifications/read', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify({ ids }),
            }),
        ).marked;
    },

    /** Removes notifications from the person's list. An empty list clears the read ones. */
    async clearNotifications(ids: readonly string[]): Promise<number> {
        return z.object({ cleared: z.int().nonnegative() }).parse(
            await request('/api/me/notifications/clear', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify({ ids }),
            }),
        ).cleared;
    },

    /** The classification lists, by topic, with each list's columns and row count. */
    async classificationLists(): Promise<
        readonly (ClassificationList & { readonly count: number | null })[]
    > {
        return z
            .object({
                lists: z.array(
                    classificationListSchema.extend({ count: z.int().nonnegative().nullable() }),
                ),
            })
            .parse(await request('/api/classifications', { method: 'GET' })).lists;
    },

    /** The reference data resources, with their key fields and the permissions each needs. */
    async refdataRegistry(): Promise<readonly RecordResourceView[]> {
        return z
            .object({ resources: z.array(recordResourceViewSchema) })
            .parse(await request('/api/refdata', { method: 'GET' })).resources;
    },

    /** Every row of a reference data resource, or one parent's rows of a junction. */
    async records(resource: string, parent?: string): Promise<readonly RecordRow[]> {
        const path =
            parent === undefined
                ? `/api/refdata/${encodeURIComponent(resource)}`
                : `/api/refdata/${encodeURIComponent(resource)}/by/${encodeURIComponent(parent)}`;
        return z
            .object({ rows: z.array(recordRowSchema) })
            .parse(await request(path, { method: 'GET' })).rows;
    },

    /** One page of a resource and the server's total, searched and ordered on the server. */
    async recordPage(
        resource: string,
        page: {
            readonly offset: number;
            readonly limit: number;
            readonly search: string;
            readonly sort: string;
            readonly descending: boolean;
        },
    ): Promise<{ readonly rows: readonly RecordRow[]; readonly total: number }> {
        const query = new URLSearchParams({
            offset: String(page.offset),
            limit: String(page.limit),
            search: page.search,
            sort: page.sort,
            descending: String(page.descending),
        });
        return z.object({ rows: z.array(recordRowSchema), total: z.int().nonnegative() }).parse(
            await request(`/api/refdata/${encodeURIComponent(resource)}?${query.toString()}`, {
                method: 'GET',
            }),
        );
    },

    /** Which image each flagged code uses, for the whole tenant. */
    async imageMap(): Promise<ImageMap> {
        return imageMapSchema.parse(await request('/api/image-map', { method: 'GET' }));
    },

    /** One page of the tenant's images, for a chooser, searched on the server. */
    async imageSummaries(page: {
        readonly offset: number;
        readonly limit: number;
        readonly search: string;
    }): Promise<{ readonly images: readonly ImageSummary[]; readonly total: number }> {
        const query = new URLSearchParams({
            offset: String(page.offset),
            limit: String(page.limit),
            search: page.search,
        });
        return z
            .object({ images: z.array(imageSummarySchema), total: z.int().nonnegative() })
            .parse(await request(`/api/images?${query.toString()}`, { method: 'GET' }));
    },

    /** One record, named by its key. */
    async record(resource: string, key: string): Promise<RecordRow> {
        return z
            .object({ row: recordRowSchema })
            .parse(
                await request(
                    `/api/refdata/${encodeURIComponent(resource)}/key/${encodeURIComponent(key)}`,
                    { method: 'GET' },
                ),
            ).row;
    },

    /** Writes one row: a new row with no version, else a correction of the version read. */
    async saveRecord(
        resource: string,
        input: {
            readonly write: Readonly<Record<string, unknown>>;
            readonly version: number | null;
            readonly reasonCode: string;
            readonly commentary: string;
        },
    ): Promise<void> {
        await request(`/api/refdata/${encodeURIComponent(resource)}`, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** Removes one row, named by its key fields. */
    async removeRecord(
        resource: string,
        input: {
            readonly key: Readonly<Record<string, string>>;
            readonly version?: number | null;
            readonly reasonCode: string;
            readonly commentary: string;
        },
    ): Promise<void> {
        await request(`/api/refdata/${encodeURIComponent(resource)}`, {
            method: 'DELETE',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** The synthetic resources, with what each searches and sorts and the permissions each needs. */
    async syntheticRegistry(): Promise<readonly SyntheticResourceView[]> {
        return z
            .object({ resources: z.array(syntheticResourceViewSchema) })
            .parse(await request('/api/synthetic', { method: 'GET' })).resources;
    },

    /** Every row of a synthetic resource, or the rows of one parent, such as one feed's mixture. */
    async syntheticRecords(resource: string, parent?: string): Promise<readonly RecordRow[]> {
        const path =
            parent === undefined
                ? `/api/synthetic/${encodeURIComponent(resource)}`
                : `/api/synthetic/${encodeURIComponent(resource)}/by/${encodeURIComponent(parent)}`;
        return z
            .object({ rows: z.array(recordRowSchema) })
            .parse(await request(path, { method: 'GET' })).rows;
    },

    /** One page of a synthetic resource and the server's total, searched and ordered on the server. */
    async syntheticPage(
        resource: string,
        page: {
            readonly offset: number;
            readonly limit: number;
            readonly search: string;
            readonly sort: string;
            readonly descending: boolean;
        },
    ): Promise<{ readonly rows: readonly RecordRow[]; readonly total: number }> {
        const query = new URLSearchParams({
            offset: String(page.offset),
            limit: String(page.limit),
            search: page.search,
            sort: page.sort,
            descending: String(page.descending),
        });
        return z.object({ rows: z.array(recordRowSchema), total: z.int().nonnegative() }).parse(
            await request(`/api/synthetic/${encodeURIComponent(resource)}?${query.toString()}`, {
                method: 'GET',
            }),
        );
    },

    /** One synthetic row, named by its key. */
    async syntheticRecord(resource: string, key: string): Promise<RecordRow> {
        return z
            .object({ row: recordRowSchema })
            .parse(
                await request(
                    `/api/synthetic/${encodeURIComponent(resource)}/key/${encodeURIComponent(key)}`,
                    { method: 'GET' },
                ),
            ).row;
    },

    /**
     * Writes one or many synthetic rows in one call: each with the version it
     * was read at, or none for a new row. The server writes all of them or none.
     */
    async saveSynthetic(
        resource: string,
        changes: readonly {
            readonly write: Readonly<Record<string, unknown>>;
            readonly version: number | null;
        }[],
        intent: { readonly reasonCode: string; readonly commentary: string },
    ): Promise<void> {
        await request(`/api/synthetic/${encodeURIComponent(resource)}`, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify({
                changes,
                intent: { reason_code: intent.reasonCode, commentary: intent.commentary },
            }),
        });
    },

    /**
     * Removes synthetic rows by key. One removal may name the version it read;
     * a removal of many rows is unconditional and names none.
     */
    async removeSynthetic(
        resource: string,
        removals: readonly { readonly key: string; readonly version: number | null }[],
        intent: { readonly reasonCode: string; readonly commentary: string },
    ): Promise<void> {
        await request(`/api/synthetic/${encodeURIComponent(resource)}`, {
            method: 'DELETE',
            headers: JSON_HEADERS,
            body: JSON.stringify({
                removals,
                intent: { reason_code: intent.reasonCode, commentary: intent.commentary },
            }),
        });
    },

    /** Sample FX spot paths from the values on a screen; nothing is saved. */
    async simulateFx(input: {
        readonly gmm_means: readonly number[];
        readonly gmm_stdevs: readonly number[];
        readonly gmm_weights: readonly number[];
        readonly process_type: string;
        readonly initial_price: number;
        readonly num_ticks: number;
        readonly num_paths: number;
        readonly seed: number;
    }): Promise<readonly (readonly number[])[]> {
        return z.object({ paths: z.array(z.array(z.number())) }).parse(
            await request('/api/synthetic/simulate/fx', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(input),
            }),
        ).paths;
    },

    /** Sample short-rate paths from the engine and parameters on a screen; nothing is saved. */
    async simulateIr(input: {
        readonly process_type: string;
        readonly parameters: readonly {
            readonly parameter_name: string;
            readonly parameter_value: number;
        }[];
        readonly num_ticks: number;
        readonly num_paths: number;
        readonly seed: number;
    }): Promise<readonly (readonly number[])[]> {
        return z.object({ paths: z.array(z.array(z.number())) }).parse(
            await request('/api/synthetic/simulate/ir', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(input),
            }),
        ).paths;
    },

    /** The rate at each row of a curve template, priced from the values on a screen; nothing is saved. */
    async previewIrShape(input: {
        readonly process_type: string;
        readonly parameters: readonly {
            readonly parameter_name: string;
            readonly parameter_value: number;
        }[];
        readonly fixed_leg_payment_frequency_code: string;
        readonly entries: readonly {
            readonly sequence_index: number;
            readonly start_tenor_code: string;
            readonly end_tenor_code: string;
            readonly instrument_code: string;
        }[];
    }): Promise<
        readonly {
            readonly sequence_index: number;
            readonly start_tenor_code: string;
            readonly end_tenor_code: string;
            readonly rate: number;
        }[]
    > {
        return z
            .object({
                points: z.array(
                    z.object({
                        sequence_index: z.number(),
                        start_tenor_code: z.string(),
                        end_tenor_code: z.string(),
                        rate: z.number(),
                    }),
                ),
            })
            .parse(
                await request('/api/synthetic/preview/ir-shape', {
                    method: 'POST',
                    headers: JSON_HEADERS,
                    body: JSON.stringify(input),
                }),
            ).points;
    },

    /** The mixture an engine change leaves, as the synthetic service converts it. */
    async convertFxMixture(input: {
        readonly gmm_means: readonly number[];
        readonly gmm_stdevs: readonly number[];
        readonly gmm_weights: readonly number[];
        readonly from_process_type: string;
        readonly to_process_type: string;
        readonly initial_price: number;
        readonly ticks_per_hour: number;
    }): Promise<{
        readonly gmm_means: readonly number[];
        readonly gmm_stdevs: readonly number[];
        readonly gmm_weights: readonly number[];
    }> {
        return z
            .object({
                gmm_means: z.array(z.number()),
                gmm_stdevs: z.array(z.number()),
                gmm_weights: z.array(z.number()),
            })
            .parse(
                await request('/api/synthetic/convert/fx', {
                    method: 'POST',
                    headers: JSON_HEADERS,
                    body: JSON.stringify(input),
                }),
            );
    },

    /**
     * An FX mixture read as the simple view, or the simple view written as a
     * mixture, as the synthetic service converts it. Every figure is per hour.
     */
    async convertFxSimple(input: {
        readonly process_type: string;
        readonly to_simple: boolean;
        readonly initial_price: number;
        readonly ticks_per_hour: number;
        readonly gmm_means?: readonly number[];
        readonly gmm_stdevs?: readonly number[];
        readonly gmm_weights?: readonly number[];
        readonly trend_pct_per_hour?: number;
        readonly volatility_pct_per_hour?: number;
        readonly jump_share_pct?: number;
        readonly half_life_minutes?: number;
    }): Promise<{
        readonly has_simple_form: boolean;
        readonly message: string;
        readonly trend_pct_per_hour: number;
        readonly volatility_pct_per_hour: number;
        readonly jump_share_pct: number;
        readonly half_life_minutes: number;
        readonly gmm_means: readonly number[];
        readonly gmm_stdevs: readonly number[];
        readonly gmm_weights: readonly number[];
    }> {
        return z
            .object({
                has_simple_form: z.boolean(),
                message: z.string(),
                trend_pct_per_hour: z.number(),
                volatility_pct_per_hour: z.number(),
                jump_share_pct: z.number(),
                half_life_minutes: z.number(),
                gmm_means: z.array(z.number()),
                gmm_stdevs: z.array(z.number()),
                gmm_weights: z.array(z.number()),
            })
            .parse(
                await request('/api/synthetic/convert/fx-simple', {
                    method: 'POST',
                    headers: JSON_HEADERS,
                    body: JSON.stringify(input),
                }),
            );
    },

    /**
     * An IR engine's parameters read as the simple view, or the simple view
     * written as the engine's parameters, as the synthetic service converts
     * them. Rates are decimals.
     */
    async convertIrParameters(input: {
        readonly process_type: string;
        readonly to_simple: boolean;
        readonly values: Readonly<Record<string, number>>;
    }): Promise<{
        readonly has_simple_form: boolean;
        readonly message: string;
        readonly caution: string;
        readonly values: Readonly<Record<string, number>>;
    }> {
        return z
            .object({
                has_simple_form: z.boolean(),
                message: z.string(),
                caution: z.string(),
                values: z.record(z.string(), z.number()),
            })
            .parse(
                await request('/api/synthetic/convert/ir', {
                    method: 'POST',
                    headers: JSON_HEADERS,
                    body: JSON.stringify(input),
                }),
            );
    },

    /** Stops one feed. A feed that is not running stops nothing. */
    async stopFeed(id: string): Promise<void> {
        await request(`/api/synthetic/feeds/${encodeURIComponent(id)}/stop`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: '{}',
        });
    },

    /** Starts one feed. The server refuses a feed whose source or pair another running feed holds. */
    async startFeed(id: string): Promise<void> {
        await request(`/api/synthetic/feeds/${encodeURIComponent(id)}/start`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: '{}',
        });
    },

    /** The source names of the feeds running now. */
    async runningFeeds(): Promise<readonly string[]> {
        return z
            .object({ sourceNames: z.array(z.string()) })
            .parse(await request('/api/synthetic/running', { method: 'GET' })).sourceNames;
    },

    /**
     * The children of one folder, or the roots when none is named, each with the
     * feeds under it at any depth and how many run. The service counts them.
     */
    async feedFolders(parent = ''): Promise<readonly FeedFolder[]> {
        return z
            .object({ folders: z.array(feedFolderSchema) })
            .parse(
                await request(
                    `/api/synthetic/feed-folders?${new URLSearchParams(
                        parent === '' ? {} : { parent },
                    ).toString()}`,
                    { method: 'GET' },
                ),
            ).folders;
    },

    /**
     * A page of the feeds a filter matches, with how many match in all. The service
     * pages and counts, so no screen reads the feeds to count them.
     */
    async folderFeeds(
        filter: FeedFilter,
        page: { readonly offset: number; readonly limit: number },
    ): Promise<{ readonly total: number; readonly feeds: readonly FolderFeed[] }> {
        const query = filterQuery(filter);
        query.set('offset', String(page.offset));
        query.set('limit', String(page.limit));
        return z.object({ total: z.number(), feeds: z.array(folderFeedSchema) }).parse(
            await request(`/api/synthetic/folder-feeds?${query.toString()}`, {
                method: 'GET',
            }),
        );
    },

    /** Starts the picked feeds, or the feeds a filter matches, and says what it did. */
    async startSelection(selection: FeedSelection): Promise<{
        readonly message: string;
        readonly started: number;
        readonly alreadyRunning: number;
        readonly skipped: number;
    }> {
        return z
            .object({
                message: z.string(),
                started: z.number(),
                alreadyRunning: z.number(),
                skipped: z.number(),
            })
            .parse(
                await request('/api/synthetic/feed-selection/start', {
                    method: 'POST',
                    headers: JSON_HEADERS,
                    body: JSON.stringify(selectionBody(selection)),
                }),
            );
    },

    /** Stops the picked feeds, or the feeds a filter matches, and says what it did. */
    async stopSelection(
        selection: FeedSelection,
    ): Promise<{ readonly message: string; readonly stopped: number }> {
        return z.object({ message: z.string(), stopped: z.number() }).parse(
            await request('/api/synthetic/feed-selection/stop', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(selectionBody(selection)),
            }),
        );
    },

    /** The newest ticks the BFF heard from one feed, oldest first. */
    async feedTicks(
        id: string,
        limit: number,
        resource: 'fx-feeds' | 'ir-curve-feeds' = 'fx-feeds',
    ): Promise<{
        readonly sourceName: string;
        readonly ticks: readonly {
            readonly at: string;
            readonly value: number;
            readonly uri: string;
        }[];
    }> {
        return z
            .object({
                sourceName: z.string(),
                ticks: z.array(
                    z.object({ at: z.string(), value: z.number(), uri: z.string().default('') }),
                ),
            })
            .parse(
                await request(
                    `/api/synthetic/feeds/${encodeURIComponent(id)}/ticks?${new URLSearchParams({
                        limit: String(limit),
                        resource,
                    }).toString()}`,
                    { method: 'GET' },
                ),
            );
    },

    /** The materialised days of one calendar in one year. */
    async calendarDays(calendar: string, year: number): Promise<readonly CalendarDay[]> {
        return z
            .object({ days: z.array(calendarDaySchema) })
            .parse(
                await request(
                    `/api/refdata/calendars/${encodeURIComponent(calendar)}/days?year=${String(year)}`,
                    { method: 'GET' },
                ),
            ).days;
    },

    /** Builds the business days of one calendar up to a year; answers the days written. */
    async rebuildCalendar(calendar: string, endYear: number): Promise<number> {
        return z.object({ written: z.int() }).parse(
            await request(`/api/refdata/calendars/${encodeURIComponent(calendar)}/rebuild`, {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify({ endYear }),
            }),
        ).written;
    },

    /** The shared label catalogue: every label, and the labels each code domain uses. */
    async labels(): Promise<{
        readonly labels: readonly BadgePresentation[];
        readonly domains: Readonly<Record<string, readonly string[]>>;
    }> {
        return z
            .object({
                labels: z.array(badgePresentationSchema),
                domains: z.record(z.string(), z.array(z.string())),
            })
            .parse(await request('/api/labels', { method: 'GET' }));
    },

    /** Gives a row a label from the catalogue, or takes it away with null. */
    async setClassificationLabel(
        list: string,
        code: string,
        input: {
            readonly badgeCode: string | null;
            readonly reasonCode: string;
            readonly commentary: string;
        },
    ): Promise<void> {
        await request(
            `/api/classifications/${encodeURIComponent(list)}/rows/${encodeURIComponent(code)}/label`,
            { method: 'PUT', headers: JSON_HEADERS, body: JSON.stringify(input) },
        );
    },

    /** Every row of one classification list. */
    async classificationRows(list: string): Promise<readonly ClassificationRow[]> {
        return z.object({ rows: z.array(classificationRowSchema) }).parse(
            await request(`/api/classifications/${encodeURIComponent(list)}`, {
                method: 'GET',
            }),
        ).rows;
    },

    /** Adds a row to a classification list. */
    async addClassificationRow(
        list: string,
        input: {
            readonly code: string;
            readonly name: string;
            readonly description: string;
            readonly displayOrder: number | null;
            readonly reasonCode: string;
            readonly commentary: string;
        },
    ): Promise<void> {
        await request(`/api/classifications/${encodeURIComponent(list)}/rows`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** Corrects a row, against the version the screen read. */
    async correctClassificationRow(
        list: string,
        code: string,
        input: {
            readonly name: string;
            readonly description: string;
            readonly displayOrder: number | null;
            readonly version: number;
            readonly reasonCode: string;
            readonly commentary: string;
        },
    ): Promise<void> {
        await request(
            `/api/classifications/${encodeURIComponent(list)}/rows/${encodeURIComponent(code)}`,
            { method: 'PUT', headers: JSON_HEADERS, body: JSON.stringify(input) },
        );
    },

    /** Writes the new display order of the rows that moved. */
    async reorderClassificationRows(
        list: string,
        input: {
            readonly rows: readonly {
                readonly code: string;
                readonly name: string;
                readonly description: string;
                readonly displayOrder: number;
                readonly version: number;
            }[];
            readonly reasonCode: string;
            readonly commentary: string;
        },
    ): Promise<void> {
        await request(`/api/classifications/${encodeURIComponent(list)}/order`, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** Removes a row. Its versions stay in the history. */
    async removeClassificationRow(
        list: string,
        code: string,
        input: { readonly reasonCode: string; readonly commentary: string },
    ): Promise<void> {
        await request(
            `/api/classifications/${encodeURIComponent(list)}/rows/${encodeURIComponent(code)}`,
            { method: 'DELETE', headers: JSON_HEADERS, body: JSON.stringify(input) },
        );
    },

    /** Every version of one record, newest first, with what changed in each. */
    async history(entityType: string, entityId: string): Promise<readonly HistoryVersion[]> {
        const query = new URLSearchParams({ entityType, entityId });
        return z
            .object({ versions: z.array(historyVersionSchema) })
            .parse(await request(`/api/history?${query.toString()}`, { method: 'GET' })).versions;
    },

    /**
     * One subject's whole story, newest first.
     *
     * The subject is what a screen knows: a person it is looking at, or a
     * request it opened. Which rows make up the story is the server's to
     * decide, so the browser never names an entity type here.
     */
    async timeline(subject: 'person' | 'party' | 'request', id: string): Promise<Timeline> {
        const query = new URLSearchParams({ subject, id });
        const answer = await request(`/api/timeline?${query.toString()}`, { method: 'GET' });
        return timelineSchema.parse(answer);
    },

    /** The tenant's roles, each with the permissions it grants. */
    async roles(): Promise<readonly RoleSummary[]> {
        return z
            .object({ roles: z.array(roleSummarySchema) })
            .parse(await request('/api/roles', { method: 'GET' })).roles;
    },

    /** The roles that could be asked for to hold a permission, less the ones already held. */
    async waysToHold(
        permission: string,
    ): Promise<
        readonly { readonly id: string; readonly name: string; readonly description: string }[]
    > {
        return z
            .object({
                roles: z.array(
                    z.object({ id: z.string(), name: z.string(), description: z.string() }),
                ),
            })
            .parse(
                await request(`/api/me/ways-to-hold?permission=${encodeURIComponent(permission)}`, {
                    method: 'GET',
                }),
            ).roles;
    },

    /** Every permission the platform defines. */
    async permissions(): Promise<readonly PermissionEntry[]> {
        return z
            .object({ permissions: z.array(permissionEntrySchema) })
            .parse(await request('/api/permissions', { method: 'GET' })).permissions;
    },

    /** Creates a role that grants nothing yet, and answers its identifier. */
    async createRole(input: {
        readonly name: string;
        readonly description: string;
    }): Promise<string> {
        return z.object({ id: z.string() }).parse(
            await request('/api/roles', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(input),
            }),
        ).id;
    },

    /** Renames or redescribes a role, against the version the screen read. */
    async updateRole(
        roleId: string,
        input: {
            readonly name: string;
            readonly description: string;
            readonly version: number;
            readonly registrationDefault: boolean;
            readonly requestable: boolean;
        },
    ): Promise<void> {
        await request(`/api/roles/${encodeURIComponent(roleId)}`, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
    },

    /** Replaces what a role grants, and answers the codes as stored. */
    async saveRolePermissions(
        roleId: string,
        codes: readonly string[],
        note: string,
    ): Promise<readonly string[]> {
        return z.object({ codes: z.array(z.string()) }).parse(
            await request(`/api/roles/${encodeURIComponent(roleId)}/permissions`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({ codes, note }),
            }),
        ).codes;
    },

    /** Deletes a role by its name. */
    async deleteRole(name: string): Promise<void> {
        await request(`/api/roles/${encodeURIComponent(name)}`, { method: 'DELETE' });
    },

    /**
     * One account by username, or nothing when the tenant has none with it.
     *
     * The caller names a row it has just read out of the list, so the row
     * having gone is an expected answer and not a failure.
     */
    async account(username: string): Promise<Account | null> {
        const answer = z
            .object({ account: accountSchema.nullable() })
            .parse(
                await request(`/api/accounts/${encodeURIComponent(username)}`, { method: 'GET' }),
            );
        return answer.account;
    },

    /**
     * One account's login record, or nothing when it has never signed in.
     *
     * A record is written by signing in, so an account that has never done so
     * has none, and the screen says that rather than showing zeros.
     */
    async loginInfo(accountId: string): Promise<LoginInfo | null> {
        const answer = z.object({ loginInfo: loginInfoSchema.nullable() }).parse(
            await request(`/api/login-info/${encodeURIComponent(accountId)}`, {
                method: 'GET',
            }),
        );
        return answer.loginInfo;
    },

    /** The tenant's login records, one page at a time. */
    async loginInfoPage(page?: { readonly limit: number }): Promise<{
        readonly loginInfo: readonly LoginInfo[];
        readonly totalCount: number;
    }> {
        const query =
            page === undefined
                ? ''
                : `?${new URLSearchParams({ limit: String(page.limit) }).toString()}`;
        return z
            .object({ loginInfo: z.array(loginInfoSchema), totalCount: z.int().nonnegative() })
            .parse(await request(`/api/login-info${query}`, { method: 'GET' }));
    },

    /** The tenant's sessions, one page at a time. */
    async sessions(): Promise<{
        readonly sessions: readonly Session[];
        readonly totalCount: number;
    }> {
        return z
            .object({ sessions: z.array(sessionSchema), totalCount: z.int().nonnegative() })
            .parse(await request('/api/sessions', { method: 'GET' }));
    },

    /** The signed-in person's own open sessions. */
    async mySessions(): Promise<readonly Session[]> {
        const answer = z
            .object({ sessions: z.array(sessionSchema) })
            .parse(await request('/api/me/sessions', { method: 'GET' }));
        return answer.sessions;
    },

    /** The tenant's open sessions, every account's: the administrator's audit. */
    async activeSessions(): Promise<readonly Session[]> {
        const answer = z
            .object({ sessions: z.array(sessionSchema) })
            .parse(await request('/api/sessions/active', { method: 'GET' }));
        return answer.sessions;
    },

    /**
     * One page of the tenant's authentication events, newest first.
     *
     * The period travels as a preset rather than as two instants, because the
     * window is the deployment's to compute: a browser that sent its own times
     * would state a window the deployment never measured.
     */
    async authEvents(query: {
        readonly period: AuditPeriod;
        readonly eventType: string;
        readonly offset: number;
        readonly limit: number;
    }): Promise<readonly AuthEvent[]> {
        const params = new URLSearchParams({
            period: query.period,
            offset: String(query.offset),
            limit: String(query.limit),
        });
        if (query.eventType !== '') {
            params.set('eventType', query.eventType);
        }
        return z
            .object({ events: z.array(authEventSchema) })
            .parse(await request(`/api/auth-events?${params.toString()}`, { method: 'GET' }))
            .events;
    },

    /** One page of the tenant's session statistics, newest day first. */
    async sessionStatistics(query: {
        readonly period: AuditPeriod;
        readonly offset: number;
        readonly limit: number;
    }): Promise<readonly SessionStatisticsRow[]> {
        const params = new URLSearchParams({
            period: query.period,
            offset: String(query.offset),
            limit: String(query.limit),
        });
        return z
            .object({ rows: z.array(sessionStatisticsSchema) })
            .parse(await request(`/api/session-statistics?${params.toString()}`, { method: 'GET' }))
            .rows;
    },

    /** Ends one session of the caller's tenant. */
    async endSession(sessionId: string): Promise<void> {
        await request(`/api/sessions/${encodeURIComponent(sessionId)}/end`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({}),
        });
    },

    /**
     * Locks or unlocks one account.
     *
     * The administrator acts on the account the screen is showing, so the id
     * travels and the server's answer is a refusal the screen renders rather
     * than a list of per-account results it would have to collapse itself.
     */
    async setAccountLocked(accountId: string, locked: boolean): Promise<void> {
        await request(
            `/api/accounts/${encodeURIComponent(accountId)}/${locked ? 'lock' : 'unlock'}`,
            { method: 'POST', headers: JSON_HEADERS },
        );
    },

    async passwordPolicy(): Promise<PasswordPolicy> {
        return passwordPolicySchema.parse(await request('/api/password-policy', { method: 'GET' }));
    },

    /**
     * What the deployment offers somebody who is not in it yet.
     *
     * Asked before the door offers a form, because a deployment that refuses
     * registrations should say so rather than accept a form and refuse it. The
     * answer is a value rather than an error: a closed door is a state the
     * screen renders, and the code beside it says which closure it is.
     */
    async registrationPolicy(): Promise<RegistrationPolicyView> {
        return registrationPolicyViewSchema.parse(
            await request('/api/registration-policy', { method: 'GET' }),
        );
    },

    /**
     * Registers an account.
     *
     * The address the person arrived at is not sent: the BFF forwards the
     * hostname the request was served at, because the tenant is resolved from
     * the address rather than typed into the form.
     */
    async signup(input: SignupRequest): Promise<SignupResult> {
        const payload = await request('/api/signup', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
        return signupResultSchema.parse(payload);
    },

    /**
     * The root legal entities a tenant can be started from, matched against
     * what a person typed.
     *
     * The matching is the read's, because the deployment holds far more
     * entities than one answer can carry: a screen sends the text rather than
     * fetching a page and filtering it here.
     */
    async leiEntities(search: string): Promise<readonly LeiEntityChoice[]> {
        const query = new URLSearchParams({ search });
        const payload = leiEntitiesResponseSchema.parse(
            await request(`/api/lei-entities?${query.toString()}`, { method: 'GET' }),
        );
        return payload.entities;
    },

    /** The starting points a new tenant may be provisioned from. */
    async seedProfiles(): Promise<readonly SeedProfileChoice[]> {
        const payload = seedProfilesResponseSchema.parse(
            await request('/api/seed-profiles', { method: 'GET' }),
        );
        return payload.profiles;
    },

    /**
     * The tenant lifecycle statuses, with the badge each one is painted with.
     *
     * The words are the status's own and the colours are the badge's, so a
     * screen showing a tenant's status reads one list rather than joining two.
     * A status whose badge has left the catalogue carries no colours.
     */
    async tenantStatuses(): Promise<readonly TenantStatus[]> {
        const payload = tenantStatusesResponseSchema.parse(
            await request('/api/tenant-statuses', { method: 'GET' }),
        );
        return payload.statuses;
    },

    /** The tenant types, each with the badge its row names. */
    async tenantTypes(): Promise<readonly TenantType[]> {
        const payload = tenantTypesResponseSchema.parse(
            await request('/api/tenant-types', { method: 'GET' }),
        );
        return payload.types;
    },

    /**
     * The services roster: every expected instance and when it last reported.
     *
     * The BFF marks each row's age from the deployment's clock, so a row's age
     * is the deployment's measurement rather than this browser's subtraction.
     */
    async services(): Promise<readonly ServiceRosterRow[]> {
        const payload = serviceRosterViewSchema.parse(
            await request('/api/operations/services', { method: 'GET' }),
        );
        return payload.rows;
    },

    /**
     * The compute grid: the stored counters and the nodes, each with its runner.
     *
     * The BFF joins the host registry onto the node rows and folds each node's
     * runner onto them, so a name and a runner state are the deployment's answer
     * rather than joins this browser makes from pages it would have had to read
     * itself.
     */
    async grid(): Promise<GridView> {
        return gridViewSchema.parse(await request('/api/operations/grid', { method: 'GET' }));
    },

    /**
     * The grid's samples over a range, as columns on one time axis.
     *
     * The range travels as a preset, as the bus and logs ranges do, because the
     * window and the bucket width are the deployment's to compute. The reply
     * holds the grid counters per bucket and one row of buckets per node, and a
     * bucket with no sample says so through its samples column.
     */
    async gridSeries(range: SeriesRange): Promise<GridSeriesView> {
        const query = new URLSearchParams({ range });
        return gridSeriesViewSchema.parse(
            await request(`/api/operations/grid/series?${query.toString()}`, { method: 'GET' }),
        );
    },

    /**
     * Who uses the grid: the work counted by tenant for an operator, and by
     * party for a tenant's administrator.
     *
     * The call names no scope. The server reads the session and decides which
     * rows the caller may count and how to group them.
     */
    async gridUsage(): Promise<GridUsageView> {
        return gridUsageViewSchema.parse(
            await request('/api/operations/grid/usage', { method: 'GET' }),
        );
    },

    /**
     * The message bus: the NATS server samples over the chosen range, and one
     * row per stream.
     *
     * The range travels as a preset rather than as two instants, because the
     * window is the deployment's to compute: a browser that sent its own times
     * would state a window the deployment never measured, and the read's range
     * is start-inclusive and end-exclusive.
     */
    async bus(range: BusRange): Promise<BusView> {
        const query = new URLSearchParams({ range });
        return busViewSchema.parse(
            await request(`/api/operations/bus?${query.toString()}`, { method: 'GET' }),
        );
    },

    /**
     * One page of the telemetry logs a filter selects, and the total it matches.
     *
     * The range travels as a preset rather than as two instants, for the same
     * reason the bus range does: the window is the deployment's to compute.
     * Every other filter travels only when the person set it, because an empty
     * one means the filter is off and a value the store cannot match is worse
     * than no filter at all. The source is offered as `server` alone, the one
     * word the store stamps today.
     */
    async logs(query: LogsQuery): Promise<LogsView> {
        const params = new URLSearchParams({
            range: query.range,
            offset: String(query.offset),
            limit: String(query.limit),
        });
        if (query.level !== '') {
            params.set('level', query.level);
        }
        if (query.source !== '') {
            params.set('source', query.source);
        }
        if (query.component !== '') {
            params.set('component', query.component);
        }
        if (query.tag !== '') {
            params.set('tag', query.tag);
        }
        if (query.message !== '') {
            params.set('message', query.message);
        }
        return logsViewSchema.parse(
            await request(`/api/operations/logs?${params.toString()}`, { method: 'GET' }),
        );
    },

    /**
     * The tenants this deployment holds.
     *
     * The system tenant is not among them: it is the deployment's own
     * bookkeeping rather than a tenant somebody set up, and the answer has
     * already dropped it.
     */
    async tenants(
        query: {
            readonly search?: string;
            readonly type?: string;
            readonly status?: string;
            readonly includeTest?: boolean;
            readonly sort?: string;
            readonly descending?: boolean;
            readonly offset?: number;
            readonly limit?: number;
        } = {},
    ): Promise<TenantPage> {
        const params = new URLSearchParams();
        for (const key of ['search', 'type', 'status'] as const) {
            const value = query[key];
            if (value !== undefined && value !== '') {
                params.set(key, value);
            }
        }
        if (query.includeTest === true) {
            params.set('includeTest', 'true');
        }
        if (query.sort !== undefined && query.sort !== '') {
            params.set('sort', query.sort);
        }
        if (query.descending === true) {
            params.set('descending', 'true');
        }
        if (query.offset !== undefined) {
            params.set('offset', String(query.offset));
        }
        if (query.limit !== undefined) {
            params.set('limit', String(query.limit));
        }
        const suffix = params.size === 0 ? '' : `?${params.toString()}`;
        return tenantPageSchema.parse(await request(`/api/tenants${suffix}`, { method: 'GET' }));
    },

    /**
     * The state of the deployment's tenants, for the system administrator's
     * home: the counts, what needs attention, the first tenants and the newest
     * setups.
     */
    async overview(): Promise<DeploymentOverview> {
        return deploymentOverviewSchema.parse(await request('/api/overview', { method: 'GET' }));
    },

    /** One page of the parties of the session's own tenant. */
    async parties(query: {
        readonly offset: number;
        readonly limit: number;
        readonly search?: string;
        readonly sort?: string;
        readonly descending?: boolean;
    }): Promise<PartyPage> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
        });
        if (query.search !== undefined && query.search !== '') {
            params.set('search', query.search);
        }
        if (query.sort !== undefined && query.sort !== '') {
            params.set('sort', query.sort);
        }
        if (query.descending === true) {
            params.set('descending', 'true');
        }
        return partyPageSchema.parse(
            await request(`/api/parties?${params.toString()}`, { method: 'GET' }),
        );
    },

    /**
     * Removes a tenant. The code is typed again by the person, and the server
     * checks it against the tenant the address names.
     */
    async removeTenant(code: string, confirmCode: string): Promise<void> {
        await request(`/api/tenants/${encodeURIComponent(code)}`, {
            method: 'DELETE',
            headers: JSON_HEADERS,
            body: JSON.stringify({ confirmCode }),
        });
    },

    /** One page of a tenant's parties, read inside it from system administration. */
    async tenantParties(
        code: string,
        query: {
            readonly offset: number;
            readonly limit: number;
            readonly search?: string;
            readonly sort?: string;
            readonly descending?: boolean;
        },
    ): Promise<PartyPage> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
        });
        if (query.search !== undefined && query.search !== '') {
            params.set('search', query.search);
        }
        if (query.sort !== undefined && query.sort !== '') {
            params.set('sort', query.sort);
        }
        if (query.descending === true) {
            params.set('descending', 'true');
        }
        return partyPageSchema.parse(
            await request(`/api/tenants/${encodeURIComponent(code)}/parties?${params.toString()}`, {
                method: 'GET',
            }),
        );
    },

    /** One page of a tenant's people, read inside it from system administration. */
    async tenantPeople(
        code: string,
        query: { readonly offset: number; readonly limit: number },
    ): Promise<{ readonly accounts: readonly Account[]; readonly totalCount: number }> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
        });
        return z
            .object({ accounts: z.array(accountSchema), totalCount: z.int().nonnegative() })
            .parse(
                await request(
                    `/api/tenants/${encodeURIComponent(code)}/people?${params.toString()}`,
                    { method: 'GET' },
                ),
            );
    },

    /** One tenant by its code: the tenant and its setup run. */
    async tenant(code: string): Promise<TenantDetailResponse> {
        return tenantDetailResponseSchema.parse(
            await request(`/api/tenants/${encodeURIComponent(code)}`, { method: 'GET' }),
        );
    },

    /**
     * Creates a tenant from a starting point.
     *
     * The tenant and its administrator exist when this answers, and the run
     * that provisions the rest is followed by the id it carries rather than by
     * waiting here.
     */
    async provisionTenant(input: ProvisionTenantRequest): Promise<ProvisionTenantResult> {
        const payload = await request('/api/provision-tenant', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
        return provisionTenantResultSchema.parse(payload);
    },

    /**
     * Re-scopes the open session to another of the account's parties.
     *
     * A party added during this session is not in the list the login answered
     * with, so the server reads the tenant's parties and states the chosen one
     * in the session rather than taking the caller's word for it.
     */
    async switchParty(partyId: string): Promise<SessionView> {
        const payload = await request('/api/session/switch-party', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ partyId }),
        });
        return sessionViewSchema.parse(payload);
    },

    /**
     * Adds a party to the tenant the person works in.
     *
     * The party exists when this answers, and the run that publishes its data,
     * activates it and joins the person to it is followed by the id it carries.
     */
    async provisionParty(input: ProvisionPartyRequest): Promise<ProvisionPartyResult> {
        const payload = await request('/api/provision-party', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify(input),
        });
        return provisionPartyResultSchema.parse(payload);
    },

    /** The state of a provisioning run, as its journey's rail renders it. */
    async provisionTenantProgress(instanceId: string): Promise<WorkflowProgress> {
        return workflowProgressSchema.parse(
            await request(`/api/workflow/${encodeURIComponent(instanceId)}`, {
                method: 'GET',
            }),
        );
    },

    /**
     * Resumes a stopped run from the step that failed.
     *
     * A refusal is an answer rather than a failure: a run that has not stopped,
     * or a step it does not hold, is something the person asking can see.
     */
    async retryProvisionTenant(
        instanceId: string,
        stepName = '',
    ): Promise<RetryWorkflowInstanceResult> {
        const payload = await request(`/api/workflow/${encodeURIComponent(instanceId)}/retry`, {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ stepName }),
        });
        return retryWorkflowInstanceResultSchema.parse(payload);
    },

    /**
     * Sets a password of the signed-in account's own.
     *
     * The current password travels with the request, because the account is
     * changing a credential it holds rather than one an administrator issued.
     */
    async changePassword(currentPassword: string, newPassword: string): Promise<void> {
        await request('/api/account/password', {
            method: 'POST',
            headers: JSON_HEADERS,
            body: JSON.stringify({ currentPassword, newPassword }),
        });
    },

    /**
     * Writes the signed-in person's own name, job title and picture.
     *
     * The session names the account, so the body carries no account id. A
     * refusal -- a field a member does not own -- is the answer's result and
     * not a failed call, because the panel has to read its code.
     */
    async saveMyProfile(write: ProfileWrite): Promise<AccountWriteView> {
        return accountWriteViewSchema.parse(
            await request('/api/me/profile', {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify(write),
            }),
        );
    },

    /** The signed-in person's own contact record, or nothing when none exists. */
    async myContactInformation(): Promise<AccountContactInformation | null> {
        const view = contactViewSchema.parse(
            await request('/api/me/contact-information', { method: 'GET' }),
        );
        return view.contact;
    },

    /** Writes the signed-in person's own contact record, creating it on the first write. */
    async saveMyContactInformation(write: ContactWrite): Promise<ContactWriteView> {
        return contactWriteViewSchema.parse(
            await request('/api/me/contact-information', {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify(write),
            }),
        );
    },

    /**
     * Writes one account's profile fields, as a tenant administrator.
     *
     * The route reads the account and sends the record whole, so the fields
     * this screen does not set are kept. The server refuses a caller without
     * =iam::accounts:update=.
     */
    async saveAccountProfile(username: string, write: ProfileWrite): Promise<void> {
        await request(`/api/accounts/${encodeURIComponent(username)}/profile`, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify(write),
        });
    },

    /** One account's contact record, or nothing when none exists. */
    async accountContactInformation(accountId: string): Promise<AccountContactInformation | null> {
        const view = contactViewSchema.parse(
            await request(`/api/accounts/${encodeURIComponent(accountId)}/contact-information`, {
                method: 'GET',
            }),
        );
        return view.contact;
    },

    /**
     * Writes one account's contact record, as a tenant administrator.
     *
     * The version the panel read travels as the claim, so the store refuses a
     * record that moved under the save rather than overwriting it.
     */
    async saveAccountContactInformation(
        accountId: string,
        write: ClaimedContactWrite,
    ): Promise<ContactWriteView> {
        return contactWriteViewSchema.parse(
            await request(`/api/accounts/${encodeURIComponent(accountId)}/contact-information`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify(write),
            }),
        );
    },

    /**
     * Uploads one image and answers its identifier.
     *
     * The upload sets no photo: the identifier rides into the panel's own
     * save. A refused image is the answer's result and not a failed call,
     * because the picker branches on the server's code.
     */
    async uploadImage(mimeType: string, data: string): Promise<ImageUploadView> {
        return imageUploadViewSchema.parse(
            await request('/api/images', {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify({ mimeType, data }),
            }),
        );
    },

    /** The rule an uploaded image must satisfy, read from the validator that enforces it. */
    async imageUploadPolicy(): Promise<ImageUploadPolicy> {
        return imageUploadPolicyViewSchema.parse(
            await request('/api/image-upload-policy', { method: 'GET' }),
        );
    },
};

export { ApiFailure };
