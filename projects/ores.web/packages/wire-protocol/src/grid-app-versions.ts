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
import type { AuthenticatedCaller } from './account-operations.js';
import type { App } from './generated/compute/domain/app.js';
import type { AppVersion } from './generated/compute/domain/app_version.js';
import type { AppVersionPlatform } from './generated/compute/domain/app_version_platform.js';
import type { Platform } from './generated/compute/domain/platform.js';
import {
    subjects as appSubjects,
    type ListAppsRequest,
    type ListAppsResponse,
} from './generated/compute/protocol/app_protocol.js';
import {
    subjects as appVersionPlatformSubjects,
    type ListByAppVersionIdAppVersionPlatformsRequest,
    type ListByAppVersionIdAppVersionPlatformsResponse,
} from './generated/compute/protocol/app_version_platform_protocol.js';
import {
    subjects as appVersionSubjects,
    type GetAppVersionRequest,
    type GetAppVersionResponse,
    type ListAppVersionsRequest,
    type ListAppVersionsResponse,
    type PutAppVersionRequest,
    type PutAppVersionResponse,
} from './generated/compute/protocol/app_version_protocol.js';
import {
    subjects as platformSubjects,
    type ListPlatformsRequest,
    type ListPlatformsResponse,
} from './generated/compute/protocol/platform_protocol.js';
import { requireOk } from './grid-jobs.js';
import { decidedResultSchema, orderSchema } from './operations.js';
import { wireTimestampSchema } from './time.js';

/**
 * One app version of the compute catalogue, as the app version page reads it: the
 * build a job pins, the release note stored with it, the packages the wrapper
 * downloads and the other versions of its app.
 *
 * Every read is the entity's own generated read, so the compute service checks the
 * read code and the policy of the tenant. The page writes one field, the release
 * note, through the generated put, which states the version it read.
 */

const text = z.string().default('');

const appSchema = z.object({
    version: z.int().nonnegative().default(0),
    tenant_id: z.string(),
    id: z.string(),
    name: text,
    description: text,
    modified_by: text,
    performed_by: text,
    change_reason_code: text,
    change_commentary: text,
    recorded_at: wireTimestampSchema,
}) satisfies z.ZodType<App>;

const appVersionSchema = z.object({
    version: z.int().nonnegative().default(0),
    tenant_id: z.string(),
    id: z.string(),
    app_id: z.string(),
    wrapper_version: text,
    engine_version: text,
    min_ram_mb: z.int().default(0),
    release_note: text,
    modified_by: text,
    performed_by: text,
    change_reason_code: text,
    change_commentary: text,
    recorded_at: wireTimestampSchema,
}) satisfies z.ZodType<AppVersion>;

const platformSchema = z.object({
    version: z.int().nonnegative().default(0),
    tenant_id: z.string(),
    id: z.string(),
    code: text,
    display_name: text,
    description: text,
    os_family: text,
    cpu_arch: text,
    abi: text,
    is_active: z.boolean().default(false),
    modified_by: text,
    performed_by: text,
    change_reason_code: text,
    change_commentary: text,
    recorded_at: wireTimestampSchema,
}) satisfies z.ZodType<Platform>;

const appVersionPlatformSchema = z.object({
    version: z.int().nonnegative().default(0),
    tenant_id: z.string(),
    app_version_id: z.string(),
    platform_id: z.string(),
    package_uri: text,
    sha256: text,
    modified_by: text,
    performed_by: text,
    change_reason_code: text,
    change_commentary: text,
    recorded_at: wireTimestampSchema,
}) satisfies z.ZodType<AppVersionPlatform>;

const pageFields = {
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(1000),
    order: orderSchema.default({ field: '', descending: false }),
};

const idFilter = z
    .object({ id_one_of: z.array(z.string()).nullable() })
    .nullable()
    .default(null);

export const listAppsRequestSchema = z.object({
    ...pageFields,
    filter: idFilter,
    as_of: z.string().nullable().default(null),
}) satisfies z.ZodType<ListAppsRequest>;

export const listAppsReplySchema = z.object({
    result: decidedResultSchema,
    apps: z.array(appSchema).default([]),
    total: z.int().nonnegative().default(0),
}) satisfies z.ZodType<ListAppsResponse>;

export const listAppVersionsRequestSchema = z.object({
    ...pageFields,
    filter: idFilter,
    as_of: z.string().nullable().default(null),
}) satisfies z.ZodType<ListAppVersionsRequest>;

export const listAppVersionsReplySchema = z.object({
    result: decidedResultSchema,
    app_versions: z.array(appVersionSchema).default([]),
    total: z.int().nonnegative().default(0),
}) satisfies z.ZodType<ListAppVersionsResponse>;

export const getAppVersionRequestSchema = z.object({
    key: z.object({ id: z.string() }),
}) satisfies z.ZodType<GetAppVersionRequest>;

export const getAppVersionReplySchema = z.object({
    result: decidedResultSchema,
    app_version: appVersionSchema.nullable().default(null),
}) satisfies z.ZodType<GetAppVersionResponse>;

export const listPlatformsRequestSchema = z.object({
    ...pageFields,
    filter: idFilter,
    as_of: z.string().nullable().default(null),
}) satisfies z.ZodType<ListPlatformsRequest>;

export const listPlatformsReplySchema = z.object({
    result: decidedResultSchema,
    platforms: z.array(platformSchema).default([]),
    total: z.int().nonnegative().default(0),
}) satisfies z.ZodType<ListPlatformsResponse>;

export const listPackagesRequestSchema = z.object({
    app_version_id: z.string(),
    scope: z.enum(['direct', 'subtree']).default('direct'),
    ...pageFields,
    filter: z.null().default(null),
}) satisfies z.ZodType<ListByAppVersionIdAppVersionPlatformsRequest>;

export const listPackagesReplySchema = z.object({
    result: decidedResultSchema,
    app_version_platforms: z.array(appVersionPlatformSchema).default([]),
    total: z.int().nonnegative().default(0),
}) satisfies z.ZodType<ListByAppVersionIdAppVersionPlatformsResponse>;

export const putAppVersionRequestSchema = z.object({
    change: z.object({
        write: z.object({
            id: z.string(),
            app_id: z.string(),
            wrapper_version: z.string(),
            engine_version: z.string(),
            min_ram_mb: z.int(),
            release_note: z.string(),
        }),
        precondition: z.object({
            kind: z.enum(['any', 'must_not_exist', 'must_match_version']),
            version: z.int().nullable(),
        }),
    }),
    intent: z.object({ reason_code: z.string(), commentary: z.string() }),
}) satisfies z.ZodType<PutAppVersionRequest>;

export const putAppVersionReplySchema = z.object({
    result: decidedResultSchema,
    app_version: appVersionSchema.nullable().default(null),
}) satisfies z.ZodType<PutAppVersionResponse>;

/** The subjects the reads and the write travel on, from the generated protocols. */
export const GRID_APP_VERSION_SUBJECTS = {
    getAppVersion: appVersionSubjects.get_app_version_request,
    listAppVersions: appVersionSubjects.list_app_versions_request,
    putAppVersion: appVersionSubjects.put_app_version_request,
    listApps: appSubjects.list_apps_request,
    listPlatforms: platformSubjects.list_platforms_request,
    listPackages: appVersionPlatformSubjects.list_by_app_version_id_app_version_platforms_request,
} as const;

/** One build of the catalogue, a package of it for one platform, and its siblings. */
export const appVersionViewSchema = z.object({
    id: z.string(),
    app_id: z.string(),
    app_name: z.string().nullable(),
    wrapper_version: z.string(),
    engine_version: z.string(),
    min_ram_mb: z.int(),
    release_note: z.string(),
    version: z.int().nonnegative(),
    modified_by: z.string(),
    recorded_at: wireTimestampSchema,
});
export type AppVersionView = z.infer<typeof appVersionViewSchema>;

export const packageViewSchema = z.object({
    platform_id: z.string(),
    code: z.string(),
    display_name: z.string(),
    is_active: z.boolean(),
    package_uri: z.string(),
    sha256: z.string(),
});
export type PackageView = z.infer<typeof packageViewSchema>;

export const siblingVersionViewSchema = z.object({
    id: z.string(),
    engine_version: z.string(),
    wrapper_version: z.string(),
    recorded_at: wireTimestampSchema,
});
export type SiblingVersionView = z.infer<typeof siblingVersionViewSchema>;

/**
 * The app version page: the build with its note, its packages and the other
 * versions of its app, newest first. `may_write` says whether the session may
 * edit the note, so the page withholds the control from every other role.
 */
export const appVersionPageViewSchema = z.object({
    version: appVersionViewSchema,
    packages: z.array(packageViewSchema),
    versions: z.array(siblingVersionViewSchema),
    may_write: z.boolean(),
});
export type AppVersionPageView = z.infer<typeof appVersionPageViewSchema>;

/** One app version by its id, or nothing when the session may not see it. */
export async function readAppVersion(
    caller: AuthenticatedCaller,
    id: string,
): Promise<AppVersion | null> {
    const reply = await caller.callAuthenticated(
        GRID_APP_VERSION_SUBJECTS.getAppVersion,
        getAppVersionRequestSchema.parse({ key: { id } }),
        getAppVersionReplySchema,
    );
    if (reply.result.outcome === 'missing') {
        return null;
    }
    requireOk(GRID_APP_VERSION_SUBJECTS.getAppVersion, reply.result);
    return reply.app_version;
}

async function readApp(caller: AuthenticatedCaller, id: string): Promise<App | null> {
    const reply = await caller.callAuthenticated(
        GRID_APP_VERSION_SUBJECTS.listApps,
        listAppsRequestSchema.parse({ limit: 1, filter: { id_one_of: [id] } }),
        listAppsReplySchema,
    );
    requireOk(GRID_APP_VERSION_SUBJECTS.listApps, reply.result);
    return reply.apps[0] ?? null;
}

/**
 * Every version of an app, newest first.
 *
 * The generated read has no filter by app, so the page reads the catalogue's
 * versions and keeps those of the app. The catalogue holds a handful per app.
 */
async function readVersionsOfApp(
    caller: AuthenticatedCaller,
    appId: string,
): Promise<readonly AppVersion[]> {
    const reply = await caller.callAuthenticated(
        GRID_APP_VERSION_SUBJECTS.listAppVersions,
        listAppVersionsRequestSchema.parse({}),
        listAppVersionsReplySchema,
    );
    requireOk(GRID_APP_VERSION_SUBJECTS.listAppVersions, reply.result);
    return reply.app_versions
        .filter((candidate) => candidate.app_id === appId)
        .sort((a, b) =>
            a.recorded_at < b.recorded_at ? 1 : a.recorded_at > b.recorded_at ? -1 : 0,
        );
}

async function readPackages(
    caller: AuthenticatedCaller,
    appVersionId: string,
): Promise<readonly PackageView[]> {
    const [packages, platforms] = await Promise.all([
        caller.callAuthenticated(
            GRID_APP_VERSION_SUBJECTS.listPackages,
            listPackagesRequestSchema.parse({ app_version_id: appVersionId }),
            listPackagesReplySchema,
        ),
        caller.callAuthenticated(
            GRID_APP_VERSION_SUBJECTS.listPlatforms,
            listPlatformsRequestSchema.parse({}),
            listPlatformsReplySchema,
        ),
    ]);
    requireOk(GRID_APP_VERSION_SUBJECTS.listPackages, packages.result);
    requireOk(GRID_APP_VERSION_SUBJECTS.listPlatforms, platforms.result);
    const byId = new Map(platforms.platforms.map((platform) => [platform.id, platform]));
    return packages.app_version_platforms
        .map((row) => {
            const platform = byId.get(row.platform_id);
            return packageViewSchema.parse({
                platform_id: row.platform_id,
                code: platform?.code ?? row.platform_id,
                display_name: platform?.display_name ?? '',
                is_active: platform?.is_active ?? false,
                package_uri: row.package_uri,
                sha256: row.sha256,
            });
        })
        .sort((a, b) => a.code.localeCompare(b.code));
}

/** The page of one app version, or nothing when the session may not see it. */
export async function readAppVersionPage(
    caller: AuthenticatedCaller,
    id: string,
    mayWrite: boolean,
): Promise<AppVersionPageView | null> {
    const version = await readAppVersion(caller, id);
    if (version === null) {
        return null;
    }
    const [app, versions, packages] = await Promise.all([
        readApp(caller, version.app_id),
        readVersionsOfApp(caller, version.app_id),
        readPackages(caller, id),
    ]);
    return appVersionPageViewSchema.parse({
        version: {
            id: version.id,
            app_id: version.app_id,
            app_name: app?.name ?? null,
            wrapper_version: version.wrapper_version,
            engine_version: version.engine_version,
            min_ram_mb: version.min_ram_mb,
            release_note: version.release_note,
            version: version.version,
            modified_by: version.modified_by,
            recorded_at: version.recorded_at,
        },
        packages,
        versions: versions.map((sibling) => ({
            id: sibling.id,
            engine_version: sibling.engine_version,
            wrapper_version: sibling.wrapper_version,
            recorded_at: sibling.recorded_at,
        })),
        may_write: mayWrite,
    });
}

/** The most text a release note holds. */
export const RELEASE_NOTE_MAX = 20000;

/**
 * Replaces the release note of an app version.
 *
 * The write states the version it read, so a note edited by someone else since
 * is refused as a conflict and not overwritten.
 */
export async function writeReleaseNote(
    caller: AuthenticatedCaller,
    id: string,
    input: { readonly releaseNote: string; readonly version: number; readonly commentary: string },
): Promise<AppVersion | null> {
    const current = await readAppVersion(caller, id);
    if (current === null) {
        return null;
    }
    const reply = await caller.callAuthenticated(
        GRID_APP_VERSION_SUBJECTS.putAppVersion,
        putAppVersionRequestSchema.parse({
            change: {
                write: {
                    id: current.id,
                    app_id: current.app_id,
                    wrapper_version: current.wrapper_version,
                    engine_version: current.engine_version,
                    min_ram_mb: current.min_ram_mb,
                    release_note: input.releaseNote,
                },
                precondition: { kind: 'must_match_version', version: input.version },
            },
            intent: { reason_code: 'common.non_material_update', commentary: input.commentary },
        }),
        putAppVersionReplySchema,
    );
    requireOk(GRID_APP_VERSION_SUBJECTS.putAppVersion, reply.result);
    return reply.app_version;
}
