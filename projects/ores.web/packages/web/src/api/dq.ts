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
 */

import { z } from 'zod';
import type {
    BundleRosterBundle,
    BundleRosterMember,
    BundleRosterResponse,
} from '@ores/wire-protocol/generated/dq/protocol/bundle_roster_protocol';
import type { DatasetPublication } from '@ores/wire-protocol/generated/dq/protocol/dataset_publications_protocol';
import type { DatasetTable } from '@ores/wire-protocol/generated/dq/protocol/dataset_contents_protocol';
import { baseFor } from './servers.js';
import { request } from './transport.js';

/**
 * The DQ screens' reads.
 *
 * The roster is the one read the coverage screen needs. It answers the level
 * each bundle publishes at, how many datasets it carries and, for the bundle a
 * caller names, its members with the number of rows each has staged. The
 * bundles and the members are not read as separate collections: the roster
 * resolves both against the store's own discovery functions, which is where the
 * staged count comes from and what lets a tenant read a shared bundle.
 *
 * The screen names the module in VITE_ORE_CPP_SCREENS, so a build can send this
 * call to the C++ server while the rest of the application stays where it is.
 */

const CPP = baseFor('dq');

/**
 * The bundle roster, or the roster with one bundle's members.
 *
 * The server takes the staged count with one count per member, so it is asked
 * for the bundle in view and never for the roster as a whole. A dataset's own
 * page knows the dataset and not the bundle that carries it, so it names the
 * dataset and the server resolves the bundle.
 */
export async function readBundleRoster(
    bundleCode = '',
    datasetCode = '',
): Promise<readonly BundleRosterBundle[]> {
    const params = new URLSearchParams();
    if (bundleCode !== '') {
        params.set('bundle_code', bundleCode);
    }
    if (datasetCode !== '') {
        params.set('dataset_code', datasetCode);
    }
    const query = params.size === 0 ? '' : `?${params.toString()}`;
    const page = await request(`${CPP}/api/v1/dq/bundle_roster${query}`, { method: 'GET' });
    const answer = z
        .object({
            success: z.boolean(),
            error_message: z.string(),
            bundles: z.array(z.looseObject({})),
        })
        .parse(page) as unknown as BundleRosterResponse;
    if (!answer.success) {
        throw new Error(answer.error_message);
    }
    return answer.bundles;
}

/**
 * The runs that published one dataset, newest first.
 *
 * The log states an outcome and the messages the run produced, so a screen can
 * say whether a publication worked and why it did not, and not only when it last
 * happened.
 */
export async function readDatasetPublications(
    datasetCode: string,
): Promise<readonly DatasetPublication[]> {
    const page = await request(
        `${CPP}/api/v1/dq/dataset_publications?dataset_code=${encodeURIComponent(datasetCode)}`,
        { method: 'GET' },
    );
    const answer = z
        .object({
            success: z.boolean(),
            error_message: z.string(),
            publications: z.array(z.looseObject({})),
        })
        .parse(page) as unknown as {
        success: boolean;
        error_message: string;
        publications: readonly DatasetPublication[];
    };
    if (!answer.success) {
        throw new Error(answer.error_message);
    }
    return answer.publications;
}

/**
 * Why a bundle member cannot publish, when it cannot.
 *
 * The four reasons have different owners, which is why the screen states one and
 * not "not ready": a member with no dataset row, no artefact type, or no target
 * subject is DQ's to fix, and a member whose staging table holds no rows is the
 * importer's. The server states the three facts the first three reasons rest on,
 * so the screen reads them and never a sentence the server wrote about a
 * missing row.
 */
export type NotReadyReason =
    'no dataset' | 'no artefact type' | 'no target subject' | 'no staged rows';

/** The reason one member is not ready, or null when it is. */
export function reasonOf(member: BundleRosterMember): NotReadyReason | null {
    if (!member.has_dataset) {
        return 'no dataset';
    }
    if (!member.has_artefact_type) {
        return 'no artefact type';
    }
    if (member.target_subject === '') {
        return 'no target subject';
    }
    if (member.staged_count === 0) {
        return 'no staged rows';
    }
    return null;
}

/**
 * One column of a table's rows, as the store states it.
 *
 * The rows carry their own columns because every artefact family carries
 * different ones, and no screen knows them in advance.
 */
export interface DatasetRowColumn {
    readonly name: string;
    readonly type: string;
}

/** One page of a table's rows, with the columns the rows carry. */
export interface DatasetRowsPage {
    readonly columns: readonly DatasetRowColumn[];
    readonly rows: readonly Readonly<Record<string, unknown>>[];
    readonly total: number;
}

/**
 * The tables a dataset stages into.
 *
 * A dataset names one artefact type and the type names a table or a family of
 * them, so this is the list a person reads before picking one table to open.
 * Each table states its own columns and its own staged count: the roster's
 * figure is the family's sum and cannot say which table is empty.
 */
export async function readDatasetTables(datasetCode: string): Promise<readonly DatasetTable[]> {
    const answer = await readContents(datasetCode, '', 0, 0);
    return answer.tables;
}

/**
 * One page of one table's rows.
 *
 * The table is named as the list showed it. The server refuses any name the
 * dataset's own artefact type does not state, so a stale page answers an error
 * rather than another dataset's rows.
 */
export async function readDatasetRows(
    datasetCode: string,
    tableName: string,
    limit = 15,
    offset = 0,
): Promise<DatasetRowsPage> {
    const answer = await readContents(datasetCode, tableName, limit, offset);
    if (answer.page === null) {
        throw new Error(`No rows were read from ${tableName}.`);
    }
    return answer.page;
}

/** The two reads above, as the one operation the server serves. */
async function readContents(
    datasetCode: string,
    tableName: string,
    limit: number,
    offset: number,
): Promise<{ readonly tables: readonly DatasetTable[]; readonly page: DatasetRowsPage | null }> {
    const params = new URLSearchParams({ dataset_code: datasetCode });
    if (tableName !== '') {
        params.set('table_name', tableName);
        params.set('limit', String(limit));
        params.set('offset', String(offset));
    }
    const answer = z
        .object({
            success: z.boolean(),
            error_message: z.string(),
            tables: z.array(z.looseObject({})),
            rows: z.string(),
        })
        .parse(
            await request(`${CPP}/api/v1/dq/dataset_contents?${params.toString()}`, {
                method: 'GET',
            }),
        );
    if (!answer.success) {
        throw new Error(answer.error_message);
    }
    const tables = answer.tables as unknown as readonly DatasetTable[];
    if (tableName === '') {
        return { tables, page: null };
    }
    const page = z
        .object({
            columns: z.array(z.object({ name: z.string(), type: z.string() })),
            rows: z.array(z.record(z.string(), z.unknown())),
            total: z.number(),
        })
        .parse(JSON.parse(answer.rows)) as DatasetRowsPage;
    return { tables, page };
}
