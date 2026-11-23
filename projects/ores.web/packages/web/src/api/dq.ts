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
import type { ArtefactType } from '@ores/wire-protocol/generated/dq/domain/artefact_type';
import type { Dataset } from '@ores/wire-protocol/generated/dq/domain/dataset';
import type { DatasetBundle } from '@ores/wire-protocol/generated/dq/domain/dataset_bundle';
import type { DatasetBundleMember } from '@ores/wire-protocol/generated/dq/domain/dataset_bundle_member';
import { baseFor } from './servers.js';
import { request } from './transport.js';

/**
 * The DQ screens' reads.
 *
 * These are the collections the C++ server publishes under the dq area: the
 * datasets, the bundles that carry them, the junctions between the two, and the
 * artefact types that name each dataset's staging table, target table and target
 * subject. The rows are the DQ service's own, so each is typed from the
 * generated domain row and parsed only as an object.
 *
 * The screen names the module in VITE_ORE_CPP_SCREENS, so a build can send these
 * calls to the C++ server while the rest of the application stays where it is.
 */

const CPP = baseFor('dq');

function rowsOf<Row>(): z.ZodType<readonly Row[]> {
    return z.array(z.looseObject({})) as unknown as z.ZodType<readonly Row[]>;
}

/** The datasets, with the page the collection takes. */
export async function readDatasets(): Promise<readonly Dataset[]> {
    const page = await request(`${CPP}/api/v1/dq/datasets?limit=500`, { method: 'GET' });
    return z.object({ datasets: rowsOf<Dataset>() }).parse(page).datasets;
}

/** The bundles, with the level each of them states. */
export async function readDatasetBundles(): Promise<readonly DatasetBundle[]> {
    const page = await request(`${CPP}/api/v1/dq/dataset_bundles?limit=500`, { method: 'GET' });
    return z.object({ dataset_bundles: rowsOf<DatasetBundle>() }).parse(page).dataset_bundles;
}

/** Every bundle member, which the coverage screen groups by bundle. */
export async function readDatasetBundleMembers(): Promise<readonly DatasetBundleMember[]> {
    const page = await request(`${CPP}/api/v1/dq/dataset_bundle_members?limit=500`, {
        method: 'GET',
    });
    return z.object({ dataset_bundle_members: rowsOf<DatasetBundleMember>() }).parse(page)
        .dataset_bundle_members;
}

/** The artefact types, which name where a dataset's rows are staged and published. */
export async function readArtefactTypes(): Promise<readonly ArtefactType[]> {
    const page = await request(`${CPP}/api/v1/dq/artefact_types?limit=500`, { method: 'GET' });
    return z.object({ artefact_types: rowsOf<ArtefactType>() }).parse(page).artefact_types;
}

/** Everything the coverage screen reads, in one call, so the four arrive together. */
export interface Coverage {
    readonly bundles: readonly DatasetBundle[];
    readonly members: readonly DatasetBundleMember[];
    readonly datasets: readonly Dataset[];
    readonly artefactTypes: readonly ArtefactType[];
}

export async function readCoverage(): Promise<Coverage> {
    const [bundles, members, datasets, artefactTypes] = await Promise.all([
        readDatasetBundles(),
        readDatasetBundleMembers(),
        readDatasets(),
        readArtefactTypes(),
    ]);
    return { bundles, members, datasets, artefactTypes };
}

/**
 * Whether a bundle member can be published, and if not, why not.
 *
 * The four reasons have different owners, which is why the screen states one and
 * not "not ready": a member with no dataset row, no artefact type, or no target
 * subject is DQ's to fix, and a member whose staging table holds no rows is the
 * importer's.
 */
export type NotReadyReason =
    'no dataset' | 'no artefact type' | 'no target subject' | 'no staged rows';

/** What the coverage screen can say about one member. */
export interface MemberReadiness {
    readonly member: DatasetBundleMember;
    readonly dataset: Dataset | undefined;
    readonly artefactType: ArtefactType | undefined;
    readonly reason: NotReadyReason | null;
}

/** Resolve one member against the datasets and their artefact types. */
export function readinessOf(
    member: DatasetBundleMember,
    datasets: readonly Dataset[],
    artefactTypes: readonly ArtefactType[],
): MemberReadiness {
    const dataset = datasets.find((one) => one.code === member.dataset_code);
    if (!dataset) {
        return { member, dataset, artefactType: undefined, reason: 'no dataset' };
    }
    const artefactType = artefactTypes.find((one) => one.code === dataset.artefact_type);
    if (!artefactType) {
        return { member, dataset, artefactType, reason: 'no artefact type' };
    }
    if (!artefactType.target_subject) {
        return { member, dataset, artefactType, reason: 'no target subject' };
    }
    return { member, dataset, artefactType, reason: null };
}
