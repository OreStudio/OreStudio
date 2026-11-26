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

import { afterEach, describe, expect, it, vi } from 'vitest';
import { readArtefactTypes, readDatasetBundles, readinessOf } from './dq.js';

/**
 * The DQ reads, and the readiness the coverage screen draws.
 *
 * The server's collection envelopes are asserted here because a screen that read
 * the wrong key would draw an empty table and look like a tenant with no
 * reference data, which is the failure the coverage screen exists to catch.
 */

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('a DQ collection read', () => {
    it('takes the rows from the collection the server names', async () => {
        vi.stubGlobal(
            'fetch',
            async () =>
                new Response(
                    JSON.stringify({
                        bundles: [
                            { code: 'party_essentials', name: 'Party essentials', level: 'tenant' },
                        ],
                    }),
                    { status: 200 },
                ),
        );

        const bundles = await readDatasetBundles();

        expect(bundles.map((one) => one.code)).toEqual(['party_essentials']);
        expect(bundles[0].level).toBe('tenant');
    });
});

describe('why a bundle member is not ready', () => {
    const member = { bundle_code: 'party_essentials', dataset_code: 'refdata.conventions' };
    const dataset = { code: 'refdata.conventions', artefact_type: 'conventions' };
    const artefactType = { code: 'conventions', target_subject: 'refdata.v1.ops.publish' };

    it('names the missing dataset', () => {
        expect(readinessOf(member, [], [artefactType]).reason).toBe('no dataset');
    });

    it('names the missing artefact type', () => {
        expect(readinessOf(member, [dataset], []).reason).toBe('no artefact type');
    });

    it('names the missing target subject', () => {
        const bare = { code: 'conventions', target_subject: null };
        expect(readinessOf(member, [dataset], [bare]).reason).toBe('no target subject');
    });

    it('is ready when the dataset and its subject are there', () => {
        expect(readinessOf(member, [dataset], [artefactType]).reason).toBeNull();
    });
});

describe('a DQ collection whose name is not the entity', () => {
    it('takes the artefact types from the member the server calls types', async () => {
        vi.stubGlobal(
            'fetch',
            async () =>
                new Response(
                    JSON.stringify({ types: [{ code: 'currency', target_subject: 'refdata' }] }),
                    { status: 200 },
                ),
        );

        const types = await readArtefactTypes();

        expect(types.map((one) => one.code)).toEqual(['currency']);
    });
});
