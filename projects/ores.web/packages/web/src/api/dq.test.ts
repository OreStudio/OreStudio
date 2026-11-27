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
import type { BundleRosterMember } from '@ores/wire-protocol/generated/dq/protocol/bundle_roster_protocol';
import { readBundleRoster, reasonOf } from './dq.js';

/**
 * The DQ read, and the readiness the coverage screen draws from it.
 *
 * The roster's envelope is asserted here because a screen that read the wrong
 * key would draw an empty page and look like a tenant with no reference data,
 * which is the failure the coverage screen exists to catch.
 */

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('the bundle roster read', () => {
    it('takes the bundles from the answer the server writes', async () => {
        vi.stubGlobal(
            'fetch',
            async () =>
                new Response(
                    JSON.stringify({
                        success: true,
                        error_message: '',
                        bundles: [
                            {
                                code: 'party_essentials',
                                name: 'Party essentials',
                                level: 'tenant',
                                dataset_count: 2,
                                members: [],
                            },
                        ],
                    }),
                    { status: 200 },
                ),
        );

        const bundles = await readBundleRoster();

        expect(bundles.map((one) => one.code)).toEqual(['party_essentials']);
        expect(bundles[0].level).toBe('tenant');
    });

    it('asks for the members of the bundle it names', async () => {
        const asked: string[] = [];
        vi.stubGlobal('fetch', async (url: string) => {
            asked.push(url);
            return new Response(JSON.stringify({ success: true, error_message: '', bundles: [] }), {
                status: 200,
            });
        });

        await readBundleRoster('world');

        expect(asked[0]).toContain('bundle_code=world');
    });

    it('reports what the server said when the read fails', async () => {
        vi.stubGlobal(
            'fetch',
            async () =>
                new Response(
                    JSON.stringify({
                        success: false,
                        error_message: 'The store is not reachable.',
                        bundles: [],
                    }),
                    { status: 200 },
                ),
        );

        await expect(readBundleRoster()).rejects.toThrow('The store is not reachable.');
    });
});

describe('why a bundle member is not ready', () => {
    const member: BundleRosterMember = {
        display_order: 1,
        dataset_code: 'refdata.conventions',
        dataset_name: 'Conventions',
        artefact_type: 'conventions',
        artefact_table: 'dq_conventions_artefact_tbl',
        target_subject: 'refdata.v1.ops.publish',
        has_dataset: true,
        has_artefact_type: true,
        staged_count: 12,
        is_publishable: true,
        optional: false,
    };

    it('names the missing dataset', () => {
        expect(reasonOf({ ...member, has_dataset: false })).toBe('no dataset');
    });

    it('names the missing artefact type', () => {
        expect(reasonOf({ ...member, has_artefact_type: false })).toBe('no artefact type');
    });

    it('names the missing target subject', () => {
        expect(reasonOf({ ...member, target_subject: '' })).toBe('no target subject');
    });

    it('names the member whose table staged no rows', () => {
        expect(reasonOf({ ...member, staged_count: 0 })).toBe('no staged rows');
    });

    it('is ready when the dataset, its type, its subject and its rows are there', () => {
        expect(reasonOf(member)).toBeNull();
    });
});
