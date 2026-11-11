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
import { api } from './client.js';

/**
 * The calibration of an asset class as either server answers it. The BFF states
 * a class with no policy row with a null version; the C++ server leaves the
 * version out, as its codec leaves out an empty optional.
 */

const policy = {
    from_policy_row: false,
    window_days: 90,
    allow_inverse: true,
    fallback_code: 'keep_fixed',
    provider_order: [],
};

function answering(body: unknown): void {
    vi.stubGlobal(
        'fetch',
        async () =>
            new Response(JSON.stringify(body), {
                status: 200,
                headers: { 'content-type': 'application/json' },
            }),
    );
}

const view = (stated: Record<string, unknown>) => ({
    assetClass: 'fx',
    policy: { ...policy, ...stated },
    policyWritable: false,
    feeds: [],
    coverage: { total: 0, fromFixing: 0, keptFixed: 0, refused: 0, overridden: 0 },
});

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('the calibration of an asset class', () => {
    it('reads a version the server leaves out as null', async () => {
        answering(view({}));
        expect((await api.calibration('fx')).policy.version).toBeNull();
    });

    it('reads a null version as null', async () => {
        answering(view({ version: null }));
        expect((await api.calibration('fx')).policy.version).toBeNull();
    });

    it('reads a stated version', async () => {
        answering(view({ from_policy_row: true, version: 4 }));
        expect((await api.calibration('fx')).policy.version).toBe(4);
    });
});
