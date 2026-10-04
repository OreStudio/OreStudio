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

import { describe, expect, it } from 'vitest';
import type { AuthenticatedCaller } from './account-operations.js';
import { readTenantSetups, TENANT_SETUP_READ_LIMIT } from './tenants.js';

/**
 * The roster's run read, against a caller that answers what it is given.
 *
 * The engine answers the run that changed last first. The read keeps the first
 * run it sees for each tenant, so the order is what decides which run a row
 * reports, and these cases pin it.
 */

const ACME = '44444444-4444-4444-4444-444444444444';
const NORTHWIND = '77777777-7777-7777-7777-777777777777';

function run(id: string, target: string, status: string): unknown {
    return {
        id,
        type: 'provision_tenant_workflow',
        status,
        current_step_index: 1,
        step_count: 4,
        created_at: '',
        error: '',
        target_kind: 'tenant',
        target_id: target,
    };
}

function callerAnswering(instances: readonly unknown[]): AuthenticatedCaller {
    return {
        async callAuthenticated(
            _subject: string,
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            return schema.parse({ success: true, message: '', instances });
        },
    } as unknown as AuthenticatedCaller;
}

describe('the tenant setup read', () => {
    it('keeps the first run the engine answers for each tenant', async () => {
        const read = await readTenantSetups(
            callerAnswering([
                run('run-latest', ACME, 'failed'),
                run('run-older', ACME, 'completed'),
                run('run-other', NORTHWIND, 'in_progress'),
            ]),
            [ACME, NORTHWIND],
        );

        expect(read.setups.get(ACME)?.instanceId).toBe('run-latest');
        expect(read.setups.get(NORTHWIND)?.instanceId).toBe('run-other');
        expect(read.complete).toBe(true);
    });

    it('ignores a run that names no target', async () => {
        const read = await readTenantSetups(callerAnswering([run('untargeted', '', 'failed')]), [
            ACME,
        ]);

        expect(read.setups.size).toBe(0);
    });

    /*
     * An answer that reaches the limit may have cut off the oldest runs, and
     * a tenant whose only run is among them would show no setup. The read
     * says so, so the caller can report it.
     */
    it('says the read is incomplete when the answer reaches the limit', async () => {
        const runs = Array.from({ length: TENANT_SETUP_READ_LIMIT }, (_, index) =>
            run(`run-${index}`, ACME, 'completed'),
        );

        const read = await readTenantSetups(callerAnswering(runs), [ACME]);

        expect(read.complete).toBe(false);
    });

    /*
     * An empty target list asks for every run, so a page with no tenants on
     * it asks for nothing.
     */
    it('reads nothing when no tenant is named', async () => {
        let called = false;
        const caller = {
            async callAuthenticated(): Promise<unknown> {
                called = true;
                return undefined;
            },
        } as unknown as AuthenticatedCaller;

        const read = await readTenantSetups(caller, []);

        expect(called).toBe(false);
        expect(read.setups.size).toBe(0);
        expect(read.complete).toBe(true);
    });
});
