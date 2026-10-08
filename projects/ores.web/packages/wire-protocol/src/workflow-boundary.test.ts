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

import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { AuthenticatedCaller } from './account-operations.js';
import { WireCodec } from './codec.js';
import {
    PROVISION_TENANT_TARGET_KIND,
    PROVISION_TENANT_WORKFLOW_TYPE,
    readTenantSetups,
} from './tenants.js';

/**
 * The bytes the roster's run read puts on the wire, held against C++.
 *
 * The fixture is the request this client sends, encoded as the deployment
 * encodes it. The workflow api's own tests decode the same file into the
 * generated C++ struct, so a field one side has and the other lacks fails a
 * test on one side or the other instead of failing every request at run time.
 *
 * Set ORES_UPDATE_FIXTURES=1 to rewrite the fixture after a deliberate change,
 * and run the C++ test against it before committing.
 */

const FIXTURE = resolve(
    dirname(fileURLToPath(import.meta.url)),
    '../../../../ores.workflow/api/tests/fixtures/list_workflow_instance_summaries_request.msgpack.hex',
);

/*
 * The provisioning run's type and target kind are values ores.iam states in
 * C++ and no model declares, so the roster copies them. The header is read
 * here so that a rename on either side fails a test.
 */
const IAM_WORKFLOW_HEADER = resolve(
    dirname(fileURLToPath(import.meta.url)),
    '../../../../ores.iam/api/include/ores.iam.api/workflow/provision_tenant_workflow.hpp',
);

function toHex(bytes: Uint8Array): string {
    return Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('');
}

describe('the workflow wire boundary', () => {
    it('sends the instances list request the C++ fixture holds', async () => {
        let sent: unknown;
        const caller = {
            async callAuthenticated(
                _subject: string,
                body: unknown,
                schema: { parse: (value: unknown) => unknown },
            ): Promise<unknown> {
                sent = body;
                return schema.parse({ success: true, message: '', instances: [] });
            },
        } as unknown as AuthenticatedCaller;

        await readTenantSetups(caller, ['11111111-1111-1111-1111-111111111111'], async () => 0);
        const encoded = `${toHex(new WireCodec('msgpack').encode(sent))}\n`;

        if (process.env['ORES_UPDATE_FIXTURES'] === '1') {
            writeFileSync(FIXTURE, encoded);
        }
        expect(encoded).toBe(readFileSync(FIXTURE, 'utf8'));
    });

    it('names the provisioning run the way ores.iam does', () => {
        const header = readFileSync(IAM_WORKFLOW_HEADER, 'utf8');

        expect(header).toContain(
            `provision_tenant_workflow_type = "${PROVISION_TENANT_WORKFLOW_TYPE}";`,
        );
        expect(header).toContain(
            `provision_tenant_target_kind = "${PROVISION_TENANT_TARGET_KIND}";`,
        );
    });
});
