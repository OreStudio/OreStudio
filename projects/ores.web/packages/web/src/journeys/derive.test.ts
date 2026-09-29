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
import {
    TENANT_CODE_MAX_LENGTH,
    codeFromName,
    emailFromPrincipal,
    hostnameFromCode,
    hostnameFromName,
    isTenantCode,
    codeAsTyped,
} from './derive.js';

/**
 * The proposals, against the shapes the server states: the code's rule is the
 * one the provisioning service holds every tenant to, and these cases are the
 * names a person actually types.
 */
describe('the code a name proposes', () => {
    it('lowercases, joins words with underscores and drops the rest', () => {
        expect(codeFromName('Barclays Bank PLC')).toBe('barclays_bank_plc');
        expect(codeFromName('Northwind  Capital')).toBe('northwind_capital');
        expect(codeFromName('ACME Corporation (UK) plc')).toBe('acme_corporation_uk_plc');
        expect(codeFromName('Banco  Santander - S.A.')).toBe('banco_santander_sa');
    });

    it('starts where the first letter is, because a code starts with one', () => {
        expect(codeFromName('123 Northwind')).toBe('northwind');
        expect(codeFromName('   Leading spaces')).toBe('leading_spaces');
    });

    it('stops at the length the server accepts', () => {
        const long = codeFromName('a'.repeat(120));

        expect(long).toHaveLength(TENANT_CODE_MAX_LENGTH);
        expect(isTenantCode(long)).toBe(true);
    });

    it('proposes nothing for a name with no letters at all', () => {
        expect(codeFromName('123 456')).toBe('');
        expect(codeFromName('')).toBe('');
    });

    it('proposes a code the server accepts, whatever the name was', () => {
        for (const name of [
            'Barclays Bank PLC',
            'Banco  Santander - S.A.',
            '123 x',
            'Ünïcödé Ltd',
        ]) {
            const code = codeFromName(name);
            if (code !== '') {
                expect(isTenantCode(code)).toBe(true);
            }
        }
    });
});

describe('the shapes the server states', () => {
    it('accepts a lowercase letter first, then letters, digits and underscores', () => {
        expect(isTenantCode('northwind')).toBe(true);
        expect(isTenantCode('northwind_2')).toBe(true);
        expect(isTenantCode('a')).toBe(true);
    });

    it('refuses a code that breaks the shape', () => {
        expect(isTenantCode('')).toBe(false);
        expect(isTenantCode('Northwind')).toBe(false);
        expect(isTenantCode('northwind-capital')).toBe(false);
        expect(isTenantCode('2northwind')).toBe(false);
        expect(isTenantCode('northwind capital')).toBe(false);
        expect(isTenantCode('a'.repeat(TENANT_CODE_MAX_LENGTH + 1))).toBe(false);
    });
});

describe('the hostname and the address a tenant proposes', () => {
    it('serves the tenant on its own code', () => {
        expect(hostnameFromCode('barclays_bank_plc')).toBe('barclays_bank_plc');
    });

    it('serves a tenant built around an entity at the entity\u2019s own name', () => {
        expect(hostnameFromName('BARCLAYS PLC')).toBe('barclaysplc.com');
        expect(hostnameFromName('Banco Santander - S.A.')).toBe('bancosantandersa.com');
        expect(hostnameFromName('  Acme  Corporation  ')).toBe('acmecorporation.com');
        expect(hostnameFromName('123')).toBe('123.com');
        expect(hostnameFromName('---')).toBe('');
    });

    it('shapes a code to what the server accepts as somebody types it', () => {
        expect(codeAsTyped('Barclays PLC')).toBe('barclaysplc');
        expect(codeAsTyped('barclays-bank')).toBe('barclaysbank');
        expect(codeAsTyped('barclays_bank_2')).toBe('barclays_bank_2');
        // A code starts with a letter, so leading digits and underscores go.
        expect(codeAsTyped('2barclays')).toBe('barclays');
        expect(codeAsTyped('__barclays')).toBe('barclays');
        expect(codeAsTyped('')).toBe('');
        expect(codeAsTyped('123')).toBe('');
        // Nothing the server would refuse survives, including its length.
        expect(codeAsTyped('a'.repeat(80)).length).toBe(50);
        expect(isTenantCode(codeAsTyped('Émile & Co. (2)'))).toBe(true);
    });

    it('addresses the administrator at the tenant it administers', () => {
        expect(emailFromPrincipal('tenant_admin', 'barclaysplc.com')).toBe(
            'tenant_admin@barclaysplc.com',
        );
        expect(emailFromPrincipal('', 'barclays_bank_plc')).toBe('');
        expect(emailFromPrincipal('tenant_admin', '')).toBe('');
    });
});
