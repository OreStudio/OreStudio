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
import { administratorPassword, detailsFor, provisionRequest, tenantPrincipal } from './state.js';
import type { SeedProfileChoice } from '@ores/wire-protocol/browser';

const empty: SeedProfileChoice = {
    code: 'empty_operational',
    name: 'Empty operational',
    summary: '',
    audience: 'Production',
    bullets: [],
    tenant: {
        name: '',
        code: '',
        hostname: '',
        adminUsername: '',
        adminEmail: '',
    },
    inheritsAdminPassword: false,
    forcePasswordChange: true,
    order: 1,
    steps: [
        { kind: 'provision_party', order: 1 },
        { kind: 'publish_bundle', order: 2 },
    ],
    parameters: [
        {
            name: 'root_lei',
            label: 'Root LEI',
            dataType: 'string',
            choices: [],
            defaultValue: '',
            required: true,
            hint: '',
            order: 1,
        },
        {
            name: 'counterparty_size',
            label: 'Counterparty set',
            dataType: 'choice',
            choices: ['small', 'large'],
            defaultValue: 'large',
            required: false,
            hint: '',
            order: 2,
        },
    ],
};

const acme: SeedProfileChoice = {
    ...empty,
    code: 'acme_demo',
    name: 'ACME demo',
    tenant: {
        name: 'ACME Corporation',
        code: 'acme',
        hostname: 'acme.example.com',
        adminUsername: 'acme_admin',
        adminEmail: 'acme_admin@acme.example.com',
    },
    inheritsAdminPassword: true,
    forcePasswordChange: false,
    parameters: [],
};

describe('what a profile proposes', () => {
    it('starts the tenant fields empty when the profile declares none', () => {
        expect(detailsFor(empty)).toMatchObject({
            name: '',
            code: '',
            hostname: '',
            useMyPassword: false,
        });
    });

    it('starts them at the profile\u2019s own values when it declares them', () => {
        expect(detailsFor(acme)).toMatchObject({
            name: 'ACME Corporation',
            code: 'acme',
            hostname: 'acme.example.com',
            adminUsername: 'acme_admin',
        });
    });

    it('takes the profile\u2019s answer on reusing the creating password', () => {
        expect(detailsFor(acme).useMyPassword).toBe(true);
        expect(detailsFor(empty).useMyPassword).toBe(false);
    });

    it('fills each declared setting with the default the profile states', () => {
        expect(detailsFor(empty).parameters).toEqual({
            root_lei: '',
            counterparty_size: 'large',
        });
    });
});

describe('the password a tenant administrator signs in with', () => {
    it('is the one the creating administrator typed when the profile reuses it', () => {
        const details = detailsFor(acme);
        expect(administratorPassword(details, 'Issued-Password-1')).toBe('Issued-Password-1');
    });

    it('is the one typed for the tenant when the profile does not', () => {
        const details = { ...detailsFor(empty), adminPassword: 'Chosen-Password-2' };
        expect(administratorPassword(details, 'Issued-Password-1')).toBe('Chosen-Password-2');
    });
});

describe('the request a described tenant becomes', () => {
    it('carries every field the provision verb declares', () => {
        const details = {
            ...detailsFor(empty),
            name: 'Northwind Capital',
            code: 'northwind',
            hostname: 'northwind.example.com',
            adminUsername: 'northwind_admin',
            adminEmail: 'admin@northwind.example.com',
            adminPassword: 'Chosen-Password-2',
            parameters: { root_lei: '9695ACMEGROUP0000030', counterparty_size: 'small' },
        };

        expect(provisionRequest(empty, details, 'Issued-Password-1')).toEqual({
            profileCode: 'empty_operational',
            tenantCode: 'northwind',
            tenantName: 'Northwind Capital',
            tenantHostname: 'northwind.example.com',
            tenantDescription: '',
            adminUsername: 'northwind_admin',
            adminEmail: 'admin@northwind.example.com',
            adminPassword: 'Chosen-Password-2',
            parameters: { root_lei: '9695ACMEGROUP0000030', counterparty_size: 'small' },
        });
    });

    it('states no tenant type, because the starting point already did', () => {
        const details = detailsFor(acme);
        const request = provisionRequest(acme, details, 'Issued-Password-1');

        expect(Object.keys(request)).not.toContain('tenantType');
    });
});

describe('the principal a tenant administrator signs in with', () => {
    it('names the hostname the tenant is served on', () => {
        expect(tenantPrincipal(detailsFor(acme))).toBe('acme_admin@acme.example.com');
    });

    it('is the bare username when the tenant has no hostname yet', () => {
        expect(tenantPrincipal(detailsFor(empty))).toBe('');
        expect(tenantPrincipal({ ...detailsFor(empty), adminUsername: 'northwind_admin' })).toBe(
            'northwind_admin',
        );
    });
});
