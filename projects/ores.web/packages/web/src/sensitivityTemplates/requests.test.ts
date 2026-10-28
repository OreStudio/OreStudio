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
import { api } from '../api/client.js';
import { MENU, offered } from '../shell/areas.js';
import { CARDS, AREA_CARDS } from '../shell/cards.js';
import { PATHS, sensitivityTemplatePath } from '../shell/paths.js';

/**
 * The routes and the place of the sensitivity template screens.
 *
 * A create refused for a code the tenant holds has to reach the person in the
 * server's own words, and the screen has to be reachable where a person looks
 * for sensitivity data: the reporting area, beside product pricing and the
 * engine run settings.
 */

const ROW = {
    code: 'IR_FD',
    methodCode: 'FD',
    methodName: 'Finite difference',
    assetClassCode: 'interest_rates',
    assetClassName: 'Interest rate',
    templatePrefix: 'IR',
    description: 'Interest rate products priced on a finite difference grid.',
    version: 1,
    modifiedBy: 'priya',
    performedBy: 'ores_analytics_service',
    reasonCode: 'system.new_record',
    commentary: '',
    recordedAt: '2026-10-04 09:05:00.000000Z',
};

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('the address of one template', () => {
    it('is named by one helper, so a screen never writes it as text', () => {
        expect(PATHS.sensitivityTemplates).toBe('/reporting/sensitivity-templates');
        expect(sensitivityTemplatePath('IR_FD')).toBe('/reporting/sensitivity-templates/IR_FD');
    });

    it('encodes the code, so it cannot leave the route', () => {
        expect(sensitivityTemplatePath('IR FD')).toBe('/reporting/sensitivity-templates/IR%20FD');
    });
});

describe('a create the server refused', () => {
    it('carries the server words, and names the code it refused', async () => {
        const asked: { path: string; method: string; body: string }[] = [];
        vi.stubGlobal('fetch', async (path: string, init: RequestInit) => {
            asked.push({ path, method: init.method ?? '', body: String(init.body ?? '') });
            return new Response(
                JSON.stringify({
                    code: 'conflict',
                    message: 'A sensitivity template with this code already exists.',
                }),
                { status: 409, headers: { 'content-type': 'application/json' } },
            );
        });

        await expect(
            api.addSensitivityTemplate({
                code: 'IR_FD',
                methodCode: 'FD',
                assetClassCode: 'interest_rates',
                description: '',
                reasonCode: 'system.new_record',
                commentary: '',
            }),
        ).rejects.toThrow('A sensitivity template with this code already exists.');
        expect(asked[0]?.path).toBe('/api/analytics/sensitivity-templates');
        expect(asked[0]?.method).toBe('POST');
        expect(JSON.parse(asked[0]?.body ?? '{}')).toEqual({
            code: 'IR_FD',
            methodCode: 'FD',
            assetClassCode: 'interest_rates',
            description: '',
            reasonCode: 'system.new_record',
            commentary: '',
        });
    });

    it('sends the chosen codes, and no words a person typed', async () => {
        const sent: string[] = [];
        vi.stubGlobal('fetch', async (_path: string, init: RequestInit) => {
            sent.push(String(init.body ?? ''));
            return new Response(JSON.stringify({ template: ROW }), {
                status: 201,
                headers: { 'content-type': 'application/json' },
            });
        });

        await api.addSensitivityTemplate({
            code: 'IR_FD',
            methodCode: 'FD',
            assetClassCode: 'interest_rates',
            description: '',
            reasonCode: 'system.new_record',
            commentary: '',
        });
        const body = JSON.parse(sent[0] ?? '{}') as Record<string, unknown>;
        expect(body['methodCode']).toBe('FD');
        expect(body['assetClassCode']).toBe('interest_rates');
        // The words a person reads live in the lookups, so they never travel.
        expect(body).not.toHaveProperty('method');
        expect(body).not.toHaveProperty('assetClass');
        expect(body).not.toHaveProperty('method_code');
    });

    it('sends no asset class for a trade-level template', async () => {
        const sent: string[] = [];
        vi.stubGlobal('fetch', async (_path: string, init: RequestInit) => {
            sent.push(String(init.body ?? ''));
            return new Response(JSON.stringify({ template: ROW }), {
                status: 201,
                headers: { 'content-type': 'application/json' },
            });
        });

        await api.addSensitivityTemplate({
            code: 'ACME_Trade',
            methodCode: 'Trade',
            assetClassCode: null,
            description: '',
            reasonCode: 'system.new_record',
            commentary: '',
        });
        expect(
            (JSON.parse(sent[0] ?? '{}') as Record<string, unknown>)['assetClassCode'],
        ).toBeNull();
    });
});

describe('a removal', () => {
    it('answers the row the server removed', async () => {
        vi.stubGlobal(
            'fetch',
            async () =>
                new Response(JSON.stringify({ removed: ROW }), {
                    status: 200,
                    headers: { 'content-type': 'application/json' },
                }),
        );

        const removed = await api.removeSensitivityTemplate('IR_FD', {
            version: 1,
            reasonCode: 'common.rectification',
            commentary: 'Seeded in error',
        });
        expect(removed.code).toBe('IR_FD');
        expect(removed.methodCode).toBe('FD');
    });
});

describe('the place of the screen', () => {
    it('sits in the reporting area, beside product pricing and the engine run settings', () => {
        expect(AREA_CARDS.reporting).toEqual([
            'reports',
            'pricingEngines',
            'sensitivityTemplates',
            'curveConfigurations',
            'conventions',
            'engineSettings',
        ]);
        expect(CARDS.sensitivityTemplates.to).toBe(PATHS.sensitivityTemplates);
        expect(CARDS.sensitivityTemplates.screenTitleKey).toBe('sensitivityTemplates.title');
    });

    it('opens the reporting area for a person who may read the templates', () => {
        const reporting = MENU.find((item) => item.to === PATHS.reporting);
        expect(reporting).toBeDefined();
        expect(
            offered(reporting!, (code) => code === 'analytics::sensitivity_templates:read'),
        ).toBe(true);
        expect(offered(reporting!, (code) => code === 'refdata::parties:read')).toBe(false);
    });
});
