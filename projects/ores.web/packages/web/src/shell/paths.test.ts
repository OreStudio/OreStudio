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
import { AREA_PAGE } from './cards.js';
import { MOVED, PATHS, personPath, reportPath, rescuePath, rolePath } from './paths.js';

/**
 * Where each screen lives, and where the addresses it used to have lead.
 */

const CURRENT = new Set<string>(Object.values(PATHS));

describe('the addresses of the screens', () => {
    it('sit under their area', () => {
        // The areas are read from the registry, so adding an area does not need
        // a second list here to agree with it.
        const areas = new Set<string>(Object.values(AREA_PAGE));
        for (const [name, path] of Object.entries(PATHS)) {
            const under = [...areas].some((area) => path === area || path.startsWith(`${area}/`));
            expect(under, `${name}: ${path}`).toBe(true);
        }
        expect(PATHS.parties).toBe('/refdata/parties');
        expect(PATHS.rescue).toBe('/organisation/rescue');
        expect(PATHS.signIns).toBe('/operations/sign-ins');
        expect(PATHS.reporting).toBe('/reporting');
        expect(PATHS.reports).toBe('/reporting/reports');
        expect(PATHS.trading).toBe('/trading');
        expect(PATHS.trades).toBe('/trading/trades');
        expect(PATHS.dataQuality).toBe('/data-quality');
        expect(PATHS.dataQualityCoverage).toBe('/data-quality/coverage');
    });

    it('are named by one helper when they take a key', () => {
        expect(personPath('ada lovelace')).toBe('/organisation/people/ada%20lovelace');
        expect(rolePath('r1')).toBe('/organisation/roles/r1');
        expect(rescuePath('ada')).toBe('/organisation/rescue?username=ada');
        // A report definition is named by its id, because its name is shared
        // across the parties a session sees.
        expect(reportPath('55555555-5555-4555-8555-555555555555')).toBe(
            '/reporting/reports/55555555-5555-4555-8555-555555555555',
        );
    });

    it('leave no earlier address that is also a current one', () => {
        for (const [from] of MOVED) {
            expect(CURRENT.has(from), from).toBe(false);
        }
    });

    it('send every earlier address to a screen under an area', () => {
        for (const [, to] of MOVED) {
            expect(to).toMatch(/^\/(refdata|organisation|operations|market-data|reporting)\//);
        }
    });
});
