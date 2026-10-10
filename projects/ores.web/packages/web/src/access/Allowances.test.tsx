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
import { renderToStaticMarkup } from 'react-dom/server';
import type { HeldRole, PermissionEntry } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { CanIPanel, RolesAllow } from './Allowances.js';

const COMPONENTS = ['alpha', 'bravo', 'charlie', 'delta', 'echo', 'foxtrot', 'golf'];

const catalogue: readonly PermissionEntry[] = COMPONENTS.flatMap((component) => [
    { code: `${component}::things:read`, description: `Read ${component} things` },
    { code: `${component}::things:write`, description: `Write ${component} things` },
]);

const role: HeldRole = {
    roleId: 'r',
    name: 'Trading',
    description: '',
    permissionCodes: COMPONENTS.map((component) => `${component}::*`),
    givenBy: 'system',
    givenAt: '2026-10-05 09:30:00Z',
    reasonCode: 'access.initial',
    commentary: '',
};

function render(node: React.ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

/** One area with forty resources: the shape of reference data, which one role can grant whole. */
const BIG_AREA: readonly PermissionEntry[] = Array.from({ length: 40 }, (_unused, index) => {
    const name = `thing_${String(index + 1).padStart(2, '0')}`;
    return { code: `refdata::${name}:read`, description: `Read ${name}` };
});

const refdataRole: HeldRole = { ...role, permissionCodes: ['refdata::*'] };

function shownResources(html: string): number {
    return (html.match(/>thing \d\d</g) ?? []).length;
}

describe('RolesAllow', () => {
    it('pages the permissions of a single area, fifteen at a time', () => {
        const html = render(<RolesAllow roles={[refdataRole]} catalogue={BIG_AREA} />);

        expect(shownResources(html)).toBe(15);
        expect(html).toContain('Showing 1–15 of 40 permissions');
    });

    it('offers the standard page sizes', () => {
        const html = render(<RolesAllow roles={[refdataRole]} catalogue={BIG_AREA} />);

        expect(html).toContain('value="25"');
        expect(html).toContain('value="100"');
    });

    it('offers every area to choose from', () => {
        const html = render(<RolesAllow roles={[role]} catalogue={catalogue} />);

        for (const component of COMPONENTS) {
            expect(html).toContain(`value="${component}"`);
        }
    });

    it('says everything when a role grants everything', () => {
        const everything = { ...role, permissionCodes: ['*'] };
        const html = render(<RolesAllow roles={[everything]} catalogue={catalogue} />);

        expect(html).toContain('lets you do everything');
        expect(html).not.toContain('<details');
    });
});

describe('CanIPanel', () => {
    it('is the question box, for any set of roles', () => {
        const html = render(<CanIPanel roles={[role]} catalogue={catalogue} />);

        expect(html).toContain('type="search"');
    });
});
