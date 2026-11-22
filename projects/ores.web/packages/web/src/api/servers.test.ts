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
import { baseForScreen, otherOf, peerOf, servingDeployment, type page } from './servers.js';

const PORTS = { cpp: '20401', bff: '20402' };

function at(port: string, path = '/', search = '', hash = ''): page {
    return { protocol: 'http:', hostname: 'localhost', port, pathname: path, search, hash };
}

describe('the server a screen asks', () => {
    it('asks the BFF for a screen that has not moved', () => {
        expect(baseForScreen('currencies', [])).toBe('');
    });

    it('asks the new server for a screen the deployment moved', () => {
        expect(baseForScreen('currencies', ['currencies'])).toBe('/cpp');
    });

    it('moves only the screens the flag names', () => {
        expect(baseForScreen('currencies', ['tenors'])).toBe('');
    });
});

describe('the server the page is on', () => {
    it('is the C++ server on its port', () => {
        expect(servingDeployment(PORTS, at('20401'))).toBe('cpp');
    });

    it('is the BFF on its port', () => {
        expect(servingDeployment(PORTS, at('20402'))).toBe('bff');
    });

    it('is neither on a port that serves no deployment', () => {
        expect(servingDeployment(PORTS, at('5173'))).toBeUndefined();
    });

    it('offers the other one, whichever it is', () => {
        expect(otherOf('cpp')).toBe('bff');
        expect(otherOf('bff')).toBe('cpp');
    });
});

describe('the way to the other server', () => {
    it('carries the screen the person is looking at', () => {
        expect(peerOf(PORTS, at('20402', '/market-data/quotes', '?after=7'))).toEqual({
            deployment: 'cpp',
            url: 'http://localhost:20401/market-data/quotes?after=7',
        });
    });

    it('goes back the way it came', () => {
        expect(peerOf(PORTS, at('20401', '/login'))).toEqual({
            deployment: 'bff',
            url: 'http://localhost:20402/login',
        });
    });

    it('offers nothing from a page no deployment serves', () => {
        expect(peerOf(PORTS, at('5173'))).toBeUndefined();
    });
});
