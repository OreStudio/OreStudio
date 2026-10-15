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

import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../../i18n/Provider.js';
import type { BindingStatus } from './bindingView.js';
import { StatusTag } from './StatusTag.js';

const draw = (status: BindingStatus | undefined): string =>
    renderToStaticMarkup(
        <TranslationProvider>
            <StatusTag status={status} />
        </TranslationProvider>,
    );

describe('StatusTag', () => {
    it('says each status in words', () => {
        expect(draw('running')).toContain('Running');
        expect(draw('stopped')).toContain('Stopped');
        expect(draw('disabled')).toContain('Disabled');
        expect(draw('quiet')).toContain('Quiet');
        expect(draw('unsupported')).toContain('Not supported');
    });

    it('draws nothing while the status is not known', () => {
        expect(draw(undefined)).toBe('');
    });
});
