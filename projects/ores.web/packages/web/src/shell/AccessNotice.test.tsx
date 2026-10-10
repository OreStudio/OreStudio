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
import { TranslationProvider } from '../i18n/Provider.js';
import type { AccessState } from '../access/holds.js';
import { AccessNoticeFor } from './AccessNotice.js';

/**
 * The notice that tells a person their permissions were not read, so that a
 * menu with parts missing is not taken for a menu they may not use.
 */

function render(state: AccessState): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <AccessNoticeFor state={state} />
        </TranslationProvider>,
    );
}

describe('the notice about a failed read of access', () => {
    it('says nothing when the permissions were read', () => {
        expect(render({ kind: 'ready' })).toBe('');
    });

    it('states the failure in the server words and offers Retry', () => {
        const html = render({
            kind: 'failed',
            message: 'Request to get_my_roles timed out after 30000ms',
            retry: () => undefined,
        });

        expect(html).toContain('Your access could not be read');
        expect(html).toContain('timed out after 30000ms');
        expect(html).toContain('Retry');
    });

    it('says the server is slow while a failed read is tried again', () => {
        expect(render({ kind: 'slow' })).toContain('The server is slow to answer');
    });
});
