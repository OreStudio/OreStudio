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
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { ToastViewport, closeToast, toast } from './Toasts.js';

function html(): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <ToastViewport />
        </TranslationProvider>,
    );
}

function clear(): void {
    for (let id = 1; id < 200; id += 1) {
        closeToast(id);
    }
}

describe('toasts', () => {
    afterEach(() => {
        clear();
        vi.useRealTimers();
    });

    it('shows a message with a way to close it', () => {
        toast('Converted from Geometric to Arithmetic.');
        expect(html()).toContain('Converted from Geometric to Arithmetic.');
        expect(html()).toContain('aria-label="Close"');
    });

    it('does not show the same message twice', () => {
        toast('Same news.');
        toast('Same news.');
        expect(html().split('Same news.').length).toBe(2);
    });

    it('lets a newer message of a group take the place of the older one', () => {
        toast('Rises 1%.', 'info', 'summary');
        toast('Rises 2%.', 'info', 'summary');
        expect(html()).not.toContain('Rises 1%.');
        expect(html()).toContain('Rises 2%.');
    });

    it('lets information leave by itself and keeps a warning until it is closed', () => {
        vi.useFakeTimers();
        toast('News.');
        toast('Careful.', 'warn');
        vi.advanceTimersByTime(11_000);
        expect(html()).not.toContain('News.');
        expect(html()).toContain('Careful.');
    });
});
