/* -*- mode: typescript; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
import { describe, expect, it } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { ApprovalsModeNotice } from './AppShell.js';

function notice(autoApproved: boolean, scope = 'tenant'): string {
    const client = new QueryClient();
    client.setQueryData(['approvals-mode'], {
        autoApproved,
        scope,
        setBy: 'tenant_admin',
        setAt: '2026-11-09 18:28:49Z',
        reason: 'Provisioning runs without waiting on approvals.',
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <ApprovalsModeNotice />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('ApprovalsModeNotice', () => {
    it('says who turned auto-approval on, when and why', () => {
        const html = notice(true);
        expect(html).toContain('Auto-approval is on in this tenant');
        expect(html).toContain('tenant_admin');
        expect(html).toContain('2026-11-09 18:28:49Z');
        expect(html).toContain('Provisioning runs without waiting on approvals.');
    });

    it('names the installation when the tenant did not set it', () => {
        expect(notice(true, 'installation')).toContain('for this installation');
    });

    it('says nothing while people decide', () => {
        expect(notice(false)).toBe('');
    });
});
