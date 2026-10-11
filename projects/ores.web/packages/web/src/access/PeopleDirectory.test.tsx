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

import { describe, expect, it } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { usePeopleDirectory } from './PersonRef.js';

function kindsOf(
    permissionCodes: string[],
    accounts: { username: string; accountType: string }[] | undefined,
    names: string[],
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], { roles: [{ roleId: 'r', permissionCodes }] });
    if (accounts !== undefined) {
        client.setQueryData(['accounts'], {
            accounts: accounts.map((a, i) => ({ id: `id-${i}`, fullName: a.username, ...a })),
            totalCount: accounts.length,
        });
    }
    function Probe() {
        const { isService } = usePeopleDirectory();
        return <>{names.map((n) => `${n}=${String(isService?.(n))}`).join(',')}</>;
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <Probe />
        </QueryClientProvider>,
    );
}

describe('telling a service from a person', () => {
    const accounts = [
        { username: 'refdata_service', accountType: 'service' },
        { username: 'model_runner', accountType: 'algorithm' },
        { username: 'assistant', accountType: 'llm' },
        { username: 'tenant_admin', accountType: 'user' },
    ];

    it('counts every account type except a user as system activity', () => {
        expect(
            kindsOf(['iam::accounts:read'], accounts, [
                'refdata_service',
                'model_runner',
                'assistant',
                'tenant_admin',
            ]),
        ).toBe('refdata_service=true,model_runner=true,assistant=true,tenant_admin=false');
    });

    it('does not call a name it does not know a service', () => {
        expect(kindsOf(['iam::accounts:read'], accounts, ['someone.else'])).toBe(
            'someone.else=false',
        );
    });

    it('cannot tell for a reader who may not read accounts', () => {
        expect(kindsOf(['iam::organisation:read'], undefined, ['refdata_service'])).toBe(
            'refdata_service=undefined',
        );
    });
});
