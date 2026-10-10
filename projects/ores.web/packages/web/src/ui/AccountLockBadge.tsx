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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import type { BadgePresentation } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { Label } from './Label.js';

/**
 * One badge of the shared catalogue, by its code.
 *
 * The catalogue is read once and shared with every screen that paints from it,
 * so a badge is drawn in the colours the deployment holds and never in a
 * colour a screen chose.
 */
export function useBadge(code: string): BadgePresentation | undefined {
    const catalogue = useQuery({ queryKey: ['labels'], queryFn: api.labels });
    return catalogue.data?.labels.find((label) => label.code === code);
}

/**
 * Whether an account is locked, drawn as the catalogue's `account_locked` code
 * domain draws it: the locked badge in red, the unlocked badge in green.
 */
export function AccountLockBadge({ locked }: { readonly locked: boolean }): ReactNode {
    const badge = useBadge(locked ? 'account_locked' : 'account_unlocked');
    return <Label text={badge?.label ?? (locked ? 'Locked' : 'Unlocked')} badge={badge} />;
}
