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

import type { ReactNode } from 'react';
import { Button, Tag } from './Primitives.js';
import { useTranslation } from '../i18n/Provider.js';

/** A party a person may choose, by the name they know it by. */
export interface PartyOption {
    readonly id: string;
    readonly name: string;
}

/**
 * The one way a person chooses a party.
 *
 * Signing in with an account that works in several parties draws it, and so
 * does changing party from the account menu, so the two never look or behave
 * differently. The caller owns what choosing does and what a refusal says; this
 * draws the choice and, where the person is already acting for one, marks it.
 */
export function PartyChoice({
    parties,
    onChoose,
    current,
}: {
    readonly parties: readonly PartyOption[];
    readonly onChoose: (party: PartyOption) => void;
    /** The party the session acts for now, which cannot be chosen again. */
    readonly current?: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <ul className="mt-4 space-y-2">
            {parties.map((party) => (
                <li key={party.id}>
                    <Button
                        variant="secondary"
                        className="w-full justify-between"
                        disabled={party.id === current}
                        onClick={() => onChoose(party)}
                    >
                        <span className="truncate">{party.name}</span>
                        {party.id === current && <Tag>{t('shell.menu.currentParty')}</Tag>}
                    </Button>
                </li>
            ))}
        </ul>
    );
}
