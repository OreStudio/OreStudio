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
import type { CutRow, CutStateWord } from '@ores/wire-protocol/browser';
import { useTranslation } from '../../i18n/Provider.js';
import { useBadge } from '../../ui/AccountLockBadge.js';
import { Label } from '../../ui/Label.js';

/**
 * The parts the cut screens share: the state as the catalogue paints it, the
 * basis under it, the indicative mark, and the grouping a type's cuts read by.
 *
 * A cut states no state and no label, and the server derives both. These parts
 * therefore print the word they were given and derive nothing about the cut:
 * what they do derive is the sentence under the badge, and that sentence is the
 * screen's own.
 */

/**
 * The badge each state wears.
 *
 * The codes are the shared catalogue's own vocabulary, so the seven states are
 * painted in the colours the deployment holds: grey for a record that is
 * inactive, blue for one that waits on a person, green for one that is active
 * and in order, and red for one that failed its own standard.
 */
const STATE_BADGES: Readonly<Record<CutStateWord, string>> = {
    draft: 'fsm_draft',
    incomplete: 'outcome_failed',
    taken: 'pending',
    in_use: 'active',
    authorised: 'active',
    refused: 'outcome_failed',
    superseded: 'inactive',
};

/** The badge an indicative value wears, which is the kind's fact and not the state's. */
const INDICATIVE_BADGE = 'nature_estimated';

/** The catalogue's fallback badge code, drawn for a value with no definition of its own. */
const UNMAPPED = '__unmapped__';

/** The name of a state, in the language the person reads. */
export function useStateName(): (state: CutStateWord) => string {
    const { t } = useTranslation();
    return (state) => t(`cuts.states.${state}`);
}

/** One state, in the badge the catalogue gives it. */
export function CutStateTag({ state }: { readonly state: CutStateWord }): ReactNode {
    const stateName = useStateName();
    const badge = useBadge(STATE_BADGES[state] ?? UNMAPPED);
    return <Label text={stateName(state)} badge={badge} />;
}

/**
 * The fact that qualifies the state, drawn under its badge.
 *
 * A superseded cut says which body replaced it, a taken cut says that it waits
 * on a person, and a cut in use says that its kind required no authorisation.
 * The basis is a sentence about the facts, so it is derived here and never
 * stored; a state with nothing to add says nothing.
 */
export function StateBasis({ row }: { readonly row: CutRow }): ReactNode {
    const { t } = useTranslation();
    const basis = ((): string => {
        switch (row.state) {
            case 'taken':
                return t('cuts.basis.taken');
            case 'in_use':
                return t('cuts.basis.inUse');
            case 'authorised':
                return row.approvedBy === ''
                    ? t('cuts.basis.authorised')
                    : t('cuts.basis.authorisedBy', { who: row.approvedBy });
            case 'incomplete':
                return t('cuts.basis.incomplete');
            case 'refused':
                return t('cuts.basis.refused');
            case 'superseded':
                return t('cuts.basis.superseded');
            case 'draft':
                return t('cuts.basis.draft');
        }
    })();
    return <span className="text-xs text-ink-faint">{basis}</span>;
}

/**
 * The indicative mark, drawn beside the state and never inside it.
 *
 * The mark is the kind's and not the state's: a flash cut is in use and carries
 * it, while the start of day kind requires no authorisation either and carries
 * none. A mark that stood in the state column would say the two cuts stand
 * differently, and they do not.
 */
export function IndicativeTag(): ReactNode {
    const { t } = useTranslation();
    const badge = useBadge(INDICATIVE_BADGE);
    return <Label text={t('cuts.indicative')} badge={badge} />;
}

/** The state and its basis, stacked, as a list row draws them. */
export function StateCell({ row }: { readonly row: CutRow }): ReactNode {
    return (
        <span className="flex flex-col gap-1">
            <span className="flex items-center gap-1.5">
                <CutStateTag state={row.state} />
                {row.outputIsIndicative && <IndicativeTag />}
            </span>
            <StateBasis row={row} />
        </span>
    );
}

/** The mark of a cell with nothing to state, as every list draws it. */
export function Absent(): ReactNode {
    return (
        <span className="text-ink-faint" aria-hidden="true">
            —
        </span>
    );
}

/** A key that is an identifier, or whichever of two texts is present. */
export function Identity({ value }: { readonly value: string | null }): ReactNode {
    if (value === null || value === '') {
        return (
            <span className="text-ink-faint" aria-hidden="true">
                —
            </span>
        );
    }
    return <span className="font-mono text-xs break-all">{value}</span>;
}

/** A figure stated where the screen draws one, with the words it means. */
export function Reference({ label, value }: { readonly label: string; readonly value: string | null }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="min-w-0">
            <dt className="text-xs text-ink-faint">{label}</dt>
            <dd className="mt-0.5 text-sm">
                {value === null || value === '' ? (
                    <span className="text-ink-faint">{t('cuts.body.notFixed')}</span>
                ) : (
                    <span className="font-mono text-xs break-all">{value}</span>
                )}
            </dd>
        </div>
    );
}

/**
 * The month a cut's instant falls in, as `YYYY-MM`.
 *
 * The instant arrives as an ISO 8601 string and the month is its own first
 * seven characters, so a group's key needs no parsing of the clock.
 */
export function monthOf(row: CutRow): string {
    return row.timepoint.slice(0, 7);
}

/**
 * The rows grouped by the month of their instants, newest month first and newest
 * instant first inside it.
 *
 * The month is the period above a kind taken every day, which is what every kind
 * a firm seeds today is. A kind taken every month would read by year, and no read
 * states a kind's frequency yet, so the screen groups by the month.
 */
export function byMonth(rows: readonly CutRow[]): readonly {
    readonly month: string;
    readonly rows: readonly CutRow[];
}[] {
    const months = new Map<string, CutRow[]>();
    for (const row of rows) {
        const month = monthOf(row);
        const group = months.get(month);
        if (group === undefined) {
            months.set(month, [row]);
        } else {
            group.push(row);
        }
    }
    return [...months.entries()]
        .sort(([left], [right]) => right.localeCompare(left))
        .map(([month, group]) => ({
            month,
            rows: [...group].sort((left, right) => right.timepoint.localeCompare(left.timepoint)),
        }));
}
