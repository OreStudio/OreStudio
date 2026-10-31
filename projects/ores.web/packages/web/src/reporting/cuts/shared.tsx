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

import { useMemo, type ReactNode } from 'react';
import type { CutRecord, CutRow, CutStateWord } from '@ores/wire-protocol/browser';
import { useTranslation } from '../../i18n/Provider.js';
import { badgeFor, useLabelCatalogue } from '../../refdata/shared.js';
import { useBadge } from '../../ui/AccountLockBadge.js';
import { Label } from '../../ui/Label.js';

/**
 * The parts the cut screens share: the state as the catalogue paints it, the
 * basis under it, the marks the DQ label catalogue gives the words a kind holds,
 * and the grouping a type's cuts read by.
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
 * The words a cut kind's four decisions are read with, as DQ code domains.
 *
 * The platform states these words in its data quality catalogue and not in a
 * screen: a code domain holds the codes the rows carry, a badge definition
 * holds the paint, and a badge mapping joins the two. The domain is the same
 * code the lookup row carries, so a kind's family code and the words a report
 * prints about it are read from one place.
 */
const FAMILY_DOMAIN = 'cut_family';
const VALUES_DOMAIN = 'cut_values';
const AUTHORISATION_DOMAIN = 'cut_authorisation';

/**
 * One word of a kind's rules, painted by the DQ label catalogue.
 *
 * The catalogue owns the paint and the mapping from the code the row holds;
 * the word is this interface's, keyed on the same code, so it reads in the
 * language the person chose. A code the catalogue has not mapped is still
 * written, in its interface's words and with no pill, because hiding it would
 * hide the gap in the deployment's labels rather than show it.
 */
function CatalogueWord({
    domain,
    code,
    words,
}: {
    /** The code domain that owns the word, such as `cut_family`. */
    readonly domain: string;
    /** The code the row carries, which is the domain's entity code. */
    readonly code: string;
    /** The interface key the word is written under, in every language offered. */
    readonly words: string;
}): ReactNode {
    const { t } = useTranslation();
    const catalogue = useLabelCatalogue();
    const badge = badgeFor(catalogue, domain, code);
    const key = `${words}.${code}`;
    const word = t(key);
    // The translator echoes the key when nobody wrote the word, so a kind whose
    // code the interface does not know still reads as the catalogue names it.
    return <Label text={word === key ? (badge?.label ?? code) : word} badge={badge} />;
}

/** The family a kind of cut belongs to, as the label catalogue paints it. */
export function CutFamilyLabel({ code }: { readonly code: string }): ReactNode {
    return <CatalogueWord domain={FAMILY_DOMAIN} code={code} words="cuts.family" />;
}

/**
 * Whether a kind's values are official or indicative, as the catalogue paints it.
 *
 * The kind holds a boolean and the catalogue holds the two words, so the screen
 * names the value and never invents the tag that stands for it.
 */
export function CutValuesLabel({ indicative }: { readonly indicative: boolean }): ReactNode {
    return (
        <CatalogueWord
            domain={VALUES_DOMAIN}
            code={indicative ? 'indicative' : 'official'}
            words="cuts.values"
        />
    );
}

/**
 * The word a kind with no approver reads with, as the catalogue paints it.
 *
 * A kind that names an approval kind prints that kind's own name, which is the
 * lookup's; this word stands only for the absence of one.
 */
export function CutAuthorisationLabel(): ReactNode {
    return (
        <CatalogueWord domain={AUTHORISATION_DOMAIN} code="unsigned" words="cuts.authorisation" />
    );
}

/**
 * The facts a state's basis sentence is written from.
 *
 * It is what the sentence needs and no more: a list row and a record state the
 * same three facts, and neither is rebuilt into the other's shape to say them.
 */
export interface StateFacts {
    readonly state: CutStateWord;
    /** The person whose authorisation released the body, or empty where none did. */
    readonly approvedBy: string;
    /** The body that holds a superseded cut's place, or null where none is known. */
    readonly supersededByLabel: string | null;
}

/** The facts a list row states about its state. */
export function factsOfRow(row: CutRow): StateFacts {
    return { state: row.state, approvedBy: row.approvedBy, supersededByLabel: null };
}

/** The facts one cut's record states about its state, where the body that replaced it is known. */
export function factsOfRecord(record: CutRecord): StateFacts {
    return {
        state: record.state,
        approvedBy: record.cut.approvedBy,
        supersededByLabel: record.supersededByLabel,
    };
}

/**
 * The fact that qualifies the state, drawn under its badge.
 *
 * A superseded cut says whether it was authorised before a later cut replaced
 * it, and names the body that replaced it where the screen knows the label; a
 * taken cut says that it waits on a person, and a cut in use says that its kind
 * required no authorisation. The basis is a sentence about the facts, so it is
 * derived here and never stored; a state with nothing to add says nothing.
 */
export function StateBasis({ facts }: { readonly facts: StateFacts }): ReactNode {
    const { t } = useTranslation();
    const basis = ((): string => {
        switch (facts.state) {
            case 'taken':
                return t('cuts.basis.taken');
            case 'in_use':
                return t('cuts.basis.inUse');
            case 'authorised':
                return facts.approvedBy === ''
                    ? t('cuts.basis.authorised')
                    : t('cuts.basis.authorisedBy', { who: facts.approvedBy });
            case 'incomplete':
                return t('cuts.basis.incomplete');
            case 'refused':
                return t('cuts.basis.refused');
            case 'superseded':
                return facts.approvedBy === ''
                    ? t('cuts.basis.supersededInUse')
                    : t('cuts.basis.supersededAuthorised');
            case 'draft':
                return t('cuts.basis.draft');
        }
    })();
    return (
        <span className="flex flex-col gap-0.5 text-xs text-ink-faint">
            <span>{basis}</span>
            {facts.state === 'superseded' && facts.supersededByLabel !== null && (
                <span>{t('cuts.basis.replacedBy', { label: facts.supersededByLabel })}</span>
            )}
        </span>
    );
}

/**
 * The indicative mark, drawn beside the state and never inside it.
 *
 * The mark is the kind's and not the state's: a flash cut is in use and carries
 * it, while the start of day kind requires no authorisation either and carries
 * none. A mark that stood in the state column would say the two cuts stand
 * differently, and they do not. The word and the paint are the catalogue's, so
 * the mark reads the same as the values column that states it.
 */
export function IndicativeTag(): ReactNode {
    return <CutValuesLabel indicative />;
}

/** The state and its basis, stacked, as a list row draws them. */
export function StateCell({ row }: { readonly row: CutRow }): ReactNode {
    return (
        <span className="flex flex-col gap-1">
            <span className="flex items-center gap-1.5">
                <CutStateTag state={row.state} />
                {row.outputIsIndicative && <IndicativeTag />}
            </span>
            <StateBasis facts={factsOfRow(row)} />
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

/** A figure stated where the screen draws one, with the words it means. */
export function Reference({
    label,
    value,
}: {
    readonly label: string;
    readonly value: string | null;
}): ReactNode {
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
 * How often a kind of cut is taken, read from the rule a report resolves its
 * selection by.
 *
 * The rule is the kind's own declaration, so the frequency is derived and never
 * a column: a kind that resolves to the close of the day the report covers is
 * taken every day, the month end and the year end once a month and once a year,
 * and a kind whose instant the run or a process names answers to no schedule.
 */
export type CutFrequency = 'daily' | 'monthly' | 'yearly' | 'onDemand';

/** The frequency each resolution rule states. */
const FREQUENCY_OF_RULE: Readonly<Record<string, CutFrequency>> = {
    same_day_close: 'daily',
    same_day_open: 'daily',
    // A verification cut is built on the close of a day's end of day cut, so it
    // is taken at the same rate as the body it verifies.
    cut_it_is_built_on: 'daily',
    last_business_day_of_month_close: 'monthly',
    last_business_day_of_year_close: 'yearly',
    run_instant: 'onDemand',
    the_cut_named: 'onDemand',
};

/**
 * The frequency a kind is taken at, or the daily rate where the rule is one this
 * screen does not know: a rule it cannot read states no period, and a daily body
 * is the occasion the fixtures seed most of.
 */
export function frequencyOf(resolutionRuleCode: string): CutFrequency {
    return FREQUENCY_OF_RULE[resolutionRuleCode] ?? 'daily';
}

/** One group of a type's cuts: the period, the words it reads by and its rows. */
export interface CutGroup {
    readonly key: string;
    readonly rows: readonly CutRow[];
}

/** The period one instant falls in, as the frequency counts it. */
function groupKeyOf(instant: string, frequency: CutFrequency): string {
    switch (frequency) {
        case 'daily':
            return instant.slice(0, 7);
        case 'monthly':
            return instant.slice(0, 4);
        case 'onDemand':
            return instant.slice(0, 10);
        case 'yearly':
            return 'all';
    }
}

/**
 * The rows grouped by the period one step above the kind's own frequency.
 *
 * A daily kind reads by month, a monthly kind by year, a yearly kind as one
 * group, and an on-demand kind by day. Newest period first and newest instant
 * first inside it, so the cut in use leads. The grouping is the screen's: the
 * read answers the cuts and states no period of its own.
 */
export function groupsOf(rows: readonly CutRow[], frequency: CutFrequency): readonly CutGroup[] {
    const groups = new Map<string, CutRow[]>();
    for (const row of rows) {
        const key = groupKeyOf(row.timepoint, frequency);
        const group = groups.get(key);
        if (group === undefined) {
            groups.set(key, [row]);
        } else {
            group.push(row);
        }
    }
    return [...groups.entries()]
        .sort(([left], [right]) => right.localeCompare(left))
        .map(([key, group]) => ({
            key,
            rows: [...group].sort((left, right) => right.timepoint.localeCompare(left.timepoint)),
        }));
}

/**
 * The heading a group reads, in the language the person reads and in the period
 * the frequency counts.
 *
 * The date parts come from the instant's own text, so a group's heading never
 * moves with the browser's clock, and the words come from the language the
 * screen is in, so no month name is stored in three catalogues.
 */
export function useGroupHeading(): (key: string, frequency: CutFrequency) => string {
    const { t, language } = useTranslation();
    const formatters = useMemo(
        () => ({
            month: new Intl.DateTimeFormat(language, {
                month: 'long',
                year: 'numeric',
                timeZone: 'UTC',
            }),
            monthName: new Intl.DateTimeFormat(language, { month: 'long', timeZone: 'UTC' }),
        }),
        [language],
    );
    return (key, frequency) => {
        if (frequency === 'yearly' || key === 'all') {
            return t('cuts.group.everyYear');
        }
        const [year, month, day] = key.split('-');
        if (frequency === 'monthly' || month === undefined) {
            return t('cuts.group.year', { year: year ?? key });
        }
        const at = new Date(Date.UTC(Number(year), Number(month) - 1, 1));
        if (frequency === 'onDemand' && day !== undefined) {
            // The day's own order is the language's, so the words come from the
            // catalogue and the month's name from the language's own formatter.
            return t('cuts.group.day', {
                // The day is the language's own number, so a leading zero goes.
                day: String(Number(day)),
                month: formatters.monthName.format(at),
                year: year ?? '',
            });
        }
        return formatters.month.format(at);
    };
}
