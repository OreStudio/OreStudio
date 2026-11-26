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

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useNavigate } from 'react-router';
import type {
    BundleRosterBundle,
    BundleRosterMember,
} from '@ores/wire-protocol/generated/dq/protocol/bundle_roster_protocol';
import { readBundleRoster, reasonOf, type NotReadyReason } from '../api/dq.js';
import { useTranslation } from '../i18n/Provider.js';
import { datasetPath } from '../shell/paths.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshQueries } from '../ui/RefreshButton.js';

/** The reason a member is not ready, in the words the screen gives it. */
const REASON_KEY: Record<NotReadyReason, string> = {
    'no dataset': 'dq.coverage.reason.noDataset',
    'no artefact type': 'dq.coverage.reason.noArtefactType',
    'no target subject': 'dq.coverage.reason.noTargetSubject',
    'no staged rows': 'dq.coverage.reason.noStagedRows',
};

/** The levels a bundle publishes at, in the order the publication model names them. */
const LEVELS: readonly string[] = ['platform', 'world', 'tenant'];

/** The states the table filters by, in the order the filter lists them. */
const STATES = ['all', 'not-ready', 'unpublished', 'published'] as const;

type StateFilter = (typeof STATES)[number];

/** The bundles of one level. */
function bundlesAt(
    bundles: readonly BundleRosterBundle[],
    level: string,
): readonly BundleRosterBundle[] {
    return bundles.filter((bundle) => bundle.level === level);
}

/**
 * The levels the screen draws: the three the publication model names, then any
 * level a bundle states that the model does not, in the order the bundles give.
 */
function levelsOf(bundles: readonly BundleRosterBundle[]): readonly string[] {
    const extra = bundles
        .map((bundle) => bundle.level)
        .filter((level) => level !== '' && !LEVELS.includes(level));
    return [...LEVELS, ...new Set(extra)];
}

/**
 * Whether the tenant holds what its valuations read.
 *
 * The list is a master and a detail, in the shape of the Qt Data Librarian it
 * replaces: the bundles grouped by the level they publish at on the left, and
 * the chosen bundle's datasets in a dense table on the right. A row opens the
 * dataset's own page, because a dataset is a record and a record has a page.
 *
 * The level is grouped and not shown as a column, because it decides which
 * tenant holds the rows and whether a tenant owes a copy.
 *
 * The roster carries the bundles and, for the bundle a caller names, its members
 * with the number of rows each has staged. It is read twice for that reason: the
 * staged count costs one count per member in the service.
 */
export function CoveragePage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [chosen, setChosen] = useState<string>();
    const [text, setText] = useState('');
    const [state, setState] = useState<StateFilter>('all');
    const roster = useQuery({ queryKey: ['dq-roster', ''], queryFn: () => readBundleRoster() });
    // The bundle in view opens on the first of the roster, so a person sees the
    // datasets without choosing one first. It is derived here rather than set
    // from an effect, so the first render already asks for the right bundle.
    const current = chosen ?? roster.data?.[0]?.code ?? '';
    const detail = useQuery({
        queryKey: ['dq-roster', current],
        queryFn: () => readBundleRoster(current),
        enabled: current !== '',
    });

    const trail = <AreaTrail area="dq" screen={t('dq.coverage.title')} />;

    if (roster.isError) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('dq.coverage.title')}
                description={t('dq.coverage.lead')}
            >
                <Notice tone="error">{roster.error.message}</Notice>
            </ScreenFrame>
        );
    }
    if (roster.data === undefined) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('dq.coverage.title')}
                description={t('dq.coverage.lead')}
            >
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            </ScreenFrame>
        );
    }

    const bundles = roster.data;
    const carried = detail.data?.find((bundle) => bundle.code === current);
    const held = carried?.members ?? [];
    const members = held.filter((member) => shown(member, text, state));

    return (
        <div className="space-y-4">
            <div>
                {trail}
                <PageHeader
                    title={t('dq.coverage.title')}
                    description={t('dq.coverage.lead')}
                    actions={
                        <RefreshQueries
                            keys={[
                                ['dq-roster', ''],
                                ['dq-roster', current],
                            ]}
                        />
                    }
                />
            </div>

            {bundles.length === 0 ? (
                <Notice tone="info">{t('dq.coverage.noBundles')}</Notice>
            ) : (
                <div className="grid grid-cols-[232px_minmax(0,1fr)] items-start gap-4">
                    <BundleTree bundles={bundles} current={current} onPick={setChosen} />
                    <section className="overflow-hidden rounded-md border border-line">
                        <header className="flex items-baseline gap-2 border-b border-line-soft px-4 py-3">
                            <h2 className="text-sm font-medium">
                                {carried?.name ?? t('dq.coverage.datasets')}
                            </h2>
                            <span className="text-xs text-ink-faint">
                                {carried === undefined
                                    ? ''
                                    : t('dq.coverage.bundleLead', {
                                          code: carried.code,
                                          count: String(carried.dataset_count),
                                          level: carried.level,
                                      })}
                            </span>
                        </header>
                        <Filters text={text} state={state} onText={setText} onState={setState} />
                        <MembersTable
                            members={members}
                            empty={
                                held.length > 0
                                    ? t('dq.coverage.noMatch')
                                    : t('dq.coverage.noMembers')
                            }
                            onOpen={(code) => navigate(datasetPath(code))}
                        />
                    </section>
                </div>
            )}

            <Notice tone="warn">{t('dq.coverage.gap')}</Notice>
        </div>
    );
}

/** Whether one member passes the text and state filters. */
function shown(member: BundleRosterMember, text: string, state: StateFilter): boolean {
    if (text !== '') {
        const needle = text.toLowerCase();
        const hay = `${member.dataset_code} ${member.dataset_name}`.toLowerCase();
        if (!hay.includes(needle)) {
            return false;
        }
    }
    const ready = reasonOf(member) === null;
    if (state === 'not-ready') {
        return !ready;
    }
    if (state === 'published') {
        return ready;
    }
    return true;
}

/** The bundles of every level, as the tree the person picks from. */
function BundleTree({
    bundles,
    current,
    onPick,
}: {
    readonly bundles: readonly BundleRosterBundle[];
    readonly current: string;
    readonly onPick: (code: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <nav
            aria-label={t('dq.coverage.bundlesTitle')}
            className="overflow-hidden rounded-md border border-line"
        >
            <header className="flex items-baseline gap-2 border-b border-line-soft px-3 py-3">
                <h2 className="text-xs font-semibold uppercase tracking-wide text-ink-dim">
                    {t('dq.coverage.bundlesTitle')}
                </h2>
                <span className="text-xs text-ink-faint">{bundles.length}</span>
            </header>
            <div className="space-y-0.5 p-2">
                {levelsOf(bundles).map((level) => {
                    const levelBundles = bundlesAt(bundles, level);
                    if (levelBundles.length === 0) {
                        return null;
                    }
                    return (
                        <div key={level}>
                            <p className="px-2 pt-2 pb-1 text-[11px] font-semibold uppercase tracking-wide text-ink-faint">
                                {level === '' ? t('dq.coverage.noLevel') : level}
                            </p>
                            {levelBundles.map((bundle) => (
                                <button
                                    key={bundle.code}
                                    type="button"
                                    aria-pressed={bundle.code === current}
                                    onClick={() => onPick(bundle.code)}
                                    className={
                                        bundle.code === current
                                            ? 'grid w-full grid-cols-[minmax(0,1fr)_auto] gap-x-2 rounded-md border border-line-strong bg-surface-hover px-2 py-1.5 text-left'
                                            : 'grid w-full grid-cols-[minmax(0,1fr)_auto] gap-x-2 rounded-md border border-transparent px-2 py-1.5 text-left hover:bg-surface-hover'
                                    }
                                >
                                    <span className="truncate font-mono text-xs">
                                        {bundle.code}
                                    </span>
                                    <span className="font-mono text-xs text-ink-faint">
                                        {bundle.dataset_count}
                                    </span>
                                    <span className="col-span-2 truncate text-[11px] text-ink-dim">
                                        {bundle.name}
                                    </span>
                                </button>
                            ))}
                        </div>
                    );
                })}
            </div>
        </nav>
    );
}

/** The text and state filters over the table. */
function Filters({
    text,
    state,
    onText,
    onState,
}: {
    readonly text: string;
    readonly state: StateFilter;
    readonly onText: (value: string) => void;
    readonly onState: (value: StateFilter) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="flex gap-2 border-b border-line-soft px-4 py-2.5">
            <input
                type="search"
                value={text}
                aria-label={t('dq.coverage.filterText')}
                placeholder={t('dq.coverage.filterText')}
                onChange={(event) => onText(event.target.value)}
                className="min-w-0 flex-1 rounded-md border border-line bg-surface-raised px-2 py-1.5 text-xs"
            />
            <select
                value={state}
                aria-label={t('dq.coverage.filterState')}
                onChange={(event) => onState(event.target.value as StateFilter)}
                className="rounded-md border border-line bg-surface-raised px-2 py-1.5 text-xs"
            >
                {STATES.map((one) => (
                    <option key={one} value={one}>
                        {t(`dq.coverage.state.${one}`)}
                    </option>
                ))}
            </select>
        </div>
    );
}

/** The members of one bundle: a row opens the dataset's own page. */
function MembersTable({
    members,
    empty,
    onOpen,
}: {
    readonly members: readonly BundleRosterMember[];
    readonly empty: string;
    readonly onOpen: (code: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    if (members.length === 0) {
        return <p className="px-4 py-6 text-sm text-ink-muted">{empty}</p>;
    }
    return (
        <div className="overflow-x-auto">
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                        <th className="px-4 py-2 font-medium">{t('dq.coverage.dataset')}</th>
                        <th className="px-4 py-2 text-right font-medium">
                            {t('dq.coverage.staged')}
                        </th>
                        <th className="px-4 py-2 text-right font-medium">
                            {t('dq.coverage.published')}
                        </th>
                        <th className="px-4 py-2 font-medium">{t('dq.coverage.stateTitle')}</th>
                    </tr>
                </thead>
                <tbody>
                    {members.map((member) => {
                        const reason = reasonOf(member);
                        return (
                            <tr
                                key={member.dataset_code}
                                tabIndex={0}
                                className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                onClick={() => onOpen(member.dataset_code)}
                                onKeyDown={(event) => {
                                    if (event.key === 'Enter' || event.key === ' ') {
                                        onOpen(member.dataset_code);
                                    }
                                }}
                            >
                                <td className="px-4 py-2">
                                    <span className="block font-mono text-xs">
                                        {member.dataset_code}
                                    </span>
                                    <span className="block text-xs text-ink-faint">
                                        {member.dataset_name}
                                    </span>
                                </td>
                                <td className="px-4 py-2 text-right font-mono text-xs">
                                    {member.staged_count.toLocaleString()}
                                </td>
                                <td
                                    className="px-4 py-2 text-right text-ink-faint"
                                    title={t('dq.coverage.notServed')}
                                >
                                    —
                                </td>
                                <td className="px-4 py-2">
                                    {reason === null ? (
                                        <Tag>{t('dq.coverage.metadataComplete')}</Tag>
                                    ) : (
                                        <Tag tone="down">{t(REASON_KEY[reason])}</Tag>
                                    )}
                                </td>
                            </tr>
                        );
                    })}
                </tbody>
            </table>
        </div>
    );
}
