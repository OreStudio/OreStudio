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
import type {
    BundleRosterBundle,
    BundleRosterMember,
} from '@ores/wire-protocol/generated/dq/protocol/bundle_roster_protocol';
import { readBundleRoster, reasonOf, type NotReadyReason } from '../api/dq.js';
import { useTranslation } from '../i18n/Provider.js';
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

/** The owner of each reason, because a reason without one sends nobody anywhere. */
const OWNER_KEY: Record<NotReadyReason, string> = {
    'no dataset': 'dq.coverage.owner.dq',
    'no artefact type': 'dq.coverage.owner.dq',
    'no target subject': 'dq.coverage.owner.dq',
    'no staged rows': 'dq.coverage.owner.importer',
};

/** Every reason, in the order the screen lists them. */
const REASONS: readonly NotReadyReason[] = [
    'no dataset',
    'no artefact type',
    'no target subject',
    'no staged rows',
];

/** The levels a bundle publishes at, in the order the publication model names them. */
const LEVELS: readonly string[] = ['platform', 'world', 'tenant'];

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
 * One read of the bundle roster, drawn as three cards: the bundles grouped by
 * the level they publish at, the members of the chosen bundle, and the reasons a
 * member cannot publish with the owner of each. The level is grouped and not
 * shown as a column, because it decides which tenant holds the rows and whether
 * a tenant owes a copy.
 *
 * The roster carries both the bundles and, for the bundle a caller names, its
 * members with the number of rows each has staged. It is read twice for that
 * reason: once for the roster, which names the bundle to open on, and once for
 * the bundle in view, because the staged count costs one count per member in the
 * service.
 *
 * The screen is a read. It publishes nothing: the publish itself is the bundle's
 * journey, and this page stays answerable to "is my reference data complete?".
 *
 * One figure the design asks for is not served yet, so the screen states its
 * absence rather than drawing a zero: the rows already published need a
 * provenance read. A zero here would say the run wrote nothing, which is the
 * opposite claim.
 */
export function CoveragePage(): ReactNode {
    const { t } = useTranslation();
    const [chosen, setChosen] = useState<string>();
    const roster = useQuery({ queryKey: ['dq-roster', ''], queryFn: () => readBundleRoster() });
    // The bundle in view opens on the first of the roster, so a person sees the
    // counts without having to choose one first. It is derived here rather than
    // set from an effect, so the first render already asks for the right bundle.
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
    const levels = levelsOf(bundles);
    const members = detail.data?.find((bundle) => bundle.code === current)?.members ?? [];

    return (
        <div className="space-y-6">
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
                <>
                    <Card title={t('dq.coverage.bundlesTitle')} lead={t('dq.coverage.bundlesLead')}>
                        <div className="space-y-4">
                            {levels.map((level) => {
                                const held = bundlesAt(bundles, level);
                                if (held.length === 0) {
                                    return null;
                                }
                                return (
                                    <div key={level}>
                                        <h3 className="mb-2 text-xs font-medium uppercase tracking-wide text-ink-muted">
                                            {level === '' ? t('dq.coverage.noLevel') : level}
                                        </h3>
                                        <div className="flex flex-wrap gap-2">
                                            {held.map((bundle) => (
                                                <button
                                                    key={bundle.code}
                                                    type="button"
                                                    aria-pressed={bundle.code === current}
                                                    onClick={() => setChosen(bundle.code)}
                                                    className={
                                                        bundle.code === current
                                                            ? 'rounded-md border border-accent bg-surface-hover px-3 py-2 text-left text-sm'
                                                            : 'rounded-md border border-line px-3 py-2 text-left text-sm hover:bg-surface-hover'
                                                    }
                                                >
                                                    <span className="block font-medium">
                                                        {bundle.name}
                                                    </span>
                                                    <span className="block font-mono text-xs text-ink-faint">
                                                        {bundle.code}
                                                    </span>
                                                </button>
                                            ))}
                                        </div>
                                    </div>
                                );
                            })}
                        </div>
                    </Card>

                    <Card title={t('dq.coverage.membersTitle')} lead={t('dq.coverage.membersLead')}>
                        <MembersTable members={members} />
                    </Card>
                </>
            )}

            <Card title={t('dq.coverage.reasonsTitle')} lead={t('dq.coverage.reasonsLead')}>
                <ul className="space-y-2 text-sm">
                    {REASONS.map((reason) => (
                        <li key={reason} className="flex flex-wrap items-baseline gap-2">
                            <Tag tone="down">{t(REASON_KEY[reason])}</Tag>
                            <span className="text-ink-muted">{t(OWNER_KEY[reason])}</span>
                        </li>
                    ))}
                </ul>
            </Card>

            <Notice tone="warn">{t('dq.coverage.gap')}</Notice>
        </div>
    );
}

/** One card of the screen: a heading, a line about it and the content. */
function Card({
    title,
    lead,
    children,
}: {
    readonly title: string;
    readonly lead: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="rounded-md border border-line p-4">
            <h2 className="text-sm font-medium">{title}</h2>
            <p className="mb-3 text-xs text-ink-muted">{lead}</p>
            {children}
        </section>
    );
}

/** The members of one bundle, each with the reason it is not ready. */
function MembersTable({ members }: { readonly members: readonly BundleRosterMember[] }): ReactNode {
    const { t } = useTranslation();
    const held = [...members].sort((left, right) => left.display_order - right.display_order);

    if (held.length === 0) {
        return <p className="text-sm text-ink-muted">{t('dq.coverage.noMembers')}</p>;
    }

    return (
        <div className="overflow-x-auto rounded-md border border-line">
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                        <th className="px-4 py-2 font-medium">{t('dq.coverage.dataset')}</th>
                        <th className="px-4 py-2 font-medium">{t('dq.coverage.artefactType')}</th>
                        <th className="px-4 py-2 font-medium">{t('dq.coverage.staged')}</th>
                        <th className="px-4 py-2 font-medium">{t('dq.coverage.published')}</th>
                        <th className="px-4 py-2 font-medium">{t('dq.coverage.readiness')}</th>
                    </tr>
                </thead>
                <tbody>
                    {held.map((member) => {
                        const reason = reasonOf(member);
                        return (
                            <tr
                                key={member.dataset_code}
                                className="border-b border-line-subtle last:border-b-0"
                            >
                                <td className="px-4 py-2">
                                    <span className="block font-mono text-xs">
                                        {member.dataset_code}
                                    </span>
                                    <span className="block text-xs text-ink-faint">
                                        {member.dataset_name}
                                    </span>
                                </td>
                                <td className="px-4 py-2 font-mono text-xs text-ink-muted">
                                    {member.artefact_type === '' ? '—' : member.artefact_type}
                                </td>
                                <td className="px-4 py-2">
                                    {member.staged_count.toLocaleString()}
                                </td>
                                <td
                                    className="px-4 py-2 text-ink-faint"
                                    title={t('dq.coverage.notServed')}
                                >
                                    —
                                </td>
                                <td className="px-4 py-2">
                                    {reason === null ? (
                                        <Tag>{t('dq.coverage.metadataComplete')}</Tag>
                                    ) : (
                                        <Tag tone="down">
                                            {t(REASON_KEY[reason])}
                                            <span className="ml-2 text-ink-muted">
                                                {t(OWNER_KEY[reason])}
                                            </span>
                                        </Tag>
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
