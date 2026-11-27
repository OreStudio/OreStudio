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
import type { ReactNode } from 'react';
import { useParams } from 'react-router';
import type {
    BundleRosterBundle,
    BundleRosterMember,
} from '@ores/wire-protocol/generated/dq/protocol/bundle_roster_protocol';
import { readBundleRoster, reasonOf } from '../api/dq.js';
import { OWNER_KEY, REASON_KEY } from './readiness.js';
import { useTranslation } from '../i18n/Provider.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { RecordHeader } from '../refdata/records.js';

/**
 * One dataset, on the page the Record screen standard shapes.
 *
 * A dataset is a record, so its page has the breadcrumb, the display field as
 * the title and the key as the lead. The details state where its rows are staged,
 * what a publish dispatches to, both counts and the reason it cannot publish;
 * the publications state what each run wrote and what it said.
 *
 * The page names the dataset and the server resolves the bundle that carries it,
 * so a saved address works without the list having been read first.
 */
export function DatasetPage(): ReactNode {
    const { t } = useTranslation();
    const { code = '' } = useParams<{ code: string }>();
    const roster = useQuery({
        queryKey: ['dq-roster', 'dataset', code],
        queryFn: () => readBundleRoster('', code),
        enabled: code !== '',
    });
    const { tab, bar } = useTabs({
        label: code,
        tabs: ['details', 'publications'],
        titleOf: (one) => t(`dq.dataset.tabs.${one}`),
    });

    const found = memberOf(roster.data, code);
    const crumbs = [
        ...useAreaRoot('dq'),
        { label: t('dq.coverage.title'), to: PATHS.dataQualityCoverage },
        { label: code },
    ];

    const bare = <RecordHeader crumbs={crumbs} title={code} recordKey={code} version={null} />;
    if (roster.isError) {
        return (
            <div className="space-y-4">
                {bare}
                <Notice tone="error">{roster.error.message}</Notice>
            </div>
        );
    }
    if (roster.data === undefined) {
        return (
            <div className="space-y-4">
                {bare}
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            </div>
        );
    }
    if (found === undefined) {
        return (
            <div className="space-y-4">
                {bare}
                <Notice tone="info">{t('dq.dataset.notCarried', { code })}</Notice>
            </div>
        );
    }

    const { bundle, member } = found;
    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={crumbs}
                title={member.dataset_name === '' ? code : member.dataset_name}
                recordKey={code}
                version={null}
                detail={t('dq.dataset.level', { level: bundle.level })}
            />
            {bar}
            {tab === 'details' ? <Details bundle={bundle} member={member} /> : <Publications />}
        </div>
    );
}

/** The bundle that carries a dataset, and the dataset's row within it. */
function memberOf(
    bundles: readonly BundleRosterBundle[] | undefined,
    code: string,
): { readonly bundle: BundleRosterBundle; readonly member: BundleRosterMember } | undefined {
    for (const bundle of bundles ?? []) {
        const member = bundle.members.find((one) => one.dataset_code === code);
        if (member !== undefined) {
            return { bundle, member };
        }
    }
    return undefined;
}

/** The fields of the dataset, in the grid the standard lays a record out in. */
function Details({
    bundle,
    member,
}: {
    readonly bundle: BundleRosterBundle;
    readonly member: BundleRosterMember;
}): ReactNode {
    const { t } = useTranslation();
    const reason = reasonOf(member);
    const fields: readonly (readonly [string, ReactNode, string])[] = [
        [t('dq.dataset.name'), member.dataset_name, ''],
        [
            t('dq.dataset.code'),
            <span className="font-mono text-xs">{member.dataset_code}</span>,
            '',
        ],
        [
            t('dq.dataset.bundle'),
            `${bundle.name} · ${bundle.code}`,
            t('dq.dataset.level', { level: bundle.level }),
        ],
        [
            t('dq.dataset.artefactType'),
            <span className="font-mono text-xs">{member.artefact_type || '—'}</span>,
            '',
        ],
        [
            t('dq.dataset.artefactTable'),
            <span className="font-mono text-xs">{member.artefact_table || '—'}</span>,
            t('dq.dataset.artefactTableHint'),
        ],
        [
            t('dq.dataset.targetSubject'),
            <span className="font-mono text-xs">{member.target_subject || '—'}</span>,
            t('dq.dataset.targetSubjectHint'),
        ],
        [t('dq.dataset.staged'), member.staged_count.toLocaleString(), t('dq.dataset.stagedHint')],
        [t('dq.dataset.published'), '—', t('dq.dataset.notServed')],
        [
            t('dq.dataset.optional'),
            member.optional ? t('common.yes') : t('common.no'),
            t('dq.dataset.optionalHint'),
        ],
        [t('dq.dataset.lastPublished'), '—', t('dq.dataset.lastPublishedGap')],
        [
            t('dq.dataset.readiness'),
            reason === null ? (
                <Tag>{t('dq.dataset.ready')}</Tag>
            ) : (
                <Tag tone="down">{t(REASON_KEY[reason])}</Tag>
            ),
            reason === null ? t('dq.dataset.readyHint') : t(OWNER_KEY[reason]),
        ],
    ];
    return (
        <section className="rounded-md border border-line bg-surface-raised">
            <dl className="grid gap-4 p-4 sm:grid-cols-2 lg:grid-cols-4">
                {fields.map(([label, value, hint]) => (
                    <div key={label}>
                        <dt className="text-xs text-ink-faint">{label}</dt>
                        <dd className="mt-0.5 text-sm">
                            {value}
                            {hint === '' ? null : (
                                <span className="mt-1 block text-xs text-ink-faint">{hint}</span>
                            )}
                        </dd>
                    </div>
                ))}
            </dl>
        </section>
    );
}

/**
 * The publishes of one dataset.
 *
 * The run log that holds them has no writer and no column for an outcome or a
 * message, so this tab states that and draws nothing. A table that said "none
 * yet" would claim nothing was published, which is the opposite of what is known.
 */
function Publications(): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="rounded-md border border-line bg-surface-raised">
            <div className="p-4">
                <h2 className="text-sm font-medium">{t('dq.dataset.publicationsTitle')}</h2>
                <p className="mt-1 text-xs text-ink-muted">{t('dq.dataset.publicationsLead')}</p>
            </div>
            <div className="px-4 pb-4">
                <Notice tone="warn">{t('dq.dataset.publicationsGap')}</Notice>
            </div>
        </section>
    );
}
