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

/**
 * One report configuration, read only: its kind and name, who owns it, the
 * analytics it holds and the reports that bind it.
 *
 * A configuration is shared by binding and never copied, so the page states the
 * reports that use it. A person who weighs a change to a configuration needs
 * that list more than any other field. The page offers no change: the server
 * writes configurations through the analytics screens, and a page that drew a
 * disabled Edit would only repeat that.
 */

import type { ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import type { ReportConfigurationBinding } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordDetails, RecordHeader, type FieldSpec } from '../refdata/records.js';
import { Crumbs } from '../refdata/shared.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS, configurationPath, reportPath } from '../shell/paths.js';
import { AccessMark } from '../ui/AccessMark.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { Notice, Tag } from '../ui/Primitives.js';

/** Where one configuration is cached. */
export function configurationKey(id: string): readonly unknown[] {
    return ['configuration', id];
}

/** Where the bindings of one definition are cached. */
export function bindingsKey(definitionId: string): readonly unknown[] {
    return ['report-configurations', definitionId];
}

const CONFIGURATION_EVENTS = [
    { component: 'reporting', entity: 'configurations' },
    { component: 'reporting', entity: 'report_configurations' },
    { component: 'reporting', entity: 'configuration_analytics' },
] as const;

/** The eight characters a person reads to tell one identifier from another. */
function shortId(id: string): string {
    return id.slice(0, 8);
}

/** The configuration's own fields, in the order the page states them. */
const CONFIGURATION_FIELDS: readonly FieldSpec[] = [
    {
        field: 'name',
        history: 'Name',
        labelKey: 'reporting.configurations.fields.name',
        kind: { kind: 'text', max: 200 },
    },
    {
        field: 'typeName',
        history: 'Kind',
        labelKey: 'reporting.configurations.fields.type',
        kind: { kind: 'text', max: 200 },
    },
    {
        field: 'owningComponent',
        history: 'Owning Component',
        labelKey: 'reporting.configurations.fields.owningComponent',
        kind: { kind: 'text', max: 200 },
    },
];

/** The page of one configuration. */
export function ConfigurationPage(): ReactNode {
    const { id = '' } = useParams();
    const { t } = useTranslation();
    const queries = useQueryClient();
    const area = useAreaRoot('reporting');
    const open = useQuery({
        queryKey: configurationKey(id),
        queryFn: () => api.configuration(id),
        meta: { quiet: true },
    });
    const load = useLoadState([configurationKey(id)]);
    const news = useEntityChanges(CONFIGURATION_EVENTS, load);

    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={area} />}
                title={shortId(id)}
                description={t('reporting.configurations.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (open.isError) {
        const missing = open.error instanceof ApiFailure && open.error.status === 404;
        const refused = open.error instanceof ApiFailure && open.error.status === 403;
        return (
            <ScreenFrame
                trail={<Crumbs parts={[...area, { label: shortId(id) }]} />}
                title={shortId(id)}
                description={t('reporting.configurations.lead')}
            >
                {missing ? (
                    <div className="space-y-3">
                        <Notice tone="info">{t('refdata.records.notFound')}</Notice>
                        <Link
                            to={PATHS.reports}
                            className="text-sm text-accent hover:text-accent-bright"
                        >
                            {t('refdata.records.backToList')}
                        </Link>
                    </div>
                ) : refused ? (
                    <Notice tone="info">{t('reporting.configurations.refused')}</Notice>
                ) : (
                    <Notice tone="error">{open.error.message}</Notice>
                )}
            </ScreenFrame>
        );
    }

    const { configuration, analytics, reports } = open.data;
    const reload = (): void => {
        void queries.invalidateQueries({ queryKey: configurationKey(id) });
    };
    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[...area, { label: configuration.name }]}
                title={configuration.name}
                recordKey={shortId(configuration.id)}
                version={configuration.version}
                subdued
                access={null}
                own={<RefreshButton onClick={reload} load={load} news={news} />}
            />
            <div className="space-y-4">
                <RecordDetails
                    specs={CONFIGURATION_FIELDS}
                    row={configuration}
                    heading={{
                        title: t('reporting.configurations.record'),
                        onEdit: undefined,
                    }}
                    extra={[
                        [
                            t('reporting.detail.party'),
                            configuration.partyName === '' ? (
                                <span className="text-ink-faint">
                                    {t('reporting.cell.partyNotStated')}
                                </span>
                            ) : (
                                <Link
                                    to={`/parties/${encodeURIComponent(configuration.partyId)}`}
                                    className="text-accent hover:text-accent-bright"
                                >
                                    {configuration.partyName}
                                </Link>
                            ),
                        ],
                    ]}
                />
                <section className="card space-y-3 p-4">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-sm font-medium">
                            {t('reporting.configurations.analytics')}
                        </h2>
                        <AccessMark canWrite={false} />
                    </header>
                    {analytics.length === 0 ? (
                        <p className="text-sm text-ink-muted">
                            {t('reporting.configurations.noAnalytics')}
                        </p>
                    ) : (
                        <ul className="space-y-1 text-sm">
                            {analytics.map((analytic) => (
                                <li key={analytic.code} className="flex items-center gap-2">
                                    <span>{analytic.name}</span>
                                    {!analytic.active && (
                                        <Tag>{t('reporting.configurations.off')}</Tag>
                                    )}
                                </li>
                            ))}
                        </ul>
                    )}
                </section>
                <section className="card space-y-3 p-4">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-sm font-medium">
                            {t('reporting.configurations.usedBy')}
                        </h2>
                        <AccessMark canWrite={false} />
                    </header>
                    {reports.length === 0 ? (
                        <p className="text-sm text-ink-muted">
                            {t('reporting.configurations.noReports')}
                        </p>
                    ) : (
                        <ul className="space-y-1 text-sm">
                            {reports.map((report) => (
                                <li key={report.id} className="flex items-center gap-2">
                                    <Link
                                        to={reportPath(report.id)}
                                        className="text-accent hover:text-accent-bright"
                                    >
                                        {report.name}
                                    </Link>
                                    {report.partyName !== '' && <Tag>{report.partyName}</Tag>}
                                </li>
                            ))}
                        </ul>
                    )}
                </section>
            </div>
        </div>
    );
}

/**
 * The configurations a report binds, as a card of the report page: the kind of
 * each and its name, which opens the configuration.
 *
 * A reader who may not see bindings is told so in words. The card never reads as
 * a report that binds nothing, because nothing was checked.
 */
export function ReportConfigurations({
    definitionId,
}: {
    readonly definitionId: string;
}): ReactNode {
    const { t } = useTranslation();
    const bindings = useQuery({
        queryKey: bindingsKey(definitionId),
        queryFn: () => api.reportConfigurations(definitionId),
        meta: { quiet: true },
    });
    return (
        <section className="card space-y-3 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-sm font-medium">{t('reporting.configurations.title')}</h2>
                <AccessMark canWrite={false} />
            </header>
            <BindingList bindings={bindings} />
        </section>
    );
}

function BindingList({
    bindings,
}: {
    readonly bindings: {
        readonly isPending: boolean;
        readonly isError: boolean;
        readonly error: Error | null;
        readonly data: readonly ReportConfigurationBinding[] | undefined;
    };
}): ReactNode {
    const { t } = useTranslation();
    if (bindings.isPending) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('common.loading')}
            </p>
        );
    }
    if (bindings.isError) {
        return bindings.error instanceof ApiFailure && bindings.error.status === 403 ? (
            <p className="text-sm text-ink-muted">{t('reporting.configurations.refused')}</p>
        ) : (
            <Notice tone="error">{bindings.error?.message ?? ''}</Notice>
        );
    }
    const list = bindings.data ?? [];
    if (list.length === 0) {
        return <p className="text-sm text-ink-muted">{t('reporting.configurations.none')}</p>;
    }
    return (
        <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-2 lg:grid-cols-3">
            {list.map((binding) => (
                <div key={binding.id}>
                    <dt className="text-xs text-ink-faint">{binding.typeName}</dt>
                    <dd className="mt-0.5 text-sm">
                        <Link
                            to={configurationPath(binding.configurationId)}
                            className="text-accent hover:text-accent-bright"
                        >
                            {binding.name === '' ? shortId(binding.configurationId) : binding.name}
                        </Link>
                    </dd>
                </div>
            ))}
        </dl>
    );
}
