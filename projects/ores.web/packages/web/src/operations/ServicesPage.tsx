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

/**
 * See the running services, from
 * doc/knowledge/journeys/operations/journey_see_the_running_services.org.
 *
 * The screen answers the roster the registry expects with the samples the
 * instances send, so a service that went quiet keeps its row and a version that
 * lags behind is visible. The states use the read's own words and the shell's
 * tag tones. The compute wrappers belong to the grid screen, which shows them
 * against the nodes they run on.
 *
 * The read is a point in time, so the screen offers a refresh control rather
 * than polling: the ages in the last column move when the person asks them to.
 */

import type { ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import type { ServiceRosterRow } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { Button, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import {
    GapPanel,
    InstanceStateTag,
    InstanceVersion,
    OperationsBack,
    SERVICE_RUNNING_WINDOW_MINUTES,
    compareVersions,
    newestVersionOf,
    sameVersion,
    type ScreenGap,
} from './OperationsParts.js';
import { RelatedJourneys, type JourneyId } from './RelatedJourneys.js';

/** The service whose instances run on the grid's nodes, which the grid screen owns. */
const COMPUTE_WRAPPER = 'ores.compute.wrapper';

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = [
    '7B820710-161C-4926-B5AA-5EF2772A3652',
    'EE26E58C-F389-4E82-BF96-EC5324F9F795',
    '57C4B9A6-DA79-403E-984E-50D2561352B3',
    'C3D59907-9D6A-448C-9A61-9E750755BBFB',
    '22AC8DD8-A440-4330-9992-47A5E9985473',
];

/** Where the roster read's answer is cached, so a test can seed it. */
export const SERVICES_QUERY_KEY = ['operations-services'] as const;

/** The instant a read answered, as the wall clock a person reads, in UTC. */
export function readTime(at: number): string {
    return `${new Date(at).toISOString().slice(11, 19)} UTC`;
}

/**
 * How long ago an instance reported, in the units a person reads.
 *
 * The unit words come from the catalogue rather than from here, so a French or
 * Portuguese reader does not read English abbreviations inside their own
 * sentence. `ui/Time.tsx` rounds to one unit, which cannot state the two
 * components the services screen promises.
 */
export function formatAge(seconds: number, t: Translator['t']): string {
    if (seconds < 60) {
        return t('operations.services.age.seconds', { seconds });
    }
    const minutes = Math.floor(seconds / 60);
    if (minutes < 60) {
        return t('operations.services.age.minutesSeconds', {
            minutes,
            seconds: seconds % 60,
        });
    }
    return t('operations.services.age.hoursMinutes', {
        hours: Math.floor(minutes / 60),
        minutes: minutes % 60,
    });
}

interface ServiceGroup {
    readonly serviceName: string;
    readonly rows: readonly ServiceRosterRow[];
}

/**
 * The rows gathered under their service, in the order the read answered them.
 *
 * The roster read orders its slots by service name and then slot, so the rows
 * arrive grouped already and this only gathers consecutive runs: sorting here
 * would be a second opinion about an order the read has settled.
 */
export function groupByService(rows: readonly ServiceRosterRow[]): readonly ServiceGroup[] {
    const groups: { serviceName: string; rows: ServiceRosterRow[] }[] = [];
    for (const row of rows) {
        const last = groups[groups.length - 1];
        if (last !== undefined && last.serviceName === row.service_name) {
            last.rows.push(row);
        } else {
            groups.push({ serviceName: row.service_name, rows: [row] });
        }
    }
    return groups;
}

/** The service names and versions that trail the newest running release. */
interface VersionSkew {
    readonly services: string;
    readonly versions: string;
    readonly newest: string;
}

export function versionSkew(
    running: readonly ServiceRosterRow[],
    newest: string | undefined,
): VersionSkew | undefined {
    if (newest === undefined) {
        return undefined;
    }
    const behind = running.filter(
        (row) => row.version !== null && row.version !== '' && !sameVersion(row.version, newest),
    );
    if (behind.length === 0) {
        return undefined;
    }
    return {
        services: [...new Set(behind.map((row) => row.service_name))].join(', '),
        versions: [...new Set(behind.map((row) => row.version ?? ''))]
            .sort(compareVersions)
            .join(', '),
        newest,
    };
}

/** One service's row count, warned when fewer instances reported than expected. */
function InstanceCount({
    reported,
    expected,
    label,
}: {
    readonly reported: number;
    readonly expected: number;
    readonly label: string;
}): ReactNode {
    if (reported < expected) {
        return <Tag tone="warn">{label}</Tag>;
    }
    return <span className="font-mono text-ink-muted">{label}</span>;
}

export function ServicesPage(): ReactNode {
    const { t } = useTranslation();
    const roster = useQuery({ queryKey: SERVICES_QUERY_KEY, queryFn: api.services });

    if (roster.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (roster.isError) {
        return <Notice tone="error">{roster.error.message}</Notice>;
    }

    const rows = roster.data.filter((row) => row.service_name !== COMPUTE_WRAPPER);
    const groups = groupByService(rows);
    const running = rows.filter((row) => row.state === 'running');
    const lost = rows.filter((row) => row.state === 'lost');
    const missing = rows.filter((row) => row.state === 'missing');
    const newestVersion = newestVersionOf(running);
    const skew = versionSkew(running, newestVersion);
    const gaps: readonly ScreenGap[] = [
        {
            title: t('operations.services.gap.quiet.title'),
            body: t('operations.services.gap.quiet.body'),
            journey: t('operations.journeys.services'),
        },
        {
            title: t('operations.services.gap.release.title'),
            body: t('operations.services.gap.release.body'),
            journey: t('operations.journeys.services'),
        },
        {
            title: t('operations.services.gap.uptime.title'),
            body: t('operations.services.gap.uptime.body'),
            journey: t('operations.journeys.services'),
        },
    ];

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('operations.services.title')}
                description={t('operations.services.description')}
                actions={
                    <div className="flex items-center gap-3">
                        <span className="text-xs text-ink-faint">
                            {t('operations.services.updated', {
                                at: readTime(roster.dataUpdatedAt),
                            })}
                        </span>
                        <Button variant="secondary" onClick={() => void roster.refetch()}>
                            {t('operations.services.refresh')}
                        </Button>
                        <OperationsBack />
                    </div>
                }
            />

            {skew !== undefined && (
                <Notice tone="warn">
                    {t('operations.services.skew', {
                        services: skew.services,
                        versions: skew.versions,
                        newest: skew.newest,
                    })}
                </Notice>
            )}

            <section className="card space-y-4 p-6">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">
                        {t('operations.services.instances.title')}
                    </h2>
                    <div className="flex items-center gap-2 text-xs text-ink-faint">
                        <span>
                            {t('operations.services.instances.reported', {
                                running: running.length,
                                total: rows.length,
                                minutes: SERVICE_RUNNING_WINDOW_MINUTES,
                            })}
                        </span>
                        {lost.length > 0 && (
                            <Tag tone="muted">
                                {t('operations.services.instances.lost', { count: lost.length })}
                            </Tag>
                        )}
                        {missing.length > 0 && (
                            <Tag tone="warn">
                                {t('operations.services.instances.missing', {
                                    count: missing.length,
                                })}
                            </Tag>
                        )}
                    </div>
                </header>

                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">
                                {t('operations.services.columns.service')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.services.columns.instances')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.services.columns.instance')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.services.columns.status')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.services.columns.version')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.services.columns.lastHeartbeat')}
                            </th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {groups.map((group) => {
                            const reported = group.rows.filter(
                                (instance) => instance.state === 'running',
                            ).length;
                            return group.rows.map((row, index) => (
                                <tr key={`${group.serviceName}-${String(row.slot)}`}>
                                    {index === 0 && (
                                        <td className="py-2" rowSpan={group.rows.length}>
                                            <span className="font-mono">{group.serviceName}</span>
                                        </td>
                                    )}
                                    {index === 0 && (
                                        <td className="py-2" rowSpan={group.rows.length}>
                                            <InstanceCount
                                                reported={reported}
                                                expected={group.rows.length}
                                                label={t('operations.services.count', {
                                                    reported,
                                                    expected: group.rows.length,
                                                })}
                                            />
                                        </td>
                                    )}
                                    <td
                                        className="py-2 font-mono"
                                        title={row.instance_id ?? undefined}
                                    >
                                        {row.instance_id === null
                                            ? '—'
                                            : row.instance_id.slice(0, 8)}
                                    </td>
                                    <td className="py-2">
                                        <InstanceStateTag state={row.state} />
                                    </td>
                                    <td className="py-2">
                                        <InstanceVersion
                                            instance={row}
                                            newestVersion={newestVersion}
                                        />
                                    </td>
                                    <td className="py-2 font-mono">
                                        {row.age_seconds === null
                                            ? '—'
                                            : t('operations.services.ago', {
                                                  age: formatAge(row.age_seconds, t),
                                              })}
                                    </td>
                                </tr>
                            ));
                        })}
                    </tbody>
                </table>

                <p className="text-xs text-ink-faint">{t('operations.services.hint')}</p>
            </section>

            <GapPanel gaps={gaps} />
            <RelatedJourneys ids={JOURNEYS} />
        </div>
    );
}
