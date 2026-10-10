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

/*
 * How healthy the installation is, in six figures.
 *
 * The services screen opens with these and the system administrator's home
 * shows them, from this one place, so the two cannot disagree. Nothing here
 * needs a backend of its own: the roster says which services run, and the logs
 * read states a total for any level and range, so a count of errors is a read
 * with a page size of one.
 */

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { ServiceRosterRow } from '@ores/wire-protocol/browser';
import { api, type LogsRange } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { newestVersionOf, sameVersion } from './OperationsParts.js';

/** The compute service's runner, which the grid screen owns; the wire name is the registry's. */
export const COMPUTE_RUNNER = 'ores.compute.wrapper';

/** Where the roster read's answer is cached, so the two screens share one read. */
export const SERVICES_QUERY_KEY = ['operations-services'] as const;

/** The range the home states, and the services screen opens on. */
export const DEFAULT_HEALTH_RANGE: LogsRange = '1h';

/** Where the count of one level in one range is cached. */
export function logCountKey(level: 'error' | 'warn', range: LogsRange): readonly string[] {
    return ['operations-log-count', level, range];
}

/**
 * A release as the application writes it: with a leading `v`.
 *
 * The services report the number bare and the rest of the application states
 * `v0.0.27`, so the figure adds the letter when it is missing and never doubles
 * it.
 */
export function releaseLabel(version: string): string {
    return /^[0-9]/.test(version) ? `v${version}` : version;
}

export interface RosterSummary {
    readonly expected: number;
    readonly running: number;
    readonly lost: number;
    readonly missing: number;
    /** The newest release among the running instances. */
    readonly newest: string | undefined;
    /** How many services run a release older than the newest. */
    readonly servicesBehind: number;
}

/**
 * The roster in four counts and a release.
 *
 * The compute runners are left out, as the services screen leaves them out:
 * the grid screen shows them against the nodes they run on, and counting them
 * here would make the two screens state different totals.
 */
export function summariseRoster(rows: readonly ServiceRosterRow[]): RosterSummary {
    const own = rows.filter((row) => row.service_name !== COMPUTE_RUNNER);
    const running = own.filter((row) => row.state === 'running');
    const newest = newestVersionOf(running);
    const behind = new Set(
        running
            .filter(
                (row) =>
                    newest !== undefined &&
                    row.version !== null &&
                    row.version !== '' &&
                    !sameVersion(row.version, newest),
            )
            .map((row) => row.service_name),
    );
    return {
        expected: own.length,
        running: running.length,
        lost: own.filter((row) => row.state === 'lost').length,
        missing: own.filter((row) => row.state === 'missing').length,
        newest,
        servicesBehind: behind.size,
    };
}

/** How many entries of one level the logs hold in a range, by asking for a page of one. */
async function countLogs(level: 'error' | 'warn', range: LogsRange): Promise<number> {
    const view = await api.logs({
        range,
        level,
        source: '',
        component: '',
        tag: '',
        message: '',
        offset: 0,
        limit: 1,
    });
    return view.total;
}

/**
 * The figures, each read on its own.
 *
 * A failed logs read leaves the roster figures standing and states the counts
 * as unread, because six figures that disappear together say less than four
 * that stay and two that say they could not be read.
 */
export function useInstallationHealth(range: LogsRange): {
    readonly roster: RosterSummary | undefined;
    readonly rosterFailed: boolean;
    readonly errors: number | undefined;
    readonly warnings: number | undefined;
    readonly logsFailed: boolean;
} {
    const roster = useQuery({ queryKey: SERVICES_QUERY_KEY, queryFn: api.services });
    const errors = useQuery({
        queryKey: logCountKey('error', range),
        queryFn: () => countLogs('error', range),
    });
    const warnings = useQuery({
        queryKey: logCountKey('warn', range),
        queryFn: () => countLogs('warn', range),
    });
    return {
        roster: roster.data === undefined ? undefined : summariseRoster(roster.data),
        rosterFailed: roster.isError,
        errors: errors.data,
        warnings: warnings.data,
        logsFailed: errors.isError || warnings.isError,
    };
}

type Tone = 'good' | 'warn' | 'bad' | 'neutral';

const TONE_CLASS: Readonly<Record<Tone, string>> = {
    good: 'text-up',
    warn: 'text-warn',
    bad: 'text-down',
    neutral: 'text-ink',
};

function Figure({
    value,
    label,
    to,
    tone,
    note,
}: {
    readonly value: string;
    readonly label: string;
    readonly to: string;
    readonly tone: Tone;
    readonly note?: string;
}): ReactNode {
    return (
        <Link to={to} className="grid gap-1 hover:text-accent-bright">
            <span className={`text-3xl font-semibold tabular-nums ${TONE_CLASS[tone]}`}>
                {value}
            </span>
            <span className="text-xs text-ink-muted">{label}</span>
            {note !== undefined && <span className="text-xs text-warn">{note}</span>}
        </Link>
    );
}

/** The six figures for one range. */
export function InstallationFigures({
    range,
    compact = false,
}: {
    readonly range: LogsRange;
    /** Two columns instead of six, for a panel a third of the page wide. */
    readonly compact?: boolean;
}): ReactNode {
    const { t, plural } = useTranslation();
    const { roster, rosterFailed, errors, warnings, logsFailed } = useInstallationHealth(range);
    const logsNote = logsFailed ? t('operations.overview.logsUnread') : undefined;
    const unread = '—';
    const count = (value: number | undefined): string =>
        value === undefined ? unread : String(value);

    if (rosterFailed && roster === undefined) {
        return <p className="text-sm text-ink-muted">{t('operations.overview.unreadable')}</p>;
    }

    return (
        <div
            className={
                compact ? 'grid grid-cols-2 gap-4' : 'grid gap-6 sm:grid-cols-3 lg:grid-cols-6'
            }
        >
            <Figure
                to="/operations/services"
                label={t('operations.overview.running')}
                value={
                    roster === undefined
                        ? unread
                        : t('operations.services.count', {
                              reported: roster.running,
                              expected: roster.expected,
                          })
                }
                tone={
                    roster === undefined
                        ? 'neutral'
                        : roster.expected > 0 && roster.running === roster.expected
                          ? 'good'
                          : 'warn'
                }
            />
            <Figure
                to="/operations/services"
                label={t('operations.overview.lost')}
                value={count(roster?.lost)}
                tone={roster !== undefined && roster.lost > 0 ? 'warn' : 'neutral'}
            />
            <Figure
                to="/operations/services"
                label={t('operations.overview.missing')}
                value={count(roster?.missing)}
                tone={roster !== undefined && roster.missing > 0 ? 'bad' : 'neutral'}
            />
            <Figure
                to="/operations/logs"
                label={t('operations.overview.errors', {
                    range: t(`operations.logs.range.${range}`),
                })}
                value={count(errors)}
                {...(logsNote === undefined ? {} : { note: logsNote })}
                tone={errors !== undefined && errors > 0 ? 'bad' : 'neutral'}
            />
            <Figure
                to="/operations/logs"
                label={t('operations.overview.warnings', {
                    range: t(`operations.logs.range.${range}`),
                })}
                value={count(warnings)}
                {...(logsNote === undefined ? {} : { note: logsNote })}
                tone={warnings !== undefined && warnings > 0 ? 'warn' : 'neutral'}
            />
            <Figure
                to="/operations/services"
                label={
                    roster === undefined || roster.servicesBehind === 0
                        ? t('operations.overview.release')
                        : plural('operations.overview.behind', roster.servicesBehind)
                }
                value={roster?.newest === undefined ? unread : releaseLabel(roster.newest)}
                tone={roster !== undefined && roster.servicesBehind > 0 ? 'warn' : 'neutral'}
            />
        </div>
    );
}
