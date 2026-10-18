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
import type { ReportHealthWord } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { Tag } from '../ui/Primitives.js';
import { parseTimestamp } from '../ui/Time.js';

/**
 * What the two reporting screens share: the health word as a marker, the
 * duration of a run, and the words the cells use when a value is absent.
 *
 * A health word is the server's vocabulary and never the definition's state, so
 * this module only paints it. The duration is derived here because the browser
 * is the only side that holds the wire timestamp's reader, and the run states
 * two instants rather than a length.
 */

/** The translation key of each health word, and the tone it is drawn with. */
const HEALTH: Readonly<
    Record<
        ReportHealthWord,
        { readonly key: string; readonly tone: 'down' | 'warn' | 'accent' | 'muted' | 'up' }
    >
> = {
    failed: { key: 'reporting.health.failed', tone: 'down' },
    'completed with errors': { key: 'reporting.health.completedWithErrors', tone: 'warn' },
    running: { key: 'reporting.health.running', tone: 'accent' },
    'never run': { key: 'reporting.health.neverRun', tone: 'muted' },
    'completed clean': { key: 'reporting.health.completedClean', tone: 'up' },
};

/**
 * The health of a definition's latest run, as a marker.
 *
 * A stated `stateName` with no word is a state the server has not mapped to one
 * of the five, so the marker says that in words and names the state it came
 * from rather than borrowing a nearby word.
 */
export function HealthTag({
    health,
    stateName,
}: {
    readonly health: ReportHealthWord | null;
    readonly stateName: string;
}): ReactNode {
    const { t } = useTranslation();
    if (health === null) {
        const state = stateName === '' ? '' : ` (${stateName})`;
        return (
            <Tag tone="muted">
                {t('reporting.health.noWord')}
                {state}
            </Tag>
        );
    }
    const { key, tone } = HEALTH[health];
    return <Tag tone={tone}>{t(key)}</Tag>;
}

/** How long a run took, in milliseconds, or nothing when it has not finished. */
export function runDuration(startedAt: string | null, completedAt: string | null): number | null {
    if (startedAt === null || completedAt === null) {
        return null;
    }
    const elapsed = parseTimestamp(completedAt).getTime() - parseTimestamp(startedAt).getTime();
    return elapsed < 0 ? null : elapsed;
}

/** A length of time as the units a person reads, so a duration of 12.5s is not 12500. */
export function formatDuration(ms: number, t: Translator['t']): string {
    if (ms < 1000) {
        return t('reporting.duration.milliseconds', { value: ms });
    }
    if (ms < 60_000) {
        return t('reporting.duration.seconds', { value: (ms / 1000).toFixed(1) });
    }
    if (ms < 3_600_000) {
        return t('reporting.duration.minutes', { value: (ms / 60_000).toFixed(1) });
    }
    return t('reporting.duration.hours', { value: (ms / 3_600_000).toFixed(1) });
}

/** A count in the reader's own digits and grouping, from the platform's number formatter. */
export function formatCount(value: number, language: string): string {
    return new Intl.NumberFormat(language).format(value);
}

/**
 * The problems a run recorded, split into errors and warnings.
 *
 * The split is the whole point of the cell: a run that completed carrying errors
 * is drawn apart from a clean one without opening anything.
 */
export function ProblemCounts({
    errors,
    warnings,
}: {
    readonly errors: number | null;
    readonly warnings: number | null;
}): ReactNode {
    const { t, plural, language } = useTranslation();
    if (errors === null || warnings === null) {
        return <span className="text-ink-faint">{t('reporting.cell.unread')}</span>;
    }
    if (errors === 0 && warnings === 0) {
        return <span className="text-ink-faint">{t('reporting.cell.noProblems')}</span>;
    }
    return (
        <span className="flex flex-wrap gap-1">
            <Tag tone={errors > 0 ? 'down' : 'muted'}>
                {plural('reporting.problems.errors', errors, {
                    count: formatCount(errors, language),
                })}
            </Tag>
            <Tag tone={warnings > 0 ? 'warn' : 'muted'}>
                {plural('reporting.problems.warnings', warnings, {
                    count: formatCount(warnings, language),
                })}
            </Tag>
        </span>
    );
}

/** A cell the server stated nothing for, drawn as an absence rather than as a value. */
export function Absent(): ReactNode {
    return (
        <span className="text-ink-faint" aria-hidden="true">
            —
        </span>
    );
}
