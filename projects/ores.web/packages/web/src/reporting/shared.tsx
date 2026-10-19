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
import { useBadge } from '../ui/AccountLockBadge.js';
import { Label } from '../ui/Label.js';
import { parseTimestamp } from '../ui/Time.js';

/**
 * What the two reporting screens share: the health word as a badge, the
 * duration of a run, and the words the cells use when a value is absent.
 *
 * A health word is the server's vocabulary and never the definition's state, so
 * this module only paints it. The duration is derived here because the browser
 * is the only side that holds the wire timestamp's reader, and the run states
 * two instants rather than a length.
 */

/**
 * The badge of the shared catalogue that carries each health word's colour.
 *
 * The catalogue is the source of every badge in the interface, so the word is
 * painted in the badge the deployment holds rather than in a colour this screen
 * chose: a failed run is the red the catalogue gives a failed task, a clean one
 * the green of a done task, a run in flight the sky of a running task, and a
 * definition that never ran the gray the catalogue reserves for inactive. A word
 * with no definition — /completed with errors/ has none in the seed — is drawn
 * as the catalogue's unmapped badge, which is orange on purpose, so the gap in
 * the seed data is visible rather than camouflaged as a state.
 */
const HEALTH_BADGES: Readonly<Record<ReportHealthWord, string | undefined>> = {
    failed: 'outcome_failed',
    'completed with errors': undefined,
    running: 'state_running',
    'never run': 'inactive',
    'completed clean': 'state_done',
};

/** The catalogue's fallback badge code, drawn for a value with no definition of its own. */
const UNMAPPED = '__unmapped__';

/**
 * The health of a definition's latest run, as the catalogue paints it.
 *
 * A stated `stateName` with no word is a state the server has not mapped to one
 * of the five, so the badge says that in words and names the state it came from
 * rather than borrowing a nearby word. A row whose latest-run read failed is
 * `unknown`: the absence belongs to the read, so no state was seen and none can
 * be named, and no badge is painted for a state nobody read.
 */
export function HealthTag({
    health,
    stateName,
    hasRun,
}: {
    readonly health: ReportHealthWord | null;
    readonly stateName: string;
    /** Whether the definition has a run at all; a definition with none never answered. */
    readonly hasRun: boolean;
}): ReactNode {
    const { t } = useTranslation();
    if (health === null) {
        // A definition with no run and no word is one whose latest-run read did
        // not answer: the absence belongs to the read, so no word is claimed.
        if (!hasRun) {
            return <Label text={t('reporting.health.unknown')} badge={undefined} />;
        }
        const state = stateName === '' ? '' : ` (${stateName})`;
        return <Label text={`${t('reporting.health.noWord')}${state}`} badge={undefined} />;
    }
    return <HealthBadge word={health} text={t(HEALTH_KEY[health])} />;
}

/** The translation key of each health word. */
const HEALTH_KEY: Readonly<Record<ReportHealthWord, string>> = {
    failed: 'reporting.health.failed',
    'completed with errors': 'reporting.health.completedWithErrors',
    running: 'reporting.health.running',
    'never run': 'reporting.health.neverRun',
    'completed clean': 'reporting.health.completedClean',
};

/** One health word in the badge the catalogue gives it, or the unmapped badge when it has none. */
function HealthBadge({
    word,
    text,
}: {
    readonly word: ReportHealthWord;
    readonly text: string;
}): ReactNode {
    const badge = useBadge(HEALTH_BADGES[word] ?? UNMAPPED);
    return <Label text={text} badge={badge} />;
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

/** A cell the server stated nothing for, drawn as an absence rather than as a value. */
export function Absent(): ReactNode {
    return (
        <span className="text-ink-faint" aria-hidden="true">
            —
        </span>
    );
}
