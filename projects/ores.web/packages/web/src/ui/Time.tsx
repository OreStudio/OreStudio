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
import { useTranslation } from '../i18n/Provider.js';

const STEPS: readonly (readonly [Intl.RelativeTimeFormatUnit, number])[] = [
    ['second', 60],
    ['minute', 60],
    ['hour', 24],
    ['day', 30],
    ['month', 12],
    ['year', Number.POSITIVE_INFINITY],
];

/** A server timestamp, such as "2026-10-04 22:48:27Z", as a date; invalid when it is not one. */
export function parseTimestamp(at: string): Date {
    return new Date(at.includes('T') ? at : at.replace(' ', 'T'));
}

/** How long ago a moment was, in the person's language, such as "2 hours ago". */
export function relativeTime(at: Date, now: Date, language: string): string {
    let amount = (at.getTime() - now.getTime()) / 1000;
    for (const [unit, size] of STEPS) {
        if (Math.abs(amount) < size) {
            return new Intl.RelativeTimeFormat(language, { numeric: 'auto' }).format(
                Math.round(amount),
                unit,
            );
        }
        amount /= size;
    }
    return at.toISOString();
}

/**
 * A server timestamp as its local date and time, such as "8 Oct 2026, 01:15".
 *
 * A record a person reads has to say *when*, and a date alone cannot say it:
 * two requests made on the same day are hours apart, and which of them came
 * first is the thing the list is sorted by. A server states the moment in UTC,
 * so it is drawn in the reader's own zone, and in the reader's own language.
 * A value that is not a timestamp is returned as it arrived, because showing
 * the raw value is better than showing a wrong one.
 */
export function formatDateTime(at: string, language: string): string {
    const date = parseTimestamp(at);
    if (Number.isNaN(date.getTime())) {
        return at;
    }
    return new Intl.DateTimeFormat(language, {
        dateStyle: 'medium',
        timeStyle: 'short',
    }).format(date);
}

/** A timestamp drawn relative, with the exact value on hover, as the record screen standard sets it. */
export function RelativeTime({ at }: { readonly at: string }): ReactNode {
    const { language } = useTranslation();
    const date = parseTimestamp(at);
    if (Number.isNaN(date.getTime())) {
        return <>{at}</>;
    }
    return (
        <time dateTime={date.toISOString()} title={at}>
            {relativeTime(date, new Date(), language)}
        </time>
    );
}
