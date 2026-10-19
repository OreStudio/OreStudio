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

import cronstrue from 'cronstrue';
import 'cronstrue/locales/fr';
import 'cronstrue/locales/pt_PT';
import type { Language } from '../i18n/languages.js';

/**
 * The cron expression the scheduler reads: five fields, minute hour day month
 * weekday. This file holds what the cron field and its builder agree on, so the
 * rules are tested without a page. The server stays the authority on whether an
 * expression runs; the check here only tells a person early.
 *
 * The builder states a schedule the way a person who does not know cron says
 * it: every few minutes or hours inside a window, at a time of day, or on a day
 * of the month, on the days picked. An expression of another shape is valid and
 * is kept as typed; it is never rewritten into one of these.
 */

const FIELD_RANGES = [
    { name: 'minute', min: 0, max: 59, words: [] as readonly string[] },
    { name: 'hour', min: 0, max: 23, words: [] as readonly string[] },
    { name: 'day', min: 1, max: 31, words: [] as readonly string[] },
    {
        name: 'month',
        min: 1,
        max: 12,
        words: ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'],
    },
    { name: 'weekday', min: 0, max: 6, words: ['sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat'] },
] as const;

type FieldRange = (typeof FIELD_RANGES)[number];

/** The default a new schedule starts from: the top of every hour. */
export const DEFAULT_CRON = '0 * * * *';

/** The fields of an expression, split on spaces as the server splits them. */
export function cronParts(expression: string): readonly string[] {
    return expression.split(' ').filter((part) => part !== '');
}

/** The number a token stands for, by digits or by the word the field goes by. */
function numberOf(range: FieldRange, token: string): number | undefined {
    if (/^[0-9]+$/.test(token)) {
        return Number.parseInt(token, 10);
    }
    const index = (range.words as readonly string[]).indexOf(token.toLowerCase());
    return index < 0 ? undefined : index + range.min;
}

/** Whether a value lies where the field allows; Sunday may be 0 or 7. */
function inRange(range: FieldRange, value: number): boolean {
    const max = range.name === 'weekday' ? 7 : range.max;
    return value >= range.min && value <= max;
}

/** Whether one comma-free item of a field is well formed and in range. */
function itemValid(range: FieldRange, item: string): boolean {
    const [span = '', step, ...rest] = item.split('/');
    if (rest.length > 0 || (step !== undefined && !/^[0-9]+$/.test(step))) {
        return false;
    }
    if (step !== undefined && Number.parseInt(step, 10) < 1) {
        return false;
    }
    if (span === '*') {
        return true;
    }
    const ends = span.split('-');
    if (ends.length > 2) {
        return false;
    }
    return ends
        .map((end) => numberOf(range, end))
        .every((value) => value !== undefined && inRange(range, value));
}

/** Whether an expression holds five well-formed fields. */
export function cronValid(expression: string): boolean {
    const parts = cronParts(expression);
    return (
        parts.length === FIELD_RANGES.length &&
        FIELD_RANGES.every((range, index) => {
            const field = parts[index] ?? '';
            return field !== '' && field.split(',').every((item) => itemValid(range, item));
        })
    );
}

export type Repeat = 'interval' | 'daily' | 'monthly';

/** The steps an interval offers, in minutes; an hour and more are whole hours. */
export const INTERVAL_STEPS: readonly number[] = [5, 10, 15, 30, 60, 120, 240];

/** The days of the week in the order a working week reads, Sunday last. */
export const WEEK: readonly number[] = [1, 2, 3, 4, 5, 6, 0];

const WORKING_DAYS: readonly number[] = [1, 2, 3, 4, 5];

/**
 * A schedule as the builder states it. Only the fields of its `repeat` are
 * read: an interval has a step and a window, a daily run a time, a monthly run
 * a time and a day of the month.
 */
export interface Schedule {
    readonly repeat: Repeat;
    readonly step: number;
    /** The first and last hour of an interval's window. */
    readonly from: number;
    readonly to: number;
    readonly hour: number;
    readonly minute: number;
    readonly day: number;
    /** The days of the week it runs on, Sunday 0; none or all means every day. */
    readonly days: readonly number[];
}

/** What the builder starts from when an expression is none of its shapes. */
export const STARTING_SCHEDULE: Schedule = {
    repeat: 'daily',
    step: 60,
    from: 0,
    to: 23,
    hour: 7,
    minute: 0,
    day: 1,
    days: WORKING_DAYS,
};

/** The expressions a middle office asks for most, offered before any control. */
export const PRESETS = [
    { id: 'premarket', expression: '0 7 * * 1-5' },
    { id: 'intraday', expression: '0 8-18 * * 1-5' },
    { id: 'eod', expression: '30 18 * * 1-5' },
] as const;

function daysField(days: readonly number[]): string {
    const picked = [...new Set(days)].sort((a, b) => a - b);
    if (picked.length === 0 || picked.length === 7) {
        return '*';
    }
    return picked.join(',') === WORKING_DAYS.join(',') ? '1-5' : picked.join(',');
}

function windowField(schedule: Schedule): string {
    return schedule.from === 0 && schedule.to === 23 ? '*' : `${schedule.from}-${schedule.to}`;
}

/** The expression a schedule states. */
export function cronOf(schedule: Schedule): string {
    const days = daysField(schedule.days);
    if (schedule.repeat === 'monthly') {
        return `${schedule.minute} ${schedule.hour} ${schedule.day} * *`;
    }
    if (schedule.repeat === 'daily') {
        return `${schedule.minute} ${schedule.hour} * * ${days}`;
    }
    const window = windowField(schedule);
    if (schedule.step < 60) {
        return `*/${schedule.step} ${window} * * ${days}`;
    }
    if (schedule.step === 60) {
        return `0 ${window} * * ${days}`;
    }
    return `0 ${window}/${schedule.step / 60} * * ${days}`;
}

function digits(token: string | undefined): number | undefined {
    return token !== undefined && /^[0-9]+$/.test(token) ? Number.parseInt(token, 10) : undefined;
}

function daysOf(field: string): readonly number[] | undefined {
    if (field === '*') {
        return [];
    }
    if (field === '1-5') {
        return WORKING_DAYS;
    }
    const days = field.split(',').map(digits);
    return days.every((day): day is number => day !== undefined && day <= 6) ? days : undefined;
}

/** The hours of an interval's window: every hour, or a first and a last. */
function windowOf(field: string): { from: number; to: number } | undefined {
    if (field === '*') {
        return { from: 0, to: 23 };
    }
    const [from, to, ...rest] = field.split('-').map(digits);
    return rest.length === 0 && from !== undefined && to !== undefined && from <= to && to <= 23
        ? { from, to }
        : undefined;
}

function scheduleOf(parts: readonly string[]): Schedule | undefined {
    const [minute = '', hour = '', day = '', month = '', weekday = ''] = parts;
    const days = daysOf(weekday);
    if (month !== '*' || days === undefined) {
        return undefined;
    }
    const base = { ...STARTING_SCHEDULE, days };
    const at = { minute: digits(minute), hour: digits(hour), day: digits(day) };
    if (at.minute !== undefined && at.hour !== undefined && at.minute <= 59 && at.hour <= 23) {
        if (day === '*') {
            return { ...base, repeat: 'daily', minute: at.minute, hour: at.hour };
        }
        if (at.day !== undefined && at.day >= 1 && at.day <= 31 && weekday === '*') {
            return { ...base, repeat: 'monthly', minute: at.minute, hour: at.hour, day: at.day };
        }
        return undefined;
    }
    if (day !== '*') {
        return undefined;
    }
    if (minute.startsWith('*/')) {
        const step = digits(minute.slice(2));
        const window = windowOf(hour);
        return step !== undefined && INTERVAL_STEPS.includes(step) && step < 60 && window
            ? { ...base, repeat: 'interval', step, ...window }
            : undefined;
    }
    if (minute !== '0') {
        return undefined;
    }
    const [span = '', every, ...rest] = hour.split('/');
    const hours = every === undefined ? 1 : digits(every);
    const window = windowOf(span);
    return rest.length === 0 && hours !== undefined && INTERVAL_STEPS.includes(hours * 60) && window
        ? { ...base, repeat: 'interval', step: hours * 60, ...window }
        : undefined;
}

/**
 * The schedule an expression states, or nothing when it is none of the
 * builder's shapes. A shape is recognised only when it writes back as the same
 * expression, so the builder never changes one it shows.
 */
export function parseSchedule(expression: string): Schedule | undefined {
    const parts = cronParts(expression);
    if (parts.length !== 5 || !cronValid(expression)) {
        return undefined;
    }
    const schedule = scheduleOf(parts);
    return schedule !== undefined && cronOf(schedule) === parts.join(' ') ? schedule : undefined;
}

/** The cronstrue locale for each language the interface speaks. */
const LOCALE: Readonly<Record<Language, string>> = { en: 'en', fr: 'fr', pt: 'pt_PT' };

/**
 * An expression in words, in the language given: the one place the codebase
 * turns a cron expression into a sentence, so the field, the details, the list
 * and the builder cannot word one schedule two ways. The words come from
 * cronstrue, a library for this, on a 24-hour clock. An expression the server
 * would refuse has no words.
 */
export function describeCron(expression: string, language: Language): string | undefined {
    const text = expression.trim();
    if (!cronValid(text)) {
        return undefined;
    }
    try {
        return cronstrue.toString(text, {
            locale: LOCALE[language],
            use24HourTimeFormat: true,
            throwExceptionOnParseError: true,
        });
    } catch {
        return undefined;
    }
}
