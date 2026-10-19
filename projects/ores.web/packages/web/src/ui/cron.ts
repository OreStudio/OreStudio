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

/**
 * The cron expression the scheduler reads: five fields, minute hour day month
 * weekday. This file holds what the cron field and its builder agree on, so the
 * rules are tested without a page. The server stays the authority on whether an
 * expression runs; the check here only tells a person early.
 */

export type CronFieldName = 'minute' | 'hour' | 'day' | 'month' | 'weekday';

/** What one field of the expression accepts and how the builder offers it. */
export interface CronFieldConfig {
    readonly name: CronFieldName;
    readonly min: number;
    readonly max: number;
    /** The words the values go by, from the minimum up; empty for plain numbers. */
    readonly words: readonly string[];
    /** The steps the Frequency choice offers besides every value. */
    readonly steps: readonly number[];
    readonly perRow: number;
}

export const CRON_FIELDS: readonly CronFieldConfig[] = [
    {
        name: 'minute',
        min: 0,
        max: 59,
        words: [],
        steps: [2, 3, 4, 5, 6, 10, 12, 15, 20, 30],
        perRow: 10,
    },
    { name: 'hour', min: 0, max: 23, words: [], steps: [2, 3, 4, 6, 8, 12], perRow: 12 },
    { name: 'day', min: 1, max: 31, words: [], steps: [2, 3, 4, 5, 7, 10, 14], perRow: 10 },
    {
        name: 'month',
        min: 1,
        max: 12,
        words: ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'],
        steps: [2, 3, 4, 6],
        perRow: 6,
    },
    {
        name: 'weekday',
        min: 0,
        max: 6,
        words: ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'],
        steps: [2, 3],
        perRow: 7,
    },
];

/** The default a new schedule starts from: the top of every hour. */
export const DEFAULT_CRON = '0 * * * *';

/** The fields of an expression, split on spaces as the server splits them. */
export function cronParts(expression: string): readonly string[] {
    return expression.split(' ').filter((part) => part !== '');
}

/** A value of a field as the builder labels it. */
export function valueLabel(config: CronFieldConfig, value: number): string {
    return config.words[value - config.min] ?? String(value).padStart(2, '0');
}

/** The number a token stands for, by digits or by the word the field goes by. */
function numberOf(config: CronFieldConfig, token: string): number | undefined {
    if (/^[0-9]+$/.test(token)) {
        return Number.parseInt(token, 10);
    }
    const index = config.words.findIndex((word) => word.toLowerCase() === token.toLowerCase());
    return index < 0 ? undefined : index + config.min;
}

/** Whether a value lies where the field allows; Sunday may be 0 or 7. */
function inRange(config: CronFieldConfig, value: number): boolean {
    const max = config.name === 'weekday' ? 7 : config.max;
    return value >= config.min && value <= max;
}

/** Whether one comma-free item of a field is well formed and in range. */
function itemValid(config: CronFieldConfig, item: string): boolean {
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
    const values = ends.map((end) => numberOf(config, end));
    return values.every((value) => value !== undefined && inRange(config, value));
}

/** Whether one field is well formed: a list of items, each valid. */
export function fieldValid(config: CronFieldConfig, field: string): boolean {
    return field !== '' && field.split(',').every((item) => itemValid(config, item));
}

/** Whether an expression holds five well-formed fields. */
export function cronValid(expression: string): boolean {
    const parts = cronParts(expression);
    return (
        parts.length === CRON_FIELDS.length &&
        CRON_FIELDS.every((config, index) => fieldValid(config, parts[index] ?? ''))
    );
}

export type CronMode = 'frequency' | 'range' | 'choice';

/**
 * One field as the builder edits it: a frequency, a range or a set of ticked
 * values. A field the three cannot state, such as a list of ranges, keeps its
 * text in `kept` and is written back unchanged until the person edits it.
 */
export interface CronFieldModel {
    readonly mode: CronMode;
    /** Frequency: how many values apart; 1 is every value. */
    readonly every: number;
    /** Frequency: the value it starts from. */
    readonly start: number;
    readonly from: number;
    readonly to: number;
    readonly picked: readonly number[];
    readonly kept?: string;
}

export function blankModel(config: CronFieldConfig): CronFieldModel {
    return {
        mode: 'frequency',
        every: 1,
        start: config.min,
        from: config.min,
        to: config.max,
        picked: [],
    };
}

function integer(token: string | undefined): number | undefined {
    return token !== undefined && /^[0-9]+$/.test(token) ? Number.parseInt(token, 10) : undefined;
}

/** The model a field's text states; a text it cannot state is kept as typed. */
export function parseField(config: CronFieldConfig, field: string): CronFieldModel {
    const blank = blankModel(config);
    const keep = { ...blank, kept: field };
    if (!fieldValid(config, field)) {
        return keep;
    }
    if (field === '*') {
        return blank;
    }
    if (field.includes('/')) {
        const [span = '', step] = field.split('/');
        const every = integer(step);
        const start = span === '*' ? config.min : integer(span);
        const inGrid = every !== undefined && every >= 2 && every <= config.max;
        return start !== undefined && inGrid && inRange(config, start) && start <= config.max
            ? { ...blank, every, start }
            : keep;
    }
    if (field.includes('-') && !field.includes(',')) {
        const [low, high] = field.split('-').map((end) => numberOf(config, end));
        return low !== undefined && high !== undefined && high <= config.max
            ? { ...blank, mode: 'range', from: low, to: high }
            : keep;
    }
    const picked = field.split(',').map((item) => numberOf(config, item));
    const plain = picked.every(
        (value): value is number =>
            value !== undefined && value >= config.min && value <= config.max,
    );
    return plain ? { ...blank, mode: 'choice', picked: [...new Set(picked)] } : keep;
}

/** The text of a field for its model. */
export function fieldText(config: CronFieldConfig, model: CronFieldModel): string {
    if (model.kept !== undefined) {
        return model.kept;
    }
    if (model.mode === 'frequency') {
        const atMin = model.start === config.min;
        if (model.every === 1) {
            return atMin ? '*' : String(model.start);
        }
        return `${atMin ? '*' : model.start}/${model.every}`;
    }
    if (model.mode === 'range') {
        return `${model.from}-${model.to}`;
    }
    const total = config.max - config.min + 1;
    const values = [...model.picked].sort((a, b) => a - b);
    return values.length === 0 || values.length === total ? '*' : values.join(',');
}

/** The models of an expression; an expression of another size starts from the default. */
export function parseCron(expression: string): readonly CronFieldModel[] {
    const parts = cronParts(cronParts(expression).length === 5 ? expression : DEFAULT_CRON);
    return CRON_FIELDS.map((config, index) => parseField(config, parts[index] ?? '*'));
}

/** The expression the models state. */
export function cronText(models: readonly CronFieldModel[]): string {
    return CRON_FIELDS.map((config, index) => {
        const model = models[index];
        return model === undefined ? '*' : fieldText(config, model);
    }).join(' ');
}

/** A sentence for an expression, as a translation key and its values. */
export interface CronSentence {
    readonly key: string;
    readonly values?: Readonly<Record<string, string>>;
    /** The day of the week the sentence names, 0 for Sunday; the page words it. */
    readonly weekday?: number;
}

/** The hour as a 24-hour clock reads it, which no language reads two ways. */
function clock(hour: string): string {
    const value = integer(hour);
    return value === undefined || value > 23
        ? `${hour}:00`
        : `${String(value).padStart(2, '0')}:00`;
}

/**
 * What an expression means, when it is one of the common shapes. Any other
 * expression has no sentence, and the field shows the text alone.
 */
export function describeCron(expression: string): CronSentence | undefined {
    const parts = cronParts(expression);
    if (parts.length !== 5 || !cronValid(expression)) {
        return undefined;
    }
    const [minute = '', hour = '', day = '', month = '', weekday = ''] = parts;
    if (parts.every((part) => part === '*')) {
        return { key: 'cron.says.everyMinute' };
    }
    if (minute === '0' && hour === '*' && day === '*' && month === '*' && weekday === '*') {
        return { key: 'cron.says.everyHour' };
    }
    if (minute === '0' && day === '*' && month === '*') {
        const at = clock(hour);
        if (weekday === '*') {
            return { key: 'cron.says.daily', values: { time: at } };
        }
        if (weekday === '1-5') {
            return { key: 'cron.says.weekdays', values: { time: at } };
        }
        const number = integer(weekday);
        if (number !== undefined && number <= 7) {
            return { key: 'cron.says.weekly', values: { time: at }, weekday: number % 7 };
        }
    }
    if (minute === '0' && weekday === '*' && month === '*') {
        const number = integer(day);
        if (number !== undefined && number >= 1 && number <= 31) {
            return { key: 'cron.says.monthly', values: { time: clock(hour), day: String(number) } };
        }
    }
    return undefined;
}
