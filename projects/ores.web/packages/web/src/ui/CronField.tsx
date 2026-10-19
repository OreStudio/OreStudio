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
 * A field for a schedule, and the builder that fills it.
 *
 * The field states the schedule in words and cannot be typed into: a person who
 * does not know cron never meets it, and one who does opens the builder. The
 * builder offers the common schedules first, then controls for the rest: every
 * few minutes or hours inside a window, a time of day, or a day of the month,
 * on the days picked. The cron expression itself is under Advanced, for the
 * schedules the controls do not state. A schedule that needs a business-day
 * calendar is not offered, because the scheduler reads cron and no calendar.
 */

import { useId, useState, type ReactNode } from 'react';
import { createPortal } from 'react-dom';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Input, Select, cx } from './Primitives.js';
import {
    INTERVAL_STEPS,
    PRESETS,
    STARTING_SCHEDULE,
    WEEK,
    cronOf,
    cronValid,
    describeCron,
    parseSchedule,
    type Repeat,
    type Schedule,
} from './cron.js';

type Translate = (key: string, values?: Record<string, string>) => string;

/** The days a sentence runs on, in words. */
function daysIn(t: Translate, days: readonly number[]): string {
    const picked = WEEK.filter((day) => days.includes(day));
    if (picked.length === 0) {
        return t('cron.days.every');
    }
    if (picked.join() === '1,2,3,4,5') {
        return t('cron.days.weekdays');
    }
    if (picked.join() === '6,0') {
        return t('cron.days.weekend');
    }
    return picked.map((day) => t(`cron.day.${day}`)).join(', ');
}

/** What an expression means in words, when it is one the builder states. */
export function sayCron(t: Translate, expression: string): string | undefined {
    const sentence = describeCron(expression.trim());
    if (sentence === undefined) {
        return undefined;
    }
    const unit = sentence.values['unit'];
    return t(sentence.key, {
        ...sentence.values,
        ...(unit === undefined ? {} : { step: t(`cron.says.step.${unit}`, sentence.values) }),
        days: daysIn(t, sentence.days),
    });
}

export function CronField({
    label,
    value,
    onChange,
    disabled = false,
    hint,
    error,
}: {
    readonly label: string;
    readonly value: string;
    readonly onChange: (value: string) => void;
    readonly onBlur?: (() => void) | undefined;
    readonly disabled?: boolean;
    readonly hint?: string | undefined;
    readonly error?: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const id = useId();
    const [building, setBuilding] = useState(false);
    const trimmed = value.trim();
    const invalid = trimmed !== '' && !cronValid(trimmed);
    const shown = trimmed === '' ? '' : ((invalid ? undefined : sayCron(t, trimmed)) ?? trimmed);
    return (
        <div>
            <label htmlFor={id} className="mb-1.5 block text-sm font-medium text-ink-muted">
                {label}
            </label>
            <div className="flex gap-2">
                <Input
                    id={id}
                    readOnly
                    value={shown ?? trimmed}
                    placeholder={t('cron.none')}
                    aria-invalid={invalid}
                    className={cx('cursor-default', invalid && 'border-down')}
                />
                <Button
                    type="button"
                    disabled={disabled}
                    onClick={() => setBuilding(true)}
                    aria-label={t('cron.open')}
                >
                    {t('cron.change')}
                </Button>
            </div>
            {hint !== undefined && (
                <span className="mt-1 block text-xs text-ink-faint">{hint}</span>
            )}
            {invalid && <span className="mt-1 block text-xs text-down">{t('cron.invalid')}</span>}
            {error !== undefined && <span className="mt-1 block text-xs text-down">{error}</span>}
            {building &&
                createPortal(
                    <CronBuilder
                        expression={trimmed}
                        onCancel={() => setBuilding(false)}
                        onApply={(expression) => {
                            onChange(expression);
                            setBuilding(false);
                        }}
                    />,
                    document.body,
                )}
        </div>
    );
}

const hourOptions = Array.from({ length: 24 }, (_, hour) => hour);

/** The builder: presets, then the controls, applied only on the button. */
export function CronBuilder({
    expression,
    onApply,
    onCancel,
}: {
    readonly expression: string;
    readonly onApply: (expression: string) => void;
    readonly onCancel: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [text, setText] = useState(expression === '' ? PRESETS[0].expression : expression);
    const schedule = parseSchedule(text);
    const preset = PRESETS.find((candidate) => candidate.expression === text.trim());
    const valid = cronValid(text.trim());
    const says = valid ? sayCron(t, text) : undefined;
    const edit = (patch: Partial<Schedule>): void =>
        setText(cronOf({ ...(schedule ?? STARTING_SCHEDULE), ...patch }));
    return (
        <Dialog
            title={t('cron.title')}
            name="schedule builder"
            onClose={onCancel}
            wide
            footer={
                <div className="flex justify-end gap-2">
                    <Button type="button" variant="ghost" onClick={onCancel}>
                        {t('cron.cancel')}
                    </Button>
                    <Button
                        type="button"
                        variant="primary"
                        disabled={!valid}
                        onClick={() => onApply(text.trim())}
                    >
                        {t('cron.apply')}
                    </Button>
                </div>
            }
        >
            <div className="space-y-5">
                <div className="rounded-md border border-line bg-surface-sunken px-3 py-2">
                    <div className="text-xs text-ink-muted">{t('cron.runs')}</div>
                    <div className="text-sm">
                        {valid ? (says ?? t('cron.says.other')) : t('cron.invalid')}
                    </div>
                </div>

                <fieldset className="space-y-2">
                    <legend className="mb-1 text-sm font-medium text-ink-muted">
                        {t('cron.presets.title')}
                    </legend>
                    {PRESETS.map((entry) => (
                        <label key={entry.id} className="flex items-start gap-2 text-sm">
                            <input
                                type="radio"
                                name="schedule-preset"
                                checked={preset?.id === entry.id}
                                onChange={() => setText(entry.expression)}
                            />
                            <span>
                                {t(`cron.presets.${entry.id}.name`)}
                                <span className="block text-xs text-ink-faint">
                                    {t(`cron.presets.${entry.id}.hint`)}
                                </span>
                            </span>
                        </label>
                    ))}
                    <label className="flex items-start gap-2 text-sm">
                        <input
                            type="radio"
                            name="schedule-preset"
                            checked={preset === undefined}
                            onChange={() => edit({})}
                        />
                        <span>
                            {t('cron.presets.custom.name')}
                            <span className="block text-xs text-ink-faint">
                                {t('cron.presets.custom.hint')}
                            </span>
                        </span>
                    </label>
                </fieldset>

                {preset === undefined &&
                    (schedule === undefined ? (
                        <p className="text-sm text-ink-muted">{t('cron.unshown')}</p>
                    ) : (
                        <Controls schedule={schedule} edit={edit} />
                    ))}

                <details className="text-sm" open={preset === undefined && schedule === undefined}>
                    <summary className="cursor-pointer text-ink-muted">
                        {t('cron.advanced')}
                    </summary>
                    <div className="mt-2 space-y-1">
                        <Input
                            value={text}
                            spellCheck={false}
                            aria-label={t('cron.expression')}
                            aria-invalid={!valid}
                            className={cx('font-mono', !valid && 'border-down')}
                            onChange={(event) => setText(event.target.value)}
                        />
                        <span className="block text-xs text-ink-faint">
                            {t('cron.expressionHint')}
                        </span>
                    </div>
                </details>
            </div>
        </Dialog>
    );
}

/** The controls for a schedule the builder states. */
function Controls({
    schedule,
    edit,
}: {
    readonly schedule: Schedule;
    readonly edit: (patch: Partial<Schedule>) => void;
}): ReactNode {
    const { t } = useTranslation();
    const time = `${String(schedule.hour).padStart(2, '0')}:${String(schedule.minute).padStart(2, '0')}`;
    const hours = (value: number, set: (hour: number) => void, name: string): ReactNode => (
        <Select
            className="w-24"
            value={value}
            aria-label={name}
            onChange={(event) => set(Number.parseInt(event.target.value, 10))}
        >
            {hourOptions.map((hour) => (
                <option key={hour} value={hour}>
                    {String(hour).padStart(2, '0')}:00
                </option>
            ))}
        </Select>
    );
    return (
        <div className="space-y-4">
            <div className="flex flex-wrap items-center gap-3">
                <span className="text-sm text-ink-muted">{t('cron.repeat')}</span>
                <Select
                    className="w-64"
                    value={schedule.repeat}
                    aria-label={t('cron.repeat')}
                    onChange={(event) => edit({ repeat: event.target.value as Repeat })}
                >
                    <option value="interval">{t('cron.repeats.interval')}</option>
                    <option value="daily">{t('cron.repeats.daily')}</option>
                    <option value="monthly">{t('cron.repeats.monthly')}</option>
                </Select>
            </div>

            {schedule.repeat === 'interval' && (
                <div className="flex flex-wrap items-center gap-3">
                    <span className="text-sm text-ink-muted">{t('cron.every')}</span>
                    <Select
                        className="w-36"
                        value={schedule.step}
                        aria-label={t('cron.every')}
                        onChange={(event) =>
                            edit({ step: Number.parseInt(event.target.value, 10) })
                        }
                    >
                        {INTERVAL_STEPS.map((step) => (
                            <option key={step} value={step}>
                                {step < 60
                                    ? t('cron.steps.minutes', { n: String(step) })
                                    : step === 60
                                      ? t('cron.steps.hour')
                                      : t('cron.steps.hours', { n: String(step / 60) })}
                            </option>
                        ))}
                    </Select>
                    <span className="text-sm text-ink-muted">{t('cron.between')}</span>
                    {hours(
                        schedule.from,
                        (from) => edit({ from, to: Math.max(from, schedule.to) }),
                        t('cron.between'),
                    )}
                    <span className="text-sm text-ink-muted">{t('cron.and')}</span>
                    {hours(
                        schedule.to,
                        (to) => edit({ to, from: Math.min(to, schedule.from) }),
                        t('cron.and'),
                    )}
                </div>
            )}

            {schedule.repeat !== 'interval' && (
                <div className="flex flex-wrap items-center gap-3">
                    <span className="text-sm text-ink-muted">{t('cron.at')}</span>
                    <Input
                        type="time"
                        className="w-32"
                        value={time}
                        aria-label={t('cron.at')}
                        onChange={(event) => {
                            const [hour, minute] = event.target.value.split(':').map(Number);
                            if (
                                hour !== undefined &&
                                minute !== undefined &&
                                !Number.isNaN(hour + minute)
                            ) {
                                edit({ hour, minute });
                            }
                        }}
                    />
                </div>
            )}

            {schedule.repeat === 'monthly' ? (
                <div className="flex flex-wrap items-center gap-3">
                    <span className="text-sm text-ink-muted">{t('cron.onDay')}</span>
                    <Select
                        className="w-24"
                        value={schedule.day}
                        aria-label={t('cron.onDay')}
                        onChange={(event) => edit({ day: Number.parseInt(event.target.value, 10) })}
                    >
                        {Array.from({ length: 31 }, (_, i) => i + 1).map((day) => (
                            <option key={day} value={day}>
                                {day}
                            </option>
                        ))}
                    </Select>
                </div>
            ) : (
                <div className="space-y-2">
                    <div className="text-sm text-ink-muted">{t('cron.onDays')}</div>
                    <div className="flex flex-wrap gap-2">
                        {WEEK.map((day) => {
                            const on = schedule.days.length === 0 || schedule.days.includes(day);
                            const days = schedule.days.length === 0 ? WEEK : schedule.days;
                            return (
                                <button
                                    key={day}
                                    type="button"
                                    aria-pressed={on}
                                    className={cx(
                                        'rounded-full border px-3 py-1 text-sm',
                                        on
                                            ? 'border-accent bg-accent/15 text-ink'
                                            : 'border-line text-ink-muted hover:text-ink',
                                    )}
                                    onClick={() => {
                                        const next = on
                                            ? days.filter((d) => d !== day)
                                            : [...days, day];
                                        if (next.length > 0) {
                                            edit({ days: next });
                                        }
                                    }}
                                >
                                    {t(`cron.dayShort.${day}`)}
                                </button>
                            );
                        })}
                        <Button
                            type="button"
                            size="sm"
                            variant="ghost"
                            onClick={() => edit({ days: [1, 2, 3, 4, 5] })}
                        >
                            {t('cron.days.weekdaysOnly')}
                        </Button>
                    </div>
                </div>
            )}
        </div>
    );
}
