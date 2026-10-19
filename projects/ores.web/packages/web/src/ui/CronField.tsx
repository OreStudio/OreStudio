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

/**
 * A schedule where a record states it: in words. An expression the builder does
 * not put in words is shown as it is, because hiding it would hide the schedule.
 */
export function CronValue({ expression }: { readonly expression: string }): ReactNode {
    const { language } = useTranslation();
    const says = describeCron(expression, language);
    return says === undefined ? <span className="font-mono">{expression}</span> : <>{says}</>;
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
    const { t, language } = useTranslation();
    const id = useId();
    const [building, setBuilding] = useState(false);
    const trimmed = value.trim();
    const invalid = trimmed !== '' && !cronValid(trimmed);
    const says = describeCron(trimmed, language);
    return (
        <div>
            <label htmlFor={id} className="mb-1.5 block text-sm font-medium text-ink-muted">
                {label}
            </label>
            <div className="flex gap-2">
                <Input
                    id={id}
                    readOnly
                    value={says ?? trimmed}
                    placeholder={t('cron.none')}
                    aria-invalid={invalid}
                    className={cx(
                        'cursor-default',
                        says === undefined && trimmed !== '' && 'font-mono',
                        invalid && 'border-down',
                    )}
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
type Way = 'common' | 'custom' | 'advanced';

/** The builder: three ways to state a schedule, of which one is live at a time. */
export function CronBuilder({
    expression,
    onApply,
    onCancel,
}: {
    readonly expression: string;
    readonly onApply: (expression: string) => void;
    readonly onCancel: () => void;
}): ReactNode {
    const { t, language } = useTranslation();
    const [text, setText] = useState(expression === '' ? PRESETS[0].expression : expression);
    const schedule = parseSchedule(text);
    const preset = PRESETS.find((candidate) => candidate.expression === text.trim());
    const [way, setWay] = useState<Way>(
        preset !== undefined ? 'common' : schedule !== undefined ? 'custom' : 'advanced',
    );
    const valid = cronValid(text.trim());
    const says = describeCron(text, language);
    const edit = (patch: Partial<Schedule>): void =>
        setText(cronOf({ ...(schedule ?? STARTING_SCHEDULE), ...patch }));
    /*
     * Choosing a way makes the text one that way can show, so the controls of
     * the live group always read what will be applied.
     */
    const choose = (next: Way): void => {
        setWay(next);
        if (next === 'common' && preset === undefined) {
            setText(PRESETS[0].expression);
        } else if (next === 'custom' && schedule === undefined) {
            setText(cronOf(STARTING_SCHEDULE));
        }
    };
    const group = (id: Way, title: string, children: ReactNode): ReactNode => (
        <section className="space-y-3 rounded-md border border-line p-3">
            <label className="flex items-center gap-2 text-sm font-medium">
                <input
                    type="radio"
                    name="schedule-way"
                    checked={way === id}
                    onChange={() => choose(id)}
                />
                {title}
            </label>
            <fieldset disabled={way !== id} className={cx('pl-6', way !== id && 'opacity-50')}>
                {children}
            </fieldset>
        </section>
    );
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
            <div className="space-y-4">
                <div className="rounded-md border border-line bg-surface-sunken px-3 py-2">
                    <div className="text-xs text-ink-muted">{t('cron.runs')}</div>
                    <div className="text-sm">
                        {valid ? (says ?? text.trim()) : t('cron.invalid')}
                    </div>
                </div>

                {group(
                    'common',
                    t('cron.ways.common'),
                    <div className="space-y-2">
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
                                        {describeCron(entry.expression, language)}
                                    </span>
                                </span>
                            </label>
                        ))}
                    </div>,
                )}

                {group(
                    'custom',
                    t('cron.ways.custom'),
                    <Controls schedule={schedule ?? STARTING_SCHEDULE} edit={edit} />,
                )}

                {group(
                    'advanced',
                    t('cron.ways.advanced'),
                    <div className="space-y-1">
                        <Input
                            value={text}
                            spellCheck={false}
                            aria-label={t('cron.expression')}
                            aria-invalid={way === 'advanced' && !valid}
                            className={cx(
                                'font-mono',
                                way === 'advanced' && !valid && 'border-down',
                            )}
                            onChange={(event) => setText(event.target.value)}
                        />
                        {way === 'advanced' &&
                            (valid ? (
                                says !== undefined && (
                                    <span className="block text-xs text-ink-faint">{says}</span>
                                )
                            ) : (
                                <span className="block text-xs text-down">{t('cron.invalid')}</span>
                            ))}
                    </div>,
                )}
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
                            {t('cron.weekdaysOnly')}
                        </Button>
                    </div>
                </div>
            )}
        </div>
    );
}
