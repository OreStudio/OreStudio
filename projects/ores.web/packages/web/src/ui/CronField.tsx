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
 * A field for a cron expression, and the builder that fills it.
 *
 * The field is the expression as text, which a person who knows cron types, with
 * its meaning in words under it when it is a common shape, and a refusal when it
 * is not an expression. The Build button opens the builder: one tab for each of
 * the five parts, each stating its part as a frequency, a range or a set of
 * ticked values. Any form that asks for a schedule uses this field.
 */

import { useId, useState, type ReactNode } from 'react';
import { createPortal } from 'react-dom';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Input, Select, cx } from './Primitives.js';
import {
    CRON_FIELDS,
    DEFAULT_CRON,
    cronText,
    cronValid,
    describeCron,
    parseCron,
    valueLabel,
    type CronFieldConfig,
    type CronFieldModel,
    type CronMode,
} from './cron.js';

/** What an expression means in words, when it is a common shape. */
function say(
    t: (key: string, values?: Record<string, string>) => string,
    expression: string,
): string | undefined {
    const sentence = describeCron(expression);
    if (sentence === undefined) {
        return undefined;
    }
    const day = sentence.weekday === undefined ? {} : { day: t(`cron.days.${sentence.weekday}`) };
    return t(sentence.key, { ...sentence.values, ...day });
}

export function CronField({
    label,
    value,
    onChange,
    onBlur,
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
    const says = invalid ? undefined : say(t, trimmed);
    return (
        <div>
            <label htmlFor={id} className="mb-1.5 block text-sm font-medium text-ink-muted">
                {label}
            </label>
            <div className="flex gap-2">
                <Input
                    id={id}
                    value={value}
                    disabled={disabled}
                    spellCheck={false}
                    placeholder={DEFAULT_CRON}
                    aria-invalid={invalid}
                    className={cx('font-mono', invalid && 'border-down')}
                    onBlur={onBlur}
                    onChange={(event) => onChange(event.target.value)}
                />
                <Button
                    type="button"
                    disabled={disabled}
                    onClick={() => setBuilding(true)}
                    aria-label={t('cron.open')}
                >
                    {t('cron.build')}
                </Button>
            </div>
            {says !== undefined && <span className="mt-1 block text-xs text-ink">{says}</span>}
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

/** The builder: five tabs over one expression, applied only on OK. */
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
    const [models, setModels] = useState<readonly CronFieldModel[]>(() => parseCron(expression));
    const [tab, setTab] = useState(0);
    const text = cronText(models);
    const config = CRON_FIELDS[tab] ?? CRON_FIELDS[0];
    const model = models[tab];
    return (
        <Dialog
            title={t('cron.title')}
            name="cron builder"
            onClose={onCancel}
            wide
            footer={
                <div className="flex justify-end gap-2">
                    <Button type="button" variant="ghost" onClick={onCancel}>
                        {t('cron.cancel')}
                    </Button>
                    <Button type="button" variant="primary" onClick={() => onApply(text)}>
                        {t('cron.apply')}
                    </Button>
                </div>
            }
        >
            <div className="space-y-4">
                <div className="rounded-md border border-line bg-surface-sunken px-3 py-2 text-sm">
                    <div className="font-mono">{text}</div>
                    <div className="text-xs text-ink-muted">
                        {say(t, text) ?? t('cron.says.other')}
                    </div>
                </div>
                <div
                    role="tablist"
                    aria-label={t('cron.title')}
                    className="flex gap-1 border-b border-line"
                >
                    {CRON_FIELDS.map((field, index) => (
                        <button
                            key={field.name}
                            type="button"
                            role="tab"
                            aria-selected={index === tab}
                            className={cx(
                                '-mb-px border-b-2 px-3 py-2 text-sm',
                                index === tab
                                    ? 'border-accent text-ink'
                                    : 'border-transparent text-ink-muted hover:text-ink',
                            )}
                            onClick={() => setTab(index)}
                        >
                            {t(`cron.fields.${field.name}`)}
                        </button>
                    ))}
                </div>
                {config !== undefined && model !== undefined && (
                    <FieldPanel
                        config={config}
                        model={model}
                        onChange={(next) =>
                            setModels(models.map((was, index) => (index === tab ? next : was)))
                        }
                    />
                )}
            </div>
        </Dialog>
    );
}

/** One part of the expression, stated three ways of which one is on. */
function FieldPanel({
    config,
    model,
    onChange,
}: {
    readonly config: CronFieldConfig;
    readonly model: CronFieldModel;
    readonly onChange: (model: CronFieldModel) => void;
}): ReactNode {
    const { t } = useTranslation();
    const group = useId();
    const values = Array.from({ length: config.max - config.min + 1 }, (_, i) => config.min + i);
    const change = (patch: Partial<CronFieldModel>): void => {
        const { kept: _kept, ...rest } = model;
        onChange({ ...rest, ...patch });
    };
    const mode = (name: CronMode): ReactNode => (
        <input
            type="radio"
            name={group}
            checked={model.kept === undefined && model.mode === name}
            onChange={() => change({ mode: name })}
            aria-label={t(`cron.modes.${name}`)}
        />
    );
    const option = (value: number): ReactNode => (
        <option key={value} value={value}>
            {valueLabel(config, value)}
        </option>
    );
    const number = (event: { target: { value: string } }): number =>
        Number.parseInt(event.target.value, 10);
    const on = (name: CronMode): boolean => model.kept === undefined && model.mode === name;
    return (
        <div className="space-y-4">
            {model.kept !== undefined && (
                <p className="text-xs text-ink-muted">{t('cron.kept', { value: model.kept })}</p>
            )}
            <div className="flex flex-wrap items-center gap-3">
                {mode('frequency')}
                <span className="w-24 text-sm">{t('cron.modes.frequency')}</span>
                <span className="text-sm text-ink-muted">{t('cron.every')}</span>
                <Select
                    className="w-24"
                    disabled={!on('frequency')}
                    value={model.every}
                    aria-label={t('cron.every')}
                    onChange={(event) => change({ every: number(event) })}
                >
                    <option value={1}>{t('cron.each')}</option>
                    {config.steps.map((step) => (
                        <option key={step} value={step}>
                            {step}
                        </option>
                    ))}
                </Select>
                <span className="text-sm text-ink-muted">{t('cron.starting')}</span>
                <Select
                    className="w-28"
                    disabled={!on('frequency')}
                    value={model.start}
                    aria-label={t('cron.starting')}
                    onChange={(event) => change({ start: number(event) })}
                >
                    {values.map(option)}
                </Select>
            </div>
            <div className="flex flex-wrap items-center gap-3">
                {mode('range')}
                <span className="w-24 text-sm">{t('cron.modes.range')}</span>
                <span className="text-sm text-ink-muted">{t('cron.from')}</span>
                <Select
                    className="w-28"
                    disabled={!on('range')}
                    value={model.from}
                    aria-label={t('cron.from')}
                    onChange={(event) => change({ from: number(event) })}
                >
                    {values.map(option)}
                </Select>
                <span className="text-sm text-ink-muted">{t('cron.to')}</span>
                <Select
                    className="w-28"
                    disabled={!on('range')}
                    value={model.to}
                    aria-label={t('cron.to')}
                    onChange={(event) => change({ to: number(event) })}
                >
                    {values.map(option)}
                </Select>
            </div>
            <div className="space-y-2 border-t border-line pt-3">
                <div className="flex items-center gap-3">
                    {mode('choice')}
                    <span className="text-sm">{t('cron.modes.choice')}</span>
                </div>
                <div
                    className="grid gap-x-3 gap-y-1 pl-6"
                    style={{ gridTemplateColumns: `repeat(${config.perRow}, minmax(0, 1fr))` }}
                >
                    {values.map((value) => (
                        <label key={value} className="flex items-center gap-1 text-sm">
                            <input
                                type="checkbox"
                                disabled={!on('choice')}
                                checked={model.picked.includes(value)}
                                onChange={(event) =>
                                    change({
                                        picked: event.target.checked
                                            ? [...model.picked, value]
                                            : model.picked.filter((picked) => picked !== value),
                                    })
                                }
                            />
                            {valueLabel(config, value)}
                        </label>
                    ))}
                </div>
            </div>
        </div>
    );
}
