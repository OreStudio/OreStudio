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

import type { ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import type { PricingVocabulary } from '../api/pricingEngines.js';
import { Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import {
    methodInfo,
    modelInfo,
    pairNote,
    settingsFor,
    templateInfo,
    type Change,
    type Described,
    type Finding,
} from './model.js';

/**
 * The parts the pricing engine screens share: the findings of a check and the
 * difference between two versions, drawn the same way on the record page, in
 * the author journey and on the import.
 */

type Translate = (key: string, values?: Record<string, string>) => string;

/** The sentence a finding is told as. */
export function findingText(t: Translate, finding: Finding): string {
    return t(`pricingEngines.findings.${finding.code}`, {
        type: finding.typeCode ?? '',
        detail: finding.detail ?? '',
    });
}

/** The findings of a check, worst first, each in its own tone. */
export function FindingsList({
    findings,
    heading,
}: {
    readonly findings: readonly Finding[];
    readonly heading?: string;
}): ReactNode {
    const { t } = useTranslation();
    if (findings.length === 0) {
        return <Notice tone="success">{t('pricingEngines.findings.clean')}</Notice>;
    }
    const tone = { error: 'error', warning: 'warn', info: 'info' } as const;
    return (
        <section className="space-y-2" aria-label={heading ?? t('pricingEngines.findings.heading')}>
            {heading !== undefined && <h3 className="text-sm font-medium">{heading}</h3>}
            {findings.map((finding, index) => (
                <Notice
                    key={`${finding.code}-${finding.productId ?? ''}-${index}`}
                    tone={tone[finding.level]}
                >
                    {findingText(t, finding)}
                </Notice>
            ))}
        </section>
    );
}

/** The change as a line a person reads: what was, and what is. */
function ChangeLine({ change }: { readonly change: Change }): ReactNode {
    const { t } = useTranslation();
    const subject =
        change.what === 'configuration'
            ? t(`pricingEngines.fields.${change.name}`)
            : change.typeCode === '' || change.what === 'product'
              ? change.name
              : `${change.typeCode} · ${change.name}`;
    const tone = change.kind === 'added' ? 'up' : change.kind === 'removed' ? 'down' : 'warn';
    return (
        <li className="flex flex-wrap items-baseline gap-2 text-sm">
            <Tag tone={tone} small>
                {t(`pricingEngines.diff.${change.kind}`)}
            </Tag>
            <span className="text-ink-muted">{t(`pricingEngines.diff.${change.what}`)}</span>
            <span className="font-medium">{subject}</span>
            {change.before !== '' && (
                <span className="font-mono text-xs text-ink-faint line-through">
                    {change.before}
                </span>
            )}
            {change.after !== '' && <span className="font-mono text-xs">{change.after}</span>}
        </li>
    );
}

/** What a save would change, as the history will draw it. */
export function DifferenceList({ changes }: { readonly changes: readonly Change[] }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="space-y-2" aria-label={t('pricingEngines.diff.heading')}>
            <h3 className="text-sm font-medium">{t('pricingEngines.diff.heading')}</h3>
            {changes.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('pricingEngines.diff.none')}</p>
            ) : (
                <ul className="space-y-1">
                    {changes.map((change, index) => (
                        <ChangeLine
                            key={`${change.what}-${change.name}-${index}`}
                            change={change}
                        />
                    ))}
                </ul>
            )}
        </section>
    );
}

/** Where the words of a model or an engine come from, as the person reads it. */
function SourceTag({ source }: { readonly source: string }): ReactNode {
    const { t } = useTranslation();
    return (
        <Tag small tone={source === 'authored' ? 'warn' : 'muted'}>
            {t(`pricingEngines.source.${source === '' ? 'unknown' : source}`)}
        </Tag>
    );
}

/** What a model or an engine is, drawn beside the control that picks it. */
function Description({
    info,
    empty,
}: {
    readonly info: Described | undefined;
    readonly empty: string;
}): ReactNode {
    if (info === undefined) {
        return <p className="text-xs text-ink-faint">{empty}</p>;
    }
    return (
        <div className="space-y-1 rounded-md border border-line-subtle bg-surface-raised/40 p-3 text-xs">
            <p className="font-medium text-ink">{info.description}</p>
            {info.detail !== '' && <p className="text-ink-muted">{info.detail}</p>}
            <SourceTag source={info.source} />
        </div>
    );
}

/**
 * The model and the engine of a product, each with its own description beside it,
 * and below them how ORE builds the engine of the pair for this product type.
 */
export function ModelEnginePanels({
    vocabulary,
    typeCode,
    model,
    engine,
    models,
    engines,
    onModel,
    onEngine,
    allowBlank = false,
}: {
    readonly vocabulary: PricingVocabulary;
    readonly typeCode: string;
    readonly model: string;
    readonly engine: string;
    readonly models: readonly string[];
    readonly engines: readonly string[];
    readonly onModel: (model: string) => void;
    readonly onEngine: (engine: string) => void;
    readonly allowBlank?: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const note = pairNote(vocabulary, typeCode, model, engine);
    const pick = (
        label: string,
        value: string,
        options: readonly string[],
        onChange: (value: string) => void,
        info: Described | undefined,
    ) => (
        <div className="space-y-2">
            <Field label={label}>
                <Select value={value} onChange={(event) => onChange(event.target.value)}>
                    {allowBlank && <option value="" />}
                    {options.map((entry) => (
                        <option key={entry} value={entry}>
                            {entry}
                        </option>
                    ))}
                </Select>
            </Field>
            <Description info={info} empty={t('pricingEngines.describe.none')} />
        </div>
    );
    return (
        <div className="space-y-3">
            <div className="grid gap-4 sm:grid-cols-2">
                {pick(
                    t('pricingEngines.fields.model'),
                    model,
                    models,
                    onModel,
                    modelInfo(vocabulary, model),
                )}
                {pick(
                    t('pricingEngines.fields.engine'),
                    engine,
                    engines,
                    onEngine,
                    methodInfo(vocabulary, engine),
                )}
            </div>
            {note !== '' && (
                <p className="text-xs text-ink-muted">
                    <span className="font-medium">
                        {t('pricingEngines.describe.pair', { type: typeCode })}
                    </span>{' '}
                    {note}
                </p>
            )}
        </div>
    );
}

/**
 * The choices a value set states, as a select of their labels. A value the set
 * no longer offers stays selectable, so a save never drops it unasked.
 */
export function ChoiceSelect({
    choices,
    value,
    label,
    onChange,
}: {
    readonly choices: readonly { readonly value: string; readonly label: string }[];
    readonly value: string;
    readonly label?: string;
    readonly onChange: (value: string) => void;
}): ReactNode {
    return (
        <Select
            {...(label !== undefined ? { 'aria-label': label } : {})}
            value={value}
            onChange={(event) => onChange(event.target.value)}
        >
            {[
                ...choices,
                ...(choices.some((choice) => choice.value === value)
                    ? []
                    : [{ value, label: value }]),
            ].map((choice) => (
                <option key={choice.value} value={choice.value}>
                    {choice.label}
                </option>
            ))}
        </Select>
    );
}

/**
 * One setting's value, drawn for the values the vocabulary gives it: a select
 * for a list the value set states, a tenor or a sensitivity template, a number
 * bounded by the range, and a field for the rest. The meaning of the setting
 * stands under the field.
 */
export /** The scope a link row uses for a setting's scope in the contract. */
function linkScope(scope: string): string {
    if (scope === 'model') return 'ModelParameters';
    if (scope === 'engine') return 'EngineParameters';
    return scope;
}

export function SettingValue({
    vocabulary,
    typeCode,
    model,
    engine,
    name,
    scope,
    value,
    onChange,
}: {
    readonly vocabulary: PricingVocabulary;
    readonly typeCode: string;
    readonly model: string;
    readonly engine: string;
    readonly name: string;
    readonly scope: string;
    readonly value: string;
    readonly onChange: (value: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    /*
     * The value model is the contract's: the link table names one set for every
     * (scope, name), and the reply serves them all in one map. The setting row
     * supplies the words, never the values.
     */
    const setting =
        settingsFor(vocabulary, typeCode, model, engine).find(
            (candidate) => candidate.name === name && candidate.scope === scope,
        ) ??
        vocabulary.settings.find(
            (candidate) => candidate.name === name && candidate.scope === scope,
        ) ??
        vocabulary.settings.find((candidate) => candidate.name === name);
    const allowed = vocabulary.valueSets[`${linkScope(scope)}|${name}`];
    const kind = setting?.kind ?? 'text';
    const choices = allowed?.choices ?? [];
    const sourceKind = allowed?.sourceKind ?? '';
    const ranged = sourceKind === 'range';
    const numeric = ranged || sourceKind === 'integer' || sourceKind === 'number';
    const template = kind === 'reference' ? templateInfo(vocabulary, value) : undefined;
    const meaning =
        template === undefined
            ? (setting?.help ?? '')
            : `${template.assetClass} · ${template.method}. ${template.description}`;
    const label = `${name} (${scope})${setting?.required === true ? ` · ${t('pricingEngines.fields.required')}` : ''}`;
    return (
        <Field label={label} {...(meaning !== '' ? { hint: meaning } : {})}>
            {choices.length > 0 ? (
                <ChoiceSelect choices={choices} value={value} onChange={onChange} />
            ) : (
                <Input
                    value={value}
                    {...(numeric ? { type: 'number' as const } : {})}
                    {...(ranged && allowed?.minimum !== null && allowed?.minimum !== undefined
                        ? { min: allowed.minimum }
                        : {})}
                    {...(ranged && allowed?.maximum !== null && allowed?.maximum !== undefined
                        ? { max: allowed.maximum }
                        : {})}
                    {...(numeric ? { step: sourceKind === 'integer' ? 1 : ('any' as const) } : {})}
                    {...(numeric
                        ? {
                              inputMode:
                                  sourceKind === 'integer'
                                      ? ('numeric' as const)
                                      : ('decimal' as const),
                          }
                        : {})}
                    onChange={(event) => onChange(event.target.value)}
                />
            )}
        </Field>
    );
}
