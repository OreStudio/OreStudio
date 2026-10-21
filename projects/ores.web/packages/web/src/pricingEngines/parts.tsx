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
import { settingsFor, type Change, type Finding } from './model.js';

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

/** One setting's value, as a select where the vocabulary knows the options and as text where it does not. */
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
    const setting = settingsFor(vocabulary, typeCode, model, engine).find(
        (candidate) => candidate.name === name && candidate.scope === scope,
    );
    const options = setting?.options ?? [];
    return (
        <Field label={`${name} (${scope})`} {...(setting?.note ? { hint: setting.note } : {})}>
            {options.length > 0 ? (
                <Select value={value} onChange={(event) => onChange(event.target.value)}>
                    {[...new Set([...options, value])].map((entry) => (
                        <option key={entry} value={entry}>
                            {entry}
                        </option>
                    ))}
                </Select>
            ) : (
                <Input value={value} onChange={(event) => onChange(event.target.value)} />
            )}
        </Field>
    );
}
