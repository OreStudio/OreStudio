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
import type { SensitivityTemplateChoices } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Field, Input, Select } from '../ui/Primitives.js';
import { useChoices } from './choices.js';

/**
 * The token ORE writes in a key for one method, or nothing while the choices
 * are still being read. A key carries the token and never the words, so the
 * form states the token rather than asking for it.
 */
export function methodCodeOf(
    choices: SensitivityTemplateChoices | undefined,
    method: string,
): string {
    return choices?.methods.find((candidate) => candidate.label === method)?.code ?? '';
}

/** The method code the chosen method implies, stated under the form's values. */
export function MethodCode({ code }: { readonly code: string }): ReactNode {
    const { t } = useTranslation();
    return (
        <div>
            <span className="block text-sm font-medium text-ink-muted">
                {t('sensitivityTemplates.methodCode')}
            </span>
            <p className="mt-0.5 text-sm">
                {code === '' ? (
                    <span className="text-ink-faint">—</span>
                ) : (
                    <span className="font-mono text-xs">{code}</span>
                )}
            </p>
            <p className="mt-0.5 text-xs text-ink-faint">
                {t('sensitivityTemplates.methodCodeHelp')}
            </p>
        </div>
    );
}

/**
 * The values a template states, as every form that changes one draws them: the
 * asset class and the method are each a select over what the table allows,
 * because a row that states anything else is refused by the store.
 */
export function TemplateFields({
    assetClass,
    method,
    description,
    onAssetClass,
    onMethod,
    onDescription,
}: {
    readonly assetClass: string;
    readonly method: string;
    readonly description: string;
    readonly onAssetClass: (value: string) => void;
    readonly onMethod: (value: string) => void;
    readonly onDescription: (value: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const choices = useChoices();
    return (
        <div className="grid gap-3 sm:grid-cols-2">
            <Field label={t('sensitivityTemplates.assetClass')}>
                <Select value={assetClass} onChange={(event) => onAssetClass(event.target.value)}>
                    {(choices?.assetClasses ?? []).map((code) => (
                        <option key={code} value={code}>
                            {code}
                        </option>
                    ))}
                </Select>
            </Field>
            <Field label={t('sensitivityTemplates.method')}>
                <Select value={method} onChange={(event) => onMethod(event.target.value)}>
                    {(choices?.methods ?? []).map((choice) => (
                        <option key={choice.label} value={choice.label}>
                            {choice.label}
                        </option>
                    ))}
                </Select>
            </Field>
            <div className="sm:col-span-2">
                <Field label={t('sensitivityTemplates.description')}>
                    <Input
                        value={description}
                        maxLength={2000}
                        onChange={(event) => onDescription(event.target.value)}
                    />
                </Field>
            </div>
        </div>
    );
}
