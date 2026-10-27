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
 * Whether the chosen method is one whose templates name an asset class.
 *
 * The methods table states it, so the form asks rather than repeating that the
 * trade-level method is the one ORE defines without a class.
 */
export function needsAssetClass(
    choices: SensitivityTemplateChoices | undefined,
    methodCode: string,
): boolean {
    return (
        choices?.methods.find((candidate) => candidate.code === methodCode)?.needsAssetClass ?? true
    );
}

/**
 * The code a method's words stand for, or empty when no method reads so.
 *
 * A history entry states the words a person reads, so restoring a version maps
 * them back through the same lookup that wrote them.
 */
export function codeOfMethodName(
    choices: SensitivityTemplateChoices | undefined,
    name: string,
): string {
    return choices?.methods.find((candidate) => candidate.name === name)?.code ?? '';
}

/** The code an asset class's words stand for, or null when no class reads so. */
export function codeOfAssetClassName(
    choices: SensitivityTemplateChoices | undefined,
    name: string,
): string | null {
    return choices?.assetClasses.find((candidate) => candidate.name === name)?.code ?? null;
}

/** The token ORE writes in a key for the chosen method, as the method states it. */
export function methodCodeOf(
    choices: SensitivityTemplateChoices | undefined,
    methodCode: string,
): string {
    return choices?.methods.find((candidate) => candidate.code === methodCode)?.code ?? methodCode;
}

/** The method code the chosen method spells, stated under the form's values. */
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
 * The values a template states, as every form that changes one draws them.
 *
 * Both are selects over what the tables hold: the method by the words a person
 * reads, and the asset class by its name. A method whose templates name no class
 * clears the class and holds it closed, so a trade-level template is written
 * without one rather than refused for carrying one.
 */
export function TemplateFields({
    methodCode,
    assetClassCode,
    description,
    onMethod,
    onAssetClass,
    onDescription,
}: {
    readonly methodCode: string;
    /** The chosen class, or empty when the method names none. */
    readonly assetClassCode: string;
    readonly description: string;
    readonly onMethod: (code: string) => void;
    readonly onAssetClass: (code: string) => void;
    readonly onDescription: (value: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const choices = useChoices();
    const classless = !needsAssetClass(choices, methodCode);
    const choose = (code: string): void => {
        onMethod(code);
        if (!needsAssetClass(choices, code)) {
            onAssetClass('');
        }
    };

    return (
        <div className="grid gap-3 sm:grid-cols-2">
            <Field label={t('sensitivityTemplates.method')}>
                <Select value={methodCode} onChange={(event) => choose(event.target.value)}>
                    {(choices?.methods ?? []).map((method) => (
                        <option key={method.code} value={method.code}>
                            {method.name}
                        </option>
                    ))}
                </Select>
            </Field>
            <Field
                label={t('sensitivityTemplates.assetClass')}
                {...(classless ? { hint: t('sensitivityTemplates.tradeNoClass') } : {})}
            >
                <Select
                    value={classless ? '' : assetClassCode}
                    disabled={classless}
                    onChange={(event) => onAssetClass(event.target.value)}
                >
                    {classless ? (
                        <option value="">{t('sensitivityTemplates.noAssetClass')}</option>
                    ) : (
                        (choices?.assetClasses ?? []).map((assetClass) => (
                            <option key={assetClass.code} value={assetClass.code}>
                                {assetClass.name}
                            </option>
                        ))
                    )}
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
