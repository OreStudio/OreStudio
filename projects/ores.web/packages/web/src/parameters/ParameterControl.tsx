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
import { Input, Select } from '../ui/Primitives.js';
import type { ControlChoice, ControlModel } from './control.js';

/**
 * The control one parameter's value needs, drawn for the control its value set
 * or its setting row calls for.
 *
 * The value is text either way: a screen holds what the person typed, and what
 * the text means is the screen's own reading, so the control never converts it.
 * A choice keeps the current value selectable when the list no longer offers
 * it, because a save must never drop a value nobody asked to drop.
 */
export function ParameterControl({
    model,
    value,
    onChange,
    label,
    unset,
    invalid,
    describedBy,
    onFocus,
}: {
    readonly model: ControlModel;
    readonly value: string;
    readonly onChange: (value: string) => void;
    /** The accessible name of the control. */
    readonly label: string;
    /**
     * The words for the state in which no value is set: an option on a list, a
     * placeholder in a field. A screen states them only when its value may be absent.
     */
    readonly unset?: string;
    /** Whether the value shown is one the parameter does not take. */
    readonly invalid?: boolean;
    /** The id of the text that states what is wrong with the value. */
    readonly describedBy?: string;
    readonly onFocus?: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const shared = {
        'aria-label': label,
        ...(invalid === undefined ? {} : { 'aria-invalid': invalid }),
        ...(describedBy === undefined ? {} : { 'aria-describedby': describedBy }),
        ...(onFocus === undefined ? {} : { onFocus }),
    };
    switch (model.kind) {
        case 'choice':
            return (
                <Select
                    {...shared}
                    value={value}
                    onChange={(event) => onChange(event.target.value)}
                >
                    {offered(model.choices, value, unset).map((choice) => (
                        <option key={choice.value} value={choice.value}>
                            {choice.label}
                        </option>
                    ))}
                </Select>
            );
        case 'boolean':
            return (
                <Select
                    {...shared}
                    value={value}
                    onChange={(event) => onChange(event.target.value)}
                >
                    {unset !== undefined && <option value="">{unset}</option>}
                    <option value="true">{t('parameters.true')}</option>
                    <option value="false">{t('parameters.false')}</option>
                </Select>
            );
        case 'number':
            return (
                <Input
                    {...shared}
                    type="number"
                    inputMode={model.whole ? 'numeric' : 'decimal'}
                    {...(model.minimum === null ? {} : { min: model.minimum })}
                    {...(model.maximum === null ? {} : { max: model.maximum })}
                    step={model.whole ? 1 : 'any'}
                    {...(unset === undefined ? {} : { placeholder: unset })}
                    value={value}
                    onChange={(event) => onChange(event.target.value)}
                />
            );
        case 'text':
            return (
                <Input
                    {...shared}
                    {...(model.pattern === '' ? {} : { pattern: model.pattern })}
                    {...(unset === undefined ? {} : { placeholder: unset })}
                    value={value}
                    onChange={(event) => onChange(event.target.value)}
                />
            );
    }
}

/**
 * The options a choice draws: the not-set one when the screen offers it, the
 * choices the value set states, and the current value when it states it no more.
 */
function offered(
    choices: readonly ControlChoice[],
    value: string,
    unset: string | undefined,
): readonly ControlChoice[] {
    const stated = unset === undefined ? choices : [{ value: '', label: unset }, ...choices];
    return stated.some((choice) => choice.value === value)
        ? stated
        : [...stated, { value, label: value }];
}
