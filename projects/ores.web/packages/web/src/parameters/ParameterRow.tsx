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
import { Field } from '../ui/Primitives.js';

/**
 * One parameter as both configuration screens frame it: its name, the mark of a
 * value it must have, the control that holds the value and the hint under it.
 *
 * A screen states the label and the hint in its own words, because they come
 * from the data it reads; what it does not state is how a parameter's identity
 * is drawn, which is why one row is shared.
 */
export function ParameterRow({
    label,
    hint,
    required = false,
    children,
}: {
    readonly label: string;
    /** What the parameter means, drawn under the control. */
    readonly hint?: string;
    /** Whether the parameter must have a value, marked beside its label. */
    readonly required?: boolean;
    readonly children: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    const name = required ? `${label} · ${t('parameters.required')}` : label;
    return (
        <Field label={name} {...(hint === undefined || hint === '' ? {} : { hint })}>
            {children}
        </Field>
    );
}
