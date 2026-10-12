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
 *
 */

import type { ReactNode } from 'react';
import type { RequestChecks } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Notice, Tag } from '../ui/Primitives.js';

/**
 * What running the request's lines through the real write now says.
 *
 * The record may have moved since the request was raised, so a decider reads
 * the checks as they stand. A request whose lines all stand says so in one
 * line, and a refused line says why, so the decider does not approve a change
 * that cannot be made. A kind that keeps no lines has no checks and draws
 * nothing.
 */
export function ChecksPanel({ checks }: { readonly checks: RequestChecks }): ReactNode {
    const { t } = useTranslation();
    if (checks.lines.length === 0) return null;
    const refused = checks.lines.filter((line) => line.refusal !== '');
    return (
        <section className="space-y-2">
            <h2 className="text-sm font-semibold">{t('inbox.checks.title')}</h2>
            {refused.length === 0 ? (
                <Notice tone="success">{t('inbox.checks.allStand')}</Notice>
            ) : (
                <ul className="space-y-2">
                    {refused.map((line) => (
                        <li key={line.lineNo}>
                            <Notice tone="error">
                                <span className="mr-2">
                                    <Tag tone="down">{t('inbox.checks.refused')}</Tag>
                                </span>
                                {t('inbox.delta.line', { line: line.lineNo })}: {line.refusal}
                            </Notice>
                        </li>
                    ))}
                </ul>
            )}
        </section>
    );
}
