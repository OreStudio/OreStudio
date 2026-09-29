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

/**
 * Finding the legal entity a tenant is built around.
 *
 * A person who has an LEI knows it; a person who does not cannot guess one, so
 * the field is a search over the entities the deployment holds rather than a
 * text box. The list is read once, when the field first appears, and the search
 * filters it here: the read answers a country and a page, so a name filter is
 * the caller's, and the shortcoming that a set larger than one page cannot be
 * searched honestly is captured rather than hidden.
 *
 * What the screen states about a match is only what the read answered: the
 * legal name, the LEI and the country. Nothing about the entity's hierarchy is
 * invented here.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Input, Notice, cx } from '../ui/Primitives.js';
import type { JourneyServer } from './server.js';
import type { LeiEntityChoice } from '@ores/wire-protocol/browser';

/** How many matches a search offers before it asks for a narrower one. */
const MAX_MATCHES = 20;

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

export function LegalEntitySearch({
    server,
    value,
    label,
    hint,
    onChoose,
}: {
    readonly server: JourneyServer;
    /** The LEI chosen so far, which is what the run will carry. */
    readonly value: string;
    readonly label: string;
    readonly hint: string | undefined;
    readonly onChoose: (entity: LeiEntityChoice) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [entities, setEntities] = useState<readonly LeiEntityChoice[]>([]);
    const [query, setQuery] = useState('');
    const [failure, setFailure] = useState<string>();

    useEffect(() => {
        let cancelled = false;
        void (async () => {
            try {
                const answer = await server.leiEntities();
                if (!cancelled) {
                    setEntities(answer);
                }
            } catch (error) {
                if (!cancelled) {
                    setFailure(reasonOf(error));
                }
            }
        })();
        return () => {
            cancelled = true;
        };
    }, [server]);

    const needle = query.trim().toLowerCase();
    const matches =
        needle === ''
            ? []
            : entities
                  .filter(
                      (entity) =>
                          entity.legalName.toLowerCase().includes(needle) ||
                          entity.lei.toLowerCase().startsWith(needle),
                  )
                  .slice(0, MAX_MATCHES);
    const chosen = entities.find((entity) => entity.lei === value);

    return (
        <div className="sm:col-span-2">
            <label className="block">
                <span className="mb-1.5 block text-sm font-medium text-ink-muted">{label}</span>
                <Input
                    value={query}
                    placeholder={t('journey.details.leiSearch')}
                    onChange={(event) => setQuery(event.target.value)}
                />
            </label>
            {hint !== undefined && hint !== '' && (
                <span className="mt-1 block text-xs text-ink-faint">{hint}</span>
            )}

            {failure !== undefined && (
                <div className="mt-3">
                    <Notice tone="error">
                        {t('journey.details.leiReadFailed', { message: failure })}
                    </Notice>
                </div>
            )}

            {chosen !== undefined && (
                <p className="mt-3 text-sm">
                    <span className="font-medium">{chosen.legalName}</span>
                    <span className="ml-2 font-mono text-xs text-ink-faint">{chosen.lei}</span>
                    <span className="ml-2 text-xs text-ink-faint">{chosen.country}</span>
                </p>
            )}

            {needle !== '' && matches.length === 0 && failure === undefined && (
                <p className="mt-3 text-sm text-ink-muted">{t('journey.details.leiNoMatch')}</p>
            )}

            {matches.length > 0 && (
                <ul className="mt-3 max-h-64 space-y-1 overflow-y-auto">
                    {matches.map((entity) => (
                        <li key={entity.lei}>
                            <button
                                type="button"
                                onClick={() => onChoose(entity)}
                                className={cx(
                                    'w-full rounded-md border border-line p-2 text-left text-sm hover:border-accent',
                                    entity.lei === value && 'border-accent',
                                )}
                            >
                                <span className="block font-medium">{entity.legalName}</span>
                                <span className="block font-mono text-xs text-ink-faint">
                                    {entity.lei}
                                    {entity.country === '' ? '' : ` · ${entity.country}`}
                                </span>
                            </button>
                        </li>
                    ))}
                </ul>
            )}
        </div>
    );
}
