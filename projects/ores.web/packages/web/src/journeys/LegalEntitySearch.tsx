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
 * text box. The matching belongs to the read: the deployment holds tens of
 * thousands of entities, so the one somebody is looking for is not on any page
 * a screen could fetch, and a search that fetched one page and filtered it
 * answered "no match" for entities that were there.
 *
 * Typing asks the read again, once the person stops for a moment, and a match
 * states how many parties its hierarchy would create, because that is the work
 * choosing it starts.
 *
 * What the screen states about a match is only what the read answered: the
 * legal name, the LEI, the country and the count. Nothing about the entity's
 * hierarchy is invented here.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Input, Notice, cx } from '../ui/Primitives.js';
import type { JourneyServer } from './server.js';
import type { LeiEntityChoice } from '@ores/wire-protocol/browser';

/** How long the typing has to stop before the read is asked. */
export const SEARCH_SETTLE_MS = 250;

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
    const { t, plural } = useTranslation();
    const [matches, setMatches] = useState<readonly LeiEntityChoice[]>([]);
    const [query, setQuery] = useState('');
    const [searched, setSearched] = useState('');
    const [failure, setFailure] = useState<string>();

    /*
     * The entity the person picked, held here rather than looked up among the
     * matches: the query that found it is usually no longer the one in the box,
     * and the summary of what was chosen has to survive that.
     */
    const [picked, setPicked] = useState<LeiEntityChoice>();
    const chosen = picked?.lei === value ? picked : undefined;

    useEffect(() => {
        const text = query.trim();
        if (text === '') {
            setMatches([]);
            setSearched('');
            setFailure(undefined);
            return;
        }
        let cancelled = false;
        const timer = setTimeout(() => {
            void (async () => {
                try {
                    const answer = await server.leiEntities(text);
                    if (!cancelled) {
                        setMatches(answer);
                        setSearched(text);
                        setFailure(undefined);
                    }
                } catch (error) {
                    if (!cancelled) {
                        setMatches([]);
                        setSearched(text);
                        setFailure(reasonOf(error));
                    }
                }
            })();
        }, SEARCH_SETTLE_MS);
        return () => {
            cancelled = true;
            clearTimeout(timer);
        };
    }, [server, query]);

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
                    <span className="ml-2 text-xs text-ink-faint">
                        {plural('journey.details.leiParties', chosen.partyCount)}
                    </span>
                </p>
            )}

            {searched !== '' && matches.length === 0 && failure === undefined && (
                <p className="mt-3 text-sm text-ink-muted">{t('journey.details.leiNoMatch')}</p>
            )}

            {matches.length > 0 && (
                <ul className="mt-3 max-h-64 space-y-1 overflow-y-auto">
                    {matches.map((entity) => (
                        <li key={entity.lei}>
                            <button
                                type="button"
                                onClick={() => {
                                    setPicked(entity);
                                    onChoose(entity);
                                }}
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
                                <span className="block text-xs text-ink-faint">
                                    {plural('journey.details.leiParties', entity.partyCount)}
                                </span>
                            </button>
                        </li>
                    ))}
                </ul>
            )}
        </div>
    );
}
