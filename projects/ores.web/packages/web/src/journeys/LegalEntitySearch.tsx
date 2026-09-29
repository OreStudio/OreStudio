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
 * Finding the legal entity a tenant or a party is built around.
 *
 * A person who has an LEI knows it; a person who does not cannot guess one, so
 * the field is a search over the entities the deployment holds rather than a
 * text box. The matching belongs to the read: the deployment holds tens of
 * thousands of entities, so the one somebody is looking for is not on any page
 * a screen could fetch.
 *
 * The field has two states and shows one at a time. While nothing is chosen it
 * is a search box: typing asks the read again once the typing settles, and the
 * matches are listed with what each would bring. Once an entity is chosen the
 * box goes away and the entity stands in its place — named, with its LEI, its
 * country, and the parties its hierarchy would create — beside a *Change*
 * action. A search box that keeps the letters somebody typed after choosing
 * from it says nothing about what was chosen, which is the state this field
 * exists to state.
 *
 * What the screen says about an entity is only what the read answered: the
 * legal name, the LEI, the country and the count. Nothing about the hierarchy
 * is invented here.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Input, Notice, cx } from '../ui/Primitives.js';
import type { JourneyServer } from './server.js';
import type { LeiEntityChoice } from '@ores/wire-protocol/browser';

/** How long the typing has to stop before the read is asked. */
export const SEARCH_SETTLE_MS = 250;

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

/**
 * The entity a tenant is built around, once one is chosen.
 *
 * `entity` is absent while the form holds a value this screen has not read, or
 * could not read: the LEI is what the run carries, so the LEI is what stands.
 */
function Chosen({
    label,
    lei,
    entity,
    onChange,
}: {
    readonly label: string;
    readonly lei: string;
    readonly entity: LeiEntityChoice | undefined;
    readonly onChange: () => void;
}): ReactNode {
    const { t, plural } = useTranslation();
    return (
        <div className="sm:col-span-2">
            <span className="mb-1.5 block text-sm font-medium text-ink-muted">{label}</span>
            <div className="rounded-md border border-accent bg-surface-overlay p-3">
                {entity === undefined ? (
                    <p className="font-mono text-sm">{lei}</p>
                ) : (
                    <>
                        <p className="text-sm font-medium text-ink">{entity.legalName}</p>
                        <p className="mt-0.5 font-mono text-xs text-ink-faint">
                            {entity.lei}
                            {entity.country === '' ? '' : ` · ${entity.country}`}
                        </p>
                        <p className="mt-2 text-xs text-ink-muted">
                            {plural('journey.details.leiParties', entity.partyCount)}
                        </p>
                    </>
                )}
                <div className="mt-2 flex justify-end">
                    <Button variant="ghost" size="sm" onClick={onChange}>
                        {t('journey.details.leiChange')}
                    </Button>
                </div>
            </div>
        </div>
    );
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
    /**
     * What the field says underneath itself, and the whole of it.
     *
     * The sentence a journey wants here is the journey's: what choosing an
     * entity fills in is a tenant's name and a party's name, and a field that
     * carried one of those sentences would say it in the other journey too.
     */
    readonly hint: string;
    readonly onChoose: (entity: LeiEntityChoice) => void;
}): ReactNode {
    const { t, plural } = useTranslation();
    const [query, setQuery] = useState('');
    const [matches, setMatches] = useState<readonly LeiEntityChoice[]>([]);
    const [searched, setSearched] = useState('');
    const [failure, setFailure] = useState<string>();
    const [chosen, setChosen] = useState<LeiEntityChoice>();
    const [changing, setChanging] = useState(false);

    /*
     * The entity a value names, when this screen did not do the choosing: the
     * form may hold one from a profile's defaults, or from before a reload.
     */
    useEffect(() => {
        if (value === '' || chosen?.lei === value) {
            return;
        }
        let cancelled = false;
        void (async () => {
            try {
                const found = await server.leiEntities(value);
                const exact = found.find((entity) => entity.lei === value);
                if (!cancelled && exact !== undefined) {
                    setChosen(exact);
                }
            } catch {
                /*
                 * The name and the count are what is missing, not the choice:
                 * the LEI the form holds is what the run carries.
                 */
            }
        })();
        return () => {
            cancelled = true;
        };
    }, [server, value, chosen]);

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

    const pick = (entity: LeiEntityChoice): void => {
        setChosen(entity);
        setChanging(false);
        setQuery('');
        setMatches([]);
        setSearched('');
        onChoose(entity);
    };

    if (value !== '' && !changing) {
        return (
            <Chosen
                label={label}
                lei={value}
                entity={chosen?.lei === value ? chosen : undefined}
                onChange={() => setChanging(true)}
            />
        );
    }

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
            {hint !== '' && <span className="mt-1 block text-xs text-ink-faint">{hint}</span>}
            {value !== '' && (
                <div className="mt-2 flex justify-end">
                    <Button variant="ghost" size="sm" onClick={() => setChanging(false)}>
                        {t('journey.details.leiKeep')}
                    </Button>
                </div>
            )}

            {failure !== undefined && (
                <div className="mt-3">
                    <Notice tone="error">
                        {t('journey.details.leiReadFailed', { message: failure })}
                    </Notice>
                </div>
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
                                onClick={() => pick(entity)}
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
