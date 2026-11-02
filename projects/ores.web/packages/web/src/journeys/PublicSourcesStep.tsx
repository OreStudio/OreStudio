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
import { useEffect, useState, type ReactNode } from 'react';
import type { PublicRatesProvider } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice, Tag } from '../ui/Primitives.js';
import type { JourneyServer } from './server.js';

/** What the administrator decided for one publisher in this run. */
export type SourceDecision = 'accepted' | 'declined';

/** How the administrator decides: every publisher at once, or each in turn. */
export type SourcesWay = 'all' | 'each';

/**
 * The decision a publisher stands at: the one drafted in this run, else accepted
 * when its terms already are, else none.
 */
export function decisionOf(
    provider: PublicRatesProvider,
    drafted: Readonly<Record<string, SourceDecision>>,
): SourceDecision | undefined {
    return drafted[provider.code] ?? (provider.terms_accepted ? 'accepted' : undefined);
}

/** Every publisher that states terms has a decision. */
export function everyDecided(
    providers: readonly PublicRatesProvider[],
    drafted: Readonly<Record<string, SourceDecision>>,
): boolean {
    return providers.every((p) => p.terms_url === '' || decisionOf(p, drafted) !== undefined);
}

/** The requests Continue sends: the publishers to accept and the ones to withdraw. */
export interface SourceChanges {
    readonly accept: readonly { readonly code: string; readonly version: number }[];
    readonly decline: readonly string[];
}

/**
 * What the drafted way and decisions ask of the server, or nothing while a
 * publisher is still undecided. Only a change is sent: a publisher already
 * accepted is not accepted again, and one never accepted needs no withdrawal.
 */
export function sourceChanges(
    providers: readonly PublicRatesProvider[],
    way: SourcesWay | undefined,
    drafted: Readonly<Record<string, SourceDecision>>,
): SourceChanges | undefined {
    if (way === undefined) {
        return undefined;
    }
    const decisions = providers.map((p) => ({
        provider: p,
        decision: way === 'all' ? ('accepted' as const) : decisionOf(p, drafted),
    }));
    if (decisions.some((d) => d.decision === undefined)) {
        return undefined;
    }
    return {
        accept: decisions
            .filter((d) => d.decision === 'accepted' && !d.provider.terms_accepted)
            .map((d) => ({ code: d.provider.code, version: d.provider.version })),
        decline: decisions
            .filter((d) => d.decision === 'declined' && d.provider.terms_accepted)
            .map((d) => d.provider.code),
    };
}

type SourcesServer = Pick<
    JourneyServer,
    'publicSources' | 'acceptAllSourceTerms' | 'declineSourceTerms'
>;

/** Sends the changes; a publisher the server refuses fails the whole step with its reason. */
async function sendChanges(server: SourcesServer, changes: SourceChanges): Promise<void> {
    if (changes.accept.length > 0) {
        const outcomes = await server.acceptAllSourceTerms(changes.accept);
        const refused = outcomes.filter((o) => !o.success);
        if (refused.length > 0) {
            throw new Error(refused.map((o) => o.message).join(' '));
        }
    }
    for (const code of changes.decline) {
        await server.declineSourceTerms(code);
    }
}

/**
 * The public sources step of the first run. The administrator accepts every
 * publisher's terms at once, or decides each one, and nothing reaches the
 * server until Continue. An acceptance is recorded by the server against the
 * terms it holds; a decline withdraws any acceptance, so the publisher is not
 * fetched.
 */
export function PublicSourcesStep({
    server,
    onReady,
}: {
    readonly server: SourcesServer;
    /** Hands the journey what Continue sends, or nothing while a publisher is undecided. */
    readonly onReady: (send: (() => Promise<void>) | undefined) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [providers, setProviders] = useState<readonly PublicRatesProvider[]>();
    const [failure, setFailure] = useState<string>();
    const [drafted, setDrafted] = useState<Readonly<Record<string, SourceDecision>>>({});
    const [way, setWay] = useState<SourcesWay>();
    useEffect(() => {
        let live = true;
        server.publicSources().then(
            (rows) => live && setProviders(rows),
            (error: unknown) =>
                live && setFailure(String(error instanceof Error ? error.message : error)),
        );
        return () => {
            live = false;
        };
    }, [server]);
    const list = (providers ?? []).filter((p) => p.terms_url !== '');
    const changes = providers === undefined ? undefined : sourceChanges(list, way, drafted);
    // The changes are a fresh object each render, so their content is what is watched.
    const changesKey = JSON.stringify(changes ?? null);
    useEffect(
        () => onReady(changes === undefined ? undefined : () => sendChanges(server, changes)),
        [changesKey, server, onReady],
    );
    const draft = (provider: PublicRatesProvider, decision: SourceDecision) =>
        setDrafted((before) => ({ ...before, [provider.code]: decision }));

    if (providers === undefined && failure !== undefined) {
        return <Notice tone="error">{failure}</Notice>;
    }
    if (providers === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (list.length === 0) {
        return <p className="text-sm text-ink-muted">{t('journey.sources.none')}</p>;
    }
    const option = (id: SourcesWay, label: string): ReactNode => (
        <label className="flex items-center gap-2 text-sm font-medium">
            <input
                type="radio"
                name="public-sources-way"
                checked={way === id}
                onChange={() => setWay(id)}
            />
            {label}
        </label>
    );
    return (
        <div className="space-y-3">
            <fieldset className="space-y-2">
                <legend className="sr-only">{t('journey.sources.way')}</legend>
                {option('all', t('journey.sources.acceptAll'))}
                {option('each', t('journey.sources.decideEach'))}
            </fieldset>
            <ul className="space-y-2">
                {list.map((p) => {
                    const decision = way === 'all' ? 'accepted' : decisionOf(p, drafted);
                    return (
                        <li key={p.code}>
                            <details
                                className="rounded-md border border-line bg-surface-raised"
                                aria-label={p.name}
                            >
                                <summary className="flex cursor-pointer flex-wrap items-center gap-x-3 gap-y-1 px-4 py-2.5 text-sm">
                                    <span className="min-w-0 flex-1 font-medium">{p.name}</span>
                                    <a
                                        className="text-accent hover:underline"
                                        href={p.terms_url}
                                        target="_blank"
                                        rel="noreferrer"
                                    >
                                        {t('journey.sources.termsLink', {
                                            name: p.code.toUpperCase(),
                                        })}
                                    </a>
                                    {decision === 'accepted' && (
                                        <Tag tone="up" small>
                                            {t('journey.sources.accepted')}
                                        </Tag>
                                    )}
                                    {decision === 'declined' && (
                                        <Tag tone="warn" small>
                                            {t('journey.sources.declined')}
                                        </Tag>
                                    )}
                                </summary>
                                <div className="space-y-2 border-t border-line px-4 py-3 text-sm">
                                    <p className="text-ink-muted">
                                        {p.description !== ''
                                            ? p.description
                                            : t('journey.sources.gives', {
                                                  count: String(p.series.length),
                                              })}
                                    </p>
                                    <dl className="grid gap-2 sm:grid-cols-2">
                                        <div className="min-w-0">
                                            <dt className="text-xs text-ink-faint">
                                                {t('journey.sources.credit')}
                                            </dt>
                                            <dd className="break-words">
                                                {p.terms_credit === '' ? '—' : p.terms_credit}
                                            </dd>
                                        </div>
                                        <div className="min-w-0">
                                            <dt className="text-xs text-ink-faint">
                                                {t('journey.sources.storage')}
                                            </dt>
                                            <dd>
                                                {p.allows_storage
                                                    ? t('journey.sources.storageAllowed')
                                                    : t('journey.sources.storageRefused')}
                                            </dd>
                                        </div>
                                    </dl>
                                    {way === 'each' && (
                                        <div className="flex flex-wrap gap-2">
                                            <Button
                                                variant={
                                                    decision === 'accepted'
                                                        ? 'primary'
                                                        : 'secondary'
                                                }
                                                onClick={() => draft(p, 'accepted')}
                                            >
                                                {t('journey.sources.accept')}
                                            </Button>
                                            <Button
                                                variant={
                                                    decision === 'declined'
                                                        ? 'primary'
                                                        : 'secondary'
                                                }
                                                onClick={() => draft(p, 'declined')}
                                            >
                                                {t('journey.sources.decline')}
                                            </Button>
                                        </div>
                                    )}
                                </div>
                            </details>
                        </li>
                    );
                })}
            </ul>
            {failure !== undefined && <Notice tone="error">{failure}</Notice>}
        </div>
    );
}
