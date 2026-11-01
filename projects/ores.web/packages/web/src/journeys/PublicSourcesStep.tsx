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

/**
 * The decision a publisher stands at: the one made in this run, else accepted
 * when its terms already are, else none.
 */
export function decisionOf(
    provider: PublicRatesProvider,
    made: Readonly<Record<string, SourceDecision>>,
): SourceDecision | undefined {
    return made[provider.code] ?? (provider.terms_accepted ? 'accepted' : undefined);
}

/** Every publisher that states terms has a decision. */
export function everyDecided(
    providers: readonly PublicRatesProvider[],
    made: Readonly<Record<string, SourceDecision>>,
): boolean {
    return providers.every((p) => p.terms_url === '' || decisionOf(p, made) !== undefined);
}

/**
 * The public sources step of the first run. The administrator reads each
 * publisher's terms and accepts or declines it before any of its quotes are
 * fetched. An acceptance is recorded by the server against the terms it holds;
 * a decline withdraws any acceptance, so the publisher is not fetched.
 */
export function PublicSourcesStep({
    server,
    onDecided,
}: {
    readonly server: Pick<
        JourneyServer,
        'publicSources' | 'acceptSourceTerms' | 'declineSourceTerms'
    >;
    /** Tells the journey whether every publisher has a decision. */
    readonly onDecided: (all: boolean) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [providers, setProviders] = useState<readonly PublicRatesProvider[]>();
    const [failure, setFailure] = useState<string>();
    const [pending, setPending] = useState(false);
    const [made, setMade] = useState<Readonly<Record<string, SourceDecision>>>({});
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
    const decide = async (provider: PublicRatesProvider, decision: SourceDecision) => {
        setPending(true);
        setFailure(undefined);
        try {
            if (decision === 'accepted') {
                await server.acceptSourceTerms(provider.code, provider.version);
            } else {
                await server.declineSourceTerms(provider.code);
            }
            setMade((before) => ({ ...before, [provider.code]: decision }));
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setPending(false);
        }
    };
    const list = (providers ?? []).filter((p) => p.terms_url !== '');
    const all = providers !== undefined && everyDecided(list, made);
    useEffect(() => onDecided(all), [all, onDecided]);

    if (providers === undefined && failure !== undefined) {
        return <Notice tone="error">{failure}</Notice>;
    }
    if (providers === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (list.length === 0) {
        return <p className="text-sm text-ink-muted">{t('journey.sources.none')}</p>;
    }
    return (
        <div className="space-y-3">
            {list.map((p) => {
                const decision = decisionOf(p, made);
                return (
                    <section key={p.code} className="card space-y-2 p-4" aria-label={p.name}>
                        <header className="flex flex-wrap items-baseline justify-between gap-2">
                            <h3 className="font-medium">{p.name}</h3>
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
                        </header>
                        <p className="text-sm text-ink-muted">
                            {p.description !== ''
                                ? p.description
                                : t('journey.sources.gives', { count: String(p.series.length) })}
                        </p>
                        <dl className="grid gap-1 text-sm sm:grid-cols-3">
                            <div>
                                <dt className="text-xs text-ink-faint">
                                    {t('journey.sources.terms')}
                                </dt>
                                <dd>
                                    <a
                                        className="text-accent hover:underline"
                                        href={p.terms_url}
                                        target="_blank"
                                        rel="noreferrer"
                                    >
                                        {p.terms_url}
                                    </a>
                                </dd>
                            </div>
                            <div>
                                <dt className="text-xs text-ink-faint">
                                    {t('journey.sources.credit')}
                                </dt>
                                <dd>{p.terms_credit === '' ? '—' : p.terms_credit}</dd>
                            </div>
                            <div>
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
                        <div className="flex flex-wrap gap-2">
                            <Button
                                variant={decision === 'accepted' ? 'primary' : 'secondary'}
                                disabled={pending}
                                onClick={() => void decide(p, 'accepted')}
                            >
                                {t('journey.sources.accept')}
                            </Button>
                            <Button disabled={pending} onClick={() => void decide(p, 'declined')}>
                                {t('journey.sources.decline')}
                            </Button>
                        </div>
                    </section>
                );
            })}
            {failure !== undefined && <Notice tone="error">{failure}</Notice>}
        </div>
    );
}
