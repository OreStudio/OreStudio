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
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useHeld } from '../../access/holds.js';
import { PERMISSION } from '../../access/permissions.js';
import { api, type SimulatedSource } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { ReasonFields, useReason } from '../../refdata/shared.js';
import { Card } from '../../ui/Card.js';
import { Button, Dialog, Notice, Tag } from '../../ui/Primitives.js';

const SOURCES_KEY = ['simulated-sources'];
const EVERY_MS = 5000;

/** What stopping a source does, in the sentences the dialog says before it acts. */
export function stopEffects(
    source: SimulatedSource,
    t: (key: string, values?: Record<string, string | number>) => string,
): readonly string[] {
    const runOn = source.held_surfaces - source.surfaces_only_held_here;
    return [
        t('marketdata.synthetic.source.effect.feeds', { count: source.served_feeds }),
        ...(source.held_surfaces === 0
            ? []
            : [
                  t('marketdata.synthetic.source.effect.surfaces', {
                      count: source.held_surfaces,
                      stop: source.surfaces_only_held_here,
                      runOn,
                  }),
              ]),
        t('marketdata.synthetic.source.effect.byHand'),
        t('marketdata.synthetic.source.effect.stays'),
    ];
}

/**
 * The simulated sources of one provider, above its feeds: each one's state, what it runs and
 * holds, and one button to stop or start it. A stop asks first and says what it stops.
 */
export function SourceStrip({ provider }: { readonly provider: string }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const mayRun = useHeld()(PERMISSION.feedsRun);
    const sources = useQuery({
        queryKey: SOURCES_KEY,
        queryFn: () => api.simulatedSources(),
        refetchInterval: EVERY_MS,
    });
    const [stopping, setStopping] = useState<SimulatedSource | undefined>();
    const start = useMutation({
        mutationFn: (name: string) => api.startFeedNode(name),
        onSettled: () => queries.invalidateQueries({ queryKey: SOURCES_KEY }),
    });
    const mine = (sources.data ?? []).filter((source) => source.provider_code === provider);
    if (mine.length === 0) {
        return null;
    }
    return (
        <Card title={t('marketdata.synthetic.source.title')}>
            {start.error !== null && <Notice tone="error">{start.error.message}</Notice>}
            <ul className="space-y-2" data-source-strip>
                {mine.map((source) => (
                    <li
                        key={source.source_name}
                        className="flex flex-wrap items-center gap-3 text-sm"
                    >
                        <span className="font-mono">{source.source_name}</span>
                        {source.stopped ? (
                            <Tag tone="muted">{t('marketdata.synthetic.state.stopped')}</Tag>
                        ) : (
                            <Tag tone="up">{t('marketdata.synthetic.state.serving')}</Tag>
                        )}
                        {!source.stopped && (
                            <span className="text-ink-muted">
                                {t('marketdata.synthetic.source.holds', {
                                    feeds: source.served_feeds,
                                    surfaces: source.held_surfaces,
                                })}
                            </span>
                        )}
                        {mayRun && (
                            <span className="ml-auto">
                                {source.stopped ? (
                                    <Button
                                        size="sm"
                                        disabled={start.isPending}
                                        onClick={() => start.mutate(source.source_name)}
                                    >
                                        {t('marketdata.synthetic.source.start')}
                                    </Button>
                                ) : (
                                    <Button
                                        size="sm"
                                        variant="danger"
                                        onClick={() => setStopping(source)}
                                    >
                                        {t('marketdata.synthetic.source.stop')}
                                    </Button>
                                )}
                            </span>
                        )}
                    </li>
                ))}
            </ul>
            {stopping !== undefined && (
                <StopSourceDialog source={stopping} onClose={() => setStopping(undefined)} />
            )}
        </Card>
    );
}

/** The question a stop asks: what it stops, and why the person stops it. */
function StopSourceDialog({
    source,
    onClose,
}: {
    readonly source: SimulatedSource;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const reason = useReason('amend', true);
    const [commentary, setCommentary] = useState('');
    const missing = reason.needsCommentary && commentary.trim() === '';
    const stop = useMutation({
        mutationFn: () =>
            api.stopFeedNode(source.source_name, {
                reason_code: reason.code,
                commentary: commentary.trim(),
            }),
        onSuccess: () => {
            void queries.invalidateQueries({ queryKey: SOURCES_KEY });
            onClose();
        },
    });
    return (
        <Dialog
            title={t('marketdata.synthetic.source.stopTitle', { source: source.source_name })}
            name="stop-simulated-source"
            onClose={onClose}
            footer={
                <>
                    <Button onClick={onClose}>{t('refdata.records.cancel')}</Button>
                    <Button
                        variant="danger"
                        disabled={reason.code === '' || missing || stop.isPending}
                        onClick={() => stop.mutate()}
                    >
                        {t('marketdata.synthetic.source.stop')}
                    </Button>
                </>
            }
        >
            <ul className="list-disc space-y-1 pl-5 text-sm">
                {stopEffects(source, t).map((line) => (
                    <li key={line}>{line}</li>
                ))}
            </ul>
            <ReasonFields
                reason={reason}
                commentary={commentary}
                onCommentary={setCommentary}
                missing={missing}
            />
            {stop.error !== null && <Notice tone="error">{stop.error.message}</Notice>}
        </Dialog>
    );
}
