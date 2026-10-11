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
import type { ReactNode } from 'react';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Tag } from '../ui/Primitives.js';

export const RUNNING_KEY = ['synthetic-running'] as const;

/** How often the screens ask what runs, since a feed can start or stop from anywhere. */
const RUNNING_EVERY_MS = 5_000;

/**
 * Which feeds run now, by source name: undefined until the server answers.
 * The server knows the running feeds by source name only.
 */
export function useRunningFeeds(): ReadonlySet<string> | undefined {
    const running = useQuery({
        queryKey: RUNNING_KEY,
        queryFn: api.runningFeeds,
        refetchInterval: RUNNING_EVERY_MS,
    });
    return running.data === undefined ? undefined : new Set(running.data);
}

/** Whether a feed runs, as a label; nothing while it is not yet known. */
export function FeedStatus({ running }: { readonly running: boolean | undefined }): ReactNode {
    const { t } = useTranslation();
    if (running === undefined) {
        return null;
    }
    return running ? (
        <Tag tone="up">{t('marketdata.runs.running')}</Tag>
    ) : (
        <Tag tone="muted">{t('marketdata.runs.stopped')}</Tag>
    );
}

/** A feed to start or stop, and the name the outcome is told by. */
export interface FeedToRun {
    readonly id: string;
    readonly label: string;
}

/** What a start or stop of many feeds did: how many it did, and each one the server refused, with why. */
export interface RunOutcome {
    readonly action: 'start' | 'stop';
    readonly done: number;
    readonly refused: readonly { readonly label: string; readonly message: string }[];
}

/**
 * Makes one call for each feed, all of them even when some are refused, and
 * says how many went through and why each refusal happened.
 */
export async function runFeeds(
    action: 'start' | 'stop',
    feeds: readonly FeedToRun[],
    call: (id: string) => Promise<void>,
): Promise<RunOutcome> {
    const settled = await Promise.allSettled(feeds.map((feed) => call(feed.id)));
    const refused = settled.flatMap((result, index) =>
        result.status === 'rejected'
            ? [
                  {
                      label: feeds[index]?.label ?? '',
                      message: (result.reason instanceof Error
                          ? result.reason.message
                          : String(result.reason)
                      ).replaceAll(feeds[index]?.id ?? '', feeds[index]?.label ?? ''),
                  },
              ]
            : [],
    );
    return { action, done: feeds.length - refused.length, refused };
}

/**
 * Starts or stops many feeds, each in its own call, so one refusal does not
 * stop the others. Every call is made before the answer is told, and the
 * running set is read again after.
 */
export function useFeedRuns(): {
    readonly run: (action: 'start' | 'stop', feeds: readonly FeedToRun[]) => void;
    readonly pending: boolean;
    readonly outcome: RunOutcome | undefined;
    readonly clear: () => void;
} {
    const queryClient = useQueryClient();
    const mutation = useMutation({
        mutationFn: async ({
            action,
            feeds,
        }: {
            readonly action: 'start' | 'stop';
            readonly feeds: readonly FeedToRun[];
        }): Promise<RunOutcome> => {
            return runFeeds(action, feeds, action === 'start' ? api.startFeed : api.stopFeed);
        },
        onSettled: () => queryClient.invalidateQueries({ queryKey: RUNNING_KEY }),
    });
    return {
        run: (action, feeds) => mutation.mutate({ action, feeds }),
        pending: mutation.isPending,
        outcome: mutation.data,
        clear: () => mutation.reset(),
    };
}

/** What a start or stop of many feeds did, in a sentence, and each refusal under it. */
export function RunOutcomeText({ outcome }: { readonly outcome: RunOutcome }): ReactNode {
    const { t } = useTranslation();
    return (
        <>
            <p>
                {t(
                    outcome.action === 'start'
                        ? 'marketdata.runs.startedCount'
                        : 'marketdata.runs.stoppedCount',
                    { count: String(outcome.done) },
                )}
            </p>
            {outcome.refused.length > 0 && (
                <ul className="mt-1 list-disc pl-5">
                    {outcome.refused.map((one) => (
                        <li key={one.label}>
                            {t('marketdata.runs.refused', {
                                feed: one.label,
                                message: one.message,
                            })}
                        </li>
                    ))}
                </ul>
            )}
        </>
    );
}
