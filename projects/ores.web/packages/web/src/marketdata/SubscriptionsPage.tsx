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
 * Manage the market data subscriptions and their routes, from
 * doc/knowledge/journeys/market_data/journey_manage-market-data-subscriptions.org.
 *
 * A subscription names a provider and its rights, and a route names the one
 * source that serves the party for that provider. The service states the party
 * of both from the session, so the screen decides which rows it may remove and
 * never asks a person for a party.
 */

import { useState, type ReactNode } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { api, type RouteRow, type SubscriptionRow } from '../api/client.js';
import { useHeld } from '../access/holds.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReasonFields, useReason } from '../refdata/shared.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Button, Dialog, Field, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { GapPanel, type ScreenGap } from '../operations/OperationsParts.js';
import { RelatedJourneys, type JourneyId } from '../operations/RelatedJourneys.js';
import {
    RIGHTS,
    heldBy,
    partyName,
    providerName,
    providersToRoute,
    providersToTake,
    rightsOf,
    routeSource,
    sourceName,
    sourcesForProvider,
    useCatalogue,
    usePartyNames,
    useRoutes,
    useSubscriptions,
    type Right,
} from './subscriptions.js';

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = ['986CED57-341F-49DE-8F5D-F186771F10D8'];

/** What the screen has open over it, if anything. */
type Open =
    | { readonly kind: 'add-subscription' }
    | { readonly kind: 'add-route' }
    | { readonly kind: 'remove-subscription'; readonly row: SubscriptionRow }
    | { readonly kind: 'remove-route'; readonly row: RouteRow };

/** The rights of one subscription, as the badges a person reads. */
function Rights({ subscription }: { readonly subscription: SubscriptionRow }): ReactNode {
    const { t } = useTranslation();
    const rights = rightsOf(subscription);
    if (rights.length === 0) {
        return <span className="text-ink-faint">—</span>;
    }
    return (
        <span className="flex flex-wrap gap-1">
            {rights.map((right) => (
                <Tag key={right} tone="accent">
                    {t(`marketdata.subscriptions.right.${right}`)}
                </Tag>
            ))}
        </span>
    );
}

/** One right as a checkbox, which the form and the standard both ask for. */
function RightField({
    right,
    held,
    onChange,
}: {
    readonly right: Right;
    readonly held: boolean;
    readonly onChange: (held: boolean) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <label className="flex items-start gap-2 text-sm">
            <input
                type="checkbox"
                className="mt-0.5"
                checked={held}
                onChange={(event) => onChange(event.target.checked)}
            />
            <span>
                {t(`marketdata.subscriptions.right.${right}`)}
                <span className="block text-xs text-ink-faint">
                    {t(`marketdata.subscriptions.right.${right}Means`)}
                </span>
            </span>
        </label>
    );
}

export function SubscriptionsPage({ partyId }: { readonly partyId: string }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const holds = useHeld();
    const [open, setOpen] = useState<Open | undefined>(undefined);
    const [provider, setProvider] = useState('');
    const [source, setSource] = useState('');
    const [rights, setRights] = useState<Record<Right, boolean>>({
        display: true,
        non_display: false,
        derived: false,
    });
    const [commentary, setCommentary] = useState('');
    const [failure, setFailure] = useState('');

    const subscriptions = useSubscriptions();
    const routes = useRoutes();
    const catalogue = useCatalogue();
    const names = usePartyNames();
    const reason = useReason('delete');

    const load = {
        dataUpdatedAt: Math.min(
            subscriptions.dataUpdatedAt,
            routes.dataUpdatedAt,
            catalogue.dataUpdatedAt,
        ),
        isFetching: subscriptions.isFetching || routes.isFetching || catalogue.isFetching,
    };
    const news = useEntityChanges(
        [
            { component: 'marketdata', entity: 'subscriptions' },
            { component: 'marketdata', entity: 'routes' },
        ],
        load,
    );

    /**
     * A write closes its dialog and reloads the rows the screen shows. The
     * service's change events tell other screens; this one reads again so the
     * person sees the row they just wrote.
     */
    const afterWrite = async (): Promise<void> => {
        setOpen(undefined);
        setFailure('');
        await queries.invalidateQueries();
    };

    const addSubscription = useMutation({
        mutationFn: () =>
            api.addSubscription({
                provider_code: provider,
                display: rights.display,
                non_display: rights.non_display,
                derived: rights.derived,
            }),
        onSuccess: afterWrite,
        onError: (error: Error) => setFailure(error.message),
    });
    const addRoute = useMutation({
        mutationFn: () => api.addRoute({ provider_code: provider, source_id: source }),
        onSuccess: afterWrite,
        onError: (error: Error) => setFailure(error.message),
    });
    const removeSubscription = useMutation({
        mutationFn: (row: SubscriptionRow) =>
            api.removeSubscription(row.provider_code, {
                version: row.version,
                reasonCode: reason.code,
                commentary,
            }),
        onSuccess: afterWrite,
        onError: (error: Error) => setFailure(error.message),
    });
    const removeRoute = useMutation({
        mutationFn: (row: RouteRow) =>
            api.removeRoute(row.provider_code, {
                version: row.version,
                reasonCode: reason.code,
                commentary,
            }),
        onSuccess: afterWrite,
        onError: (error: Error) => setFailure(error.message),
    });

    const trail = <AreaTrail area="marketdata" screen={t('marketdata.subscriptions.title')} />;
    if (subscriptions.isPending || routes.isPending || subscriptions.isError || routes.isError) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('marketdata.subscriptions.title')}
                description={t('marketdata.subscriptions.description')}
            >
                {subscriptions.isError ? (
                    <Notice tone="error">{subscriptions.error.message}</Notice>
                ) : routes.isError ? (
                    <Notice tone="error">{routes.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }

    const mayWriteSubscriptions = holds('marketdata::subscriptions:write');
    const mayRemoveSubscriptions = holds('marketdata::subscriptions:delete');
    const mayWriteRoutes = holds('marketdata::routes:write');
    const mayRemoveRoutes = holds('marketdata::routes:delete');
    const parties = names.data ?? new Map<string, string>();
    const takeable = providersToTake(catalogue.data, subscriptions.data, partyId);
    const routable = providersToRoute(subscriptions.data, routes.data, partyId);
    const gaps: readonly ScreenGap[] = [
        {
            title: t('marketdata.subscriptions.gap.suspend.title'),
            body: t('marketdata.subscriptions.gap.suspend.body'),
            journey: t('marketdata.journeys.subscriptions'),
        },
        {
            title: t('marketdata.subscriptions.gap.history.title'),
            body: t('marketdata.subscriptions.gap.history.body'),
            journey: t('marketdata.journeys.subscriptions'),
        },
        {
            title: t('marketdata.subscriptions.gap.usage.title'),
            body: t('marketdata.subscriptions.gap.usage.body'),
            journey: t('marketdata.journeys.subscriptions'),
        },
    ];

    const startAdd = (what: 'subscription' | 'route'): void => {
        setFailure('');
        setCommentary('');
        if (what === 'subscription') {
            setProvider(takeable[0]?.code ?? '');
            setRights({ display: true, non_display: false, derived: false });
            setOpen({ kind: 'add-subscription' });
            return;
        }
        setProvider(routable[0] ?? '');
        setSource(sourcesForProvider(catalogue.data, routable[0] ?? '')[0]?.id ?? '');
        setOpen({ kind: 'add-route' });
    };

    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('marketdata.subscriptions.title')}
                    description={t('marketdata.subscriptions.description')}
                    actions={
                        <div className="flex items-center gap-2">
                            {mayWriteRoutes && (
                                <Button
                                    icon="add"
                                    onClick={() => startAdd('route')}
                                    disabled={routable.length === 0}
                                >
                                    {t('marketdata.subscriptions.addRoute')}
                                </Button>
                            )}
                            {mayWriteSubscriptions && (
                                <Button
                                    variant="primary"
                                    icon="add"
                                    onClick={() => startAdd('subscription')}
                                    disabled={takeable.length === 0}
                                >
                                    {t('marketdata.subscriptions.add')}
                                </Button>
                            )}
                            <RefreshButton
                                onClick={() => {
                                    void subscriptions.refetch();
                                    void routes.refetch();
                                    void catalogue.refetch();
                                }}
                                load={load}
                                news={news}
                            />
                        </div>
                    }
                />
            </div>

            <section className="card space-y-4 p-6">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">
                        {t('marketdata.subscriptions.subscriptions.title')}
                    </h2>
                    <span className="text-xs text-ink-faint">
                        {t('marketdata.subscriptions.subscriptions.lead')}
                    </span>
                </header>
                {subscriptions.data.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.subscriptions.subscriptions.empty')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.subscriptions.columns.party')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.subscriptions.columns.provider')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.subscriptions.columns.rights')}
                                </th>
                                <th className="py-1" />
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-line-subtle">
                            {subscriptions.data.map((row) => (
                                <tr key={row.id}>
                                    <td className="py-2">{partyName(parties, row.party_id)}</td>
                                    <td className="py-2">
                                        {providerName(catalogue.data, row.provider_code)}
                                        <span className="ml-2 font-mono text-xs text-ink-faint">
                                            {row.provider_code}
                                        </span>
                                    </td>
                                    <td className="py-2">
                                        <Rights subscription={row} />
                                    </td>
                                    <td className="py-2 text-right">
                                        {mayRemoveSubscriptions && heldBy(row, partyId) && (
                                            <Button
                                                size="sm"
                                                icon="delete"
                                                onClick={() =>
                                                    setOpen({ kind: 'remove-subscription', row })
                                                }
                                            >
                                                {t('marketdata.subscriptions.remove')}
                                            </Button>
                                        )}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
                <p className="text-xs text-ink-faint">
                    {t('marketdata.subscriptions.subscriptions.note')}
                </p>
            </section>

            <section className="card space-y-4 p-6">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">
                        {t('marketdata.subscriptions.routes.title')}
                    </h2>
                    <span className="text-xs text-ink-faint">
                        {t('marketdata.subscriptions.routes.lead')}
                    </span>
                </header>
                {routes.data.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.subscriptions.routes.empty')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.subscriptions.columns.party')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.subscriptions.columns.provider')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.subscriptions.columns.source')}
                                </th>
                                <th className="py-1" />
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-line-subtle">
                            {routes.data.map((row) => {
                                const source = routeSource(catalogue.data, row);
                                return (
                                    <tr key={row.id}>
                                        <td className="py-2">{partyName(parties, row.party_id)}</td>
                                        <td className="py-2">
                                            {providerName(catalogue.data, row.provider_code)}
                                            <span className="ml-2 font-mono text-xs text-ink-faint">
                                                {row.provider_code}
                                            </span>
                                        </td>
                                        <td className="py-2">
                                            <span className="font-mono text-xs">
                                                {sourceName(catalogue.data, row)}
                                            </span>
                                            {source !== undefined && source.kind !== '' && (
                                                <span className="ml-2">
                                                    <Tag tone="neutral" small>
                                                        {source.kind}
                                                    </Tag>
                                                </span>
                                            )}
                                        </td>
                                        <td className="py-2 text-right">
                                            {mayRemoveRoutes && heldBy(row, partyId) && (
                                                <Button
                                                    size="sm"
                                                    icon="delete"
                                                    onClick={() =>
                                                        setOpen({ kind: 'remove-route', row })
                                                    }
                                                >
                                                    {t('marketdata.subscriptions.remove')}
                                                </Button>
                                            )}
                                        </td>
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                )}
                <p className="text-xs text-ink-faint">
                    {t('marketdata.subscriptions.routes.note')}
                </p>
            </section>

            <GapPanel gaps={gaps} />
            <RelatedJourneys ids={JOURNEYS} />

            {open?.kind === 'add-subscription' && (
                <Dialog
                    title={t('marketdata.subscriptions.addTitle')}
                    name="add-subscription"
                    onClose={() => setOpen(undefined)}
                    footer={
                        <>
                            <Button onClick={() => setOpen(undefined)}>
                                {t('marketdata.subscriptions.cancel')}
                            </Button>
                            <Button
                                variant="primary"
                                icon="add"
                                pending={addSubscription.isPending}
                                disabled={
                                    provider === '' ||
                                    (!rights.display && !rights.non_display && !rights.derived)
                                }
                                onClick={() => addSubscription.mutate()}
                            >
                                {t('marketdata.subscriptions.add')}
                            </Button>
                        </>
                    }
                >
                    <div className="space-y-4">
                        {failure !== '' && <Notice tone="error">{failure}</Notice>}
                        <Field label={t('marketdata.subscriptions.columns.provider')}>
                            <Select
                                value={provider}
                                onChange={(event) => setProvider(event.target.value)}
                            >
                                {takeable.map((choice) => (
                                    <option key={choice.code} value={choice.code}>
                                        {choice.name === '' ? choice.code : choice.name}
                                    </option>
                                ))}
                            </Select>
                        </Field>
                        <div className="space-y-2">
                            {RIGHTS.map((right) => (
                                <RightField
                                    key={right}
                                    right={right}
                                    held={rights[right]}
                                    onChange={(held) =>
                                        setRights((before) => ({ ...before, [right]: held }))
                                    }
                                />
                            ))}
                        </div>
                        <p className="text-xs text-ink-faint">
                            {t('marketdata.subscriptions.newRecord')}
                        </p>
                    </div>
                </Dialog>
            )}

            {open?.kind === 'add-route' && (
                <Dialog
                    title={t('marketdata.subscriptions.addRouteTitle')}
                    name="add-route"
                    onClose={() => setOpen(undefined)}
                    footer={
                        <>
                            <Button onClick={() => setOpen(undefined)}>
                                {t('marketdata.subscriptions.cancel')}
                            </Button>
                            <Button
                                variant="primary"
                                icon="add"
                                pending={addRoute.isPending}
                                disabled={provider === '' || source === ''}
                                onClick={() => addRoute.mutate()}
                            >
                                {t('marketdata.subscriptions.addRoute')}
                            </Button>
                        </>
                    }
                >
                    <div className="space-y-4">
                        {failure !== '' && <Notice tone="error">{failure}</Notice>}
                        <Field label={t('marketdata.subscriptions.columns.provider')}>
                            <Select
                                value={provider}
                                onChange={(event) => {
                                    const code = event.target.value;
                                    setProvider(code);
                                    setSource(
                                        sourcesForProvider(catalogue.data, code)[0]?.id ?? '',
                                    );
                                }}
                            >
                                {routable.map((code) => (
                                    <option key={code} value={code}>
                                        {providerName(catalogue.data, code)}
                                    </option>
                                ))}
                            </Select>
                        </Field>
                        <Field
                            label={t('marketdata.subscriptions.columns.source')}
                            hint={t('marketdata.subscriptions.sourceHint')}
                        >
                            <Select
                                value={source}
                                onChange={(event) => setSource(event.target.value)}
                            >
                                {sourcesForProvider(catalogue.data, provider).map((choice) => (
                                    <option key={choice.id} value={choice.id}>
                                        {choice.name}
                                    </option>
                                ))}
                            </Select>
                        </Field>
                        <p className="text-xs text-ink-faint">
                            {t('marketdata.subscriptions.newRecord')}
                        </p>
                    </div>
                </Dialog>
            )}

            {(open?.kind === 'remove-subscription' || open?.kind === 'remove-route') && (
                <Dialog
                    title={t('marketdata.subscriptions.removeTitle', {
                        provider: providerName(catalogue.data, open.row.provider_code),
                    })}
                    name="remove-subscription"
                    onClose={() => setOpen(undefined)}
                    footer={
                        <>
                            <Button onClick={() => setOpen(undefined)}>
                                {t('marketdata.subscriptions.cancel')}
                            </Button>
                            <Button
                                variant="danger"
                                icon="delete"
                                pending={removeSubscription.isPending || removeRoute.isPending}
                                disabled={
                                    reason.code === '' ||
                                    (reason.needsCommentary && commentary.trim() === '')
                                }
                                onClick={() => {
                                    if (open.kind === 'remove-subscription') {
                                        removeSubscription.mutate(open.row);
                                        return;
                                    }
                                    removeRoute.mutate(open.row);
                                }}
                            >
                                {t('marketdata.subscriptions.remove')}
                            </Button>
                        </>
                    }
                >
                    <div className="space-y-4">
                        {failure !== '' && <Notice tone="error">{failure}</Notice>}
                        <p className="text-sm text-ink-muted">
                            {open.kind === 'remove-subscription'
                                ? t('marketdata.subscriptions.removeSubscriptionWhat')
                                : t('marketdata.subscriptions.removeRouteWhat')}
                        </p>
                        <ReasonFields
                            reason={reason}
                            commentary={commentary}
                            onCommentary={setCommentary}
                            missing={reason.needsCommentary && commentary.trim() === ''}
                        />
                    </div>
                </Dialog>
            )}
        </div>
    );
}
