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

import { useMemo, useState, type ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
    api,
    type Connectivity,
    type ConnectivityAdapter,
    type ConnectivityFeed,
    type ConnectivityPublisher,
    type ConnectivitySession,
    type ConnectivitySupplier,
} from '../api/client.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import {
    attentionOf,
    classesOfFeed,
    classesOfPublisher,
    feedsOfPublisher,
    feedsOfSession,
    feedsOfSupplier,
    matches,
    publisherMatches,
    publishersOfSupplier,
    servicesOfSupplier,
    sessionsOfSupplier,
    supplierMatches,
    type Selected,
} from './connectivity.js';

function Field({ label, value }: { readonly label: string; readonly value: ReactNode }): ReactNode {
    return (
        <div className="space-y-0.5">
            <dt className="text-xs text-ink-faint">{label}</dt>
            <dd className="text-sm text-ink">{value}</dd>
        </div>
    );
}

function Counts({
    label,
    value,
    note,
}: {
    readonly label: string;
    readonly value: string | number;
    readonly note?: string;
}): ReactNode {
    return (
        <div className="card p-4">
            <p className="text-xs text-ink-faint">{label}</p>
            <p className="text-2xl font-medium tabular-nums">{value}</p>
            {note !== undefined && <p className="text-xs text-ink-faint">{note}</p>}
        </div>
    );
}

function Overview({
    catalogue,
    onAdapters,
}: {
    readonly catalogue: Connectivity;
    readonly onAdapters: () => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <div>
                    <h2 className="text-lg font-medium">
                        {t('marketdata.connectivity.overview.title')}
                    </h2>
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.connectivity.overview.lead')}
                    </p>
                </div>
                <button
                    type="button"
                    onClick={onAdapters}
                    className="text-sm text-accent hover:underline"
                >
                    {t('marketdata.connectivity.overview.adapters')}
                </button>
            </header>
            <div className="grid gap-3 sm:grid-cols-3 lg:grid-cols-5">
                <Counts
                    label={t('marketdata.connectivity.counts.publishers')}
                    value={catalogue.publishers.length}
                />
                <Counts
                    label={t('marketdata.connectivity.counts.suppliers')}
                    value={catalogue.suppliers.length}
                />
                <Counts
                    label={t('marketdata.connectivity.counts.sessions')}
                    value={catalogue.sessions.length}
                />
                <Counts
                    label={t('marketdata.connectivity.counts.feeds')}
                    value={catalogue.feeds.length}
                />
                <Counts
                    label={t('marketdata.connectivity.counts.adapters')}
                    value={catalogue.adapters.length}
                />
            </div>
            <p className="text-sm text-ink-muted">{t('marketdata.connectivity.overview.note')}</p>
        </div>
    );
}

function AdapterCatalogue({ catalogue }: { readonly catalogue: Connectivity }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-4">
            <header>
                <h2 className="text-lg font-medium">
                    {t('marketdata.connectivity.adapters.title')}
                </h2>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.connectivity.adapters.lead')}
                </p>
            </header>
            <div className="card overflow-x-auto p-4">
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">
                                {t('marketdata.connectivity.adapters.columns.adapter')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.connectivity.adapters.columns.version')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.connectivity.adapters.columns.classes')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.connectivity.adapters.columns.services')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.connectivity.adapters.columns.schema')}
                            </th>
                            <th className="py-1 text-right font-normal">
                                {t('marketdata.connectivity.adapters.columns.sessions')}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {catalogue.adapters.map((adapter: ConnectivityAdapter) => {
                            const sessions = catalogue.sessions.filter(
                                (session) => session.adapter_code === adapter.code,
                            );
                            return (
                                <tr key={adapter.code} className="border-t border-line-subtle">
                                    <td className="py-2 align-top">
                                        <p className="font-medium">{adapter.name}</p>
                                        <p className="font-mono text-xs text-ink-faint">
                                            {adapter.code} · {adapter.process}
                                        </p>
                                        <p className="max-w-md text-xs text-ink-faint">
                                            {adapter.description}
                                        </p>
                                    </td>
                                    <td className="py-2 align-top font-mono text-xs">
                                        {adapter.module_version || '—'}
                                    </td>
                                    <td className="py-2 align-top">
                                        <div className="flex flex-wrap gap-1">
                                            {adapter.asset_classes.map((code) => (
                                                <Tag key={code} tone="neutral" small>
                                                    {code}
                                                </Tag>
                                            ))}
                                        </div>
                                    </td>
                                    <td className="py-2 align-top text-xs">
                                        {adapter.services.join(', ') || '—'}
                                    </td>
                                    <td className="py-2 align-top text-xs">
                                        {adapter.schema.length === 0
                                            ? t('marketdata.connectivity.adapters.nothing')
                                            : adapter.schema.map((field) => field.name).join(', ')}
                                    </td>
                                    <td className="py-2 text-right align-top tabular-nums">
                                        {sessions.length}
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            </div>
            <p className="text-xs text-ink-faint">{t('marketdata.connectivity.adapters.note')}</p>
        </div>
    );
}

function SupplierDashboard({
    catalogue,
    supplier,
}: {
    readonly catalogue: Connectivity;
    readonly supplier: ConnectivitySupplier;
}): ReactNode {
    const { t } = useTranslation();
    const sessions = sessionsOfSupplier(catalogue, supplier.code);
    const feeds = feedsOfSupplier(catalogue, supplier.code);
    const publishers = publishersOfSupplier(catalogue, supplier.code);
    const services = servicesOfSupplier(catalogue, supplier.code);
    const attention = attentionOf(catalogue, supplier.code);
    return (
        <div className="space-y-4">
            <header>
                <h2 className="text-lg font-medium">{supplier.name}</h2>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.connectivity.supplier.meta', {
                        kind: supplier.kind,
                        sessions: String(sessions.length),
                        feeds: String(feeds.length),
                    })}
                </p>
            </header>
            <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
                <Counts
                    label={t('marketdata.connectivity.supplier.kpi.sessions')}
                    value={sessions.length}
                    note={t('marketdata.connectivity.supplier.kpi.serving', {
                        count: String(sessions.filter((session) => session.enabled).length),
                    })}
                />
                <Counts
                    label={t('marketdata.connectivity.supplier.kpi.feeds')}
                    value={feeds.length}
                />
                <Counts
                    label={t('marketdata.connectivity.supplier.kpi.publishers')}
                    value={publishers.length}
                />
                <Counts
                    label={t('marketdata.connectivity.supplier.kpi.attention')}
                    value={attention.length}
                />
            </div>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.supplier.sessions')}
                </h3>
                {sessions.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.connectivity.supplier.noSessions')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.session')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.environment')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.adapter')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.state')}
                                </th>
                                <th className="py-1 text-right font-normal">
                                    {t('marketdata.connectivity.columns.feeds')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {sessions.map((session) => (
                                <tr key={session.code} className="border-t border-line-subtle">
                                    <td className="py-1 font-mono text-xs">{session.code}</td>
                                    <td className="py-1 font-mono text-xs">
                                        {session.environment}
                                    </td>
                                    <td className="py-1 font-mono text-xs">
                                        {session.adapter_code}
                                    </td>
                                    <td className="py-1">
                                        <Tag tone={session.enabled ? 'up' : 'warn'} small>
                                            {session.enabled
                                                ? t('marketdata.connectivity.state.enabled')
                                                : t('marketdata.connectivity.state.disabled')}
                                        </Tag>
                                    </td>
                                    <td className="py-1 text-right tabular-nums">
                                        {feedsOfSession(catalogue, session.code).length}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.supplier.coverage')}
                </h3>
                {publishers.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.connectivity.supplier.noPublishers')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.publisher')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.classes')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.services')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {publishers.map((publisher) => (
                                <tr key={publisher.code} className="border-t border-line-subtle">
                                    <td className="py-1">{publisher.name}</td>
                                    <td className="py-1 font-mono text-xs">
                                        {publisher.classes.join(', ') || '—'}
                                    </td>
                                    <td className="py-1 text-xs">{services.join(', ') || '—'}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.supplier.attention')}
                </h3>
                {attention.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.connectivity.supplier.allClear')}
                    </p>
                ) : (
                    <ul className="space-y-1 text-sm">
                        {attention.map((entry) => (
                            <li key={`${entry.kind}-${entry.label}`} className="flex gap-2">
                                <Tag tone="warn" small>
                                    {t(`marketdata.connectivity.reason.${entry.reason}`)}
                                </Tag>
                                <span className="font-mono text-xs">{entry.label}</span>
                            </li>
                        ))}
                    </ul>
                )}
            </div>
            <div className="card space-y-2 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.supplier.contract')}
                </h3>
                <dl className="grid gap-3 sm:grid-cols-2">
                    <Field
                        label={t('marketdata.connectivity.columns.kind')}
                        value={supplier.kind || '—'}
                    />
                    <Field
                        label={t('marketdata.connectivity.supplier.contractReference')}
                        value={supplier.contract_reference || '—'}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.support')}
                        value={supplier.support || '—'}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.description')}
                        value={supplier.description || '—'}
                    />
                </dl>
            </div>
        </div>
    );
}

function PublisherView({
    catalogue,
    publisher,
}: {
    readonly catalogue: Connectivity;
    readonly publisher: ConnectivityPublisher;
}): ReactNode {
    const { t } = useTranslation();
    const feeds = feedsOfPublisher(catalogue, publisher.code);
    return (
        <div className="space-y-4">
            <header>
                <h2 className="text-lg font-medium">{publisher.name}</h2>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.connectivity.publisher.meta', {
                        kind: publisher.kind,
                        classes: classesOfPublisher(catalogue, publisher.code).join(', ') || '—',
                        feeds: String(feeds.length),
                    })}
                </p>
            </header>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.publisher.details')}
                </h3>
                <dl className="grid gap-3 sm:grid-cols-2">
                    <Field
                        label={t('marketdata.connectivity.columns.code')}
                        value={publisher.code}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.terms')}
                        value={
                            publisher.terms_accepted
                                ? t('marketdata.connectivity.publisher.termsAccepted', {
                                      who: publisher.terms_accepted_by,
                                      when: (publisher.terms_accepted_at ?? '').slice(0, 10),
                                  })
                                : t('marketdata.connectivity.publisher.termsNotAccepted')
                        }
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.classes')}
                        value={classesOfPublisher(catalogue, publisher.code).join(', ') || '—'}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.description')}
                        value={publisher.description || '—'}
                    />
                </dl>
                {publisher.terms_url !== '' && (
                    <a
                        href={publisher.terms_url}
                        target="_blank"
                        rel="noreferrer"
                        className="text-xs text-accent hover:underline"
                    >
                        {t('marketdata.connectivity.publisher.readTerms')}
                    </a>
                )}
            </div>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.publisher.feeds')}
                </h3>
                {feeds.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.connectivity.publisher.noFeeds')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.feed')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.session')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.service')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.classes')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.default')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {feeds.map((feed) => (
                                <tr key={feed.id} className="border-t border-line-subtle">
                                    <td className="py-1 font-mono text-xs">{feed.name}</td>
                                    <td className="py-1 font-mono text-xs">{feed.session_code}</td>
                                    <td className="py-1 text-xs">{feed.service}</td>
                                    <td className="py-1 font-mono text-xs">
                                        {classesOfFeed(feed).join(', ') || '—'}
                                    </td>
                                    <td className="py-1 text-xs">
                                        {feed.is_default ? t('common.yes') : t('common.no')}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
        </div>
    );
}

function SessionView({
    catalogue,
    session,
}: {
    readonly catalogue: Connectivity;
    readonly session: ConnectivitySession;
}): ReactNode {
    const { t } = useTranslation();
    const feeds = feedsOfSession(catalogue, session.code);
    const settings = Object.entries(session.settings);
    return (
        <div className="space-y-4">
            <header>
                <h2 className="text-lg font-medium">{session.name}</h2>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.connectivity.session.meta', {
                        supplier: session.supplier_code,
                        adapter: session.adapter_code,
                        environment: session.environment,
                    })}
                </p>
            </header>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.session.details')}
                </h3>
                <dl className="grid gap-3 sm:grid-cols-2">
                    <Field label={t('marketdata.connectivity.columns.code')} value={session.code} />
                    <Field
                        label={t('marketdata.connectivity.columns.supplier')}
                        value={session.supplier_code}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.adapter')}
                        value={session.adapter_code}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.environment')}
                        value={session.environment}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.endpoint')}
                        value={<span className="font-mono text-xs">{session.endpoint || '—'}</span>}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.credential')}
                        value={
                            <span className="font-mono text-xs">
                                {session.credential_ref || '—'}
                            </span>
                        }
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.state')}
                        value={
                            <Tag tone={session.enabled ? 'up' : 'warn'} small>
                                {session.enabled
                                    ? t('marketdata.connectivity.state.enabled')
                                    : t('marketdata.connectivity.state.disabled')}
                            </Tag>
                        }
                    />
                </dl>
            </div>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.session.settings')}
                </h3>
                {settings.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.connectivity.session.noSettings')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <tbody>
                            {settings.map(([key, value]) => (
                                <tr key={key} className="border-t border-line-subtle">
                                    <td className="py-1 font-mono text-xs">{key}</td>
                                    <td className="py-1 font-mono text-xs">
                                        {typeof value === 'string' ? value : JSON.stringify(value)}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">
                    {t('marketdata.connectivity.session.feeds')}
                </h3>
                {feeds.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.connectivity.session.noFeeds')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.feed')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.publisher')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.service')}
                                </th>
                                <th className="py-1 text-right font-normal">
                                    {t('marketdata.connectivity.columns.scope')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {feeds.map((feed) => (
                                <tr key={feed.id} className="border-t border-line-subtle">
                                    <td className="py-1 font-mono text-xs">{feed.name}</td>
                                    <td className="py-1 font-mono text-xs">
                                        {feed.publisher_code}
                                    </td>
                                    <td className="py-1 text-xs">{feed.service}</td>
                                    <td className="py-1 text-right tabular-nums">
                                        {feed.scopes.length}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
        </div>
    );
}

function FeedView({ feed }: { readonly feed: ConnectivityFeed }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-4">
            <header>
                <h2 className="text-lg font-medium">{feed.name}</h2>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.connectivity.feed.meta', {
                        publisher: feed.publisher_code,
                        session: feed.session_code,
                        service: feed.service,
                    })}
                </p>
            </header>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">{t('marketdata.connectivity.feed.details')}</h3>
                <dl className="grid gap-3 sm:grid-cols-2">
                    <Field
                        label={t('marketdata.connectivity.columns.publisher')}
                        value={feed.publisher_code}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.session')}
                        value={feed.session_code}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.service')}
                        value={feed.service}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.providerKey')}
                        value={feed.provider_key}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.default')}
                        value={feed.is_default ? t('common.yes') : t('common.no')}
                    />
                    <Field
                        label={t('marketdata.connectivity.columns.classes')}
                        value={classesOfFeed(feed).join(', ') || '—'}
                    />
                </dl>
                <p className="text-xs text-ink-faint">{t('marketdata.connectivity.feed.note')}</p>
            </div>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">{t('marketdata.connectivity.feed.scope')}</h3>
                {feed.scopes.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.connectivity.feed.noScope')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.item')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.class')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {feed.scopes.map((scope) => (
                                <tr key={scope.item} className="border-t border-line-subtle">
                                    <td className="py-1 font-mono text-xs">{scope.item}</td>
                                    <td className="py-1 font-mono text-xs">{scope.asset_class}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
            <div className="card space-y-3 p-4">
                <h3 className="text-sm font-medium">{t('marketdata.connectivity.feed.routes')}</h3>
                {feed.routes.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.connectivity.feed.noRoutes')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.party')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.publisher')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.connectivity.columns.class')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {feed.routes.map((route) => (
                                <tr
                                    key={`${route.party_id}-${route.asset_class}`}
                                    className="border-t border-line-subtle"
                                >
                                    <td className="py-1 font-mono text-xs">{route.party_id}</td>
                                    <td className="py-1 font-mono text-xs">
                                        {route.publisher_code}
                                    </td>
                                    <td className="py-1 font-mono text-xs">{route.asset_class}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
        </div>
    );
}

/**
 * Connectivity: who publishes, who supplies, the adapter modules the release
 * holds, the sessions configured from them, and the feeds that bind a
 * publisher to a session and a service.
 */
export function ConnectivityPage(): ReactNode {
    const { t } = useTranslation();
    const catalogue = useQuery({ queryKey: ['connectivity'], queryFn: api.connectivity });
    const [selected, setSelected] = useState<Selected>({ kind: 'overview' });
    const [search, setSearch] = useState('');
    const load = { dataUpdatedAt: catalogue.dataUpdatedAt, isFetching: catalogue.isFetching };
    const news = useEntityChanges(
        [
            { component: 'marketdata', entity: 'providers' },
            { component: 'marketdata', entity: 'suppliers' },
            { component: 'marketdata', entity: 'sessions' },
            { component: 'marketdata', entity: 'feeds' },
        ],
        load,
    );
    const trail = <AreaTrail area="marketdata" screen={t('marketdata.connectivity.title')} />;
    const suppliers = useMemo(
        () =>
            (catalogue.data?.suppliers ?? []).filter((supplier) =>
                catalogue.data === undefined
                    ? true
                    : supplierMatches(catalogue.data, supplier, search),
            ),
        [catalogue.data, search],
    );
    const publishers = useMemo(
        () =>
            (catalogue.data?.publishers ?? []).filter((publisher) =>
                catalogue.data === undefined
                    ? true
                    : publisherMatches(catalogue.data, publisher, search),
            ),
        [catalogue.data, search],
    );
    if (catalogue.data === undefined) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('marketdata.connectivity.title')}
                description={t('marketdata.connectivity.lead')}
            >
                {catalogue.isError ? (
                    <Notice tone="error">{catalogue.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }
    const data = catalogue.data;
    const session =
        selected.kind === 'session'
            ? data.sessions.find((s) => s.code === selected.code)
            : undefined;
    const supplier =
        selected.kind === 'supplier'
            ? data.suppliers.find((s) => s.code === selected.code)
            : undefined;
    const publisher =
        selected.kind === 'publisher'
            ? data.publishers.find((p) => p.code === selected.code)
            : undefined;
    const feed =
        selected.kind === 'feed' ? data.feeds.find((f) => f.id === selected.id) : undefined;
    const isSelected = (kind: Selected['kind'], id: string): boolean =>
        selected.kind === kind &&
        ((selected.kind === 'feed' && selected.id === id) ||
            (selected.kind !== 'feed' && 'code' in selected && selected.code === id));
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('marketdata.connectivity.title')}
                    description={t('marketdata.connectivity.lead')}
                    actions={
                        <RefreshButton
                            onClick={() => void catalogue.refetch()}
                            load={load}
                            news={news}
                        />
                    }
                />
            </div>
            <div className="grid gap-6 lg:grid-cols-[330px_minmax(0,1fr)]">
                <aside className="card space-y-4 p-4 lg:sticky lg:top-4 lg:self-start">
                    <input
                        type="search"
                        aria-label={t('marketdata.connectivity.search')}
                        placeholder={t('marketdata.connectivity.search')}
                        value={search}
                        onChange={(e) => setSearch(e.target.value)}
                        className="w-full rounded-control border border-line bg-surface px-2 py-1 text-sm"
                    />
                    <div className="space-y-2">
                        <h2 className="text-xs uppercase tracking-wide text-ink-faint">
                            {t('marketdata.connectivity.delivery')}
                        </h2>
                        {suppliers.length === 0 ? (
                            <p className="text-xs text-ink-faint">
                                {t('marketdata.connectivity.noMatch')}
                            </p>
                        ) : (
                            <ul className="space-y-2 text-sm">
                                {suppliers.map((entry) => {
                                    const entrySessions = sessionsOfSupplier(data, entry.code);
                                    const entryFeeds = feedsOfSupplier(data, entry.code);
                                    return (
                                        <li key={entry.code}>
                                            <button
                                                type="button"
                                                onClick={() =>
                                                    setSelected({
                                                        kind: 'supplier',
                                                        code: entry.code,
                                                    })
                                                }
                                                className={
                                                    isSelected('supplier', entry.code)
                                                        ? 'w-full text-left font-medium text-accent'
                                                        : 'w-full text-left font-medium hover:text-accent'
                                                }
                                            >
                                                {entry.name}
                                            </button>
                                            <p className="text-xs text-ink-faint">
                                                {t('marketdata.connectivity.supplier.line', {
                                                    kind: entry.kind,
                                                    sessions: String(entrySessions.length),
                                                    feeds: String(entryFeeds.length),
                                                })}
                                            </p>
                                            <ul className="ml-3 space-y-1 border-l border-line-subtle pl-2">
                                                {entrySessions.length === 0 ? (
                                                    <li className="text-xs text-ink-faint">
                                                        {t('marketdata.connectivity.noSession')}
                                                    </li>
                                                ) : (
                                                    entrySessions.map((sessionRow) => (
                                                        <li key={sessionRow.code}>
                                                            <button
                                                                type="button"
                                                                onClick={() =>
                                                                    setSelected({
                                                                        kind: 'session',
                                                                        code: sessionRow.code,
                                                                    })
                                                                }
                                                                className={
                                                                    isSelected(
                                                                        'session',
                                                                        sessionRow.code,
                                                                    )
                                                                        ? 'w-full text-left text-xs text-accent'
                                                                        : 'w-full text-left text-xs hover:text-accent'
                                                                }
                                                            >
                                                                {sessionRow.code}
                                                            </button>
                                                            <p className="text-xs text-ink-faint">
                                                                {t(
                                                                    'marketdata.connectivity.session.line',
                                                                    {
                                                                        adapter:
                                                                            sessionRow.adapter_code,
                                                                        environment:
                                                                            sessionRow.environment,
                                                                    },
                                                                )}
                                                            </p>
                                                            <ul className="ml-2 space-y-0.5">
                                                                {feedsOfSession(
                                                                    data,
                                                                    sessionRow.code,
                                                                ).map((feedRow) => (
                                                                    <li key={feedRow.id}>
                                                                        <button
                                                                            type="button"
                                                                            onClick={() =>
                                                                                setSelected({
                                                                                    kind: 'feed',
                                                                                    id: feedRow.id,
                                                                                })
                                                                            }
                                                                            className={
                                                                                isSelected(
                                                                                    'feed',
                                                                                    feedRow.id,
                                                                                )
                                                                                    ? 'w-full text-left text-xs text-accent'
                                                                                    : 'w-full text-left text-xs text-ink-muted hover:text-accent'
                                                                            }
                                                                        >
                                                                            {feedRow.name}
                                                                        </button>
                                                                    </li>
                                                                ))}
                                                            </ul>
                                                        </li>
                                                    ))
                                                )}
                                            </ul>
                                        </li>
                                    );
                                })}
                            </ul>
                        )}
                    </div>
                    <div className="space-y-2">
                        <h2 className="text-xs uppercase tracking-wide text-ink-faint">
                            {t('marketdata.connectivity.publishers')}
                        </h2>
                        <ul className="space-y-2 text-sm">
                            {publishers.map((entry) => (
                                <li key={entry.code}>
                                    <button
                                        type="button"
                                        onClick={() =>
                                            setSelected({ kind: 'publisher', code: entry.code })
                                        }
                                        className={
                                            isSelected('publisher', entry.code)
                                                ? 'w-full text-left font-medium text-accent'
                                                : 'w-full text-left font-medium hover:text-accent'
                                        }
                                    >
                                        {entry.name}
                                    </button>
                                    <p className="text-xs text-ink-faint">
                                        {t('marketdata.connectivity.publisher.line', {
                                            classes:
                                                classesOfPublisher(data, entry.code).join(', ') ||
                                                '—',
                                            feeds: String(
                                                feedsOfPublisher(data, entry.code).length,
                                            ),
                                        })}
                                    </p>
                                    <ul className="ml-3 space-y-0.5 border-l border-line-subtle pl-2">
                                        {feedsOfPublisher(data, entry.code).length === 0 ? (
                                            <li className="text-xs text-ink-faint">
                                                {t('marketdata.connectivity.noFeed')}
                                            </li>
                                        ) : (
                                            feedsOfPublisher(data, entry.code).map((feedRow) => (
                                                <li key={feedRow.id}>
                                                    <button
                                                        type="button"
                                                        onClick={() =>
                                                            setSelected({
                                                                kind: 'feed',
                                                                id: feedRow.id,
                                                            })
                                                        }
                                                        className={
                                                            isSelected('feed', feedRow.id)
                                                                ? 'w-full text-left text-xs text-accent'
                                                                : 'w-full text-left text-xs text-ink-muted hover:text-accent'
                                                        }
                                                    >
                                                        {feedRow.name}
                                                    </button>
                                                </li>
                                            ))
                                        )}
                                    </ul>
                                </li>
                            ))}
                        </ul>
                    </div>
                    <button
                        type="button"
                        onClick={() => setSelected({ kind: 'adapters' })}
                        className="text-sm text-accent hover:underline"
                    >
                        {t('marketdata.connectivity.adaptersLink')}
                    </button>
                </aside>
                <section className="min-w-0 space-y-4">
                    {selected.kind === 'overview' && (
                        <Overview
                            catalogue={data}
                            onAdapters={() => setSelected({ kind: 'adapters' })}
                        />
                    )}
                    {selected.kind === 'adapters' && <AdapterCatalogue catalogue={data} />}
                    {supplier !== undefined && (
                        <SupplierDashboard catalogue={data} supplier={supplier} />
                    )}
                    {publisher !== undefined && (
                        <PublisherView catalogue={data} publisher={publisher} />
                    )}
                    {session !== undefined && <SessionView catalogue={data} session={session} />}
                    {feed !== undefined && <FeedView feed={feed} />}
                    {selected.kind !== 'overview' &&
                        selected.kind !== 'adapters' &&
                        supplier === undefined &&
                        publisher === undefined &&
                        session === undefined &&
                        feed === undefined && (
                            <p className="text-sm text-ink-muted">
                                {t('marketdata.connectivity.missing')}
                            </p>
                        )}
                </section>
            </div>
        </div>
    );
}
