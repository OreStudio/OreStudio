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
import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { subjects as providerSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/provider_protocol';
import { subjects as supplierSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/supplier_protocol';
import { subjects as adapterSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/adapter_protocol';
import { subjects as sessionSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/session_protocol';
import { subjects as feedSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/feed_protocol';
import { subjects as scopeSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/feed_scope_protocol';
import { subjects as routeSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/route_protocol';
import { readAll } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The delivery side of market data: who publishes, who supplies, the adapter
 * modules the release holds, the sessions configured from them, and the feeds
 * that bind a publisher to a session and a service.
 *
 * The screen is a tree, so one read answers it whole: the catalogue is small,
 * and a screen that fetched it in pieces would draw a tree whose halves arrive
 * at different times.
 */

const publisherSchema = z.looseObject({
    code: z.string(),
    name: z.string(),
    kind: z.string().default(''),
    description: z.string().default(''),
    terms_url: z.string().default(''),
    terms_credit: z.string().default(''),
    terms_accepted_digest: z.string().default(''),
    terms_accepted_by: z.string().default(''),
    terms_accepted_at: z.string().nullable().default(null),
});

const supplierSchema = z.looseObject({
    code: z.string(),
    name: z.string(),
    kind: z.string().default(''),
    description: z.string().default(''),
    contract_reference: z.string().default(''),
    support: z.string().default(''),
});

const adapterSchema = z.looseObject({
    code: z.string(),
    name: z.string(),
    module_version: z.string().default(''),
    description: z.string().default(''),
    process: z.string().default(''),
    asset_classes: z.string().default(''),
    services: z.string().default(''),
    configuration_schema: z.string().default('{}'),
});

const sessionSchema = z.looseObject({
    code: z.string(),
    name: z.string(),
    supplier_code: z.string(),
    adapter_code: z.string(),
    environment: z.string().default(''),
    endpoint: z.string().default(''),
    credential_ref: z.string().default(''),
    settings: z.string().default('{}'),
    enabled: z.boolean().default(false),
    description: z.string().default(''),
});

const feedSchema = z.looseObject({
    id: z.string(),
    name: z.string(),
    publisher_code: z.string(),
    session_code: z.string(),
    service: z.string().default(''),
    provider_key: z.string().default(''),
    is_default: z.boolean().default(false),
    description: z.string().default(''),
});

const scopeSchema = z.looseObject({
    feed_id: z.string(),
    asset_class: z.string(),
    item: z.string(),
    vendor_instrument_id: z.string().nullable().default(null),
});

const routeSchema = z.looseObject({
    party_id: z.string(),
    publisher_code: z.string(),
    asset_class: z.string(),
    feed_id: z.string(),
});

/** A comma-separated catalogue cell as the list it states. */
function words(text: string): readonly string[] {
    return text
        .split(',')
        .map((word) => word.trim())
        .filter((word) => word !== '');
}

/**
 * A JSON document the catalogue stores as text. A row that does not parse is
 * drawn as an empty document: the screen shows a session with no settings
 * rather than failing the tree for every other record.
 */
function document(text: string): Readonly<Record<string, unknown>> {
    try {
        const parsed: unknown = JSON.parse(text);
        return typeof parsed === 'object' && parsed !== null && !Array.isArray(parsed)
            ? (parsed as Readonly<Record<string, unknown>>)
            : {};
    } catch {
        return {};
    }
}

/** The fields an adapter's configuration schema names, in the order it states them. */
function schemaFields(text: string): readonly {
    readonly name: string;
    readonly type: string;
    readonly required: boolean;
    readonly default: string;
}[] {
    return Object.entries(document(text)).map(([name, value]) => {
        const field = typeof value === 'object' && value !== null ? value : {};
        const type = 'type' in field && typeof field.type === 'string' ? field.type : 'string';
        const required = 'required' in field && field.required === true;
        const fallback = 'default' in field ? field.default : undefined;
        return {
            name,
            type,
            required,
            default:
                fallback === undefined || fallback === null
                    ? ''
                    : typeof fallback === 'string'
                      ? fallback
                      : JSON.stringify(fallback),
        };
    });
}

export function registerConnectivityRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** Every catalogue the Connectivity screen draws. */
    server.get('/api/marketdata/connectivity', async (request) => {
        const session = requireSession(request);
        const empty = { filter: null, as_of: null };
        const [publishers, suppliers, adapters, sessions, feeds, scopes, routes] =
            await Promise.all([
                readAll(session, providerSubjects.list_providers_request, 'providers', empty),
                readAll(session, supplierSubjects.list_suppliers_request, 'suppliers', empty),
                readAll(session, adapterSubjects.list_adapters_request, 'adapters', empty),
                readAll(session, sessionSubjects.list_sessions_request, 'sessions', empty),
                readAll(session, feedSubjects.list_feeds_request, 'feeds', empty),
                readAll(session, scopeSubjects.list_feed_scopes_request, 'scopes', empty),
                readAll(session, routeSubjects.list_routes_request, 'routes', empty),
            ]);
        const parsedScopes = scopes.map((row) => scopeSchema.parse(row));
        const parsedRoutes = routes.map((row) => routeSchema.parse(row));
        return {
            publishers: publishers
                .map((row) => publisherSchema.parse(row))
                .map((publisher) => ({
                    code: publisher.code,
                    name: publisher.name,
                    kind: publisher.kind,
                    description: publisher.description,
                    terms_url: publisher.terms_url,
                    terms_credit: publisher.terms_credit,
                    terms_accepted: publisher.terms_accepted_digest !== '',
                    terms_accepted_by: publisher.terms_accepted_by,
                    terms_accepted_at: publisher.terms_accepted_at,
                }))
                .sort((left, right) => left.name.localeCompare(right.name)),
            suppliers: suppliers
                .map((row) => supplierSchema.parse(row))
                .map((supplier) => ({
                    code: supplier.code,
                    name: supplier.name,
                    kind: supplier.kind,
                    description: supplier.description,
                    contract_reference: supplier.contract_reference,
                    support: supplier.support,
                }))
                .sort((left, right) => left.name.localeCompare(right.name)),
            adapters: adapters
                .map((row) => adapterSchema.parse(row))
                .map((adapter) => ({
                    code: adapter.code,
                    name: adapter.name,
                    module_version: adapter.module_version,
                    description: adapter.description,
                    process: adapter.process,
                    asset_classes: words(adapter.asset_classes),
                    services: words(adapter.services),
                    schema: schemaFields(adapter.configuration_schema),
                }))
                .sort((left, right) => left.name.localeCompare(right.name)),
            sessions: sessions
                .map((row) => sessionSchema.parse(row))
                .map((row) => ({
                    code: row.code,
                    name: row.name,
                    supplier_code: row.supplier_code,
                    adapter_code: row.adapter_code,
                    environment: row.environment,
                    endpoint: row.endpoint,
                    credential_ref: row.credential_ref,
                    settings: document(row.settings),
                    enabled: row.enabled,
                    description: row.description,
                }))
                .sort((left, right) => left.code.localeCompare(right.code)),
            feeds: feeds
                .map((row) => feedSchema.parse(row))
                .map((feed) => ({
                    id: feed.id,
                    name: feed.name,
                    publisher_code: feed.publisher_code,
                    session_code: feed.session_code,
                    service: feed.service,
                    provider_key: feed.provider_key,
                    is_default: feed.is_default,
                    description: feed.description,
                    scopes: parsedScopes
                        .filter((scope) => scope.feed_id === feed.id)
                        .map((scope) => ({
                            asset_class: scope.asset_class,
                            item: scope.item,
                            vendor_instrument_id: scope.vendor_instrument_id,
                        }))
                        .sort((left, right) => left.item.localeCompare(right.item)),
                    routes: parsedRoutes
                        .filter((route) => route.feed_id === feed.id)
                        .map((route) => ({
                            party_id: route.party_id,
                            publisher_code: route.publisher_code,
                            asset_class: route.asset_class,
                        })),
                }))
                .sort((left, right) => left.name.localeCompare(right.name)),
        };
    });
}
