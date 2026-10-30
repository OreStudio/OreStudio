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

import type {
    Connectivity,
    ConnectivityFeed,
    ConnectivityPublisher,
    ConnectivitySession,
    ConnectivitySupplier,
} from '../api/client.js';

/** What the pane shows: a record, the adapter catalogue, or the opening overview. */
export type Selected =
    | { readonly kind: 'overview' }
    | { readonly kind: 'adapters' }
    | { readonly kind: 'supplier'; readonly code: string }
    | { readonly kind: 'publisher'; readonly code: string }
    | { readonly kind: 'session'; readonly code: string }
    | { readonly kind: 'feed'; readonly id: string };

export function feedsOfPublisher(
    catalogue: Connectivity,
    code: string,
): readonly ConnectivityFeed[] {
    return catalogue.feeds.filter((feed) => feed.publisher_code === code);
}

export function sessionsOfSupplier(
    catalogue: Connectivity,
    code: string,
): readonly ConnectivitySession[] {
    return catalogue.sessions.filter((session) => session.supplier_code === code);
}

export function feedsOfSession(catalogue: Connectivity, code: string): readonly ConnectivityFeed[] {
    return catalogue.feeds.filter((feed) => feed.session_code === code);
}

/** The feeds a supplier delivers, which are the feeds of every session it holds. */
export function feedsOfSupplier(
    catalogue: Connectivity,
    code: string,
): readonly ConnectivityFeed[] {
    const sessions = new Set(sessionsOfSupplier(catalogue, code).map((session) => session.code));
    return catalogue.feeds.filter((feed) => sessions.has(feed.session_code));
}

/** The classes a feed's scope names, sorted. */
export function classesOfFeed(feed: ConnectivityFeed): readonly string[] {
    return [...new Set(feed.scopes.map((scope) => scope.asset_class))].sort();
}

/** The classes a publisher delivers through all its feeds, sorted. */
export function classesOfPublisher(catalogue: Connectivity, code: string): readonly string[] {
    return [
        ...new Set(feedsOfPublisher(catalogue, code).flatMap((feed) => classesOfFeed(feed))),
    ].sort();
}

/** The services a supplier's sessions offer. */
export function servicesOfSupplier(catalogue: Connectivity, code: string): readonly string[] {
    const sessions = new Set(sessionsOfSupplier(catalogue, code).map((session) => session.code));
    return [
        ...new Set(
            feedsOfSupplier(catalogue, code)
                .filter((feed) => sessions.has(feed.session_code))
                .map((feed) => feed.service),
        ),
    ].sort();
}

/** The publishers a supplier serves, aggregated by name. */
export function publishersOfSupplier(
    catalogue: Connectivity,
    code: string,
): readonly {
    readonly code: string;
    readonly name: string;
    readonly classes: readonly string[];
}[] {
    const feeds = feedsOfSupplier(catalogue, code);
    const codes = [...new Set(feeds.map((feed) => feed.publisher_code))];
    return codes
        .map((publisher) => {
            const classes = [
                ...new Set(
                    feeds
                        .filter((feed) => feed.publisher_code === publisher)
                        .flatMap((feed) => classesOfFeed(feed)),
                ),
            ].sort();
            return {
                code: publisher,
                name: catalogue.publishers.find((p) => p.code === publisher)?.name ?? publisher,
                classes,
            };
        })
        .sort((left, right) => left.name.localeCompare(right.name));
}

/**
 * What needs a person: a session that is disabled, a feed with no scope, and a
 * feed nothing routes. A feed with no scope is configured but fetches nothing;
 * a feed with no route is configured but read by no party.
 */
export function attentionOf(
    catalogue: Connectivity,
    supplier?: string,
): readonly {
    readonly kind: 'session' | 'feed';
    readonly label: string;
    readonly reason: 'disabled' | 'no-scope' | 'no-route';
}[] {
    const sessions =
        supplier === undefined ? catalogue.sessions : sessionsOfSupplier(catalogue, supplier);
    const sessionCodes = new Set(sessions.map((session) => session.code));
    const feeds =
        supplier === undefined
            ? catalogue.feeds
            : catalogue.feeds.filter((feed) => sessionCodes.has(feed.session_code));
    const out: {
        kind: 'session' | 'feed';
        label: string;
        reason: 'disabled' | 'no-scope' | 'no-route';
    }[] = [];
    for (const session of sessions) {
        if (!session.enabled) {
            out.push({ kind: 'session', label: session.code, reason: 'disabled' });
        }
    }
    for (const feed of feeds) {
        if (feed.scopes.length === 0) {
            out.push({ kind: 'feed', label: feed.name, reason: 'no-scope' });
        } else if (feed.routes.length === 0) {
            out.push({ kind: 'feed', label: feed.name, reason: 'no-route' });
        }
    }
    return out;
}

/** Whether a supplier, publisher, session or feed answers the search box. */
export function matches(search: string, ...values: readonly string[]): boolean {
    const needle = search.trim().toLowerCase();
    if (needle === '') {
        return true;
    }
    return values.some((value) => value.toLowerCase().includes(needle));
}

/** A supplier's feeds whose publisher the search box names. */
export function supplierMatches(
    catalogue: Connectivity,
    supplier: ConnectivitySupplier,
    search: string,
): boolean {
    return matches(
        search,
        supplier.name,
        supplier.code,
        ...sessionsOfSupplier(catalogue, supplier.code).map((session) => session.code),
        ...feedsOfSupplier(catalogue, supplier.code).map((feed) => feed.name),
    );
}

/** A publisher's feeds whose feed or code the search box names. */
export function publisherMatches(
    catalogue: Connectivity,
    publisher: ConnectivityPublisher,
    search: string,
): boolean {
    return matches(
        search,
        publisher.name,
        publisher.code,
        ...feedsOfPublisher(catalogue, publisher.code).map((feed) => feed.name),
    );
}
