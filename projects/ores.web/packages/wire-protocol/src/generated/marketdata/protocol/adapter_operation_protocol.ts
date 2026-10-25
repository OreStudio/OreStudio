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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief One value a provider publishes: an id scheme, an external id and a field.
 *
 * The three parts are the vendor's own notation, so the text is stored exactly
 * as the vendor writes it, with its spaces and punctuation.
 */
export interface VendorId {
    id_scheme: string;
    external_id: string;
    field: string;
}

/**
 * @brief An id that an adapter refuses to serve, with the reason.
 *
 * The reason is a short code such as not_entitled or unknown_id.
 */
export interface RejectedId {
    id: VendorId;
    reason: string;
}

/**
 * @brief An adapter announces itself, its provider and its universe.
 *
 * The universe is the set of ids the adapter can serve. The service checks it
 * against the catalogue of the provider and never takes it as the catalogue.
 */
export interface AdapterAnnounceRequest {
    adapter_id: string;
    provider_code: string;
    universe: VendorId[];
}

/**
 * @brief The service accepts or refuses an announce.
 *
 * The counts say how the universe compared with the catalogue.
 */
export interface AdapterAnnounceResponse {
    success: boolean;
    message: string;
    served_count: number;
    unknown_count: number;
}

/**
 * @brief The service tells an adapter which ids to publish.
 *
 * The set replaces the last one: an adapter opens subscriptions for the ids in it
 * and closes the others.
 */
export interface AdapterDemandRequest {
    ids: VendorId[];
}

/**
 * @brief The adapter accepts some ids and rejects the others, each with a reason.
 */
export interface AdapterDemandResponse {
    accepted: VendorId[];
    rejected: RejectedId[];
}

/**
 * @brief One value of one vendor id at one time.
 *
 * It carries no oresmd URI, no tenant, no party and no source kind. The service
 * maps the id and finds the parties.
 */
export interface AdapterTick {
    id_scheme: string;
    external_id: string;
    field: string;
    value: string;
    vendor_time: string;
    sequence: number;
}

/**
 * @brief A periodic sign of life.
 *
 * A missed heartbeat makes the adapter stale.
 */
export interface AdapterHeartbeat {
    adapter_id: string;
    counter: number;
}

/**
 * @brief The state of one adapter and the last demand it answered.
 *
 * The state is announced, ready, serving, stale or rejected. The counts say how the
 * announced universe compared with the catalogue. The accepted and rejected ids are
 * those of the last demand reply.
 */
export interface AdapterStatus {
    adapter_id: string;
    provider_code: string;
    state: string;
    last_heartbeat_at: string;
    heartbeat_counter: number;
    served_count: number;
    unknown_count: number;
    demanded_count: number;
    routes_seen: number;
    ticks_last_minute: number;
    last_tick_age_ms: number;
    accepted: VendorId[];
    rejected: RejectedId[];
}

/**
 * @brief Asks for the state of every adapter the service tracks.
 *
 * The request carries nothing.
 */
export interface ListAdapterStatusRequest {}

/**
 * @brief The state of every adapter, in the order of their ids.
 */
export interface ListAdapterStatusResponse {
    adapters: AdapterStatus[];
    success: boolean;
    message: string;
}

export const subjects = {
    list_adapter_status_request: 'marketdata.v1.ops.list_adapter_status',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_adapter_status_request: true,
} as const;
