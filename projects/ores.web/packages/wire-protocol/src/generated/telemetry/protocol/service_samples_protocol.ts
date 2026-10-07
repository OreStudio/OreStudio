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
 * @brief The latest heartbeat recorded for one running service instance.
 */
export interface ServiceSample {
    /**
     * @brief When the service last reported.
     */
    sampled_at: string;
    /**
     * @brief Canonical service name, for example @c ores.compute.service.
     */
    service_name: string;
    /**
     * @brief Per-process identifier, so two instances of one service are told
     * apart.
     */
    instance_id: string;
    /**
     * @brief The host the instance runs on, empty when the publisher does not
     * know it.
     */
    host_id: string;
    /**
     * @brief Version the instance reports.
     */
    version: string;
}

/**
 * @brief One service instance reporting that it is alive.
 *
 * The service publishes it on a timer, and the telemetry service timestamps
 * the receipt and stores it. It carries no response, so the publisher does
 * not wait for one.
 */
export interface ServiceHeartbeatMessage {
    /**
     * @brief Canonical service name, for example @c ores.compute.service.
     */
    service_name: string;
    /**
     * @brief Per-process identifier, generated once at startup.
     */
    instance_id: string;
    /**
     * @brief The host the instance runs on, empty when the publisher does not
     * know it.
     */
    host_id: string;
    /**
     * @brief Version the instance reports.
     */
    version: string;
}

/**
 * @brief Asks for the latest sample of every running instance.
 *
 * It carries no fields: the reply covers every service the caller may see.
 */
export interface GetServiceSamplesRequest {}

/**
 * @brief The latest sample of every running instance.
 */
export interface GetServiceSamplesResponse {
    /**
     * @brief Whether the samples were read.
     */
    success: boolean;
    /**
     * @brief Why they were not, when they were not.
     */
    message: string;
    /**
     * @brief One sample per running instance, in no particular order.
     */
    samples: ServiceSample[];
}

/**
 * @brief One expected instance of one service, and its last report.
 *
 * A service expects as many slots as the registry gives it replicas. The
 * newest reporting instances fill the slots, newest first; a slot no
 * instance fills is missing, and its report fields are empty.
 */
export interface ServiceRosterSlot {
    /**
     * @brief Canonical service name, for example @c ores.iam.service.
     */
    service_name: string;
    /**
     * @brief The name a person reads, for example @c Analytics @c Service.
     */
    display_name: string;
    /**
     * @brief What the service does, in one sentence from the service registry.
     */
    description: string;
    /**
     * @brief The IAM service account the process signs in as, empty for a process
     * that signs in as no account of its own.
     */
    service_account: string | null;
    /**
     * @brief Which expected instance this is, from 1 to the service's replicas.
     */
    slot: number;
    /**
     * @brief Running, lost or missing, read from the heartbeats.
     */
    state: string;
    /**
     * @brief The instance that fills the slot, empty when the slot is missing.
     */
    instance_id: string | null;
    /**
     * @brief The host the instance last reported from, empty when the slot is
     * missing.
     */
    host_id: string | null;
    /**
     * @brief The version the instance last reported, empty when the slot is
     * missing.
     */
    version: string | null;
    /**
     * @brief When the instance last reported, at any age; empty when the slot is
     * missing.
     */
    sampled_at: string | null;
}

/**
 * @brief Asks for the services roster: every expected instance and its state.
 *
 * It carries no fields: the roster covers the whole installation.
 */
export interface GetServiceRosterRequest {}

/**
 * @brief Every expected instance of every enabled service, with its state.
 */
export interface GetServiceRosterResponse {
    /**
     * @brief Whether the roster was read.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
    /**
     * @brief One slot per expected instance, ordered by service name, then slot.
     */
    slots: ServiceRosterSlot[];
}

export const subjects = {
    service_heartbeat_message: 'telemetry.v1.ops.service_heartbeat',
    get_service_samples_request: 'telemetry.v1.services.list',
    get_service_roster_request: 'telemetry.v1.ops.get_service_roster',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    service_heartbeat_message: false,
    get_service_samples_request: true,
    get_service_roster_request: true,
} as const;
