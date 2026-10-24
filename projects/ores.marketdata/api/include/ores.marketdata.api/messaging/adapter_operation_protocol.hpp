/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_MESSAGING_ADAPTER_OPERATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_ADAPTER_OPERATION_PROTOCOL_HPP

#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One value a provider publishes: an id scheme, an external id and a field.
 *
 * The three parts are the vendor's own notation, so the text is stored exactly
 * as the vendor writes it, with its spaces and punctuation.
 */
struct vendor_id {
    std::string id_scheme;
    std::string external_id;
    std::string field;
};

/**
 * @brief An id that an adapter refuses to serve, with the reason.
 *
 * The reason is a short code such as not_entitled or unknown_id.
 */
struct rejected_id {
    vendor_id id;
    std::string reason;
};

/**
 * @brief An adapter announces itself, its provider and its universe.
 *
 * The universe is the set of ids the adapter can serve. The service checks it
 * against the catalogue of the provider and never takes it as the catalogue.
 */
struct adapter_announce_request {
    std::string adapter_id;
    std::string provider_code;
    std::vector<vendor_id> universe;
};

/**
 * @brief The service accepts or refuses an announce.
 *
 * The counts say how the universe compared with the catalogue.
 */
struct adapter_announce_response {
    bool success = false;
    std::string message;
    std::uint32_t served_count = 0;
    std::uint32_t unknown_count = 0;
};

/**
 * @brief The service tells an adapter which ids to publish.
 *
 * The set replaces the last one: an adapter opens subscriptions for the ids in it
 * and closes the others.
 */
struct adapter_demand_request {
    std::vector<vendor_id> ids;
};

/**
 * @brief The adapter accepts some ids and rejects the others, each with a reason.
 */
struct adapter_demand_response {
    std::vector<vendor_id> accepted;
    std::vector<rejected_id> rejected;
};

/**
 * @brief One value of one vendor id at one time.
 *
 * It carries no oresmd URI, no tenant, no party and no source kind. The service
 * maps the id and finds the parties.
 */
struct adapter_tick {
    std::string id_scheme;
    std::string external_id;
    std::string field;
    std::string value;
    std::chrono::system_clock::time_point vendor_time;
    std::uint64_t sequence = 0;
};

/**
 * @brief A periodic sign of life.
 *
 * A missed heartbeat makes the adapter stale.
 */
struct adapter_heartbeat {
    std::string adapter_id;
    std::uint64_t counter = 0;
};

/**
 * @brief The state of one adapter and the last demand it answered.
 *
 * The state is announced, ready, serving, stale or rejected. The counts say how the
 * announced universe compared with the catalogue. The accepted and rejected ids are
 * those of the last demand reply.
 */
struct adapter_status {
    std::string adapter_id;
    std::string provider_code;
    std::string state;
    std::chrono::system_clock::time_point last_heartbeat_at;
    std::uint64_t heartbeat_counter = 0;
    std::uint32_t served_count = 0;
    std::uint32_t unknown_count = 0;
    std::uint32_t demanded_count = 0;
    std::vector<vendor_id> accepted;
    std::vector<rejected_id> rejected;
};

/**
 * @brief Asks for the state of every adapter the service tracks.
 *
 * The request carries nothing.
 */
struct list_adapter_status_request {
    using response_type = struct list_adapter_status_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.list_adapter_status";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The state of every adapter, in the order of their ids.
 */
struct list_adapter_status_response {
    std::vector<adapter_status> adapters;
    bool success = false;
    std::string message;
};

}

#endif
