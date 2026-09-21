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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_DOMAIN_SESSION_HPP
#define ORES_IAM_API_DOMAIN_SESSION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A user session, recorded in the TimescaleDB sessions hypertable.
 *
 * A user session: one login or service-account connection, recorded for
 * analytics, session listing and end-time tracking. The table is a
 * TimescaleDB hypertable partitioned by start_time with 7-day chunks (see
 * projects/ores.sql/create/iam/iam_sessions_create.sql).
 *
 * The table has no valid_from/valid_to, no GIST exclusion, no version
 * column and no audit tail, and it keys on (id, start_time) rather than a
 * single surrogate key -- the partition column must sit in the primary key
 * of a hypertable. The :current_state: and :hypertable: flags in the
 * * SQL ** Flags drawer select exactly that shape. The compound key is
 * what makes this the first entity in the estate whose key carries a
 * timestamp; the mapper, entity and generator templates gained the
 * timestamp key-column branches they were missing.
 *
 * end_time is text not null default '', not a nullable timestamp, and
 * client_ip is text, not inet. The model mirrors each column's real
 * type; the domain projection is the column's type too, so a session's
 * end_time is an empty string while the session is active and
 * client_ip is a string. The richer readings (an instant, an IP address)
 * belong to the consumer that needs them.
 *
 * The hand-written domain struct also carried party_id,
 * visible_party_ids and username. No column backs any of them; they are
 * the denormalised, party-scoped shape of a session, and the rule that an
 * entity describes its table puts them on a message instead. They are
 * declared as message fields on session_view in
 * ores.iam.session_messages.
 *
 * The entity's CRUD handler and sub-registrar are switched off below: the
 * hand-written session_operations_handler already owns the iam.v1.sessions.*
 * subjects that ores.iam.session_messages declares, and the generated
 * session_operations_handler.hpp would overwrite it. The generated
 * session_protocol.hpp is suppressed by the same one-owner gate that
 * the operation model already satisfies; only the competing handler is
 * switched off here.
 */
struct session final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique identifier for the session.
     */
    boost::uuids::uuid id;

    /**
     * @brief Timestamp when the session started (login time). It is the hypertable's partition
     * column, so it is part of the primary key.
     */
    std::chrono::system_clock::time_point start_time;

    /**
     * @brief Foreign key referencing the associated account.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief Timestamp when the session ended (logout or disconnect), stored as an ISO 8601 string.
     * Empty while the session is still active.
     */
    std::string end_time = "";

    /**
     * @brief Client IP address (IPv4 or IPv6).
     */
    boost::asio::ip::address client_ip;

    /**
     * @brief Client identifier string from the handshake, typically the client application name.
     */
    std::string client_identifier = "";

    /**
     * @brief Client protocol version major number.
     */
    std::uint16_t client_version_major = 0;

    /**
     * @brief Client protocol version minor number.
     */
    std::uint16_t client_version_minor = 0;

    /**
     * @brief Total bytes sent to the client during this session.
     */
    std::uint64_t bytes_sent = 0;

    /**
     * @brief Total bytes received from the client during this session.
     */
    std::uint64_t bytes_received = 0;

    /**
     * @brief ISO 3166-1 alpha-2 country code from geolocation. Empty if geolocation is unavailable
     * or the IP is private or localhost.
     */
    std::string country_code = "";

    /**
     * @brief Protocol used for this session: binary or http.
     */
    std::string protocol = "binary";
};

/**
 * @brief Dispatch-key identifier for session, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const session&) {
    return "ores.iam.session";
}

}

#endif
