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
#ifndef ORES_REFDATA_API_MESSAGING_PARTY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PARTY_PROTOCOL_HPP

#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.api/domain/party_contact_information.hpp"
#include "ores.refdata.api/domain/party_identifier.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct get_parties_request {
    using response_type = struct get_parties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_parties_response {
    std::vector<ores::refdata::domain::party> parties;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_party_request {
    using response_type = struct save_party_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.save";
    ores::refdata::domain::party data;

    static save_party_request from(ores::refdata::domain::party v) {
        return {.data = std::move(v)};
    }
};

struct save_party_response {
    bool success = false;
    std::string message;
};

struct delete_party_request {
    using response_type = struct delete_party_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.delete";
    std::vector<std::string> ids;
};

struct delete_party_response {
    bool success = false;
    std::string message;
};

struct get_party_history_request {
    using response_type = struct get_party_history_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.history";
    std::string id;
};

struct get_party_history_response {
    std::vector<ores::refdata::domain::party> history;
    bool success = false;
    std::string message;
};

/**
 * @brief Reads the party hierarchy rooted at, or containing,
 * a given party.
 */
struct get_party_hierarchy_request {
    using response_type = struct get_party_hierarchy_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.hierarchy";
    std::string root_id;
    bool from_root = false;
};

struct get_party_hierarchy_response {
    bool success = false;
    std::string message;
    std::vector<ores::utility::domain::hierarchy_node> roots;
};

/**
 * @brief Reads all active parties for a tenant — used by
 * client-side caches to warm up without multiple round-trips.
 */
struct read_parties_for_cache_request {
    using response_type = struct read_parties_for_cache_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.read";
    std::string tenant_id;
};

struct read_parties_for_cache_response {
    bool success = false;
    std::string message;
    std::vector<ores::refdata::domain::party> parties;
};

/**
 * @brief Reads a party as it stood at a specific version, together with its
 * identifiers and contact information as they stood during that same
 * version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
struct get_party_composite_as_of_request {
    using response_type = struct get_party_composite_as_of_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.composite_as_of";
    std::string id;
    int version = 0;
};

struct get_party_composite_as_of_response {
    bool success = false;
    std::string message;
    ores::refdata::domain::party party;
    std::vector<ores::refdata::domain::party_identifier> identifiers;
    std::vector<ores::refdata::domain::party_contact_information> contacts;
};
}

#endif
