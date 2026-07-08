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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information.hpp"
#include "ores.refdata.api/domain/counterparty_identifier.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct get_counterparties_request {
    using response_type = struct get_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_counterparties_response {
    std::vector<ores::refdata::domain::counterparty> counterparties;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_counterparty_request {
    using response_type = struct save_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.save";
    ores::refdata::domain::counterparty data;

    static save_counterparty_request from(ores::refdata::domain::counterparty v) {
        return {.data = std::move(v)};
    }
};

struct save_counterparty_response {
    bool success = false;
    std::string message;
};

struct delete_counterparty_request {
    using response_type = struct delete_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.delete";
    std::vector<std::string> ids;
};

struct delete_counterparty_response {
    bool success = false;
    std::string message;
};

struct get_counterparty_history_request {
    using response_type = struct get_counterparty_history_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.history";
    std::string id;
};

struct get_counterparty_history_response {
    std::vector<ores::refdata::domain::counterparty> history;
    bool success = false;
    std::string message;
};

/**
 * @brief Reads the counterparty hierarchy rooted at, or containing,
 * a given counterparty.
 */
struct get_counterparty_hierarchy_request {
    using response_type = struct get_counterparty_hierarchy_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.hierarchy";
    std::string root_id;
    bool from_root = false;
};

struct get_counterparty_hierarchy_response {
    bool success = false;
    std::string message;
    std::vector<ores::utility::domain::hierarchy_node> roots;
};

/**
 * @brief Reads a counterparty as it stood at a specific version, together
 * with its identifiers and contact information as they stood during that
 * same version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
struct get_counterparty_composite_as_of_request {
    using response_type = struct get_counterparty_composite_as_of_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.composite_as_of";
    std::string id;
    int version = 0;
};

struct get_counterparty_composite_as_of_response {
    bool success = false;
    std::string message;
    ores::refdata::domain::counterparty counterparty;
    std::vector<ores::refdata::domain::counterparty_identifier> identifiers;
    std::vector<ores::refdata::domain::counterparty_contact_information> contacts;
};

}

#endif
