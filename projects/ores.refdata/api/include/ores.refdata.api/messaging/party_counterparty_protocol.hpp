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
#ifndef ORES_REFDATA_API_MESSAGING_PARTY_COUNTERPARTY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PARTY_COUNTERPARTY_PROTOCOL_HPP

#include "ores.refdata.api/domain/party_counterparty.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief The party counterparty row enriched with the joined row's
 * display fields, so a screen needs one request for the whole set rather
 * than one per row. The by-side read returns this view.
 */
struct party_counterparty_view {
    ores::refdata::domain::party_counterparty party_counterparty;
};

struct get_party_counterparties_request {
    using response_type = struct get_party_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_party_counterparties_response {
    std::vector<ores::refdata::domain::party_counterparty> party_counterparties;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct get_party_counterparties_by_party_request {
    using response_type = struct get_party_counterparties_by_party_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.party_counterparties.list_by_party_id";
    std::string party_id;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_party_counterparties_by_party_response {
    std::vector<party_counterparty_view> party_counterparties;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_party_counterparty_request {
    using response_type = struct save_party_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.save";
    std::vector<ores::refdata::domain::party_counterparty> party_counterparties;

    static save_party_counterparty_request
    from(std::vector<ores::refdata::domain::party_counterparty> v) {
        return {.party_counterparties = std::move(v)};
    }
};

struct save_party_counterparty_response {
    bool success = false;
    std::string message;
};

struct delete_party_counterparty_request {
    using response_type = struct delete_party_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_counterparties.delete";
    std::vector<std::string> party_ids;
    std::vector<std::string> counterparty_ids;
};

struct delete_party_counterparty_response {
    bool success = false;
    std::string message;
};

struct count_party_counterparties_by_party_request {
    using response_type = struct count_party_counterparties_by_party_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.party_counterparties.count_by_party_id";
    std::string party_id;
};

struct count_party_counterparties_by_party_response {
    int total_available_count = 0;
};

struct count_party_counterparties_by_counterparty_request {
    using response_type = struct count_party_counterparties_by_counterparty_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.party_counterparties.count_by_counterparty_id";
    std::string counterparty_id;
};

struct count_party_counterparties_by_counterparty_response {
    int total_available_count = 0;
};
}

#endif
