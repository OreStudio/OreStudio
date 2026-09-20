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
#ifndef ORES_REFDATA_API_MESSAGING_PARTY_CURRENCY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PARTY_CURRENCY_PROTOCOL_HPP

#include "ores.refdata.api/domain/party_currency.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief The party currency row enriched with the joined row's
 * display fields, so a screen needs one request for the whole set rather
 * than one per row. The by-side read returns this view.
 */
struct party_currency_view {
    ores::refdata::domain::party_currency party_currency;
};

struct get_party_currencies_request {
    using response_type = struct get_party_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_party_currencies_response {
    std::vector<ores::refdata::domain::party_currency> party_currencies;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct get_party_currencies_by_party_request {
    using response_type = struct get_party_currencies_by_party_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.list_by_party_id";
    std::string party_id;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_party_currencies_by_party_response {
    std::vector<party_currency_view> party_currencies;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_party_currency_request {
    using response_type = struct save_party_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.save";
    std::vector<ores::refdata::domain::party_currency> party_currencies;

    static save_party_currency_request from(std::vector<ores::refdata::domain::party_currency> v) {
        return {.party_currencies = std::move(v)};
    }
};

struct save_party_currency_response {
    bool success = false;
    std::string message;
};

struct delete_party_currency_request {
    using response_type = struct delete_party_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.delete";
    std::vector<std::string> party_ids;
    std::vector<std::string> currency_iso_codes;
};

struct delete_party_currency_response {
    bool success = false;
    std::string message;
};

struct count_party_currencies_by_party_request {
    using response_type = struct count_party_currencies_by_party_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.party_currencies.count_by_party_id";
    std::string party_id;
};

struct count_party_currencies_by_party_response {
    int total_available_count = 0;
};

struct count_party_currencies_by_currency_request {
    using response_type = struct count_party_currencies_by_currency_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.party_currencies.count_by_currency_iso_code";
    std::string currency_iso_code;
};

struct count_party_currencies_by_currency_response {
    int total_available_count = 0;
};
}

#endif
