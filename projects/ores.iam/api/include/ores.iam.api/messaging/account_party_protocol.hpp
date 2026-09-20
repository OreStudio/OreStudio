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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP

#include "ores.iam.api/domain/account_party.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief The account party row enriched with the joined row's
 * display fields, so a screen needs one request for the whole set rather
 * than one per row. The by-side read returns this view.
 */
struct account_party_view {
    ores::iam::domain::account_party account_party;
};

struct get_account_parties_request {
    using response_type = struct get_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_account_parties_response {
    std::vector<ores::iam::domain::account_party> account_parties;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct get_account_parties_by_account_request {
    using response_type = struct get_account_parties_by_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.list_by_account_id";
    std::string account_id;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_account_parties_by_account_response {
    std::vector<account_party_view> account_parties;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_account_party_request {
    using response_type = struct save_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.save";
    std::vector<ores::iam::domain::account_party> account_parties;

    static save_account_party_request from(std::vector<ores::iam::domain::account_party> v) {
        return {.account_parties = std::move(v)};
    }
};

struct save_account_party_response {
    bool success = false;
    std::string message;
};

struct delete_account_party_request {
    using response_type = struct delete_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.delete";
    std::vector<std::string> account_ids;
    std::vector<std::string> party_ids;
};

struct delete_account_party_response {
    bool success = false;
    std::string message;
};

struct replace_account_parties_by_account_request {
    using response_type = struct replace_account_parties_by_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.replace_by_account_id";
    std::string account_id;
    std::vector<ores::iam::domain::account_party> account_parties;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
};

struct replace_account_parties_by_account_response {
    bool success = false;
    std::string message;
};

struct count_account_parties_by_account_request {
    using response_type = struct count_account_parties_by_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.count_by_account_id";
    std::string account_id;
};

struct count_account_parties_by_account_response {
    int total_available_count = 0;
};

struct count_account_parties_by_party_request {
    using response_type = struct count_account_parties_by_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.count_by_party_id";
    std::string party_id;
};

struct count_account_parties_by_party_response {
    int total_available_count = 0;
};
}

#endif
