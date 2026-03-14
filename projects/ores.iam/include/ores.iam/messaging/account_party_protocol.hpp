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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.iam/domain/account_party.hpp"

namespace ores::iam::messaging {

struct account_party_key {
    std::string account_id;
    std::string party_id;
};

struct get_account_parties_request {
    using response_type = struct get_account_parties_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.account-parties.list";
    int offset = 0;
    int limit = 100;
};

struct get_account_parties_response {
    std::vector<ores::iam::domain::account_party> account_parties;
    int total_available_count = 0;
};

struct get_account_parties_by_account_request {
    using response_type = struct get_account_parties_by_account_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.account-parties.by-account";
    std::string account_id;
};

struct get_account_parties_by_account_response {
    std::vector<ores::iam::domain::account_party> account_parties;
};

struct save_account_party_request {
    using response_type = struct save_account_party_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.account-parties.save";
    std::vector<ores::iam::domain::account_party> account_parties;
};

struct save_account_party_response {
    bool success = false;
    std::string message;
};

struct delete_account_party_request {
    using response_type = struct delete_account_party_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.account-parties.delete";
    std::vector<account_party_key> keys;
};

struct delete_account_party_response {
    bool success = false;
    std::string message;
};

}

#endif
