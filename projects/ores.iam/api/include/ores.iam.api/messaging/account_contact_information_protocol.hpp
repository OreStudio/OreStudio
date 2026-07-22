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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_CONTACT_INFORMATION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_CONTACT_INFORMATION_PROTOCOL_HPP

#include "ores.iam.api/domain/account_contact_information.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct get_account_contact_informations_request {
    using response_type = struct get_account_contact_informations_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_account_contact_informations_response {
    std::vector<ores::iam::domain::account_contact_information> account_contact_informations;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_account_contact_information_request {
    using response_type = struct save_account_contact_information_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.save";
    ores::iam::domain::account_contact_information data;

    static save_account_contact_information_request
    from(ores::iam::domain::account_contact_information v) {
        return {.data = std::move(v)};
    }
};

struct save_account_contact_information_response {
    bool success = false;
    std::string message;
};

struct delete_account_contact_information_request {
    using response_type = struct delete_account_contact_information_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.delete";
    std::vector<std::string> ids;
};

struct delete_account_contact_information_response {
    bool success = false;
    std::string message;
};

struct get_account_contact_information_history_request {
    using response_type = struct get_account_contact_information_history_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.history";
    std::string id;
};

struct get_account_contact_information_history_response {
    std::vector<ores::iam::domain::account_contact_information> history;
    bool success = false;
    std::string message;
};

struct get_account_contact_informations_by_account_id_request {
    using response_type = struct get_account_contact_informations_by_account_id_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations.list_by_account_id";
    std::string account_id;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_account_contact_informations_by_account_id_response {
    std::vector<ores::iam::domain::account_contact_information> account_contact_informations;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

}

#endif
