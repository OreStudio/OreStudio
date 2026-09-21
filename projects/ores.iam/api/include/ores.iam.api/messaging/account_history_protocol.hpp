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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_HISTORY_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_HISTORY_PROTOCOL_HPP

#include "ores.iam.api/domain/account.hpp"
#include <chrono>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief One version of an account, with the metadata a history view
 * renders beside it.
 */
struct account_version {
    /**
     * @brief The account data at this version.
     */
    ores::iam::domain::account data;
    /**
     * @brief Version number (1-based, higher is newer).
     */
    int version_number = 0;
    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string modified_by;
    /**
     * @brief Timestamp when this version was recorded in the system.
     */
    std::chrono::system_clock::time_point recorded_at;
    /**
     * @brief Summary of changes made in this version.
     *
     * Examples: "Created account", "Modified 2 fields", "Updated email".
     */
    std::string change_summary;
};

struct account_version_history {
    std::vector<account_version> versions;
};

struct get_account_history_request {
    using response_type = struct get_account_history_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.history";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string username;
};

struct get_account_history_response {
    bool success = false;
    std::string message;
    account_version_history history;
};

}

#endif
