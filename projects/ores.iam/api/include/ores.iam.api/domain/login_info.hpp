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
#ifndef ORES_IAM_API_DOMAIN_LOGIN_INFO_HPP
#define ORES_IAM_API_DOMAIN_LOGIN_INFO_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief Login tracking and security state for an account.
 *
 * Login tracking and security state for one account: the last successful
 * login, the running failed-attempt count, the lock and online flags, the
 * forced password-reset flag, and the IP address of the last success and
 * the last attempt. One row per account, keyed by account_id.
 *
 * The table is current-state (see
 * projects/ores.sql/create/iam/iam_login_info_create.sql): it carries no
 * valid_from/valid_to, no GIST exclusion, no version column and no
 * audit tail, unlike the bi-temporal tables every other domain_entity in
 * this component generates. The :current_state: flag in the * SQL **
 * Flags drawer selects that shape. Three suppressions keep the generated
 * DDL at exactly the constraints the hand-written table has:
 * :skip_uuid_check: on account_id drops the nil-UUID check,
 * :skip_check: on the account foreign key drops the account existence
 * check, and the * SQL ** Indexes drawer restates the three hand-written
 * indexes so none is lost.
 */
struct login_info final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The account this login state belongs to. It is the primary key, so the table holds
     * exactly one row per account.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief IP address the account last logged in from successfully.
     */
    boost::asio::ip::address last_ip;

    /**
     * @brief IP address of the most recent login attempt, successful or failed.
     */
    boost::asio::ip::address last_attempt_ip;

    /**
     * @brief Count of consecutive failed login attempts since the last success.
     */
    int failed_logins = 0;

    /**
     * @brief Flag indicating whether the account is locked for security reasons.
     */
    bool locked = false;

    /**
     * @brief Timestamp of the last successful login.
     */
    std::chrono::system_clock::time_point last_login;

    /**
     * @brief Flag indicating whether the account is currently logged in.
     */
    bool online = false;

    /**
     * @brief Flag indicating the user must change their password on the next login.
     *
     * Set by an admin when resetting a password; cleared after the user sets a new one.
     */
    bool password_reset_required = false;
};

/**
 * @brief Dispatch-key identifier for login_info, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const login_info&) {
    return "ores.iam.login_info";
}

}

#endif
