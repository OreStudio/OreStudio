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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/domain/login_info.hpp"

namespace ores::iam::messaging {

struct get_accounts_request {
    int offset = 0;
    int limit = 100;
};

struct get_accounts_response {
    std::vector<ores::iam::domain::account> accounts;
    int total_available_count = 0;
};

struct save_account_request {
    using response_type = struct save_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.save";
    std::string principal;
    std::string password;
    std::string totp_secret;
    std::string email;
    std::string account_type;
};

struct update_account_request {
    using response_type = struct update_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.update";
    std::string account_id;
    std::string email;
    std::string change_reason_code;
    std::string change_commentary;
};

struct update_account_response {
    bool success = false;
    std::string message;
};

struct save_account_response {
    bool success = false;
    std::string message;
    std::string account_id;
};

struct delete_account_request {
    using response_type = struct delete_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.delete";
    std::string account_id;
};

struct delete_account_response {
    bool success = false;
    std::string message;
};

struct account_operation_result {
    bool success = false;
    std::string message;
};

struct lock_account_request {
    using response_type = struct lock_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.lock";
    std::vector<std::string> account_ids;
};

struct lock_account_response {
    std::vector<account_operation_result> results;
};

struct unlock_account_request {
    using response_type = struct unlock_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.unlock";
    std::vector<std::string> account_ids;
};

struct unlock_account_response {
    std::vector<account_operation_result> results;
};

struct list_login_info_request {
    using response_type = struct list_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.login-info";
};

struct list_login_info_response {
    std::vector<ores::iam::domain::login_info> login_infos;
};

struct reset_password_request {
    using response_type = struct reset_password_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.reset-password";
    std::vector<std::string> account_ids;
    std::string new_password;
};

struct reset_password_response {
    bool success = false;
    std::string message;
    std::vector<account_operation_result> results;
};

struct change_password_request {
    std::string current_password;
    std::string new_password;
};

struct change_password_response {
    bool success = false;
    std::string message;
};

struct update_my_email_request {
    using response_type = struct update_my_email_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.update-email";
    std::string email;
};

struct update_my_email_response {
    bool success = false;
    std::string message;
};

struct select_party_request {
    using response_type = struct select_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.select-party";
    std::string party_id;
};

struct select_party_response {
    bool success = false;
    std::string message;
    std::string token;
    std::string username;
    std::string tenant_name;
    std::string party_name;
    /**
     * @brief Token lifetime in seconds for the newly issued token.
     *
     * Clients re-arm the proactive refresh timer using this value.
     */
    int access_lifetime_s = 1800;
};

struct get_accounts_request_typed {
    using response_type = struct get_accounts_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.list";
    int offset = 0;
    int limit = 100;
};

struct change_password_request_typed {
    using response_type = struct change_password_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.change-password";
    std::string current_password;
    std::string new_password;
};

}

#endif
