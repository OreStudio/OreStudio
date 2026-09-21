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
#ifndef ORES_IAM_API_MESSAGING_LOGIN_INFO_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_LOGIN_INFO_PROTOCOL_HPP

#include "ores.iam.api/domain/login_info.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct login_info_key {
    boost::uuids::uuid account_id;
};

struct login_info_write {
    boost::uuids::uuid account_id;
    boost::asio::ip::address last_ip;
    boost::asio::ip::address last_attempt_ip;
    int failed_logins;
    bool locked;
    std::chrono::system_clock::time_point last_login;
    bool online;
    bool password_reset_required;
};

struct login_info_change {
    login_info_write write;
    ores::utility::domain::precondition precondition;
};

struct login_info_removal {
    login_info_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct login_info_lookup {
    login_info_key key;
    std::optional<ores::iam::domain::login_info> login_info;
};

struct list_login_info_request {
    using response_type = struct list_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
};

struct list_login_info_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::login_info> login_info;
    std::uint64_t total;
};

struct get_login_info_request {
    using response_type = struct get_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.get";
    login_info_key key;
};

struct get_login_info_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::login_info> login_info;
};

struct get_many_login_info_request {
    using response_type = struct get_many_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.get_many";
    std::vector<login_info_key> keys;
};

struct get_many_login_info_response {
    ores::utility::domain::result result;
    std::vector<login_info_lookup> entries;
};

struct put_login_info_request {
    using response_type = struct put_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.put";
    login_info_change change;
    ores::utility::domain::change_intent intent;
};

struct put_login_info_response {
    ores::utility::domain::result result;
    ores::iam::domain::login_info login_info;
};

struct put_many_login_info_request {
    using response_type = struct put_many_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.put_many";
    std::vector<login_info_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_login_info_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::login_info> login_info;
};

struct delete_login_info_request {
    using response_type = struct delete_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.delete";
    login_info_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_login_info_response {
    ores::utility::domain::result result;
};

struct delete_many_login_info_request {
    using response_type = struct delete_many_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.delete_many";
    std::vector<login_info_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_login_info_response {
    ores::utility::domain::result result;
};

}

#endif
