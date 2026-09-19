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
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/repository/login_info_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/login_info_json_io.hpp" // IWYU pragma: keep.
#include "ores.platform/time/datetime.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <sstream>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::login_info login_info_mapper::map(const login_info_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::login_info r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.account_id = boost::lexical_cast<boost::uuids::uuid>(v.account_id.value());
    r.last_ip = boost::asio::ip::make_address(v.last_ip);
    r.last_attempt_ip = boost::asio::ip::make_address(v.last_attempt_ip);
    r.failed_logins = v.failed_logins;
    r.locked = v.locked;
    r.last_login = timestamp_to_timepoint(std::string_view{v.last_login});
    r.online = v.online;
    r.password_reset_required = v.password_reset_required;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

login_info_entity login_info_mapper::map(const domain::login_info& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    login_info_entity r;
    r.account_id = boost::uuids::to_string(v.account_id);
    r.tenant_id = v.tenant_id.to_string();
    r.last_ip = v.last_ip.to_string();
    r.last_attempt_ip = v.last_attempt_ip.to_string();
    r.failed_logins = v.failed_logins;
    r.locked = v.locked;
    r.last_login = ores::platform::time::datetime::to_iso8601_utc(v.last_login);
    r.online = v.online;
    r.password_reset_required = v.password_reset_required;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::login_info> login_info_mapper::map(const std::vector<login_info_entity>& v) {
    return map_vector<login_info_entity, domain::login_info>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<login_info_entity> login_info_mapper::map(const std::vector<domain::login_info>& v) {
    return map_vector<domain::login_info, login_info_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
