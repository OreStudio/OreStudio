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
#include "ores.iam.core/repository/session_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/session_json_io.hpp" // IWYU pragma: keep.
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

domain::session session_mapper::map(const session_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::session r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.start_time = timestamp_to_timepoint(std::string_view{v.start_time.value()});
    r.account_id = boost::lexical_cast<boost::uuids::uuid>(v.account_id);
    r.end_time = v.end_time;
    r.client_ip = boost::asio::ip::make_address(v.client_ip);
    r.client_identifier = v.client_identifier;
    r.client_version_major = v.client_version_major;
    r.client_version_minor = v.client_version_minor;
    r.bytes_sent = v.bytes_sent;
    r.bytes_received = v.bytes_received;
    r.country_code = v.country_code;
    r.protocol = v.protocol;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

session_entity session_mapper::map(const domain::session& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    session_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.start_time = ores::platform::time::datetime::to_db_string(v.start_time);
    r.tenant_id = v.tenant_id.to_string();
    r.account_id = boost::uuids::to_string(v.account_id);
    r.end_time = v.end_time;
    r.client_ip = v.client_ip.to_string();
    r.client_identifier = v.client_identifier;
    r.client_version_major = v.client_version_major;
    r.client_version_minor = v.client_version_minor;
    r.bytes_sent = v.bytes_sent;
    r.bytes_received = v.bytes_received;
    r.country_code = v.country_code;
    r.protocol = v.protocol;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::session> session_mapper::map(const std::vector<session_entity>& v) {
    return map_vector<session_entity, domain::session>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<session_entity> session_mapper::map(const std::vector<domain::session>& v) {
    return map_vector<domain::session, session_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
