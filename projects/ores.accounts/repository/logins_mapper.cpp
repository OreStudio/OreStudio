/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/log/logger.hpp"
#include "ores.accounts/repository/logins_mapper.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.accounts.repository.logins_mapper"));

std::chrono::system_clock::time_point
timestamp_to_timepoint(const sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">& ts) {
    const auto str = ts.str();
    std::tm tm = {};
    std::istringstream ss(str);
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">
timepoint_to_timestamp(const std::chrono::system_clock::time_point& tp) {
    const auto s = std::format("{:%Y-%m-%d %H:%M:%S}", tp);
    const auto r = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">::from_string(s);
    if (!r) {
        BOOST_LOG_SEV(lg, error) << "Error converting timepoint to timestamp";
        return {};
    }
    return r.value();
}

}

namespace ores::accounts::repository {

domain::logins logins_mapper::map(const logins_entity& v) {
    BOOST_LOG_SEV(lg, debug) << "Mapping db entity: " << v;

    domain::logins r;
    r.account_id = boost::lexical_cast<boost::uuids::uuid>(v.account_id.value());
    r.last_ip = boost::asio::ip::make_address(v.last_ip);
    r.last_attempt_ip = boost::asio::ip::make_address(v.last_attempt_ip);
    r.failed_logins = v.failed_logins;
    r.locked = v.locked;
    r.last_login = timestamp_to_timepoint(v.last_login);
    r.online = v.online;

    BOOST_LOG_SEV(lg, debug) << "Mapped db entity. Result: " << r;
    return r;
}

logins_entity logins_mapper::map(const domain::logins& v) {
    BOOST_LOG_SEV(lg, debug) << "Mapping domain entity: " << v;

    logins_entity r;
    r.account_id = boost::lexical_cast<std::string>(v.account_id);
    r.last_ip = v.last_ip.to_string();
    r.last_attempt_ip = v.last_attempt_ip.to_string();
    r.failed_logins = v.failed_logins;
    r.locked = v.locked;
    r.last_login = timepoint_to_timestamp(v.last_login);
    r.online = v.online;

    BOOST_LOG_SEV(lg, debug) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::logins>
logins_mapper::map(const std::vector<logins_entity>& v) {
    BOOST_LOG_SEV(lg, debug) << "Mapping db entities. Total: " << v.size();

    std::vector<domain::logins> r;
    r.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });

    BOOST_LOG_SEV(lg, debug) << "Mapped db entities.";
    return r;
}

std::vector<logins_entity>
logins_mapper::map(const std::vector<domain::logins>& v) {
    BOOST_LOG_SEV(lg, debug) << "Mapping domain entities. Count: " << v.size();

    std::vector<logins_entity> r;
    r.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });
    BOOST_LOG_SEV(lg, debug) << "Mapped domain entities.";
    return r;
}

}
