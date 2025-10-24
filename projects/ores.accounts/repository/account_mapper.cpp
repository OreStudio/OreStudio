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
#include "ores.accounts/repository/account_mapper.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.accounts.repository.account_mapper"));

}

namespace ores::accounts::repository {

domain::account account_mapper::map(const account_entity& v) {
    BOOST_LOG_SEV(lg, debug) << "Mapping db entity: " << v;

    domain::account r;
    r.version = v.version;
    r.modified_by = v.modified_by;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.username = v.username;
    r.password_hash = v.password_hash;
    r.password_salt = v.password_salt;
    r.totp_secret = v.totp_secret;
    r.email = v.email;
    r.is_admin = v.is_admin != 0 ? true : false;

    BOOST_LOG_SEV(lg, debug) << "Mapped db entity. Result: " << r;
    return r;
}

account_entity account_mapper::map(const domain::account& v) {
    BOOST_LOG_SEV(lg, debug) << "Mapping domain entity: " << v;

    account_entity r;
    r.id = boost::lexical_cast<std::string>(v.id);
    r.version = v.version;
    r.username = v.username;
    r.password_hash = v.password_hash;
    r.password_salt = v.password_salt;
    r.totp_secret = v.totp_secret;
    r.email = v.email;
    r.is_admin = v.is_admin;
    r.modified_by = v.modified_by;

    BOOST_LOG_SEV(lg, debug) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::account>
account_mapper::map(const std::vector<account_entity>& v) {
    BOOST_LOG_SEV(lg, debug) << "Mapping db entities. Total: " << v.size();

    std::vector<domain::account> r;
    r.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });

    BOOST_LOG_SEV(lg, debug) << "Mapped db entities.";
    return r;
}

std::vector<account_entity>
account_mapper::map(const std::vector<domain::account>& v) {
    BOOST_LOG_SEV(lg, debug) << "Mapping domain entities. Count: " << v.size();

    std::vector<account_entity> r;
    r.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });
    BOOST_LOG_SEV(lg, debug) << "Mapped domain entities.";
    return r;
}

}
