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
#include "ores.iam/repository/account_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::account account_mapper::map(const account_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::account r;
    r.version = v.version;
    r.recorded_by = v.modified_by;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.username = v.username;
    r.password_hash = v.password_hash;
    r.password_salt = v.password_salt;
    r.totp_secret = v.totp_secret;
    r.email = v.email;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

account_entity account_mapper::map(const domain::account& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    account_entity r;
    r.id = boost::lexical_cast<std::string>(v.id);
    r.version = v.version;
    r.username = v.username;
    r.password_hash = v.password_hash;
    r.password_salt = v.password_salt;
    r.totp_secret = v.totp_secret;
    r.email = v.email;
    r.modified_by = v.recorded_by;
    // Note: recorded_at is read-only; valid_from/valid_to are managed by database triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::account>
account_mapper::map(const std::vector<account_entity>& v) {
    return map_vector<account_entity, domain::account>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<account_entity>
account_mapper::map(const std::vector<domain::account>& v) {
    return map_vector<domain::account, account_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
