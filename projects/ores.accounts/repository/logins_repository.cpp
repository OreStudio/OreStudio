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
#include <format>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/log/logger.hpp"
#include "ores.utility/repository/repository_exception.hpp"
#include "ores.accounts/repository/logins_mapper.hpp"
#include "ores.accounts/repository/logins_entity.hpp"
#include "ores.accounts/repository/logins_repository.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.accounts.repository.logins_repository"));
using ores::utility::repository::repository_exception;

void ensure_success(const auto result) {
    if (!result) {
        BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();
        BOOST_THROW_EXCEPTION(
            repository_exception(std::format("Repository error: {}",
                    result.error().what())));
    }
}

}

namespace ores::accounts::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

std::string logins_repository::sql() {
    const auto query = create_table<logins_entity> | if_not_exists;
    const auto sql = postgres::to_sql(query);

    BOOST_LOG_SEV(lg, debug) << sql;
    return sql;
}

void logins_repository::
write(context ctx, const std::vector<domain::logins>& logins) {
    BOOST_LOG_SEV(lg, debug) << "Writing logins to database. Count: "
                             << logins.size();

    const auto r = session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(logins_mapper::map(logins)))
        .and_then(commit);
    ensure_success(r);

    BOOST_LOG_SEV(lg, debug) << "Finished writing logins to database.";
}

std::vector<domain::logins> logins_repository::read(context ctx) {
    BOOST_LOG_SEV(lg, debug) << "Reading all logins.";

    const auto query = sqlgen::read<std::vector<logins_entity>>;

    const auto r = session(ctx.connection_pool())
        .and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read all logins. Total: " << r->size();
    return logins_mapper::map(*r);
}

std::vector<domain::logins>
logins_repository::read(context ctx, const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg, debug) << "Reading logins for account: " << account_id;

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto query = sqlgen::read<std::vector<logins_entity>> |
        where("account_id"_c == account_id_str);

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read logins. Total: " << r->size();
    return logins_mapper::map(*r);
}

}
