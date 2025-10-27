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
#include <boost/asio/ip/address.hpp>
#include "ores.utility/repository/repository_exception.hpp"
#include "ores.accounts/repository/login_info_mapper.hpp"
#include "ores.accounts/repository/login_info_entity.hpp"
#include "ores.accounts/repository/login_info_repository.hpp"

namespace ores::accounts::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::utility::log;
using ores::utility::repository::repository_exception;

void login_info_repository::ensure_success(const auto result) {
    if (!result) {
        BOOST_LOG_SEV(lg(), severity_level::error) << result.error().what();
        BOOST_THROW_EXCEPTION(
            repository_exception(std::format("Repository error: {}",
                    result.error().what())));
    }
}

std::string login_info_repository::sql() {
    const auto query = create_table<login_info_entity> | if_not_exists;
    const auto sql = postgres::to_sql(query);

    BOOST_LOG_SEV(lg(), debug) << sql;
    return sql;
}

void login_info_repository::
write(context ctx, const std::vector<domain::login_info>& login_infos) {
    BOOST_LOG_SEV(lg(), debug) << "Writing login_info to database. Count: "
                             << login_infos.size();

    const auto r = session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(login_info_mapper::map(login_infos)))
        .and_then(commit);
    ensure_success(r);

    BOOST_LOG_SEV(lg(), debug) << "Finished writing login_info to database.";
}

void login_info_repository::
update(context ctx, const domain::login_info& login_info) {
    BOOST_LOG_SEV(lg(), debug) << "Updating login_info for account: "
                             << boost::uuids::to_string(login_info.account_id);

    auto entity = login_info_mapper::map(login_info);
    const auto query = sqlgen::update<login_info_entity>(
        "last_ip"_c.set(entity.last_ip),
        "last_attempt_ip"_c.set(entity.last_attempt_ip),
        "failed_logins"_c.set(entity.failed_logins),
        "locked"_c.set(entity.locked),
        "last_login"_c.set(entity.last_login),
        "online"_c.set(entity.online)
    ) | where("account_id"_c == entity.account_id);

    const auto r = session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(query)
        .and_then(commit);
    ensure_success(r);

    BOOST_LOG_SEV(lg(), debug) << "Finished updating login_info.";
}

std::vector<domain::login_info> login_info_repository::read(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all login_info.";

    const auto query = sqlgen::read<std::vector<login_info_entity>>;

    const auto r = session(ctx.connection_pool())
        .and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg(), debug) << "Read all login_info. Total: " << r->size();
    return login_info_mapper::map(*r);
}

std::vector<domain::login_info>
login_info_repository::read(context ctx, const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading login_info for account: " << account_id;

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto query = sqlgen::read<std::vector<login_info_entity>> |
        where("account_id"_c == account_id_str);

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg(), debug) << "Read login_info. Total: " << r->size();
    return login_info_mapper::map(*r);
}

}
