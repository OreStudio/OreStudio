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
#include "ores.iam.core/repository/auth_event_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.core/repository/auth_event_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;
using ores::platform::time::datetime;

auth_event_repository::auth_event_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void auth_event_repository::insert(const std::string& event_type,
                                   const std::chrono::system_clock::time_point& event_time,
                                   const std::string& tenant_id,
                                   const std::string& account_id,
                                   const std::string& username,
                                   const std::string& session_id,
                                   const std::string& party_id,
                                   const std::string& error_detail) {

    BOOST_LOG_SEV(lg(), debug) << "Recording auth event: " << event_type;

    boost::uuids::random_generator uuid_gen;
    const auto id_str = boost::lexical_cast<std::string>(uuid_gen());

    auth_event_entity entity;
    entity.id = id_str;
    entity.event_time = datetime::to_db_string(event_time);
    entity.tenant_id = tenant_id;
    entity.account_id = account_id;
    entity.event_type = event_type;
    entity.username = username;
    entity.session_id = session_id;
    entity.party_id = party_id;
    entity.error_detail = error_detail;

    const auto r = sqlgen::session(ctx_.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(sqlgen::insert(entity))
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Auth event recorded: " << event_type;
}

void auth_event_repository::record_login_success(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id,
    const std::string& party_id) {
    insert("login_success", event_time, tenant_id, account_id, username, session_id, party_id, "");
}

void auth_event_repository::record_login_failure(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& username,
    const std::string& error_detail) {
    insert("login_failure", event_time, tenant_id, "", username, "", "", error_detail);
}

void auth_event_repository::record_logout(const std::chrono::system_clock::time_point& event_time,
                                          const std::string& tenant_id,
                                          const std::string& account_id,
                                          const std::string& username,
                                          const std::string& session_id) {
    insert("logout", event_time, tenant_id, account_id, username, session_id, "", "");
}

void auth_event_repository::record_token_refresh(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id) {
    insert("token_refresh", event_time, tenant_id, account_id, username, session_id, "", "");
}

void auth_event_repository::record_max_session_exceeded(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id) {
    insert("max_session_exceeded", event_time, tenant_id, account_id, username, session_id, "", "");
}

void auth_event_repository::record_tenant_entered(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id,
    const std::string& party_id) {
    insert("tenant_entered", event_time, tenant_id, account_id, username, session_id, party_id, "");
}

void auth_event_repository::record_tenant_left(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id,
    const std::string& party_id) {
    insert("tenant_left", event_time, tenant_id, account_id, username, session_id, party_id, "");
}

void auth_event_repository::record_signup_success(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username) {
    insert("signup_success", event_time, tenant_id, account_id, username, "", "", "");
}

void auth_event_repository::record_signup_failure(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& username,
    const std::string& error_detail) {
    insert("signup_failure", event_time, tenant_id, "", username, "", "", error_detail);
}

std::vector<repository::auth_event_entity>
auth_event_repository::read_events(const context& ctx,
                                   const std::string& tenant_id,
                                   const std::string& account_id,
                                   const std::string& event_type,
                                   const std::string& from_time,
                                   const std::string& to_time,
                                   std::uint32_t limit,
                                   std::uint32_t offset) {
    /*
     * The filters are optional, and the statement says so itself: an empty
     * parameter does not filter. Building the WHERE clause in C++ would
     * branch once per combination, and the table's (tenant_id, event_time)
     * index still orders the scan.
     *
     * The time bounds arrive as text, because a wire timestamp crosses the
     * protocol as one, and text does not compare against the timestamptz
     * column: PostgreSQL refuses the comparison rather than coercing the
     * parameter, so the read has to name the cast. It goes through nullif
     * so the empty value stays the filter turned off -- casting the empty
     * string on its own raises instead of yielding null.
     */
    static const std::string sql =
        "select id, event_time, tenant_id, account_id, event_type, username, "
        "session_id, party_id, error_detail "
        "from ores_iam_auth_events_tbl "
        "where tenant_id = $1 "
        "and ($2 = '' or account_id = $2) "
        "and ($3 = '' or event_type = $3) "
        "and (nullif($4, '') is null or event_time >= nullif($4, '')::timestamptz) "
        "and (nullif($5, '') is null or event_time <= nullif($5, '')::timestamptz) "
        "order by event_time desc "
        "limit $6::bigint offset $7::bigint";

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {tenant_id,
         account_id,
         event_type,
         from_time,
         to_time,
         std::to_string(limit),
         std::to_string(offset)},
        lg(),
        "Reading auth events");

    std::vector<repository::auth_event_entity> events;
    events.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 9)
            continue;

        repository::auth_event_entity e;
        e.id = row[0].value_or("");
        e.event_time = row[1].value_or("");
        e.tenant_id = row[2].value_or("");
        e.account_id = row[3].value_or("");
        e.event_type = row[4].value_or("");
        e.username = row[5].value_or("");
        e.session_id = row[6].value_or("");
        e.party_id = row[7].value_or("");
        e.error_detail = row[8].value_or("");
        events.push_back(std::move(e));
    }

    return events;
}

}
