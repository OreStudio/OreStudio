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
#include "ores.iam/repository/auth_event_repository.hpp"

#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam/repository/session_entity.hpp"

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

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
    entity.event_time = timepoint_to_timestamp(event_time, lg());
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
    insert("login_success", event_time, tenant_id, account_id,
        username, session_id, party_id, "");
}

void auth_event_repository::record_login_failure(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& username,
    const std::string& error_detail) {
    insert("login_failure", event_time, tenant_id, "",
        username, "", "", error_detail);
}

void auth_event_repository::record_logout(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id) {
    insert("logout", event_time, tenant_id, account_id,
        username, session_id, "", "");
}

void auth_event_repository::record_token_refresh(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id) {
    insert("token_refresh", event_time, tenant_id, account_id,
        username, session_id, "", "");
}

void auth_event_repository::record_max_session_exceeded(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id) {
    insert("max_session_exceeded", event_time, tenant_id, account_id,
        username, session_id, "", "");
}

void auth_event_repository::record_signup_success(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username) {
    insert("signup_success", event_time, tenant_id, account_id,
        username, "", "", "");
}

void auth_event_repository::record_signup_failure(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& username,
    const std::string& error_detail) {
    insert("signup_failure", event_time, tenant_id, "",
        username, "", "", error_detail);
}

}
