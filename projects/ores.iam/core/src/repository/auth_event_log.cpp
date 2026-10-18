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
#include "ores.iam.core/repository/auth_event_log.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.core/repository/audit_scope.hpp"
#include "ores.iam.core/repository/auth_event_entity.hpp"
#include "ores.iam.core/repository/auth_event_repository.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;
using ores::platform::time::datetime;

auth_event_log::auth_event_log(context ctx)
    : ctx_(std::move(ctx)) {}

void auth_event_log::insert(const std::string& event_type,
                            const std::chrono::system_clock::time_point& event_time,
                            const std::string& tenant_id,
                            const std::string& account_id,
                            const std::string& username,
                            const std::string& session_id,
                            const std::string& party_id,
                            const std::string& error_detail) {

    BOOST_LOG_SEV(lg(), debug) << "Recording auth event: " << event_type;

    boost::uuids::random_generator uuid_gen;

    domain::auth_event event;
    event.id = boost::lexical_cast<std::string>(uuid_gen());
    event.event_time = event_time;
    event.tenant_id = tenant_id;
    event.account_id = account_id;
    event.event_type = event_type;
    event.username = username;
    event.session_id = session_id;
    event.party_id = party_id;
    event.error_detail = error_detail;

    // The row is written by a session of its own tenant, which the isolation
    // policy requires, and an event with no tenant is the platform's.
    const auto scope = audit_scope_for(ctx_, tenant_id);
    event.tenant_id = scope.tenant_id;
    auth_event_repository().write(scope.ctx, event);

    BOOST_LOG_SEV(lg(), debug) << "Auth event recorded: " << event_type;
}

void auth_event_log::record_login_success(const std::chrono::system_clock::time_point& event_time,
                                          const std::string& tenant_id,
                                          const std::string& account_id,
                                          const std::string& username,
                                          const std::string& session_id,
                                          const std::string& party_id) {
    insert("login_success", event_time, tenant_id, account_id, username, session_id, party_id, "");
}

void auth_event_log::record_login_failure(const std::chrono::system_clock::time_point& event_time,
                                          const std::string& tenant_id,
                                          const std::string& username,
                                          const std::string& error_detail) {
    insert("login_failure", event_time, tenant_id, "", username, "", "", error_detail);
}

void auth_event_log::record_logout(const std::chrono::system_clock::time_point& event_time,
                                   const std::string& tenant_id,
                                   const std::string& account_id,
                                   const std::string& username,
                                   const std::string& session_id) {
    insert("logout", event_time, tenant_id, account_id, username, session_id, "", "");
}

void auth_event_log::record_token_refresh(const std::chrono::system_clock::time_point& event_time,
                                          const std::string& tenant_id,
                                          const std::string& account_id,
                                          const std::string& username,
                                          const std::string& session_id) {
    insert("token_refresh", event_time, tenant_id, account_id, username, session_id, "", "");
}

void auth_event_log::record_max_session_exceeded(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& session_id) {
    insert("max_session_exceeded", event_time, tenant_id, account_id, username, session_id, "", "");
}

void auth_event_log::record_tenant_entered(const std::chrono::system_clock::time_point& event_time,
                                           const std::string& tenant_id,
                                           const std::string& account_id,
                                           const std::string& username,
                                           const std::string& session_id,
                                           const std::string& party_id) {
    insert("tenant_entered", event_time, tenant_id, account_id, username, session_id, party_id, "");
}

void auth_event_log::record_tenant_left(const std::chrono::system_clock::time_point& event_time,
                                        const std::string& tenant_id,
                                        const std::string& account_id,
                                        const std::string& username,
                                        const std::string& session_id,
                                        const std::string& party_id) {
    insert("tenant_left", event_time, tenant_id, account_id, username, session_id, party_id, "");
}

void auth_event_log::record_account_locked(const std::chrono::system_clock::time_point& event_time,
                                           const std::string& tenant_id,
                                           const std::string& account_id,
                                           const std::string& username,
                                           const std::string& error_detail) {
    insert("account_locked", event_time, tenant_id, account_id, username, "", "", error_detail);
}

void auth_event_log::record_account_unlocked(
    const std::chrono::system_clock::time_point& event_time,
    const std::string& tenant_id,
    const std::string& account_id,
    const std::string& username,
    const std::string& error_detail) {
    insert("account_unlocked", event_time, tenant_id, account_id, username, "", "", error_detail);
}

void auth_event_log::record_signup_success(const std::chrono::system_clock::time_point& event_time,
                                           const std::string& tenant_id,
                                           const std::string& account_id,
                                           const std::string& username) {
    insert("signup_success", event_time, tenant_id, account_id, username, "", "", "");
}

void auth_event_log::record_signup_failure(const std::chrono::system_clock::time_point& event_time,
                                           const std::string& tenant_id,
                                           const std::string& username,
                                           const std::string& error_detail) {
    insert("signup_failure", event_time, tenant_id, "", username, "", "", error_detail);
}

std::vector<repository::auth_event_entity>
auth_event_log::read_events(const context& ctx,
                            const std::string& tenant_id,
                            const std::string& account_id,
                            const std::string& event_type,
                            const std::optional<std::chrono::system_clock::time_point>& from_time,
                            const std::optional<std::chrono::system_clock::time_point>& to_time,
                            std::uint32_t limit,
                            std::uint32_t offset) {
    /*
     * A window bound the caller states no value for binds as SQL NULL, so the
     * statement asks is null and applies no bound on that side. The bounds
     * reach the column as timestamptz: the parameter is cast once, in the
     * statement, because text does not compare against the column.
     *
     * event_time is rendered in UTC here, in the project's one wire shape.
     * A bare timestamptz column carries the reading session's text
     * rendering, which changes with its timezone; to_char at time zone 'UTC'
     * states the microsecond and appends the Z that from_iso8601_utc
     * requires. See doc/knowledge/architecture/time_architecture.org.
     */
    static const std::string sql =
        "select id, "
        "to_char(event_time at time zone 'UTC', 'YYYY-MM-DD HH24:MI:SS.US') || 'Z', "
        "tenant_id, account_id, event_type, username, "
        "session_id, party_id, error_detail "
        "from ores_iam_auth_events_tbl "
        "where tenant_id = $1 "
        "and ($2 = '' or account_id = $2) "
        "and ($3 = '' or event_type = $3) "
        "and ($4::timestamptz is null or event_time >= $4::timestamptz) "
        "and ($5::timestamptz is null or event_time <= $5::timestamptz) "
        "order by event_time desc "
        "limit $6::bigint offset $7::bigint";

    const auto from_bound =
        from_time.has_value() ? std::optional(datetime::to_iso8601_utc(*from_time)) : std::nullopt;
    const auto to_bound =
        to_time.has_value() ? std::optional(datetime::to_iso8601_utc(*to_time)) : std::nullopt;

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        std::vector<std::optional<std::string>>{tenant_id,
                                                account_id,
                                                event_type,
                                                from_bound,
                                                to_bound,
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
