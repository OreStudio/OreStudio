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
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/repository/notification_kind_repository.hpp"
#include "ores.security/authorization/grants.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <map>
#include <stdexcept>

namespace ores::inbox::service {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;
using ores::database::repository::execute_parameterized_string_query;

namespace {

constexpr int max_page = 200;

/**
 * @brief A PostgreSQL array literal, each element quoted, so a value holding
 * a comma, brace or quote reaches the database as written.
 */
std::string array_literal(const std::vector<std::string>& values) {
    std::string out = "{";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i > 0)
            out += ',';
        out += '"';
        for (const char c : values[i]) {
            if (c == '"' || c == '\\')
                out += '\\';
            out += c;
        }
        out += '"';
    }
    out += '}';
    return out;
}

constexpr auto utc_text = "to_char(%s at time zone 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')";

std::string utc(const std::string& column) {
    std::string out = utc_text;
    out.replace(out.find("%s"), 2, column);
    return out;
}

int first_int(const std::vector<std::string>& rows) {
    return rows.empty() ? 0 : std::stoi(rows.front());
}

}

notification_center::notification_center(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

std::optional<boost::uuids::uuid> notification_center::actor_account_id() {
    ores::iam::repository::account_repository accounts;
    const auto found = accounts.read_latest_by_username(ctx_, ctx_.actor());
    if (found.empty())
        return std::nullopt;
    return found.front().id;
}

bool notification_center::kind_exists(const std::string& code) {
    repository::notification_kind_repository repo;
    return !repo.read_latest(ctx_.with_tenant(utility::uuid::tenant_id::system(), ctx_.actor()),
                             code)
                .empty();
}

std::vector<std::string> notification_center::holders_of(const std::string& permission_code) {
    // Each person's effective codes in one read, then the same wildcard rule
    // a handler's permission check applies: a holder of iam::* or * holds
    // iam::roles:assign.
    const auto rows = execute_parameterized_multi_column_query(
        ctx_,
        "select a.id::text, e.code "
        "from ores_iam_accounts_tbl a "
        "cross join lateral ores_iam_get_effective_permissions_fn(a.id) e "
        "where a.tenant_id = ores_iam_current_tenant_id_fn() "
        "and a.account_type = $1 "
        "and a.valid_to = ores_utility_infinity_timestamp_fn()",
        {"user"},
        lg(),
        "Reading the permissions of the tenant's people");

    std::map<std::string, std::vector<std::string>> codes_by_account;
    for (const auto& row : rows)
        if (row.size() == 2 && row[0] && row[1])
            codes_by_account[*row[0]].push_back(*row[1]);

    std::vector<std::string> holders;
    for (const auto& [account_id, codes] : codes_by_account)
        if (ores::security::authorization::grants(codes, permission_code))
            holders.push_back(account_id);
    return holders;
}

raise_result notification_center::raise(const messaging::raise_notification_request& req,
                                        const std::vector<std::string>& recipient_ids,
                                        const boost::uuids::uuid& raised_by) {
    std::vector<std::string> names;
    std::vector<std::string> values;
    for (const auto& a : req.arguments) {
        names.push_back(a.name);
        values.push_back(a.value);
    }

    BOOST_LOG_SEV(lg(), info) << "Raising a " << req.kind_code << " notification for "
                              << recipient_ids.size() << " recipient(s)";
    const auto id = execute_parameterized_string_query(
        ctx_,
        "select ores_inbox_raise_notification_fn($1, $2, $3, $4, $5::uuid, $6, "
        "$7::text[], $8::text[], $9::uuid[])::text",
        {req.kind_code,
         req.link_route,
         req.link_id,
         req.audience_permission_code,
         boost::uuids::to_string(raised_by),
         ctx_.actor(),
         array_literal(names),
         array_literal(values),
         array_literal(recipient_ids)},
        lg(),
        "Raising a notification");
    if (id.empty())
        throw std::runtime_error("Raising the notification returned no id.");
    return raise_result{.notification_id = id.front(),
                        .recipient_count = static_cast<int>(recipient_ids.size())};
}

notification_page notification_center::mine(const boost::uuids::uuid& account_id,
                                            bool unread_only,
                                            int offset,
                                            int limit) {
    const auto me = boost::uuids::to_string(account_id);
    const std::string unread_clause = unread_only ? " and r.read_at is null" : "";
    const std::string from =
        " from ores_inbox_notification_recipients_tbl r"
        " join ores_inbox_notifications_tbl n on n.id = r.notification_id"
        " and n.tenant_id = r.tenant_id and n.valid_to = ores_utility_infinity_timestamp_fn()"
        " where r.account_id = $1::uuid and r.cleared_at is null"
        " and r.valid_to = ores_utility_infinity_timestamp_fn()" +
        unread_clause;

    notification_page page;
    page.total = first_int(execute_parameterized_string_query(
        ctx_, "select count(*)::text" + from, {me}, lg(), "Counting a person's notifications"));

    const auto rows = execute_parameterized_multi_column_query(
        ctx_,
        "select n.id::text, n.kind_code, coalesce(k.message_key, ''), coalesce(a.username, ''), " +
            utc("n.raised_at") + ", n.link_route, coalesce(n.link_id, ''), coalesce(" +
            utc("r.read_at") + ", '')" +
            " from ores_inbox_notification_recipients_tbl r"
            " join ores_inbox_notifications_tbl n on n.id = r.notification_id"
            " and n.tenant_id = r.tenant_id and n.valid_to = ores_utility_infinity_timestamp_fn()"
            " left join ores_inbox_notification_kinds_tbl k on k.code = n.kind_code"
            " and k.tenant_id = ores_utility_system_tenant_id_fn()"
            " and k.valid_to = ores_utility_infinity_timestamp_fn()"
            " left join ores_iam_accounts_tbl a on a.id = n.raised_by"
            " and a.valid_to = ores_utility_infinity_timestamp_fn()"
            " where r.account_id = $1::uuid and r.cleared_at is null"
            " and r.valid_to = ores_utility_infinity_timestamp_fn()" +
            unread_clause + " order by n.raised_at desc offset $2::integer limit $3::integer",
        {me, std::to_string(std::max(offset, 0)), std::to_string(std::clamp(limit, 1, max_page))},
        lg(),
        "Reading a person's notifications");

    std::vector<std::string> ids;
    for (const auto& row : rows) {
        messaging::inbox_notification n;
        n.id = row[0].value_or("");
        n.kind_code = row[1].value_or("");
        n.message_key = row[2].value_or("");
        n.raised_by = row[3].value_or("");
        n.raised_at = row[4].value_or("");
        n.link_route = row[5].value_or("");
        n.link_id = row[6].value_or("");
        n.read_at = row[7].value_or("");
        ids.push_back(n.id);
        page.notifications.push_back(std::move(n));
    }
    if (ids.empty())
        return page;

    const auto args = execute_parameterized_multi_column_query(
        ctx_,
        "select notification_id::text, name, value"
        " from ores_inbox_notification_arguments_tbl"
        " where notification_id = any($1::uuid[])"
        " and valid_to = ores_utility_infinity_timestamp_fn()"
        " order by notification_id, name",
        {array_literal(ids)},
        lg(),
        "Reading the arguments of a person's notifications");
    for (const auto& row : args) {
        const auto it = std::ranges::find_if(
            page.notifications, [&](const auto& n) { return n.id == row[0].value_or(""); });
        if (it != page.notifications.end())
            it->arguments.push_back(messaging::notification_argument_value{
                .name = row[1].value_or(""), .value = row[2].value_or("")});
    }
    return page;
}

int notification_center::unread(const boost::uuids::uuid& account_id) {
    return first_int(execute_parameterized_string_query(
        ctx_,
        "select count(*)::text from ores_inbox_notification_recipients_tbl"
        " where account_id = $1::uuid and read_at is null and cleared_at is null"
        " and valid_to = ores_utility_infinity_timestamp_fn()",
        {boost::uuids::to_string(account_id)},
        lg(),
        "Counting a person's unread notifications"));
}

int notification_center::mark_read(const boost::uuids::uuid& account_id,
                                   const std::vector<std::string>& ids) {
    return first_int(execute_parameterized_string_query(
        ctx_,
        "select ores_inbox_mark_notifications_read_fn($1::uuid, $2::uuid[], $3)::text",
        {boost::uuids::to_string(account_id), array_literal(ids), ctx_.actor()},
        lg(),
        "Marking notifications read"));
}

int notification_center::clear(const boost::uuids::uuid& account_id,
                               const std::vector<std::string>& ids) {
    return first_int(execute_parameterized_string_query(
        ctx_,
        "select ores_inbox_clear_notifications_fn($1::uuid, $2::uuid[], $3)::text",
        {boost::uuids::to_string(account_id), array_literal(ids), ctx_.actor()},
        lg(),
        "Clearing notifications"));
}

}
