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
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.inbox.core/repository/notification_preference_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.inbox.api/domain/notification_preference_json_io.hpp" // IWYU pragma: keep.
#include "ores.inbox.core/repository/notification_preference_entity.hpp"
#include "ores.inbox.core/repository/notification_preference_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <initializer_list>
#include <set>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string_view>
#include <tuple>

namespace ores::inbox::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string notification_preference_repository::sql() {
    return generate_create_table_sql<notification_preference_entity>(lg());
}

bool notification_preference_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy list_order(const ores::utility::domain::order& order,
                                    std::initializer_list<std::string> default_columns,
                                    bool default_descending) {
    if (order.field.empty())
        return make_order(default_columns,
                          default_descending != order.descending,
                          {"account_id", "kind_code", "channel_code"});
    if (!notification_preference_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of notification preferences cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"account_id", "kind_code", "channel_code"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::notification_preferences_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->account_id)
        r.push_back(equals("account_id", filter_value(*filter->account_id)));
    if (filter->account_id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->account_id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("account_id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
notification_preference_repository::replace_claim(context ctx,
                                                  const domain::notification_preference& v) {
    const auto current =
        read_latest(ctx, boost::uuids::to_string(v.account_id), v.kind_code, v.channel_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::notification_preference
notification_preference_repository::apply_claim(context ctx,
                                                const domain::notification_preference& v,
                                                const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(
                ctx, boost::uuids::to_string(v.account_id), v.kind_code, v.channel_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void notification_preference_repository::write(context ctx,
                                               const domain::notification_preference& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void notification_preference_repository::write(
    context ctx, const std::vector<domain::notification_preference>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void notification_preference_repository::write(context ctx,
                                               const domain::notification_preference& v,
                                               const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing notification preference. "
                               << "account_id: " << v.account_id << " kind_code: " << v.kind_code
                               << " channel_code: " << v.channel_code;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        notification_preference_mapper::map(t),
                        lg(),
                        "Writing notification preference to database.");
}

void notification_preference_repository::write(
    context ctx,
    const std::vector<domain::notification_preference>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing notification preferences. Count: " << v.size();
    std::vector<domain::notification_preference> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        notification_preference_mapper::map(batch),
                        lg(),
                        "Writing notification preferences to database.");
}

std::vector<domain::notification_preference>
notification_preference_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_preference_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("account_id"_c, "kind_code"_c, "channel_code"_c);

    return execute_read_query<notification_preference_entity, domain::notification_preference>(
        ctx,
        query,
        [](const auto& entities) { return notification_preference_mapper::map(entities); },
        lg(),
        "Reading latest notification preferences");
}

std::vector<domain::notification_preference>
notification_preference_repository::read_latest(context ctx,
                                                const std::string& account_id,
                                                const std::string& kind_code,
                                                const std::string& channel_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification preference. "
                               << "account_id: " << account_id << " kind_code: " << kind_code
                               << " channel_code: " << channel_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<notification_preference_entity>> |
        where("tenant_id"_c == tid && "account_id"_c == account_id && "kind_code"_c == kind_code &&
              "channel_code"_c == channel_code && "valid_to"_c == max.value());

    return execute_read_query<notification_preference_entity, domain::notification_preference>(
        ctx,
        query,
        [](const auto& entities) { return notification_preference_mapper::map(entities); },
        lg(),
        "Reading latest notification preference by account_id.");
}


std::vector<domain::notification_preference>
notification_preference_repository::read_all(context ctx,
                                             const std::string& account_id,
                                             const std::string& kind_code,
                                             const std::string& channel_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all notification preference versions. "
                               << "account_id: " << account_id << " kind_code: " << kind_code
                               << " channel_code: " << channel_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_preference_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id &&
                             "kind_code"_c == kind_code && "channel_code"_c == channel_code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<notification_preference_entity, domain::notification_preference>(
        ctx,
        query,
        [](const auto& entities) { return notification_preference_mapper::map(entities); },
        lg(),
        "Reading all notification preference versions by account_id.");
}

std::optional<domain::notification_preference>
notification_preference_repository::read_at_version(context ctx,
                                                    const std::string& account_id,
                                                    const std::string& kind_code,
                                                    const std::string& channel_code,
                                                    std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading notification preference at version. "
                               << "account_id: " << account_id << " kind_code: " << kind_code
                               << " channel_code: " << channel_code << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<notification_preference_entity>> |
        where("tenant_id"_c == tid && "account_id"_c == account_id && "kind_code"_c == kind_code &&
              "channel_code"_c == channel_code && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities =
        execute_read_query<notification_preference_entity, domain::notification_preference>(
            ctx,
            query,
            [](const auto& entities) { return notification_preference_mapper::map(entities); },
            lg(),
            "Reading notification preference at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::notification_preference>
notification_preference_repository::read_latest_by_account_id(
    context ctx,
    const std::string& account_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::notification_preferences_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification preferences. account_id: "
                               << account_id << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<notification_preference_entity>> |
        where("tenant_id"_c == tid && "account_id"_c == account_id && "valid_to"_c == max.value()) |
        sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<notification_preference_entity,
                                      domain::notification_preference>(
        ctx,
        query,
        list_order(order, {"account_id", "kind_code", "channel_code"}, false),
        filter_condition(filter),
        [](const auto& entities) { return notification_preference_mapper::map(entities); },
        lg(),
        "Reading latest notification preferences by account_id.");
}

std::uint32_t notification_preference_repository::get_total_preference_count_by_account_id(
    context ctx,
    const std::string& account_id,
    const std::optional<messaging::notification_preferences_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active notification preferences count. account_id: " << account_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<notification_preference_entity>> |
        where("tenant_id"_c == tid && "account_id"_c == account_id && "valid_to"_c == max.value());

    return execute_count_query<notification_preference_entity>(
        ctx,
        query,
        filter_condition(filter),
        lg(),
        "Counting notification preferences by account_id");
}


notification_preference_repository::remove_status
notification_preference_repository::remove(context ctx,
                                           const std::string& account_id,
                                           const std::string& kind_code,
                                           const std::string& channel_code,
                                           std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing notification preference. "
                               << "account_id: " << account_id << " kind_code: " << kind_code
                               << " channel_code: " << channel_code;
    const auto current = read_latest(ctx, account_id, kind_code, channel_code);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<notification_preference_entity> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id &&
                             "kind_code"_c == kind_code && "channel_code"_c == channel_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing notification preference from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, account_id, kind_code, channel_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void notification_preference_repository::remove(context ctx,
                                                const std::string& account_id,
                                                const std::string& kind_code,
                                                const std::string& channel_code) {
    static_cast<void>(remove(ctx, account_id, kind_code, channel_code, std::nullopt));
}

std::vector<domain::notification_preference> notification_preference_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::notification_preferences_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification preferences with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_preference_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<notification_preference_entity,
                                      domain::notification_preference>(
        ctx,
        query,
        list_order(order, {"account_id", "kind_code", "channel_code"}, false),
        filter_condition(filter),
        [](const auto& entities) { return notification_preference_mapper::map(entities); },
        lg(),
        "Reading latest notification preferences with pagination.");
}

std::uint32_t notification_preference_repository::get_total_preference_count(
    context ctx, const std::optional<messaging::notification_preferences_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active notification preference count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_preference_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value());

    return execute_count_query<notification_preference_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting notification preferences");
}

std::vector<domain::notification_preference>
notification_preference_repository::read_latest(context ctx,
                                                const std::vector<std::string>& account_ids,
                                                const std::vector<std::string>& kind_codes,
                                                const std::vector<std::string>& channel_codes) {
    if (account_ids.empty() || kind_codes.empty() || channel_codes.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_preference_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c.in(account_ids) &&
                             "kind_code"_c.in(kind_codes) && "channel_code"_c.in(channel_codes) &&
                             "valid_to"_c == max.value());
    auto result =
        execute_read_query<notification_preference_entity, domain::notification_preference>(
            ctx,
            query,
            [](const auto& entities) { return notification_preference_mapper::map(entities); },
            lg(),
            "Reading latest notification preferences by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (kind_codes.size() != account_ids.size() || channel_codes.size() != account_ids.size())
        throw std::invalid_argument("notification_preference_repository::read_latest: key column "
                                    "vectors must be the same length");
    std::set<std::tuple<std::string, std::string, std::string>> requested;
    for (std::size_t i = 0; i < account_ids.size(); ++i)
        requested.emplace(account_ids[i], kind_codes[i], channel_codes[i]);
    std::vector<domain::notification_preference> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(
                boost::uuids::to_string(item.account_id), item.kind_code, item.channel_code)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void notification_preference_repository::remove(context ctx,
                                                const std::vector<std::string>& account_ids,
                                                const std::vector<std::string>& kind_codes,
                                                const std::vector<std::string>& channel_codes) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (kind_codes.size() != account_ids.size() || channel_codes.size() != account_ids.size())
        throw std::invalid_argument("notification_preference_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < account_ids.size(); ++i)
        remove(ctx, account_ids[i], kind_codes[i], channel_codes[i]);
}


}
