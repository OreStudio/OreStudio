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
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/messaging/tenor_basis_swap_pay_receive_protocol.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_entity.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/Condition.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/dynamic/Value.hpp>
#include <sqlgen/limit.hpp>
#include <sqlgen/literals.hpp>
#include <sqlgen/offset.hpp>
#include <sqlgen/order_by.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/where.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenor_basis_swap_pay_receive_repository::sql() {
    return generate_create_table_sql<tenor_basis_swap_pay_receive_entity>(lg());
}

bool tenor_basis_swap_pay_receive_repository::is_sortable(std::string_view field) {
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
        return make_order(default_columns, default_descending != order.descending, {"id"});
    if (!tenor_basis_swap_pay_receive_repository::is_sortable(order.field))
        throw std::invalid_argument(
            "A list of tenor basis swap pay receives cannot be ordered by " + order.field + ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::tenor_basis_swap_pay_receives_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->convention_id)
        r.push_back(equals("convention_id", filter_value(*filter->convention_id)));
    if (filter->id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("id", std::move(values)));
    }
    if (filter->convention_id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->convention_id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("convention_id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition tenor_basis_swap_pay_receive_repository::replace_claim(
    context ctx, const domain::tenor_basis_swap_pay_receive& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().identity.version)};
}

domain::tenor_basis_swap_pay_receive tenor_basis_swap_pay_receive_repository::apply_claim(
    context ctx,
    const domain::tenor_basis_swap_pay_receive& v,
    const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.identity.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.identity.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.id));
            t.identity.version = current.empty() ? 0 : current.front().identity.version;
            break;
        }
    }
    return t;
}

void tenor_basis_swap_pay_receive_repository::write(context ctx,
                                                    const domain::tenor_basis_swap_pay_receive& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void tenor_basis_swap_pay_receive_repository::write(
    context ctx, const std::vector<domain::tenor_basis_swap_pay_receive>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void tenor_basis_swap_pay_receive_repository::write(
    context ctx,
    const domain::tenor_basis_swap_pay_receive& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor basis swap pay receive. "
                               << "id: " << v.identity.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        tenor_basis_swap_pay_receive_mapper::map(t),
                        lg(),
                        "Writing tenor basis swap pay receive to database.");
}

void tenor_basis_swap_pay_receive_repository::write(
    context ctx,
    const std::vector<domain::tenor_basis_swap_pay_receive>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor basis swap pay receives. Count: " << v.size();
    std::vector<domain::tenor_basis_swap_pay_receive> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        tenor_basis_swap_pay_receive_mapper::map(batch),
                        lg(),
                        "Writing tenor basis swap pay receives to database.");
}

std::vector<domain::tenor_basis_swap_pay_receive>
tenor_basis_swap_pay_receive_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<tenor_basis_swap_pay_receive_entity,
                              domain::tenor_basis_swap_pay_receive>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_pay_receive_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap pay receives");
}

std::vector<domain::tenor_basis_swap_pay_receive>
tenor_basis_swap_pay_receive_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor basis swap pay receive. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<tenor_basis_swap_pay_receive_entity,
                              domain::tenor_basis_swap_pay_receive>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_pay_receive_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap pay receive by id.");
}


std::vector<domain::tenor_basis_swap_pay_receive>
tenor_basis_swap_pay_receive_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenor basis swap pay receive versions. "
                               << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<tenor_basis_swap_pay_receive_entity,
                              domain::tenor_basis_swap_pay_receive>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_pay_receive_mapper::map(entities); },
        lg(),
        "Reading all tenor basis swap pay receive versions by id.");
}

std::optional<domain::tenor_basis_swap_pay_receive>
tenor_basis_swap_pay_receive_repository::read_at_version(context ctx,
                                                         const std::string& id,
                                                         std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading tenor basis swap pay receive at version. "
                               << "id: " << id << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<tenor_basis_swap_pay_receive_entity,
                                             domain::tenor_basis_swap_pay_receive>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_pay_receive_mapper::map(entities); },
        lg(),
        "Reading tenor basis swap pay receive at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::tenor_basis_swap_pay_receive>
tenor_basis_swap_pay_receive_repository::read_latest_by_convention_id(
    context ctx,
    const std::string& convention_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::tenor_basis_swap_pay_receives_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor basis swap pay receives. convention_id: "
                               << convention_id << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid && "convention_id"_c == convention_id &&
                             "valid_to"_c == max.value()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<tenor_basis_swap_pay_receive_entity,
                                      domain::tenor_basis_swap_pay_receive>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        filter_condition(filter),
        [](const auto& entities) { return tenor_basis_swap_pay_receive_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap pay receives by convention_id.");
}

std::uint32_t tenor_basis_swap_pay_receive_repository::
    get_total_tenor_basis_swap_pay_receive_count_by_convention_id(
        context ctx,
        const std::string& convention_id,
        const std::optional<messaging::tenor_basis_swap_pay_receives_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active tenor basis swap pay receives count. convention_id: "
        << convention_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid && "convention_id"_c == convention_id &&
                             "valid_to"_c == max.value());

    return execute_count_query<tenor_basis_swap_pay_receive_entity>(
        ctx,
        query,
        filter_condition(filter),
        lg(),
        "Counting tenor basis swap pay receives by convention_id");
}


std::vector<domain::tenor_basis_swap_pay_receive>
tenor_basis_swap_pay_receive_repository::read_by_convention_id_as_of(
    context ctx,
    const std::string& convention_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading tenor basis swap pay receives as of window. convention_id: " << convention_id;

    const auto vf(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_from_bound), lg()));
    const auto vt(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_to_bound), lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid && "convention_id"_c == convention_id &&
                             "valid_from"_c < vt.value() && "valid_to"_c > vf.value()) |
                       order_by("id"_c);

    return execute_read_query<tenor_basis_swap_pay_receive_entity,
                              domain::tenor_basis_swap_pay_receive>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_pay_receive_mapper::map(entities); },
        lg(),
        "Reading tenor basis swap pay receives as of window by convention_id.");
}

tenor_basis_swap_pay_receive_repository::remove_status
tenor_basis_swap_pay_receive_repository::remove(context ctx,
                                                const std::string& id,
                                                std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor basis swap pay receive. " << "id: " << id;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().identity.version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().identity.version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<tenor_basis_swap_pay_receive_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing tenor basis swap pay receive from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void tenor_basis_swap_pay_receive_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::tenor_basis_swap_pay_receive>
tenor_basis_swap_pay_receive_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::tenor_basis_swap_pay_receives_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor basis swap pay receives with offset: "
                               << offset << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<tenor_basis_swap_pay_receive_entity,
                                      domain::tenor_basis_swap_pay_receive>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        narrowed(valid_at(as_of), filter_condition(filter)),
        [](const auto& entities) { return tenor_basis_swap_pay_receive_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap pay receives with pagination.");
}

std::uint32_t tenor_basis_swap_pay_receive_repository::get_total_tenor_basis_swap_pay_receive_count(
    context ctx,
    const std::optional<messaging::tenor_basis_swap_pay_receives_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenor basis swap pay receive count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid);

    return execute_count_query<tenor_basis_swap_pay_receive_entity>(
        ctx,
        query,
        narrowed(valid_at(as_of), filter_condition(filter)),
        lg(),
        "Counting tenor basis swap pay receives");
}

std::vector<domain::tenor_basis_swap_pay_receive>
tenor_basis_swap_pay_receive_repository::read_latest(context ctx,
                                                     const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_basis_swap_pay_receive_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<tenor_basis_swap_pay_receive_entity,
                                     domain::tenor_basis_swap_pay_receive>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_pay_receive_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap pay receives by ids.");
    return result;
}

void tenor_basis_swap_pay_receive_repository::remove(context ctx,
                                                     const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<tenor_basis_swap_pay_receive_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing tenor basis swap pay receives.");
}


}
