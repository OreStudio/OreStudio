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
#include "ores.iam.core/repository/session_sample_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/session_sample.hpp"
#include "ores.iam.api/domain/session_sample_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/session_sample_entity.hpp"
#include "ores.iam.core/repository/session_sample_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <iterator>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string session_sample_repository::sql() {
    return query_builder::insert_statement_sql<session_sample_entity>(lg());
}

bool session_sample_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order session_sample_list_order(const ores::utility::domain::order& order,
                                               std::initializer_list<std::string> default_columns,
                                               bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"session_id", "sample_time"});
    if (!session_sample_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of session samples cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order(
        {order.field}, order.descending, {"session_id", "sample_time"});
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool session_sample_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // No version to state, so only a create needs to know whether a row exists.
    return claim.kind == precondition_kind::must_not_exist;
}

using session_sample_rows_by_key =
    std::map<std::tuple<std::string, std::string>, std::vector<domain::session_sample>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
session_sample_rows_by_key
session_sample_current_rows(session_sample_repository& repo,
                            const ores::database::context& ctx,
                            const std::vector<domain::session_sample>& of) {
    std::vector<std::string> session_ids;
    session_ids.reserve(of.size());
    std::vector<std::string> sample_times;
    sample_times.reserve(of.size());
    for (const auto& row : of) {
        session_ids.push_back(boost::uuids::to_string(row.session_id));
        sample_times.push_back(ores::platform::time::datetime::to_db_string(row.sample_time));
    }
    session_sample_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, session_ids, sample_times))
        current[std::make_tuple(boost::uuids::to_string(item.session_id),
                                ores::platform::time::datetime::to_db_string(item.sample_time))]
            .push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::session_sample>&
session_sample_rows_of(const session_sample_rows_by_key& current,
                       const domain::session_sample& item) {
    static const std::vector<domain::session_sample> none;
    const auto found = current.find(
        std::make_tuple(boost::uuids::to_string(item.session_id),
                        ores::platform::time::datetime::to_db_string(item.sample_time)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
session_sample_replace_claim(const std::vector<domain::session_sample>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // when the claim is applied.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the row the key names now whenever the claim reads it. A
 * claim the store cannot check is refused here rather than ignored.
 */
domain::session_sample
session_sample_apply_claim(const domain::session_sample& v,
                           const ores::utility::domain::precondition& claim,
                           const std::vector<domain::session_sample>& current) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "session_sample_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !current.empty())
        throw std::invalid_argument(
            "session_sample_repository::write: a current row already exists");
    return t;
}

}

void session_sample_repository::write(const context& ctx, const domain::session_sample& v) {
    write(ctx,
          v,
          session_sample_replace_claim(
              read_latest(ctx,
                          boost::uuids::to_string(v.session_id),
                          ores::platform::time::datetime::to_db_string(v.sample_time))));
}

void session_sample_repository::write(const context& ctx,
                                      const std::vector<domain::session_sample>& v) {
    const auto current = session_sample_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(session_sample_replace_claim(session_sample_rows_of(current, item)));
    write(ctx, v, claims);
}

void session_sample_repository::write(const context& ctx,
                                      const domain::session_sample& v,
                                      const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing sample. " << "session_id: " << v.session_id
                               << " sample_time: " << v.sample_time;
    const auto t = session_sample_apply_claim(
        v,
        claim,
        session_sample_claim_reads_current(claim) ?
            read_latest(ctx,
                        boost::uuids::to_string(v.session_id),
                        ores::platform::time::datetime::to_db_string(v.sample_time)) :
            std::vector<domain::session_sample>{});
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(session_sample_mapper::map(t)),
                                    lg(),
                                    "Writing sample to database.");
}

void session_sample_repository::write(
    const context& ctx,
    const std::vector<domain::session_sample>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing session samples. Count: " << v.size();
    std::vector<domain::session_sample> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (session_sample_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = session_sample_current_rows(*this, ctx, to_read);
    std::vector<domain::session_sample> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            session_sample_apply_claim(v[i], claims[i], session_sample_rows_of(current, v[i])));
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(session_sample_mapper::map(batch)),
        lg(),
        "Writing session samples to database.");
}

std::vector<domain::session_sample> session_sample_repository::read_latest(const context& ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_sample_entity>> |
                       query_builder::where("tenant_id"_c == tid) |
                       query_builder::order_by("session_id"_c, "sample_time"_c);

    return query_builder::execute_read_query<session_sample_entity, domain::session_sample>(
        ctx,
        query,
        [](const auto& entities) { return session_sample_mapper::map(entities); },
        lg(),
        "Reading latest session samples");
}

std::vector<domain::session_sample> session_sample_repository::read_latest(
    const context& ctx, const std::string& session_id, const std::string& sample_time) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest sample. " << "session_id: " << session_id
                               << " sample_time: " << sample_time;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_sample_entity>> |
                       query_builder::where("tenant_id"_c == tid && "session_id"_c == session_id &&
                                            "sample_time"_c == sample_time);

    return query_builder::execute_read_query<session_sample_entity, domain::session_sample>(
        ctx,
        query,
        [](const auto& entities) { return session_sample_mapper::map(entities); },
        lg(),
        "Reading latest sample by session_id.");
}


std::vector<domain::session_sample> session_sample_repository::read_all(
    const context& ctx, const std::string& session_id, const std::string& sample_time) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all sample versions. " << "session_id: " << session_id
                               << " sample_time: " << sample_time;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_sample_entity>> |
                       query_builder::where("tenant_id"_c == tid && "session_id"_c == session_id &&
                                            "sample_time"_c == sample_time) |
                       query_builder::order_by("session_id"_c, "sample_time"_c);

    return query_builder::execute_read_query<session_sample_entity, domain::session_sample>(
        ctx,
        query,
        [](const auto& entities) { return session_sample_mapper::map(entities); },
        lg(),
        "Reading all sample versions by session_id.");
}


session_sample_repository::remove_status
session_sample_repository::remove(const context& ctx,
                                  const std::string& session_id,
                                  const std::string& sample_time,
                                  std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing sample. " << "session_id: " << session_id
                               << " sample_time: " << sample_time;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, session_id, sample_time);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<session_sample_entity> |
                       query_builder::where("tenant_id"_c == tid && "session_id"_c == session_id &&
                                            "sample_time"_c == sample_time);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing sample from database.");
    return remove_status::removed;
}

void session_sample_repository::remove(const context& ctx,
                                       const std::string& session_id,
                                       const std::string& sample_time) {
    static_cast<void>(remove(ctx, session_id, sample_time, std::nullopt));
}

std::vector<domain::session_sample>
session_sample_repository::read_latest(const context& ctx,
                                       std::uint32_t offset,
                                       std::uint32_t limit,
                                       const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest session samples with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_sample_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<session_sample_entity, domain::session_sample>(
        ctx,
        query,
        session_sample_list_order(order, {"session_id", "sample_time"}, false),
        std::nullopt,
        [](const auto& entities) { return session_sample_mapper::map(entities); },
        lg(),
        "Reading latest session samples with pagination.");
}

std::uint32_t session_sample_repository::get_total__count(const context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active sample count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_sample_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<session_sample_entity>(
        ctx, query, std::nullopt, lg(), "Counting session samples");
}

std::vector<domain::session_sample>
session_sample_repository::read_latest(const context& ctx,
                                       const std::vector<std::string>& session_ids,
                                       const std::vector<std::string>& sample_times) {
    if (session_ids.empty() || sample_times.empty())
        return {};
    if (sample_times.size() != session_ids.size())
        throw std::invalid_argument(
            "session_sample_repository::read_latest: key column vectors must be the same length");
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (session_ids.size() > chunk_size) {
        std::vector<domain::session_sample> all;
        for (std::size_t begin = 0; begin < session_ids.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, session_ids.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(session_ids.begin() + begin, session_ids.begin() + end),
                std::vector<std::string>(sample_times.begin() + begin, sample_times.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<session_sample_entity>> |
        query_builder::where("tenant_id"_c == tid && "session_id"_c.in(session_ids) &&
                             "sample_time"_c.in(sample_times));
    auto result = query_builder::execute_read_query<session_sample_entity, domain::session_sample>(
        ctx,
        query,
        [](const auto& entities) { return session_sample_mapper::map(entities); },
        lg(),
        "Reading latest session samples by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < session_ids.size(); ++i)
        requested.emplace(session_ids[i], sample_times[i]);
    std::vector<domain::session_sample> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(
                std::make_tuple(boost::uuids::to_string(item.session_id),
                                ores::platform::time::datetime::to_db_string(item.sample_time))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void session_sample_repository::remove(const context& ctx,
                                       const std::vector<std::string>& session_ids,
                                       const std::vector<std::string>& sample_times) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (sample_times.size() != session_ids.size())
        throw std::invalid_argument(
            "session_sample_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < session_ids.size(); ++i)
        remove(ctx, session_ids[i], sample_times[i]);
}


}
