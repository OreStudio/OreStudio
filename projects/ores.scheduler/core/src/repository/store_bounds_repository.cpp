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
#include "ores.scheduler.core/repository/store_bounds_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include <charconv>
#include <stdexcept>

namespace ores::scheduler::repository {

namespace {

auto& lg() {
    static auto instance =
        ores::logging::make_logger("ores.scheduler.repository.store_bounds_repository");
    return instance;
}

std::int64_t to_int64(const std::optional<std::string>& text) {
    if (!text || text->empty())
        return 0;
    std::int64_t value = 0;
    const auto result = std::from_chars(text->data(), text->data() + text->size(), value);
    if (result.ec != std::errc{})
        return 0;
    return value;
}

int to_int(const std::optional<std::string>& text) {
    return static_cast<int>(to_int64(text));
}

bool to_bool(const std::optional<std::string>& text) {
    return text && (*text == "t" || *text == "true");
}

std::optional<std::string> absent_if_empty(const std::optional<std::string>& text) {
    if (!text || text->empty())
        return std::nullopt;
    return text;
}

const std::string&
column(const std::vector<std::optional<std::string>>& row, std::size_t index, const char* name) {
    static const std::string empty;
    if (index >= row.size())
        throw std::invalid_argument(std::string("The bounds row has no column ") + name);
    return row[index] ? *row[index] : empty;
}

}

store_bounds_repository::store_bounds_repository(const ores::database::context& ctx)
    : ctx_(ctx) {}

std::vector<messaging::store_bound> store_bounds_repository::read_bounds() {
    const std::string sql =
        "select code, display_name, description, owner_component, target_kind, target,"
        " bound_kind, action, retention_days::text, policy_code,"
        " coalesce(policy_enabled, false)::text, mechanism,"
        " coalesce(bounded_now, false)::text, last_outcome,"
        " to_char(last_run at time zone 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"'),"
        " coalesce(last_removed, 0)::text, last_detail,"
        " coalesce(size_bytes, 0)::text, coalesce(measure_count, 0)::text,"
        " to_char(sampled_at at time zone 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')"
        " from ores_scheduler_store_bounds_vw"
        " order by display_name, code";

    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx_, sql, {}, lg(), "Reading the bounds of every store");

    std::vector<messaging::store_bound> bounds;
    bounds.reserve(rows.size());
    for (const auto& row : rows)
        bounds.push_back(store_bound_from_row(row));
    return bounds;
}

std::vector<messaging::store_sample>
store_bounds_repository::read_samples(const std::string& store_code, std::uint32_t limit) {
    const std::string sql =
        "select to_char(sampled_at at time zone 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"'),"
        " size_bytes::text, measure_count::text"
        " from ("
        "   select sampled_at, size_bytes, measure_count"
        "   from ores_scheduler_retained_store_samples_tbl"
        "   where store_code = $1"
        "   order by sampled_at desc"
        "   limit $2::bigint"
        " ) recent"
        " order by sampled_at";

    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx_,
        sql,
        {store_code, std::to_string(limit)},
        lg(),
        "Reading the measurements of " + store_code);

    std::vector<messaging::store_sample> samples;
    samples.reserve(rows.size());
    for (const auto& row : rows) {
        messaging::store_sample sample;
        sample.sampled_at = column(row, 0, "sampled_at");
        sample.size_bytes = to_int64(row.size() > 1 ? row[1] : std::nullopt);
        sample.measure_count = to_int64(row.size() > 2 ? row[2] : std::nullopt);
        samples.push_back(std::move(sample));
    }
    return samples;
}

messaging::store_bound store_bound_from_row(const std::vector<std::optional<std::string>>& row) {
    messaging::store_bound bound;
    bound.code = column(row, 0, "code");
    bound.display_name = column(row, 1, "display_name");
    bound.description = column(row, 2, "description");
    bound.owner_component = column(row, 3, "owner_component");
    bound.target_kind = column(row, 4, "target_kind");
    bound.target = column(row, 5, "target");
    bound.bound_kind = column(row, 6, "bound_kind");
    bound.action = absent_if_empty(row.size() > 7 ? row[7] : std::nullopt);
    if (row.size() > 8 && row[8] && !row[8]->empty())
        bound.retention_days = to_int(row[8]);
    bound.policy_code = absent_if_empty(row.size() > 9 ? row[9] : std::nullopt);
    bound.policy_enabled = to_bool(row.size() > 10 ? row[10] : std::nullopt);
    bound.mechanism = column(row, 11, "mechanism");
    bound.bounded_now = to_bool(row.size() > 12 ? row[12] : std::nullopt);
    bound.last_outcome = absent_if_empty(row.size() > 13 ? row[13] : std::nullopt);
    bound.last_run_at = absent_if_empty(row.size() > 14 ? row[14] : std::nullopt);
    bound.last_removed = to_int64(row.size() > 15 ? row[15] : std::nullopt);
    bound.last_detail = absent_if_empty(row.size() > 16 ? row[16] : std::nullopt);
    bound.size_bytes = to_int64(row.size() > 17 ? row[17] : std::nullopt);
    bound.measure_count = to_int64(row.size() > 18 ? row[18] : std::nullopt);
    bound.sampled_at = absent_if_empty(row.size() > 19 ? row[19] : std::nullopt);
    return bound;
}

}
