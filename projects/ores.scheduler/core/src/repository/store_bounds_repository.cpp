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
#include "ores.platform/time/datetime.hpp"
#include <charconv>
#include <chrono>
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
        " time_column, measure_kind, bound_kind, action, retention_days::text,"
        " cadence_hours::text, policy_code,"
        " coalesce(policy_enabled, false)::text, coalesce(policy_version, 0)::text,"
        " policy_description, mechanism, coalesce(bounded_now, false)::text, last_outcome,"
        " to_char(last_run at time zone 'UTC', 'YYYY-MM-DD HH24:MI:SS.US') || 'Z',"
        " coalesce(last_removed, 0)::text, last_detail,"
        " coalesce(size_bytes, 0)::text, coalesce(measure_count, 0)::text,"
        " to_char(sampled_at at time zone 'UTC', 'YYYY-MM-DD HH24:MI:SS.US') || 'Z'"
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

int range_hours(const std::string& range) {
    if (range == "1h")
        return 1;
    if (range == "7d")
        return 7 * 24;
    return 24;
}

store_sample_series store_bounds_repository::read_samples(const std::string& store_code,
                                                          const std::string& range) {
    const auto to = std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    const auto from = to - std::chrono::hours(range_hours(range));

    const std::string sql =
        "select to_char(sampled_at at time zone 'UTC', 'YYYY-MM-DD HH24:MI:SS.US') || 'Z',"
        " size_bytes::text, measure_count::text"
        " from ores_scheduler_retained_store_samples_tbl"
        " where store_code = $1"
        "   and sampled_at >= $2::timestamptz"
        "   and sampled_at <= $3::timestamptz"
        " order by sampled_at";

    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx_,
        sql,
        {store_code,
         ores::platform::time::datetime::to_iso8601_utc(from),
         ores::platform::time::datetime::to_iso8601_utc(to)},
        lg(),
        "Reading the measurements of " + store_code);

    store_sample_series series;
    series.from_at = ores::platform::time::datetime::to_iso8601_utc(from);
    series.to_at = ores::platform::time::datetime::to_iso8601_utc(to);
    series.samples.reserve(rows.size());
    for (const auto& row : rows) {
        messaging::store_sample sample;
        sample.sampled_at = column(row, 0, "sampled_at");
        sample.size_bytes = to_int64(row.size() > 1 ? row[1] : std::nullopt);
        sample.measure_count = to_int64(row.size() > 2 ? row[2] : std::nullopt);
        series.samples.push_back(std::move(sample));
    }
    return series;
}

capacity_sample_series store_bounds_repository::read_capacity(const std::string& range) {
    const auto to = std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    const auto from = to - std::chrono::hours(range_hours(range));

    const std::string sql =
        "select to_char(sampled_at at time zone 'UTC', 'YYYY-MM-DD HH24:MI:SS.US') || 'Z',"
        " database_name, host_name, measured_path, database_bytes::text,"
        " coalesce(disk_free_bytes::text, ''), coalesce(disk_total_bytes::text, ''), note"
        " from ores_scheduler_capacity_samples_tbl"
        " where sampled_at >= $1::timestamptz"
        "   and sampled_at <= $2::timestamptz"
        " order by sampled_at";

    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx_,
        sql,
        {ores::platform::time::datetime::to_iso8601_utc(from),
         ores::platform::time::datetime::to_iso8601_utc(to)},
        lg(),
        "Reading the installation's capacity");

    capacity_sample_series series;
    series.from_at = ores::platform::time::datetime::to_iso8601_utc(from);
    series.to_at = ores::platform::time::datetime::to_iso8601_utc(to);
    series.samples.reserve(rows.size());
    for (const auto& row : rows) {
        messaging::capacity_sample sample;
        sample.sampled_at = column(row, 0, "sampled_at");
        sample.database_name = column(row, 1, "database_name");
        sample.host_name = column(row, 2, "host_name");
        sample.measured_path = column(row, 3, "measured_path");
        sample.database_bytes = to_int64(row.size() > 4 ? row[4] : std::nullopt);
        if (row.size() > 5 && row[5] && !row[5]->empty())
            sample.disk_free_bytes = to_int64(row[5]);
        if (row.size() > 6 && row[6] && !row[6]->empty())
            sample.disk_total_bytes = to_int64(row[6]);
        sample.note = column(row, 7, "note");
        series.samples.push_back(std::move(sample));
    }
    return series;
}

messaging::store_bound store_bound_from_row(const std::vector<std::optional<std::string>>& row) {
    messaging::store_bound bound;
    bound.code = column(row, 0, "code");
    bound.display_name = column(row, 1, "display_name");
    bound.description = column(row, 2, "description");
    bound.owner_component = column(row, 3, "owner_component");
    bound.target_kind = column(row, 4, "target_kind");
    bound.target = column(row, 5, "target");
    bound.time_column = absent_if_empty(row.size() > 6 ? row[6] : std::nullopt);
    bound.measure_kind = absent_if_empty(row.size() > 7 ? row[7] : std::nullopt);
    bound.bound_kind = column(row, 8, "bound_kind");

    bound.bound.action = absent_if_empty(row.size() > 9 ? row[9] : std::nullopt);
    if (row.size() > 10 && row[10] && !row[10]->empty())
        bound.bound.retention_days = to_int(row[10]);
    if (row.size() > 11 && row[11] && !row[11]->empty())
        bound.bound.cadence_hours = to_int(row[11]);
    bound.bound.policy_code = absent_if_empty(row.size() > 12 ? row[12] : std::nullopt);
    bound.bound.policy_enabled = to_bool(row.size() > 13 ? row[13] : std::nullopt);
    bound.bound.policy_version = to_int(row.size() > 14 ? row[14] : std::nullopt);
    bound.bound.policy_description = absent_if_empty(row.size() > 15 ? row[15] : std::nullopt);
    bound.bound.mechanism = column(row, 16, "mechanism");
    bound.bound.bounded_now = to_bool(row.size() > 17 ? row[17] : std::nullopt);

    bound.last_run.outcome = absent_if_empty(row.size() > 18 ? row[18] : std::nullopt);
    bound.last_run.ran_at = absent_if_empty(row.size() > 19 ? row[19] : std::nullopt);
    bound.last_run.removed = to_int64(row.size() > 20 ? row[20] : std::nullopt);
    bound.last_run.detail = absent_if_empty(row.size() > 21 ? row[21] : std::nullopt);

    bound.size.size_bytes = to_int64(row.size() > 22 ? row[22] : std::nullopt);
    bound.size.measure_count = to_int64(row.size() > 23 ? row[23] : std::nullopt);
    bound.size.sampled_at = absent_if_empty(row.size() > 24 ? row[24] : std::nullopt);
    return bound;
}

}
