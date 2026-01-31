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
#include "ores.assets/repository/image_repository.hpp"

#include <charconv>
#include <sstream>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.assets/repository/image_mapper.hpp"
#include "ores.assets/repository/image_entity.hpp"

namespace ores::assets::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string image_repository::sql() {
    return generate_create_table_sql<image_entity>(lg());
}

void image_repository::write(context ctx, const domain::image& image) {
    BOOST_LOG_SEV(lg(), debug) << "Writing image to database. Key: " << image.key;

    execute_write_query(ctx, image_mapper::map(image),
        lg(), "Writing image to database.");
}

void image_repository::write(context ctx, const std::vector<domain::image>& images) {
    BOOST_LOG_SEV(lg(), debug) << "Writing images to database. Count: "
                               << images.size();

    execute_write_query(ctx, image_mapper::map(images),
        lg(), "Writing images to database.");
}

std::vector<domain::image> image_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<image_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<image_entity, domain::image>(ctx, query,
        [](const auto& entities) { return image_mapper::map(entities); },
        lg(), "Reading latest images");
}

std::vector<domain::image>
image_repository::read_latest_by_id(context ctx, const std::string& image_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest images by ID: " << image_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<image_entity>> |
        where("image_id"_c == image_id && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<image_entity, domain::image>(ctx, query,
        [](const auto& entities) { return image_mapper::map(entities); },
        lg(), "Reading latest images by ID.");
}

std::vector<domain::image>
image_repository::read_latest_by_ids(context ctx,
    const std::vector<std::string>& image_ids) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest images by IDs. Count: "
                               << image_ids.size();

    if (image_ids.empty()) {
        return {};
    }

    // Build IN clause with properly escaped values
    std::ostringstream in_clause;
    for (size_t i = 0; i < image_ids.size(); ++i) {
        if (i > 0) in_clause << ", ";
        // Escape single quotes by doubling them
        std::string escaped_id;
        for (char c : image_ids[i]) {
            if (c == '\'') escaped_id += "''";
            else escaped_id += c;
        }
        in_clause << "'" << escaped_id << "'";
    }

    // Build the complete SQL query
    std::ostringstream sql;
    sql << "SELECT image_id, version, key, description, svg_data, "
        << "modified_by, valid_from, valid_to "
        << "FROM ores_assets_images_tbl "
        << "WHERE image_id IN (" << in_clause.str() << ") "
        << "AND valid_to = '9999-12-31 23:59:59' "
        << "ORDER BY valid_from DESC";

    auto rows = execute_raw_multi_column_query(ctx, sql.str(), lg(),
        "Reading latest images by IDs");

    // Convert raw results to domain objects
    std::vector<domain::image> results;
    results.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() < 8) continue;

        image_entity entity;
        entity.image_id = row[0].value_or("");

        // Use std::from_chars for safe, non-throwing integer parsing
        int version = 0;
        if (row[1]) {
            std::from_chars(row[1]->data(), row[1]->data() + row[1]->size(), version);
        }
        entity.version = version;

        entity.key = row[2].value_or("");
        entity.description = row[3].value_or("");
        entity.svg_data = row[4].value_or("");
        entity.modified_by = row[5].value_or("");
        entity.valid_from = row[6].value_or("9999-12-31 23:59:59");
        entity.valid_to = row[7].value_or("9999-12-31 23:59:59");

        results.push_back(image_mapper::map(entity));
    }

    return results;
}

std::vector<domain::image>
image_repository::read_latest_by_key(context ctx, const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest images by key: " << key;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<image_entity>> |
        where("key"_c == key && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<image_entity, domain::image>(ctx, query,
        [](const auto& entities) { return image_mapper::map(entities); },
        lg(), "Reading latest images by key.");
}

std::vector<domain::image>
image_repository::read_latest(context ctx, std::uint32_t offset,
                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest images with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<image_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc()) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<image_entity, domain::image>(ctx, query,
        [](const auto& entities) { return image_mapper::map(entities); },
        lg(), "Reading latest images with pagination.");
}

std::vector<domain::image>
image_repository::read_latest_since(context ctx,
    std::chrono::system_clock::time_point modified_since) {

    // Format timestamp for sqlgen query (thread-safe)
    const auto timestamp_str =
        platform::time::datetime::format_time_point_utc(modified_since);

    BOOST_LOG_SEV(lg(), debug) << "Reading latest images modified since: "
                               << timestamp_str;

    // Use sqlgen query with timestamp comparison
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto since_ts = make_timestamp(timestamp_str, lg());

    const auto query = sqlgen::read<std::vector<image_entity>> |
        where("valid_to"_c == max.value() && "valid_from"_c >= since_ts.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<image_entity, domain::image>(ctx, query,
        [](const auto& entities) { return image_mapper::map(entities); },
        lg(), "Reading latest images since timestamp");
}

std::uint32_t image_repository::get_total_image_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active image count";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<image_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active image count: " << count;
    return count;
}

std::vector<domain::image> image_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<image_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<image_entity, domain::image>(ctx, query,
        [](const auto& entities) { return image_mapper::map(entities); },
        lg(), "Reading all images.");
}

void image_repository::remove(context ctx, const std::string& image_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing image from database: " << image_id;

    const auto query = sqlgen::delete_from<image_entity> |
        where("image_id"_c == image_id);

    execute_delete_query(ctx, query, lg(), "Removing image from database.");
}

}
