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

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.assets/repository/image_mapper.hpp"
#include "ores.assets/repository/image_entity.hpp"

namespace ores::assets::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::utility::log;
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

    // Build results by querying each ID individually
    // This could be optimized with raw SQL IN clause if needed
    std::vector<domain::image> results;
    results.reserve(image_ids.size());

    for (const auto& id : image_ids) {
        auto images = read_latest_by_id(ctx, id);
        for (auto& img : images) {
            results.push_back(std::move(img));
        }
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
