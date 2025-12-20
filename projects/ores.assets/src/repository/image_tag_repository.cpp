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
#include "ores.assets/repository/image_tag_repository.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.assets/repository/image_tag_mapper.hpp"
#include "ores.assets/repository/image_tag_entity.hpp"

namespace ores::assets::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::utility::log;
using namespace ores::database::repository;

std::string image_tag_repository::sql() {
    return generate_create_table_sql<image_tag_entity>(lg());
}

void image_tag_repository::write(context ctx, const domain::image_tag& image_tag) {
    BOOST_LOG_SEV(lg(), debug) << "Writing image-tag to database. Image: "
                               << image_tag.image_id << " Tag: " << image_tag.tag_id;

    execute_write_query(ctx, image_tag_mapper::map(image_tag),
        lg(), "Writing image-tag to database.");
}

void image_tag_repository::write(context ctx, const std::vector<domain::image_tag>& image_tags) {
    BOOST_LOG_SEV(lg(), debug) << "Writing image-tags to database. Count: "
                               << image_tags.size();

    execute_write_query(ctx, image_tag_mapper::map(image_tags),
        lg(), "Writing image-tags to database.");
}

std::vector<domain::image_tag> image_tag_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<image_tag_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<image_tag_entity, domain::image_tag>(ctx, query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(), "Reading latest image-tags");
}

std::vector<domain::image_tag>
image_tag_repository::read_latest_by_image(context ctx, const std::string& image_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest image-tags by image: " << image_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<image_tag_entity>> |
        where("image_id"_c == image_id && "valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<image_tag_entity, domain::image_tag>(ctx, query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(), "Reading latest image-tags by image.");
}

std::vector<domain::image_tag>
image_tag_repository::read_latest_by_tag(context ctx, const std::string& tag_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest image-tags by tag: " << tag_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<image_tag_entity>> |
        where("tag_id"_c == tag_id && "valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<image_tag_entity, domain::image_tag>(ctx, query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(), "Reading latest image-tags by tag.");
}

void image_tag_repository::remove(context ctx, const std::string& image_id,
                                   const std::string& tag_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing image-tag from database. Image: "
                               << image_id << " Tag: " << tag_id;

    const auto query = sqlgen::delete_from<image_tag_entity> |
        where("image_id"_c == image_id && "tag_id"_c == tag_id);

    execute_delete_query(ctx, query, lg(), "Removing image-tag from database.");
}

}
