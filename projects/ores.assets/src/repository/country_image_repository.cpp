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
#include "ores.assets/repository/country_image_repository.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.assets/repository/country_image_mapper.hpp"
#include "ores.assets/repository/country_image_entity.hpp"

namespace ores::assets::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::telemetry::log;
using namespace ores::database::repository;

std::string country_image_repository::sql() {
    return generate_create_table_sql<country_image_entity>(lg());
}

void country_image_repository::write(context ctx, const domain::country_image& country_image) {
    BOOST_LOG_SEV(lg(), debug) << "Writing country-image to database. Alpha-2: "
                               << country_image.alpha2_code;

    execute_write_query(ctx, country_image_mapper::map(country_image),
        lg(), "Writing country-image to database.");
}

void country_image_repository::write(context ctx, const std::vector<domain::country_image>& country_images) {
    BOOST_LOG_SEV(lg(), debug) << "Writing country-images to database. Count: "
                               << country_images.size();

    execute_write_query(ctx, country_image_mapper::map(country_images),
        lg(), "Writing country-images to database.");
}

std::vector<domain::country_image> country_image_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<country_image_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<country_image_entity, domain::country_image>(ctx, query,
        [](const auto& entities) { return country_image_mapper::map(entities); },
        lg(), "Reading latest country-images");
}

std::vector<domain::country_image>
country_image_repository::read_latest_by_country(context ctx, const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest country-images by alpha-2: " << alpha2_code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<country_image_entity>> |
        where("alpha2_code"_c == alpha2_code && "valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<country_image_entity, domain::country_image>(ctx, query,
        [](const auto& entities) { return country_image_mapper::map(entities); },
        lg(), "Reading latest country-images by alpha-2.");
}

std::vector<domain::country_image>
country_image_repository::read_latest_by_image(context ctx, const std::string& image_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest country-images by image: " << image_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<country_image_entity>> |
        where("image_id"_c == image_id && "valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<country_image_entity, domain::country_image>(ctx, query,
        [](const auto& entities) { return country_image_mapper::map(entities); },
        lg(), "Reading latest country-images by image.");
}

void country_image_repository::remove(context ctx, const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing country-image from database. Alpha-2: "
                               << alpha2_code;

    const auto query = sqlgen::delete_from<country_image_entity> |
        where("alpha2_code"_c == alpha2_code);

    execute_delete_query(ctx, query, lg(), "Removing country-image from database.");
}

}
