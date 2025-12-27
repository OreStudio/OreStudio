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
#include "ores.assets/repository/currency_image_repository.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.assets/repository/currency_image_mapper.hpp"
#include "ores.assets/repository/currency_image_entity.hpp"

namespace ores::assets::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::telemetry::log;
using namespace ores::database::repository;

std::string currency_image_repository::sql() {
    return generate_create_table_sql<currency_image_entity>(lg());
}

void currency_image_repository::write(context ctx, const domain::currency_image& currency_image) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency-image to database. ISO: "
                               << currency_image.iso_code;

    execute_write_query(ctx, currency_image_mapper::map(currency_image),
        lg(), "Writing currency-image to database.");
}

void currency_image_repository::write(context ctx, const std::vector<domain::currency_image>& currency_images) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency-images to database. Count: "
                               << currency_images.size();

    execute_write_query(ctx, currency_image_mapper::map(currency_images),
        lg(), "Writing currency-images to database.");
}

std::vector<domain::currency_image> currency_image_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<currency_image_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<currency_image_entity, domain::currency_image>(ctx, query,
        [](const auto& entities) { return currency_image_mapper::map(entities); },
        lg(), "Reading latest currency-images");
}

std::vector<domain::currency_image>
currency_image_repository::read_latest_by_currency(context ctx, const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency-images by ISO: " << iso_code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<currency_image_entity>> |
        where("iso_code"_c == iso_code && "valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<currency_image_entity, domain::currency_image>(ctx, query,
        [](const auto& entities) { return currency_image_mapper::map(entities); },
        lg(), "Reading latest currency-images by ISO.");
}

std::vector<domain::currency_image>
currency_image_repository::read_latest_by_image(context ctx, const std::string& image_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency-images by image: " << image_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<currency_image_entity>> |
        where("image_id"_c == image_id && "valid_to"_c == max.value()) |
        order_by("assigned_at"_c.desc());

    return execute_read_query<currency_image_entity, domain::currency_image>(ctx, query,
        [](const auto& entities) { return currency_image_mapper::map(entities); },
        lg(), "Reading latest currency-images by image.");
}

void currency_image_repository::remove(context ctx, const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency-image from database. ISO: "
                               << iso_code;

    const auto query = sqlgen::delete_from<currency_image_entity> |
        where("iso_code"_c == iso_code);

    execute_delete_query(ctx, query, lg(), "Removing currency-image from database.");
}

}
