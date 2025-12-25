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
#ifndef ORES_ASSETS_REPOSITORY_IMAGE_REPOSITORY_HPP
#define ORES_ASSETS_REPOSITORY_IMAGE_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.assets/domain/image.hpp"

namespace ores::assets::repository {

/**
 * @brief Reads and writes images off of data storage.
 */
class image_repository {
private:
    inline static std::string_view logger_name =
        "ores.assets.repository.image_repository";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes an image to database.
     */
    /**@{*/
    void write(context ctx, const domain::image& image);
    void write(context ctx, const std::vector<domain::image>& images);
    /**@}*/

    /**
     * @brief Reads latest images, possibly filtered by image_id or key.
     */
    /**@{*/
    std::vector<domain::image> read_latest(context ctx);
    std::vector<domain::image>
    read_latest_by_id(context ctx, const std::string& image_id);
    std::vector<domain::image>
    read_latest_by_ids(context ctx, const std::vector<std::string>& image_ids);
    std::vector<domain::image>
    read_latest_by_key(context ctx, const std::string& key);
    /**@}*/

    /**
     * @brief Reads all images (including historical versions).
     */
    std::vector<domain::image> read_all(context ctx);

    /**
     * @brief Deletes an image by closing its temporal validity.
     */
    void remove(context ctx, const std::string& image_id);
};

}

#endif
