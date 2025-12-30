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
#ifndef ORES_ASSETS_SERVICE_ASSETS_SERVICE_HPP
#define ORES_ASSETS_SERVICE_ASSETS_SERVICE_HPP

#include <string>
#include <vector>
#include "ores.database/domain/context.hpp"
#include "ores.assets/domain/currency_image.hpp"
#include "ores.assets/domain/image.hpp"
#include "ores.assets/repository/currency_image_repository.hpp"
#include "ores.assets/repository/image_repository.hpp"
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::assets::service {

/**
 * @brief Service for managing assets including images and currency-image mappings.
 *
 * Provides a higher-level interface for asset operations, wrapping
 * the underlying repositories.
 */
class assets_service {
private:
    inline static std::string_view logger_name =
        "ores.assets.service.assets_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs an assets_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit assets_service(context ctx);

    /**
     * @brief Retrieves all currency-image mappings.
     *
     * @return Vector of all currency-image associations.
     */
    std::vector<domain::currency_image> get_currency_images();

    /**
     * @brief Retrieves images by their IDs.
     *
     * @param image_ids The IDs of the images to retrieve.
     * @return Vector of images found for the given IDs.
     */
    std::vector<domain::image> get_images(const std::vector<std::string>& image_ids);

private:
    context ctx_;
    repository::currency_image_repository currency_image_repo_;
    repository::image_repository image_repo_;
};

}

#endif
