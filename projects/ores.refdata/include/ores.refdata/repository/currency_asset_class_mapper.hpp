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
#ifndef ORES_REFDATA_REPOSITORY_CURRENCY_ASSET_CLASS_MAPPER_HPP
#define ORES_REFDATA_REPOSITORY_CURRENCY_ASSET_CLASS_MAPPER_HPP

#include "ores.refdata/domain/currency_asset_class.hpp"
#include "ores.refdata/repository/currency_asset_class_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::refdata::repository {

/**
 * @brief Maps currency_asset_class domain entities to data storage layer and vice-versa.
 */
class currency_asset_class_mapper {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.currency_asset_class_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain::currency_asset_class map(const currency_asset_class_entity& v);
    static currency_asset_class_entity map(const domain::currency_asset_class& v);

    static std::vector<domain::currency_asset_class>
    map(const std::vector<currency_asset_class_entity>& v);
    static std::vector<currency_asset_class_entity>
    map(const std::vector<domain::currency_asset_class>& v);
};

}

#endif
