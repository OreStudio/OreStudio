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
#ifndef ORES_REFDATA_CORE_SERVICE_ASSET_CLASS_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_ASSET_CLASS_SERVICE_HPP

#include <string>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/asset_class_info.hpp"
#include "ores.refdata.core/repository/asset_class_repository.hpp"

namespace ores::refdata::service {

class asset_class_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.asset_class_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    using context = ores::database::context;

public:
    explicit asset_class_service(context ctx) : ctx_(std::move(ctx)) {}

    std::vector<domain::asset_class_info> list_asset_classes(
        const std::string& coding_scheme = {},
        std::uint32_t offset = 0,
        std::uint32_t limit = 200);

    std::uint32_t count_asset_classes(
        const std::string& coding_scheme = {});

private:
    context ctx_;
    repository::asset_class_repository repo_;
};

}

#endif
