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
#ifndef ORES_REFDATA_CORE_REPOSITORY_ASSET_CLASS_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_ASSET_CLASS_REPOSITORY_HPP

#include <string>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/asset_class_info.hpp"

namespace ores::refdata::repository {

/**
 * @brief Reads published asset classes from ores_refdata_asset_classes_tbl.
 */
class asset_class_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.asset_class_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Lists all currently valid asset classes for a tenant.
     *
     * @param ctx            Request context (provides tenant_id and connection pool).
     * @param coding_scheme  Optional filter; pass empty string to return all schemes.
     * @param offset         Pagination offset.
     * @param limit          Maximum number of rows to return.
     */
    std::vector<domain::asset_class_info> read_latest(context ctx,
        const std::string& coding_scheme = {},
        std::uint32_t offset = 0,
        std::uint32_t limit = 200);

    std::uint32_t count_latest(context ctx,
        const std::string& coding_scheme = {});
};

}

#endif
