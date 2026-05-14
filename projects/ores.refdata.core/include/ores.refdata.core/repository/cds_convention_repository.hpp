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
#ifndef ORES_REFDATA_REPOSITORY_CDS_CONVENTION_REPOSITORY_HPP
#define ORES_REFDATA_REPOSITORY_CDS_CONVENTION_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/cds_convention.hpp"
#include "ores.refdata.core/export.hpp"

namespace ores::refdata::repository {

/**
 * @brief Reads and writes CDS conventions to data storage.
 */
class ORES_REFDATA_CORE_EXPORT cds_convention_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.cds_convention_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::string sql();

    void write(context ctx, const domain::cds_convention& v);
    void write(context ctx, const std::vector<domain::cds_convention>& v);

    std::vector<domain::cds_convention> read_latest(context ctx);
    std::vector<domain::cds_convention>
    read_latest(context ctx, const std::string& id);
    std::vector<domain::cds_convention>
    read_all(context ctx, const std::string& id);

    void remove(context ctx, const std::string& id);
};

}

#endif
