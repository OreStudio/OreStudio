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
#ifndef ORES_REFDATA_SERVICE_OVERNIGHT_INDEX_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_SERVICE_OVERNIGHT_INDEX_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/overnight_index_convention_repository.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing overnight index conventions.
 */
class ORES_REFDATA_CORE_EXPORT overnight_index_convention_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.overnight_index_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit overnight_index_convention_service(context ctx);

    std::vector<domain::overnight_index_convention> list_overnight_index_conventions();

    std::optional<domain::overnight_index_convention>
    get_overnight_index_convention(const std::string& id);

    void save_overnight_index_convention(const domain::overnight_index_convention& v);

    void remove_overnight_index_convention(const std::string& id);

    std::vector<domain::overnight_index_convention>
    get_overnight_index_convention_history(const std::string& id);

private:
    context ctx_;
    repository::overnight_index_convention_repository repo_;
};

}

#endif
