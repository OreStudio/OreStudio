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
#ifndef ORES_REFDATA_SERVICE_ZERO_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_SERVICE_ZERO_CONVENTION_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/zero_convention.hpp"
#include "ores.refdata.core/repository/zero_convention_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing zero conventions.
 */
class zero_convention_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.zero_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit zero_convention_service(context ctx);

    std::vector<domain::zero_convention> list_cnvs();

    std::optional<domain::zero_convention>
    find_cnv(const std::string& id);

    void save_cnv(const domain::zero_convention& v);

    void remove_cnv(const std::string& id);

    std::vector<domain::zero_convention>
    get_cnv_history(const std::string& id);

private:
    context ctx_;
    repository::zero_convention_repository repo_;
};

}

#endif
