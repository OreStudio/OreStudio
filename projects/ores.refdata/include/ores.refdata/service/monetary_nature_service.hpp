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
#ifndef ORES_REFDATA_SERVICE_MONETARY_NATURE_SERVICE_HPP
#define ORES_REFDATA_SERVICE_MONETARY_NATURE_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/monetary_nature.hpp"
#include "ores.refdata/repository/monetary_nature_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing currency asset classes.
 */
class monetary_nature_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.monetary_nature_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit monetary_nature_service(context ctx);

    std::vector<domain::monetary_nature> list_types();

    std::optional<domain::monetary_nature>
    find_type(const std::string& code);

    void save_type(const domain::monetary_nature& v);

    void save_types(const std::vector<domain::monetary_nature>& types);

    void remove_type(const std::string& code);

    void remove_types(const std::vector<std::string>& codes);

    std::vector<domain::monetary_nature>
    get_type_history(const std::string& code);

private:
    context ctx_;
    repository::monetary_nature_repository repo_;
};

}

#endif
