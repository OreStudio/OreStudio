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
#ifndef ORES_IAM_SERVICE_TENANT_TYPE_SERVICE_HPP
#define ORES_IAM_SERVICE_TENANT_TYPE_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/tenant_type.hpp"
#include "ores.iam/repository/tenant_type_repository.hpp"

namespace ores::iam::service {

/**
 * @brief Service for managing tenant types.
 */
class tenant_type_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.tenant_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit tenant_type_service(context ctx);

    std::vector<domain::tenant_type> list_types();

    std::optional<domain::tenant_type>
    find_type(const std::string& type);

    void save_type(const domain::tenant_type& type);

    void remove_type(const std::string& type);

    std::vector<domain::tenant_type>
    get_type_history(const std::string& type);

private:
    context ctx_;
    repository::tenant_type_repository repo_;
};

}

#endif
