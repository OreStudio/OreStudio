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
#ifndef ORES_IAM_CORE_REPOSITORY_ACCOUNT_TYPE_MAPPER_HPP
#define ORES_IAM_CORE_REPOSITORY_ACCOUNT_TYPE_MAPPER_HPP

#include "ores.iam.api/domain/account_type.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_type_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::iam::repository {

/**
 * @brief Maps account_type domain entities to data storage layer and vice-versa.
 */
class ORES_IAM_CORE_EXPORT account_type_mapper {
private:
    inline static std::string_view logger_name = "ores.iam.repository.account_type_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::account_type map(const account_type_entity& v);
    static account_type_entity map(const domain::account_type& v);

    static std::vector<domain::account_type> map(const std::vector<account_type_entity>& v);
    static std::vector<account_type_entity> map(const std::vector<domain::account_type>& v);
};

}

#endif
