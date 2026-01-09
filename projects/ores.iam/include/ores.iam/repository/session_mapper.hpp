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
#ifndef ORES_IAM_REPOSITORY_SESSION_MAPPER_HPP
#define ORES_IAM_REPOSITORY_SESSION_MAPPER_HPP

#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/session.hpp"
#include "ores.iam/repository/session_entity.hpp"

namespace ores::iam::repository {

/**
 * @brief Maps session domain objects to database entities and vice-versa.
 */
class session_mapper {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.session_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::session map(const session_entity& v);
    static session_entity map(const domain::session& v);

    static std::vector<domain::session>
    map(const std::vector<session_entity>& v);
    static std::vector<session_entity>
    map(const std::vector<domain::session>& v);

    static domain::session_statistics map(const session_statistics_entity& v);
    static std::vector<domain::session_statistics>
    map(const std::vector<session_statistics_entity>& v);
};

}

#endif
