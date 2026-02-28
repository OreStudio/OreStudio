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
#ifndef ORES_DQ_REPOSITORY_NATURE_DIMENSION_REPOSITORY_HPP
#define ORES_DQ_REPOSITORY_NATURE_DIMENSION_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/nature_dimension.hpp"

namespace ores::dq::repository {

/**
 * @brief Reads and writes nature_dimensions to data storage.
 */
class nature_dimension_repository {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.nature_dimension_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit nature_dimension_repository(context ctx);

    std::string sql();

    void write(const domain::nature_dimension& dimension);
    void write(const std::vector<domain::nature_dimension>& dimensions);

    std::vector<domain::nature_dimension> read_latest();
    std::vector<domain::nature_dimension> read_latest(const std::string& code);
    std::vector<domain::nature_dimension>
    read_latest(std::uint32_t offset, std::uint32_t limit);

    std::uint32_t get_total_count();
    std::vector<domain::nature_dimension> read_all(const std::string& code);
    void remove(const std::string& code);
    void remove(const std::vector<std::string>& codes);

private:
    context ctx_;
};

}

#endif
