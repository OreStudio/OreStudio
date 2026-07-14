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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_IR_CURVE_TEMPLATE_ENTRY_MAPPER_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_IR_CURVE_TEMPLATE_ENTRY_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_entity.hpp"

namespace ores::synthetic::repository {

/**
 * @brief Maps ir_curve_template_entry domain entities to data storage layer and vice-versa.
 */
class ORES_SYNTHETIC_CORE_EXPORT ir_curve_template_entry_mapper {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.repository.ir_curve_template_entry_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::ir_curve_template_entry map(const ir_curve_template_entry_entity& v);
    static ir_curve_template_entry_entity map(const domain::ir_curve_template_entry& v);

    static std::vector<domain::ir_curve_template_entry>
    map(const std::vector<ir_curve_template_entry_entity>& v);
    static std::vector<ir_curve_template_entry_entity>
    map(const std::vector<domain::ir_curve_template_entry>& v);
};

}

#endif
