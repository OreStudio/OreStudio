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
#ifndef ORES_REPORTING_REPOSITORY_REPORT_INPUT_BUNDLE_REPOSITORY_HPP
#define ORES_REPORTING_REPOSITORY_REPORT_INPUT_BUNDLE_REPOSITORY_HPP

#include <optional>
#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.reporting.core/repository/report_input_bundle_entity.hpp"

namespace ores::reporting::repository {

/**
 * @brief Persists and retrieves report_input_bundle records.
 *
 * Bundles are write-once artifacts; no update path is provided.
 */
class report_input_bundle_repository {
private:
    inline static std::string_view logger_name =
        "ores.reporting.repository.report_input_bundle_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Persists a new bundle record.
     */
    void create(context ctx, const report_input_bundle_entity& bundle);

    /**
     * @brief Finds the bundle for a given report instance.
     *
     * @return The bundle if found, nullopt otherwise.
     */
    std::optional<report_input_bundle_entity>
    find_by_instance_id(context ctx, const std::string& instance_id);
};

}

#endif
