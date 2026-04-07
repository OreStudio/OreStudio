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
#ifndef ORES_DQ_CORE_SERVICE_BADGE_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_BADGE_SERVICE_HPP

#include <string>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/badge_severity.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.dq.api/domain/code_domain.hpp"
#include "ores.dq.core/repository/badge_severity_repository.hpp"
#include "ores.dq.core/repository/badge_definition_repository.hpp"
#include "ores.dq.core/repository/code_domain_repository.hpp"
#include "ores.dq.api/messaging/badge_protocol.hpp"
#include "ores.dq.core/repository/badge_mapping_repository.hpp"

namespace ores::dq::service {

/**
 * @brief Service for managing badges, badge severities, code domains, and
 * badge mappings.
 */
class badge_service {
private:
    inline static std::string_view logger_name =
        "ores.dq.service.badge_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit badge_service(context ctx);

    // =========================================================================
    // Badge Severity
    // =========================================================================

    std::vector<domain::badge_severity> list_severities();
    void save_severity(const domain::badge_severity& v);
    void remove_severities(const std::vector<std::string>& codes);
    std::vector<domain::badge_severity> get_severity_history(const std::string& code);

    // =========================================================================
    // Code Domain
    // =========================================================================

    std::vector<domain::code_domain> list_code_domains();
    void save_code_domain(const domain::code_domain& v);
    void remove_code_domains(const std::vector<std::string>& codes);
    std::vector<domain::code_domain> get_code_domain_history(const std::string& code);

    // =========================================================================
    // Badge Definition
    // =========================================================================

    std::vector<domain::badge_definition> list_definitions();
    void save_definition(const domain::badge_definition& v);
    void remove_definitions(const std::vector<std::string>& codes);
    std::vector<domain::badge_definition> get_definition_history(const std::string& code);

    // =========================================================================
    // Badge Mapping (read-only)
    // =========================================================================

    std::vector<messaging::badge_mapping> list_mappings();

private:
    context ctx_;
    repository::badge_severity_repository sev_repo_;
    repository::badge_definition_repository def_repo_;
    repository::code_domain_repository dom_repo_;
    repository::badge_mapping_repository map_repo_;
};

}

#endif
