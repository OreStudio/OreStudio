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
#include "ores.dq.core/service/badge_service.hpp"

#include "ores.dq.api/domain/badge_severity_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/domain/badge_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/domain/code_domain_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::service {

using namespace ores::logging;

badge_service::badge_service(context ctx)
    : ctx_(ctx),
      map_repo_(ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Badge service initialised.";
}

// =============================================================================
// Badge Severity
// =============================================================================

std::vector<domain::badge_severity> badge_service::list_severities() {
    BOOST_LOG_SEV(lg(), debug) << "Listing badge severities.";
    return sev_repo_.read_latest(ctx_);
}

void badge_service::save_severity(const domain::badge_severity& v) {
    BOOST_LOG_SEV(lg(), debug) << "Saving badge severity: " << v.code;
    sev_repo_.write(ctx_, v);
}

void badge_service::remove_severities(const std::vector<std::string>& codes) {
    BOOST_LOG_SEV(lg(), debug) << "Removing " << codes.size() << " badge severities.";
    for (const auto& code : codes)
        sev_repo_.remove(ctx_, code);
}

std::vector<domain::badge_severity>
badge_service::get_severity_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting badge severity history for: " << code;
    return sev_repo_.read_all(ctx_, code);
}

// =============================================================================
// Code Domain
// =============================================================================

std::vector<domain::code_domain> badge_service::list_code_domains() {
    BOOST_LOG_SEV(lg(), debug) << "Listing code domains.";
    return dom_repo_.read_latest(ctx_);
}

void badge_service::save_code_domain(const domain::code_domain& v) {
    BOOST_LOG_SEV(lg(), debug) << "Saving code domain: " << v.code;
    dom_repo_.write(ctx_, v);
}

void badge_service::remove_code_domains(const std::vector<std::string>& codes) {
    BOOST_LOG_SEV(lg(), debug) << "Removing " << codes.size() << " code domains.";
    for (const auto& code : codes)
        dom_repo_.remove(ctx_, code);
}

std::vector<domain::code_domain>
badge_service::get_code_domain_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting code domain history for: " << code;
    return dom_repo_.read_all(ctx_, code);
}

// =============================================================================
// Badge Definition
// =============================================================================

std::vector<domain::badge_definition> badge_service::list_definitions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing badge definitions.";
    return def_repo_.read_latest(ctx_);
}

void badge_service::save_definition(const domain::badge_definition& v) {
    BOOST_LOG_SEV(lg(), debug) << "Saving badge definition: " << v.code;
    def_repo_.write(ctx_, v);
}

void badge_service::remove_definitions(const std::vector<std::string>& codes) {
    BOOST_LOG_SEV(lg(), debug) << "Removing " << codes.size() << " badge definitions.";
    for (const auto& code : codes)
        def_repo_.remove(ctx_, code);
}

std::vector<domain::badge_definition>
badge_service::get_definition_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting badge definition history for: " << code;
    return def_repo_.read_all(ctx_, code);
}

// =============================================================================
// Badge Mapping (read-only)
// =============================================================================

std::vector<messaging::badge_mapping> badge_service::list_mappings() {
    BOOST_LOG_SEV(lg(), debug) << "Listing badge mappings.";
    return map_repo_.read_all();
}

}
