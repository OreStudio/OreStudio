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
#ifndef ORES_DQ_API_MESSAGING_BADGE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_BADGE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.dq.api/domain/badge_severity.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.dq.api/domain/code_domain.hpp"

namespace ores::dq::messaging {

// =============================================================================
// Badge Mapping Domain Type
// =============================================================================

/**
 * @brief Lightweight read-only projection of a badge mapping entry.
 *
 * Associates a (code_domain_code, entity_code) pair with a badge_code.
 * Used by BadgeCache to build the lookup index at startup.
 */
struct badge_mapping {
    std::string code_domain_code;
    std::string entity_code;
    std::string badge_code;
};

// =============================================================================
// Badge Severity Protocol
// =============================================================================

struct get_badge_severities_request {
    using response_type = struct get_badge_severities_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.list";
    int offset = 0;
    int limit = 1000;
};

struct get_badge_severities_response {
    std::vector<ores::dq::domain::badge_severity> badge_severities;
    int total_available_count = 0;
};

struct save_badge_severity_request {
    using response_type = struct save_badge_severity_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.save";
    ores::dq::domain::badge_severity data;
};

struct save_badge_severity_response {
    bool success = false;
    std::string message;
};

struct delete_badge_severity_request {
    using response_type = struct delete_badge_severity_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.delete";
    std::vector<std::string> codes;
};

struct delete_badge_severity_response {
    bool success = false;
    std::string message;
};

struct get_badge_severity_history_request {
    using response_type = struct get_badge_severity_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.history";
    std::string code;
};

struct get_badge_severity_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::badge_severity> history;
};

// =============================================================================
// Code Domain Protocol
// =============================================================================

struct get_code_domains_request {
    using response_type = struct get_code_domains_response;
    static constexpr std::string_view nats_subject = "dq.v1.code_domains.list";
    int offset = 0;
    int limit = 1000;
};

struct get_code_domains_response {
    std::vector<ores::dq::domain::code_domain> code_domains;
    int total_available_count = 0;
};

struct save_code_domain_request {
    using response_type = struct save_code_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.code_domains.save";
    ores::dq::domain::code_domain data;
};

struct save_code_domain_response {
    bool success = false;
    std::string message;
};

struct delete_code_domain_request {
    using response_type = struct delete_code_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.code_domains.delete";
    std::vector<std::string> codes;
};

struct delete_code_domain_response {
    bool success = false;
    std::string message;
};

struct get_code_domain_history_request {
    using response_type = struct get_code_domain_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.code_domains.history";
    std::string code;
};

struct get_code_domain_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::code_domain> history;
};

// =============================================================================
// Badge Definition Protocol
// =============================================================================

struct get_badge_definitions_request {
    using response_type = struct get_badge_definitions_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_definitions.list";
    int offset = 0;
    int limit = 1000;
};

struct get_badge_definitions_response {
    std::vector<ores::dq::domain::badge_definition> definitions;
    int total_available_count = 0;
};

struct save_badge_definition_request {
    using response_type = struct save_badge_definition_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_definitions.save";
    ores::dq::domain::badge_definition data;
};

struct save_badge_definition_response {
    bool success = false;
    std::string message;
};

struct delete_badge_definition_request {
    using response_type = struct delete_badge_definition_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_definitions.delete";
    std::vector<std::string> codes;
};

struct delete_badge_definition_response {
    bool success = false;
    std::string message;
};

struct get_badge_definition_history_request {
    using response_type = struct get_badge_definition_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_definitions.history";
    std::string code;
};

struct get_badge_definition_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::domain::badge_definition> history;
};

// =============================================================================
// Badge Mapping Protocol (read-only)
// =============================================================================

struct get_badge_mappings_request {
    using response_type = struct get_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.list";
};

struct get_badge_mappings_response {
    std::vector<badge_mapping> mappings;
};

}

#endif
