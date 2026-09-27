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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_API_MESSAGING_REPORT_DEFINITION_TEMPLATE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_REPORT_DEFINITION_TEMPLATE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief One report definition offered as a template.
 */
struct dq_report_definition_template {
    /**
     * @brief The template's name.
     */
    std::string name;
    /**
     * @brief What the template produces.
     */
    std::string description;
    /**
     * @brief The kind of report the template instantiates.
     */
    std::string report_type;
    /**
     * @brief The schedule the template suggests.
     */
    std::string schedule_expression;
    /**
     * @brief How overlapping runs of the template are treated.
     */
    std::string concurrency_policy;
    /**
     * @brief Where the template sits in the list.
     */
    int display_order = 0;
};

/**
 * @brief Asks for the templates one dataset bundle offers.
 */
struct list_dq_report_definition_templates_request {
    using response_type = struct list_dq_report_definition_templates_response;
    static constexpr std::string_view nats_subject = "dq.v1.report-definition-templates.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The bundle whose templates are listed.
     */
    std::string bundle_code = "risk_management";
};

/**
 * @brief Reports the templates, or why they could not be read.
 */
struct list_dq_report_definition_templates_response {
    /**
     * @brief Whether the read completed.
     */
    bool success = false;
    /**
     * @brief Why it failed, when it did.
     */
    std::string message;
    /**
     * @brief The templates, in display order.
     */
    std::vector<dq_report_definition_template> templates;
};

}

#endif
