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
#ifndef ORES_DQ_API_MESSAGING_REPORT_DEFINITION_TEMPLATE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_REPORT_DEFINITION_TEMPLATE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::dq::messaging {

struct dq_report_definition_template {
    std::string name;
    std::string description;
    std::string report_type;
    std::string schedule_expression;
    std::string concurrency_policy;
    int display_order = 0;
};

struct list_dq_report_definition_templates_request {
    using response_type = struct list_dq_report_definition_templates_response;
    static constexpr std::string_view nats_subject = "dq.v1.report-definition-templates.list";
    std::string bundle_code = "risk_management";
};

struct list_dq_report_definition_templates_response {
    bool success = false;
    std::string message;
    std::vector<dq_report_definition_template> templates;
};

}

#endif
